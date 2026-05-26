# ===========================================================================
# R/qc_engine.R
# Builds a lazy dplyr/bigrquery query for a single QC rule.
# Never calls collect() — that happens once at the end.
# ===========================================================================

#' Build a lazy query for a single QC rule
#'
#' @param bq_tbl      A lazy bigrquery table (from tbl(con, "table")).
#' @param concept_col The column name to validate (string).
#' @param c_type      One of "List", "Length", "Numeric".
#' @param is_na_ok    Logical. Is NA an acceptable value?
#' @param valid_values  A vector of valid values (for "List") or max length (for "Length").
#' @param cross_columns  Character vector of cross-condition column names. NULL if none.
#' @param cross_values  List of vectors, one per cross_col, of acceptable values.
#' @param rule_id     A rule identifier, added to the output for traceability.
#' @return A lazy dplyr query (not yet collected).
#' @noRd
build_rule_query <- function(bq_tbl, concept_col, c_type, is_na_ok,
                             valid_values, cross_columns, cross_values, rule_id) {
  cli::cli_inform(paste0("... starting rule_id: ",rule_id,"\n"))
  col_sym <- rlang::sym(concept_col)

  # ---------------------------------------------------------
  # STEP A: Cross-condition gate (filter rows that trigger the rule)
  # ---------------------------------------------------------
  lazy_q <- bq_tbl

  if (length(cross_columns) > 0 && !all(is.na(cross_columns))) {
    gate_exprs <- purrr::map2(cross_columns, cross_values, \(col, vals) {
      rlang::expr(!!rlang::sym(col) %in% local(!!vals))
    })
    lazy_q <- dplyr::filter(lazy_q, !!!gate_exprs)
  }

  # ---------------------------------------------------------
  # STEP B: Value check — what counts as a FAILURE?
  # ---------------------------------------------------------
  fail_expr <- switch(c_type,
                      list       = rlang::expr(!(!!col_sym %in% local(!!valid_values))),
                      length_eq  = rlang::expr(str_length(!!col_sym) != local(!!as.integer(valid_values[[1]]))),
                      length_le  = rlang::expr(str_length(!!col_sym) > local(!!as.integer(valid_values[[1]]))),
                      datetime   = rlang::expr(!is.datetime(!!col_sym)),
                      datebefore = rlang::expr(!!col_sym >= local(!!valid_values[[1]])),
                      is_na      = rlang::expr(!is.na(!!col_sym)),
                      not_na     = rlang::expr(is.na(!!col_sym)),
                      stop("Unknown check type: ", c_type)
  )

  na_expr <- rlang::expr(is.na(!!col_sym))

  # ---------------------------------------------------------
  # STEP C: Combine failure + NA logic
  #   is_na_ok = TRUE  → fails if (fail AND NOT NA)   i.e. NA is a free pass
  #   is_na_ok = FALSE → fails if (fail OR  NA)       i.e. NA is also a failure
  # ---------------------------------------------------------
  final_expr <- if (is_na_ok) {
    rlang::expr((!!fail_expr) & !(!!na_expr))
  } else {
    rlang::expr((!!fail_expr) | (!!na_expr))
  }

  cli::cli_inform(rlang::expr_text(final_expr))
  # ---------------------------------------------------------
  # STEP D: Filter to failures, tag with rule metadata
  # ---------------------------------------------------------
  lazy_q |>
    dplyr::filter(!!final_expr) |>
    dplyr::mutate(
      qc_rule_id     = local(!!rule_id),
      qc_column      = local(!!concept_col),
      qc_check_type  = local(!!c_type),
      qc_na_ok       = local(!!is_na_ok)
    )
}

# ===========================================================================
# Run all rules — one trip to BigQuery
# ===========================================================================

#' Run all QC rules against a BigQuery table
#'
#' @param bq_tbl   A lazy bigrquery table.
#' @param rules    A tibble of rules (see details).
#' @return A collected tibble of all failing rows, tagged with rule metadata.
#'
#' @details
#' \code{rules} must have columns:
#' \describe{
#'   \item{rule_id}{Unique rule identifier}
#'   \item{ConceptID}{Column in \code{bq_tbl} to validate}
#'   \item{check_type}{One of "List", "Length", "Numeric"}
#'   \item{is_na_ok}{Logical — is NA an acceptable value?}
#'   \item{valid_values}{A list-column of valid values / max length}
#'   \item{cross_columns}{A list-column of cross-condition column names (or NULL)}
#'   \item{cross_values}{A list-column of vectors of valid cross values (or NULL)}
#' }
#' @export
run_qc <- function(bq_tbl, rules,chunk_size = 30) {
  ## --- 1. select the columns required for QAQC
  key_cols <- c("Connect_ID", "token")
  required_columns <- unique(c(key_cols,rules$ConceptID, unlist(rules$cross_columns,use.names=FALSE)))
  required_columns <- required_columns[!is.na(required_columns)]

  ## --- 2. Materialize a trimmed temp table in BigQuery -------------------
  cli::cli_inform("Materializing trimmed table to BigQuery temp table...")
  tbl <- bq_tbl |>
    dplyr::select(dplyr::any_of(required_columns)) |>
    dplyr::compute(
      name      = paste0("qc_temp_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      temporary = TRUE
    )
  cli::cli_inform("Temp table ready.")


  chunks <- rules |>
    ## Prepare the metadata...
    dplyr::mutate(
      check_type=purrr::map_chr(Qctype,\(x) Qctype_mapping[[tolower(x)]]$check_type),
      is_na_ok=purrr::map_lgl(Qctype,\(x) Qctype_mapping[[tolower(x)]]$is_na_ok)
    ) |>
    ## split the data into chunks...
    dplyr::mutate(chunk_id = ceiling(dplyr::row_number()/chunk_size)) |>
    dplyr::group_split(chunk_id, .keep = FALSE)

  chunks |> purrr::map(\(chunk){

    chunk |> purrr::pmap(
      \(rule_id, ConceptID, check_type, is_na_ok,
        ValidValues, cross_columns, cross_values, ...) {
        build_rule_query(
          bq_tbl    = tbl,
          concept_col = ConceptID,
          c_type    = check_type,
          is_na_ok  = is_na_ok,
          valid_values = ValidValues,
          cross_columns = unname(cross_columns),
          cross_values = cross_values,
          rule_id   = rule_id
        )
      }
    ) |>
      purrr::reduce(dplyr::union_all) |>  # one massive lazy query
      #dplyr::count(qc_rule_id, qc_column, qc_check_type) |>
      dplyr::collect()                    # one round trip to BigQuery
  },.progress = "Running QC chunks") |>
    dplyr::bind_rows()
}
