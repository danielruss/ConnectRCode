# ===========================================================================
# R/qc_engine.R
# Builds a lazy dplyr/bigrquery query for a single QC rule.
# Never calls collect() — that happens once at the end.
# ===========================================================================

#' Build per-rule flag/value expressions and the columns they need
#'
#' @param rule_id     A rule identifier, added to the output for traceability.
#' @param ConceptID   The column to validate.
#' @param check_type  The heart of the
#'
#' @noRd
build_single_rule_query <- function(rule_id,ConceptID, check_type, is_na_ok,
                             ValidValues, cross_columns, cross_values, ...){

  col_sym <- rlang::sym(ConceptID)
  cross_columns <- unname(cross_columns)

  fail_expr <- switch(check_type,
                      list       = rlang::expr(!(!!col_sym %in% local(!!ValidValues))),
                      length_eq  = rlang::expr(str_length(!!col_sym) != local(!!as.integer(ValidValues[[1]]))),
                      length_le  = rlang::expr(str_length(!!col_sym) > local(!!as.integer(ValidValues[[1]]))),
                      datetime   = rlang::expr(!is.datetime(!!col_sym)),
                      datebefore = rlang::expr(!!col_sym >= local(!!ValidValues[[1]])),
                      is_na      = rlang::expr(!is.na(!!col_sym) & str_length(!!col_sym)>0),
                      not_na     = rlang::expr(is.na(!!col_sym) | str_length(!!col_sym)==0 ),
                      stop("Unknown check type: ", check_type)
  )

  missing_expr <- rlang::expr(is.na(!!col_sym))
  blank_expr <- rlang::expr(str_length(!!col_sym) == 0)
  na_expr <- rlang::expr((!!missing_expr) | (!!blank_expr))

  base_expr <- if (is_na_ok) {
    rlang::expr((!!fail_expr) & !(!!na_expr))
  } else {
    rlang::expr((!!fail_expr) | (!!na_expr))
  }

  final_expr <- if (length(cross_columns) > 0 && !all(is.na(cross_columns))) {
    keep <- !is.na(cross_columns)
    cross_columns <- cross_columns[keep]
    cross_values <- cross_values[keep]
    needed_cols = unique(c(ConceptID, cross_columns))
    gate_exprs <- purrr::map2(cross_columns, cross_values, \(col, vals) {
      rlang::expr(!!rlang::sym(col) %in% local(!!vals))
    })
    gate_expr <- purrr::reduce(gate_exprs, \(a, b) rlang::expr((!!a) & (!!b)))

    # A CASE expression keeps every part of the failure check inside the
    # cross-rule gate, including check types whose failure expression has ORs.
    rlang::expr(dplyr::if_else(!!gate_expr, !!base_expr, FALSE))
  } else {
    needed_cols = ConceptID
    base_expr
  }

  list(
    rule_id     = rule_id,
    flag        = final_expr,
    needed_cols = needed_cols
  )
}

#' Build ONE lazy query for an entire chunk of rules
#'
#' @param bq_tbl A lazy bigrquery table (raw or pre-selected -- either is fine,
#'   see note on laziness/column pruning).
#' @param chunk  A tibble of rules (one chunk_id's worth).
#' @param key_cols Character vector of identifier columns to always keep.
#' @return A lazy dplyr query -- not yet collected. Columns:
#'   key_cols, flag_<rule_id>..., value_<rule_id>...
#' @noRd
build_chunk_query <- function(bq_tbl, chunk, key_cols){
  # make sure the rule_id is a valid column name
  # this is for temporary columns...
  safe_ids <- make.names(chunk$rule_id, unique = TRUE)
  id_map <- rlang::set_names(chunk$rule_id, paste0("flag_", safe_ids))

  parsed_chunk <- chunk |>
    dplyr::mutate(check_type = purrr::map_chr(Qctype, \(x) Qctype_mapping[[tolower(x)]]$check_type),
                  is_na_ok   = purrr::map_lgl(Qctype, \(x) Qctype_mapping[[tolower(x)]]$is_na_ok) ) |>
    purrr::pmap(build_single_rule_query)


  needed_cols <- unique(c(key_cols, unlist(purrr::map(parsed_chunk, "needed_cols"))))

  flag_exprs  <- purrr::map(parsed_chunk, "flag")  |> rlang::set_names(paste0("flag_", safe_ids))
  value_exprs <- purrr::map(parsed_chunk, "value") |> rlang::set_names(paste0("value_", safe_ids))

  q <- bq_tbl |>
    dplyr::select(dplyr::all_of(needed_cols)) |>
    dplyr::mutate(!!!flag_exprs, !!!value_exprs) |>
    dplyr::filter(dplyr::if_any(dplyr::starts_with("flag_"), \(x) x))

  list(query=q,id_map=id_map)
}

collapse_flags_to_rule_ids <- function(results,id_map){
  if (nrow(results) == 0) return(tibble::tibble())

  flag_cols <- names(id_map)

  results |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(flag_cols),
      names_to = "flag_col",
      values_to = "flagged"
    ) |>
    dplyr::filter(flagged) |>
    dplyr::mutate(rule_id = id_map[flag_col]) |>
    dplyr::select(-flag_col, -flagged)
}

############################ Build_rule_query is slated for removal ##############
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
  #cli::cli_inform(paste0("... starting rule_id: ",rule_id,"\n"))
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
                      is_na      = rlang::expr(!is.na(!!col_sym) & str_length(!!col_sym)>0),
                      not_na     = rlang::expr(is.na(!!col_sym) | str_length(!!col_sym)==0 ),
                      stop("Unknown check type: ", c_type)
  )

  na_expr <- rlang::expr(is.na(!!col_sym) | str_length(!!col_sym)==0)

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


  #cli::cli_inform(rlang::expr_text(final_expr))
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
run_qc <- function(bq_tbl, chunk_size = 30) {
  rules <- .app_state$rules

  ## --- 1. select the columns required for QAQC
  tables <- .app_state$config$modules[[.app_state$module]]$envs[[.app_state$environment]]$tables
  table_keys <- purrr::map(tables, "key")
  missing_keys <- purrr::map_lgl(table_keys, \(x) {
    is.null(x) || length(x) == 0 || all(is.na(x) | !nzchar(x))
  })
  if (any(missing_keys)) {
    table_names <- purrr::map_chr(tables[missing_keys], \(x) x$name %||% "<unnamed>")
    stop("Table config missing key for: ", paste(table_names, collapse = ", "))
  }
  key_cols <- unique(unlist(table_keys, use.names = FALSE))
  data_cols <- colnames(bq_tbl)
  missing_key_cols <- setdiff(key_cols, data_cols)
  if (length(missing_key_cols) > 0) {
    stop("Data is missing key column(s): ", paste(missing_key_cols, collapse = ", "))
  }

  rules <- rules |> dplyr::mutate(
    needed = purrr::map2(ConceptID, cross_columns, \(x, y) {
      cols <- unique(c(x, y))
      cols[!is.na(cols) & nzchar(cols)]
    }),
    bad_column = purrr::map_lgl(needed, \(cols) !all(cols %in% data_cols))
  )
  required_columns <- unique(c(key_cols, unlist(rules$needed[!rules$bad_column], use.names=FALSE)))
  required_columns <- required_columns[!is.na(required_columns) & nzchar(required_columns) ]

  # only rules in the Qctype_mapping work...
  bad_rules <- rules |> dplyr::filter(
    !tolower(Qctype) %in% names(Qctype_mapping) |
    bad_cross |
    bad_column
  )

  if (nrow(bad_rules)>0) {
    message(rep("-",55),"\nBad rules\n",rep("-",55))
    print(bad_rules)
    message(rep("-",55))
    #arrow::write_feather(bad_rules,"bad_rules.feather")
    #stop()
  }

  rules <- rules |> dplyr::filter(!(rule_id %in% bad_rules$rule_id))

  lzy_tbl <- bq_tbl |>
    dplyr::select(dplyr::all_of(required_columns))

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
    lazy_results <- build_chunk_query(lzy_tbl,chunk,key_cols)
    materialized <- lazy_results$query |> dplyr::collect()
    results <- collapse_flags_to_rule_ids(materialized,lazy_results$id_map)
#    chunk |> purrr::pmap(
#      \(rule_id, ConceptID, check_type, is_na_ok,
#        ValidValues, cross_columns, cross_values, ...) {
#        build_rule_query(
#          bq_tbl    = lzy_tbl,
#          concept_col = ConceptID,
#          c_type    = check_type,
#          is_na_ok  = is_na_ok,
#          valid_values = ValidValues,
#          cross_columns = unname(cross_columns),
#          cross_values = cross_values,
#          rule_id   = rule_id
#        )
#      }
#    )
#    |>
  },.progress = "Running QC chunks") |>
    dplyr::bind_rows()
}
