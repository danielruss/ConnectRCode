.dict_store <- new.env(parent = emptyenv())
.dict_store$dictionary <- NULL
.dict_store$normalize_cid <- identity

#' Configure the data dictionary
#'
#' Store the dictionary and concept ID normalizer used by [lookup()] and
#' [explain()] for the current R session. Calling this function replaces the
#' previously configured dictionary and normalizer.
#'
#' @param df A data frame containing `concept_id` and `definitions` columns.
#' @param normalize_cid A function that takes a character vector of identifiers
#'   and returns a vector of the same length for matching against `concept_id`.
#'   Defaults to [identity()]. Dictionary identifiers are not normalized.
#' @return The supplied `normalize_cid` function, invisibly.
#' @seealso [lookup()], [c4cp_normalize_cid()]
#' @export
set_dict <- function(df, normalize_cid = identity){
  required_cols <- c("concept_id","definitions")
  missing_col <- setdiff(required_cols,names(df))

  if ( length(missing_col) ) {
    rlang::abort(paste0("The dictionary needs the columns: `concept_id` and `definitions` missing: ",paste(missing_col,collapse = ", ")) )
  }

  .dict_store$dictionary <- df
  .dict_store$normalize_cid <- normalize_cid
}


#' Look up concept definitions
#'
#' Translate concept identifiers using the configured data dictionary.
#' Identifiers are converted to character and normalized with the function
#' configured by `set_dict()` before matching. Missing definitions and unmatched
#' identifiers fall back to the original identifiers.
#'
#' @param cids A vector or list of concept identifiers.
#' @return A vector of definitions, or a list with the same structure as `cids`
#'   when the input is a list.
#' @details A dictionary with `concept_id` and `definitions` columns must be
#'   configured with `set_dict()` before calling this function. An error is
#'   raised if no dictionary is configured.
#' @seealso [explain()]
#' @export
lookup <- function(cids){
  if (is.null(.dict_store$dictionary$definitions)){
    rlang::abort("The data dictionary is not defined!")
  }

  flat_cids <- if (is.list(cids)){
    unlist(cids, use.names = FALSE)
  } else {
    cids
  }

  original_cids <- as.character(flat_cids)
  flat_cids <- .dict_store$normalize_cid(original_cids)

  definitions <- .dict_store$dictionary$definitions[
    match(flat_cids, .dict_store$dictionary$concept_id)
  ] %??% original_cids

  if (is.list(cids)) {
    utils::relist(definitions, cids)
  } else {
    definitions
  }

}

# Original implementation (preserved for reference):
# explainers <- list(
#   crossvalid = \(row) {
#     lup_cid <- lookup(row$ConceptID)
#     lup_vv  <- lookup(row$ValidValue)
#     lup_1 <- lookup(row[[row$ConceptID]])
#     purrr::map2_chr(row$cross_column,row$cross_value, \(xcol,xvalue){
#       lup_xcol <- lookup(xcol)
#       lup_xval <- lookup(xvalue)
#       paste0("If ",lup_xcol," is in [",paste(lup_xval,collapse = ", "),"] then ",
#       lup_cid," should be in [",paste(lup_vv,collapse = ", "),"] but it is ",lookup(row[[row$ConceptID]])
#       )
#     }) |> paste(collapse = "\n")
#   },
#   default = \(row) {
#     paste0(row$Qctype, " is not configured for explainations ....")
#   }
# )
#
#
# get_explainer <- function(qc_type){
#   # Use the default explainer if qc_type is NULL, NA,
#   # or not a registered explainer.
#   qc <- ifelse(is.null(qc_type) || is.na(qc_type) || !qc_type %in% names(explainers),"default",qc_type)
#
#   explainers[[qc]]
# }
#
# # results (tibble) |> pmap_df(get_explanation)
# get_explanation <- function(...) {
#   row <- list(...)
#   fun <- get_explainer(row$Qctype)
#   fun(row)
# }

# Explain the underlying checks; Qctype_mapping is the sole list of allowed
# QC types and supplies the missing-value policy for each alias.
explainers <- list(
  list = \(row) paste0("be in [", explain_values(row$ValidValues), "]"),
  length_eq = \(row) paste0("have exactly ", row$ValidValues[[1]], " characters"),
  length_le = \(row) paste0("have at most ", row$ValidValues[[1]], " characters"),
  datetime = \(row) "be a valid date/time",
  datebefore = \(row) paste0("be before ", row$ValidValues[[1]]),
  is_na = \(row) "be missing or blank",
  not_na = \(row) "be populated",
  isnumeric = \(row) "be numeric",
  default = \(row) {
    qc <- row$Qctype
    if (length(qc) != 1L || is.na(qc)) qc <- "<missing>"
    paste0(qc, " is not configured for explanations.")
  }
)

# Use dictionary labels when available, retaining literal values otherwise.
explain_values <- function(values) {
  values <- as.character(unlist(values, use.names = FALSE))
  labels <- values
  if (!is.null(.dict_store$dictionary)) {
    definitions <- lookup(values)
    known <- !is.na(definitions) & nzchar(definitions)
    labels[known] <- definitions[known]
  }
  labels[is.na(values)] <- "<missing>"
  labels[!is.na(values) & values == ""] <- "<blank>"
  if (!length(labels)) return("<missing>")
  paste(labels, collapse = ", ")
}

explain_rule <- function(row, mapping, explain_check) {
  expectation <- explain_check(row)
  if (mapping$is_na_ok && mapping$check_type != "is_na") {
    expectation <- paste0(expectation, " or be missing or blank")
  } else if (!mapping$is_na_ok && mapping$check_type != "not_na") {
    expectation <- paste0(expectation, " and be populated")
  }

  explanation <- paste0(
    explain_values(row$ConceptID), " should ", expectation,
    "; observed: ", explain_values(row[[row$ConceptID]]), "."
  )

  columns <- row$cross_columns
  keep <- !is.na(columns) & nzchar(columns)
  if (any(keep)) {
    conditions <- purrr::map2_chr(columns[keep], row$cross_values[keep], \(column, values) {
      if (length(values) == 1L && isTRUE(values[[1]] == "*")) {
        paste0(explain_values(column), " is populated")
      } else {
        paste0(explain_values(column), " is in [", explain_values(values), "]")
      }
    })
    explanation <- paste0("If ", paste(conditions, collapse = " and "), ", then ", explanation)
  }
  explanation
}

get_explainer <- function(qc_type) {
  if (length(qc_type) != 1L || is.na(qc_type)) return(explainers$default)
  mapping <- Qctype_mapping[[tolower(qc_type)]]
  if (is.null(mapping)) return(explainers$default)

  explain_check <- explainers[[mapping$check_type]]
  if (!is.function(explain_check)) {
    stop("No explainer for check type: ", mapping$check_type)
  }
  function(row) explain_rule(row, mapping, explain_check)
}

# purrr::pmap_chr(results, get_explanation)
get_explanation <- function(...) {
  row <- list(...)
  fun <- get_explainer(row$Qctype)
  fun(row)
}


#' Add explanations to quality-control results
#'
#' Describe each quality-control rule and its observed value, including any
#' cross-column conditions and the rule's missing-value policy. Dictionary
#' definitions are used when available; otherwise, literal values are shown.
#'
#' @param df A data frame of quality-control results containing `Qctype`,
#'   `ConceptID`, `ValidValues`, `cross_columns`, and `cross_values`, together
#'   with the observed-value columns named by `ConceptID`. Rule values and
#'   cross-column conditions may be stored in list-columns.
#' @return `df` with a character column named `explanation`
#'   containing one explanation per row. An existing column of that name is
#'   replaced.
#' @details Unrecognized or missing quality-control types receive a fallback
#'   message indicating that an explanation is not configured.
#' @seealso [lookup()], [run_qc()]
#' @export
explain <- function(df){
  df |> dplyr::mutate(explanation=purrr::pmap_chr(pick(everything()),get_explanation))
}
