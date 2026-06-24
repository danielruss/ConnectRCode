## need this for the R checker.
## these are column names for the rules.
utils::globalVariables(c("ValidValues","cross_columns","cross_values"))

#' Load rules from the active environment file
#'
#' @description
#' A function that creates a tibble containing all the individual
#' rules.  These should be in an Excel file
#'
#'
#' @returns a tibble of rules
#' @export
#'
load_rules <- function(){
  get_cols <- function(...){
    x <- trimws(c(...))
    x[!is.na(x)]
  }

  if (is.null(.app_state$rules_file)){
    stop("the rules file is not defined.  Did you forget to activate() ?")
  }

  readxl::read_excel(.app_state$rules_file) |>
    dplyr::mutate(
      rule_id = as.character(rule_id),
      dplyr::across(dplyr::where(is.character),\(x){
        x <- stringr::str_trim(x)
        dplyr::na_if(x, "")
        })
      ) |>
    dplyr::mutate(
      cross_columns= purrr::pmap(dplyr::pick( dplyr::matches("CrossVariableConceptID\\d+$")),get_cols),
      cross_values = purrr::pmap(dplyr::pick( dplyr::matches("CrossVariableConceptID\\d+Value$")),get_cols),
      ValidValues = strsplit(ValidValues,",\\s*"),
      Qctype = gsub("crossvalid\\d+","crossvalid",Qctype,ignore.case = TRUE)
    ) |>
    dplyr::filter(!is.na(Qctype)) |>
    dplyr::relocate(c(cross_columns,cross_values),.after=ValidValues) |>
    dplyr::select(-dplyr::matches("CrossVariableConceptID\\d+($|Value$)"))
}

#' reload rules
#'
#' @description
#' A function reloads rules in case you changed them
#'
#'
#' @returns a tibble of rules
#' @export
#'
reload_rules <- function(){
  .app_state$rules   <- load_rules()
}


force_rules <- function(df){
  .app_state$rules   <- df
}
