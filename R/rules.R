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
  get_cross_pairs <- function(...){
    x <- list(...)


    cross_cols <- grep("^CrossVariableConceptID\\d+$", names(x), value = TRUE)
    cross_vals <- paste0(cross_cols,"Value")

    xcols <- trimws(unlist(x[cross_cols], use.names = FALSE))
    xvals <- trimws(unlist(x[cross_vals], use.names = FALSE))

    col_present <- !is.na(xcols) & nzchar(xcols)
    val_present <- !is.na(xvals) & nzchar(xvals)

    bad_rule <- any(xor(col_present,val_present))
    if (bad_rule) {
      return(list(cross_cols=NA,cross_vals=NA,bad_cross=TRUE))
    }

    keep <- col_present & val_present
    if (!any(keep)){
      return(list(cross_cols=NA,cross_vals=NA,bad_cross=FALSE))
    }

    col_values <- xcols[keep]
    val_values <- stringr::str_split(xvals[keep], "\\s*,\\s*")


    list(
      cross_cols=col_values,
      cross_vals=val_values,
      bad_cross=bad_rule
    )
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
      cross_pairs = purrr::pmap(dplyr::pick( dplyr::matches("CrossVariableConceptID\\d+($|Value$)")),get_cross_pairs),
      cross_values = purrr::map(cross_pairs,\(x) x$cross_vals),
      cross_columns = purrr::map(cross_pairs,\(x) x$cross_cols),
      bad_cross = purrr::map_lgl(cross_pairs,\(x) x$bad_cross),
      ValidValues = strsplit(ValidValues,",\\s*"),
      Qctype = gsub("crossvalid\\d+","crossvalid",Qctype,ignore.case = TRUE)
    ) |>
    dplyr::filter(!is.na(Qctype)) |>
    dplyr::relocate(c(cross_columns,cross_values),.after=ValidValues) |>
    dplyr::select(-c(cross_pairs,dplyr::matches("CrossVariableConceptID\\d+($|Value$)")))
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
