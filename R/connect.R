# place connect-specific stuff here:


#' Normalize Connect concept IDs
#'
#' Extract the trailing digits from each ID, including compound IDs such as
#' `d_123_d_456`. Use this function as the `normalize_cid` argument to
#' `set_dict()` when configuring a Connect dictionary.
#'
#' @param cid A vector of concept IDs.
#' @returns A character vector of trailing digits, falling back to the original
#'   ID when no trailing digits are found. Missing inputs remain missing.
#' @examples
#' c4cp_normalize_cid(c("123", "d_123_d_456", "abc", NA_character_))
c4cp_normalize_cid <- function(cid) {
  stringr::str_extract(cid,"\\d+$") %??% as.character(cid)
}

#' Create a Connect operations report
#'
#' Look up site labels using the dictionary configured by `set_dict()` and
#' select the columns needed for the operations report. Unmatched site IDs
#' retain their original IDs.
#'
#' @param df A data frame of QC results containing `rule_id`, `token`,
#'   `Connect_ID`, and `d_827220437` (the Connect site concept).
#' @returns A data frame with columns `rule_id`, `token`, `Connect_ID`, and
#'   `Site`, preserving the input rows.
#' @details A dictionary must be configured with `set_dict()` before calling
#'   this function. Site labels are obtained through `lookup()`, which uses
#'   the configured concept ID normalizer.
make_ops_report <- function(df){
  df |> dplyr::mutate(Site = lookup(d_827220437)) |>
    dplyr::select(rule_id,token,Connect_ID,Site)
}
