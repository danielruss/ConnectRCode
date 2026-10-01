### THIS IS CONNECT SPECIFIC..
### NEEDS TO BE REMOVED FROM EXPLAIN.R

dict <- c(
  "512820379" = "Recruit Type",
  "821247024" = "Verif Status",
  "486306141" = "Active",
  "180583933" = "Not Active",
  "854703046" = "Passive",
  "197316935" = "Verified",
  "875007964" = "Not yet verified",
  "219863910" = "Cannot be verified",
  "160161595" = "Outreach timed out",
  "667474224" = "Campaign Type",
  "512820379" = "Recruit Type",
  "348281054" = "Screening appointment",
  "926338735" = "Random",
  "324692899" = "Non-screening appointment",
  "351257378" = "Demographic Group",
  "647148178" = "Aging out of study",
  "834544960" = "Geographic group",
  "682916147" = "Post-Screening Selection",
  "153365143" = "Technology adapters",
  "663706936" = "Low-income/health professional shortage areas",
  "208952854" = "Research Registry",
  "296312382" = "Pop up",
  "181769837" = "Other",
  "398561594" = "None of these apply",
  "130709488" = "Urgent Care Visit"
)
lookup <- function(x){
  x <- sub(".*_([0-9]+)$", "\\1", x)
  dict[x] 
}

explainers <- list(
  crossvalid = \(row) {
    lup_cid <- lookup(row$ConceptID)
    lup_vv  <- lookup(row$ValidValue)
    lup_1 <- lookup(row[[row$ConceptID]])
    purrr::map2_chr(row$cross_column,row$cross_value, \(xcol,xvalue){
      lup_xcol <- lookup(xcol)
      lup_xval <- lookup(xvalue)
      paste0("If ",lup_xcol," is in [",paste(lup_xval,collapse = ", "),"] then ",
      lup_cid," should be in [",paste(lup_vv,collapse = ", "),"] but it is ",lookup(row[[row$ConceptID]]) 
      )
    }) |> paste(collapse = "\n")
  }, 
  default = \(row) {
    paste0(row$Qctype, " is not configured for explainations ....")
  }
)


get_explainer <- function(qc_type){
  # Use the default explainer if qc_type is NULL, NA,
  # or not a registered explainer.
  qc <- ifelse(is.null(qc_type) || is.na(qc_type) || !qc_type %in% names(explainers),"default",qc_type)

  explainers[[qc]]
}

# results (tibble) |> pmap_df(get_explanation)
get_explanation <- function(...) {
  row <- list(...)
  fun <- get_explainer(row$Qctype)
  fun(row)
}