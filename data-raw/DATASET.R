## code to prepare `QCType_mapping` dataset goes here
raw_csv <- "
QCType|check_type|is_na_ok
valid|list|FALSE
na or valid|list|TRUE
crossvalid|list|FALSE
na or crossvalid|list|TRUE
crossvalidnotna|not_na|FALSE
crossvalid is populated|not_na|FALSE
na or crossvalid is not populated|is_na|TRUE
crossvalid equal to char()|length_eq|FALSE
na or equal to char()|length_eq|TRUE
equal to or less than char()|length_le|TRUE
na or equal to or less than char()|length_le|TRUE
crossvalid equal to or less than char()|length_le|TRUE
crossvaliddate|datetime|FALSE
na or datetime|datetime|TRUE
na or date|datetime|TRUE
na or crossvalid datetime|datetime|TRUE
na or valid before date|datebefore|TRUE
"
df <- read.csv(text = raw_csv,sep = "|")
Qctype_mapping <- df |> purrr::pmap(\(check_type,is_na_ok,...) list(check_type=check_type,is_na_ok=as.logical(is_na_ok)) )
names(Qctype_mapping) <- df$QCType

usethis::use_data(Qctype_mapping, overwrite = TRUE)
