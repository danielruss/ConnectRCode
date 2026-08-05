.onLoad <- function(libname,pkgname){
  # Load bigrquery first so our SQL translation is registered afterward.
  loadNamespace("bigrquery")
  register_bq_translation()
  register_duckdb_translation()
}
