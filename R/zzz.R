.onLoad <- function(libname,pkgname){
  # Load bigrquery first so our SQL translation is registered afterward.
  loadNamespace("bigrquery")
  register_bq_translation()

  if (requireNamespace("duckdb", quietly = TRUE)) {
    register_duckdb_translation()
  }
}
