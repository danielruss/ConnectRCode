.onLoad <- function(libname,pkgname){
  register_bq_translation()
  register_duckdb_translation()
}
