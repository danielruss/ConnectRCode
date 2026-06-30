is_ymd <- function(x){
  !is.na(strptime(as.character(x),format = "%Y%m%d"))
}
is_ym <- function(x){
  !is.na(strptime(as.character(x),format = "%Y%m"))
}

# This adds sql translations to various connection types that we use
register_bq_translation <- function(){
  # 1. Define the SQL logic for BigQuery
  bigquery_is_timestamp = function(x) {
    dbplyr::build_sql("SAFE_CAST(", x, " AS TIMESTAMP) IS NOT NULL")
  }
  bigquery_is_ymd = function(x){
    dbplyr::build_sql("SAFE.PARSE_DATE('%Y%m%d', SAFE_CAST(", x, " AS STRING)) IS NOT NULL")
  }
  bigquery_is_ym = function(x){
    dbplyr::build_sql("SAFE.PARSE_DATE('%Y%m', SAFE_CAST(", x, " AS STRING)) IS NOT NULL")
  }
  bigquery_str_length <- function(x){
    dbplyr::build_sql("LENGTH(COALESCE(SAFE_CAST(", x, " AS STRING), ''))")
  }
  # 2. Register it specifically for the BigQuery connection class
  s3_method <- function(con){
    dbplyr::sql_variant(
      scalar = dbplyr::sql_translator(
        .parent = dbplyr::base_scalar,
        is.timestamp = bigquery_is_timestamp,
        is.datetime = bigquery_is_timestamp,
        str_length = bigquery_str_length,
        is_ymd = bigquery_is_ymd,
        is_ym = bigquery_is_ym
      ),
      aggregate = dbplyr::base_agg,
      window = dbplyr::base_win
    )
  }
  registerS3method("sql_translation", "BigQueryConnection", s3_method,
                   asNamespace("dbplyr"))
}

register_duckdb_translation <- function(){
  # 1. Define the SQL logic for DuckDB
  # DuckDB uses TRY_CAST; returns NULL if conversion fails
  duckdb_is_timestamp <- function(x) {
    dbplyr::build_sql("TRY_CAST(", x, " AS TIMESTAMP) IS NOT NULL")
  }
  duckdb_is_ymd = function(x){
    dbplyr::build_sql("TRY_STRPTIME(TRY_CAST(", x, " AS VARCHAR), '%Y%m%d') IS NOT NULL")
  }
  duckdb_is_ym = function(x){
    dbplyr::build_sql("TRY_STRPTIME(TRY_CAST(", x, " AS VARCHAR), '%Y%m') IS NOT NULL")
  }
  duckdb_str_length <- function(x) {
    dbplyr::build_sql("LENGTH(COALESCE(TRY_CAST(", x, " AS VARCHAR), ''))")
  }

  # 2. Register it specifically for the DuckDB connection class
  # Note: DuckDB's connection class is 'duckdb_connection'
  s3_method <- function(con){
    dbplyr::sql_variant(
      scalar = dbplyr::sql_translator(
        .parent = dbplyr::base_scalar,
        is.timestamp = duckdb_is_timestamp,
        is.datetime = duckdb_is_timestamp,
        is_ymd = duckdb_is_ymd,
        is_ym = duckdb_is_ym,
        str_length = duckdb_str_length
      ),
      aggregate = dbplyr::base_agg,
      window = dbplyr::base_win
    )
  }

  registerS3method("sql_translation", "duckdb_connection", s3_method,
                   asNamespace("dbplyr"))
}
