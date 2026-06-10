# ===========================================================================
# R/registry_helpers.R
# Internal helpers for reading and writing the registry.
# ===========================================================================



#' @noRd
package_config_dir <- function() {
  dir <- tools::R_user_dir("ConnectRCode", which = "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

#' @noRd
registry_path <- function() file.path(package_config_dir(), "registry.json")

#' @noRd
read_registry <- function() {
  path <- registry_path()
  if (file.exists(path)) {
    jsonlite::read_json(path)
  } else {
    list(config_file = NULL, active = list(module = NULL, env = NULL))
  }
}

#' @noRd
write_registry <- function(reg) {
  jsonlite::write_json(reg, registry_path(), pretty = TRUE, auto_unbox = TRUE)
}

#' @noRd
read_config <- function() {
  reg <- read_registry()
  if (is.null(reg$config_file))
    stop("No config file found. Run load_config() first.")
  yaml::read_yaml(reg$config_file)
}

#' @noRd
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ===========================================================================
# R/registry.R
# The registry only stores the YAML config file path and the active
# module/env. Everything else is read directly from the YAML at runtime.
#
# Registry structure (registry.json in R_user_dir):
# {
#   "config_file": "~/configs/connectr.yaml",
#   "active": { "module": "mod1", "env": "dev" }
# }
# ===========================================================================

# ---------------------------------------------------------------------------
# Active module/env
# ---------------------------------------------------------------------------

#' Set the active module and environment
#'
#' @param module Module name.
#' @param env    Environment name.
#' @rdname active
#' @export
set_active <- function(module, env) {
  cfg <- read_config()

  if (!module %in% names(cfg$modules))
    stop("Module '", module, "' not found.")
  if (!env %in% names(cfg$modules[[module]]$envs))
    stop("Env '", env, "' not found in module '", module, "'. ",
         "Available: ", paste(names(cfg$modules[[module]]$envs), collapse = ", "))

  .app_state$project    <- cfg
  .app_state$module     <- cfg$modules[[module]]
  .app_state$env        <- cfg$envs[[env]]
  .app_state$rules_file <- normalizePath(
    file.path(cfg$rules_dir, cfg$modules[[module]]$rules_file),
    mustWork = FALSE
  )

  rules <- load_rules()
  .app_state$rules   <- rules

  reg <- read_registry()
  reg$active=list(module=module,env=env)
  write_registry(reg)
  message("Active set to '", module, "' / '", env, "'.")
  invisible(NULL)
}

#' Get the full config for the active (or specified) module/env
#'
#' Merges the global env config (project, billing) with the
#' module env config (dataset, table) and joins.
#'
#' @param module Optional module override.
#' @param env    Optional env override.
#' @return A list with project, billing_project, dataset, table, joins, rules_file.
#' @rdname active
#' @export
get_active_env <- function(module = NULL, env = NULL) {
  reg    <- read_registry()
  cfg    <- read_config()
  module <- module %||% reg$active$module
  env    <- env    %||% reg$active$env

  if (is.null(module) || is.null(env))
    stop("No active module/env. Use set_active().")
  if (!module %in% names(cfg$modules))
    stop("Module '", module, "' not found.")
  if (!env %in% names(cfg$modules[[module]]$envs))
    stop("Env '", env, "' not found in module '", module, "'.")

  global_env <- cfg$envs[[env]]
  module_env <- cfg$modules[[module]]$envs[[env]]
  rules_file <- normalizePath(
    file.path(cfg$rules_dir, cfg$modules[[module]]$rules_file),
    mustWork = FALSE
  )

  list(
    project         = global_env$project,
    billing_project = global_env$billing_project %||% global_env$project,
    dataset         = module_env$dataset,
    table           = module_env$table,
    joins           = cfg$modules[[module]]$joins %||% list(),
    rules_file      = rules_file
  )
}

# ---------------------------------------------------------------------------
# List
# ---------------------------------------------------------------------------

#' List all modules and environments from the config
#'
#' @export
list_modules <- function() {
  reg <- read_registry()
  cfg <- read_config()

  if (length(cfg$modules) == 0L) {
    message("No modules found in config.")
    return(invisible(NULL))
  }

  for (mod in names(cfg$modules)) {
    m <- cfg$modules[[mod]]
    cat(sprintf("\n[%s]  rules: %s\n", mod, m$rules_file))
    for (env in names(m$envs)) {
      e       <- m$envs[[env]]
      g       <- cfg$envs[[env]]
      active  <- identical(reg$active$module, mod) && identical(reg$active$env, env)
      marker  <- if (active) " <--" else ""
      billing <- if (!is.null(g$billing_project))
        paste0(" (billing: ", g$billing_project, ")") else ""
      cat(sprintf("  %-10s %s.%s.%s%s%s\n",
                  env, g$project, e$dataset, e$table, billing, marker))
    }
    if (length(m$joins) > 0) {
      for (j in m$joins) {
        where <- if (!is.null(j$where))
          paste0(" [where: ", paste(names(j$where), j$where, sep = "=", collapse = ", "), "]")
        else ""
        cat(sprintf("  join: %s on %s [%s]%s\n",
                    j$table, j$key,
                    paste(j$columns, collapse = ", "),
                    where))
      }
    }
  }
  invisible(cfg$modules)
}

# ---------------------------------------------------------------------------
# BigQuery connection
# ---------------------------------------------------------------------------

#' Connect to BigQuery using the active (or specified) module/env
#'
#' Calls \code{bigrquery::bq_auth()} if not already authenticated.
#' Returns a pre-joined lazy \code{dplyr} tbl.
#'
#' @param module Optional module override.
#' @param env    Optional env override.
#' @return A lazy \code{dplyr} tbl.
#' @export
bq_connect <- function(module = NULL, env = NULL) {
  cfg <- get_active_env(module, env)

  if (!bigrquery::bq_has_token()) bigrquery::bq_auth()

  con <- bigrquery::dbConnect(
    bigrquery::bigquery(),
    project = cfg$project,
    dataset = cfg$dataset,
    billing = cfg$billing_project
  )

  tbl <- dplyr::tbl(con, cfg$table)

  for (j in cfg$joins) {
    where_exprs <- purrr::map2(
      names(j$where %||% list()),
      j$where %||% list(),
      \(col, val) rlang::expr(!!rlang::sym(col) == !!val)
    )
    join_tbl <- dplyr::tbl(con, j$table)
    if (length(where_exprs) > 0)
      join_tbl <- dplyr::filter(join_tbl, !!!where_exprs)
    join_tbl <- dplyr::select(join_tbl, dplyr::all_of(c(j$key, j$columns)))
    tbl <- dplyr::left_join(tbl, join_tbl, by = j$key)
  }

  tbl
}


.app_state <- new.env(parent = emptyenv())
#' @noRd
#' Create an active environment to hold the state.
.onAttach <- function(libname,pkgname){
  # Initialize empty slots
  .app_state$project    <- NULL
  .app_state$module     <- NULL
  .app_state$rules_file <- NULL
  .app_state$env        <- NULL
  .app_state$rules      <- NULL

  reg <- tryCatch(read_registry(), error = function(e) NULL)
  if (!is.null(reg$active$module)){
    set_active(reg$active$module,reg$active$env)
  }
}

rules <- function(){
  .app_state$rules
}
