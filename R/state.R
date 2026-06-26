# ===========================================================================
# R/state.R
# Internal helpers for reading and writing the registry.
# ===========================================================================

#' @noRd
package_config_dir <- function() {
  dir <- tools::R_user_dir("ConnectRCode", which = "config")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  dir
}

# the following is stored in the state.json
# config_file... (yaml file holding the various configurations)
# active module. (module1, module2... )
# active environment (prod/stage/dev)
#' @noRd
state_path <- function() file.path(package_config_dir(), "state.json")

#' @noRd
load_state <- function() {
  path <- state_path()
  rm(list=ls(.app_state),envir = .app_state)
  # if the state was not initialized
  # just return..  the env is clean
  if (!file.exists(path)) {
      return(invisible(NULL))
  }

  # otherwise load the the enviroment...
  l=jsonlite::read_json(path)

  # if the user loaded a config.yaml file...
  # reload it now..
  if (!is.null(l$config_file)) {
    .app_state$config_file <- l$config_file
    load_config()
  }

  # if the user selected a module/env
  # reload it now...
  if (!is.null(l$module)){
    .app_state$module <- l$module
    .app_state$environment <- l$environment
    activate()
  }
  invisible(NULL)
}
#' @noRd
write_state <- function(){

  save<-intersect(c("config_file", "module","environment"),ls(.app_state))

  l <- save  |> purrr::reduce(\(a,x) {
    a[[x]] <- get(x,envir = .app_state)
    a
  },.init=list())

  jsonlite::write_json(l,path = state_path(),auto_unbox = T)
  invisible(NULL)
}

state <- function(){
  .app_state
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
activate <- function(module=NULL, env=NULL) {
  save = !is.null(module) && !is.null(env)
  if (is.null(module)) { module = .app_state$module }
  if (is.null(env)) { env = .app_state$environment }

  cfg <- .app_state$config
  if (is.null(cfg)){
    stop("The study YAML configs where not loaded.  Please run `load_config(yaml)`")
  }
  if (!module %in% names(cfg$modules))
    stop("Module '", module, "' not found.")
  if (!env %in% names(cfg$modules[[module]]$envs))
    stop("Env '", env, "' not found in module '", module, "'. ",
         "Available: ", paste(names(cfg$modules[[module]]$envs), collapse = ", "))

  ## the env=prod/stage/dev
  .app_state$environment <- env
  ## the module is module1, module2
  .app_state$module      <- module
  .app_state$rules_file  <- normalizePath(
    file.path(cfg$rules_dir, cfg$modules[[module]]$rules_file),
    mustWork = FALSE
  )

  .app_state$rules   <- load_rules()

  if (save) { write_state() }
  message("Active module set to '", module, " '('", env, "').")
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
active_env <- function() {
  if (!exists("module",envir = .app_state)){
    message("no active module, use activate(module,environment)")
  }
  module = .app_state$module
  env = .app_state$environment
  rules_file = .app_state$rules_file

  message("module: ",module,"\nenvironment: ",env,"\nrules :",rules_file)

  project <- .app_state$config$envs[[env]]$project
  billing <- .app_state$config$modules[[module]]$envs[[env]]$billing %||% project
  dataset <- .app_state$config$modules[[module]]$envs[[env]]$dataset
  table   <- .app_state$config$modules[[module]]$envs[[env]]$table
  joins   <- .app_state$config$modules[[.app_state$module]]$joins
  message("project: ",project,"\n\tbilling: ",billing,"\n\tdataset: ",dataset,"\n\ttable:",table)
  if (!is.null(joins)) print(joins)
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

  if (!bigrquery::bq_has_token()) bigrquery::bq_auth()

  module = .app_state$module
  env = .app_state$environment

  project <- .app_state$config$envs[[env]]$project
  billing <- .app_state$config$modules[[module]]$envs[[env]]$billing %||% project
  dataset <- .app_state$config$modules[[module]]$envs[[env]]$dataset
  table   <- .app_state$config$modules[[module]]$envs[[env]]$table
  joins   <- .app_state$config$modules[[module]]$joins
  wheres  <- .app_state$config$modules[[module]]$where

  con <- bigrquery::dbConnect(
    bigrquery::bigquery(),
    project = project,
    dataset = dataset,
    billing = billing
  )

  tbl <- dplyr::tbl(con, table)

  #---------------------------------------------------------
  # Join the current table
  # with all the "joined tables (only taking the needed columns)
  #---------------------------------------------------------
  for (j in joins) {
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

  #---------------------------------------------------------
  # Handle global wheres
  # Do it now while I have all the variables.
  #---------------------------------------------------------
  tbl <- purrr::reduce(names(wheres),
                       .init=tbl,
                       .f=\(x,nm){
                         dplyr::filter(x,!!rlang::sym(nm) %in% local(!!wheres[[nm]]))
                       })

  tbl
}


.app_state <- new.env(parent = emptyenv())
#' @noRd
#' Create an active environment to hold the state.
.onAttach <- function(libname,pkgname){
  tryCatch(load_state(), error = function(e) NULL)
}

rules <- function(){
  .app_state$rules
}
