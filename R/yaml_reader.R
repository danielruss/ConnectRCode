# ===========================================================================
# R/yaml_reader.R
# Validates the YAML config and stores its path in the registry.
# ===========================================================================

#' Load the package config from a YAML file
#'
#' Validates the YAML and stores its path in the registry.
#' Everything else is read from the YAML at runtime.
#'
#' @param path Path to the YAML config file.
#' @export
load_config <- function(path) {
  ## if you gave me a path, I need save it later.
  save_path=TRUE
  if (missingArg(path)){
    path = .app_state$config_file
    save_path = FALSE
  }
  path = path %||% .app_state$config_file

  if (is.null(path)) stop("path is null")
  path <- normalizePath(path, mustWork = FALSE)
  if (!file.exists(path)) stop("Config file not found: ", path)

  cfg <- yaml::read_yaml(path)

  # --- Validate top-level fields ---
  missing <- setdiff(c("rules_dir", "envs", "modules"), names(cfg))
  if (length(missing) > 0)
    stop("Config missing required fields: ", paste(missing, collapse = ", "))

  rules_dir <- normalizePath(cfg$rules_dir, mustWork = FALSE)
  if (!dir.exists(rules_dir)) stop("rules_dir not found: ", rules_dir)

  # --- Validate envs ---
  for (env in names(cfg$envs)) {
    if (is.null(cfg$envs[[env]]$project))
      stop("Env '", env, "' missing required field: project")
  }

  # --- Validate modules ---
  known_envs <- names(cfg$envs)
  for (mod in names(cfg$modules)) {
    m <- cfg$modules[[mod]]

    if (is.null(m$rules_file))
      stop("Module '", mod, "' missing required field: rules_file")

    rules_file <- file.path(rules_dir, m$rules_file)
    if (!file.exists(rules_file))
      stop("Module '", mod, "' rules file not found: ", rules_file)

    if (is.null(m$envs))
      stop("Module '", mod, "' missing required field: envs")

    unknown_envs <- setdiff(names(m$envs), known_envs)
    if (length(unknown_envs) > 0)
      stop("Module '", mod, "' has unknown environment(s): ",
           paste(unknown_envs, collapse = ", "),
           ". Known: ", paste(known_envs, collapse = ", "))

    for (env in names(m$envs)) {
      e <- m$envs[[env]]
      if (is.null(e$dataset)) stop("Module '", mod, "' env '", env, "' missing: dataset")
      if (is.null(e$table))   stop("Module '", mod, "' env '", env, "' missing: table")
    }

    # --- Validate joins ---
    if (!is.null(m$joins)) {
      for (i in seq_along(m$joins)) {
        j       <- m$joins[[i]]
        missing <- setdiff(c("table", "key", "columns"), names(j))
        if (length(missing) > 0)
          stop("Module '", mod, "' join ", i, " missing: ", paste(missing, collapse = ", "))
      }
    }
  }

  # --- Store only the path in the registry ---
  .app_state$config_file <- path
  .app_state$config <- cfg
  #...  need to update the config_file
  if (save_path){
    write_state()
  }

  configs()
  invisible(path)
}

configs <- function(){
  message("Loaded config: ", .app_state$config_file)
  message("  Environments : ", paste(names(.app_state$config$envs), collapse = ", "))
  message("  Modules      : ", paste(names(.app_state$config$modules), collapse = ", "))
}
