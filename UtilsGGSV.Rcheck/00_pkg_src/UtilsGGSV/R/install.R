.utilsggsv_dep_install <- function(dep) {
  # don't install any already available
  # (we're not trying to force the latest version)
  dep_required <- dep[
    vapply(dep, function(x) !requireNamespace(x, quietly = TRUE), logical(1))
  ]
  if (length(dep_required) == 0L) {
    return(invisible(TRUE))
  }
  for (i in seq_along(dep_required)) {
    if (.utilsggsv_renv_detect()) {
      .utilsggsv_dep_install_rscript(dep_required[[i]])
    } else {
      if (grepl("^\\w+/\\w+", gsub("\\.", "", dep_required[[i]]))) {
        if (!requireNamespace("remotes", quietly = TRUE)) {
          utils::install.packages("remotes")
        }
        remotes::install_github(dep_required[[i]])
      } else {
        utils::install.packages(dep_required[[i]])
      }
    }
  }
  invisible(TRUE)
}

.utilsggsv_renv_detect <- function() {
  .utilsggsv_renv_lockfile_path_get() |> file.exists()
}

.utilsggsv_renv_lockfile_path_get <- function() {
  # taken from renv:::renv_paths_lockfile.
  # we're not finding the lockfile when testing on
  # GH to add dependencies, so we're gonna our
  # own .dir_proj_get to get the project root
  override <- Sys.getenv("RENV_PATHS_LOCKFILE", unset = NA)
  if (!is.na(override)) {
    last <- substr(override, nchar(override), nchar(override))
    if (last %in% c("/", "\\")) {
      override <- paste0(override, "renv.lock")
    }
    return(override)
  }
  .utilsggsv_renv_lockfile_path_get_non_override()
}

.utilsggsv_renv_lockfile_path_get_non_override <- function() {
  tryCatch(
    file.path(
      rprojroot::find_root(rprojroot::has_file("renv.lock")),
      "renv.lock"
    ),
    error = function(e) "renv.lock"
  )
}

.utilsggsv_dep_install_rscript <- function(dep) {
  cmd_txt <- paste0(
    "-e '",
    "renv::install(",
    paste0('"', dep, '"'),
    ", prompt = FALSE)'"
  )
  system2(
    .utilsggsv_path_rscript_get(),
    args = cmd_txt, stdout = FALSE
  )
}

.utilsggsv_path_rscript_get <- function() {
  file.path(R.home("bin"), "Rscript")
}
