#' @title Set R Ghostscrip env variable
#'
#' @description Set \code{R_GSCMD} environment variable
#' that must be set for Ghostscript to be used.
#'
#' @param gs_version character. Version to use.
#'
#' @export
#'
#' @return \code{invisible(TRUE)}.
set_env_var_r_gscmd <- function(gs_version = "9.54.0"){
  if(!is.character(gs_version)){
    stop("gs_version must be of class character")
  }
  path_gs_exe <- file.path(
    "C:/Program Files (x86)", "gs",
    paste0('gs', gs_version), 'bin', 'gswin32c.exe'
    )
  if(!file.exists(path_gs_exe)){
    stop("path to Ghostscript executable not valid")
  }
  Sys.setenv(R_GSCMD = path_gs_exe)
  invisible(TRUE)
}

#' @export
set_gs_var <- set_env_var_r_gscmd
