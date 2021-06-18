#' @title Manage axis limits
#'
#' @param p object of class 'ggplot'. Limits are adjuste for this plot.
#' @param expand_limits list. If not \code{NULL},
#' then it is (effectively) passed onto \code{ggplot2::expand_limits} to
#' ensure that certain values are included in the plot (such as, for example, 0
#' if that is the minimum value possible but it may not be plotted). If not named, then
#' must consist of one numeric vector that will then force all values in the numeric value
#' to be included in the plot. If named, then must have names \code{x} and/or \code{y},
#' with the elements again being numeric vectors that must be included in plot.
#' @param axis_limits_equal logical. If \code{TRUE}, then the ranges on the x- and y-axes
#' must be equal. Effectively applied after expand_grid is applied. Default is \code{FALSE}.
#'
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' data('cars', package = 'datasets')
#' library(ggplot2)
#' p <- ggplot(cars, aes(speed, dist)) +
#'   geom_point()
#'
#' axis_limits(
#'   p,
#'   axis_limits_equal = TRUE)
#'
#' # both axes
#' axis_limits(
#'   p,
#'   expand_limits = list(200))
#' # x only
#' axis_limits(
#'   p,
#'   expand_limits = list(x = 75))
#' # y only
#' axis_limits(
#'   p,
#'   expand_limits = list(y = 200))
#' # lower and upper expansion
#' axis_limits(
#'   p,
#'   expand_limits = list(y = c(-50, 200),
#'                        x = c(-10, 75)))
#'
#' # note that when fixing range and expanding, range is fixed
#' # after expansions are applied, so effectively the larger expansions apply to both.
#' # compare the following output to the previous output:
#' axis_limits(
#'   p,
#'   expand_limits = list(y = c(-50, 200),
#'                        x = c(-10, 75)),
#'   axis_limits_equal = TRUE)
axis_limits <- function(p, expand_limits = NULL, axis_limits_equal = FALSE){

  # initial check
  # ------------------------

  if(!is.logical(axis_limits_equal)){
    stop("axis_limits_equal must be logical")
  }

  # do nothing
  if(is.null(expand_limits) && !axis_limits_equal) return(p)

  # checks
  # ----------------------------

  if(!identical(class(p),
                c("gg", "ggplot"))){
    stop("p must be of class c('gg', 'ggplot')")
  }

  if(!is.null(expand_limits)){
    if(!is.list(expand_limits)){
      stop("expand_limits must be a list (if not NULL)")
    }
    if(length(expand_limits) == 2 && is.null(names(expand_limits))){
      stop("expand_limits must be named if of length 2")
    }
    if(length(expand_limits) > 2){
      stop("expand_limits must have length 1 or 2 (if not NULL)")
    }
    if(!is.null(names(expand_limits))){
      if(length(setdiff(names(expand_limits), c('x', 'y'))) > 0){
        stop("expand_limits must have names of 'x' and/or 'y' (if named)")
      }
    }
    class_input <- purrr::map_lgl(expand_limits, is.numeric) %>% all()
    if(!class_input){
      stop("input to expand_limits must be numeric (if expand_limits not NULL)")
    }
  }

  # prep
  # -------------------



  # ===================
  # adjustments
  # ===================

  # calc ranges in advance if needed
  # --------------------
  if(axis_limits_equal){
    plot_tbl <- p$data
    x_var <- as.character(rlang::get_expr(p$mapping$x))
    y_var <- as.character(rlang::get_expr(p$mapping$y))
    range_x <- range(plot_tbl[[x_var]])
    range_y <- range(plot_tbl[[y_var]])
    range <- c(min(range_x[1], range_y[1]),
               c(max(range_x[2], range_y[2])))
  }

  # tidy expand_limits if provided
  # ------------------

  # ensure that expand_limits is named if
  # it's specified
  if(!is.null(expand_limits)){
    if(is.null(names(expand_limits))){
      expand_limits <- list(
        x = expand_limits[[1]],
        y = expand_limits[[1]]
      )
    }
    # ensure that expand_limits consists of
    # two sorted (not strictly) variables
    for(i in seq_along(expand_limits)){
      expand_limits[[i]] <- c(min(expand_limits[[i]]),
                              max(expand_limits[[i]]))
    }
  }

  # put expand_limits together with axis_limits_equal,
  # if provided
  if(is.null(expand_limits)){
    # we know now that axis_limits_equal is true
    expand_limits <- list(
      x = range,
      y = range
    )
  } else{
    # axis_limits_equal may or may not be true
    if(axis_limits_equal){
      expand_limits_all <- expand_limits %>%
        unlist()
      lims <- c(min(range, expand_limits_all),
                max(range, expand_limits_all))
      for(i in seq_along(expand_limits)){
        expand_limits[[i]] <- lims
      }
      if(length(expand_limits) == 1){
        nm <- setdiff(c('x', 'y'), names(expand_limits))
        expand_limits %<>%
          append(list(range) %>% setNames(nm))
      }
    }
  }

  expand_limits_arg <- purrr::map_chr(seq_along(expand_limits), function(i){
    vals <- paste0(expand_limits[[i]], collapse = ", ")
    paste0(names(expand_limits)[i], " = c(", vals, ")")
  }) %>%
    paste0(collapse = ", ")

  parse_text <- paste0("p <- p + expand_limits(", expand_limits_arg, ")")
  env <- environment()
  eval(parse(text = parse_text), envir = env)

  p


}
