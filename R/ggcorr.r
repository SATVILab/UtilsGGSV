#' @title Plot scatterplot with CCC and ab-line overlaid
#'
#' @description
#' At present only allows the concordance correlation
#' coefficient.
#' More correlation coefficients will be added in future.
#'
#' @inheritParams add_text_column
#' @inheritParams axis_limits
#' @inheritParams get_trans
#' @param data dataframe. Has columns .
#' @param grp character.
#' Name of column in \code{data} that specifies
#' the variables to compare between.
#' The first level within \code{grp}
#' (i.e. \code{data[[grp]][1]}) is
#' the level that is plotted on the x-axis.
#' @param grp_base character.
#' If not \code{NULL}, then only
#' correlations between this level
#' within \code{data[[grp]]} and
#' other levels are displayed.
#' If \code{NULL}, then all
#' two-way correlations ae displayed.
#' Default is \code{NULL}.
#' @param grp_x character.
#' If \code{NULL}, then
#' this level of \code{grp}
#' is the level plotted along the
#' x-axis.
#' Default is \code{NULL}.
#' @param y character.
#' Name of column in \code{data} that specifies
#' the values/measurements/responses
#' being correlated between groups.
#' @param id character.
#' Name of column in \code{data} that specifies
#' the subject from which multiple measurements were taken.
#' @param label_id logical.
#' If \code{TRUE}, then points labels as given
#' by \code{data[[id]]} are displayed using
#' \code{ggrepel::geom_text_repel}.
#' Default is \code{FALSE}.
#' @param label_id_size. Numeric.
#' Size of labels.
#' Default is \code{3}.
#' @param thm object of class \code{c("theme", "gg")}.
#' Specifies \code{ggplot2} theme.
#' Default is \code{cowplot::theme_cowplot()}.
#' @param grid object of class \code{c("theme", "gg")}.
#' Specifies background grid.
#' If not using \code{cowplot::theme_cowplot()}
#' for theme, then likely best to set to NULL.
#' Default is \code{cowplot::background_grid(major = "xy")}.
#' @param grp_to_col character vector.
#' Specifies colours for elements in \code{grp}.
#' Elements are colours.
#' If \code{named}, then the names specify the
#' level within \code{grp} to which the colours
#' are matched.
#' If \code{NULL} and only two groups are compared,
#' then all points are black.
#' If \code{NULL} and there are more than two groups
#' compared, then defaults to palette \code{Set1}
#' of the RColorBrewer package.
#' Default is \code{NULL}.
#' @param abline logical.
#' If \code{TRUE}, then the y=x line is plotted
#' in colour "gray85".
#' @param smooth_method.
#' Smoothing method to pass to
#' \code{geom_smooth}.
#' Default is \code{"lm"}.
#' If \code{NULL}, then
#' no smoothed line is drawn.
#' @param trans character or trans object.
#' Specifies scaling of y- and x-axes.
#' If class is character, then it is converted to a trans object.
#' Adds "root_cube", "root_fourth", "root_fifth" and
#' "asinh" transformations, as well as "sqrt" transformation
#' that allows plotting of lines at zero.
#' If class is a trans object, then it is returned as is.
#' Default is \code{"identity"}.
#' @export
#' @examples
#' response_vec_a <- rnorm(5)
#'   response_tbl <- data.frame(
#'   group = rep(letters[1:3], each = 5),
#'   response = c(
#'     response_vec_a,
#'     response_vec_a * 1.2 + rnorm(5, sd = 0.2),
#'     response_vec_a * 2 + rnorm(5, sd = 2)
#'   ),
#'  pid = rep(paste0("id_", 1:5), 3)
#' )
#' ggcorr(
#'  data = response_tbl,
#'  grp = "group",
#'  y = "response",
#'  id = "pid"
#' )
ggcorr <- function(data,
                   grp,
                   grp_base = NULL,
                   grp_x = NULL,
                   y,
                   id,
                   label_id = FALSE,
                   label_id_size = 3,
                   trans = "identity",
                   coord = c(0.05, 0.95),
                   skip = 0.05,
                   font_size = 10,
                   hjust = 0,
                   vjust = 0.5,
                   limits_expand = NULL,
                   limits_equal = FALSE,
                   thm = cowplot::theme_cowplot(),
                   grid = cowplot::background_grid(major = "xy"),
                   grp_to_col = NULL,
                   abline = TRUE,
                   smooth = TRUE,
                   smooth_method = "lm") {

  if (!requireNamespace("cccrm", quietly = TRUE)) {
    install.packages("cccrm")
  }
  cn_vec <- colnames(data)
  cn_vec[which(cn_vec == grp)] <- ".grp"
  cn_vec[which(cn_vec == y)] <- ".y"
  cn_vec[which(cn_vec == id)] <- ".id"
  colnames(data) <- cn_vec
  trans <- UtilsGGMR::get_trans(trans)

  grp_vec <- unique(data$`.grp`)

  if (!is.null(grp_base)) {
    grp_vec_alt <- setdiff(grp_vec, grp_base)
    combn_mat <- matrix(
      c(
        rep(grp_base, length(grp_vec_alt)),
        grp_vec_alt
      ),
      byrow = TRUE
    )
  } else {
    if (!is.null(grp_x)) grp_vec <- c(grp_x, setdiff(grp_vec, grp_x))
    combn_mat <- combn(grp_vec, 2)
  }

  results_tbl <- purrr::map_df(seq_len(ncol(combn_mat)), function(i) {
    grp_vec_curr <- combn_mat[, i]
    data_curr <- data %>%
      dplyr::filter(.grp %in% grp_vec_curr)
    corr <- suppressWarnings(cccrm::cccUst(
      dataset = data_curr,
      ry = ".y",
      rmet = ".grp",
      cl = 0.95
    ))[1:3]
    corr <- corr %>% signif(2)
    tibble::tibble(
      g1 = grp_vec_curr[1],
      g2 = grp_vec_curr[2],
      est = corr[1],
      lb = corr[2],
      ub = corr[3]
    ) %>%
      dplyr::mutate(
        txt = paste0(
          g1, " vs ", g2, ": ", corr[1],
          " (", corr[2], "; ", corr[3], ")"
        )
      )
  })

  match_elem <- grp_vec[[1]]

  plot_tbl_raw <- purrr::map_df(
    setdiff(grp_vec, grp_vec[1]),
    function(grp_alt) {
      data %>%
        dplyr::filter(.grp == grp_vec[1]) %>%
        dplyr::select(.grp, .id, .y) %>%
        dplyr::rename(
          x = .y,
          grp_x = .grp
        ) %>%
        dplyr::full_join(
          data %>%
            dplyr::filter(.grp == grp_alt) %>%
            dplyr::select(.grp, .id, .y) %>%
            dplyr::rename(
              y = .y,
              grp_y = .grp
            ),
          by = ".id"
        ) %>%
        dplyr::select(.id, grp_x, grp_y, x, y)
    }
  )

  p <- ggplot(
    plot_tbl_raw,
    aes(x = x, y = y, col = grp_y)
  ) +
    thm +
    grid

  p <- p +
    geom_point(
      data = plot_tbl_raw
    ) +
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )

  if (!is.null(grp_to_col)) {
    if (is.null(names(grp_to_col))) {
      if (length(grp_to_col) == 1) {
        grp_to_col <- setNames(
          rep(grp_to_col, length(unique(plot_tbl_raw$grp_y))),
          unique(plot_tbl_raw$grp_y)
        )
      } else {
        if (length(grp_to_col) < length(grp_vec) - 1) {
          stop(paste0("If more than length one and not named,
          the number of elements in grp_to_col
          must be at least as many as the number of groups
          less one
          in data[[grp]]"))
        }
        grp_to_col <- setNames(
          grp_to_col[seq_len(length(unique(plot_tbl_raw$grp_y)))],
          unique(plot_tbl_raw$grp_y)
        )
      }
    }
    p <- p +
      scale_colour_manual(
        values = grp_to_col,
        limits = unique(plot_tbl_raw$grp_y)
      )
  } else {
    p <- p +
      switch(as.character(length(unique(plot_tbl_raw$grp_y))),
        "1" = scale_colour_manual(
          values = setNames(
            rep("black", length(unique(plot_tbl_raw$grp_y))),
            length(unique(plot_tbl_raw$grp_y))
          )
        ),
        scale_colour_brewer(palette = "Set1")
      )
  }

  if (length(grp_vec) <= 2) {
    p <- p + guides(colour = "none")
  }

  if (is.null(limits_expand)) {
    limits_expand <- list(
      x = range(plot_tbl_raw$x),
      y = range(plot_tbl_raw$y)
    )
  }

  p <- axis_limits(
    p = p,
    limits_expand = limits_expand,
    limits_equal = limits_equal
  )

  p <- add_text_column(
    p = p,
    x = p$layers[[2]]$data$x,
    y = p$layers[[2]]$data$y,
    trans = trans,
    text = results_tbl$txt,
    coord = coord,
    skip = skip,
    hjust = hjust,
    vjust = vjust
  )

  p <- p +
    scale_x_continuous(trans = trans) +
    scale_y_continuous(trans = trans)

  p <- p + ggplot2::labs(x = match_elem)
  if (ncol(combn_mat) == 1) {
    p <- p +
      labs(y = combn_mat[2, 1]) +
      theme(legend.position = "none")
  } else {
    p <- p + labs(y = "Comparison group")
  }

  if (abline) {
    p <- p + geom_abline(
      intercept = 0, slope = 1, linetype = "solid",
      colour = "gray85"
    )
  }

  if (smooth) {
    p <- p +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE)
  }

  if (label_id) {
    p <- p +
      ggrepel::geom_text_repel(
        aes(label = .id),
        size = label_id_size
      )
  }

  p
}
