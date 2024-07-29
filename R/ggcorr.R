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
#' @param corr_method character vector,
#' of values from \code{"ccc", "pearson", "spearman"} and \code{"kendall"}.
#' Correlation method.
#' Default is spearman correlation coefficient.
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
                   corr_method = "spearman",
                   corr_lab = c(
                    "ccc" = "(Concordance)",
                    "pearson" = "(Pearson)",
                    "spearman" = "(Spearman)",
                    "kendall" = "(Kendall)"
                   ),
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
  .ggcorr_check(corr_method)
  prep_list <- .ggcorr_prep(
    data = data, grp = grp, y = y, id = id,
    grp_base = grp_base, grp_x = grp_x
  )

  results_tbl <- .ggcorr_results_get(
    combn_mat = prep_list$combn_mat, data = prep_list$data,
    corr_method = corr_method, corr_lab = corr_lab
  )

  .ggcorr_plot(
    data = prep_list$data, grp_vec = prep_list$grp_vec,
    grp_to_col = grp_to_col, thm = thm, grid = grid,
    limits_expand = limits_expand, limits_equal = limits_equal,
    trans = trans, results_tbl = results_tbl, coord = coord,
    skip = skip, hjust = hjust, vjust = vjust,
    abline = abline, smooth = smooth, label_id = label_id,
    label_id_size = label_id_size, combn_mat = prep_list$combn_mat
  )

}

# check
# ---------------------
.ggcorr_check <- function(corr_method) {
  .ggcorr_check_corr_method(corr_method)

}

.ggcorr_check_corr_method <- function(x) {
  if (!all(x %in% c("ccc", "pearson", "spearman", "kendall"))) {
    stop(
      "corr_method must be one or more of c('ccc', 'pearson', 'spearman', 'kendall')" # no lint
    )
  }
  if ("ccc" %in% corr_method) {
    .utilsggsv_dep_install("cccrm")
  }
  invisible(TRUE)
}


# prep
# ---------------------
.ggcorr_prep <- function(data,
                         grp,
                         grp_base,
                         grp_x,
                         y,
                         id) {
  data <- .ggcorr_prep_data(data, grp, y, id)

  list(
    data = data,
    trans = .ggcorr_prep_trans(trans),
    combn_mat = .ggcorr_prep_combn_mat(data, grp_base),
    grp_vec = .ggcorr_prep_grp(data, grp_base, grp_x)
  )

}

.ggcorr_prep_data <- function(data,
                               grp,
                               y,
                               id) { 
  cn_vec <- colnames(data)
  cn_vec[which(cn_vec == grp)] <- ".grp"
  cn_vec[which(cn_vec == y)] <- ".y"
  cn_vec[which(cn_vec == id)] <- ".id"
  colnames(data) <- cn_vec
  data
}

.ggcorr_prep_trans <- function(trans) UtilsGGSV::get_trans(trans)


.ggcorr_prep_grp_init <- function(data) {
  grp_vec <- unique(data$`.grp`)
  if (length(grp_vec) < 2) {
    stop("data must have at least two levels in grp")
  }
  grp_vec
}
.ggcorr_prep_combn_mat <- function(data,
                                   grp_base) {
  if (!is.null(grp_base)) {
    return(.ggcorr_prep_combn_mat_base(data, grp_base))
  }
  grp_init <- .ggcorr_prep_grp_init(data)
  combn(grp_init, 2)
}
.ggcorr_prep_combn_mat_base <- function(data,
                                        grp_base) {
  grp_vec <- .ggcorr_prep_grp_init(data)
  grp_vec_alt <- setdiff(grp_vec, grp_base)
  matrix(
    c(
      rep(grp_base, length(grp_vec_alt)),
      grp_vec_alt
    ),
    byrow = TRUE,
    ncol = length(grp_vec_alt)
  )
}
.ggcorr_prep_grp <- function(data,
                             grp_base,
                             grp_x) {
  grp_vec_init <- .ggcorr_prep_grp_init(data)
  if (!is.null(grp_base) || is.null(grp_x)) {
    return(grp_vec_init)
  }
  c(grp_x, setdiff(grp_vec_init, grp_x))
}

# results
# ---------------------
.ggcorr_results_get <- function(combn_mat,
                                 data,
                                 corr_method,
                                 corr_lab) {    
  purrr::map_df(seq_len(ncol(combn_mat)), function(i) {
    grp_vec_curr <- combn_mat[, i]
    data_curr <- data |> dplyr::filter(.grp %in% grp_vec_curr)
    purrr::map_df(corr_method, function(mthd) {
      switch(mthd,
      "ccc" = .ggcorr_results_get_ccc(data_curr, grp_vec_curr, corr_lab),
      "pearson" = ,
      "spearman" = ,
      "kendall" = .ggcorr_results_get_stats(
        data, grp_vec_curr, mthd, corr_lab
      ),
      stop(paste0("method ", mthd, " not recognised"))
      )
    })
  })
}


.ggcorr_results_get_ccc <- function(data,
                                    grp_vec,
                                    corr_lab) {
  .utilsggsv_dep_install("cccrm")
  corr <- .ggcorr_results_get_ccc_corr(data)
  .ggcorr_results_get_ccc_tbl(corr, grp_vec, corr_lab)
}

.ggcorr_results_get_ccc_corr <- function(data) {
  corr <- suppressWarnings(
    cccrm::cccUst(
      dataset = data, ry = ".y", rmet = ".grp", cl = 0.95
    ))[1:3]
  corr |> signif(2)
}


.ggcorr_results_get_ccc_tbl <- function(corr, grp_vec, corr_lab) {
  .ggcorr_results_get_ccc_tbl_init(corr, grp_vec) |>
    .ggcorr_results_get_ccc_txt(corr_lab)
}

.ggcorr_results_get_ccc_tbl_init <- function(corr, grp_vec) {
  tibble::tibble(
    g1 = grp_vec[1], g2 = grp_vec[2],
    est = corr[1], lb = corr[2], ub = corr[3]
  )
}

.ggcorr_results_get_ccc_tbl_final <- function(out_tbl, corr_lab) {
  out_tbl |>
    dplyr::mutate(
      txt = paste0(
        g1, " vs ", g2,
        .ggcorr_results_get_method_txt(corr_lab, "ccc"),
        ": ", est,
        " (", lb, "; ", ub, ")"
      )
    )
}

# stats
.ggcorr_results_get_stats <- function(data, grp_vec, mthd) {
  corr <- .ggcorr_results_get_stats_corr(data, grp_vec, mthd) 
  .ggcorr_results_get_stats_tbl(corr, grp_vec, mthd, corr_lab)
}

.ggcorr_results_get_stats_corr <- function(data, grp_vec, mthd) {
  corr <- cor.test(
    data[[".y"]][data[[".grp"]] == grp_vec[1]],
    data[[".y"]][data[[".grp"]] == grp_vec[2]],
    method = mthd
  )
  c(corr$estimate, corr[["conf.int"]]) |> signif(2)
}

.ggcorr_results_get_stats_tbl <- function(corr, grp_vec, mthd) {
  .ggcorr_results_get_stats_tbl_init(corr, grp_vec, mthd) |>
    .ggcorr_results_get_stats_tbl_final(mthd)
}

.ggcorr_results_get_stats_tbl_init <- function(corr, grp_vec, mthd) {
  out_tbl_init <- tibble::tibble(
    g1 = grp_vec_curr[1], g2 = grp_vec_curr[2], est = corr[1]
  )
  if (mthd == "spearman") return(out_tbl_init)
  out_tbl_init |> dplyr::mutate(lb = corr[2], ub = corr[3])
}

.ggcorr_results_get_stats_tbl_final <- function(out_tbl_init, mthd) {
  switch(mthd,
    "spearman" = .ggcorr_results_get_stats_tbl_final_spearman(
      out_tbl_init, mthd
    ),
    .ggcorr_results_get_stats_tbl_final_other(
      out_tbl_init, mthd, corr_lab
    )
  )
}

.ggcorr_results_get_stats_tbl_final_spearman <- function(out_tbl_init,
                                                         corr_lab) {
  out_tbl_init |>
    dplyr::mutate(
      txt = paste0(
        g1, " vs ", g2,
        .ggcorr_results_get_method_txt(corr_lab, "spearman"),
        ": ", est
      )
    )
}

.ggcorr_results_get_stats_tbl_final_other <- function(out_tbl_init,
                                                      mthd,
                                                      corr_lab) {
  out_tbl_init |>
    dplyr::mutate(
      txt = paste0(
        g1, " vs ", g2,
        .ggcorr_results_get_method_txt(corr_lab, mthd),
        ": ", est,
        " (", lb, "; ", ub, ")"
      )
    )
}

.ggcorr_results_get_method_txt <- function(corr_lab, mthd) {
  if (is.null(corr_lab) || !mthd %in% names(corr_lab)) {
    return(character(1L))
  }
  paste0(" ", corr_lab[[mthd]])
}

.ggcorr_plot <- function(data,
                         grp_vec,
                         grp_to_col,
                         thm,
                         grid,
                         limits_expand,
                         limits_equal,
                         trans,
                         results_tbl,
                         coord,
                         skip,
                         hjust,
                         vjust,
                         abline,
                         smooth,
                         label_id,
                         label_id_size,
                         combn_mat) {
  ggcorr_plot_tbl_get_raw(data, grp_vec) |>
    .ggcorr_plot_init(plot_tbl_raw, thm, grid) |>
    .ggcorr_plot_theme(thm, grid) |>
    .ggcorr_plot_colour(plot_tbl_raw, grp_to_col, grp_vec) |>
    .ggcorr_plot_limits(limits_expand, limits_equal, plot_tbl_raw) |>
    .ggcorr_plot_results(
      trans, results_tbl, coord, skip, hjust, vjust
    ) |>
    .ggcorr_plot_trans(trans) |>
    .ggcorr_plot_label_axes(p, grp_vec, combn_mat) |>
    .ggcorr_plot_abline(abline) |>
    .ggcorr_plot_smooth(smooth) |>
    .ggcorr_plot_label_points(label_id)
}

.ggcorr_plot_tbl_get_raw <- function(data, grp_vec) {
  purrr::map_df(
    setdiff(grp_vec, grp_vec[1]),
    function(grp_alt) {
      data |>
        dplyr::filter(.grp == grp_vec[1]) |>
        dplyr::select(.grp, .id, .y) |>
        dplyr::rename(x = .y, grp_x = .grp) |>
        dplyr::full_join(
          data |>
            dplyr::filter(.grp == grp_alt) |>
            dplyr::select(.grp, .id, .y) |>
            dplyr::rename(y = .y, grp_y = .grp),
          by = ".id"
        ) |>
        dplyr::select(.id, grp_x, grp_y, x, y)
    }
  )
}

.ggcorr_plot_init <- function(plot_tbl_raw) {
  p <- ggplot(
    plot_tbl_raw,
    aes(x = x, y = y, col = grp_y)
  ) +
    geom_point()
}

.ggcorr_plot_theme <- function(p, thm, grid) {
  p <- p |>
    .ggcorr_plot_init_theme(thm) |>
    .ggcorr_plot_init_grid(grid)
  p + 
    theme(
      legend.position = "bottom",
      legend.title = element_blank()
    )
}

.ggcorr_plot_init_theme <- function(p, thm) {
  if (is.null(thm)) return(p)
  p + thm
}

.ggcorr_plot_init_grid <- function(p, grid) {
  if (is.null(grid)) return(p)
  p + grid
}

.ggcorr_plot_colour <- function(p, plot_tbl_raw, grp_to_col, grp_vec) {
  if (!is.null(grp_to_col)) {
    p <- .ggcorr_plot_colour_manual(p, plot_tbl_raw, grp_to_col, grp_vec)
    p <- p +
      scale_colour_manual(
        values = grp_to_col,
        limits = unique(plot_tbl_raw$grp_y)
      )
  } else {
    p <- .ggcorr_plot_colour_auto(p, plot_tbl_raw)
  }
  if (length(grp_vec) <= 2) {
    p <- p + guides(colour = "none")
  }
  p
}

.ggcorr_plot_colour_manual <- function(p, plot_tbl_raw, grp_to_col, grp_vec) {
  grp_to_col <- .ggcorr_plot_colour_manual_ensure_name(grp_to_col, plot_tbl_raw)
  p +
    scale_colour_manual(
      values = grp_to_col,
      limits = unique(plot_tbl_raw$grp_y)
    )
}
.ggcorr_plot_colour_manual_ensure_name <- function(grp_to_col, plot_tbl_raw) {
  if (!is.null(names(grp_to_col))) {
    return(.ggcorr_plot_colour_manual_ensure_name_specified(
        grp_to_col, plot_tbl_raw
    ))
  }
  .ggcorr_plot_colour_manual_ensure_name_unspecified(
    grp_to_col, plot_tbl_raw, grp_vec
  )
  
}

.ggcorr_plot_colour_manual_ensure_name_specified <- function(grp_to_col,
                                                             plot_tbl_raw) {
  if (!all(names(grp_to_col) %in% unique(plot_tbl_raw$grp_y))) {
    stop("names of grp_to_col must be in .grp column of data")
  }
  grp_to_col
}

.ggcorr_plot_colour_manual_ensure_name_unspecified <- function(grp_to_col,
                                                               plot_tbl_raw,
                                                               grp_vec) {
  switch(as.character(length(grp_to_col)),
  "1" = .ggcorr_plot_colour_manual_ensure_name_unspecified_one(grp_to_col),
  .ggcorr_plot_colour_manual_ensure_name_unspecified_mult(
    grp_to_col, grp_vec, plot_tbl_raw
  )
  )
        
}
.ggcorr_plot_colour_manual_ensure_name_unspecified_one <- function(grp_to_col,
                                                                   plot_tbl_raw) {
  stats::setNames(
    rep(grp_to_col, length(unique(plot_tbl_raw$grp_y))),
    unique(plot_tbl_raw$grp_y)
  )
}

.ggcorr_plot_colour_manual_ensure_name_unspecified_mult <- function(grp_to_col,
                                                                    grp_vec,
                                                                    plot_tbl_raw) {
  if (length(grp_to_col) < length(grp_vec) - 1) {
    stop(paste0("If more than length one and not named,
    the number of elements in grp_to_col
    must be at least as many as the number of groups
    less one
    in data[[grp]]"))
  }
  stats::setNames(
    grp_to_col[seq_len(length(unique(plot_tbl_raw$grp_y)))],
    unique(plot_tbl_raw$grp_y)
  )
}

.ggcorr_plot_colour_auto <- function(p, plot_tbl_raw) {
  p +
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

.ggcorr_plot_limits <- function(limits_expand,
                                limits_equal,
                                plot_tbl_raw) {
  if (is.null(limits_expand)) {
    limits_expand <- list(
      x = range(plot_tbl_raw$x),
      y = range(plot_tbl_raw$y)
    )
  }

  axis_limits(
    p = p,
    limits_expand = limits_expand,
    limits_equal = limits_equal
  )
}

.ggcorr_plot_results <- function(p,
                                 trans,
                                 results_tbl,
                                 coord,
                                 skip,
                                 hjust,
                                 vjust) {
  add_text_column(
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
}

.ggcorr_plot_trans <- function(p, trans) {
  p + 
    scale_x_continuous(trans = trans) +
    scale_y_continuous(trans = trans)
}
.ggcorr_plot_label_axes <- function(p, grp_vec, combn_mat) {
  p <- p + labs(x = grp_vec[1])
  if (ncol(combn_mat) == 1) {
    p <- p +
      labs(y = combn_mat[2, 1]) +
      theme(legend.position = "none")
  } else {
    p <- p + labs(y = "Comparison group")
  }
  p
}

.ggcorr_plot_abline <- function(p, abline) {
  if (!abline) return(p)
  p +
    geom_abline(
      intercept = 0, slope = 1, linetype = "solid", colour = "gray85"
    )
}

.ggcorr_plot_smooth <- function(p, smooth) {
  if (!smooth) return(p)
  p +
    geom_smooth(method = smooth_method, formula = y ~ x, se = FALSE)
}

.ggcorr_plot_label_points <- function(p, label_id) {
  if (!label_id) return(p)
  .utilsggsv_dep_install("ggrepel")
  p +
    geom_text_repel(
      aes(label = .id),
      size = label_id_size
    )
}
