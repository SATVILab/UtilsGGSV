#' @md
#' @title Plot scatterplot with correlation coefficients and ab-line overlaid
#'
#' @description
#' Plots a scatterplot with various correlation coefficients and an optional
#' ab-line overlaid.
#'
#' @param data dataframe. Dataframe containing the data to be plotted.
#' @param grp character. Name of the column in `data` that specifies the
#' variables to compare between. The first level within `grp` (i.e. `data[[grp]][1]`)
#' is the level plotted on the x-axis, unless `grp_x` is set (see below).
#' @param grp_base character. If not `NULL`, then only correlations between this
#' level within `data[[grp]]` and other levels are displayed. If `NULL`, all
#' two-way correlations are displayed. Default is `NULL`.
#' @param grp_x character. If `NULL`, then this level of `grp` is plotted along
#' the x-axis. Default is `NULL`.
#' @param y character. Name of the column in `data` that specifies the
#' values being correlated between groups.
#' @param id character. Name of the column in `data` that specifies the subject
#' from which multiple measurements were taken.
#' @param corr_method character vector. Correlation method. One or more of
#' `c("concordance", "pearson", "spearman", "kendall")`. Default is `"spearman"`.
#' @param label_id logical. If `TRUE`, points labels as given by `data[[id]]`
#' are displayed using `ggrepel::geom_text_repel`. Default is `FALSE`.
#' @param label_id_size numeric. Size of labels. Default is `3`.
#' @param thm ggplot2 theme. Specifies `ggplot2` theme. Default is
#' `cowplot::theme_cowplot()`,
#' with a white plot background (as opposed to transparent).
#' @param grid The value from calling `cowplot::background_grid`.
#' Specifies display of grid lines.
#' If `NULL`, then no background grid is displayed
#' (recommended for non-cowplot-themed plots).
#' Default is `cowplot::background_grid(major = "xy")`, which displays
#' major grid lines on both x- and y-axes.
#' @param grp_to_col character vector.
#' Specifies colours for elements in `grp`.
#' Elements are colours.
#' If named, the names specify the level within `grp` to
#' which the colours are matched.
#' If `NULL` and only two groups are compared,
#' then all points are black.
#' If `NULL` and there are more than two groups
#' compared, then defaults to palette `Set1` of the RColorBrewer package.
#' Default is `NULL`.
#' @param abline logical. If `TRUE`, then the y=x line is plotted in colour
#' "gray85". Default is `TRUE`.
#' @param corr_lab `"never"``, `"auto"``, `"always"` or a named character vector.
#' Whether, and how, to label the correlation methods in the plot. 
#' If "always" or "never", then the correlation labels are always or never
#' displayed, respectively.
#' If `"auto"`, then they are displayed only if multiple correlation methods
#' are used.
#' If displaye dusing `"always"` or `"never"`, then default labels are used.
#' If a named character vector, then the names are the correlation methods and
#' the values are the labels,
#' e.g. `c("spearman" = "Spearman", "kendall" = "Kendall")`.
#' Default is `"auto"`.
#' correlation methods are used.
#' @param grp_lab `"always"`, `"auto"` or `"never"`. Whether to label the groups
#' being compared for a specific displayed correlation coefficients.
#' If `"always"` or `"never"`, then the groups are always or never labelled,
#' respectively.
#' If `"auto"`, then the groups are labelled only if multiple groups 
#' are compared.
#' Default is `"auto"`.
#' @param trans character or trans object. Specifies scaling of y- and x-axes.
#' If class is character, it is converted to a trans object. Adds "root_cube",
#' "root_fourth", "root_fifth" and "asinh" transformations, as well as "sqrt"
#' transformation that allows plotting of lines at zero. If class is a trans
#' object, it is returned as is. Default is `"identity"`.
#' @param coord numeric vector.
#' Coordinates for text placement for first (or only) row of results.
#' Specified in terms of proportion of axis length,
#' with the first value specifying the x-axis coordinate and the second
#' value the y-axis coordinate.
#' Default is `c(0.05, 0.95)`, which places the first row
#' in the top left corner of the plot.
#' @param skip numeric.
#' Proportion of y-axis to move down between lines of text.
#' Default is `0.05`.
#' @param font_size numeric. Font size for text in results table. Default is `10`.
#' @param hjust numeric. Horizontal justification for text. Default is `0`.
#' @param vjust numeric. Vertical justification for text. Default is `0.5`.
#' @param est_signif numeric. Significant digits for correlation estimates. 
#'   Default is `3`.
#' @param ci logical. Whether to include confidence intervals for correlation 
#'   estimates. Default is `TRUE`.
#' @param ci_signif numeric. Significant digits for confidence intervals. 
#'   Default is `3`.
#' @param pval logical. Whether to include p-values for correlation estimates. 
#'   Default is `TRUE`.
#' @param pval_signif numeric. Significant digits for p-values. Default is `3`.
#' @param pval_trunc numeric. P-values smaller than this threshold will be 
#'   displayed as "< threshold". Default is `0.001`.
#' @param limits_expand list. Expand the axis limits. Default is `NULL`.
#' @param limits_equal logical. Set axis limits equal. Default is `FALSE`.
#' @param legend_title logical.
#' If `FALSE`, then the legend title is removed. Default is `FALSE`.
#' @param legend_position character. Position of the legend. Default is `"bottom"`.
#' If `NULL`, then the legend's positioning is not jadjusted.
#' @param smooth logical. If `TRUE`, then a linear regression line is plotted.
#' @param smooth_se logical. Whether to display 95% confidence interval for the 
#'   smooth line. Default is `FALSE`.
#' @param smooth_method character. Smoothing method to pass to `geom_smooth`.
#'   Default is `"lm"`.
#' @export
#' @examples
#' response_vec_a <- rnorm(5)
#' response_tbl <- data.frame(
#'   group = rep(letters[1:3], each = 5),
#'   response = c(
#'     response_vec_a,
#'     response_vec_a * 1.2 + rnorm(5, sd = 0.2),
#'     response_vec_a * 2 + rnorm(5, sd = 2)
#'   ),
#'   pid = rep(paste0("id_", 1:5), 3)
#' )
#' library(UtilsGGSV)
#' ggcorr(
#'   data = response_tbl,
#'   grp = "group",
#'   y = "response",
#'   id = "pid"
#' )
ggcorr <- function(data,
                   corr_method = "spearman",
                   corr_lab = "auto",
                   grp,
                   grp_base = NULL,
                   grp_x = NULL,
                   grp_lab = "auto",
                   y,
                   id,
                   est_signif = 3,
                   ci = TRUE,
                   pval = TRUE,
                   ci_signif = 3,
                   pval_signif = 3,
                   pval_trunc = 0.001,
                   label_id = FALSE,
                   label_id_size = 3,
                   trans = "identity",
                   coord = c(0.05, 0.95),
                   skip = 0.05,
                   font_size = 4.2,
                   hjust = 0,
                   vjust = 0.5,
                   limits_expand = NULL,
                   limits_equal = FALSE,
                   legend_title = FALSE,
                   legend_position = "bottom",
                   thm = cowplot::theme_cowplot() +
                     theme(plot.background = element_rect(fill = "white")),
                   grid = cowplot::background_grid(major = "xy"),
                   grp_to_col = NULL,
                   abline = FALSE,
                   smooth = TRUE,
                   smooth_method = "lm",
                   smooth_se = FALSE) {
  
  .ggcorr_check(corr_method)
  
  prep_list <- .ggcorr_prep(
    data = data, grp = grp, y = y, id = id,
    grp_base = grp_base, grp_x = grp_x
  )
  
  results_tbl <- .ggcorr_results_get(
    data = prep_list$data, combn_mat = prep_list$combn_mat,
    grp_vec = prep_list$grp_vec,
    corr_method = corr_method, corr_lab = corr_lab,
    grp_lab = grp_lab, ci = ci, ci_signif = ci_signif,
    pval = pval, pval_signif = pval_signif, pval_trunc = pval_trunc,
    est_signif = est_signif
  )
  
  .ggcorr_plot(
    data = prep_list$data, grp_vec = prep_list$grp_vec,
    grp_to_col = grp_to_col, thm = thm, grid = grid,
    limits_expand = limits_expand, limits_equal = limits_equal,
    trans = trans, results_tbl = results_tbl, coord = coord,
    skip = skip, hjust = hjust, vjust = vjust,
    abline = abline, smooth = smooth, label_id = label_id,
    label_id_size = label_id_size, combn_mat = prep_list$combn_mat,
    grp_lab = grp_lab, legend_title = legend_title,
    legend_position = legend_position, smooth_se = smooth_se,
    smooth_method = smooth_method, font_size = font_size
  )
}

# ===========================
# check
# ===========================

.ggcorr_check <- function(corr_method) {
  .ggcorr_check_corr_method(corr_method)
}

.ggcorr_check_corr_method <- function(x) {
  if (!all(x %in% c("concordance", "pearson", "spearman", "kendall"))) {
    stop(
      "corr_method must be one or more of c('concordance', 'pearson', 'spearman', 'kendall')" # no lint
    )
  }
  if ("concordance" %in% x) {
    .utilsggsv_dep_install("cccrm")
  }
  invisible(TRUE)
}

# ==============================
# prep
# ==============================

.ggcorr_prep <- function(data,
                         grp,
                         grp_base,
                         grp_x,
                         y,
                         id) {
  
  data <- .ggcorr_prep_data(data, grp, y, id)
  
  list(
    data = data,
    combn_mat = .ggcorr_prep_combn_mat(data, grp_base),
    grp_vec = .ggcorr_prep_grp(data, grp_base, grp_x)
  )
}

.ggcorr_prep_data <- function(data, grp, y, id) {
  cn_vec <- colnames(data)
  cn_vec[which(cn_vec == grp)] <- ".grp"
  cn_vec[which(cn_vec == y)] <- ".y"
  cn_vec[which(cn_vec == id)] <- ".id"
  colnames(data) <- cn_vec
  data
}

.ggcorr_prep_grp_init <- function(data) {
  grp_vec <- unique(data$`.grp`)
  if (length(grp_vec) < 2) {
    stop("data must have at least two levels in grp")
  }
  grp_vec
}

.ggcorr_prep_combn_mat <- function(data, grp_base) {
  if (!is.null(grp_base)) {
    return(.ggcorr_prep_combn_mat_base(data, grp_base))
  }
  grp_init <- .ggcorr_prep_grp_init(data)
  utils::combn(grp_init, 2)
}

.ggcorr_prep_combn_mat_base <- function(data, grp_base) {
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

.ggcorr_prep_grp <- function(data, grp_base, grp_x) {
  grp_vec_init <- .ggcorr_prep_grp_init(data)
  if (!is.null(grp_base) || is.null(grp_x)) {
    return(grp_vec_init)
  }
  c(grp_x, setdiff(grp_vec_init, grp_x))
}

# =========================='
# results
# =========================='

# main function
# -----------------

.ggcorr_results_get <- function(data,
                                combn_mat,
                                grp_vec,
                                corr_method,
                                corr_lab,
                                grp_lab,
                                ci,
                                ci_signif,
                                pval,
                                pval_signif,
                                pval_trunc,
                                est_signif) {
  purrr::map_df(seq_len(ncol(combn_mat)), function(i) {
    grp_vec_curr <- combn_mat[, i]
    data_curr <- data |> dplyr::filter(.grp %in% grp_vec_curr)
    purrr::map_df(corr_method, function(mthd) {
      out_tbl_init <- .ggcorr_results_get_tbl_init(data_curr, grp_vec_curr, mthd)
      .ggcorr_results_get_tbl_final(
        out_tbl_init, corr_lab, grp_lab, grp_vec_curr, length(corr_method),
        mthd, ci, ci_signif, pval, pval_signif, pval_trunc, est_signif,
        length(unique(grp_vec))
      )
    })
  })
}

# initial table
# ----------------------

.ggcorr_results_get_tbl_init <- function(data, grp_vec, mthd) {
  switch(mthd,
    "concordance" = .ggcorr_results_get_init_conc(data, grp_vec),
    "pearson" = ,
    "spearman" = ,
    "kendall" = .ggcorr_results_get_init_stats(data, grp_vec, mthd),
    stop(paste0("method ", mthd, " not recognised"))
  )
}

.ggcorr_results_get_init_conc <- function(data, grp_vec) {  
  .utilsggsv_dep_install("cccrm")
  corr <- .ggcorr_results_get_init_conc_corr(data)
  tibble::tibble(
    method = "concordance",
    g1 = grp_vec[1], g2 = grp_vec[2],
    est = corr[1], lb = corr[2], ub = corr[3],
    pval = NA_real_
  )
}

.ggcorr_results_get_init_conc_corr <- function(data) {
  suppressWarnings(cccrm::cccUst(
    dataset = data, ry = ".y", rmet = ".grp", cl = 0.95
  )[1:3])
}

.ggcorr_results_get_init_stats <- function(data, grp_vec, mthd) {
  corr <- .ggcorr_results_get_init_stats_corr(data, grp_vec, mthd)
  out_tbl <- tibble::tibble(
    g1 = grp_vec[1], g2 = grp_vec[2], est = corr[1]
  )
  # add ci's and p-values
  switch(mthd,
    "kendall" = ,
    "spearman" = .ggcorr_results_get_init_stats_no_ci(out_tbl, corr),
    .ggcorr_results_get_init_stats_ci(out_tbl, corr)
  )
}

.ggcorr_results_get_init_stats_no_ci <- function(out_tbl,
                                                 corr) {
  out_tbl |> dplyr::mutate(lb = NA_real_, ub = NA_real_, pval = corr[[2]])
}

.ggcorr_results_get_init_stats_ci <- function(out_tbl,
                                              corr) {
  out_tbl |> dplyr::mutate(lb = corr[2], ub = corr[3], pval = corr[[4]])
}

.ggcorr_results_get_init_stats_corr <- function(data, grp_vec, mthd) {
  corr <- stats::cor.test(
    data[[".y"]][data[[".grp"]] == grp_vec[1]],
    data[[".y"]][data[[".grp"]] == grp_vec[2]],
    method = mthd
  )
  c(corr$estimate, corr[["conf.int"]], corr[["p.value"]])
}

# final table 
# ----------------------

.ggcorr_results_get_tbl_final <- function(out_tbl,
                                          corr_lab,
                                          grp_lab,
                                          grp_vec,
                                          n_corr_method,
                                          mthd,
                                          ci,
                                          ci_signif,
                                          pval,
                                          pval_signif,
                                          pval_trunc,
                                          est_signif,
                                          n_grp) {
  out_tbl |>
    .ggcorr_results_get_all_tbl_final_grp(grp_vec, grp_lab, n_grp) |>
    .ggcorr_results_get_all_tbl_final_corr_method(
      corr_lab, n_corr_method, grp_vec, grp_lab, mthd
    ) |>
    .ggcorr_results_get_all_tbl_final_corr_est(est_signif) |>
    .ggcorr_results_get_all_tbl_final_corr_ci(ci, mthd, ci_signif) |>
    .ggcorr_results_get_all_tbl_final_corr_pval(
      pval, pval_signif, pval_trunc, mthd, ci
    )
}

# ----------------------------
# display groups 
# -----------------

.ggcorr_results_get_all_tbl_final_grp <- function(out_tbl,
                                                  grp_vec,
                                                  grp_lab,
                                                  n_grp) {
  disp_grp_not <- grp_lab[[1]] == "never" ||
    (grp_lab[[1]] == "auto" && n_grp == 2)
  if (disp_grp_not) {
    return(out_tbl |> dplyr::mutate(txt = ""))
  }
  out_tbl |> dplyr::mutate(txt = paste0(g2, " vs ", g1))
}

# ----------------------------
# display correlation method
# ----------------------------

.ggcorr_results_get_all_tbl_final_corr_method <- function(out_tbl,
                                                          n_corr_method,
                                                          corr_lab,
                                                          grp_vec,
                                                          grp_lab,
                                                          mthd) {
  no_corr_lab <- .ggcorr_results_get_all_tbl_final_corr_method_check_not(
    corr_lab, n_corr_method
  )
  if (no_corr_lab) return(out_tbl)

  specified_lab <- .ggcorr_results_get_all_tbl_final_corr_method_check_not(
    corr_lab, n_corr_method
  )

  if (specified_lab) {
    return(.ggcorr_results_get_all_tbl_final_corr_method_get_specified(
        out_tbl, corr_lab, mthd
    ))
  }

  .ggcorr_results_get_all_tbl_final_corr_method_get_auto(
    out_tbl, corr_lab, mthd, n_corr_method
  )
}

.ggcorr_results_get_all_tbl_final_corr_method_check_not <-
  function(corr_lab, n_corr_method) {
  grepl("^never$", corr_lab[[1]]) ||
    (grepl("^auto$", corr_lab[[1]]) && n_corr_method == 1)
}

.ggcorr_results_get_all_tbl_final_corr_method_check_specified <-
  function(corr_lab) {
    !grepl("^auto$|^never$", corr_lab[[1]] |> trimws())
}

.ggcorr_results_get_all_tbl_final_corr_method_get_specified <-
  function(out_tbl,
           corr_lab,
           mthd) {
  if (!is.null(names(corr_lab))) {
    out_tbl <- out_tbl |>
      dplyr::mutate(
        txt = paste0(.data$txt, " ", gsub("^\\s+", "", corr_lab[[mthd]]))
      )
  } else {
    out_tbl |> dplyr::mutate(
      txt = paste0(data$txt, gsub("^\\s+", "", corr_lab))
    )
  }
  out_tbl
}

.ggcorr_results_get_all_tbl_final_corr_method_get_auto <- function(out_tbl,
                                                                   corr_lab,
                                                                   mthd,
                                                                   n_corr_method) {
  corr_lab_curr <- corr_lab_auto[[mthd]]
  if (!nzchar(out_tbl$txt[[1]])) {
    # if there are no groups specified,
    # then remove the brackets
    corr_lab_curr <- gsub("^\\(", "", gsub("\\)$", "", corr_lab_curr))
  } else {
    # if we've specified groups, add a 
    # space afterwards
    out_tbl <- out_tbl |> dplyr::mutate(txt = paste0(txt, " "))
  }
  out_tbl |>
    dplyr::mutate(txt = paste0(txt, corr_lab_curr))
}

# ----------------------------
# display estimate
# ----------------------------

.ggcorr_results_get_all_tbl_final_corr_est <- function(out_tbl, est_signif) {
  out_tbl |>
    .ggcorr_results_get_all_tbl_final_corr_colon() |>
    dplyr::mutate(txt = paste0(txt, signif(est, est_signif)))
}

.ggcorr_results_get_all_tbl_final_corr_colon <- function(out_tbl) {
  if (out_tbl$txt[[1]] == "") {
    return(out_tbl)
  }
  out_tbl |> dplyr::mutate(txt = paste0(txt, ": "))
}

# ----------------------------
# display ci
# ----------------------------

.ggcorr_results_get_all_tbl_final_corr_ci <- function(out_tbl,
                                                      ci,
                                                      mthd,
                                                      ci_signif) {
  if (!ci || mthd == "spearman" || is.na(out_tbl$lb[[1]])) return(out_tbl)
  out_tbl |>
    dplyr::mutate(
      txt = paste0(
        txt, " (", signif(lb, ci_signif), ", ", signif(ub, ci_signif), ")"
        )
      )
}

# ----------------------------
# display p-value
# ----------------------------

.ggcorr_results_get_all_tbl_final_corr_pval <- function(out_tbl,
                                                        pval,
                                                        pval_signif,
                                                        pval_trunc,
                                                        mthd,
                                                        ci) {
  if (!pval || is.na(out_tbl$pval[[1]])) return(out_tbl)
  if (ci && !is.na(out_tbl$lb[[1]])) {
    out_tbl <- .ggcorr_results_get_all_tbl_final_corr_pval_ci(
      out_tbl, pval_signif, pval_trunc, mthd
      )
  } else {
    out_tbl <- .ggcorr_results_get_all_tbl_final_corr_pval_ci_non(
      out_tbl, pval_signif, pval_trunc
      )
  }
  out_tbl
}

.ggcorr_results_get_all_tbl_final_corr_pval_ci <- function(out_tbl,
                                                           pval_signif,
                                                           pval_trunc,
                                                           mthd) {
  out_tbl <- out_tbl |> dplyr::mutate(txt = gsub("\\)$", "", txt)) 
  out_tbl |>
    dplyr::mutate(
      txt = paste0(
        txt, .ggcorr_results_get_all_tbl_final_corr_pval_ci_txt(
          pval, pval_signif, pval_trunc
        )
      )
    )
}

.ggcorr_results_get_all_tbl_final_corr_pval_ci_txt <- function(pval,
                                                               pval_signif,
                                                               pval_trunc) {
  vapply(pval, function(p) {
    if (p < pval_trunc) {
      return(paste0("; p < ", pval_trunc, ")"))
    }
    paste0("; p = ", signif(pval, pval_signif), ")")
  }, character(1))
}

.ggcorr_results_get_all_tbl_final_corr_pval_ci_non <- function(out_tbl,
                                                               pval_signif,
                                                               pval_trunc) {
   out_tbl |>
     dplyr::mutate(
      txt = paste0(
        txt,
        .ggcorr_results_get_all_tbl_final_corr_pval_ci_non_txt(
          pval, pval_signif, pval_trunc
          )
        )
    )
}

.ggcorr_results_get_all_tbl_final_corr_pval_ci_non_txt <- function(pval,
                                                                   pval_signif,
                                                                   pval_trunc) {
  vapply(pval, function(p) {
    if (p < pval_trunc) {
      return(paste0(" (p = ", pval_trunc), ")")
    }
    paste0(" (p = ", signif(pval, pval_signif), ")")
  }, character(1))
}

# ============================
# plot
# ============================

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
                         combn_mat,
                         grp_lab,
                         legend_title,
                         legend_position,
                         smooth_se,
                         smooth_method,
                         font_size) {
  plot_tbl_raw <- .ggcorr_plot_tbl_get_raw(data, grp_vec)
  .ggcorr_plot_init(plot_tbl_raw) |>
    .ggcorr_plot_theme(thm, grid, legend_title, legend_position) |>
    .ggcorr_plot_colour(plot_tbl_raw, grp_to_col, grp_vec) |>
    .ggcorr_plot_limits(limits_expand, limits_equal, plot_tbl_raw) |>
    .ggcorr_plot_results(
      results_tbl, coord, skip, hjust, vjust, trans, font_size
     ) |>
    .ggcorr_plot_trans(trans) |>
    .ggcorr_plot_label_axes(grp_vec, combn_mat) |>
    .ggcorr_plot_abline(abline) |>
    .ggcorr_plot_smooth(smooth, smooth_se, smooth_method) |>
    .ggcorr_plot_label_points(label_id, label_id_size)
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
  ggplot(
    plot_tbl_raw,
    aes(x = x, y = y, col = grp_y)
  ) +
    geom_point()
}

# theme
# -------------------------
.ggcorr_plot_theme <- function(p,
                               thm,
                               grid,
                               legend_title,
                               legend_position) {
  p <- .ggcorr_plot_init_theme(p, thm) |>
    .ggcorr_plot_init_grid(grid)
  if (isFALSE(legend_title)) {
    p <- p + theme(legend.title = element_blank())
  }
  if (!is.null(legend_position)) {
    p <- p + theme(legend.position = legend_position)
  }
  p
}

.ggcorr_plot_init_theme <- function(p, thm) {
  if (is.null(thm)) return(p)
  p + thm
}

.ggcorr_plot_init_grid <- function(p, grid) {
  if (is.null(grid)) return(p)
  p + grid
}

# --------------------------------
# colours
# --------------------------------

.ggcorr_plot_colour <- function(p, plot_tbl_raw, grp_to_col, grp_vec) {
  if (!is.null(grp_to_col)) {
    p <- .ggcorr_plot_colour_manual(p, plot_tbl_raw, grp_to_col, grp_vec)
  } else {
    p <- .ggcorr_plot_colour_auto(p, plot_tbl_raw)
  }
  if (length(grp_vec) <= 2) {
    p <- p + guides(colour = "none")
  }
  p
}

.ggcorr_plot_colour_manual <- function(p,
                                       plot_tbl_raw,
                                       grp_to_col,
                                       grp_vec) {
  grp_to_col <- .ggcorr_plot_colour_manual_ensure_name(
    grp_to_col, plot_tbl_raw, grp_vec
  )
  p +
    scale_colour_manual(
      values = grp_to_col,
      limits
      = unique(plot_tbl_raw$grp_y)
    )
}

.ggcorr_plot_colour_manual_ensure_name <- function(grp_to_col,
                                                   plot_tbl_raw,
                                                   grp_vec) {
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
    "1" = .ggcorr_plot_colour_manual_ensure_name_unspecified_one(
      grp_to_col, plot_tbl_raw
    ),
    .ggcorr_plot_colour_manual_ensure_name_unspecified_mult(
      grp_to_col, grp_vec, plot_tbl_raw
    )
  )
}

.ggcorr_plot_colour_manual_ensure_name_unspecified_one <-
  function(grp_to_col,
           plot_tbl_raw) {
  stats::setNames(
    rep(grp_to_col, length(unique(plot_tbl_raw$grp_y))),
    unique(plot_tbl_raw$grp_y)
  )
}

.ggcorr_plot_colour_manual_ensure_name_unspecified_mult <- 
  function(grp_to_col,
           grp_vec,
           plot_tbl_raw) {
  if (length(grp_to_col) < length(grp_vec) - 1) {
    stop(paste0(
      "If more than length one and not named, the number of elements in grp_to_col must be at least as many as the number of groups less one in data[[grp]]"
    ))
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
             values = stats::setNames(
               rep("black", length(unique(plot_tbl_raw$grp_y))),
               unique(plot_tbl_raw$grp_y)
             )
           ),
           scale_colour_brewer(palette = "Set1")
    )
}

.ggcorr_plot_limits <- function(p, limits_expand, limits_equal, plot_tbl_raw) {
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
                                 results_tbl,
                                 coord,
                                 skip,
                                 hjust,
                                 vjust,
                                 trans,
                                 font_size) {
  add_text_column(
    p = p,
    x = p$layers[[2]]$data$x,
    y = p$layers[[2]]$data$y,
    text = results_tbl$txt,
    coord = coord,
    skip = skip,
    hjust = hjust,
    vjust = vjust,
    trans = trans,
    font_size = font_size
  )
}

.ggcorr_plot_trans <- function(p, trans) {
  p + 
    scale_x_continuous(trans = get_trans(trans)) +
    scale_y_continuous(trans = get_trans(trans))
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

.ggcorr_plot_smooth <- function(p, smooth, smooth_se, smooth_method) {
  if (!smooth) return(p)
  p +
    geom_smooth(method = smooth_method, formula = y ~ x, se = smooth_se)
}

.ggcorr_plot_label_points <- function(p, label_id, label_id_size) {
  if (!label_id) return(p)
  .utilsggsv_dep_install("ggrepel")
  p +
    ggrepel::geom_text_repel(
      aes(label = .id),
      size = label_id_size
    )
}
