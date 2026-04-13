# In R/utils.R or a similar file in your package
# declare these variables as global
# as they are column names in dataframes manipulated
# by dplyr
utils::globalVariables(c(
  'x', 'y', 'grp_y', '.id', '.grp', 'grp_x',
  'txt', 'lb', 'ub', 'est', 'data', 'pval', 'g2',
  'g1', '.y',
  'cluster', 'variable', 'perc'
  ))

# Internal named colour palettes for continuous fill/colour scales used by
# plot_cluster_heatmap, plot_cluster_mst, and plot_cluster_scatter.
#
# Each entry is a list with:
#   col          - character vector of colours, low to high
#   col_positions - "auto" or a numeric vector; "auto" uses the white_range
#                  argument of the calling function to stretch the middle colour
#                  of 3-colour palettes.
#
# Diverging palettes have col_positions = "auto" so the neutral midpoint is
# stretched over white_range.  Sequential palettes use evenly-spaced
# col_positions to avoid that stretching.
.cluster_colour_palettes <- function() {
  list(
    # Neutral blue-white-red. Default. Good for any pos/neg or high/low data.
    bipolar = list(
      col = c("#2166AC", "#F7F7F7", "#B2182B"),
      col_positions = "auto"
    ),
    # Green-white-red. Intuitive "good/safe/present" vs "bad/danger/absent".
    # Note: the red-green pairing is not ideal for red-green colour-blind
    # viewers; use "accessible" for a fully colour-blind-safe alternative.
    alarm = list(
      col = c("#1A9641", "#F7F7F7", "#D7191C"),
      col_positions = "auto"
    ),
    # Blue-white-burnt-orange. Fully colour-blind-safe diverging alternative
    # to "alarm" (uses the Okabe-Ito blue and vermillion).
    accessible = list(
      col = c("#0072B2", "#F7F7F7", "#D55E00"),
      col_positions = "auto"
    ),
    # Light-yellow to dark-red. Sequential warm scale for expression,
    # intensity, or activation.
    heat = list(
      col = c("#FFFFD4", "#FD8D3C", "#800026"),
      col_positions = c(0, 0.5, 1)
    ),
    # White to navy. Sequential cool scale for coverage, frequency, or
    # presence probability.
    sky = list(
      col = c("#F7FBFF", "#6BAED6", "#08306B"),
      col_positions = c(0, 0.5, 1)
    )
  )
}

# Internal helper: generate a named discrete colour vector for cluster/group
# labels. When user_colours is not NULL it is returned unchanged. Otherwise,
# colours are chosen from a tier of palettes designed for maximum perceptual
# distinguishability:
#   - up to  8 groups: Okabe-Ito (colorblind-friendly)
#   - up to 12 groups: ColorBrewer Paired
#   - up to 21 groups: Kelly's palette (Polychrome), white skipped
#   - up to 31 groups: Glasbey's palette (Polychrome), white skipped
#   - more than 31   : hue_pal() fallback (with a warning)
# The Polychrome package is optional (in Suggests); if absent a warning is
# issued and hue_pal() is used instead.
.discrete_cluster_colours <- function(groups, user_colours = NULL) {
  if (!is.null(user_colours)) return(user_colours)
  n <- length(groups)
  cols <- if (n <= 8L) {
    c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442",
      "#0072B2", "#D55E00", "#CC79A7", "#999999"
    )[seq_len(n)]
  } else if (n <= 12L) {
    scales::brewer_pal(palette = "Paired")(n)
  } else if (n <= 21L) {
    if (!requireNamespace("Polychrome", quietly = TRUE)) {
      warning(
        "Package 'Polychrome' is recommended for > 12 groups but is not ",
        "installed. Falling back to hue_pal(). Install it for better colour ",
        "separation: install.packages('Polychrome').",
        call. = FALSE
      )
      scales::hue_pal()(n)
    } else {
      unname(Polychrome::kelly.colors(22L))[-1L][seq_len(n)]
    }
  } else if (n <= 31L) {
    if (!requireNamespace("Polychrome", quietly = TRUE)) {
      warning(
        "Package 'Polychrome' is recommended for > 12 groups but is not ",
        "installed. Falling back to hue_pal(). Install it for better colour ",
        "separation: install.packages('Polychrome').",
        call. = FALSE
      )
      scales::hue_pal()(n)
    } else {
      unname(Polychrome::glasbey.colors(32L))[-1L][seq_len(n)]
    }
  } else {
    warning(
      n, " groups exceeds the recommended maximum of 31 for distinct ",
      "colours. Falling back to hue_pal(), which may produce indistinguishable ",
      "colours at this scale.",
      call. = FALSE
    )
    scales::hue_pal()(n)
  }
  stats::setNames(cols, groups)
}

# Internal helper: resolve palette → col + col_positions, with validation.
# Returns a list(col, col_positions).  If palette is NULL, returns the
# supplied col/col_positions unchanged.
.resolve_cluster_palette <- function(palette, col, col_positions) {
  if (is.null(palette)) return(list(col = col, col_positions = col_positions))
  pals <- .cluster_colour_palettes()
  if (!is.character(palette) || length(palette) != 1L || is.na(palette)) {
    stop("`palette` must be a single character string or NULL.", call. = FALSE)
  }
  if (!palette %in% names(pals)) {
    stop(
      paste0(
        "`palette` must be one of: ",
        paste(paste0('"', names(pals), '"'), collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  list(col = pals[[palette]]$col, col_positions = pals[[palette]]$col_positions)
}

# Internal helper: build colour/fill scale arguments from col + col_positions
# + white_range, applying the same stretching logic used by heatmap and MST.
# Returns a list(colours, values) suitable for scale_*_gradientn.
.build_gradientn_args <- function(col, col_positions, white_range) {
  n_col <- length(col)
  if (identical(col_positions, "auto")) {
    if (n_col == 3L) {
      list(
        colours = c(col[1], col[2], col[2], col[3]),
        values  = c(0, white_range[1], white_range[2], 1)
      )
    } else {
      list(
        colours = col,
        values  = seq(0, 1, length.out = n_col)
      )
    }
  } else {
    list(colours = col, values = col_positions)
  }
}
