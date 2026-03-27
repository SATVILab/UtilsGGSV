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
