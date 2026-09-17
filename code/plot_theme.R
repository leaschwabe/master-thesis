###############################################################################
# plot_theme.R - Shared ggplot2 theme, palette, sizing, and labels
#
# Sourced by the plotting/diagnostics scripts to keep every figure consistent
# with the thesis. Rules enforced here:
#   - Font matches the .tex document (Latin Modern / lmodern, 12pt A4).
#   - No in-chart title or subtitle (added in LaTeX captions instead).
#   - No raw variable names in charts (use var_label() for readable labels).
#   - Colour palette: PrettyCols "Bold" (hardcoded so PrettyCols need not be
#     installed).
#   - Sizing tuned to the .tex \includegraphics[width=14cm] figure width.
#
# Usage:
#   source("code/plot_theme.R")
#   ggplot(...) + ... + theme_thesis()
#   scale_colour_thesis()  / scale_fill_thesis()
#   ggsave_thesis("charts/myplot.pdf", p)              # single plot
#   ggsave_thesis("charts/myplot.pdf", p, height = 12) # taller (e.g. facets)
###############################################################################

library(ggplot2)

# --- Font: match the .tex (lmodern = Latin Modern) --------------------------
## The document uses \usepackage{lmodern} at 12pt. To render charts in the same
## family we register Latin Modern via showtext when available. If showtext or
## the font is missing, we fall back to a generic serif so scripts never error.
## (To enable the exact match once: install.packages(c("showtext","sysfonts")).)
.thesis_font <- "serif"
if (requireNamespace("showtext", quietly = TRUE) &&
    requireNamespace("sysfonts", quietly = TRUE)) {
  .sf <- sysfonts::font_files()
  .lm <- .sf$family[grepl("Latin Modern Roman|CMU Serif|LM Roman",
                          .sf$family, ignore.case = TRUE)]
  if (length(.lm) > 0) {
    .fam <- unique(.lm)[1]
    try({
      sysfonts::font_add(family = "lmroman",
                         regular = .sf$path[.sf$family == .fam][1])
      showtext::showtext_auto()
      .thesis_font <- "lmroman"
    }, silent = TRUE)
  }
}

# --- Colour palette: PrettyCols "Bold" (5 qualitative colours) --------------
## Source: PrettyCols::prettycols("Bold"). Hardcoded to avoid the dependency.
## Ordered green, magenta, blue, orange, purple so that the first two colours
## (green, magenta) are the primary pair used for the elastic/inelastic split.
thesis_palette <- c(
  "#679c35",  # green   (primary 1: elastic)
  "#cd1076",  # magenta (primary 2: inelastic)
  "#6497b1",  # blue
  "#FFB04F",  # orange
  "#6a359c"   # purple
)

## Named primary colours for the supply-elasticity distinction. Use these when a
## chart maps specifically to elastic vs. inelastic so the pairing is explicit.
thesis_primary <- c(
  elastic   = "#679c35",  # green
  inelastic = "#cd1076"   # magenta
)

## Endpoints for a continuous (diverging / sequential) scale built from the two
## primary colours, e.g. the correlation heatmap. Magenta = high, green = low,
## with a light neutral midpoint so zero reads as near-white.
thesis_grad_low  <- "#679c35"  # green   (low / negative end)
thesis_grad_mid  <- "#f2f2f2"  # neutral light grey (midpoint)
thesis_grad_high <- "#cd1076"  # magenta (high / positive end)

## Default line width for line charts. Kept thin so series stay legible at the
## 14cm figure width. Reference in geom_line(linewidth = thesis_linewidth).
thesis_linewidth <- 0.6

scale_colour_thesis <- function(...) {
  ggplot2::scale_colour_manual(values = thesis_palette, ...)
}
scale_color_thesis <- scale_colour_thesis
scale_fill_thesis <- function(...) {
  ggplot2::scale_fill_manual(values = thesis_palette, ...)
}

## Continuous fill scale in the primary colours. Diverging by default (for
## correlations centred at 0); set `diverging = FALSE` for a sequential
## green-to-magenta ramp. `limits`/`midpoint` follow ggplot2 defaults.
scale_fill_thesis_c <- function(..., diverging = TRUE, midpoint = 0) {
  if (diverging) {
    ggplot2::scale_fill_gradient2(
      low = thesis_grad_low, mid = thesis_grad_mid, high = thesis_grad_high,
      midpoint = midpoint, ...
    )
  } else {
    ggplot2::scale_fill_gradient(low = thesis_grad_low, high = thesis_grad_high, ...)
  }
}

## Continuous colour scale counterpart (same colours, for point/line colour).
scale_colour_thesis_c <- function(..., diverging = TRUE, midpoint = 0) {
  if (diverging) {
    ggplot2::scale_colour_gradient2(
      low = thesis_grad_low, mid = thesis_grad_mid, high = thesis_grad_high,
      midpoint = midpoint, ...
    )
  } else {
    ggplot2::scale_colour_gradient(low = thesis_grad_low, high = thesis_grad_high, ...)
  }
}
scale_color_thesis_c <- scale_colour_thesis_c

# --- Theme ------------------------------------------------------------------
## Base size 11pt: at the 14cm output width this sits just below the 12pt body
## text, the usual convention for figure text. No title/subtitle (plot.title
## and plot.subtitle are blanked; add them in the LaTeX caption instead).
theme_thesis <- function(base_size = 9, base_family = .thesis_font) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title       = ggplot2::element_blank(),
      plot.subtitle    = ggplot2::element_blank(),
      plot.caption     = ggplot2::element_blank(),
      axis.title       = ggplot2::element_text(size = base_size),
      # Push the axis titles away from the tick labels so they do not crowd the
      # axis (extra right margin on the y-title, top margin on the x-title).
      axis.title.y     = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
      axis.title.x     = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      axis.text        = ggplot2::element_text(size = base_size - 1, colour = "grey20"),
      # Angle the x-axis tick labels so quarterly labels (Q1 2017, ...) remain
      # legible instead of overlapping horizontally.
      axis.text.x      = ggplot2::element_text(angle = 45, hjust = 1),
      legend.title     = ggplot2::element_text(size = base_size),
      legend.text      = ggplot2::element_text(size = base_size - 1),
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(size = base_size, face = "plain"),
      # Minor gridlines re-enabled (fainter than the major lines) so minor axis
      # breaks are visible.
      panel.grid.minor = ggplot2::element_line(linewidth = 0.2, colour = "grey93"),
      panel.grid.major = ggplot2::element_line(linewidth = 0.3, colour = "grey88"),
      # Border around the plotting area (like theme_light). fill = NA keeps the
      # panel transparent so gridlines/data remain visible.
      panel.border     = ggplot2::element_rect(colour = "grey70", fill = NA, linewidth = 0.4),
      plot.margin      = ggplot2::margin(4, 6, 4, 4)
    )
}

# --- Readable variable labels (no raw variable names in charts) -------------
## Central lookup mapping internal column names to human-readable labels.
## Extend as new variables are plotted. Use via var_label("log_income_pc").
.thesis_labels <- c(
  p_composite   = "Composite price (\u20ac/m\u00b2)",
  efh_avg       = "House price (\u20ac/m\u00b2)",
  etw_avg       = "Apartment price (\u20ac/m\u00b2)",
  gst           = "Building-land price (\u20ac/m\u00b2)",
  ly_composite  = "Log composite price",
  ly_efh        = "Log house price",
  ly_etw        = "Log apartment price",
  income_pc     = "Income per capita",
  log_income_pc = "Log income per capita",
  population    = "Population",
  unemp_rate    = "Unemployment rate",
  blr_14        = "Building-land reserve (%)",
  inelastic_dummy = "Supply-inelastic",
  shock         = "Monetary policy shock",
  shock_ease    = "Easing surprise",
  shock_tight   = "Tightening surprise",
  w_house       = "House weight",
  w_apt         = "Apartment weight"
)

var_label <- function(x) {
  out <- unname(.thesis_labels[x])
  ifelse(is.na(out), x, out)  # fall back to the raw name if unmapped
}

# --- Quarterly x-axis (yearqtr) ---------------------------------------------
## scale_x_yearqtr(format = "%Y") only prints the year (e.g. "2017"). This
## helper labels ticks as quarters ("Q1 2017", "Q4 2021"). By default it shows
## every 4th quarter (one tick per year) to avoid clutter at the 14cm width;
## pass by = 0.25 for every quarter, or n for an approximate tick count.
## NOTE: scale_x_yearqtr() is provided by the zoo package (not ggplot2), so zoo
## must be attached (library(zoo)) when a chart uses this helper, as the
## diagnostics script already does.
scale_x_quarter <- function(by = 1, n = NULL, ...) {
  fmt <- function(z) paste0("Q", zoo::format.yearqtr(z, "%q"), " ",
                            zoo::format.yearqtr(z, "%Y"))
  if (is.null(n)) {
    zoo::scale_x_yearqtr(labels = fmt, breaks = function(lims) {
      lo <- as.numeric(lims[1]); hi <- as.numeric(lims[2])
      # For yearly (or coarser) spacing, anchor ticks on Q1 (whole years) so
      # labels read Q1 2017, Q1 2018, ... rather than starting on Q4. For
      # sub-annual spacing (by < 1), step from the first quarter in range.
      start <- if (by >= 1) ceiling(lo) else zoo::as.yearqtr(ceiling(lo * 4) / 4)
      seq(from = zoo::as.yearqtr(start),
          to   = zoo::as.yearqtr(floor(hi * 4) / 4),
          by   = by)
    }, ...)
  } else {
    zoo::scale_x_yearqtr(labels = fmt, n = n, ...)
  }
}

# --- Symmetric numeric y-axis around zero -----------------------------------
## For charts with a numbered vertical axis that spans positive and negative
## values (e.g. the signed shock components), make the axis symmetric so the
## min and max have equal magnitude, for example -10 / +10. Pass the values that
## the axis must cover; the helper rounds the largest absolute value up to a
## "nice" number and returns a scale_y_continuous with matching minor breaks.
##   + scale_y_symmetric(c(shock_components$value, shock_net_thesis$net))
scale_y_symmetric <- function(values, n_major = 5, ...) {
  m <- max(abs(values), na.rm = TRUE)
  if (!is.finite(m) || m == 0) m <- 1
  step  <- pretty(c(-m, m), n = n_major)
  lim   <- max(abs(step))
  major <- seq(-lim, lim, by = step[2] - step[1])
  minor <- seq(-lim, lim, by = (step[2] - step[1]) / 2)
  ggplot2::scale_y_continuous(limits = c(-lim, lim), breaks = major,
                              minor_breaks = minor, ...)
}

# --- Impulse-response (local projection) charts -----------------------------
## Integer horizon x-axis (0, 1, ..., H). Unlike scale_x_quarter this labels
## whole quarters-ahead and, since the labels are single integers, overrides the
## theme's 45-degree tick angle back to horizontal. Use for every IRF chart.
scale_x_horizon <- function(h_max = 12, by = 2, ...) {
  ggplot2::scale_x_continuous(breaks = seq(0, h_max, by = by),
                              minor_breaks = 0:h_max, ...)
}

## Standard IRF layers: a zero reference line, a shaded confidence ribbon, and
## the point-estimate line with markers. Expects a data frame with columns
## `horizon`, `coef`, `ci_lo`, `ci_hi`. `colour` sets the line/ribbon hue (a
## single thesis-palette colour); the ribbon is the same hue at low alpha.
## Returns a list of layers, so it composes with ggplot(...) + geom_irf(df).
## For multi-series IRFs (e.g. ease vs. tight) map colour inside aes() and use
## the scale_*_thesis scales instead of this single-series helper.
geom_irf <- function(df, colour = thesis_primary[["inelastic"]],
                     ribbon_alpha = 0.18, linewidth = thesis_linewidth) {
  list(
    ggplot2::geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3),
    ggplot2::geom_ribbon(data = df,
      ggplot2::aes(x = horizon, ymin = ci_lo, ymax = ci_hi),
      fill = colour, alpha = ribbon_alpha, inherit.aes = FALSE),
    ggplot2::geom_line(data = df,
      ggplot2::aes(x = horizon, y = coef),
      colour = colour, linewidth = linewidth, inherit.aes = FALSE),
    ggplot2::geom_point(data = df,
      ggplot2::aes(x = horizon, y = coef),
      colour = colour, size = 1.3, inherit.aes = FALSE)
  )
}

## Readable labels for the estimation specifications (see 03_estimation.R). Used
## for facet strips and legends in 04_results.R so raw spec codes never appear.
spec_label <- function(x) {
  lut <- c(
    baseline         = "Baseline",
    AS1_house        = "House",
    AS1_apartment    = "Apartment",
    AS2_asymmetric   = "Asymmetric",
    AS3_continuous   = "Continuous reserve",
    R1_no_vienna     = "Vienna excluded",
    R2_pre_tail      = "Pre-tail sample",
    R3_de20y         = "DE 20Y instrument",
    R3_de30y         = "DE 30Y instrument",
    R4_threshold_p20 = "Threshold: 20th pct",
    R4_threshold_p30 = "Threshold: 30th pct",
    R5_lag_augmented = "Lag-augmented"
  )
  out <- unname(lut[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}

## Readable labels for the asymmetric-specification terms.
term_label <- function(x) {
  lut <- c(shk_ease_x_inel = "Easing surprise",
           shk_tight_x_inel = "Tightening surprise")
  out <- unname(lut[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}

# --- Saving: sized to the .tex figure width (14 cm) -------------------------
## The .tex uses \includegraphics[width=14cm]. Save at that width so LaTeX does
## NOT rescale (which would distort font sizes). Default height suits a single
## plot; pass a larger height for faceted IRF grids. PDF keeps text as vector
## (sharp at any zoom); ragg::agg_png is used for raster output if requested.
ggsave_thesis <- function(filename, plot = ggplot2::last_plot(),
                          width = 14, height = 8.5, units = "cm",
                          dpi = 300, ...) {
  dev <- if (grepl("\\.png$", filename, ignore.case = TRUE) &&
             requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else NULL
  ggplot2::ggsave(filename = filename, plot = plot,
                  width = width, height = height, units = units,
                  dpi = dpi, device = dev, ...)
}
