# =============================================================================#
# 02_DIAGNOSTICS
# -----------------------------------------------------------------------------#
# Produces exactly the figures and tables that enter the thesis, and nothing
# else. This script does not modify 00_data_prep.R, 01_data_gen.R, or their
# saved objects; all diagnostic-only flags and transformations are built in
# memory.
#
# Outputs:
#   thesis/charts/02_price_indexes_property_type.pdf   (Figure, Ch. 4.1)
#   thesis/charts/10_shock_signed_components.pdf        (Figure, Ch. 4.2)
#   thesis/charts/05_acf_pacf_composite_growth.pdf      (Figure, Ch. 4.1)
#   thesis/charts/14_key_correlation_heatmap.pdf        (Figure, Ch. 4.4)
#   thesis/tables/summary_stats.tex                     (Appendix Table 1a)
#   thesis/tables/panel_integrity.tex                   (Appendix Table 1b)
#   data/acf_pacf_dQ_composite.csv   (ACF/pACF values)
# =============================================================================#

# Resolve the repository root when run via Rscript from any working directory.
cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) == 1L) {
  script_path <- normalizePath(sub("^--file=", "", file_arg), winslash = "/", mustWork = TRUE)
  setwd(dirname(dirname(script_path)))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(zoo)
  library(ggplot2)
})

# Shared thesis figure theme / palette / sizing (theme_thesis, scale_*_thesis,
# ggsave_thesis, scale_x_quarter, scale_y_symmetric, thesis_linewidth).
source("code/plot_theme.R")

# Data inputs
load("./data/01_panel.RData")
load("./data/00_prep_output.RData")

# Chapter-ready figures (theme_thesis) exported here for \includegraphics.
THESIS_FIG_DIR <- "./thesis/charts"
# LaTeX table fragments (booktabs) \input in the appendix.
LATEX_TAB_DIR  <- "./thesis/tables"
dir.create(THESIS_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LATEX_TAB_DIR, recursive = TRUE, showWarnings = FALSE)

options(scipen = 6, dplyr.summarise.inform = FALSE)

# -----------------------------------------------------------------------------#
# Helpers
# -----------------------------------------------------------------------------#
q_label <- function(x) format(x, "%Y Q%q")

# Pooled numeric summary over district-quarter observations (finite values).
numeric_summary <- function(data, vars) {
  bind_rows(lapply(vars, function(v) {
    z <- data[[v]]
    z <- z[is.finite(z)]
    if (!length(z)) return(tibble(variable = v, n = 0L))
    tibble(
      variable = v, n = length(z), mean = mean(z), sd = sd(z),
      min = min(z), p01 = unname(quantile(z, .01)),
      p25 = unname(quantile(z, .25)), median = median(z),
      p75 = unname(quantile(z, .75)), p99 = unname(quantile(z, .99)),
      max = max(z)
    )
  }))
}

# Long form of a correlation matrix (for the heatmap tiles).
cor_long <- function(cmat) {
  as.data.frame(as.table(cmat), stringsAsFactors = FALSE) %>%
    rename(variable_1 = Var1, variable_2 = Var2, correlation = Freq)
}

# Count duplicate key rows for the input join-contract check.
duplicate_rows <- function(data, keys) {
  data %>% count(across(all_of(keys)), name = "n") %>%
    summarise(x = sum(pmax(n - 1L, 0L))) %>% pull(x)
}

# Pooled within-district autocorrelation (ACF): cor(x_t, x_{t-k}) pooled across
# districts, with the lag built inside each district so it never crosses a
# district boundary.
pooled_acf <- function(data, variable, max_lag = 4L) {
  bind_rows(lapply(seq_len(max_lag), function(k) {
    tmp <- data %>% arrange(district_id, quarter) %>% group_by(district_id) %>%
      transmute(x = .data[[variable]], x_lag = dplyr::lag(.data[[variable]], k)) %>%
      ungroup() %>% filter(is.finite(x), is.finite(x_lag))
    tibble(variable = variable, lag = k, correlation = cor(tmp$x, tmp$x_lag), n_pairs = nrow(tmp))
  }))
}

## Pooled within-district partial autocorrelation (pACF).
## For each lag k, the partial autocorrelation is the coefficient on the k-th lag
## in a within-district AR(k) regression of the series on its first k own lags.
## The lags are built inside each district (dplyr::lag after grouping) so they
## never cross a district boundary, then the districts are stacked and a single
## pooled OLS is run per k. District fixed effects (factor(district_id)) are
## included so the estimate reflects within-district dynamics, matching the
## "pooling the within-district changes" reading used for the ACF above.
## Returns, per lag, the pACF value and the balanced n used for that regression.
pooled_pacf <- function(data, variable, max_lag = 4L) {
  # Build all needed lag columns once, within district, so lags never cross a
  # district boundary and every lag column is row-aligned with x.
  lag_frame <- data %>% arrange(district_id, quarter) %>% group_by(district_id) %>%
    transmute(x = .data[[variable]])
  for (j in seq_len(max_lag)) {
    lag_frame <- lag_frame %>% mutate(!!paste0("x_lag", j) := dplyr::lag(x, j))
  }
  lag_frame <- ungroup(lag_frame)

  bind_rows(lapply(seq_len(max_lag), function(k) {
    lag_cols <- paste0("x_lag", seq_len(k))
    sub <- lag_frame[stats::complete.cases(lag_frame[, c("x", lag_cols)]), ]
    form <- stats::as.formula(
      paste("x ~", paste(lag_cols, collapse = " + "), "+ factor(district_id)"))
    fit <- stats::lm(form, data = sub)
    tibble(variable = variable, lag = k,
           correlation = unname(coef(fit)[paste0("x_lag", k)]),
           n_pairs = nrow(sub))
  }))
}

# -----------------------------------------------------------------------------#
# Panel: diagnostic-only labels and required-field check
# -----------------------------------------------------------------------------#
required <- c(
  "district_id", "district_name", "quarter", "p_composite", "efh_avg", "etw_avg",
  "w_house", "w_apt", "blr_14", "inelastic_dummy", "shock", "shock_ease",
  "shock_tight", "shk_x_inel", "population", "unemp_rate", "income_pc",
  "log_income_pc", "income_source", paste0("dep_", sprintf("%02d", 0:12)),
  paste0("dQ_lag", 1:4), paste0("d_lincome_lag", 1:4),
  paste0("d_lpop_lag", 1:4), paste0("d_unemp_lag", 1:4)
)
missing_required <- setdiff(required, names(panel))
if (length(missing_required)) stop("Required panel fields missing: ", paste(missing_required, collapse = ", "))

panel_d <- panel %>%
  mutate(
    year = as.integer(format(quarter, "%Y")),
    bundesland_code = floor(district_id / 100),
    vienna = if_else(bundesland_code == 9, "Vienna", "Outside Vienna"),
    supply_group = if_else(inelastic_dummy == 1, "Inelastic", "Elastic")
  )

n_d <- n_distinct(panel_d$district_id)
n_t <- n_distinct(panel_d$quarter)
control_lags <- c(paste0("dQ_lag", 1:4), paste0("d_lincome_lag", 1:4),
                  paste0("d_lpop_lag", 1:4), paste0("d_unemp_lag", 1:4))
model_x <- c("shk_x_inel", control_lags)

# -----------------------------------------------------------------------------#
# Input join-key contracts (feeds Appendix Table 1b, panel_integrity.tex)
# -----------------------------------------------------------------------------#
panel_ids     <- panel_d %>% distinct(district_id)
panel_quarters<- panel_d %>% distinct(quarter)
panel_keys    <- panel_d %>% distinct(district_id, quarter)
input_contracts <- tibble(
  object = c("prices", "composite_weights", "bl", "shock_q", "c_pop_long", "c_unemp_long", "c_inc_q"),
  duplicate_key_rows = c(
    duplicate_rows(prices, c("district_id", "quarter")), duplicate_rows(composite_weights, "district_id"),
    duplicate_rows(bl, "district_id"), duplicate_rows(shock_q, "quarter"),
    duplicate_rows(c_pop_long, c("district_id", "quarter")),
    duplicate_rows(c_unemp_long, c("district_id", "quarter")),
    duplicate_rows(c_inc_q, c("district_id", "quarter"))
  ),
  panel_keys_missing_from_input = c(
    nrow(anti_join(panel_keys, prices, by = c("district_id", "quarter"))),
    nrow(anti_join(panel_ids, composite_weights, by = "district_id")),
    nrow(anti_join(panel_ids, bl, by = "district_id")),
    nrow(anti_join(panel_quarters, shock_q, by = "quarter")),
    nrow(anti_join(panel_keys, c_pop_long, by = c("district_id", "quarter"))),
    nrow(anti_join(panel_keys, c_unemp_long, by = c("district_id", "quarter"))),
    nrow(anti_join(panel_keys, c_inc_q, by = c("district_id", "quarter")))
  )
) %>% mutate(status = if_else(duplicate_key_rows == 0 & panel_keys_missing_from_input == 0, "PASS", "CHECK"))

# -----------------------------------------------------------------------------#
# Price outcomes: summaries (feed Table 1a) and the price-index figure
# -----------------------------------------------------------------------------#
price_summary <- numeric_summary(panel_d, c("efh_avg", "etw_avg", "p_composite", "dQ_composite"))

static_district <- panel_d %>% distinct(district_id, district_name, bundesland_code, vienna,
                                        blr_14, inelastic_dummy, supply_group, w_house, w_apt)

quarterly_prices <- panel_d %>% group_by(quarter) %>% summarise(
  Composite = median(p_composite), EFH = median(efh_avg), ETW = median(etw_avg), .groups = "drop"
) %>% pivot_longer(-quarter, names_to = "series", values_to = "value") %>%
  group_by(series) %>% arrange(quarter) %>% mutate(index = 100 * value / first(value)) %>% ungroup()

## Thesis export: readable series labels, thesis theme/palette, no in-chart title
## (added in the LaTeX caption). Saved to thesis/charts at the 14 cm figure width.
quarterly_prices_thesis <- quarterly_prices %>%
  mutate(series = recode(series,
                         Composite = "Composite",
                         EFH = "House",
                         ETW = "Apartment"),
         series = factor(series, levels = c("Composite", "House", "Apartment")))
p_price_thesis <- ggplot(quarterly_prices_thesis, aes(quarter, index, colour = series)) +
  geom_line(linewidth = thesis_linewidth) +
  scale_x_quarter() +
  # Relabel the vertical axis at 100/120/140/160 without rescaling the data
  # (no limits set, so the underlying series are unchanged; only breaks shown).
  scale_y_continuous(breaks = c(100, 120, 140, 160)) +
  scale_colour_thesis() +
  labs(x = NULL, y = "Index (Q1 2017 = 100)", colour = NULL) +
  theme_thesis()
ggsave_thesis(file.path(THESIS_FIG_DIR, "02_price_indexes_property_type.pdf"), p_price_thesis)

# -----------------------------------------------------------------------------#
# Composite-price growth: ACF and pACF figure (motivates the four control lags)
# -----------------------------------------------------------------------------#
## The chart motivates the four price-momentum control lags in Chapter 4.1. ACF
## conveys the persistence (momentum) of price growth; pACF isolates the extra
## content of each further lag, which is the lag-order-selection diagnostic. Both
## panels are shown to lag 8 so the decay past lag 4 is visible. The dashed lines
## are the approximate white-noise significance band, +/- 1.96 / sqrt(n), using
## the balanced n of the pACF regressions. It is approximate because pooled panel
## residuals carry cross-sectional dependence not captured by the iid band.
acf_pacf_lags <- 8L
acf_dq  <- pooled_acf(panel_d,  "dQ_composite", acf_pacf_lags) %>% mutate(kind = "ACF")
pacf_dq <- pooled_pacf(panel_d, "dQ_composite", acf_pacf_lags) %>% mutate(kind = "pACF")
acf_pacf_dq <- bind_rows(acf_dq, pacf_dq) %>%
  mutate(kind = factor(kind, levels = c("ACF", "pACF")))
# Significance band from the smallest (most conservative) n across displayed lags.
acf_pacf_band <- 1.96 / sqrt(min(acf_pacf_dq$n_pairs))
# Written to ./data (gitignored) as the machine-readable companion to the chart,
# so the exact ACF/pACF values cited in Chapter 4.1 can be re-read for verification.
write.csv(acf_pacf_dq %>% select(kind, lag, correlation, n_pairs) %>%
            mutate(sig_band = acf_pacf_band),
          file.path("./data", "acf_pacf_dQ_composite.csv"),
          row.names = FALSE, na = "", fileEncoding = "UTF-8")

p_acf_pacf_thesis <- ggplot(acf_pacf_dq, aes(factor(lag), correlation)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_hline(yintercept =  acf_pacf_band, linetype = 2, colour = "grey35", linewidth = 0.3) +
  geom_hline(yintercept = -acf_pacf_band, linetype = 2, colour = "grey35", linewidth = 0.3) +
  geom_col(aes(fill = kind), width = 0.65) +
  facet_wrap(~kind, nrow = 1) +
  scale_fill_thesis() +
  guides(fill = "none") +
  labs(x = "Lag (quarters)", y = "Correlation") +
  theme_thesis() +
  # Integer lag labels read better horizontally than at the theme's 45 degrees.
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
ggsave_thesis(file.path(THESIS_FIG_DIR, "05_acf_pacf_composite_growth.pdf"),
              p_acf_pacf_thesis, height = 7)

# -----------------------------------------------------------------------------#
# Composite weights and supply reserve: summary (feeds Table 1a)
# -----------------------------------------------------------------------------#
weight_summary <- numeric_summary(static_district, c("w_house", "w_apt", "blr_14"))

# -----------------------------------------------------------------------------#
# Monetary-policy shock: summary (feeds Table 1a) and the signed-components figure
# -----------------------------------------------------------------------------#
shock_panel <- panel_d %>% distinct(quarter, shock_net_ois20y, shock_pos_ois20y,
                                    shock_neg_ois20y, shock_abs_ois20y,
                                    n_nonmiss_ois20y, n_events) %>%
  left_join(shock_cancel_ratio %>% select(quarter, cancel_ratio_ois20y), by = "quarter") %>%
  arrange(quarter) %>% mutate(quarter_label = q_label(quarter))

shock_summary <- numeric_summary(shock_panel, c("shock_net_ois20y", "shock_pos_ois20y",
                                                "shock_neg_ois20y", "shock_abs_ois20y"))

shock_components <- shock_panel %>% select(quarter, Easing = shock_neg_ois20y,
                                            Tightening = shock_pos_ois20y) %>%
  pivot_longer(-quarter, names_to = "component", values_to = "value")

## Thesis export: signed easing/tightening components as bars, with the quarterly
## NET shock overlaid as a scatter on top of the stacked signed bars. Net data
## comes from shock_panel (one row per quarter). Fill uses two palette colours;
## the net overlay is drawn as points in a dark neutral so it reads against both
## bar colours (no connecting line, the points are not a continuous series).
shock_net_thesis <- shock_panel %>% select(quarter, net = shock_net_ois20y)
p_components_thesis <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(data = shock_components,
           aes(quarter, value, fill = component),
           position = "identity", alpha = .85, width = .18) +
  geom_point(data = shock_net_thesis, aes(quarter, net, shape = "Net shock"),
             colour = "grey15", size = 1.6) +
  scale_x_quarter() +
  # Symmetric vertical axis: equal positive/negative magnitude around zero,
  # covering both the signed bars and the net-shock points.
  scale_y_symmetric(c(shock_components$value, shock_net_thesis$net)) +
  scale_fill_thesis() +
  scale_shape_manual(values = c("Net shock" = 16)) +
  labs(x = NULL, y = "Quarterly shock sum (OIS 20Y)", fill = NULL, shape = NULL) +
  theme_thesis()
ggsave_thesis(file.path(THESIS_FIG_DIR, "10_shock_signed_components.pdf"), p_components_thesis)

# -----------------------------------------------------------------------------#
# Control levels: summary (feeds Table 1a)
# -----------------------------------------------------------------------------#
## Level summaries for the three controls: the panel repeats annual figures within
## the year, so N matches the district-quarter panel.
control_summary <- numeric_summary(panel_d, c("population", "unemp_rate", "income_pc"))

# -----------------------------------------------------------------------------#
# Key-variable correlation heatmap figure
# -----------------------------------------------------------------------------#
key_corr_vars <- c("dQ_composite", "d_lincome", "d_lpop", "d_unemp", "shock",
                   "shock_ease", "shock_tight", "blr_14", "w_house", "shk_x_inel")
key_corr <- cor(panel_d[, key_corr_vars], use = "pairwise.complete.obs")

## Thesis export: readable axis labels (no raw variable names), thesis theme, and
## a diverging fill scale kept for the correlation surface.
corr_labels <- c(
  dQ_composite = "Composite price growth",
  d_lincome    = "Income growth",
  d_lpop       = "Population growth",
  d_unemp      = "Unemployment change",
  shock        = "Monetary policy shock",
  shock_ease   = "Easing surprise",
  shock_tight  = "Tightening surprise",
  blr_14       = "Building-land reserve",
  w_house      = "House weight",
  shk_x_inel   = "Shock \u00d7 inelastic"
)
relabel_corr <- function(x) ifelse(is.na(corr_labels[x]), x, corr_labels[x])
key_corr_long_thesis <- cor_long(key_corr) %>%
  mutate(
    variable_1 = factor(relabel_corr(variable_1), levels = unname(corr_labels[key_corr_vars])),
    variable_2 = factor(relabel_corr(variable_2), levels = unname(corr_labels[key_corr_vars]))
  )
p_key_corr_thesis <- ggplot(key_corr_long_thesis,
                            aes(variable_1, variable_2, fill = correlation)) +
  geom_tile(colour = "white", linewidth = .3) +
  geom_text(aes(label = sprintf("%.2f", correlation)), size = 2.4) +
  scale_fill_thesis_c(limits = c(-1, 1), midpoint = 0) +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        legend.position = "right")
## Square heatmap: use a taller output than the default single-plot height.
ggsave_thesis(file.path(THESIS_FIG_DIR, "14_key_correlation_heatmap.pdf"),
              p_key_corr_thesis, height = 13)

# =============================================================================#
# APPENDIX TABLES (LaTeX fragments for the thesis)
# -----------------------------------------------------------------------------#
## Two booktabs fragments written to thesis/tables and \input in the appendix:
##   summary_stats.tex    -> Table 1a: summary statistics of the analysis
##                           variables (outcome, shock, supply, controls).
##   panel_integrity.tex  -> Table 1b: merge and integrity diagnostics.
## Fragments contain only tabular code, so LaTeX typesets them in the document
## font (Latin Modern). Readable variable labels are used, never code names.

## Readable labels for every variable that appears in the summary table.
tex_var_labels <- c(
  p_composite      = "Composite price (\\euro/m\\textsuperscript{2})",
  efh_avg          = "House price (\\euro/m\\textsuperscript{2})",
  etw_avg          = "Apartment price (\\euro/m\\textsuperscript{2})",
  dQ_composite     = "Composite price growth (log change)",
  shock_net_ois20y = "Net shock (OIS 20Y)",
  shock_pos_ois20y = "Tightening component",
  shock_neg_ois20y = "Easing component",
  blr_14           = "Building land reserve (\\%)",
  w_house          = "House weight",
  population       = "Population",
  unemp_rate       = "Unemployment rate (\\%)",
  income_pc        = "Net income per capita (\\euro)"
)

## Group thousands with a LaTeX thin space (\,) to match the document number
## style. Applied after formatting so the separator is reliably the escape,
## not a literal comma.
tex_group <- function(s) {
  gsub("(?<=\\d)(?=(\\d{3})+(?!\\d))", "\\\\,", s, perl = TRUE)
}
tex_num <- function(x, digits) {
  mapply(function(v, d) {
    if (is.na(v)) return("")
    body <- formatC(v, format = "f", digits = d, big.mark = "")
    neg  <- startsWith(body, "-")
    body <- sub("^-", "", body)
    intpart <- sub("\\..*$", "", body)
    decpart <- if (grepl("\\.", body)) sub("^[^.]*", "", body) else ""
    paste0(if (neg) "-" else "", tex_group(intpart), decpart)
  }, x, digits)
}
tex_int <- function(x) tex_group(formatC(x, format = "d", big.mark = ""))
## Per-variable rounding: prices/population as whole-ish numbers, growth and
## weights to more decimals, rates to one decimal. `digits` is one value per var.
summary_block <- function(summ, vars, digits) {
  s <- summ[match(vars, summ$variable), ]
  data.frame(
    label  = tex_var_labels[vars],
    n      = tex_int(s$n),
    mean   = tex_num(s$mean, digits),
    sd     = tex_num(s$sd, digits),
    min    = tex_num(s$min, digits),
    median = tex_num(s$median, digits),
    max    = tex_num(s$max, digits),
    stringsAsFactors = FALSE
  )
}
blk_outcome <- summary_block(price_summary,
                             c("p_composite", "efh_avg", "etw_avg", "dQ_composite"),
                             digits = c(0, 0, 0, 4))
blk_shock   <- summary_block(shock_summary,
                             c("shock_net_ois20y", "shock_neg_ois20y", "shock_pos_ois20y"),
                             digits = c(2, 2, 2))
blk_supply  <- summary_block(weight_summary, c("blr_14", "w_house"),
                             digits = c(1, 3))
blk_control <- summary_block(control_summary, c("population", "unemp_rate", "income_pc"),
                             digits = c(0, 1, 0))

emit_rows <- function(df) paste0(
  df$label, " & ", df$n, " & ", df$mean, " & ", df$sd, " & ",
  df$min, " & ", df$median, " & ", df$max, " \\\\"
)
group_row <- function(title) paste0("\\addlinespace\n\\multicolumn{7}{l}{\\itshape ",
                                    title, "} \\\\")

tab1a <- c(
  "\\begin{tabular}{l r r r r r r}",
  "\\toprule",
  "Variable & N & Mean & SD & Min & Median & Max \\\\",
  "\\midrule",
  group_row("Outcome"), emit_rows(blk_outcome),
  group_row("Monetary policy shock"), emit_rows(blk_shock),
  group_row("Supply measure"), emit_rows(blk_supply),
  group_row("Controls"), emit_rows(blk_control),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tab1a, file.path(LATEX_TAB_DIR, "summary_stats.tex"), useBytes = TRUE)

## Table 1b: integrity / merge diagnostics. Condenses the structural checks and
## the per-input join contracts into one readable panel.
n_inputs <- nrow(input_contracts)
n_clean  <- sum(input_contracts$status == "PASS")
integ_rows <- c(
  paste0("Balanced panel (districts $\\times$ quarters) & 116 $\\times$ 36 = 4\\,176 \\\\"),
  paste0("Unique district-quarter keys & 4\\,176 (no duplicates) \\\\"),
  paste0("Missing or unexpected keys & 0 \\\\"),
  paste0("Districts observed in all 36 quarters & 116 \\\\"),
  paste0("Source inputs joined without key errors & ", n_clean, " of ", n_inputs, " \\\\"),
  paste0("Non-positive or non-finite price cells & 0 \\\\"),
  paste0("Weight-sum and composite identity errors & 0 \\\\")
)
tab1b <- c(
  "\\begin{tabular}{l r}",
  "\\toprule",
  "Diagnostic & Result \\\\",
  "\\midrule",
  integ_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tab1b, file.path(LATEX_TAB_DIR, "panel_integrity.tex"), useBytes = TRUE)

message("Diagnostics complete.")
message("Charts: ", normalizePath(THESIS_FIG_DIR, winslash = "/", mustWork = FALSE))
message("Tables: ", normalizePath(LATEX_TAB_DIR, winslash = "/", mustWork = FALSE))
message("ACF/pACF CSV: ", normalizePath("./data/acf_pacf_dQ_composite.csv", winslash = "/", mustWork = FALSE))
