# =============================================================================#
# 04_RESULTS
# -----------------------------------------------------------------------------#
# Reads the estimation results saved by 03_estimation.R and produces the
# impulse-response figures and coefficient tables for the thesis. This script
# does no estimation; re-run it freely for design tweaks without re-estimating.
#
# Effect scaling. The estimated interaction coefficients are per one raw unit of
# the OIS 20Y surprise. Every reported effect is scaled to the response to a
# ONE-STANDARD-DEVIATION EASING surprise and expressed in percent
# (coef x shock_sd x 100). The easing sign convention (Section 5.4) is already
# applied in 03 (coef is the response to an easing surprise). Note the binary and
# continuous specifications are in different units (binary group differential vs.
# per one standard deviation of the reserve); compare shape and precision, not
# levels.
#
# Outputs:
#   thesis/charts/20_irf_baseline_continuous.pdf   (6.1, two panels)
#   thesis/charts/21_irf_asymmetric_binary.pdf     (6.2)
#   thesis/charts/22_irf_asymmetric_continuous.pdf (6.2 / appendix)
#   thesis/charts/23_irf_robustness.pdf            (6.3, shape overlay)
#   thesis/charts/A1_irf_property_specific.pdf     (appendix)
#   thesis/tables/irf_baseline.tex                 (6.1, binary | continuous)
#   thesis/tables/robustness_focal.tex             (6.3, specs at h=7 and h=12)
#   thesis/tables/residual_dependence.tex          (appendix, real baseline resid)
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
  library(ggplot2)
})
source("code/plot_theme.R")

load("./data/estimation_results.RData")   # lp_results, lp_meta, resid_diag

THESIS_FIG_DIR <- "./thesis/charts"
LATEX_TAB_DIR  <- "./thesis/tables"
dir.create(THESIS_FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LATEX_TAB_DIR, recursive = TRUE, showWarnings = FALSE)

H_MAX <- lp_meta$n_horizons - 1L
SD    <- lp_meta$shock_sd

# Scale to percent response to a one-SD easing surprise.
pct <- function(x) 100 * x * SD
res <- lp_results %>%
  mutate(across(c(coef, ci_lo, ci_hi, se), pct))

sub <- function(s) res %>% filter(spec == s)

# Common x-axis theme tweak for horizon plots (horizontal integer labels).
horizon_x_theme <- theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
YLAB <- "Differential response to a 1-SD easing surprise (%)"

# -----------------------------------------------------------------------------#
# Figure 20: baseline (binary) and continuous reserve, two panels
# -----------------------------------------------------------------------------#
## Co-primary specifications side by side. Independent y-axes because the two are
## in different units; the caption states this. The panels invite comparison of
## SHAPE and PRECISION, not the height of the response.
base_cont <- bind_rows(
  sub("baseline")       %>% mutate(panel = "Binary classification"),
  sub("AS3_continuous") %>% mutate(panel = "Continuous reserve")
) %>% mutate(panel = factor(panel, levels = c("Binary classification", "Continuous reserve")))

p_base_cont <- ggplot(base_cont, aes(horizon, coef)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi),
              fill = thesis_primary[["inelastic"]], alpha = 0.18) +
  geom_line(colour = thesis_primary[["inelastic"]], linewidth = thesis_linewidth) +
  geom_point(colour = thesis_primary[["inelastic"]], size = 1.2) +
  facet_wrap(~panel, nrow = 1, scales = "free_y") +
  scale_x_horizon(H_MAX) +
  labs(x = "Horizon (quarters)", y = YLAB) +
  theme_thesis() + horizon_x_theme
ggsave_thesis(file.path(THESIS_FIG_DIR, "20_irf_baseline_continuous.pdf"),
              p_base_cont, height = 7)

# -----------------------------------------------------------------------------#
# Helper: two-line asymmetric IRF (easing vs tightening), shared y
# -----------------------------------------------------------------------------#
asym_plot <- function(spec_name, ylab = YLAB) {
  d <- sub(spec_name) %>%
    mutate(Term = ifelse(grepl("ease", term), "Easing surprise", "Tightening surprise"),
           Term = factor(Term, levels = c("Easing surprise", "Tightening surprise")))
  ggplot(d, aes(horizon, coef, colour = Term, fill = Term)) +
    geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = thesis_linewidth) +
    geom_point(size = 1.2) +
    facet_wrap(~Term, nrow = 1) +
    scale_x_horizon(H_MAX) +
    scale_colour_thesis() + scale_fill_thesis() +
    guides(colour = "none", fill = "none") +
    labs(x = "Horizon (quarters)", y = ylab) +
    theme_thesis() + horizon_x_theme
}

# Figure 21: binary asymmetric (easing | tightening facets).
p_asym_bin <- asym_plot("AS2_asymmetric")
ggsave_thesis(file.path(THESIS_FIG_DIR, "21_irf_asymmetric_binary.pdf"),
              p_asym_bin, height = 7)

# Figure 22: continuous asymmetric (easing | tightening facets).
p_asym_cont <- asym_plot("AS2c_asymmetric_continuous")
ggsave_thesis(file.path(THESIS_FIG_DIR, "22_irf_asymmetric_continuous.pdf"),
              p_asym_cont, height = 7)

# -----------------------------------------------------------------------------#
# Figure 23: robustness overlay (shape stability), point estimates only
# -----------------------------------------------------------------------------#
## Bands omitted for legibility; the point-estimate paths show that the shape and
## timing are stable across the shock instrument and the classification
## threshold, while excluding Vienna flattens the response.
rob <- bind_rows(
  sub("baseline"), sub("R1_no_vienna"), sub("R3_de20y"), sub("R4_threshold_p30")
) %>% mutate(Specification = factor(spec_label(spec),
             levels = c("Baseline", "Vienna excluded", "DE 20Y instrument", "Threshold: 30th pct")))
p_rob <- ggplot(rob, aes(horizon, coef, colour = Specification)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = thesis_linewidth) +
  geom_point(size = 1.1) +
  scale_x_horizon(H_MAX) +
  scale_colour_thesis() +
  labs(x = "Horizon (quarters)", y = YLAB, colour = NULL) +
  theme_thesis() + horizon_x_theme
ggsave_thesis(file.path(THESIS_FIG_DIR, "23_irf_robustness.pdf"), p_rob)

# -----------------------------------------------------------------------------#
# Appendix Figure A1: property-specific (house vs apartment)
# -----------------------------------------------------------------------------#
prop <- bind_rows(sub("AS1_house"), sub("AS1_apartment")) %>%
  mutate(Outcome = factor(spec_label(spec), levels = c("House", "Apartment")))
p_prop <- ggplot(prop, aes(horizon, coef, colour = Outcome, fill = Outcome)) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, colour = NA) +
  geom_line(linewidth = thesis_linewidth) +
  geom_point(size = 1.2) +
  scale_x_horizon(H_MAX) +
  scale_colour_thesis() + scale_fill_thesis() +
  labs(x = "Horizon (quarters)", y = YLAB, colour = NULL, fill = NULL) +
  theme_thesis() + horizon_x_theme
ggsave_thesis(file.path(THESIS_FIG_DIR, "A1_irf_property_specific.pdf"), p_prop)

# =============================================================================#
# TABLES
# =============================================================================#
tex_num <- function(x, d = 3) formatC(x, format = "f", digits = d)
fmt_ci  <- function(lo, hi) paste0("[", tex_num(lo), ", ", tex_num(hi), "]")

# -----------------------------------------------------------------------------#
# Table (6.1): baseline (binary) and continuous, selected horizons, two blocks
# -----------------------------------------------------------------------------#
sel <- c(0, 2, 4, 6, 8, 10, 12)
bin <- sub("baseline")       %>% filter(horizon %in% sel) %>% arrange(horizon)
con <- sub("AS3_continuous") %>% filter(horizon %in% sel) %>% arrange(horizon)
bc_rows <- paste0(
  bin$horizon, " & ",
  tex_num(bin$coef), " & ", tex_num(bin$se), " & ", fmt_ci(bin$ci_lo, bin$ci_hi), " & ",
  tex_num(con$coef), " & ", tex_num(con$se), " & ", fmt_ci(con$ci_lo, con$ci_hi), " \\\\"
)
tab_bc <- c(
  "\\begin{tabular}{r r r c r r c}",
  "\\toprule",
  " & \\multicolumn{3}{c}{Binary classification} & \\multicolumn{3}{c}{Continuous reserve} \\\\",
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  "Horizon $h$ & Estimate & SE & 95\\% CI & Estimate & SE & 95\\% CI \\\\",
  "\\midrule",
  bc_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tab_bc, file.path(LATEX_TAB_DIR, "irf_baseline.tex"), useBytes = TRUE)

# -----------------------------------------------------------------------------#
# Table (6.3): robustness, all specifications at focal horizons h = 7 and h = 12
# -----------------------------------------------------------------------------#
## One row per specification; two blocks (h=7 peak, h=12 long run) giving the
## point estimate and 95% CI. The asymmetric specifications contribute their
## easing term (the margin of interest); the tightening term is omitted here and
## shown in the asymmetric figures.
focal_specs <- c("baseline", "AS3_continuous", "AS2_asymmetric", "AS2c_asymmetric_continuous",
                 "R1_no_vienna", "R2_pre_tail", "R3_de20y", "R3_de30y",
                 "R4_threshold_p20", "R4_threshold_p30", "R5_lag_augmented")
focal_row <- function(s) {
  d <- res %>% filter(spec == s)
  # For asymmetric specs, report the easing term only.
  if (any(grepl("ease", d$term))) d <- d %>% filter(grepl("ease", term))
  h7  <- d %>% filter(horizon == 7)
  h12 <- d %>% filter(horizon == 12)
  paste0(spec_label(s), " & ",
         tex_num(h7$coef),  " & ", fmt_ci(h7$ci_lo,  h7$ci_hi),  " & ",
         tex_num(h12$coef), " & ", fmt_ci(h12$ci_lo, h12$ci_hi), " \\\\")
}
rob_rows <- vapply(focal_specs, focal_row, character(1))
tab_rob <- c(
  "\\begin{tabular}{l r c r c}",
  "\\toprule",
  " & \\multicolumn{2}{c}{Horizon $h = 7$} & \\multicolumn{2}{c}{Horizon $h = 12$} \\\\",
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  "Specification & Estimate & 95\\% CI & Estimate & 95\\% CI \\\\",
  "\\midrule",
  rob_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tab_rob, file.path(LATEX_TAB_DIR, "robustness_focal.tex"), useBytes = TRUE)

# -----------------------------------------------------------------------------#
# Appendix table: residual dependence of the baseline (from resid_diag)
# -----------------------------------------------------------------------------#
rd_rows <- paste0(
  resid_diag$horizon, " & ",
  tex_num(resid_diag$residual_ar1, 2), " & ",
  tex_num(resid_diag$mean_abs_pairwise, 2), " & ",
  tex_num(resid_diag$max_abs_pairwise, 2), " \\\\"
)
tab_rd <- c(
  "\\begin{tabular}{r r r r}",
  "\\toprule",
  "Horizon $h$ & Residual AR(1) & Mean $|\\rho|$ & Max $|\\rho|$ \\\\",
  "\\midrule",
  rd_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(tab_rd, file.path(LATEX_TAB_DIR, "residual_dependence.tex"), useBytes = TRUE)

message("Results figures and tables written.")
message("shock_sd used for scaling: ", round(SD, 4))
message("Charts: ", normalizePath(THESIS_FIG_DIR, winslash = "/", mustWork = FALSE))
message("Tables: ", normalizePath(LATEX_TAB_DIR, winslash = "/", mustWork = FALSE))
