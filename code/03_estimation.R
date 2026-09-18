# =============================================================================#
# 03_ESTIMATION
# -----------------------------------------------------------------------------#
# Estimates the panel local projections of the empirical strategy (Chapter 5)
# and saves a single tidy results object. Producing figures and tables is the
# job of 04_results.R, which reads the saved object; this script does no
# plotting or LaTeX output.
#
# Model (baseline, per horizon h = 0..12):
#   dep_h ~ beta_h * (Shock_t x Inelastic_i) + theta_h' X_{i,t}
#           | district_id + quarter
#   estimated by OLS with two-way fixed effects absorbed (within transformation),
#   Driscoll-Kraay standard errors with a horizon-aware lag truncation L = max(h, 1).
#
# Specifications estimated (see Section 5.7):
#   baseline                      composite outcome, binary inelastic interaction
#   AS1  property-specific        house (EFH) and apartment (ETW) outcomes
#   AS2  asymmetric               easing and tightening interactions separately
#   AS3  continuous reserve       standardized reserve (oriented high = inelastic)
#   R1   Vienna excluded
#   R2   pre-tail sample          income_source == "observed"
#   R3   alternative instruments  DE20Y and DE30Y surprises
#   R4   alternative threshold    inelastic dummy at the 20th and 30th percentile
#   R5   lag-augmented inference  lags of the dependent variable, robust SE
#
# Output: data/estimation_results.RData (tidy frame `lp_results`, gitignored).
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
  library(fixest)
})

load("./data/01_panel.RData")

H_MAX <- 12L
CONTROL_LAGS <- c(paste0("dQ_lag", 1:4), paste0("d_lincome_lag", 1:4),
                  paste0("d_lpop_lag", 1:4), paste0("d_unemp_lag", 1:4))
CONTROLS_RHS <- paste(CONTROL_LAGS, collapse = " + ")

# Sign convention (Section 5.4): a negative OIS 20Y surprise denotes easing, so
# the shock is negative in easing quarters. To report the response to a monetary
# EASING surprise of a given size (so a price increase reads as positive), the
# estimated interaction coefficients are multiplied by -1 when reported. The raw
# estimate is kept alongside so nothing is hidden.
SIGN <- -1

# -----------------------------------------------------------------------------#
# Derived regressors specific to estimation (built here, not in the panel)
# -----------------------------------------------------------------------------#
panel <- panel %>% mutate(bundesland_code = floor(district_id / 100))

# AS3: continuous supply measure. Standardize the reserve over the 116 DISTINCT
# districts (not the pooled district-quarter rows, which would count each
# district 36 times), and orient it so that a HIGHER value means MORE inelastic,
# matching the binary dummy (dummy = 1 for low-reserve, supply-inelastic
# districts). Achieved by standardizing the negated reserve.
district_reserve <- panel %>% distinct(district_id, blr_14)
blr_mean <- mean(district_reserve$blr_14, na.rm = TRUE)
blr_sd   <- sd(district_reserve$blr_14, na.rm = TRUE)
panel <- panel %>%
  mutate(
    inel_cont = -(blr_14 - blr_mean) / blr_sd,          # high = more inelastic
    shk_x_inelcont = shock * inel_cont,                 # continuous interaction
    # Continuous asymmetric decomposition (mirrors AS2 on the continuous measure).
    shk_ease_x_inelcont  = shock_ease  * inel_cont,
    shk_tight_x_inelcont = shock_tight * inel_cont,
    # R3: alternative-instrument interactions (net DE20Y / DE30Y x binary dummy)
    shk_de20y_x_inel = shock_net_de20y * inelastic_dummy,
    shk_de30y_x_inel = shock_net_de30y * inelastic_dummy
  )

# R4: alternative classification thresholds. The baseline dummy uses the 25th
# percentile of the reserve outside Vienna. Re-derive the dummy at the 20th and
# 30th percentiles (same outside-Vienna reference), applied to all districts.
reserve_outside_vienna <- panel %>%
  filter(bundesland_code != 9) %>% distinct(district_id, blr_14) %>% pull(blr_14)
thr20 <- quantile(reserve_outside_vienna, 0.20, na.rm = TRUE)
thr30 <- quantile(reserve_outside_vienna, 0.30, na.rm = TRUE)
panel <- panel %>%
  mutate(
    inelastic_p20 = as.integer(blr_14 <= thr20),
    inelastic_p30 = as.integer(blr_14 <= thr30),
    shk_x_inel_p20 = shock * inelastic_p20,
    shk_x_inel_p30 = shock * inelastic_p30
  )

# -----------------------------------------------------------------------------#
# Estimation helper: one local projection sequence over horizons
# -----------------------------------------------------------------------------#
## Fits, for each horizon h in `horizons`, the model
##   <dep_prefix>_h ~ <rhs> | district_id + quarter
## on `data`, with Driscoll-Kraay standard errors at lag L = max(h, 1), and
## returns a tidy frame of the requested coefficient(s). `keep_terms` names the
## regressor(s) to extract (e.g. the interaction). `spec` labels the run.
## The reported coefficient applies the easing sign convention (SIGN); the raw
## estimate is retained in `coef_raw`.
## `weight_var`, if given, names a column used as regression weights (WLS). This
## implements the stock-weighted property-specific specification (AS1b), where
## each segment regression weights district-quarters by that segment's fixed
## stock share, so a district counts for a property type in proportion to how
## economically relevant that type is locally.
lp_run <- function(data, dep_prefix, rhs, keep_terms, spec,
                   horizons = 0:H_MAX, apply_sign = TRUE, weight_var = NULL) {
  wt <- if (is.null(weight_var)) NULL else as.formula(paste0("~ ", weight_var))
  bind_rows(lapply(horizons, function(h) {
    dep <- paste0(dep_prefix, sprintf("%02d", h))
    fml <- as.formula(paste0(dep, " ~ ", rhs, " | district_id + quarter"))
    fit <- feols(fml, data = data, panel.id = ~ district_id + quarter,
                 weights = wt, notes = FALSE, warn = FALSE)
    L <- max(h, 1L)
    # Driscoll-Kraay vcov with horizon-aware lag truncation.
    est <- summary(fit, vcov = DK ~ quarter, lag = L)
    ct  <- coeftable(est)
    bind_rows(lapply(keep_terms, function(term) {
      if (!term %in% rownames(ct)) return(NULL)
      b  <- ct[term, "Estimate"]
      se <- ct[term, "Std. Error"]
      s  <- if (apply_sign) SIGN else 1
      tibble(
        spec = spec, term = term, horizon = h,
        coef_raw = b, coef = s * b, se = se,
        ci_lo = s * b - 1.96 * se, ci_hi = s * b + 1.96 * se,
        # ci bounds are symmetric around the (signed) point estimate; recompute
        # so lo < hi regardless of the sign flip.
        n = est$nobs, dk_lag = L
      )
    }))
  })) %>%
    mutate(ci_lo2 = pmin(ci_lo, ci_hi), ci_hi = pmax(ci_lo, ci_hi), ci_lo = ci_lo2) %>%
    select(-ci_lo2)
}

# -----------------------------------------------------------------------------#
# Baseline and alternative specifications
# -----------------------------------------------------------------------------#
lp_baseline <- lp_run(panel, "dep_", paste0("shk_x_inel + ", CONTROLS_RHS),
                      keep_terms = "shk_x_inel", spec = "baseline")

# AS1 property-specific outcomes (house and apartment), naive equal-weight split
# with the binary interaction. This is the motivating (uninformative) version:
# it estimates each segment's response with equal weight across all districts,
# including those where the segment is economically marginal.
lp_efh <- lp_run(panel, "dep_efh_", paste0("shk_x_inel + ", CONTROLS_RHS),
                 keep_terms = "shk_x_inel", spec = "AS1_house")
lp_etw <- lp_run(panel, "dep_etw_", paste0("shk_x_inel + ", CONTROLS_RHS),
                 keep_terms = "shk_x_inel", spec = "AS1_apartment")

# AS1b stock-weighted property-specific specification. Each segment regression
# weights district-quarters by that segment's fixed stock share (house by
# w_house, apartment by w_apt), so a district counts for a property type in
# proportion to how relevant that type is locally, and thinly-represented
# segments contribute little noise. Uses the continuous reserve interaction (the
# co-primary moderator), since weighting toward apartment-heavy districts would
# otherwise compress the variation of the binary dummy.
lp_efh_w <- lp_run(panel, "dep_efh_", paste0("shk_x_inelcont + ", CONTROLS_RHS),
                   keep_terms = "shk_x_inelcont", spec = "AS1b_house_weighted",
                   weight_var = "w_house")
lp_etw_w <- lp_run(panel, "dep_etw_", paste0("shk_x_inelcont + ", CONTROLS_RHS),
                   keep_terms = "shk_x_inelcont", spec = "AS1b_apartment_weighted",
                   weight_var = "w_apt")

# AS2 asymmetric: easing and tightening interactions entered together.
lp_asym <- lp_run(panel, "dep_",
                  paste0("shk_ease_x_inel + shk_tight_x_inel + ", CONTROLS_RHS),
                  keep_terms = c("shk_ease_x_inel", "shk_tight_x_inel"),
                  spec = "AS2_asymmetric")

# AS3 continuous reserve (standardized, oriented high = inelastic).
lp_cont <- lp_run(panel, "dep_", paste0("shk_x_inelcont + ", CONTROLS_RHS),
                  keep_terms = "shk_x_inelcont", spec = "AS3_continuous")

# AS2 on the continuous measure: asymmetric decomposition mirrored on the
# continuous reserve, so the ease/tight split is available for the co-primary
# specification as well as the binary one.
lp_asym_cont <- lp_run(panel, "dep_",
                       paste0("shk_ease_x_inelcont + shk_tight_x_inelcont + ", CONTROLS_RHS),
                       keep_terms = c("shk_ease_x_inelcont", "shk_tight_x_inelcont"),
                       spec = "AS2c_asymmetric_continuous")

# -----------------------------------------------------------------------------#
# Robustness checks
# -----------------------------------------------------------------------------#
# R1 Vienna excluded.
lp_novienna <- lp_run(panel %>% filter(bundesland_code != 9), "dep_",
                      paste0("shk_x_inel + ", CONTROLS_RHS),
                      keep_terms = "shk_x_inel", spec = "R1_no_vienna")

# R2 pre-tail sample (observed income only).
lp_pretail <- lp_run(panel %>% filter(income_source == "observed"), "dep_",
                     paste0("shk_x_inel + ", CONTROLS_RHS),
                     keep_terms = "shk_x_inel", spec = "R2_pre_tail")

# R3 alternative shock instruments (DE20Y, DE30Y).
lp_de20y <- lp_run(panel, "dep_", paste0("shk_de20y_x_inel + ", CONTROLS_RHS),
                   keep_terms = "shk_de20y_x_inel", spec = "R3_de20y")
lp_de30y <- lp_run(panel, "dep_", paste0("shk_de30y_x_inel + ", CONTROLS_RHS),
                   keep_terms = "shk_de30y_x_inel", spec = "R3_de30y")

# R4 alternative classification thresholds (20th and 30th percentile).
lp_thr20 <- lp_run(panel, "dep_", paste0("shk_x_inel_p20 + ", CONTROLS_RHS),
                   keep_terms = "shk_x_inel_p20", spec = "R4_threshold_p20")
lp_thr30 <- lp_run(panel, "dep_", paste0("shk_x_inel_p30 + ", CONTROLS_RHS),
                   keep_terms = "shk_x_inel_p30", spec = "R4_threshold_p30")

# R5 lag-augmented inference (Montiel Olea & Plagborg-Moller, 2021). Lag
# augmentation adds lags of the dependent variable's own dynamics to each horizon
# regression so the projection residual becomes approximately serially
# uncorrelated, after which ordinary heteroskedasticity-robust standard errors
# are valid WITHOUT the growing-time-dimension assumption the Driscoll-Kraay
# correction relies on. The baseline already includes four lags of composite
# growth (dQ_lag1..4, the one-period change y_t - y_{t-1} whose leads form the
# outcome); the pACF of composite growth is negligible from the fifth lag
# onward, so four lags provide an adequate augmentation. R5 therefore re-reports
# the baseline interaction under heteroskedasticity-robust (HC1) standard errors,
# serving as a check that the Driscoll-Kraay conclusions do not depend on the HAC
# correction at T = 36. The point estimate is identical to the baseline; only the
# standard error differs.
lp_lagaug <- bind_rows(lapply(0:H_MAX, function(h) {
  dep <- paste0("dep_", sprintf("%02d", h))
  fml <- as.formula(paste0(dep, " ~ shk_x_inel + ", CONTROLS_RHS,
                           " | district_id + quarter"))
  fit <- feols(fml, data = panel, notes = FALSE, warn = FALSE)
  ct  <- coeftable(summary(fit, vcov = "hetero"))
  b  <- ct["shk_x_inel", "Estimate"]; se <- ct["shk_x_inel", "Std. Error"]
  tibble(spec = "R5_lag_augmented", term = "shk_x_inel", horizon = h,
         coef_raw = b, coef = SIGN * b, se = se,
         ci_lo = SIGN * b - 1.96 * se, ci_hi = SIGN * b + 1.96 * se,
         n = fit$nobs, dk_lag = NA_integer_) %>%
    mutate(ci_lo2 = pmin(ci_lo, ci_hi), ci_hi = pmax(ci_lo, ci_hi), ci_lo = ci_lo2) %>%
    select(-ci_lo2)
}))

# -----------------------------------------------------------------------------#
# Collect and save
# -----------------------------------------------------------------------------#
lp_results <- bind_rows(
  lp_baseline, lp_efh, lp_etw, lp_efh_w, lp_etw_w, lp_asym, lp_cont, lp_asym_cont,
  lp_novienna, lp_pretail, lp_de20y, lp_de30y, lp_thr20, lp_thr30, lp_lagaug
) %>%
  mutate(spec = factor(spec, levels = c(
    "baseline", "AS1_house", "AS1_apartment",
    "AS1b_house_weighted", "AS1b_apartment_weighted",
    "AS2_asymmetric", "AS3_continuous", "AS2c_asymmetric_continuous",
    "R1_no_vienna", "R2_pre_tail", "R3_de20y", "R3_de30y",
    "R4_threshold_p20", "R4_threshold_p30", "R5_lag_augmented"
  )))

# Metadata retained for the write-up (standardization constants, thresholds).
lp_meta <- list(
  sign_convention = "coef reports response to an easing surprise (raw x -1)",
  dk_lag_rule = "L = max(h, 1)",
  blr_mean = blr_mean, blr_sd = blr_sd,
  threshold_p20 = unname(thr20), threshold_p30 = unname(thr30),
  # Standard deviation of the net OIS 20Y shock, over the distinct quarters (the
  # shock is common to all districts within a quarter). Used in 04_results.R to
  # scale the per-unit coefficients into the response to a one-SD easing surprise.
  shock_sd = sd(unique(panel[, c("quarter", "shock")])$shock, na.rm = TRUE),
  n_horizons = H_MAX + 1L
)

# -----------------------------------------------------------------------------#
# Residual dependence of the BASELINE specification (feeds Appendix Table 1c)
# -----------------------------------------------------------------------------#
## Computed from the actual baseline fits, replacing the preliminary diagnostic
## used earlier. For selected horizons, the two-way FE baseline is fit and its
## residuals are examined along the two dependence dimensions of Section 5.6:
##   Residual AR(1)  within-district first-order (one-quarter) autocorrelation
##   Mean|rho|/Max|rho|  mean and max absolute pairwise correlation of residuals
##                       across districts within a quarter.
## These are descriptive residual diagnostics, not additional estimates.
resid_diag <- bind_rows(lapply(c(0, 4, 8, 12), function(h) {
  dep  <- paste0("dep_", sprintf("%02d", h))
  fml  <- as.formula(paste0(dep, " ~ shk_x_inel + ", CONTROLS_RHS,
                            " | district_id + quarter"))
  keep <- stats::complete.cases(panel[, c(dep, "shk_x_inel", CONTROL_LAGS,
                                          "district_id", "quarter")])
  dat  <- panel[keep, ]
  fit  <- feols(fml, data = dat, notes = FALSE, warn = FALSE)
  dat$resid <- resid(fit)
  # Within-district AR(1) of the residuals.
  ar1 <- dat %>% arrange(district_id, quarter) %>% group_by(district_id) %>%
    mutate(r_lag = dplyr::lag(resid)) %>% ungroup() %>%
    filter(is.finite(resid), is.finite(r_lag)) %>%
    summarise(a = cor(resid, r_lag)) %>% pull(a)
  # Cross-sectional pairwise correlation of residuals across districts by quarter.
  wide <- dat %>% select(district_id, quarter, resid) %>%
    tidyr::pivot_wider(names_from = district_id, values_from = resid) %>%
    arrange(quarter)
  cmat <- suppressWarnings(cor(as.matrix(wide[, -1, drop = FALSE]),
                               use = "pairwise.complete.obs"))
  up <- cmat[upper.tri(cmat)]
  tibble(horizon = h, residual_ar1 = ar1,
         mean_abs_pairwise = mean(abs(up), na.rm = TRUE),
         max_abs_pairwise  = max(abs(up),  na.rm = TRUE),
         n = fit$nobs)
}))

save(lp_results, lp_meta, resid_diag, file = "./data/estimation_results.RData")

message("Estimation complete.")
message("Specifications: ", paste(levels(lp_results$spec), collapse = ", "))
message("Rows in lp_results: ", nrow(lp_results))
message("Saved: ", normalizePath("./data/estimation_results.RData", winslash = "/", mustWork = FALSE))
