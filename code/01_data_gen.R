# =============================================================================#
# 0. DATA IMPORT FROM 00_DATA_PREP
# -----------------------------------------------------------------------------#

library(dplyr)
library(tidyr)
library(zoo)

load("./data/00_prep_output.RData")

# =============================================================================#
# 1. PANEL MERGE
# -----------------------------------------------------------------------------#
##  - prices (balanced 116 districts x 36 quarters) is the SPINE: all joins are
##    left_joins onto it, so the panel stays balanced
##  - Join keys: district_id (+ quarter where time-varying)
##  - shock_q: national (quarter-level only): joined on quarter only
##  - bl is time-invariant (district-level): joined on district_id only

panel <- prices %>%
  # (a) building-land reserves + elasticity classification (time-invariant)
  left_join(
    bl %>% select(district_id, blr_14, inelastic_dummy),
    by = "district_id"
  ) %>%
  # (b) national quarterly monetary policy shock (quarter-level)
  left_join(
    shock_q,
    by = "quarter"
  ) %>%
  # (c) population (district x quarter)
  left_join(
    c_pop_long %>% select(district_id, quarter, population),
    by = c("district_id", "quarter")
  ) %>%
  # (d) unemployment rate (district x quarter)
  left_join(
    c_unemp_long %>% select(district_id, quarter, unemp_rate),
    by = c("district_id", "quarter")
  ) %>%
  # (e) income per capita (district x quarter) + imputation flag for robustness
  left_join(
    c_inc_q %>% select(district_id, quarter, income_pc, log_income_pc, income_source),
    by = c("district_id", "quarter")
  ) %>%
  arrange(district_id, quarter)

# -----------------------------------------------------------------------------#
# 1.1 POST-MERGE SANITY CHECKS
# -----------------------------------------------------------------------------#
## Fail if the merge silently dropped/duplicated rows or introduced NAs in
## columns that should be fully populated (Income 2023-2025 is extrapolated and
## unemployment 2025 is carried forward, so those are complete by construction;
## any NA here would signal a join-key problem)

stopifnot(
  # spine preserved: still 116 x 36 = 4176 district-quarters, no duplication
  nrow(panel) == 4176,
  nrow(panel) == n_distinct(panel$district_id) * n_distinct(panel$quarter),
  n_distinct(panel$district_id) == 116,
  n_distinct(panel$quarter) == 36
)

## Report any NAs by column (should be zero for merged controls / shock / dummy).
na_by_col <- sapply(panel, function(x) sum(is.na(x)))
print(na_by_col[na_by_col > 0])
if (all(na_by_col == 0)) message("Panel merge OK: no missing values in any column.")

## Quick structural glance.
message("panel dimensions: ", nrow(panel), " x ", ncol(panel))

# =============================================================================#
# 2. OUTCOME, MOMENTUM CONTROLS & INTERACTION
# -----------------------------------------------------------------------------#

# -----------------------------------------------------------------------------#
# 2.1 OUTCOME TRANSFORMATION: cumulative log-price change  y_{i,t+h} - y_{i,t-1}
# -----------------------------------------------------------------------------#
# - ly_composite: log(p_composite)
# - ly_efh: log(efh_avg)
# - ly_etw: lof(etw_avg)

# - ly_base: ly_composite{t-1} i.e. y_{t-1}
# - dep_00: y_t - y_{t-1}
# ...
# - dep_12: y_{t+12} - y_{t-1}

H_MAX <- 12

## Composite outcome leads dep_00..dep_12 = y_{t+h} - y_{t-1} on the log composite.
## The house (EFH) and apartment (ETW) leads are built the same way, each on its
## OWN t-1 base, to serve the property-specific alternative specification (AS1)
## estimated in 03_estimation.R. Naming mirrors the composite: dep_efh_00..12 and
## dep_etw_00..12.
panel <- panel %>%
  group_by(district_id) %>%
  mutate(
    ly_composite = log(p_composite),
    ly_efh       = log(efh_avg),
    ly_etw       = log(etw_avg),
    ly_base      = dplyr::lag(ly_composite, 1),  # composite y_{t-1}
    ly_base_efh  = dplyr::lag(ly_efh, 1),        # house     y_{t-1}
    ly_base_etw  = dplyr::lag(ly_etw, 1),        # apartment y_{t-1}
    dep_00 = dplyr::lead(ly_composite,  0) - ly_base,  # y_t    - y_{t-1}
    dep_01 = dplyr::lead(ly_composite,  1) - ly_base,
    dep_02 = dplyr::lead(ly_composite,  2) - ly_base,
    dep_03 = dplyr::lead(ly_composite,  3) - ly_base,
    dep_04 = dplyr::lead(ly_composite,  4) - ly_base,
    dep_05 = dplyr::lead(ly_composite,  5) - ly_base,
    dep_06 = dplyr::lead(ly_composite,  6) - ly_base,
    dep_07 = dplyr::lead(ly_composite,  7) - ly_base,
    dep_08 = dplyr::lead(ly_composite,  8) - ly_base,
    dep_09 = dplyr::lead(ly_composite,  9) - ly_base,
    dep_10 = dplyr::lead(ly_composite, 10) - ly_base,
    dep_11 = dplyr::lead(ly_composite, 11) - ly_base,
    dep_12 = dplyr::lead(ly_composite, 12) - ly_base,
    # House (EFH) outcome leads, own base
    dep_efh_00 = dplyr::lead(ly_efh,  0) - ly_base_efh,
    dep_efh_01 = dplyr::lead(ly_efh,  1) - ly_base_efh,
    dep_efh_02 = dplyr::lead(ly_efh,  2) - ly_base_efh,
    dep_efh_03 = dplyr::lead(ly_efh,  3) - ly_base_efh,
    dep_efh_04 = dplyr::lead(ly_efh,  4) - ly_base_efh,
    dep_efh_05 = dplyr::lead(ly_efh,  5) - ly_base_efh,
    dep_efh_06 = dplyr::lead(ly_efh,  6) - ly_base_efh,
    dep_efh_07 = dplyr::lead(ly_efh,  7) - ly_base_efh,
    dep_efh_08 = dplyr::lead(ly_efh,  8) - ly_base_efh,
    dep_efh_09 = dplyr::lead(ly_efh,  9) - ly_base_efh,
    dep_efh_10 = dplyr::lead(ly_efh, 10) - ly_base_efh,
    dep_efh_11 = dplyr::lead(ly_efh, 11) - ly_base_efh,
    dep_efh_12 = dplyr::lead(ly_efh, 12) - ly_base_efh,
    # Apartment (ETW) outcome leads, own base
    dep_etw_00 = dplyr::lead(ly_etw,  0) - ly_base_etw,
    dep_etw_01 = dplyr::lead(ly_etw,  1) - ly_base_etw,
    dep_etw_02 = dplyr::lead(ly_etw,  2) - ly_base_etw,
    dep_etw_03 = dplyr::lead(ly_etw,  3) - ly_base_etw,
    dep_etw_04 = dplyr::lead(ly_etw,  4) - ly_base_etw,
    dep_etw_05 = dplyr::lead(ly_etw,  5) - ly_base_etw,
    dep_etw_06 = dplyr::lead(ly_etw,  6) - ly_base_etw,
    dep_etw_07 = dplyr::lead(ly_etw,  7) - ly_base_etw,
    dep_etw_08 = dplyr::lead(ly_etw,  8) - ly_base_etw,
    dep_etw_09 = dplyr::lead(ly_etw,  9) - ly_base_etw,
    dep_etw_10 = dplyr::lead(ly_etw, 10) - ly_base_etw,
    dep_etw_11 = dplyr::lead(ly_etw, 11) - ly_base_etw,
    dep_etw_12 = dplyr::lead(ly_etw, 12) - ly_base_etw
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------#
# 2.2 PRICE-MOMENTUM CONTROLS: lagged quarterly log-price changes
# -----------------------------------------------------------------------------#
## Aastveit & Anundsen include lagged changes in log house prices to capture
## momentum (a key feature of housing markets)
# - dQ_composite: one-quarter log change of p_composite
# - dQ_lag1 - dQ_lag4: include 4 lags if p_composite log-changes (as in Aastveit & Anundsen)

panel <- panel %>%
  group_by(district_id) %>%
  mutate(
    dQ_composite = ly_composite - dplyr::lag(ly_composite, 1),
    dQ_lag1 = dplyr::lag(dQ_composite, 1),
    dQ_lag2 = dplyr::lag(dQ_composite, 2),
    dQ_lag3 = dplyr::lag(dQ_composite, 3),
    dQ_lag4 = dplyr::lag(dQ_composite, 4)
  ) %>%
  ungroup()

# -----------------------------------------------------------------------------#
# 2.3 INTERACTION: Shock x Inelastic  (primary regressor)
# -----------------------------------------------------------------------------#
## With time fixed effects the shock's main effect is absorbed; beta_h on the
## interaction identifies the DIFFERENTIAL response of inelastic districts
## Baseline: net OIS 20Y shock. Signed split supports the asymmetry sub-question
## (Aastveit): under the sign convention (negative surprise = EASING),
##   shock_ease  = shock_neg_ois20y  (sum of negative / easing surprises)
##   shock_tight = shock_pos_ois20y  (sum of positive / tightening surprises)

# - shock: shock_net_ois20y (net quarterly OIS 20Y shock)
# - shock_ease: shock_neg_ois20y (negative quarterly OIS 20Y shock)
# - shock_tight: shock_pos_20y (positive quarterly OIS 20Y shock)

# - shk_x_inel: shock x inelastic_dummy
# - shk_ease_x_inel: shock_ease x inelastic_dummy
# - shk_tight_x_inel: shock_tight x inelastic_dummy

panel <- panel %>%
  mutate(
    shock       = shock_net_ois20y,
    shock_ease  = shock_neg_ois20y,
    shock_tight = shock_pos_ois20y,
    shk_x_inel       = shock       * inelastic_dummy,
    shk_ease_x_inel  = shock_ease  * inelastic_dummy,
    shk_tight_x_inel = shock_tight * inelastic_dummy
  )


# -----------------------------------------------------------------------------#
# 2.4 CONTROL LAGS: lagged changes of income p.c., population and unemployment
# -----------------------------------------------------------------------------#
## Aastveit & Anundsen: the control vector enters as LAGGED CHANGES, with
## 4 quarterly lags each
## Form per control:
## - income p.c.  : quarterly change of log income p.c.  (log-difference)
## - population   : population growth = quarterly change of log population
## - unemployment : change of the unemployment RATE in percentage points
##                  (first difference of the rate, NOT a log-difference, since
##                  the series is already a percentage)
##
## CAVEAT (imputed tails): income (2023-2025 via TLI) and unemployment (2025
## carried forward) are annual figures repeated within the year, so their
## quarterly changes are ~0 within those spans. This is expected;
## these controls simply carry little within-district variation in the imputed
## periods (cf. the diagnostics discussion). The lag-0 change itself is kept for
## reference

panel <- panel %>%
  group_by(district_id) %>%
  mutate(
    # log population (level) -> quarterly growth
    lpop        = log(population),
    d_lincome   = log_income_pc - dplyr::lag(log_income_pc, 1),  # dlog income p.c.
    d_lpop      = lpop          - dplyr::lag(lpop, 1),           # population growth
    d_unemp     = unemp_rate    - dplyr::lag(unemp_rate, 1),     # pp change in rate

    # income p.c.: 4 lags of the log change
    d_lincome_lag1 = dplyr::lag(d_lincome, 1),
    d_lincome_lag2 = dplyr::lag(d_lincome, 2),
    d_lincome_lag3 = dplyr::lag(d_lincome, 3),
    d_lincome_lag4 = dplyr::lag(d_lincome, 4),

    # population growth: 4 lags
    d_lpop_lag1 = dplyr::lag(d_lpop, 1),
    d_lpop_lag2 = dplyr::lag(d_lpop, 2),
    d_lpop_lag3 = dplyr::lag(d_lpop, 3),
    d_lpop_lag4 = dplyr::lag(d_lpop, 4),

    # unemployment rate change: 4 lags
    d_unemp_lag1 = dplyr::lag(d_unemp, 1),
    d_unemp_lag2 = dplyr::lag(d_unemp, 2),
    d_unemp_lag3 = dplyr::lag(d_unemp, 3),
    d_unemp_lag4 = dplyr::lag(d_unemp, 4)
  ) %>%
  ungroup()



# -----------------------------------------------------------------------------#
# =============================================================================#
# SAVE FINALISED PANEL
# =============================================================================#
##   panel - balanced district-quarter LP panel (116 x 36), containing:
##           composite/efh/etw price levels + logs, cumulative log-change
##           outcomes dep_00..dep_12 (y_{t+h} - y_{t-1}) on the composite, plus
##           the property-specific outcome leads dep_efh_00..12 and dep_etw_00..12
##           (house and apartment, each on its own t-1 base) for the AS1 spec,
##           price-momentum lags (dQ_lag1..4), lagged control changes (income
##           p.c., population, unemployment; 4 lags each), the aggregated shock
##           variants and the Shock x Inelastic interactions (net / easing /
##           tightening).

save(
  panel,
  file = "./data/01_panel.RData"
)

