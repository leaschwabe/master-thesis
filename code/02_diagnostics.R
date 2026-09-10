# =============================================================================#
# 02_DIAGNOSTICS
# -----------------------------------------------------------------------------#
# Comprehensive, reproducible diagnostics for the district-quarter LP panel.
# This script deliberately does not modify 00_data_prep.R, 01_data_gen.R, or
# their saved objects. All diagnostic-only flags and transformations are built
# in memory and written to outputs/diagnostics/.
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
# var_label, ggsave_thesis). Used for the chapter-ready exports to thesis/charts.
source("code/plot_theme.R")

load("./data/01_panel.RData")
load("./data/00_prep_output.RData")

OUT_DIR <- "./outputs/diagnostics"
FIG_DIR <- file.path(OUT_DIR, "figures")
TAB_DIR <- file.path(OUT_DIR, "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

# Chapter-ready figures (theme_thesis) are exported here for \includegraphics.
THESIS_FIG_DIR <- "./thesis/charts"
dir.create(THESIS_FIG_DIR, recursive = TRUE, showWarnings = FALSE)

options(scipen = 6, dplyr.summarise.inform = FALSE)
theme_set(theme_minimal(base_size = 11))

# -----------------------------------------------------------------------------#
# Helpers
# -----------------------------------------------------------------------------#
q_label <- function(x) format(x, "%Y Q%q")
fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, digits = digits, format = "f", big.mark = ","))
}

md_table <- function(x, digits = 3) {
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x) == 0L) return("_No rows._")
  for (j in seq_along(x)) {
    if (is.numeric(x[[j]])) x[[j]] <- fmt_num(x[[j]], digits)
    x[[j]] <- gsub("\\|", "\\\\|", as.character(x[[j]]))
    x[[j]][is.na(x[[j]])] <- ""
  }
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  rule <- paste0("| ", paste(rep("---", ncol(x)), collapse = " | "), " |")
  rows <- apply(x, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  c(header, rule, rows)
}

save_table <- function(name, x) {
  write.csv(x, file.path(TAB_DIR, paste0(name, ".csv")), row.names = FALSE,
            na = "", fileEncoding = "UTF-8")
  invisible(x)
}

save_plot <- function(name, p, width = 9, height = 5.5) {
  ggsave(file.path(FIG_DIR, paste0(name, ".png")), p,
         width = width, height = height, dpi = 180, bg = "white")
}

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

cor_long <- function(cmat) {
  as.data.frame(as.table(cmat), stringsAsFactors = FALSE) %>%
    rename(variable_1 = Var1, variable_2 = Var2, correlation = Freq)
}

vif_values <- function(x) {
  x <- as.matrix(x)
  out <- setNames(rep(NA_real_, ncol(x)), colnames(x))
  for (j in seq_len(ncol(x))) {
    y <- x[, j]
    others <- x[, -j, drop = FALSE]
    fit <- lm.fit(cbind(1, others), y)
    rss <- sum(fit$residuals^2)
    tss <- sum((y - mean(y))^2)
    r2 <- if (tss > 0) 1 - rss / tss else 1
    out[j] <- if (is.finite(r2) && r2 < 1) 1 / (1 - r2) else Inf
  }
  out
}

two_way_demean <- function(x, id, time) {
  x - ave(x, id, FUN = function(z) mean(z, na.rm = TRUE)) -
    ave(x, time, FUN = function(z) mean(z, na.rm = TRUE)) + mean(x, na.rm = TRUE)
}

variation_decomp <- function(data, vars) {
  bind_rows(lapply(vars, function(v) {
    z <- data[[v]]
    ok <- is.finite(z)
    z <- z[ok]
    id <- data$district_id[ok]
    grand <- mean(z)
    zbar <- ave(z, id, FUN = mean)
    ss_total <- sum((z - grand)^2)
    ss_within <- sum((z - zbar)^2)
    ss_between <- sum((zbar - grand)^2)
    tibble(variable = v, total_sd = sd(z), within_sd = sqrt(ss_within / (length(z) - 1)),
           between_sd = sqrt(ss_between / (length(z) - 1)),
           within_share = ss_within / ss_total, between_share = ss_between / ss_total)
  }))
}

pooled_acf <- function(data, variable, max_lag = 4L) {
  bind_rows(lapply(seq_len(max_lag), function(k) {
    tmp <- data %>% arrange(district_id, quarter) %>% group_by(district_id) %>%
      transmute(x = .data[[variable]], x_lag = dplyr::lag(.data[[variable]], k)) %>%
      ungroup() %>% filter(is.finite(x), is.finite(x_lag))
    tibble(variable = variable, lag = k, correlation = cor(tmp$x, tmp$x_lag), n_pairs = nrow(tmp))
  }))
}

# -----------------------------------------------------------------------------#
# Diagnostic-only labels and flags
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
    quarter_label = q_label(quarter),
    year = as.integer(format(quarter, "%Y")),
    bundesland_code = floor(district_id / 100),
    bundesland = case_when(
      bundesland_code == 1 ~ "Burgenland", bundesland_code == 2 ~ "Kaernten",
      bundesland_code == 3 ~ "Niederoesterreich", bundesland_code == 4 ~ "Oberoesterreich",
      bundesland_code == 5 ~ "Salzburg", bundesland_code == 6 ~ "Steiermark",
      bundesland_code == 7 ~ "Tirol", bundesland_code == 8 ~ "Vorarlberg",
      bundesland_code == 9 ~ "Wien", TRUE ~ "Unknown"
    ),
    vienna = if_else(bundesland_code == 9, "Vienna", "Outside Vienna"),
    supply_group = if_else(inelastic_dummy == 1, "Inelastic", "Elastic"),
    income_constructed = income_source != "observed",
    unemployment_carried = year == 2025,
    population_carried = quarter > as.yearqtr("2025 Q1", format = "%Y Q%q")
  )

n_d <- n_distinct(panel_d$district_id)
n_t <- n_distinct(panel_d$quarter)
control_lags <- c(paste0("dQ_lag", 1:4), paste0("d_lincome_lag", 1:4),
                  paste0("d_lpop_lag", 1:4), paste0("d_unemp_lag", 1:4))
model_x <- c("shk_x_inel", control_lags)

# -----------------------------------------------------------------------------#
# 1. Panel integrity, keys, coverage, missingness, estimation samples
# -----------------------------------------------------------------------------#
key_counts <- panel_d %>% count(district_id, quarter, name = "n")
district_counts <- panel_d %>% count(district_id, name = "quarters")
quarter_counts <- panel_d %>% count(quarter, name = "districts")
expected_grid <- expand.grid(
  district_id = sort(unique(panel_d$district_id)),
  quarter = sort(unique(panel_d$quarter)), KEEP.OUT.ATTRS = FALSE
)
actual_keys <- panel_d %>% select(district_id, quarter)
missing_grid_keys <- anti_join(expected_grid, actual_keys, by = c("district_id", "quarter"))
unexpected_keys <- anti_join(actual_keys, expected_grid, by = c("district_id", "quarter"))

integrity <- tibble(
  diagnostic = c("Rows", "Districts", "Quarters", "Unique district-quarter keys",
                 "Duplicate key rows", "Missing expected keys", "Unexpected keys",
                 "Districts with 36 quarters", "Quarters with 116 districts",
                 "Nonpositive composite prices", "Nonpositive EFH prices",
                 "Nonpositive ETW prices", "Non-finite numeric cells"),
  observed = c(nrow(panel_d), n_d, n_t, nrow(distinct(actual_keys)),
               sum(pmax(key_counts$n - 1L, 0L)), nrow(missing_grid_keys), nrow(unexpected_keys),
               sum(district_counts$quarters == 36), sum(quarter_counts$districts == 116),
               sum(panel_d$p_composite <= 0, na.rm = TRUE),
               sum(panel_d$efh_avg <= 0, na.rm = TRUE),
               sum(panel_d$etw_avg <= 0, na.rm = TRUE),
               sum(vapply(panel_d[vapply(panel_d, is.numeric, logical(1))],
                          function(z) sum(!is.finite(z) & !is.na(z)), numeric(1)))),
  expected = c(4176, 116, 36, 4176, 0, 0, 0, 116, 36, 0, 0, 0, 0)
) %>% mutate(status = if_else(observed == expected, "PASS", "CHECK"))
save_table("01_panel_integrity", integrity)

missingness <- tibble(
  variable = names(panel_d),
  missing_n = vapply(panel_d, function(z) sum(is.na(z)), numeric(1)),
  missing_pct = 100 * missing_n / nrow(panel_d)
) %>% arrange(desc(missing_n), variable)
save_table("02_missingness_all_variables", missingness)

# Verify that every upstream object satisfies its intended join-key contract.
duplicate_rows <- function(data, keys) {
  data %>% count(across(all_of(keys)), name = "n") %>% summarise(x = sum(pmax(n - 1L, 0L))) %>% pull(x)
}
panel_ids <- panel_d %>% distinct(district_id)
panel_quarters <- panel_d %>% distinct(quarter)
panel_keys <- panel_d %>% distinct(district_id, quarter)
input_contracts <- tibble(
  object = c("prices", "composite_weights", "bl", "shock_q", "c_pop_long", "c_unemp_long", "c_inc_q"),
  key = c("district_id + quarter", "district_id", "district_id", "quarter",
          "district_id + quarter", "district_id + quarter", "district_id + quarter"),
  rows = c(nrow(prices), nrow(composite_weights), nrow(bl), nrow(shock_q),
           nrow(c_pop_long), nrow(c_unemp_long), nrow(c_inc_q)),
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
save_table("01b_input_join_contracts", input_contracts)

horizon_samples <- bind_rows(lapply(0:12, function(h) {
  dep <- paste0("dep_", sprintf("%02d", h))
  cc <- complete.cases(panel_d[, c(dep, model_x)])
  sample <- panel_d[cc, ]
  tibble(
    horizon = h, outcome_nonmissing = sum(!is.na(panel_d[[dep]])),
    expected_outcome_nonmissing = n_d * (35 - h), complete_case_n = sum(cc),
    expected_max_with_four_lags = n_d * (31 - h), districts = n_distinct(sample$district_id),
    quarters = n_distinct(sample$quarter), first_quarter = if (nrow(sample)) q_label(min(sample$quarter)) else NA_character_,
    last_quarter = if (nrow(sample)) q_label(max(sample$quarter)) else NA_character_,
    constructed_income_pct = if (nrow(sample)) 100 * mean(sample$income_constructed) else NA_real_,
    carried_unemployment_pct = if (nrow(sample)) 100 * mean(sample$unemployment_carried) else NA_real_
  )
}))
save_table("03_horizon_estimation_samples", horizon_samples)

p_sample <- ggplot(horizon_samples, aes(horizon)) +
  geom_line(aes(y = complete_case_n, colour = "Complete-case N"), linewidth = .9) +
  geom_point(aes(y = complete_case_n, colour = "Complete-case N"), size = 2) +
  geom_line(aes(y = outcome_nonmissing, colour = "Outcome available"), linewidth = .8, linetype = 2) +
  scale_x_continuous(breaks = 0:12) +
  labs(title = "Available observations decline mechanically with LP horizon",
       x = "Horizon (quarters)", y = "District-quarter observations", colour = NULL)
save_plot("01_horizon_sample_size", p_sample)

# -----------------------------------------------------------------------------#
# 2. Prices, composite construction, groups, and momentum
# -----------------------------------------------------------------------------#
price_summary <- numeric_summary(panel_d, c("gst", "efh_avg", "etw_avg", "p_composite",
                                            "dQ_composite", "ly_composite"))
save_table("04_price_summary", price_summary)

static_district <- panel_d %>% distinct(district_id, district_name, bundesland, vienna,
                                        blr_14, inelastic_dummy, supply_group, w_house, w_apt)
group_summary <- panel_d %>% group_by(supply_group) %>% summarise(
  districts = n_distinct(district_id), observations = n(),
  mean_composite = mean(p_composite), median_composite = median(p_composite),
  mean_quarterly_log_growth = mean(dQ_composite, na.rm = TRUE),
  sd_quarterly_log_growth = sd(dQ_composite, na.rm = TRUE),
  mean_house_weight = mean(w_house), mean_reserve = mean(blr_14), .groups = "drop"
)
save_table("05_supply_group_summary", group_summary)

quarterly_prices <- panel_d %>% group_by(quarter) %>% summarise(
  Composite = median(p_composite), EFH = median(efh_avg), ETW = median(etw_avg), .groups = "drop"
) %>% pivot_longer(-quarter, names_to = "series", values_to = "value") %>%
  group_by(series) %>% arrange(quarter) %>% mutate(index = 100 * value / first(value)) %>% ungroup()
p_price <- ggplot(quarterly_prices, aes(quarter, index, colour = series)) + geom_line(linewidth = .9) +
  labs(title = "Median district price indexes", subtitle = "2017 Q1 = 100; unweighted district medians",
       x = NULL, y = "Index", colour = NULL)
save_plot("02_price_indexes_property_type", p_price)

## Thesis export: readable series labels, thesis theme/palette, no in-chart title
## (added in the LaTeX caption). Saved to thesis/charts at the 14 cm figure width.
quarterly_prices_thesis <- quarterly_prices %>%
  mutate(series = recode(series,
                         Composite = "Composite",
                         EFH = "House",
                         ETW = "Apartment"),
         series = factor(series, levels = c("Composite", "House", "Apartment")))
p_price_thesis <- ggplot(quarterly_prices_thesis, aes(quarter, index, colour = series)) +
  geom_line(linewidth = .9) +
  scale_x_yearqtr(format = "%Y", n = 5) +
  scale_colour_thesis() +
  labs(x = NULL, y = "Index (Q1 2017 = 100)", colour = NULL) +
  theme_thesis()
ggsave_thesis(file.path(THESIS_FIG_DIR, "02_price_indexes_property_type.pdf"), p_price_thesis)

group_prices <- panel_d %>% group_by(supply_group, quarter) %>%
  summarise(value = median(p_composite), .groups = "drop") %>% group_by(supply_group) %>%
  arrange(quarter) %>% mutate(index = 100 * value / first(value)) %>% ungroup()
p_group_price <- ggplot(group_prices, aes(quarter, index, colour = supply_group)) + geom_line(linewidth = 1) +
  labs(title = "Median composite price index by supply group", subtitle = "2017 Q1 = 100",
       x = NULL, y = "Index", colour = NULL)
save_plot("03_composite_index_by_supply_group", p_group_price)

p_growth <- ggplot(panel_d %>% filter(is.finite(dQ_composite)),
                   aes(supply_group, 100 * dQ_composite, fill = supply_group)) +
  geom_violin(alpha = .35, trim = TRUE) + geom_boxplot(width = .18, outlier.alpha = .25) +
  coord_cartesian(ylim = quantile(100 * panel_d$dQ_composite, c(.01, .99), na.rm = TRUE)) +
  guides(fill = "none") + labs(title = "Quarterly composite-price growth distribution",
                               subtitle = "Plot restricted to the 1st-99th percentile for readability",
                               x = NULL, y = "Log change (percentage points)")
save_plot("04_price_growth_by_supply_group", p_growth)

acf_table <- bind_rows(lapply(c("dQ_composite", "d_lincome", "d_lpop", "d_unemp"),
                              function(v) pooled_acf(panel_d, v, 4)))
save_table("06_pooled_autocorrelations", acf_table)
p_acf <- ggplot(acf_table, aes(factor(lag), correlation, fill = variable)) +
  geom_col(position = "dodge") + geom_hline(yintercept = 0, colour = "grey40") +
  labs(title = "Pooled within-district autocorrelation", x = "Lag (quarters)", y = "Correlation", fill = NULL)
save_plot("05_autocorrelation", p_acf)

# -----------------------------------------------------------------------------#
# 3. Composite weights and supply classification
# -----------------------------------------------------------------------------#
weight_identity_error <- max(abs(static_district$w_house + static_district$w_apt - 1), na.rm = TRUE)
composite_identity_error <- max(abs(panel_d$p_composite -
                                      (panel_d$w_house * panel_d$efh_avg + panel_d$w_apt * panel_d$etw_avg)),
                                na.rm = TRUE)
weight_summary <- numeric_summary(static_district, c("w_house", "w_apt", "blr_14")) %>%
  bind_rows(tibble(variable = c("max_weight_sum_error", "max_composite_identity_error"),
                   n = c(nrow(static_district), nrow(panel_d)), mean = c(weight_identity_error, composite_identity_error)))
save_table("07_weights_and_reserves_summary", weight_summary)

p_weights <- ggplot(static_district, aes(w_house, fill = vienna)) +
  geom_histogram(bins = 20, alpha = .65, position = "identity") +
  facet_wrap(~vienna, ncol = 1, scales = "free_y") +
  labs(title = "Fixed 2011 house-stock weight", x = "House weight", y = "District count", fill = NULL)
save_plot("06_house_weight_distribution", p_weights)

q25_nonvienna <- quantile(static_district$blr_14[static_district$vienna == "Outside Vienna"], .25, na.rm = TRUE)
classification_check <- static_district %>% mutate(recomputed = as.integer(blr_14 <= q25_nonvienna))
classification_summary <- classification_check %>% group_by(vienna, supply_group) %>%
  summarise(districts = n(), reserve_min = min(blr_14), reserve_median = median(blr_14),
            reserve_max = max(blr_14), .groups = "drop")
classification_metrics <- tibble(
  metric = c("Non-Vienna 25th-percentile threshold", "Districts at threshold",
             "Classification disagreements", "Inelastic districts total",
             "Inelastic districts outside Vienna", "Inelastic Vienna districts"),
  value = c(q25_nonvienna, sum(abs(static_district$blr_14 - q25_nonvienna) < 1e-10),
            sum(classification_check$recomputed != classification_check$inelastic_dummy),
            sum(static_district$inelastic_dummy == 1),
            sum(static_district$inelastic_dummy == 1 & static_district$vienna == "Outside Vienna"),
            sum(static_district$inelastic_dummy == 1 & static_district$vienna == "Vienna"))
)
save_table("08_supply_classification_metrics", classification_metrics)
save_table("09_supply_classification_groups", classification_summary)

p_reserve <- ggplot(static_district, aes(blr_14, fill = supply_group)) +
  geom_histogram(bins = 25, alpha = .65, position = "identity") +
  geom_vline(xintercept = q25_nonvienna, linetype = 2, linewidth = .8) +
  labs(title = "Building-land reserve distribution and classification threshold",
       subtitle = "Dashed line: 25th percentile outside Vienna, then applied to all districts",
       x = "2014 building-land reserve", y = "District count", fill = NULL)
save_plot("07_reserve_distribution_threshold", p_reserve)

state_reserves <- static_district %>% group_by(bundesland) %>% summarise(
  districts = n(), inelastic = sum(inelastic_dummy == 1), inelastic_share = mean(inelastic_dummy == 1),
  reserve_median = median(blr_14), house_weight_median = median(w_house), .groups = "drop"
)
save_table("10_state_supply_summary", state_reserves)
p_state <- ggplot(static_district, aes(reorder(bundesland, blr_14, median), blr_14, fill = vienna)) +
  geom_boxplot(outlier.alpha = .5) + coord_flip() + guides(fill = "none") +
  labs(title = "Building-land reserves by federal state", x = NULL, y = "2014 reserve measure")
save_plot("08_reserves_by_state", p_state)

# -----------------------------------------------------------------------------#
# 4. Monetary-policy shock diagnostics
# -----------------------------------------------------------------------------#
shock_panel <- panel_d %>% distinct(quarter, shock_net_ois20y, shock_pos_ois20y,
                                    shock_neg_ois20y, shock_abs_ois20y,
                                    n_nonmiss_ois20y, n_events) %>%
  left_join(shock_cancel_ratio %>% select(quarter, cancel_ratio_ois20y), by = "quarter") %>%
  arrange(quarter) %>% mutate(quarter_label = q_label(quarter))

shock_summary <- numeric_summary(shock_panel, c("shock_net_ois20y", "shock_pos_ois20y",
                                                "shock_neg_ois20y", "shock_abs_ois20y",
                                                "cancel_ratio_ois20y", "n_events", "n_nonmiss_ois20y"))
save_table("11_shock_summary", shock_summary)
shock_checks <- tibble(
  diagnostic = c("Quarter coverage", "Missing net shock quarters", "Net = positive + negative (max error)",
                 "Absolute = positive - negative (max error)", "Quarters with easing component",
                 "Quarters with tightening component", "Quarters with both signs", "Quarters with incomplete OIS events"),
  value = c(nrow(shock_panel), sum(is.na(shock_panel$shock_net_ois20y)),
            max(abs(shock_panel$shock_net_ois20y - shock_panel$shock_pos_ois20y - shock_panel$shock_neg_ois20y), na.rm = TRUE),
            max(abs(shock_panel$shock_abs_ois20y - shock_panel$shock_pos_ois20y + shock_panel$shock_neg_ois20y), na.rm = TRUE),
            sum(shock_panel$shock_neg_ois20y < 0, na.rm = TRUE),
            sum(shock_panel$shock_pos_ois20y > 0, na.rm = TRUE),
            sum(shock_panel$shock_neg_ois20y < 0 & shock_panel$shock_pos_ois20y > 0, na.rm = TRUE),
            sum(shock_panel$n_nonmiss_ois20y < shock_panel$n_events, na.rm = TRUE))
)
save_table("12_shock_consistency_checks", shock_checks)

shock_extremes <- shock_panel %>% mutate(abs_net = abs(shock_net_ois20y),
                                          standard_deviations = abs_net / sd(shock_net_ois20y, na.rm = TRUE)) %>%
  arrange(desc(abs_net)) %>% select(quarter_label, shock_net_ois20y, shock_pos_ois20y,
                                    shock_neg_ois20y, shock_abs_ois20y, cancel_ratio_ois20y,
                                    n_events, n_nonmiss_ois20y, standard_deviations) %>% slice_head(n = 10)
save_table("13_largest_shock_quarters", shock_extremes)

p_shock <- ggplot(shock_panel, aes(quarter, shock_net_ois20y)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(aes(fill = shock_net_ois20y < 0), width = .18) +
  scale_fill_manual(values = c("TRUE" = "#2b8cbe", "FALSE" = "#d7301f"),
                    labels = c("FALSE" = "Tightening net", "TRUE" = "Easing net")) +
  labs(title = "Quarterly net OIS 20-year monetary-policy shock",
       subtitle = "Negative values denote easing surprises", x = NULL, y = "Quarterly shock sum", fill = NULL)
save_plot("09_shock_time_series", p_shock)

shock_components <- shock_panel %>% select(quarter, Easing = shock_neg_ois20y,
                                            Tightening = shock_pos_ois20y) %>%
  pivot_longer(-quarter, names_to = "component", values_to = "value")
p_components <- ggplot(shock_components, aes(quarter, value, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey50") + geom_col(position = "identity", alpha = .8, width = .18) +
  labs(title = "Signed within-quarter shock components", x = NULL, y = "Sum", fill = NULL)
save_plot("10_shock_signed_components", p_components)

## Thesis export: signed easing/tightening components as bars, with the quarterly
## NET shock overlaid as a scatter (points connected by a thin line so the net
## path is legible on top of the stacked signed bars). Net data comes from
## shock_panel (one row per quarter). Fill uses two palette colours; the net
## overlay is drawn in a dark neutral so it reads against both bar colours.
shock_net_thesis <- shock_panel %>% select(quarter, net = shock_net_ois20y)
p_components_thesis <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(data = shock_components,
           aes(quarter, value, fill = component),
           position = "identity", alpha = .85, width = .18) +
  geom_line(data = shock_net_thesis, aes(quarter, net),
            colour = "grey25", linewidth = .4) +
  geom_point(data = shock_net_thesis, aes(quarter, net, shape = "Net shock"),
             colour = "grey15", size = 1.6) +
  scale_x_yearqtr(format = "%Y", n = 5) +
  scale_fill_thesis() +
  scale_shape_manual(values = c("Net shock" = 16)) +
  labs(x = NULL, y = "Quarterly shock sum (OIS 20Y)", fill = NULL, shape = NULL) +
  theme_thesis()
ggsave_thesis(file.path(THESIS_FIG_DIR, "10_shock_signed_components.pdf"), p_components_thesis)

p_cancel <- ggplot(shock_panel, aes(quarter, cancel_ratio_ois20y)) +
  geom_line(linewidth = .8) + geom_point(aes(size = n_events), alpha = .75) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Within-quarter shock cancellation ratio",
       subtitle = "1 = no directional offsetting; 0 = complete offsetting",
       x = NULL, y = "|Net shock| / absolute shock sum", size = "Events")
save_plot("11_shock_cancellation", p_cancel)

# -----------------------------------------------------------------------------#
# 5. Control construction, variation, and imputed-tail diagnostics
# -----------------------------------------------------------------------------#
source_coverage <- panel_d %>% group_by(income_source) %>% summarise(
  observations = n(), districts = n_distinct(district_id), quarters = n_distinct(quarter),
  first_quarter = q_label(min(quarter)), last_quarter = q_label(max(quarter)), .groups = "drop"
) %>% bind_rows(tibble(income_source = "unemployment_carried_2025",
                       observations = sum(panel_d$unemployment_carried), districts = n_d,
                       quarters = n_distinct(panel_d$quarter[panel_d$unemployment_carried]),
                       first_quarter = q_label(min(panel_d$quarter[panel_d$unemployment_carried])),
                       last_quarter = q_label(max(panel_d$quarter[panel_d$unemployment_carried]))),
                tibble(income_source = "population_carried_2025Q2_Q4",
                       observations = sum(panel_d$population_carried), districts = n_d,
                       quarters = n_distinct(panel_d$quarter[panel_d$population_carried]),
                       first_quarter = q_label(min(panel_d$quarter[panel_d$population_carried])),
                       last_quarter = q_label(max(panel_d$quarter[panel_d$population_carried]))))
save_table("14_constructed_control_coverage", source_coverage)

control_period <- panel_d %>% mutate(period = case_when(
  year <= 2022 ~ "2017-2022 observed-income period",
  year %in% 2023:2024 ~ "2023-2024 extrapolated-income period",
  year == 2025 ~ "2025 all-tail-construction period"
))
zero_change <- control_period %>% group_by(period) %>% summarise(
  observations = n(),
  income_zero_pct = 100 * mean(abs(d_lincome) < 1e-12, na.rm = TRUE),
  population_zero_pct = 100 * mean(abs(d_lpop) < 1e-12, na.rm = TRUE),
  unemployment_zero_pct = 100 * mean(abs(d_unemp) < 1e-12, na.rm = TRUE),
  income_change_sd = sd(d_lincome, na.rm = TRUE), population_change_sd = sd(d_lpop, na.rm = TRUE),
  unemployment_change_sd = sd(d_unemp, na.rm = TRUE), .groups = "drop"
)
save_table("15_control_flatness_by_period", zero_change)

variation <- variation_decomp(panel_d, c("ly_composite", "log_income_pc", "lpop", "unemp_rate",
                                         "dQ_composite", "d_lincome", "d_lpop", "d_unemp"))
save_table("16_within_between_variation", variation)
variation_plot <- variation %>% select(variable, within_share, between_share) %>%
  pivot_longer(-variable, names_to = "component", values_to = "share")
p_variation <- ggplot(variation_plot, aes(reorder(variable, share), share, fill = component)) +
  geom_col(position = "fill") + coord_flip() + scale_y_continuous(labels = function(x) paste0(round(100*x), "%")) +
  labs(title = "Within- versus between-district variation", x = NULL, y = "Share of total sum of squares", fill = NULL)
save_plot("12_within_between_variation", p_variation)

control_time <- panel_d %>% group_by(quarter) %>% summarise(
  income = median(d_lincome, na.rm = TRUE), population = median(d_lpop, na.rm = TRUE),
  unemployment = median(d_unemp, na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(-quarter, names_to = "control", values_to = "median_change") %>%
  filter(is.finite(median_change))
p_controls <- ggplot(control_time, aes(quarter, median_change)) + geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = .8) + facet_wrap(~control, scales = "free_y", ncol = 1) +
  geom_vline(xintercept = as.numeric(as.yearqtr("2023 Q1", format = "%Y Q%q")), linetype = 2) +
  labs(title = "Median district control changes over time",
       subtitle = "Dashed line: income extrapolation begins in 2023 Q1", x = NULL, y = "Median change")
save_plot("13_control_changes_over_time", p_controls, height = 7)

# -----------------------------------------------------------------------------#
# 6. Correlations, VIFs, design rank, and interaction variation
# -----------------------------------------------------------------------------#
key_corr_vars <- c("dQ_composite", "d_lincome", "d_lpop", "d_unemp", "shock",
                   "shock_ease", "shock_tight", "blr_14", "w_house", "shk_x_inel")
key_corr <- cor(panel_d[, key_corr_vars], use = "pairwise.complete.obs")
key_corr_table <- data.frame(variable = rownames(key_corr), round(key_corr, 4), check.names = FALSE)
save_table("17_key_variable_correlations", key_corr_table)

p_key_corr <- ggplot(cor_long(key_corr), aes(variable_1, variable_2, fill = correlation)) +
  geom_tile() + geom_text(aes(label = sprintf("%.2f", correlation)), size = 2.6) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", limits = c(-1, 1)) +
  coord_equal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Pairwise correlations of key variables", x = NULL, y = NULL, fill = "Correlation")
save_plot("14_key_correlation_heatmap", p_key_corr, width = 10, height = 8)

## Thesis export: readable axis labels (no raw variable names), thesis theme, and
## a diverging fill scale kept for the correlation surface. Labels extend
## var_label() with the change/interaction variables specific to this matrix.
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
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", limits = c(-1, 1)) +
  coord_equal() +
  labs(x = NULL, y = NULL, fill = "Correlation") +
  theme_thesis() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        legend.position = "right")
## Square heatmap: use a taller output than the default single-plot height.
ggsave_thesis(file.path(THESIS_FIG_DIR, "14_key_correlation_heatmap.pdf"),
              p_key_corr_thesis, height = 13)

lag_corr <- cor(panel_d[, control_lags], use = "pairwise.complete.obs")
lag_corr_table <- data.frame(variable = rownames(lag_corr), round(lag_corr, 4), check.names = FALSE)
save_table("18_control_lag_correlations", lag_corr_table)
p_lag_corr <- ggplot(cor_long(lag_corr), aes(variable_1, variable_2, fill = correlation)) +
  geom_tile() + scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", limits = c(-1, 1)) +
  coord_equal() + theme(axis.text.x = element_text(angle = 55, hjust = 1, size = 7),
                        axis.text.y = element_text(size = 7)) +
  labs(title = "Correlation matrix: four lags of price and district controls",
       x = NULL, y = NULL, fill = "Correlation")
save_plot("15_control_lag_correlation_heatmap", p_lag_corr, width = 10, height = 8.5)

vif_cc <- complete.cases(panel_d[, model_x])
x_raw <- as.matrix(panel_d[vif_cc, model_x])
x_twfe <- apply(x_raw, 2, two_way_demean,
                id = panel_d$district_id[vif_cc], time = panel_d$quarter[vif_cc])
if (is.null(dim(x_twfe))) x_twfe <- matrix(x_twfe, ncol = 1)
colnames(x_twfe) <- colnames(x_raw)
raw_sd <- apply(x_raw, 2, sd)
twfe_sd <- apply(x_twfe, 2, sd)
keep_raw <- is.finite(raw_sd) & raw_sd > 0
keep_twfe <- is.finite(twfe_sd) & twfe_sd > 1e-14
vif_raw <- vif_values(x_raw[, keep_raw, drop = FALSE])
vif_twfe <- vif_values(x_twfe[, keep_twfe, drop = FALSE])
vif_table <- full_join(tibble(variable = names(vif_raw), raw_vif = as.numeric(vif_raw)),
                       tibble(variable = names(vif_twfe), two_way_fe_vif = as.numeric(vif_twfe)), by = "variable") %>%
  arrange(desc(two_way_fe_vif))
save_table("19_vif_diagnostics", vif_table)

design_metrics <- tibble(
  design = c("Raw regressors", "Two-way-FE transformed regressors"),
  observations = c(nrow(x_raw), nrow(x_twfe)), variables = c(sum(keep_raw), sum(keep_twfe)),
  rank = c(qr(scale(x_raw[, keep_raw, drop = FALSE]))$rank,
           qr(scale(x_twfe[, keep_twfe, drop = FALSE]))$rank),
  condition_number = c(kappa(scale(x_raw[, keep_raw, drop = FALSE])),
                       kappa(scale(x_twfe[, keep_twfe, drop = FALSE])))
)
save_table("20_design_matrix_diagnostics", design_metrics)

interaction_summary <- panel_d %>% group_by(supply_group) %>% summarise(
  observations = n(), mean = mean(shk_x_inel), sd = sd(shk_x_inel),
  min = min(shk_x_inel), max = max(shk_x_inel), nonzero_pct = 100 * mean(shk_x_inel != 0),
  distinct_values = n_distinct(shk_x_inel), .groups = "drop"
)
save_table("21_interaction_variation", interaction_summary)

# Strongest off-diagonal control-lag correlations for interpretation.
lag_pairs <- cor_long(lag_corr) %>% filter(variable_1 < variable_2) %>%
  mutate(abs_correlation = abs(correlation)) %>% arrange(desc(abs_correlation)) %>% slice_head(n = 15)
save_table("22_strongest_control_lag_correlations", lag_pairs)

# -----------------------------------------------------------------------------#
# 7. Preliminary residual dependence diagnostics relevant to inference
# -----------------------------------------------------------------------------#
fe_diagnostics <- bind_rows(lapply(c(0, 4, 8, 12), function(h) {
  dep <- paste0("dep_", sprintf("%02d", h))
  vars <- c(dep, model_x, "district_id", "quarter")
  dat <- panel_d[complete.cases(panel_d[, vars]), vars]
  f <- as.formula(paste(dep, "~", paste(model_x, collapse = " + "),
                        "+ factor(district_id) + factor(quarter)"))
  fit <- lm(f, data = dat)
  dat$residual <- residuals(fit)
  serial <- dat %>% arrange(district_id, quarter) %>% group_by(district_id) %>%
    mutate(residual_lag = lag(residual)) %>% ungroup() %>% filter(is.finite(residual_lag))
  residual_ar1 <- cor(serial$residual, serial$residual_lag)
  wide <- dat %>% select(district_id, quarter, residual) %>%
    pivot_wider(names_from = district_id, values_from = residual) %>% arrange(quarter)
  residual_matrix <- as.matrix(wide[, -1, drop = FALSE])
  cmat <- cor(residual_matrix, use = "pairwise.complete.obs")
  upper <- cmat[upper.tri(cmat)]
  tibble(horizon = h, observations = nrow(dat), time_periods = n_distinct(dat$quarter),
         residual_ar1 = residual_ar1, mean_pairwise_residual_correlation = mean(upper, na.rm = TRUE),
         mean_absolute_pairwise_correlation = mean(abs(upper), na.rm = TRUE),
         max_absolute_pairwise_correlation = max(abs(upper), na.rm = TRUE),
         adjusted_r_squared = summary(fit)$adj.r.squared)
}))
save_table("23_fe_residual_dependence", fe_diagnostics)

# -----------------------------------------------------------------------------#
# 8. Plain Markdown report with all generated results and interpretations
# -----------------------------------------------------------------------------#
nonzero_missing <- missingness %>% filter(missing_n > 0)
failed_integrity <- integrity %>% filter(status != "PASS")
max_vif_row <- vif_table %>% filter(is.finite(two_way_fe_vif)) %>% slice_max(two_way_fe_vif, n = 1, with_ties = FALSE)
max_corr_row <- lag_pairs %>% slice_head(n = 1)
median_cancel <- median(shock_panel$cancel_ratio_ois20y, na.rm = TRUE)
mean_cancel <- mean(shock_panel$cancel_ratio_ois20y, na.rm = TRUE)
max_shock <- shock_extremes %>% slice_head(n = 1)
h0 <- horizon_samples %>% filter(horizon == 0)
h12 <- horizon_samples %>% filter(horizon == 12)
price_ar1 <- acf_table %>% filter(variable == "dQ_composite", lag == 1) %>% pull(correlation)

report <- character()
add <- function(...) report <<- c(report, ...)
add_table <- function(title, name, x, digits = 3) {
  add(paste0("### ", title), "", paste0("CSV: [`tables/", name, ".csv`](tables/", name, ".csv)"), "",
      md_table(x, digits), "")
}
add_figure <- function(title, name, note = NULL) {
  add(paste0("### ", title), "", paste0("![", title, "](figures/", name, ".png)"), "")
  if (!is.null(note)) add(note, "")
}

add("# Panel and Variable Diagnostics", "",
    paste0("**Generated:** ", format(Sys.time(), "%Y-%m-%d %H:%M")), "",
    "**Inputs:** `data/01_panel.RData` and diagnostic reference objects from `data/00_prep_output.RData`.", "",
    "**Scope:** This is a pre-estimation audit. Descriptive group differences and diagnostic regressions are not causal estimates. The script does not alter `00_data_prep.R`, `01_data_gen.R`, or either saved input object.", "",
    "## Executive assessment", "")

if (nrow(failed_integrity) == 0) {
  add(paste0("The stored panel passes all core structural checks: ", nrow(panel_d), " rows form ", n_d,
             " unique districts by ", n_t, " quarters, with unique keys, complete Cartesian coverage, and positive finite price inputs."), "")
} else {
  add(paste0("Core structural checks requiring attention: ", paste(failed_integrity$diagnostic, collapse = ", "), "."), "")
}
add(paste0("The usable LP sample nevertheless declines from ", h0$complete_case_n, " observations at h=0 to ",
           h12$complete_case_n, " at h=12. This is expected from four lagged changes, the t-1 base, and future leads; it should be disclosed because long-horizon estimates use fewer and earlier quarters."), "")
add(paste0("The median shock cancellation ratio is ", sprintf("%.2f", median_cancel), " (mean ", sprintf("%.2f", mean_cancel),
           "). Values far below one indicate that a quarterly net sum suppresses offsetting event-level surprises; easing/tightening components and alternative instruments should remain planned robustness checks."), "")
add(paste0("Quarterly composite-price growth has pooled AR(1) = ", sprintf("%.2f", price_ar1),
           ". Momentum and the overlapping cumulative LP outcomes therefore create a clear serial-correlation concern."), "")
add(paste0("The largest two-way-FE VIF is ", sprintf("%.2f", max_vif_row$two_way_fe_vif), " for `", max_vif_row$variable,
           "`; the strongest pairwise control-lag correlation is ", sprintf("%.2f", max_corr_row$correlation), " between `",
           max_corr_row$variable_1, "` and `", max_corr_row$variable_2, "`. VIFs diagnose linear dependence among regressors, not bias from omitted variables."), "")
add("Income from 2023 onward is extrapolated using a common nominal wage-index path, unemployment in 2025 is carried from 2024, and population after 2025 Q1 is carried forward. These are non-missing observations but not newly observed district information. Baseline estimates should therefore be accompanied by a pre-2023 or pre-tail sensitivity sample.", "")
add(paste0("The binary supply split is strongly geographically and compositionally structured: all ",
           sum(static_district$vienna == "Vienna"), " Vienna districts are classified as inelastic, accounting for ",
           sum(static_district$vienna == "Vienna" & static_district$inelastic_dummy == 1), " of ",
           sum(static_district$inelastic_dummy == 1), " inelastic districts; `blr_14` and the fixed house weight correlate at ",
           sprintf("%.2f", cor(static_district$blr_14, static_district$w_house)),
           ". The interaction can therefore mix supply constraints with Vienna and property composition unless Vienna-exclusion, property-specific outcomes, and a continuous reserve specification are reported."), "")

add("## 1. Panel structure, missingness, and horizon samples", "",
    "Structural NAs in outcomes and lags are expected. They arise from lag/lead construction and must not be confused with missing source data.", "")
add_table("Panel integrity checks", "01_panel_integrity", integrity, 3)
add_table("Upstream input join-key contracts", "01b_input_join_contracts", input_contracts, 2)
add_table("Variables with non-zero missingness", "02_missingness_all_variables", nonzero_missing, 2)
add_table("LP horizon estimation samples", "03_horizon_estimation_samples", horizon_samples, 2)
add_figure("Sample size by LP horizon", "01_horizon_sample_size",
           "All districts remain represented, but each additional horizon removes the latest quarter. Comparing coefficients across horizons therefore also compares progressively earlier samples.")

add("## 2. Price outcomes and momentum", "",
    "The composite is formed in levels using fixed 2011 dwelling-stock shares and logged only afterward. This avoids time-varying endogenous weights, but it embeds a fixed property-mix choice. EFH-only, ETW-only, reclassified two-dwelling stock, and Vienna sensitivity specifications remain important.", "")
add_table("Price and growth summary", "04_price_summary", price_summary, 4)
add_table("Supply-group descriptive summary", "05_supply_group_summary", group_summary, 4)
add_table("Pooled autocorrelations", "06_pooled_autocorrelations", acf_table, 3)
add_figure("Median price indexes by property type", "02_price_indexes_property_type")
add_figure("Composite price index by supply group", "03_composite_index_by_supply_group",
           "These are unadjusted medians. Divergence can reflect urbanity, dwelling composition, income, or other district characteristics and is not evidence of monetary-policy transmission by itself.")
add_figure("Quarterly price-growth distribution", "04_price_growth_by_supply_group")
add_figure("Autocorrelation of outcome and controls", "05_autocorrelation",
           "Positive housing-price momentum motivates the four lagged price changes used by Aastveit and Anundsen. Sparse annual controls can show mechanical lag patterns because values are repeated within years.")

add("## 3. Composite weights and supply classification", "",
    paste0("The maximum error in `w_house + w_apt = 1` is ", format(weight_identity_error, scientific = TRUE),
           ", and the maximum recomputation error for the composite is ", format(composite_identity_error, scientific = TRUE), "."), "",
    paste0("The inelastic threshold is ", sprintf("%.3f", q25_nonvienna),
           ", computed from the 25th percentile outside Vienna and then applied to all districts. This is a proxy classification, not a directly estimated housing-supply elasticity."), "",
    paste0("A central identification warning is that all Vienna districts and all Salzburg districts are classified as inelastic, while the reserve measure correlates ",
           sprintf("%.3f", cor(static_district$blr_14, static_district$w_house)),
           " with the fixed house share. The dummy is therefore entangled with geography and property mix. District fixed effects remove their level differences, but they do not rule out different responses to common shocks arising from these correlated characteristics."), "")
add_table("Weights and reserve summary", "07_weights_and_reserves_summary", weight_summary, 5)
add_table("Supply-classification metrics", "08_supply_classification_metrics", classification_metrics, 4)
add_table("Supply classification by Vienna status", "09_supply_classification_groups", classification_summary, 4)
add_table("Federal-state supply summary", "10_state_supply_summary", state_reserves, 4)
add_figure("Fixed house-weight distribution", "06_house_weight_distribution",
           "Vienna is retained because district-level data are available, but its apartment-heavy stock makes separate outcome and Vienna-exclusion checks especially informative.")
add_figure("Reserve distribution and threshold", "07_reserve_distribution_threshold")
add_figure("Reserve measure by federal state", "08_reserves_by_state",
           "Strong geographic clustering would reinforce the need to discuss regional confounding and spatial dependence. Fixed district effects absorb time-invariant level differences, but not differential exposure to omitted time-varying regional shocks.")

add("## 4. Monetary-policy shock", "",
    "Negative OIS 20-year surprises denote easing. Consequently, if easing raises prices more in inelastic districts, the coefficient on the signed easing interaction is expected to be negative: a negative coefficient multiplied by a negative shock produces a positive differential response. This sign convention must be stated in every coefficient/IRF interpretation.", "",
    "With quarter fixed effects, the common national shock main effect is absorbed. The interaction identifies only the differential response of inelastic relative to elastic districts; it is not the total response of either group.", "")
add_table("Shock distribution", "11_shock_summary", shock_summary, 4)
add_table("Shock consistency and coverage", "12_shock_consistency_checks", shock_checks, 5)
add_table("Largest absolute net-shock quarters", "13_largest_shock_quarters", shock_extremes, 4)
add_figure("Quarterly net OIS shock", "09_shock_time_series",
           paste0("The largest absolute net shock occurs in ", max_shock$quarter_label, ". Such quarters may have high leverage; report leave-one-quarter-out sensitivity rather than deleting them mechanically."))
add_figure("Signed easing and tightening components", "10_shock_signed_components")
add_figure("Shock cancellation ratio", "11_shock_cancellation",
           "A low ratio means easing and tightening surprises occurred within the same quarter and offset in the net measure. The ratio is a diagnostic, not an additional baseline regressor.")

add("## 5. Controls and constructed tails", "",
    "Income totals are annual and repeated quarterly, then divided by quarterly population. Thus within-year income-per-capita changes partly mirror population movements. From 2023, all districts also share the same Tariflohnindex growth factor. Unemployment is annual and repeated quarterly, concentrating changes at year boundaries. These construction choices reduce effective within-district information even though the panel contains no source-level NAs.", "",
    "Tourism intensity is not included because consistent district-level data over the sample are unavailable. This should be recorded as a data limitation rather than approximated with inconsistent federal-state series. Migration can be considered later if consistent annual district data become available.", "")
add_table("Observed and constructed control spans", "14_constructed_control_coverage", source_coverage, 2)
add_table("Control flatness by period", "15_control_flatness_by_period", zero_change, 5)
add_table("Within/between variation decomposition", "16_within_between_variation", variation, 4)
add_figure("Within/between variation", "12_within_between_variation",
           "District fixed effects remove between-district levels. Variables with little within share or mechanically flat quarters contribute limited identifying variation in the FE model.")
add_figure("Control changes over time", "13_control_changes_over_time",
           "Spikes synchronized across districts may overlap with quarter fixed effects; annual repetition also means four nominal lags do not represent four independent quarterly innovations.")

add("## 6. Correlation and multicollinearity diagnostics", "",
    "The first matrix covers key contemporaneous variables; the second focuses on the 16 planned lagged controls. Correlations involving the common shock are descriptive because the shock is repeated for every district. Pairwise correlations and VIFs should be read jointly: high lag-to-lag correlation may inflate standard errors without invalidating the control concept.", "",
    "The approximately -0.50 same-lag correlations between income growth and population growth are partly mechanical: income per capita is constructed by dividing annual district income totals by quarterly population. This explains why their fourth lags have the largest FE-adjusted VIFs. The full-rank design and moderate condition number do not indicate exact multicollinearity, but a specification omitting one control family is a useful sensitivity check.", "")
add_table("Key-variable correlation matrix", "17_key_variable_correlations", key_corr_table, 4)
add_table("Control-lag correlation matrix", "18_control_lag_correlations", lag_corr_table, 4)
add_table("VIF diagnostics", "19_vif_diagnostics", vif_table, 3)
add_table("Design-matrix rank and condition number", "20_design_matrix_diagnostics", design_metrics, 3)
add_table("Interaction variation by supply group", "21_interaction_variation", interaction_summary, 4)
add_table("Strongest control-lag correlations", "22_strongest_control_lag_correlations", lag_pairs, 4)
add_figure("Key-variable correlation heatmap", "14_key_correlation_heatmap")
add_figure("Lagged-control correlation heatmap", "15_control_lag_correlation_heatmap")

add("## 7. Dependence and the standard-error decision", "",
    "The following is a preliminary two-way-FE residual diagnostic at selected horizons. Pairwise residual correlations are descriptive: demeaning and the short time dimension affect formal cross-sectional-dependence tests.", "")
add_table("Two-way-FE residual dependence", "23_fe_residual_dependence", fe_diagnostics, 4)
add("Overlapping cumulative LP outcomes mechanically induce serial correlation, while a common monetary shock, national housing cycle, and geographically linked districts make cross-sectional dependence plausible. This must be settled before estimation and documented in the empirical-strategy chapter.", "",
    "A defensible workflow is to estimate both (i) two-way clustered standard errors by district and quarter and (ii) Driscoll-Kraay standard errors with a horizon-aware lag choice, then compare inference transparently. Neither is automatically ideal here: two-way clustering has only 36 time clusters, while Driscoll-Kraay asymptotics are also strained by T=36 and horizons up to 12. Small-sample corrections and shorter-horizon sensitivity should be considered. The diagnostics do not justify selecting whichever estimator yields significance.", "")

add("## 8. Literature alignment and methodological review", "",
    "**Aastveit and Anundsen (2018).** The present design follows their use of cumulative log housing-price changes, local projections, asymmetric shocks, and four lags of house-price and income changes. It is an adaptation rather than a replication: the Austrian panel uses a binary pre-sample land-reserve proxy instead of a continuous Saiz elasticity, population and unemployment rather than net migration and branching deregulation, quarterly euro-area surprises, and district plus quarter fixed effects. Robustness should therefore include the continuous reserve measure, alternative thresholds, asymmetric shocks, alternative shock instruments, and property-specific outcomes.", "",
    "**Albuquerque (2024).** The FAVAR evidence supports housing-supply, debt/credit, and wealth channels, but it is not directly comparable to this panel LP. It also cautions against treating supply constraints as the sole source of heterogeneity. Unobserved leverage, credit access, ownership, urbanity, and regional structure can still shape responses. Fixed effects and predetermined weights/reserves improve design discipline but do not establish causal exogeneity.", "",
    "**Identification.** The high-frequency shock must be exogenous to Austrian district housing conditions, including central-bank information effects. The interaction additionally requires that no omitted district characteristic correlated with land reserves has a different time-varying response to the same shock. Spillovers across districts and measurement error in the reserve proxy can weaken this interpretation.", "",
    "**Recommended pre-estimation specification set.** Keep both composite and property-specific outcomes ready; report Vienna-included and Vienna-excluded results; estimate binary and continuous supply measures; compare net, easing, tightening, DE20Y and DE30Y shocks; use a pre-2023/pre-tail sample; inspect leave-one-shock-quarter-out sensitivity; and report both candidate covariance estimators. Decide baseline ordering from theory and measurement quality, not from coefficient significance.", "")

add("## 9. Reproducibility and output index", "",
    "All tables shown above are also stored as machine-readable CSV files in `tables/`; every chart is stored in `figures/`. Re-run from the project root with:", "",
    "```powershell", "& \"C:\\Program Files\\R\\R-4.3.2\\bin\\Rscript.exe\" code/02_diagnostics.R", "```", "",
    "The report intentionally does not overwrite the final panel. Diagnostic-only flags (constructed income, carried unemployment/population, Vienna, state, and supply group) exist only while this script runs.")

con <- file(file.path(OUT_DIR, "diagnostics_report.md"), open = "w", encoding = "UTF-8")
writeLines(report, con = con, useBytes = TRUE)
close(con)

message("Diagnostics complete.")
message("Report: ", normalizePath(file.path(OUT_DIR, "diagnostics_report.md"), winslash = "/", mustWork = FALSE))
message("Tables: ", length(list.files(TAB_DIR, pattern = "\\.csv$")))
message("Figures: ", length(list.files(FIG_DIR, pattern = "\\.png$")))
