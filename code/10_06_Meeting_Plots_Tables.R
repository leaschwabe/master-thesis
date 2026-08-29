
# (1) Panel Dataset and Variable Inventory

## overview table (variable, model role, frequency, sample coverage, observed/constructed, open issue)
### prices, blr shock(s), income pc, unemployment, population



var_overview <- tibble(
  Variable = c(
    "RE prices (apartment / house / land)",
    "Reserve share (2014)",
    "Inelasticity dummy",
    "Shock",
    "Net income (per capita)",
    "Unemployment rate",
    "Population"
  ),
  `Model Role` = c(
    "Dependent variable(s)",
    "Heterogeneity variable",
    "Alternative heterogeneity variable",
    "Shock variable",
    "Control",
    "Control",
    "Control"
  ),
  Frequency = c(
    "Quarterly",
    "Time-invariant",
    "Time-invariant",
    "Quarterly",
    "Quarterly",
    "Quarterly",
    "Quarterly"
  ),
  `Sample Coverage` = c(
    "Q1/2017 - Q4/2025, district-level",
    "2014, district-level",
    "2014, district-level",
    "Q1/2017 - Q4/2025",
    "Q1/2017 – Q4/2025, district-level",
    "Q1/2017 - Q4/2023 \n(2024–2025 missing)",
    "Q1/2017 –Q4/2025"
  ),
  `Source Type` = c(
    "Observed",
    "Observed",
    "Constructed",
    "Constructed",
    "Constructed + extrapolated",
    "Constructed",
    "Observed"
  ),
  `Construction / Treatment` = c(
    "District-level quarterly price series; separate candidate outcomes",
    "Share of building land reserves in total building land",
    "Low-reserve district classification using alternative thresholds",
    "Event-level monetary surprises aggregated to quarter; multiple instruments / aggregation choices",
    "Annual district-level (net) income totals extrapolated using standard wage rate index (OeNB, 2022-2025), then divided by quarterly population",
    "Annual district-level unemployment rate repeated across quarters",
    "Quarterly district-level population; 2025 Q2–Q4 filled from Q1 2025"
  ),
  `Open Issue` = c(
    "Choose main outcome vs. robustness outcomes",
    "Use of continuous interaction in baseline",
    "Threshold choice sensitive to Vienna (if dummy is used)",
    "Instrument choice and within-quarter cancellation",
    "Baseline control or robustness only?",
    "How to handle 2024–2025",
    "Minor end-of-sample carry-forward only"
  )
)

var_overview

# (2) Price Data

## Summary statistics per unit of measurement (mean, sd, min, max, no. of observations)
### Time series plot of median (AT national), maybe also per elastic/inelastic classification (inelastic vs. elastic districts)
### Boxplots for cross-sectional heterogeneity

# Summary Table:

price_summary <- prices %>%
  summarise(
    mean_house = mean(efh_avg, na.rm = TRUE),
    sd_house   = sd(efh_avg, na.rm = TRUE),
    min_house  = min(efh_avg, na.rm = TRUE),
    max_house  = max(efh_avg, na.rm = TRUE),
    n_house    = sum(!is.na(efh_avg)),
    
    mean_apartment = mean(etw_avg, na.rm = TRUE),
    sd_apartment   = sd(etw_avg, na.rm = TRUE),
    min_apartment  = min(etw_avg, na.rm = TRUE),
    max_apartment  = max(etw_avg, na.rm = TRUE),
    n_apartment    = sum(!is.na(etw_avg)),
    
    mean_land = mean(gst, na.rm = TRUE),
    sd_land   = sd(gst, na.rm = TRUE),
    min_land  = min(gst, na.rm = TRUE),
    max_land  = max(gst, na.rm = TRUE),
    n_land    = sum(!is.na(gst))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("statistic", "property_type"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(
    property_type = recode(
      property_type,
      house = "Houses",
      apartment = "Apartments",
      land = "Land"
    )
  ) %>%
  select(property_type, mean, sd, min, max, n)

# Plots # 

## National Median over Time (all three property times) ##

price_median_all <- prices %>%
  mutate(
    quarter = as.yearqtr(quarter, format = "%Y.%q")
  ) %>%
  group_by(quarter) %>%
  summarise(
    house = median(efh_avg, na.rm = TRUE),
    apartment = median(etw_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(house, apartment),
    names_to = "property_type",
    values_to = "median_price"
  ) %>%
  mutate(
    property_type = recode(
      property_type,
      house = "Houses",
      apartment = "Apartments"
    )
  )


ggplot(
  price_median_all,
  aes(
    x = quarter,
    y = median_price,
    color = property_type,
    group = property_type
  )
) +
  geom_line(linewidth = 0.9) +
  scale_x_yearqtr(
    breaks = seq(
      from = as.yearqtr("2017.1", format = "%Y.%q"),
      to = max(price_median_all$quarter, na.rm = TRUE),
      by = 1
    ),
    format = "%Y Q%q"
  ) +
  labs(
    title = "Austria: Median Residential Price by Property Type",
    x = NULL,
    y = "Median price (€/m²)",
    color = NULL
  ) +
  theme_gray()

## By initial classification ##
prices <- prices%>%
  rename(district_id=bez_id)


price_class_plot <- prices %>%
  left_join(
    bl %>% select(district_id, inelastic_dummy),
    by = "district_id"
  ) %>%
  group_by(quarter, inelastic_dummy) %>%
  summarise(
    median_house = median(efh_avg, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    inelastic_group = ifelse(inelastic_dummy == 1, "Inelastic", "Elastic")
  )

price_class_plot <- price_class_plot %>%
  mutate(quarter = as.yearqtr(quarter, format = "%Y.%q" ))

ggplot(
  price_class_plot,
  aes(
    x = quarter,
    y = median_house,
    color = inelastic_group,
    group = inelastic_group
  )
) +
  geom_line(linewidth = 0.9) +
  scale_x_yearqtr(
    breaks = seq(
      from = as.yearqtr("2017 Q1", format = "%Y Q%q"),
      to = max(price_class_plot$quarter, na.rm = TRUE),
      by = 1
    ),
    format = "%Y Q%q"
  ) +
  scale_color_manual(
    values = c(
      "Inelastic" = "darkred",
      "Elastic" = "darkgreen"
    )
  ) +
  labs(
    title = "Median House Price by Inelasticity Classification (prelim.)",
    x = NULL,
    y = "Median house price",
    color = NULL
  ) +
  theme_gray()


## growth rates AT for presentation ##


price_growth_all <- price_median_all %>%
  arrange(property_type, quarter) %>%
  group_by(property_type) %>%
  mutate(
    qoq_growth = 100 * (median_price / lag(median_price, 1) - 1),
    yoy_growth = 100 * (median_price / lag(median_price, 4) - 1)
  ) %>%
  ungroup()


selected_growth <- price_growth_all %>%
  filter(
    quarter %in% as.yearqtr(
      c("2018.1", "2020.1", "2020.2", "2020.3", "2020.4", "2021.4", "2022.4", "2023.1", "2023.2", "2023.3", "2023.4", "2024.1"),
      format = "%Y.%q"
    )
  ) %>%
  select(property_type, quarter, median_price, qoq_growth, yoy_growth) %>%
  arrange(quarter, property_type)

selected_growth


## Growth rates by elasticity group ##

library(zoo)

price_growth_plot <- prices %>%
  mutate(
    quarter = as.yearqtr(quarter, format = "%Y.%q")
  ) %>%
  left_join(
    bl %>% select(district_id, inelastic_dummy),
    by = "district_id"
  ) %>%
  arrange(district_id, quarter) %>%
  group_by(district_id) %>%
  mutate(
    house_growth_yoy = 100 * ((efh_avg - lag(efh_avg, 4))/efh_avg)
  ) %>%
  ungroup() %>%
  group_by(quarter, inelastic_dummy) %>%
  summarise(
    avg_house_growth_yoy = mean(house_growth_yoy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    inelastic_group = ifelse(inelastic_dummy == 1, "Inelastic", "Elastic")
  ) %>%
  filter(!is.na(avg_house_growth_yoy))


ggplot(
  price_growth_plot,
  aes(
    x = quarter,
    y = avg_house_growth_yoy,
    fill = inelastic_group
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_col(
    position = position_dodge(width = 0.3),
    width = 0.15
  ) +
  scale_x_yearqtr(
    breaks = seq(
      from = as.yearqtr("2017.1", format = "%Y.%q"),
      to = max(price_growth_plot$quarter, na.rm = TRUE),
      by = 1
    ),
    format = "%Y Q%q"
  ) +
  scale_fill_manual(
    values = c(
      "Inelastic" = "darkred",
      "Elastic" = "darkgreen"
    )
  ) +
  labs(
    title = "Average House Price Growth by Inelasticity Classification (% yoy)",
    x = NULL,
    y = "Average growth rate (% yoy)",
    fill = NULL
  ) +
  theme_gray()


price_growth_plot_bar <- price_growth_plot %>%
  mutate(
    quarter_label = format(quarter, "%Y Q%q"),
    quarter_label = factor(
      quarter_label,
      levels = format(sort(unique(quarter)), "%Y Q%q")
    )
  )

ggplot(
  price_growth_plot_bar,
  aes(
    x = quarter_label,
    y = avg_house_growth_yoy,
    fill = inelastic_group
  )
) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_col(
    position = position_dodge2(width = 0.8, preserve = "single", padding = 0.1),
    width = 0.75
  ) +
  scale_x_discrete(
    breaks = format(
      seq(
        from = as.yearqtr("2017.1", format = "%Y.%q"),
        to = max(price_growth_plot$quarter, na.rm = TRUE),
        by = 1
      ),
      "%Y Q%q"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Inelastic" = "darkred",
      "Elastic" = "darkgreen"
    )
  ) +
  labs(
    title = "Average House Price Growth by Inelasticity Classification (% yoy)",
    x = NULL,
    y = "Average growth rate (% yoy)",
    fill = NULL
  ) +
  theme_gray()



## Indexed ##

price_index_plot <- prices %>%
  mutate(
    quarter = as.yearqtr(quarter, format = "%Y.%q")
  ) %>%
  left_join(
    bl %>% select(district_id, inelastic_dummy),
    by = "district_id"
  ) %>%
  filter(!is.na(inelastic_dummy)) %>%
  arrange(district_id, quarter) %>%
  group_by(district_id) %>%
  mutate(
    base_price = efh_avg[quarter == as.yearqtr("2017.1", format = "%Y.%q")][1],
    house_price_index = 100 * efh_avg / base_price
  ) %>%
  ungroup() %>%
  filter(!is.na(base_price)) %>%
  group_by(quarter, inelastic_dummy) %>%
  summarise(
    median_house_price_index = median(house_price_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    inelastic_group = ifelse(inelastic_dummy == 1, "Inelastic", "Elastic")
  )


ggplot(
  price_index_plot,
  aes(
    x = quarter,
    y = median_house_price_index,
    color = inelastic_group,
    group = inelastic_group
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  scale_x_yearqtr(
    breaks = seq(
      from = as.yearqtr("2017.1", format = "%Y.%q"),
      to = max(price_index_plot$quarter, na.rm = TRUE),
      by = 1
    ),
    format = "%Y Q%q"
  ) +
  scale_color_manual(
    values = c(
      "Inelastic" = "darkred",
      "Elastic" = "darkgreen"
    )
  ) +
  labs(
    title = "House Prices by Inelasticity Classification",
    subtitle = "Index: 2017 Q1 = 100",
    x = NULL,
    y = "Median district-level price index",
    color = NULL
  ) +
  theme_gray()


# (3) Shock Construction

## Table of quarterly shock diagnostics (mean, sd, min/max)
## Cancellation diagnostics (net shock, positive shock, negative shock, abs value, cancellation ratio)
### Maybe: time-series plot of shock

shock_summary <- shock_q %>%
  summarise(
    mean_ois20y = mean(shock_net_ois20y, na.rm = TRUE),
    sd_ois20y   = sd(shock_net_ois20y, na.rm = TRUE),
    min_ois20y  = min(shock_net_ois20y, na.rm = TRUE),
    max_ois20y  = max(shock_net_ois20y, na.rm = TRUE),
    
    mean_de20y = mean(shock_net_de20y, na.rm = TRUE),
    sd_de20y   = sd(shock_net_de20y, na.rm = TRUE),
    min_de20y  = min(shock_net_de20y, na.rm = TRUE),
    max_de20y  = max(shock_net_de20y, na.rm = TRUE),
    
    mean_de30y = mean(shock_net_de30y, na.rm = TRUE),
    sd_de30y   = sd(shock_net_de30y, na.rm = TRUE),
    min_de30y  = min(shock_net_de30y, na.rm = TRUE),
    max_de30y  = max(shock_net_de30y, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("statistic", "instrument"),
    names_sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(
    instrument = recode(
      instrument,
      ois20y = "OIS 20Y",
      de20y  = "DE20Y",
      de30y  = "DE30Y"
    )
  ) %>%
  select(instrument, mean, sd, min, max)

shock_summary

## Cancellation Diagnostics

shock_cancellation <- shock_q %>%
  select(
    quarter,
    shock_net_ois20y, shock_pos_ois20y, shock_neg_ois20y, shock_abs_ois20y,
    shock_net_de20y,  shock_pos_de20y,  shock_neg_de20y,  shock_abs_de20y,
    shock_net_de30y,  shock_pos_de30y,  shock_neg_de30y,  shock_abs_de30y
  ) %>%
  left_join(shock_cancel_ratio, by = "quarter") %>%
  summarise(
    mean_net_ois20y    = mean(shock_net_ois20y, na.rm = TRUE),
    mean_pos_ois20y    = mean(shock_pos_ois20y, na.rm = TRUE),
    mean_neg_ois20y    = mean(shock_neg_ois20y, na.rm = TRUE),
    mean_abs_ois20y    = mean(shock_abs_ois20y, na.rm = TRUE),
    mean_cancel_ois20y = mean(cancel_ratio_ois20y, na.rm = TRUE),
    
    mean_net_de20y    = mean(shock_net_de20y, na.rm = TRUE),
    mean_pos_de20y    = mean(shock_pos_de20y, na.rm = TRUE),
    mean_neg_de20y    = mean(shock_neg_de20y, na.rm = TRUE),
    mean_abs_de20y    = mean(shock_abs_de20y, na.rm = TRUE),
    mean_cancel_de20y = mean(cancel_ratio_de20y, na.rm = TRUE),
    
    mean_net_de30y    = mean(shock_net_de30y, na.rm = TRUE),
    mean_pos_de30y    = mean(shock_pos_de30y, na.rm = TRUE),
    mean_neg_de30y    = mean(shock_neg_de30y, na.rm = TRUE),
    mean_abs_de30y    = mean(shock_abs_de30y, na.rm = TRUE),
    mean_cancel_de30y = mean(cancel_ratio_de30y, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("statistic", "instrument"),
    names_sep = "_(?=[^_]+$)"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  mutate(
    instrument = recode(
      instrument,
      ois20y = "OIS 20Y",
      de20y  = "DE20Y",
      de30y  = "DE30Y"
    )
  ) %>%
  select(instrument, mean_net, mean_pos, mean_neg, mean_abs, mean_cancel)

shock_cancellation

## Quarterly Table of Shock Metrics incl. Cancellation Ratio

shock_quarter_table <- shock_q %>%
  left_join(shock_cancel_ratio, by = "quarter") %>%
  select(
    quarter,
    shock_net_ois20y, shock_pos_ois20y, shock_neg_ois20y, shock_abs_ois20y, cancel_ratio_ois20y,
    shock_net_de20y,  shock_pos_de20y,  shock_neg_de20y,  shock_abs_de20y,  cancel_ratio_de20y,
    shock_net_de30y,  shock_pos_de30y,  shock_neg_de30y,  shock_abs_de30y,  cancel_ratio_de30y
  )

shock_quarter_table

## Time-Series Plots of Candidate (net) Shock Series

shock_plot_data <- shock_q %>%
  select(quarter, shock_net_ois20y) %>%
  pivot_longer(
    cols = starts_with("shock_net_"),
    names_to = "instrument",
    values_to = "shock"
  ) %>%
  mutate(
    instrument = recode(
      instrument,
      shock_net_ois20y = "OIS 20Y",
      shock_net_de20y  = "DE20Y",
      shock_net_de30y  = "DE30Y"
    )
  )

pepp_start <- as.yearqtr("2020 Q1", format = "%Y Q%q")
pepp_end   <- as.yearqtr("2022 Q1", format = "%Y Q%q")

ggplot(shock_plot_data, aes(x = quarter, y = shock)) +
  geom_line(linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = pepp_start, linetype = "dashed", linewidth = 0.7, color = "red") +
  geom_vline(xintercept = pepp_end,   linetype = "dashed", linewidth = 0.7, color = "red") +
  facet_wrap(~ instrument, ncol = 1, scales = "free_y") +
  labs(
    title = "Quarterly Monetary Policy Shock (EAMPD)",
    x = NULL,
    y = "Net Quarterly Shock"
  ) +
  theme_gray()

# (4) Inelasticity Measure

## Boxplot per Bundesland
## Histogram of full sample
## histogram excluding Vienna
### Classification alternatives (q25 vs. q25 excluding Vienna, q20... etc.) and current conclusion

ggplot(blr_diagnostics, aes(x = bundesland, y = blr_14)) + 
  geom_boxplot() + 
  labs(
    x = "Bundesland", 
    y = "Building Land Reserve by Bundesland"
  ) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## With individual district points

ggplot(blr_diagnostics, aes(x = bundesland, y = blr_14)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6) +
  labs(
    title = "Building land reserve share by Bundesland",
    x = "Bundesland",
    y = "Reserve share in total building land"
  ) +
  theme_gray() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Histograms

ggplot(
  blr_woV, aes(x = blr_14)) +
  geom_histogram(bins = 46) +
  labs(
    x = "Building Land Reserve by Bundesland (w/o Wien)",
    y = "Count") +
  theme_gray()

ggplot(
  blr_diagnostics, aes(x = blr_14)) +
  geom_histogram(bins = 58) +
  labs(
    x = "Building Land Reserve by Bundesland",
    y = "Count") +
  theme_gray()

## Histogram (combined)

hist_data <- blr_diagnostics %>%
  mutate(sample = "Full sample") %>%
  bind_rows(
    blr_diagnostics %>%
      filter(bundesland != "Wien") %>%
      mutate(sample = "Excluding Vienna")
  )


ggplot(hist_data, aes(x = blr_14, fill = sample)) +
  geom_histogram(
    position = "identity",
    alpha = 0.45,
    bins = 58
  ) +
  labs(
    title = "Distribution of building land reserve shares",
    x = "Reserve share in total building land",
    y = "Count",
    fill = NULL
  ) +
  theme_minimal()

# Classification Options:

# q25_th = Q25 without Vienna

q20_th <- blr_woV%>%
  summarise(q20 = quantile(blr_14, 0.20, na.rm = TRUE)) %>%
  pull (q20)

q25_full <- blr_diagnostics%>%
  summarise(q25 = quantile(blr_14, 0.25, na.rm = TRUE)) %>%
  pull (q25)

q20_full <- blr_diagnostics%>%
  summarise(q20 = quantile(blr_14, 0.20, na.rm = TRUE)) %>%
  pull (q20)

## Table ##

classification_table <- tibble(
  rule = c(
    "Q25 pooled sample",
    "Q20 pooled sample",
    "Q25 excluding Vienna",
    "Q20 excluding Vienna"
  ),
  threshold = c(
    q25_full,
    q20_full,
    q25_th,
    q20_th
  )
) %>%
  mutate(
    n_inelastic = c(
      sum(blr_diagnostics$blr_14 <= q25_full, na.rm = TRUE),
      sum(blr_diagnostics$blr_14 <= q20_full, na.rm = TRUE),
      sum(blr_diagnostics$blr_14 <= q25_th, na.rm = TRUE),
      sum(blr_diagnostics$blr_14 <= q20_th, na.rm = TRUE)
    ),
    share_inelastic = n_inelastic / nrow(blr_diagnostics)
  )

classification_table

## Who gets classified:

districts_inelastic <- blr_diagnostics %>%
  mutate(inelastic_q25_no_vienna = ifelse(blr_14 <= q25_th, 1, 0)) %>%
  filter(inelastic_q25_no_vienna == 1) %>%
  arrange(blr_14) %>%
  select(district_id, district_name, bundesland, blr_14)

# (5) Control Vector (population, unemployment, income pc)
