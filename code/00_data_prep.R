## Packages needed for Data cleaning ##

library(dplyr)
library(tidyr)
library(knitr)
library(zoo)
library(ggplot2)
library(lubridate)
library(stringr)
library(tibble)

# Loading endogenous variables: EFH, ETW, GST #
## Loading dataset, renaming of id/name columns, transforming date correctly and sorting by district_id & quarter ##
##
## OUTCOME PLAN: RE_prices has three price measures:
##   gst      = building land (Bauland) -> excluded from the housing outcome
##   efh_avg  = single-family houses
##   etw_avg  = apartments (condos)
## Primary outcome = a district-level COMPOSITE of efh & etw, using fixed 2011 dwelling-stock
## weights (constructed below, right after the prices). Robustness = efh-only excluding Vienna
## (co-equal identification check, cleanest of the composition/elasticity confound) and etw-only.
## Baseline vs. robustness ordering to be finalised (see structure ss. 7.1-7.3). The composite
## avoids excluding Vienna (its thin single-family market is down-weighted, apartments up-weighted).

prices <- read.csv("./data/RE_prices.csv", header = TRUE ,sep=";")

prices <- prices %>%
  rename(
    district_id = bez_id,
    district_name = bez_name
  ) %>%
  mutate(
    quarter = as.yearqtr(
      paste(
        as.integer(floor(quarter)),
        as.integer(round((quarter - floor(quarter)) * 10))
      ),
      format = "%Y %q"
    )
  ) %>%
  arrange(district_id, quarter)

# -----------------------------------------------------------------------------#

# Composite outcome weights: fixed 2011 dwelling-stock shares (house vs. apartments)
## Source: Statistik Austria Gebaeude- und Wohnungszaehlung 2011, per district.
## The columns count DWELLINGS by the size of the building they sit in:
##   w1     = dwellings in single-dwelling buildings
##   w2     = dwellings in two-dwelling buildings
##   w3_5   = dwellings in buildings with 3-5 dwellings
##   w6_10  = dwellings in buildings with 6-10 dwellings
##   w11_20 = dwellings in buildings with 11-20 dwellings
##   w21p   = dwellings in buildings with 21+ dwellings
## Dwelling counts (not building counts) are used because the price series are per-dwelling,
## so the weight should reflect the number of dwellings of each type actually being priced.
## HOUSE class = w1 + w2: single-dwelling plus two-dwelling buildings, the latter to capture
## Doppelhaushaelften / duplexes, which behave like the owner-occupied "house" (efh) market.
## APARTMENT class = w3_5 + w6_10 + w11_20 + w21p (condo/ETW-type stock).
## (Robustness idea: move w2 into the apartment class to probe this classification choice.)
## Weights are FIXED at 2011 (pre-sample) to keep them exogenous, mirroring the 2014
## building-land logic. Used later (panel/estimation script) to build the composite outcome:
##   y_composite = w_house * dlog(efh) + w_apt * dlog(etw).

cw <- read.csv("./data/composite_weight_data.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

## Drop the trailing source-note / blank rows automatically (content-based, idempotent).
cw <- cw %>%
  filter(!is.na(suppressWarnings(as.integer(district_id))))

composite_weights <- cw %>%
  mutate(
    district_id = as.integer(district_id),
    across(c(w1, w2, w3_5, w6_10, w11_20, w21p), as.numeric),
    n_house = w1 + w2,
    n_apt   = w3_5 + w6_10 + w11_20 + w21p,
    n_tot   = n_house + n_apt,
    w_house = n_house / n_tot,
    w_apt   = n_apt / n_tot
  ) %>%
  select(district_id, district_name, w_house, w_apt, n_house, n_apt, n_tot) %>%
  arrange(district_id)

# -----------------------------------------------------------------------------#

# Loading the exogenous variables: (1) shock (EAMPD)


shock <- read.csv("./data/EAMPD_event.csv", header = TRUE, sep =";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")
shock$X.date <- as.Date(gsub('"', "", shock$X.date), format= "%d.%m.%Y")
shock <- subset(
  shock, 
  X.date >= as.Date("2010-01-01"),
  select = c("X.date", "OIS_20Y", "DE20Y", "DE30Y")
)

### Construction of aggregate quarterly MP shock:
  ## TODO (SIGN CONVENTION - lock before estimation): OIS_20Y is the chosen primary instrument.
  ## A negative value = downward revision of long-term rates = monetary EASING surprise.
  ## Because the outcome/interaction is coded on the raw signed sum, the expected sign of beta_h
  ## on (Shock x Inelastic) flips with this convention. Make the Data + Empirical Strategy
  ## chapters state the convention explicitly so the interpretation of beta_h is not reversed.
  ## (DE20Y/DE30Y are retained for robustness only.)
  ## shock_net_** = net sum of shocks per quarter, 
  ## shock_pos_** = sum of positive shocks per quarter,
  ## shock_neg_** = sum of negative shocks per quarter, 
  ## shock_abs_** = absolute sum of shocks per quarter, 
  ## n_nonmiss_** = number of non-missing observations within a quarter (i.e. 3 is three event dates in a quarter and has three observed values)

shock_q <- shock %>%
  mutate(
    quarter = as.yearqtr(X.date)
  ) %>%
  group_by(quarter) %>%
  summarise(
    
    # OIS_20Y
    shock_net_ois20y = if (all(is.na(OIS_20Y))) NA_real_ else sum(OIS_20Y, na.rm = TRUE),
    shock_pos_ois20y = if (all(is.na(OIS_20Y))) NA_real_ else sum(ifelse(OIS_20Y > 0, OIS_20Y, 0), na.rm = TRUE),
    shock_neg_ois20y = if (all(is.na(OIS_20Y))) NA_real_ else sum(ifelse(OIS_20Y < 0, OIS_20Y, 0), na.rm = TRUE),
    shock_abs_ois20y = if (all(is.na(OIS_20Y))) NA_real_ else sum(abs(OIS_20Y), na.rm = TRUE),
    n_nonmiss_ois20y = sum(!is.na(OIS_20Y)),
    
    # DE20Y
    shock_net_de20y = if (all(is.na(DE20Y))) NA_real_ else sum(DE20Y, na.rm = TRUE),
    shock_pos_de20y = if (all(is.na(DE20Y))) NA_real_ else sum(ifelse(DE20Y > 0, DE20Y, 0), na.rm = TRUE),
    shock_neg_de20y = if (all(is.na(DE20Y))) NA_real_ else sum(ifelse(DE20Y < 0, DE20Y, 0), na.rm = TRUE),
    shock_abs_de20y = if (all(is.na(DE20Y))) NA_real_ else sum(abs(DE20Y), na.rm = TRUE),
    n_nonmiss_de20y = sum(!is.na(DE20Y)),
    
    # DE30Y
    shock_net_de30y = if (all(is.na(DE30Y))) NA_real_ else sum(DE30Y, na.rm = TRUE),
    shock_pos_de30y = if (all(is.na(DE30Y))) NA_real_ else sum(ifelse(DE30Y > 0, DE30Y, 0), na.rm = TRUE),
    shock_neg_de30y = if (all(is.na(DE30Y))) NA_real_ else sum(ifelse(DE30Y < 0, DE30Y, 0), na.rm = TRUE),
    shock_abs_de30y = if (all(is.na(DE30Y))) NA_real_ else sum(abs(DE30Y), na.rm = TRUE),
    n_nonmiss_de30y = sum(!is.na(DE30Y)),
    
    # general quarter info
    n_events = n(),
    .groups = "drop"
  ) %>%
  arrange(quarter)

### Restricting the quarterly shock to the maximum available estimation period (from prices data set) ###

shock_q <- shock_q %>%
  filter(
    quarter >= as.yearqtr("2017 Q1", format = "%Y Q%q"),
    quarter <= as.yearqtr("2025 Q4", format = "%Y Q%q")
  )

# -----------------------------------------------------------------------------#

#### Preliminary diagnostics of "cancellation" within a quarter i.e. if positive shocks offset negative ones and v.v. ####
  ### ratio of net_shocks to abs_shocks i.e. if near one -> little offsetting of directional shocks, if near 0 -> strong offsetting of directional shocks

shock_cancel_ratio <- shock_q %>%
  mutate(
    cancel_ratio_de20y = ifelse(shock_abs_de20y == 0, NA, abs(shock_net_de20y) / shock_abs_de20y),
    cancel_ratio_de30y = ifelse(shock_abs_de30y == 0, NA, abs(shock_net_de30y) / shock_abs_de30y),
    cancel_ratio_ois20y = ifelse(shock_abs_ois20y == 0, NA, abs(shock_net_ois20y) / shock_abs_ois20y)
  ) %>%
  select(
    c(quarter, starts_with("cancel_ratio"))
  )


## NOTE: shock_cancel_ratio is a DIAGNOSTIC only (it does not feed into the panel/estimation).
## A high mean cancellation (net << abs) means the quarterly NET sum discards within-quarter
## offsetting information. This motivates the "alternative shock aggregation" robustness check
## (structure ss. 7.3): e.g. using only easing-direction shocks (shock_neg_*) as an alternative.

# -----------------------------------------------------------------------------#

# Loading the exogenous variables: (2) Elasticity Dummy
## Using building land reserves from 2014, as to predate the overall time frame (i.e. avoid endogenetiy) ##

bl <- read.csv("./data/C_bl.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

## Select only 2014 data and slice off the source information from the csv ##

 bl <- bl %>%
  select(district_id, district_name, blr_14) %>%
  slice(1:(n()-2)) %>%
  mutate(district_id = as.integer(district_id))


### Check if any districts are missing in prices and/or reserve data ###

# id_prices <- prices %>%
#  distinct(district_id)%>%
#  arrange(district_id)

# id_bl <- bl %>%
#  distinct(district_id)%>%
#  arrange(district_id)


# id_prices %>%
#  anti_join(id_bl, by = "district_id")

# all id's in prices are also in reserves :)

# id_bl %>%
#  anti_join(id_prices, by = "district_id")

# all id's in reserves are also in prices :)


## Inspecting bl-reserves by federal state: ##

blr_diagnostics <- bl

blr_diagnostics <- blr_diagnostics %>%
  mutate(
    bundesland_code = floor(district_id / 100)
  ) %>%
  mutate(
    bundesland = case_when(
      bundesland_code == 1 ~ "Burgenland",
      bundesland_code == 2 ~ "Kärnten",
      bundesland_code == 3 ~ "Niederösterreich",
      bundesland_code == 4 ~ "Oberösterreich",
      bundesland_code == 5 ~ "Salzburg",
      bundesland_code == 6 ~ "Steiermark",
      bundesland_code == 7 ~ "Tirol",
      bundesland_code == 8 ~ "Vorarlberg",
      bundesland_code == 9 ~ "Wien",
      TRUE ~ NA_character_
    )
  )

## Boxplots ##

ggplot(blr_diagnostics, aes(x = bundesland, y = blr_14)) + 
  geom_boxplot() + 
  labs(
    x = "Bundesland", 
    y = "Building Land Reserve by Bundesland"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


blr_woV <- blr_diagnostics %>%
  filter(bundesland !="Wien")


ggplot(blr_woV, aes(x = bundesland, y = blr_14)) + 
  geom_boxplot() + 
  labs(
    x = "Bundesland", 
    y = "Building Land Reserve by Bundesland (w/o Wien)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Histograms ##

ggplot(
  blr_woV, aes(x = blr_14)) +
  geom_histogram(bins = 20) +
  labs(
    x = "Building Land Reserve by Bundesland (w/o Wien)",
    y = "Count") 


### -> From the distributon of bl reserves, the inital best approach is to classify
### the districts as elastic/inelastic from the bottom-quantile excl. Vienna.
# Check again: Does 25% make sense?? 24 districts outside Vienna would be classified as inelastic, 69 as elastic!

q25_th <- blr_woV %>%
  summarise(q25 = quantile(blr_14, 0.25, na.rm = TRUE)) %>%
  pull (q25)

bl <- bl %>%
  mutate(
    inelastic_dummy = ifelse(blr_14 <= q25_th, 1, 0)
  )

## Inspecting the classification ##

bl %>%
  count(inelastic_dummy)

inelastic_districts <- bl  %>%
  filter(inelastic_dummy == 1) %>%
  arrange(blr_14) %>%
  select(district_id, district_name, blr_14)


# -----------------------------------------------------------------------------#

# Loading Control Variables: (1) Population
## Population at boq based on CRR (central residence register)
## available quarterly by district

c_pop <- read.csv("./data/cc_pop.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

c_pop_long <- c_pop %>%
  mutate(
    across(starts_with("X"), as.character)
  ) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date_raw",
    values_to = "population_raw",
    values_transform = list(population_raw = as.character)
  ) %>%
  mutate(
    date_raw = sub("^X", "", date_raw),
    date = dmy(date_raw)
  ) %>%
  filter(date >= as.Date("2017-03-01")) %>%
  mutate(
    population = population_raw %>%
      str_trim() %>%
      na_if("#N/A") %>%
      str_replace_all("[^0-9]", "") %>%
      na_if("") %>%
      as.numeric(),
    quarter = as.yearqtr(date)
  ) %>%
  select(district_id, district_name, quarter, population) %>%
  arrange(district_id, quarter)


## Putting the population for Q1 2025 to Q2-Q4 (low fluctuations anyways, linear interpolation makes little sense) ##


all_quarters <- tibble(
  quarter = seq(
    from = as.yearqtr("2017 Q1", format = "%Y Q%q"),
    to   = as.yearqtr("2025 Q4", format = "%Y Q%q"),
    by   = 0.25
  )
)

c_pop_long <- c_pop_long %>%
  distinct(district_id, district_name, quarter, population) %>%
  right_join(
    c_pop_long %>%
      distinct(district_id, district_name) %>%
      crossing(all_quarters),
    by = c("district_id", "district_name", "quarter")
  ) %>%
  arrange(district_id, quarter) %>%
  group_by(district_id, district_name) %>%
  fill(population, .direction = "down") %>%
  ungroup()


# -----------------------------------------------------------------------------#

# Loading Control Variables: (2) Unemployment
## Unemployment is available at the district level, on an annual basis.
## Annual figures are treated as quarterly (i.e. fortschreiben auf Quartalsbasis).
##
## Coverage: observed district-level data runs 2012-2024. District-level figures for 2025 are
## not yet available (as of Aug 2026), so 2025 Q1-Q4 are imputed by carrying forward each
## district's 2024 value (fill down after the join). This mirrors the population treatment and
## is defensible for a slow-moving annual series within the scope of this thesis.


c_unemp <- read.csv("./data/C_unemp.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

## Drop the trailing source-note / blank rows automatically (content-based, idempotent).
## Real districts always have a numeric district_id; the "Quelle: ..." note and empty rows
## become NA under as.integer() and are removed. Safe to rerun; never slices real data.
c_unemp <- c_unemp %>%
  filter(!is.na(suppressWarnings(as.integer(district_id))))

c_unemp_long <- c_unemp %>%
  mutate(
    district_id = as.integer(district_id),
    across(starts_with("X"), as.numeric)
  ) %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year_raw",
    values_to = "unemp_rate"
  ) %>%
  mutate(
    year = as.integer(sub("^X", "", year_raw))
  ) %>%
  select(district_id, district_name, year, unemp_rate) %>%
  crossing(qtr = 1:4) %>%
  mutate(
    quarter = as.yearqtr(paste(year, qtr), format = "%Y %q")
  ) %>%
  select(district_id, district_name, quarter, unemp_rate) %>%
  arrange(district_id, quarter)


# restricting to relevant sample, carrying forward 2024 values for 2025 (not available as of August 31st 2026)


c_unemp_long <- c_unemp_long %>%
  filter(
    quarter >= as.yearqtr("2017 Q1", format = "%Y Q%q"),
    quarter <= as.yearqtr("2024 Q4", format = "%Y Q%q")
  )


c_unemp_long <- c_unemp_long %>%
  distinct(district_id, district_name, quarter, unemp_rate) %>%
  right_join(
    c_unemp_long %>%
      distinct(district_id, district_name) %>%
      crossing(all_quarters),
    by = c("district_id", "district_name", "quarter")
  ) %>%
  arrange(district_id, quarter) %>%
  group_by(district_id, district_name) %>%
  fill(unemp_rate, .direction = "down") %>%
  ungroup()


# -----------------------------------------------------------------------------#

# Loading Control Variables: (1) Net Income
# Annual Figures, Net Income of District
# For now: I will construct net income per capita, using population from before (i.e. building yearly averages from the quarterly population data)
# and dividing district net income by that average. 
# NOTE: The source file (C_inc.csv) contains a 01.12.2023 column, but it is EMPTY for every
# district. Observed income is therefore available only through 2022. Years 2023, 2024 and 2025
# are deliberately extrapolated from observed 2022 totals using the nominal Tariflohnindex (see below).

c_inc <- read.csv("./data/C_inc.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

date_cols <- grep("^X\\d{2}\\.\\d{2}\\.\\d{4}$", names(c_inc), value = TRUE)

ordered_date_cols <- date_cols[
  order(as.Date(sub("^X", "", date_cols), format = "%d.%m.%Y"))
]

c_inc <- c_inc[c(setdiff(names(c_inc), date_cols), ordered_date_cols)]

## Drop the trailing source-note / blank rows automatically (content-based, idempotent).
## Real districts always have a numeric district_id; the "Quelle: ..." note and empty rows
## become NA under as.integer() and are removed. Safe to rerun; never slices real data.
c_inc <- c_inc %>%
  filter(!is.na(suppressWarnings(as.integer(district_id)))) %>%
  mutate(
    district_id = as.integer(district_id),
    across(starts_with("X"), as.numeric)
  )


c_inc_long <- c_inc %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "date_raw",
    values_to = "income_total"
  ) %>%
  mutate(
    year = as.integer(sub("^X01\\.12\\.", "", date_raw))
  ) %>%
  select(district_id, district_name, year, income_total) %>%
  filter(
    year >= 2017,
    year <= 2022,          # 2023 observed column is empty -> extrapolated below, not kept here
    !is.na(income_total)
  ) %>%
  arrange(district_id, year)


## Adding extrapolated annual (district-level) income totals for 2023-2025:
## 2023 is extrapolated as well (source column empty for all districts).
## Using the nominal Tariflohnindex (OeNB)

wage_index <- tibble(
  year = c(2022, 2023, 2024, 2025),
  tli  = c(115.1, 123.9, 134.4, 139.6)
)


tli_2022 <- wage_index %>%
  filter(year == 2022) %>%
  pull(tli)

inc_2022 <- c_inc_long %>%
  filter(year == 2022) %>%
  select(district_id, district_name, income_total_2022 = income_total)

c_inc_ext_annual <- inc_2022 %>%
  crossing(wage_index %>% filter(year > 2022)) %>%
  mutate(
    income_total = income_total_2022 * (tli / tli_2022),
    income_source = "extrapolated_tli"
  ) %>%
  select(district_id, district_name, year, income_total, income_source)

c_inc_obs_annual <- c_inc_long %>%
  mutate(income_source = "observed")

c_inc_annual_full <- c_inc_obs_annual %>%
  bind_rows(c_inc_ext_annual) %>%
  arrange(district_id, year)

## Expanding annual (district-level) totals to quarterly totals (at the district-level)

c_inc_q_total <- c_inc_annual_full %>%
  crossing(qtr = 1:4) %>%
  mutate(
    quarter = as.yearqtr(paste(year, qtr), format = "%Y %q")
  ) %>%
  select(district_id, district_name, quarter, income_total, income_source) %>%
  arrange(district_id, quarter)

## Merging with (quarterly) population and calculating per capita income:

c_inc_q <- c_inc_q_total %>%
  left_join(
    c_pop_long %>% select(district_id, district_name, quarter, population),
    by = c("district_id", "district_name", "quarter")
  ) %>%
  mutate(
    income_pc = income_total / population,
    log_income_pc = log(income_pc)
  ) %>%
  arrange(district_id, quarter)




