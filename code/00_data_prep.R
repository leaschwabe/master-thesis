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

prices <- read.csv("RE_prices.csv", header = TRUE ,sep=";")

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

# Loading the exogenous variables: (1) shock (EAMPD)


shock <- read.csv("EAMPD_event.csv", header = TRUE, sep =";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")
shock$X.date <- as.Date(gsub('"', "", shock$X.date), format= "%d.%m.%Y")
shock <- subset(
  shock, 
  X.date >= as.Date("2010-01-01"),
  select = c("X.date", "OIS_20Y", "DE20Y", "DE30Y")
)

### Construction of aggregate quarterly MP shock:
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


## definetly check again!!

# -----------------------------------------------------------------------------#

# Loading the exogenous variables: (2) Elasticity Dummy
## Using building land reserves from 2014, as to predate the overall time frame (i.e. avoid endogenetiy) ##

bl <- read.csv("C_bl.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

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

c_pop <- read.csv("cc_pop.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

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
## For now, I will treat the annual figures as quarterly (i.e. fortschreiben auf quartalsbasis)


c_unemp <- read.csv("C_unemp.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

# c_unemp <- c_unemp %>%
 # slice(1:116)

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


# restricting to relevant sample, starting in Q1 2017, adding quarters from 2024 onwards (to be filled later lol)


c_unemp_long <- c_unemp_long %>%
  filter(
    quarter >= as.yearqtr("2017 Q1", format = "%Y Q%q"),
    quarter <= as.yearqtr("2023 Q4", format = "%Y Q%q")
  )


c_unemp_long <- c_unemp_long %>%
  distinct(district_id, district_name, quarter, unemp_rate) %>%
  right_join(
    c_unemp_long %>%
      distinct(district_id, district_name) %>%
      crossing(all_quarters),
    by = c("district_id", "district_name", "quarter")
  ) %>%
  arrange(district_id, quarter)


# -----------------------------------------------------------------------------#

# Loading Control Variables: (1) Net Income
# Annual Figures, Net Income of District
# For now: I will construct net income per capita, using population from before (i.e. building yearly averages from the quarterly population data)
# and dividing district net income by that average. 

c_inc <- read.csv("C_inc.csv", header = TRUE, sep=";", dec=".", quote = "", fileEncoding = "UTF-8-BOM")

date_cols <- grep("^X\\d{2}\\.\\d{2}\\.\\d{4}$", names(c_inc), value = TRUE)

ordered_date_cols <- date_cols[
  order(as.Date(sub("^X", "", date_cols), format = "%d.%m.%Y"))
]

c_inc <- c_inc[c(setdiff(names(c_inc), date_cols), ordered_date_cols)]

# c_inc <- c_inc %>%
 # slice(1:116)

c_inc <- c_inc %>%
  filter(!is.na(province), district_id != "") %>%
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
    year <= 2022,
    !is.na(income_total)
  ) %>%
  arrange(district_id, year)


## Adding extrapolated annual (district-level) income totals for 2023-2025:
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




