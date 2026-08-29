# Constructing annual population averages from c_pop_long:

pop_annual <- c_pop_long %>%
  mutate(
    year = as.integer(floor(as.numeric(quarter)))
  ) %>%
  group_by(district_id, district_name, year) %>%
  summarise(
    population_avg = mean(population, na.rm = TRUE),
    .groups = "drop"
  )


## Reshaping c_inc to long, filtering for date 

c_inc_long <- c_inc %>%
  mutate(
    district_id = as.integer(district_id),
    across(starts_with("X"), as.numeric)
  ) %>%
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
  )

c_inc_pc <- c_inc_long %>%
  left_join(pop_annual, by = c("district_id", "district_name", "year")) %>%
  mutate(
    income_pc = income_total / population_avg,
    log_income_pc = log(income_pc)
  )

# For now, I will repeat the annual figures at the quarter level and leave 2023-2025 blank
# I want to use wage growth (or something similar to extrapolate the remaining quarters)

c_inc_q <- c_inc_pc %>%
  select(district_id, district_name, year, income_pc, log_income_pc) %>%
  crossing(qtr = 1:4) %>%
  mutate(
    quarter = as.yearqtr(paste(year, qtr), format = "%Y %q")
  ) %>%
  select(district_id, district_name, quarter, income_pc, log_income_pc) %>%
  arrange(district_id, quarter)


# Using the Tariflohnindex (OeNB) to extrapolate the missing years (2023-2025)


## annual wage index:

wage_index <- tibble(
  year = c(2022, 2023, 2024, 2025),
  tli  = c(115.1, 123.9, 134.4, 139.6)
)


## base year = 2022 (i.e. when I start extrapolating)

tli_2022 <- wage_index %>%
  filter(year == 2022) %>%
  pull(tli)


inc_2022 <- c_inc_q %>%
  filter(quarter == as.yearqtr("2022 Q4", format = "%Y Q%q")) %>%
  select(district_id, district_name, income_pc)