###############################################################################
# 05_background_charts.R - Chapter 2 (Institutional and Economic Background)
#
# Builds the two descriptive figures for Chapter 2 from published OeNB series
# (external aggregates, NOT the district panel):
#
#   Figure (2.2): Austrian residential property price index, Vienna vs.
#                 Austria excluding Vienna, quarterly, rebased to 100 at the
#                 first shared quarter, with shaded monetary-policy phases.
#   Figure (2.3): Housing credit to households, two panels sharing a quarterly
#                 x-axis: (top) year-on-year growth of the outstanding stock of
#                 household housing loans; (bottom) the average interest rate on
#                 new household housing loans.
#
# Inputs (OeNB ISAWEBSTAT exports, semicolon-delimited, latin1, decimal comma):
#   data/oenb_propertypriceindex.csv   -> Figure 2.2   (cite: oenb_propertyprices)
#   data/oenb_mortgagevol.csv          -> Figure 2.3 top (cite: oenb_mortgagevol)
#   data/oenb_mortgagerates.csv        -> Figure 2.3 bottom (cite: oenb_mortgagerates)
#
# Outputs:
#   thesis/charts/10_oenb_price_index.pdf
#   thesis/charts/11_oenb_housing_credit.pdf
#   thesis/charts/12_oenb_rate_fixation.pdf   (cite: oenb_mortgageweights)
#
# Notes on the source format:
#   - Column 1 is the period type: "Jahr" (annual), "Quartal" (quarterly),
#     "Monat" (monthly). We use quarterly for prices and monthly for credit
#     (aggregated to quarterly below).
#   - Column 2 is the year. For "Quartal" rows column 4 is the running quarter
#     (1-4); for "Monat" rows column 5 is the month (01-12).
#   - The last column is a stable series code; we filter on it rather than on
#     the (umlaut-mangled) German description so the selection is robust.
#   - Values sit in the second-to-last usable numeric column; missing = "x".
#   - Stocks (Bestand) are levels: we take the END-OF-QUARTER value. Rates are
#     averaged (quarterly mean). YoY growth is the 4-quarter change of the stock.
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(zoo)
})
source("code/plot_theme.R")

THESIS_FIG_DIR <- "./thesis/charts"
dir.create(THESIS_FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# --- Helpers ----------------------------------------------------------------

## Read an OeNB ISAWEBSTAT export. The files have no header row; every line is
## one (period, series) record. We read all columns as character and name the
## ones we need. Encoding is latin1 (umlauts), separator ";", decimals ",".
read_oenb <- function(path) {
  raw <- utils::read.table(
    path, sep = ";", header = FALSE, quote = "", comment.char = "",
    stringsAsFactors = FALSE, fill = TRUE, fileEncoding = "latin1",
    colClasses = "character"
  )
  raw
}

## German decimal string -> numeric ("102,2" -> 102.2, "x" -> NA).
num_de <- function(x) {
  x <- trimws(x)
  x[x == "x" | x == ""] <- NA_character_
  as.numeric(gsub(",", ".", x, fixed = TRUE))
}

## The series code is the last non-empty field of each row; the value is the
## last numeric field before it. read.table pads short rows with "", so we scan
## from the right for each row.
last_nonempty <- function(df_row) {
  v <- df_row[df_row != "" & !is.na(df_row)]
  if (length(v) == 0) NA_character_ else v[length(v)]
}

# -----------------------------------------------------------------------------#
# FIGURE 2.2 - Residential property price index (Vienna vs. Austria ex Vienna)
# -----------------------------------------------------------------------------#
## Target series codes (index level, 2000=100):
##   ...F001  = Immobilienpreisindex Wien, 2000=100
##   ...F000  = Immobilienpreisindex Oesterreich (ohne Wien), 2000=100
## We match on the EXACT trailing code so the many sub-series (EFH/ETW/new/used,
## and the year-on-year "VerVJ%" variants) are excluded.
price_raw <- read_oenb("data/oenb_propertypriceindex.csv")

# Series code is in the final column; value in column 13 (the numeric column).
price_code  <- apply(price_raw, 1, last_nonempty)
price_type  <- trimws(price_raw[[1]])
price_year  <- suppressWarnings(as.integer(price_raw[[2]]))
price_qtr   <- suppressWarnings(as.integer(price_raw[[4]]))   # running 1-4 on Quartal rows
price_value <- num_de(price_raw[[13]])

price_df <- tibble::tibble(
  type = price_type, year = price_year, qtr = price_qtr,
  value = price_value, code = price_code
) %>%
  filter(type == "Quartal",
         code %in% c("AB003C000D000E000F001",     # Vienna
                     "AB003C000D000E000F000")) %>% # Austria ex Vienna
  mutate(
    region = if_else(code == "AB003C000D000E000F001",
                     "Vienna", "Austria excl. Vienna"),
    quarter = zoo::as.yearqtr(paste0(year, " Q", qtr))
  ) %>%
  filter(!is.na(quarter), !is.na(value)) %>%
  arrange(region, quarter)

# Rebase each region to 100 at the first shared quarter.
base_q <- max(
  min(price_df$quarter[price_df$region == "Vienna"]),
  min(price_df$quarter[price_df$region == "Austria excl. Vienna"])
)
price_df <- price_df %>%
  group_by(region) %>%
  mutate(index = 100 * value / value[quarter == base_q]) %>%
  ungroup()

# Monetary-policy phase bands (from Section 2.1): APP from 2015, PEPP 2020-2022,
# tightening from mid-2022. Drawn faintly so they frame rather than dominate.
# geom_rect with a yearqtr axis and +/-Inf y-limits silently drops the rects, so
# we use numeric x-coordinates (as.numeric on yearqtr) and finite y-limits taken
# from the data range.
x_min <- min(price_df$quarter)
x_max <- max(price_df$quarter)
y_lo  <- floor(min(price_df$index) / 25) * 25
y_hi  <- ceiling(max(price_df$index) / 25) * 25

# Build the phase coordinates as plain numerics. Do NOT mix a yearqtr object
# (x_max) with character strings inside c(): that coerces the whole vector and
# yields NA on reparse, which makes geom_rect draw nothing.
phase_start <- as.numeric(zoo::as.yearqtr(c("2015 Q1", "2020 Q1", "2022 Q3")))
phase_end   <- c(as.numeric(zoo::as.yearqtr(c("2020 Q1", "2022 Q3"))),
                 as.numeric(x_max))
phases <- tibble::tibble(
  phase = factor(c("APP", "PEPP", "Tightening"),
                 levels = c("APP", "PEPP", "Tightening")),
  start = phase_start,
  end   = phase_end
)
phase_fill <- c(APP = "#6497b1", PEPP = "#FFB04F", Tightening = "#cd1076")

p_price <- ggplot(price_df, aes(as.numeric(quarter), index, colour = region)) +
  geom_rect(data = phases, inherit.aes = FALSE,
            aes(xmin = start, xmax = end, ymin = y_lo, ymax = y_hi, fill = phase),
            alpha = 0.20) +
  geom_line(linewidth = thesis_linewidth) +
  scale_fill_manual(values = phase_fill, name = NULL,
                    guide = guide_legend(order = 2)) +
  scale_colour_manual(values = unname(thesis_primary), name = NULL,
                      guide = guide_legend(order = 1)) +
  scale_x_continuous(
    breaks = seq(2000, 2026, by = 2),
    labels = function(z) paste0("Q1 ", z)
  ) +
  # coord_cartesian zooms without dropping the rectangles (scale-level limits
  # would clip geom_rect rows whose edges sit at the limit, hiding the bands).
  coord_cartesian(xlim = c(as.numeric(x_min), as.numeric(x_max)),
                  ylim = c(y_lo, y_hi), expand = FALSE) +
  labs(x = NULL, y = paste0("Index (", format(base_q), " = 100)")) +
  theme_thesis()

ggsave_thesis(file.path(THESIS_FIG_DIR, "10_oenb_price_index.pdf"),
              p_price, height = 8.5)

# -----------------------------------------------------------------------------#
# FIGURE 2.3 - Housing credit to households (growth of stock | new-lending rate)
# -----------------------------------------------------------------------------#

## --- Panel A: outstanding household housing loans (Bestand), YoY growth -------
## Series: "Kredite fuer Wohnbauzwecke ... Private Haushalte ... Inland ...
## insgesamt ... Bestand", in EUR million. We identify it by the descriptive
## fields rather than the code AB003 alone (AB003 also tags maturity splits),
## requiring: purpose = Wohnbauzwecke, sector = Private Haushalte, region =
## Inland, maturity = insgesamt, stock = Bestand.
vol_raw <- read_oenb("data/oenb_mortgagevol.csv")

## Monthly rows in this file have 17 columns (two extra date columns), so the
## descriptive fields sit two positions further right than in the price/rate
## files: purpose V7, region V11 (Inland vs. Sonstiger Euroraum), sector V13,
## maturity V14, measure V15, value V16, code V17. Month is V5.
vol_df <- tibble::tibble(
  type  = trimws(vol_raw[[1]]),
  year  = suppressWarnings(as.integer(vol_raw[[2]])),
  month = suppressWarnings(as.integer(vol_raw[[5]])),
  purpose  = trimws(vol_raw[[7]]),
  region   = trimws(vol_raw[[11]]),
  sector   = trimws(vol_raw[[13]]),
  maturity = trimws(vol_raw[[14]]),
  measure  = trimws(vol_raw[[15]]),
  value    = num_de(vol_raw[[16]])
) %>%
  filter(type == "Monat",
         grepl("Wohnbauzwecke", purpose),
         grepl("Private Haushalte", sector),
         region == "Inland",
         maturity == "insgesamt",
         grepl("Bestand", measure),
         !is.na(value))

# Collapse months to END-OF-QUARTER stock (the last observed month in each qtr).
vol_q <- vol_df %>%
  mutate(quarter = zoo::as.yearqtr(paste0(year, "-", sprintf("%02d", month)),
                                   format = "%Y-%m")) %>%
  group_by(quarter) %>%
  slice_max(month, n = 1, with_ties = FALSE) %>%   # last month in the quarter
  ungroup() %>%
  arrange(quarter) %>%
  mutate(yoy = 100 * (value / dplyr::lag(value, 4) - 1)) %>%  # 4-quarter change
  filter(!is.na(yoy)) %>%
  transmute(quarter, series = "Housing loan growth (% y-o-y)", val = yoy)

## --- Panel B: interest rate on new household housing loans --------------------
## Series AB006: "Zinssatz fuer Wohnbaukredite ... an private Haushalte ...
## Neugeschaeft", in percent. Monthly -> quarterly mean.
rate_raw <- read_oenb("data/oenb_mortgagerates.csv")
rate_code <- apply(rate_raw, 1, last_nonempty)

rate_df <- tibble::tibble(
  type  = trimws(rate_raw[[1]]),
  year  = suppressWarnings(as.integer(rate_raw[[2]])),
  month = suppressWarnings(as.integer(rate_raw[[5]])),
  value = num_de(rate_raw[[13]]),
  code  = rate_code
) %>%
  filter(type == "Monat", code == "AB006", !is.na(value))

rate_q <- rate_df %>%
  mutate(quarter = zoo::as.yearqtr(paste0(year, "-", sprintf("%02d", month)),
                                   format = "%Y-%m")) %>%
  group_by(quarter) %>%
  summarise(val = mean(value, na.rm = TRUE), .groups = "drop") %>%  # quarterly mean
  mutate(series = "New housing-loan rate (%)") %>%
  select(quarter, series, val)

## Single panel: loan growth as bars (left axis), new-lending rate as a line
## (right axis). The chart starts in 2008: the pre-2008 stock series carries
## large base-effect swings (up to ~25% y-o-y) that pre-date the harmonised
## reporting era and distract from the post-GFC cycle that Section 2.3 discusses.
CREDIT_START <- zoo::as.yearqtr("2008 Q1")

growth_q <- vol_q %>% filter(quarter >= CREDIT_START) %>% select(quarter, growth = val)
rate_qw  <- rate_q %>% filter(quarter >= CREDIT_START) %>% select(quarter, rate = val)
credit_w <- full_join(growth_q, rate_qw, by = "quarter") %>% arrange(quarter)

# Right axis (rate) is mapped onto the left axis (growth) by a linear transform.
# Growth spans roughly -3..8; the rate spans roughly 1..6. Scale the rate up so
# both fill the panel, then invert on the secondary axis.
g_rng <- range(c(credit_w$growth, 0), na.rm = TRUE)
r_rng <- range(credit_w$rate, na.rm = TRUE)
sc    <- diff(g_rng) / diff(r_rng)            # rate -> growth-axis units
shift <- g_rng[1] - r_rng[1] * sc
to_left  <- function(r) r * sc + shift        # rate value -> left-axis position
cx_min <- as.numeric(CREDIT_START)
cx_max <- as.numeric(max(credit_w$quarter))

p_credit <- ggplot(credit_w, aes(as.numeric(quarter))) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_col(aes(y = growth, fill = "Housing loan growth (% y-o-y)"),
           width = 0.20, alpha = 0.9) +
  geom_line(aes(y = to_left(rate), colour = "New housing-loan rate (%, r.h.s.)"),
            linewidth = thesis_linewidth) +
  scale_fill_manual(values = c("Housing loan growth (% y-o-y)" = thesis_primary[["elastic"]]),
                    name = NULL) +
  scale_colour_manual(values = c("New housing-loan rate (%, r.h.s.)" = thesis_primary[["inelastic"]]),
                      name = NULL) +
  scale_x_continuous(
    limits = c(cx_min - 0.5, cx_max + 0.5), expand = c(0, 0),
    breaks = seq(2008, 2026, by = 2),
    labels = function(z) paste0("Q1 ", z)
  ) +
  scale_y_continuous(
    name = NULL,
    sec.axis = sec_axis(~ (. - shift) / sc, name = NULL)
  ) +
  labs(x = NULL) +
  theme_thesis()

ggsave_thesis(file.path(THESIS_FIG_DIR, "11_oenb_housing_credit.pdf"),
              p_credit, height = 8.5)

# -----------------------------------------------------------------------------#
# FIGURE 2.4 - New household housing loans by initial rate fixation (shares)
# -----------------------------------------------------------------------------#
## Source: OeNB "Gewichte zu Kreditzinssaetzen - Neugeschaeft" (cite:
## oenb_mortgageweights), new-business volumes in EUR million, monthly from 2003.
## The table reports the total (AB000), variable / fixed up to one year (AB012)
## and fixed 1-5 years (AB013) for housing loans to households. Fixation over
## five years is the residual: total - up to 1 year - 1 to 5 years (never
## negative in the data). Shares are volume-weighted per calendar year, i.e. the
## sum of a category's monthly volumes over the year's total. Only complete
## calendar years are shown, so the partial current year is dropped.
## These are NEW-BUSINESS shares, not the fixation structure of the outstanding
## stock; they show how exposure to rate changes was built up over time.
wgt_raw <- read_oenb("data/oenb_mortgageweights.csv")

wgt_df <- tibble::tibble(
  type  = trimws(wgt_raw[[1]]),
  year  = suppressWarnings(as.integer(wgt_raw[[2]])),
  month = suppressWarnings(as.integer(wgt_raw[[5]])),
  value = num_de(wgt_raw[[13]]),
  code  = trimws(wgt_raw[[14]])
) %>%
  filter(type == "Monat", code %in% c("AB000", "AB012", "AB013"), !is.na(value)) %>%
  tidyr::pivot_wider(names_from = code, values_from = value) %>%
  mutate(over5 = AB000 - AB012 - AB013)

full_years <- wgt_df %>% count(year) %>% filter(n == 12) %>% pull(year)

fix_labels <- c(upto1 = "Variable or fixed up to 1 year",
                y1to5 = "Fixed 1 to 5 years",
                over5 = "Fixed over 5 years")

fix_df <- wgt_df %>%
  filter(year %in% full_years) %>%
  group_by(year) %>%
  summarise(upto1 = sum(AB012), y1to5 = sum(AB013), over5 = sum(over5),
            total = sum(AB000), .groups = "drop") %>%
  tidyr::pivot_longer(c(upto1, y1to5, over5), names_to = "fixation", values_to = "vol") %>%
  mutate(share    = 100 * vol / total,
         fixation = factor(fix_labels[fixation], levels = rev(unname(fix_labels))))

p_fix <- ggplot(fix_df, aes(year, share, fill = fixation)) +
  geom_col(width = 0.8) +
  # Thesis palette in its defined order (plot_theme.R): green, magenta, blue.
  scale_fill_manual(values = setNames(thesis_palette[1:3], unname(fix_labels)),
                    breaks = unname(fix_labels), name = NULL) +
  scale_x_continuous(breaks = seq(min(full_years), max(full_years), by = 2),
                     expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 25),
                     expand = c(0, 0)) +
  labs(x = NULL, y = "Share of new housing loans (%)") +
  theme_thesis()

ggsave_thesis(file.path(THESIS_FIG_DIR, "12_oenb_rate_fixation.pdf"),
              p_fix, height = 8.5)

# -----------------------------------------------------------------------------#
# FIGURE (candidate, Ch. 4.3) - District price change vs building land reserve
# -----------------------------------------------------------------------------#
# DISABLED: this descriptive price-change-vs-reserve scatter was explored as a
# possible Chapter 4.3 figure but not included. Over the sample the raw price
# change rises with the reserve (a convergence pattern driven by Vienna and the
# cities), which would set up a tension with the hypotheses that the local
# projection, not raw growth, is designed to resolve. The reserve map and the
# Section 4.4 correlation cover the price-reserve relationship instead. Kept
# commented out in case it is wanted later for the discussion. To re-enable,
# uncomment and re-run; it writes 42_... and 43_... to thesis/charts.
#
# load("./data/01_panel.RData")   # panel
#
# price_change_scatter <- function(panel, q_start, q_end, file, groups_only = FALSE) {
#   wide <- panel %>%
#     filter(quarter %in% c(q_start, q_end)) %>%
#     select(district_id, district_name, quarter, p_composite, blr_14) %>%
#     mutate(end = if_else(quarter == q_end, "p_end", "p_start")) %>%
#     select(-quarter) %>%
#     tidyr::pivot_wider(names_from = end, values_from = p_composite) %>%
#     mutate(
#       change = 100 * (log(p_end) - log(p_start)),   # cumulative log change, in %
#       bundesland_code = floor(district_id / 100),
#       statutory = grepl("Stadt", district_name) & !grepl("Land|Umgebung", district_name),
#       group = dplyr::case_when(
#         bundesland_code == 9 ~ "Vienna",
#         statutory            ~ "Statutory city",
#         TRUE                 ~ "Rest of Austria"
#       ),
#       group = factor(group, levels = c("Rest of Austria", "Statutory city", "Vienna"))
#     )
#
#   grp_cols <- c("Rest of Austria" = "#679c35",   # green  (elastic-leaning default)
#                 "Statutory city"  = "#FFB04F",   # orange
#                 "Vienna"          = "#cd1076")   # magenta
#
#   ggplot(wide, aes(blr_14, change, colour = group)) +
#     geom_point(size = 1.6, alpha = 0.9) +
#     scale_colour_manual(values = grp_cols, breaks = names(grp_cols), name = NULL) +
#     labs(x = "Building land reserve, 2014 (\u0025)",
#          y = "Cumulative change in composite price (\u0025)") +
#     theme_thesis()
# }
#
# q_start <- zoo::as.yearqtr("2017 Q1")
# q_last  <- zoo::as.yearqtr("2025 Q4")
#
# ## Boom cutoff: the quarter at which the cross-district mean composite price is
# ## highest (computed, not assumed), printed for a sanity check against Ch. 2.2.
# peak_q <- panel %>%
#   group_by(quarter) %>%
#   summarise(m = mean(p_composite), .groups = "drop") %>%
#   slice_max(m, n = 1) %>%
#   pull(quarter)
# message("Composite-price peak quarter (boom cutoff): ", format(peak_q))
#
# p_scatter_full <- price_change_scatter(panel, q_start, q_last,
#                                        "42_price_reserve_scatter_full.pdf")
# ggsave_thesis(file.path(THESIS_FIG_DIR, "42_price_reserve_scatter_full.pdf"),
#               p_scatter_full, height = 8)
#
# p_scatter_boom <- price_change_scatter(panel, q_start, peak_q,
#                                        "43_price_reserve_scatter_boom.pdf")
# ggsave_thesis(file.path(THESIS_FIG_DIR, "43_price_reserve_scatter_boom.pdf"),
#               p_scatter_boom, height = 8)

message("05_background_charts.R complete: wrote 10_oenb_price_index.pdf, 11_oenb_housing_credit.pdf and 12_oenb_rate_fixation.pdf")
