###############################################################################
# LOCAL PROJECTIONS — Jordà (2005)
# Instrument: Struktureller Angebotsschock aus Kilian (2009) SVAR
# Zielvariablen: Ölpreistransmission auf EU/AT Makrogrößen
###############################################################################

library(sandwich); library(lmtest)

###############################################################################
# 1. SCHOCK-SERIE VORBEREITEN
###############################################################################

# Angebotsschock aus Kilian SVAR (Spalte 1, bereits vorzeichennormiert)
# Vorzeichen: positiver Schock = mehr Produktion = Preisrückgang
# → für Interpretation: negativer Schock = Versorgungsausfall = Preisanstieg
# Wir drehen das Vorzeichen: positiv = Versorgungsausfall
supply_shock_df <- tibble(
  time  = times,
  shock = str_shock[, 3]   # negativ → "Angebotsausfall"-Schock
)

###############################################################################
# 2. ZIELVARIABLEN ZUSAMMENBAUEN
###############################################################################

# Österreich/EU Inflation (Core + Headline + Energie)
inf_at <- inf_df_raw |>
  filter(geo == "AT",
         Type %in% c("Headline_Inflation", "Core_Inflation", "Energie_Inflation")) |>
  dplyr::select(time, Type, values)

# EU Industrieproduktion (Gesamt + Energie-ex)
ip_eu <- indpro_df_raw |>
  filter(geo == "EU27_2020",
         Type %in% c("Industrie_Gesamt", "Industrie_ex_Energie")) |>
  dplyr::select(time, Type, values)

# PPI Österreich (Energie-MIG)
ppi_at <- ppi |>
  filter(geo == "AT", unit == "PCH_SM",
         Type %in% c("PPI_Industrie", "MIG_Energie")) |>
  dplyr::select(time, Type, values)

# Finanzmarkt (US, global verfügbar)
fin_df <- bind_rows(
  vix_raw      |> dplyr::select(time, Type, values),
  hy_spread_raw|> dplyr::select(time, Type, values),
  sp500_raw    |> dplyr::select(time, Type, values)
)

# Alles zusammen
lp_targets_long <- bind_rows(inf_at, ip_eu, ppi_at, fin_df) |>
  filter(time >= min(supply_shock_df$time),
         time <= max(supply_shock_df$time))

###############################################################################
# 3. LP FUNKTION
###############################################################################

# Jordà LP: y_{t+h} = α_h + β_h * shock_t + Σ γ_{h,l} * shock_{t-l} + ε_{t,h}
# β_h = IRF bei Horizont h
# HAC-SE via Newey-West (BW = floor(H^(2/3)))

run_lp <- function(y_vec,        # Zielvariable (Zeitreihe, bereits transformiert)
                   shock_vec,    # Schock-Zeitreihe
                   H      = 24,  # max. Horizont
                   n_lags = 4) { # Kontroll-Lags des Schocks

  n <- length(y_vec)
  results <- tibble(h = 0:H, beta = NA_real_, se = NA_real_,
                    lo68 = NA_real_, hi68 = NA_real_,
                    lo90 = NA_real_, hi90 = NA_real_)

  for (h in 0:H) {

    # Abhängige Variable: Reaktion relativ zu t-1
    y_h <- y_vec[(1 + h):n] - y_vec[1:(n - h)]

    # Schock bei t
    x_t <- shock_vec[1:(n - h)]

    # Kontroll-Lags (Schock_{t-1}, ..., Schock_{t-n_lags})
    T_h <- length(y_h)
    lag_mat <- sapply(1:n_lags, function(l) {
      c(rep(NA, l), shock_vec[1:(T_h - l)])
    })

    df_lp <- data.frame(y = y_h, x = x_t, lag_mat) |> na.omit()

    if (nrow(df_lp) < 20) next

    fit   <- lm(y ~ ., data = df_lp)
    bw    <- floor(h^(2/3)) + 1
    vcov_nw <- NeweyWest(fit, lag = bw, prewhite = FALSE)
    ct    <- coeftest(fit, vcov. = vcov_nw)

    b  <- ct["x", "Estimate"]
    se <- ct["x", "Std. Error"]

    results$beta[h + 1]  <- b
    results$se[h + 1]    <- se
    results$lo68[h + 1]  <- b - qnorm(0.84) * se
    results$hi68[h + 1]  <- b + qnorm(0.84) * se
    results$lo90[h + 1]  <- b - qnorm(0.95) * se
    results$hi90[h + 1]  <- b + qnorm(0.95) * se
  }
  results
}

###############################################################################
# 4. LP FÜR ALLE ZIELVARIABLEN
###############################################################################

H_lp <- 12

# Variablen die bereits als YoY-Veränderung vorliegen (HICP, PPI PCH_SM)
# → direkt verwenden (keine weitere Transformation)
# IP: Index → wir nehmen log-Differenz × 100

vars_yoy <- c("Headline_Inflation", "Core_Inflation", "Energie_Inflation",
              "PPI_Industrie", "MIG_Energie")
vars_log  <- c("Industrie_Gesamt", "Industrie_ex_Energie", "SP500")
vars_level <- c("VIX", "HY_Credit_Spread")

lp_results <- lp_targets_long |>
  group_by(Type) |>
  arrange(time) |>
  group_modify(function(df, key) {

    # Auf gemeinsamen Zeitraum mergen
    merged <- df |>
      inner_join(supply_shock_df, by = "time") |>
      arrange(time)

    if (nrow(merged) < 40) return(tibble())

    y_raw <- merged$values

    # Transformation je nach Variablentyp
    y_trans <- if (key$Type %in% vars_log) {
      c(NA, diff(log(y_raw)) * 100)
    } else {
      y_raw   # YoY oder Level bereits passend
    }

    # NAs entfernen
    valid   <- !is.na(y_trans) & !is.na(merged$shock)
    y_clean <- y_trans[valid]
    s_clean <- merged$shock[valid]

    run_lp(y_clean, s_clean, H = H_lp, n_lags = 4)
  }) |>
  ungroup()

###############################################################################
# 5. VISUALISIERUNG
###############################################################################

# Schöne Labels
type_labels <- c(
  "Headline_Inflation"   = "AT Headline Inflation (YoY)",
  "Core_Inflation"       = "AT Core Inflation (YoY)",
  "Energie_Inflation"    = "AT Energie-Inflation (YoY)",
  "PPI_Industrie"        = "AT PPI Industrie (MoM%)",
  "MIG_Energie"          = "AT PPI Energie (MoM%)",
  "Industrie_Gesamt"     = "EU Industrieproduktion (log%Δ)",
  "Industrie_ex_Energie" = "EU IP ex Energie (log%Δ)",
  "SP500"                = "S&P 500 (log%Δ)",
  "VIX"                  = "VIX (Punkte)",
  "HY_Credit_Spread"     = "HY Credit Spread (Punkte)"
)

# Gruppen für separate Plots
gruppe_inflation <- c("Headline_Inflation", "Core_Inflation",
                      "Energie_Inflation", "PPI_Industrie", "MIG_Energie")
gruppe_real      <- c("Industrie_Gesamt", "Industrie_ex_Energie")
gruppe_fin       <- c("SP500", "VIX", "HY_Credit_Spread")

lp_plot <- function(df, types, title_str) {
  df |>
    filter(Type %in% types, !is.na(beta)) |>
    mutate(label = factor(Type, levels = types,
                          labels = type_labels[types])) |>
    ggplot(aes(x = h)) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90),
                fill = "#4472C4", alpha = 0.15, color = NA) +
    geom_ribbon(aes(ymin = lo68, ymax = hi68),
                fill = "#4472C4", alpha = 0.30, color = NA) +
    geom_line(aes(y = beta), color = "#C0392B", linewidth = 0.9) +
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey40", linewidth = 0.4) +
    facet_wrap(~label, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(0, H_lp, 6)) +
    labs(
      title    = title_str,
      subtitle = "Schock: Negativer Öl-Angebotsschock (Kilian 2009) | Newey-West HAC SE | 68% + 90% Band",
      x = "Monate nach Schock", y = "Kumul. Reaktion (Einheit je Variable)"
    ) +
    theme_bw(base_size = 10) +
    theme(strip.text       = element_text(face = "bold", size = 8.5),
          panel.grid.minor = element_blank())
}

p_inf <- lp_plot(lp_results, gruppe_inflation,
                 "LP-IRF: Preistransmission — Angebotsausfall")
p_real <- lp_plot(lp_results, gruppe_real,
                  "LP-IRF: Reale Aktivität — Angebotsausfall")
p_fin  <- lp_plot(lp_results, gruppe_fin,
                  "LP-IRF: Finanzmärkte — Angebotsausfall")

print(p_inf)
print(p_real)
print(p_fin)
```

---

  ### Was das Modell macht

  Die LP-Regression für jeden Horizont h ist:
  ```
y_{t+h} − y_{t−1} = α_h + β_h · ε_t^supply + Σ γ_{h,l} · ε_{t−l}^supply + u_{t,h}
