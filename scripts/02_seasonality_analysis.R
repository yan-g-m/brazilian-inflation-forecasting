# ==============================================================================
# Objective: Explore seasonality, STL decomposition, and fetch official IBGE weights via SIDRA API
# Author: Yan Galante Maestre
# Date: October 2026
# Dependencies: pacman, here, readr, dplyr, tidyr, tsibble, feasts, ggplot2, lubridate, sidrar
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, readr, dplyr, tidyr, tsibble, feasts, ggplot2, lubridate, sidrar)

ipca_data <- readr::read_rds(here::here("data", "ipca_disaggregated_clean.rds"))

# Cast raw data frame into monthly regular time series container (tsibble)
ipca_ts <- ipca_data |>
  dplyr::mutate(year_month = tsibble::yearmonth(date)) |>
  tsibble::as_tsibble(index = year_month)

# 1. 3x3 Seasonal distribution boxplots across 9 sub-groups
fig_boxplots <- ipca_ts |>
  dplyr::select(-date, -headline) |>
  tidyr::pivot_longer(
    cols      = -year_month,
    names_to  = "subgroup",
    values_to = "inflation"
  ) |>
  dplyr::mutate(month = lubridate::month(year_month, label = TRUE, abbr = TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x = month, y = inflation, fill = subgroup)) +
  ggplot2::geom_boxplot(outlier.size = 0.7, show.legend = FALSE) +
  ggplot2::facet_wrap(~subgroup, scales = "free_y", ncol = 3) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::labs(
    title    = "Seasonal Inflation Distribution by IPCA Sub-group",
    subtitle = "Monthly distribution highlighting education (Feb) and apparel (mid-year) seasonality",
    x        = "Month",
    y        = "% MoM"
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "fig_seasonality_boxplots.png"),
  plot     = fig_boxplots,
  width    = 12,
  height   = 8,
  dpi      = 300
)

# 2. Additive STL decomposition for headline inflation
fig_stl <- ipca_ts |>
  fabletools::model(stl = feasts::STL(headline ~ season(window = "periodic"))) |>
  fabletools::components() |>
  ggplot2::autoplot() +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::labs(
    title    = "STL Decomposition - Headline IPCA",
    subtitle = "Trend-cycle extraction, periodic seasonal filtering, and remainder shocks"
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "fig_headline_stl_decomposition.png"),
  plot     = fig_stl,
  width    = 10,
  height   = 6,
  dpi      = 300
)

# 3. Direct automated pull of official IBGE weights via SIDRA API (Table 7060, Variable 66)
# Usando period = "last" sem espacos para evitar erro de URL malformada
sidra_weights_raw <- sidrar::get_sidra(
  x          = 7060,
  variable   = 66,   # Variavel 66 = Peso mensal (%)
  period     = "last",
  classific  = "c315",
  category   = list(c(7170, 7445, 7486, 7558, 7625, 7660, 7712, 7766, 7786))
)

mapa_grupos <- c(
  "1.Alimentação e bebidas"     = "food_beverages",
  "2.Habitação"                 = "housing",
  "3.Artigos de residência"     = "household_goods",
  "4.Vestuário"                 = "apparel",
  "5.Transportes"               = "transportation",
  "6.Saúde e cuidados pessoais" = "health_care",
  "7.Despesas pessoais"         = "personal_exp",
  "8.Educação"                  = "education",
  "9.Comunicação"               = "communication"
)

ibge_weights <- sidra_weights_raw |>
  dplyr::select(
    grupo_ibge     = `Geral, grupo, subgrupo, item e subitem`,
    weight_percent = Valor
  ) |>
  dplyr::mutate(
    subgroup       = unname(mapa_grupos[grupo_ibge]),
    weight_percent = as.numeric(weight_percent),
    weight_decimal = weight_percent / 100
  ) |>
  dplyr::select(subgroup, weight_percent, weight_decimal)

readr::write_rds(ibge_weights, here::here("data", "ibge_weights.rds"))
readr::write_csv(ibge_weights, here::here("data", "ibge_weights.csv"))

cat("Script 02 executado com sucesso: pesos oficiais baixados via SIDRA e graficos salvos!\n")