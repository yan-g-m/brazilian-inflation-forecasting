# ==============================================================================
# Objective: Out-of-sample hold-out validation (2025): Top-Down vs Bottom-Up
# Author: Yan Galante Maestre
# Date: October 2026
# Dependencies: pacman, here, readr, dplyr, tidyr, tsibble, fable, fabletools, ggplot2
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, readr, dplyr, tidyr, tsibble, fable, fabletools, ggplot2)

ipca_data    <- readr::read_rds(here::here("data", "ipca_disaggregated_clean.rds"))
ibge_weights <- readr::read_rds(here::here("data", "ibge_weights.rds"))

ipca_ts <- ipca_data |>
  dplyr::mutate(
    year_month        = tsibble::yearmonth(date),
    dummy_tariffs2015 = dplyr::if_else(date >= as.Date("2015-01-01") & date <= as.Date("2015-03-01"), 1L, 0L),
    dummy_strike2018  = dplyr::if_else(date >= as.Date("2018-05-01") & date <= as.Date("2018-06-01"), 1L, 0L),
    dummy_covid2020   = dplyr::if_else(date >= as.Date("2020-03-01") & date <= as.Date("2020-05-01"), 1L, 0L)
  ) |>
  tsibble::as_tsibble(index = year_month)

train_ts <- ipca_ts |> dplyr::filter(date <= as.Date("2024-12-31"))
test_ts  <- ipca_ts |> dplyr::filter(date >= as.Date("2025-01-01") & date <= as.Date("2025-12-31"))

future_xreg <- test_ts |>
  dplyr::select(year_month, dummy_tariffs2015, dummy_strike2018, dummy_covid2020)

# Top-Down
fit_topdown <- train_ts |>
  fabletools::model(sarimax = fable::ARIMA(headline ~ dummy_tariffs2015 + dummy_strike2018 + dummy_covid2020))

fc_topdown <- fit_topdown |>
  fabletools::forecast(new_data = future_xreg) |>
  tibble::as_tibble() |>
  dplyr::select(year_month, pred_topdown = .mean)

# Bottom-Up
train_long <- train_ts |>
  tidyr::pivot_longer(
    cols      = dplyr::all_of(ibge_weights$subgroup),
    names_to  = "subgroup",
    values_to = "inflation"
  ) |>
  tsibble::as_tsibble(key = subgroup, index = year_month)

future_long <- tidyr::expand_grid(
  subgroup   = ibge_weights$subgroup,
  year_month = future_xreg$year_month
) |>
  dplyr::left_join(future_xreg, by = "year_month") |>
  tsibble::as_tsibble(key = subgroup, index = year_month)

fit_bottomup <- train_long |>
  fabletools::model(sarimax = fable::ARIMA(inflation ~ dummy_tariffs2015 + dummy_strike2018 + dummy_covid2020))

fc_subgroups <- fit_bottomup |>
  fabletools::forecast(new_data = future_long) |>
  tibble::as_tibble() |>
  dplyr::select(subgroup, year_month, pred_subgroup = .mean) |>
  dplyr::left_join(ibge_weights, by = "subgroup") |>
  dplyr::mutate(weighted_contrib = pred_subgroup * weight_decimal)

fc_bottomup <- fc_subgroups |>
  dplyr::group_by(year_month) |>
  dplyr::summarise(pred_bottomup = sum(weighted_contrib), .groups = "drop")

# Consolidacao das metricas
validation_df <- test_ts |>
  tibble::as_tibble() |>
  dplyr::select(year_month, actual_headline = headline) |>
  dplyr::left_join(fc_topdown, by = "year_month") |>
  dplyr::left_join(fc_bottomup, by = "year_month")

readr::write_csv(validation_df, here::here("outputs", "tables", "monthly_validation_2025.csv"))

calc_rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
calc_mae  <- function(actual, pred) mean(abs(actual - pred))

accuracy_metrics <- tibble::tibble(
  metric    = c("RMSE", "MAE"),
  top_down  = c(
    calc_rmse(validation_df$actual_headline, validation_df$pred_topdown),
    calc_mae(validation_df$actual_headline, validation_df$pred_topdown)
  ),
  bottom_up = c(
    calc_rmse(validation_df$actual_headline, validation_df$pred_bottomup),
    calc_mae(validation_df$actual_headline, validation_df$pred_bottomup)
  )
)

readr::write_csv(accuracy_metrics, here::here("outputs", "tables", "accuracy_metrics.csv"))

# Grafico de comparacao de 2025
fig_comparison <- validation_df |>
  dplyr::select(
    year_month,
    `Actual IPCA` = actual_headline,
    `Top-Down`    = pred_topdown,
    `Bottom-Up`   = pred_bottomup
  ) |>
  tidyr::pivot_longer(-year_month, names_to = "Framework", values_to = "Inflation") |>
  ggplot2::ggplot(ggplot2::aes(x = as.Date(year_month), y = Inflation, color = Framework, linetype = Framework)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_date(date_labels = "%b/%y", date_breaks = "1 month") +
  ggplot2::scale_color_manual(values = c("Actual IPCA" = "black", "Top-Down" = "#D95F02", "Bottom-Up" = "#1B9E77")) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::labs(
    title    = "Out-of-Sample Hold-Out Validation (2025)",
    subtitle = "Actual IPCA vs Top-Down and Bottom-Up forecasts",
    x        = "Month",
    y        = "% MoM"
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "fig_forecast_topdown_vs_bottomup.png"),
  plot     = fig_comparison,
  width    = 10,
  height   = 5.5,
  dpi      = 300
)

# Grafico de contribuicao setorial de 2025
fig_contrib <- fc_subgroups |>
  ggplot2::ggplot(ggplot2::aes(x = as.Date(year_month), y = weighted_contrib, fill = subgroup)) +
  ggplot2::geom_col(position = "stack", color = "white", linewidth = 0.2) +
  ggplot2::scale_x_date(date_labels = "%b/%y", date_breaks = "1 month") +
  ggplot2::scale_fill_brewer(palette = "Set3") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::labs(
    title    = "Bottom-Up Monthly Forecast Decomposition (2025)",
    subtitle = "Subgroup contributions (p.p.) to headline forecast",
    x        = "Month",
    y        = "Contribution (p.p.)",
    fill     = "Sub-group"
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "fig_subgroup_forecast_contributions.png"),
  plot     = fig_contrib,
  width    = 11,
  height   = 6,
  dpi      = 300
)

cat("Script 05 executado com sucesso: metricas e graficos de 2025 atualizados.\n")