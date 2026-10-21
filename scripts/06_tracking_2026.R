# ==============================================================================
# Objective: Forecast Q4-2026 (Oct, Nov, Dec) and compute full-year 2026 inflation
# Author: Yan Galante Maestre
# Date: October 2026
# Dependencies: pacman, here, rbcb, dplyr, purrr, tidyr, tsibble, fable, ggplot2, readr
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, rbcb, dplyr, purrr, tidyr, tsibble, fable, ggplot2, readr)

ipca_hist    <- readr::read_rds(here::here("data", "ipca_disaggregated_clean.rds"))
ibge_weights <- readr::read_rds(here::here("data", "ibge_weights.rds"))

bcb_codes <- c(
  headline        = 433L,
  food_beverages  = 1635L,
  housing         = 1636L,
  household_goods = 1637L,
  apparel         = 1638L,
  transportation  = 1639L,
  communication   = 1640L,
  health_care     = 1641L,
  personal_exp    = 1642L,
  education       = 1643L
)

# 1. Coleta resiliente das divulgações de 2026
fetch_safe <- function(code, name, max_attempts = 4) {
  attempt <- 1
  while (attempt <= max_attempts) {
    res <- tryCatch({
      df <- rbcb::get_series(code, start_date = as.Date("2026-01-01"), end_date = Sys.Date())
      val_col <- setdiff(colnames(df), "date")[1]
      df |>
        dplyr::select(date, !!name := dplyr::all_of(val_col)) |>
        dplyr::mutate(!!name := as.numeric(.data[[name]]))
    }, error = function(e) NULL)
    
    if (!is.null(res)) {
      Sys.sleep(0.4)
      return(res)
    }
    Sys.sleep(1.5 * attempt)
    attempt <- attempt + 1
  }
  stop(paste("Falha na extração da série:", name))
}

dados_2026_list <- purrr::imap(bcb_codes, ~ fetch_safe(.x, .y))
dados_2026 <- purrr::reduce(dados_2026_list, dplyr::full_join, by = "date") |>
  dplyr::arrange(date) |>
  tidyr::drop_na()

full_data <- dplyr::bind_rows(
  ipca_hist |> dplyr::filter(date < as.Date("2026-01-01")),
  dados_2026
) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    year_month        = tsibble::yearmonth(date),
    dummy_tariffs2015 = dplyr::if_else(date >= as.Date("2015-01-01") & date <= as.Date("2015-03-01"), 1L, 0L),
    dummy_strike2018  = dplyr::if_else(date >= as.Date("2018-05-01") & date <= as.Date("2018-06-01"), 1L, 0L),
    dummy_covid2020   = dplyr::if_else(date >= as.Date("2020-03-01") & date <= as.Date("2020-05-01"), 1L, 0L)
  ) |>
  tsibble::as_tsibble(index = year_month)

last_realized_date <- max(full_data$date)
cat("Último mês realizado consolidado:", format(last_realized_date, "%b/%Y"), "\n")

# 2. Horizonte de previsão explícito para fechar 2026 (Outubro, Novembro e Dezembro)
horizon_dates <- seq(as.Date("2026-10-01"), as.Date("2026-12-01"), by = "1 month")

horizon_q4 <- tsibble::tsibble(
  year_month        = tsibble::yearmonth(horizon_dates),
  dummy_tariffs2015 = 0L,
  dummy_strike2018  = 0L,
  dummy_covid2020   = 0L,
  index             = year_month
)

# 3. Estimação e Projeção Top-Down
fit_td <- full_data |>
  fabletools::model(sarimax = fable::ARIMA(headline ~ dummy_tariffs2015 + dummy_strike2018 + dummy_covid2020))

fc_td <- fit_td |>
  fabletools::forecast(new_data = horizon_q4) |>
  tibble::as_tibble() |>
  dplyr::select(year_month, pred_top_down = .mean)

# 4. Estimação e Projeção Bottom-Up
train_long <- full_data |>
  tidyr::pivot_longer(
    cols      = dplyr::all_of(ibge_weights$subgroup),
    names_to  = "subgroup",
    values_to = "inflation"
  ) |>
  tsibble::as_tsibble(key = subgroup, index = year_month)

horizon_long <- tidyr::expand_grid(
  subgroup   = ibge_weights$subgroup,
  year_month = horizon_q4$year_month
) |>
  dplyr::left_join(horizon_q4, by = "year_month") |>
  tsibble::as_tsibble(key = subgroup, index = year_month)

fit_bu <- train_long |>
  fabletools::model(sarimax = fable::ARIMA(inflation ~ dummy_tariffs2015 + dummy_strike2018 + dummy_covid2020))

fc_groups <- fit_bu |>
  fabletools::forecast(new_data = horizon_long) |>
  tibble::as_tibble() |>
  dplyr::select(subgroup, year_month, pred_subgroup = .mean) |>
  dplyr::left_join(ibge_weights, by = "subgroup") |>
  dplyr::mutate(contribution = pred_subgroup * weight_decimal)

fc_bu <- fc_groups |>
  dplyr::group_by(year_month) |>
  dplyr::summarise(pred_bottom_up = sum(contribution), .groups = "drop")

# 5. Consolidação mensal completa de 2026 (Jan a Dez)
all_2026_ym <- tsibble::yearmonth(seq(as.Date("2026-01-01"), as.Date("2026-12-01"), by = "1 month"))
realized_2026 <- full_data |>
  dplyr::filter(date >= as.Date("2026-01-01")) |>
  tibble::as_tibble() |>
  dplyr::select(year_month, actual_ipca = headline)

tracking_2026 <- tibble::tibble(year_month = all_2026_ym) |>
  dplyr::left_join(realized_2026, by = "year_month") |>
  dplyr::left_join(fc_td, by = "year_month") |>
  dplyr::left_join(fc_bu, by = "year_month")

readr::write_csv(tracking_2026, here::here("outputs", "tables", "tracking_2026_full_year.csv"))

# 6. Cálculo da Inflação Fechada de 2026 (% a.a. acumulada)
# Fator acumulado realizado (Jan a Set)
realized_vec <- tracking_2026 |> dplyr::filter(!is.na(actual_ipca)) |> dplyr::pull(actual_ipca)
factor_realized <- prod(1 + (realized_vec / 100))

# Fatores projetados Q4 (Out a Dez)
fc_td_vec <- tracking_2026 |> dplyr::filter(is.na(actual_ipca)) |> dplyr::pull(pred_top_down)
fc_bu_vec <- tracking_2026 |> dplyr::filter(is.na(actual_ipca)) |> dplyr::pull(pred_bottom_up)

full_year_td <- (factor_realized * prod(1 + (fc_td_vec / 100)) - 1) * 100
full_year_bu <- (factor_realized * prod(1 + (fc_bu_vec / 100)) - 1) * 100
realized_ytd <- (factor_realized - 1) * 100

annual_summary <- tibble::tibble(
  metric = c("Realized YTD (Jan-Sep)", "Projected Full-Year 2026 (Top-Down)", "Projected Full-Year 2026 (Bottom-Up)"),
  rate_percent = c(realized_ytd, full_year_td, full_year_bu)
)

readr::write_csv(annual_summary, here::here("outputs", "tables", "annual_inflation_2026_summary.csv"))

cat("\n========================================================\n")
cat(sprintf("IPCA Acumulado Realizado (Jan-Set/2026): %.2f%%\n", realized_ytd))
cat(sprintf("IPCA Fechado 2026 Projetado (Top-Down):   %.2f%%\n", full_year_td))
cat(sprintf("IPCA Fechado 2026 Projetado (Bottom-Up):  %.2f%%\n", full_year_bu))
cat("========================================================\n\n")

# 7. Gráfico: Histórico Realizado + Projeções até Dezembro
last_ym  <- tsibble::yearmonth(last_realized_date)
last_val <- full_data |> dplyr::filter(year_month == last_ym) |> dplyr::pull(headline)

plot_realized <- tracking_2026 |>
  dplyr::filter(!is.na(actual_ipca)) |>
  dplyr::transmute(date = as.Date(year_month), value = actual_ipca, series = "Actual IPCA (Released)")

plot_forecasts <- dplyr::bind_rows(
  tibble::tibble(date = as.Date(last_ym), value = last_val, series = c("Top-Down Forecast", "Bottom-Up Forecast")),
  tracking_2026 |> dplyr::filter(is.na(actual_ipca)) |> dplyr::transmute(date = as.Date(year_month), value = pred_top_down, series = "Top-Down Forecast"),
  tracking_2026 |> dplyr::filter(is.na(actual_ipca)) |> dplyr::transmute(date = as.Date(year_month), value = pred_bottom_up, series = "Bottom-Up Forecast")
)

fig_tracking <- ggplot2::ggplot() +
  ggplot2::geom_line(data = plot_realized, ggplot2::aes(x = date, y = value, color = series), linewidth = 1.2) +
  ggplot2::geom_point(data = plot_realized, ggplot2::aes(x = date, y = value, color = series), size = 2.5) +
  ggplot2::geom_line(data = plot_forecasts, ggplot2::aes(x = date, y = value, color = series, linetype = series), linewidth = 1.1) +
  ggplot2::geom_point(data = plot_forecasts |> dplyr::filter(date > as.Date(last_ym)), ggplot2::aes(x = date, y = value, color = series), size = 2.5) +
  ggplot2::scale_x_date(
    date_labels = "%b/%y",
    breaks = as.Date(all_2026_ym),
    limits = c(as.Date("2026-01-01"), as.Date("2026-12-01"))
  ) +
  ggplot2::scale_color_manual(values = c("Actual IPCA (Released)" = "black", "Top-Down Forecast" = "#D95F02", "Bottom-Up Forecast" = "#1B9E77")) +
  ggplot2::scale_linetype_manual(values = c("Top-Down Forecast" = "dashed", "Bottom-Up Forecast" = "twodash")) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  ) +
  ggplot2::labs(
    title    = "2026 IPCA Tracking: Realized Path and Q4 Forecasts",
    subtitle = "Actual monthly figures through September 2026 and out-of-sample projections through December 2026",
    x        = "Month",
    y        = "% MoM"
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "fig_tracking_2026_trajectories.png"),
  plot     = fig_tracking,
  width    = 10,
  height   = 5.5,
  dpi      = 300
)

# 8. Gráfico de Decomposição Setorial do Q4
fig_decomp_q4 <- fc_groups |>
  ggplot2::ggplot(ggplot2::aes(x = as.Date(year_month), y = contribution, fill = subgroup)) +
  ggplot2::geom_col(position = "stack", color = "white", linewidth = 0.2, width = 20) +
  ggplot2::scale_x_date(
    date_labels = "%b/%y",
    breaks = as.Date(horizon_q4$year_month)
  ) +
  ggplot2::scale_fill_brewer(palette = "Set3") +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::labs(
    title    = "Q4-2026 Bottom-Up Forecast: Sub-group Contributions",
    subtitle = "Projected contributions (p.p.) for October, November, and December 2026",
    x        = "Month",
    y        = "Contribution (p.p.)",
    fill     = "Sub-group"
  )

ggplot2::ggsave(
  filename = here::here("outputs", "figures", "fig_decomp_2026_contributions.png"),
  plot     = fig_decomp_q4,
  width    = 8,
  height   = 5.5,
  dpi      = 300
)

cat("Script 06 concluído com sucesso.\n")