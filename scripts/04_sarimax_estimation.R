# ==============================================================================
# Objective: Estimate SARIMAX models and compute Ljung-Box residual diagnostics
# Author: Yan Galante Maestre
# Date: October 2026
# Dependencies: pacman, here, readr, dplyr, tidyr, purrr, tibble, tsibble, fable, fabletools
# ==============================================================================

if (!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman')
pacman::p_load(here, readr, dplyr, tidyr, purrr, tibble, tsibble, fable, fabletools)

ipca_data    <- readr::read_rds(here::here('data', 'ipca_disaggregated_clean.rds'))
ibge_weights <- readr::read_rds(here::here('data', 'ibge_weights.rds'))
subgroup_cols <- ibge_weights$subgroup

ipca_ts <- ipca_data |>
  dplyr::mutate(
    year_month        = tsibble::yearmonth(date),
    dummy_tariffs2015 = dplyr::if_else(date >= as.Date('2015-01-01') & date <= as.Date('2015-03-01'), 1L, 0L),
    dummy_strike2018  = dplyr::if_else(date >= as.Date('2018-05-01') & date <= as.Date('2018-06-01'), 1L, 0L),
    dummy_covid2020   = dplyr::if_else(date >= as.Date('2020-03-01') & date <= as.Date('2020-05-01'), 1L, 0L)
  ) |>
  tsibble::as_tsibble(index = year_month)

train_ts <- ipca_ts |> dplyr::filter(date <= as.Date('2024-12-31'))

# 1. Top-Down
fit_td <- train_ts |>
  fabletools::model(sarimax = fable::ARIMA(headline ~ dummy_tariffs2015 + dummy_strike2018 + dummy_covid2020))

res_td <- fabletools::augment(fit_td) |> dplyr::pull(.innov)
res_td_clean <- res_td[!is.na(res_td)]

lb_td_12 <- stats::Box.test(res_td_clean, lag = 12, type = 'Ljung-Box')
lb_td_24 <- stats::Box.test(res_td_clean, lag = 24, type = 'Ljung-Box')

diag_td <- tibble::tibble(
  model_target    = 'Headline IPCA (Top-Down)',
  specification   = format(fit_td$sarimax[[1]]),
  lb_stat_lag12   = round(as.numeric(lb_td_12$statistic), 2),
  lb_pvalue_lag12 = round(as.numeric(lb_td_12$p.value), 4),
  lb_stat_lag24   = round(as.numeric(lb_td_24$statistic), 2),
  lb_pvalue_lag24 = round(as.numeric(lb_td_24$p.value), 4),
  residuals_white_noise = dplyr::if_else(lb_td_12$p.value > 0.05, 'Yes (White Noise)', 'Serial Correlation')
)

# 2. Bottom-Up (9 subgrupos)
train_long <- train_ts |>
  tidyr::pivot_longer(cols = dplyr::all_of(subgroup_cols), names_to = 'subgroup', values_to = 'inflation') |>
  tsibble::as_tsibble(key = subgroup, index = year_month)

fit_bu <- train_long |>
  fabletools::model(sarimax = fable::ARIMA(inflation ~ dummy_tariffs2015 + dummy_strike2018 + dummy_covid2020))

aug_bu   <- fabletools::augment(fit_bu) |> tibble::as_tibble()
specs_bu <- fabletools::glance(fit_bu) |> 
  dplyr::select(subgroup) |> 
  dplyr::mutate(spec_text = purrr::map_chr(fit_bu$sarimax, ~ format(.x)))

diag_bu <- subgroup_cols |>
  purrr::map_dfr(function(grp) {
    grp_res <- aug_bu |> dplyr::filter(subgroup == grp) |> dplyr::pull(.innov)
    grp_res <- grp_res[!is.na(grp_res)]
    
    test_12 <- stats::Box.test(grp_res, lag = 12, type = 'Ljung-Box')
    test_24 <- stats::Box.test(grp_res, lag = 24, type = 'Ljung-Box')
    
    spec_str <- specs_bu |> dplyr::filter(subgroup == grp) |> dplyr::pull(spec_text)
    grp_label <- stringr::str_to_title(gsub('_', ' ', grp))
    
    tibble::tibble(
      model_target    = paste0('Sub-group: ', grp_label),
      specification   = spec_str,
      lb_stat_lag12   = round(as.numeric(test_12$statistic), 2),
      lb_pvalue_lag12 = round(as.numeric(test_12$p.value), 4),
      lb_stat_lag24   = round(as.numeric(test_24$statistic), 2),
      lb_pvalue_lag24 = round(as.numeric(test_24$p.value), 4),
      residuals_white_noise = dplyr::if_else(test_12$p.value > 0.05, 'Yes (White Noise)', 'Serial Correlation')
    )
  })

residual_diagnostics <- dplyr::bind_rows(diag_td, diag_bu)
readr::write_csv(residual_diagnostics, here::here('outputs', 'tables', 'residual_diagnostics_ljung_box.csv'))
message('Script 04 concluido com sucesso! Tabela com 10 modelos salva.')
