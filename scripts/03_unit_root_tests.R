# ==============================================================================
# Objective: Rigorous Stationarity Diagnostics (ADF & KPSS with exact test statistics)
# Author: Yan Galante Maestre
# Date: October 2026
# Dependencies: pacman, here, readr, dplyr, purrr, tseries
# ==============================================================================

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, readr, dplyr, purrr, tseries)

ipca_data <- readr::read_rds(here::here("data", "ipca_disaggregated_clean.rds"))

# Series temporais a analisar: Headline + 9 grupos
series_cols <- setdiff(colnames(ipca_data), "date")

run_stationarity_diagnostics <- function(series_name, data) {
  x <- data[[series_name]]
  
  # Augmented Dickey-Fuller (H0: Raiz unitaria / Nao estacionaria)
  adf_res <- tseries::adf.test(x, alternative = "stationary")
  
  # KPSS (H0: Estacionaria em nivel)
  kpss_res <- tseries::kpss.test(x, null = "Level")
  
  tibble::tibble(
    series           = series_name,
    adf_stat         = round(as.numeric(adf_res$statistic), 3),
    adf_crit_5pct    = -2.86, # Valor critico assintotico tabular padrao a 5% com intercepto
    adf_pvalue       = round(as.numeric(adf_res$p.value), 4),
    kpss_stat        = round(as.numeric(kpss_res$statistic), 3),
    kpss_crit_5pct   = 0.463, # Valor critico tabular KPSS a 5% para hipotese de nivel
    kpss_pvalue      = round(as.numeric(kpss_res$p.value), 4),
    conclusion       = dplyr::if_else(adf_res$p.value < 0.05 & kpss_res$p.value > 0.05, "I(0) Stationary", "Inconclusive/Non-stationary")
  )
}

unit_root_results <- purrr::map_dfr(series_cols, run_stationarity_diagnostics, data = ipca_data)

# Guardar a tabela detalhada
readr::write_csv(unit_root_results, here::here("outputs", "tables", "unit_root_test_results.csv"))

cat("Script 03 concluido: Estatisticas exatas ADF e KPSS gravadas com sucesso!\n")