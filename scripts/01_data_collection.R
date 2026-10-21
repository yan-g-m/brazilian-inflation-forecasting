# ==============================================================================
# Objective: Automated extraction of IPCA Headline and 9 Sub-groups via BCB REST API
# Author: Yan Galante Maestre
# Date: October 2026
# Dependencies: pacman, here, httr, jsonlite, dplyr, purrr, tidyr, readr, lubridate
# ==============================================================================

if (!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman')
pacman::p_load(here, httr, jsonlite, dplyr, purrr, tidyr, readr, lubridate)

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

fetch_bcb_rest <- function(code, name, max_attempts = 5) {
  url <- sprintf('https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/dados?formato=json&dataInicial=01/01/2010&dataFinal=31/12/2025', code)
  
  attempt <- 1
  while (attempt <= max_attempts) {
    res <- tryCatch({
      resp <- httr::GET(
        url,
        httr::user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)'),
        httr::timeout(20)
      )
      if (httr::status_code(resp) == 200) {
        txt <- httr::content(resp, as = 'text', encoding = 'UTF-8')
        raw_df <- jsonlite::fromJSON(txt)
        raw_df |>
          dplyr::transmute(
            date = lubridate::dmy(data),
            !!name := as.numeric(valor)
          )
      } else {
        NULL
      }
    }, error = function(e) NULL)
    
    if (!is.null(res) && nrow(res) > 0) {
      message(sprintf('✓ Serie extraida com sucesso: %-15s (Codigo %d)', name, code))
      Sys.sleep(0.3)
      return(res)
    }
    
    message(sprintf('! Tentativa %d falhou para %s (%d). Aguardando retry...', attempt, name, code))
    Sys.sleep(1.5 * attempt)
    attempt <- attempt + 1
  }
  stop(paste('Falha definitiva na conexao para a serie:', name))
}

message('Extraindo as 10 series historicas via API REST oficial do BCB...')
dados_list <- purrr::imap(bcb_codes, ~ fetch_bcb_rest(.x, .y))

ipca_clean <- purrr::reduce(dados_list, dplyr::full_join, by = 'date') |>
  dplyr::arrange(date) |>
  tidyr::drop_na()

readr::write_rds(ipca_clean, here::here('data', 'ipca_disaggregated_clean.rds'))
readr::write_csv(ipca_clean, here::here('data', 'ipca_disaggregated_clean.csv'))

message('Script 01 executado com sucesso: base salva com ', nrow(ipca_clean), ' observacoes mensais.')
