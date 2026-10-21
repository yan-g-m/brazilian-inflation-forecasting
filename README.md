# Forecasting Brazilian Inflation (IPCA): Bottom-Up vs. Top-Down SARIMAX

[![R](https://img.shields.io/badge/Language-R-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

An end-to-end reproducible econometric pipeline in R evaluating whether disaggregated **Bottom-Up** modeling outperforms a direct **Top-Down** benchmark when forecasting headline Brazilian inflation (IPCA).

## Key Results
- **2025 Hold-Out Validation (12 months):** Bottom-Up modeling reduced out-of-sample **RMSE by 19.0%** (0.272 vs. 0.336 p.p.) and **MAE by 16.4%** (0.224 vs. 0.268 p.p.) compared to the aggregate Top-Down specification.
- **Asymmetric Seasonality:** Captures localized sector shocks that aggregate models smooth out, such as the February tuition hike in Education (+4.70% MoM).
- **Residual Diagnostics:** Ljung-Box portmanteau tests confirm white noise residuals for the aggregate model and seven sub-groups, while capturing structural persistence in administered categories (Health Care and Education).
- **2026 Real-Time Monitoring:** Continuous tracker compounding realized monthly prints through September 2026 with Q4 projections to forecast full-year inflation.

## Project Structure
```text
├── data/       <- Cleaned monthly series (BCB REST API) and dynamic IBGE expenditure weights (SIDRA API)
├── scripts/    <- Modular R pipeline (01_data_collection to 06_tracking_2026)
├── outputs/    <- Diagnostic tables (Ljung-Box, ADF, KPSS) and figures
└── reports/    <- Fully reproducible R Markdown paper and rendered HTML report
```

## How to Reproduce
1. Clone or download this repository.
2. Open the project in RStudio (`.Rproj`).
3. Run the complete pipeline via `scripts/` or knit `reports/report_ipca_forecasting.Rmd`.
