# Replication of Barro (1991): Economic Growth in a Cross Section of Countries

A replication of Robert J. Barro's seminal 1991 paper using contemporary data (2000–2023), constructed from the World Development Indicators (WDI) and Penn World Table (PWT 10.0).

---

## Overview

Barro (1991) analysed data from 98 countries over 1960–1985 to argue that poor countries do not inherently grow faster than rich ones — but do so after controlling for human capital. This study asks whether those findings hold in the modern era.

Using cross-country data for 2000–2023, this replication estimates 27 OLS regressions covering:
- Per capita GDP growth (Tables 1 and 4)
- Fertility and demographic dynamics (Table 2)
- Investment as a share of GDP (Table 3)

Key findings are broadly consistent with Barro (1991): conditional convergence holds, secondary education positively predicts growth, government consumption and investment price distortions are negatively associated with growth, and Sub-Saharan Africa exhibits persistently lower growth even after controlling for observable fundamentals.

---

## Data Sources

| Source | Coverage | Variables |
|---|---|---|
| World Development Indicators (WDI) | 2000–2023 | GDP per capita, school enrolment, fertility, child mortality, population, political instability |
| Penn World Table (PWT 10.0) | 2000–2023 | Investment share of GDP, government consumption share, PPP price of investment |

Data is downloaded directly within the R script using the `WDI` and `pwt10` packages — no manual downloads required.

---

## Variables

| Variable | Description | Source |
|---|---|---|
| `GROWTH_0023` | Log growth rate of real GDP per capita, 2000–2023 | WDI |
| `GROWTH_1023` | Log growth rate of real GDP per capita, 2010–2023 | WDI |
| `logGDP` | Log of real GDP per capita in 2000 | WDI |
| `SEC00`, `PRIM00` | Secondary and primary school enrolment rates (2000) | WDI |
| `SEC90`, `PRIM90` | School enrolment rates (1990), used as pre-determined human capital | WDI |
| `SEC_RATIO`, `PRIM_RATIO` | Pupil-teacher ratios (2000) | WDI |
| `LIT00` | Adult literacy rate (2000) | WDI |
| `gc_y` | Average government consumption / GDP, 2000–2023 | PWT 10.0 |
| `i_y` | Average investment share of GDP, 2000–2023 | PWT 10.0 |
| `PPPI_DEV` | Deviation of PPP investment price from global mean (market distortion proxy) | PWT 10.0 |
| `VIOLENCE` | Average intentional homicides per 100,000 (political instability proxy) | WDI |
| `FERTNET` | Net fertility rate: FERT × (1 − MORT04/1000) | WDI |
| `AFRICA` | Dummy = 1 for Sub-Saharan Africa | countrycode |
| `LATAM` | Dummy = 1 for Latin America & Caribbean | countrycode |

Full variable definitions are in Appendix 2 of the paper.

---

## Key Results

- **Conditional convergence confirmed:** The unconditional correlation between initial income and growth is weak (−0.31), but becomes significantly negative (−0.55) after controlling for human capital and policy variables.
- **Human capital:** Secondary enrolment is positively and significantly associated with growth across most specifications; primary enrolment is not, consistent with Barro (1991).
- **Government consumption:** Consistently negative and significant across growth regressions.
- **Investment price distortions (PPPI_DEV):** Strongly negative and significant — higher distortions are associated with lower growth.
- **Africa dummy:** Negative and statistically significant in growth regressions even after controlling for investment, fertility, and education, suggesting unobserved institutional or structural factors.
- **Fertility:** Higher net fertility reduces per capita growth; education strongly reduces fertility, consistent with demographic transition theory.

---

## Files

```
├── replication_of_barro.R   # Full analysis: data download, variable construction,
│                            # 27 regressions, 11 figures, 4 regression tables
└── REPLICATION_OF_BARRO.pdf # Written report with results, tables, and interpretation
```

---

## How to Run

### Requirements

Install the following R packages before running:

```r
install.packages(c("WDI", "dplyr", "tidyr", "pwt10", "countrycode",
                   "ggplot2", "stargazer"))
```

### Steps

1. Clone or download this repository
2. Open `replication_of_barro.R` in R or RStudio
3. Run the script from top to bottom — data is downloaded automatically via the `WDI` and `pwt10` packages
4. Outputs generated:
   - `figure1.png` through `figure11.png` — scatter plots and partial correlation plots
   - `table1.1.html`, `table1.2.html`, `table2.html`, `table3.html`, `table4.html` — regression tables
   - `summary_table.html` — summary statistics (Appendix 1)

> **Note:** Running the full script requires an internet connection for WDI data download. PWT 10.0 is loaded locally via the `pwt10` package.

---

## Limitations

- Sample size is smaller than Barro (1991) (33–38 countries in most regressions vs. 98), due to missing data across all required variables
- Barro's original political instability variables (REV, ASSASS) are not available for this period; intentional homicides serve as a proxy
- Private investment (Barro's `ipriv/y`) cannot be separated from public investment in PWT; total investment share `i_y` is used instead
- Economic systems dummy (socialist vs. non-socialist) from the original paper could not be replicated due to lack of comparable WDI data for the modern period

---

## Reference

Barro, R. J. (1991). Economic Growth in a Cross Section of Countries. *The Quarterly Journal of Economics*, 106(2), 407–443.

---

*MSc Economics, NIT Kurukshetra — Internal Assignment, 2nd Semester*
