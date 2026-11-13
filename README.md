# The Effects of a Wealth Tax Increase on Tax Revenue

Replication materials for the Master's thesis

> Furrer, Y. (2026): The Effects of a Wealth Tax Increase on Tax Revenue: A Difference-in-Differences Analysis of the Norwegian 2022 Tax Reform. Master's Thesis, University of Lucerne, Faculty of Economics and Management, Philosophy, Politics and Economics [PPE].

The thesis compares Norway's mainland tax revenue after the 2022 wealth and income tax reform with that of the other Nordic countries and of a wider pool of ten Western European countries in a difference-in-differences [DiD] design with placebo-based inference.

This repository contains one R script, `Masterarbeit.R`, the raw data it reads, and the output it writes. The script reproduces every number, table, and figure in the thesis, with three documented exceptions that are compiled directly from the cited sources rather than computed: the institutional parameters in Table A1.1, the screening of the Western European comparison group in Table A3.1, and the variable definitions in Table A3.2.

## Contents

| Path | Content |
|---|---|
| `Masterarbeit.R` | The complete analysis, organised in ten sections |
| `Masterarbeit.Rproj` | RStudio project file (sets the working directory) |
| `Data/Raw/` | Input files from the sources listed below |
| `Data/Processed/` | The two estimation panels built by Section 1, `panel_nordics.csv` and `panel_west.csv` |
| `Output/tables/` | All tables as CSV in full precision, some also as rounded TXT |
| `Output/figures/` | All figures as PDF and PNG |
| `Output/results_summary.txt` | Run log with the results of every section, the 46 verification checks, and the session information |

`Data/Processed/` and `Output/` are written by the script. The versions in this repository come from a run on the system used for the thesis (R 4.4.2 on Windows 11).

## How to run

1. Install R and the packages listed under Software.
2. Open `Masterarbeit.Rproj` in RStudio, or set the working directory to the repository folder with `setwd()`.
3. Run the whole script: `source("Masterarbeit.R")`.

A run takes one to two minutes. The script needs no internet access and no API key. It stops with an error if one of its verification checks fails.

## Software

All results in the thesis were produced with R 4.4.2 on Windows 11 and the following package versions.

| Package | Version | Source |
|---|---|---|
| fixest | 0.14.0 | CRAN |
| fwildclusterboot | 0.14.3 | GitHub, commit 336bb574 |
| didimputation | 0.5.1 | CRAN |
| synthdid | 0.0.9 | GitHub |
| dqrng | 0.4.1 | CRAN |
| data.table | 1.17.8 | CRAN |
| ggplot2 | 3.5.1 | CRAN |
| dplyr | 1.1.4 | CRAN |
| tidyr | 1.3.1 | CRAN |
| readr | 2.1.5 | CRAN |
| stringr | 1.5.1 | CRAN |

```r
install.packages(c("readr", "dplyr", "tidyr", "stringr", "ggplot2",
                   "fixest", "didimputation", "remotes"))
remotes::install_github("s3alfisc/fwildclusterboot@336bb574")
remotes::install_github("synth-inference/synthdid")
```

The complete session information of the run is at the end of `Output/results_summary.txt`.

## Structure of the script

The inputs taken from the sources (the Ministry of Finance's ex-ante estimates, the elasticities of the calibration, the register-based migration loss, and the two margins) are collected in one block at the head of the script.

| Section | Content | Thesis |
|---|---|---|
| 1 | Builds the estimation panels from the raw files and runs twelve verification checks against external anchor values | Appendix A3 |
| 2 | Descriptive figures and tables | Figure 4.1, Figures A2.1 to A2.4, Tables A2.1 and A2.2 |
| 3 | Two-way fixed effects [TWFE] models on the Nordic comparison group, wild-bootstrap reference p-values, placebo distributions, minimum detectable effect sizes [MDES] | Tables 4.1 and A1.6 |
| 4 | Western European pool, studentised placebo inference, confidence sets by test inversion | Tables 4.1, A1.8 (Panel A), and A1.9, Appendix A3 |
| 5 | Imputation event study, pre-trend tests, placebo bands | Tables A1.3 and A1.10, Figure 4.3 |
| 6 | Synthetic difference-in-differences [SDID], synthetic control [SC], and DiD on both comparison groups | Table A1.7, Figure A2.5 |
| 7 | Specification ladder, outcome and sample variants, break-even bias with the inversions for the corrected outcomes, margin ladder, scale and start-year sensitivities | Tables A1.4, A1.5, A1.8 (Panel B), A1.13, and A1.14, Figures 4.2 and A2.6 |
| 8 | Treatment from 2021, realised against ex-ante wealth tax revenue, naive comparison | Table A1.11, Chapter 4.1 |
| 9 | Calibration of the margins | Table A1.12 |
| 10 | Mapping of the robustness checks to the identifying assumptions | Table A1.2 |

## Output files

| Thesis | Files in `Output/tables/` and `Output/figures/` |
|---|---|
| Table 4.1 | `tab_main_did`, `tab_pools`, `tab_inference` |
| Figure 4.1 | `fig_deviations` |
| Figure 4.2 | `fig_bounds` |
| Figure 4.3 | `fig_event_bands_both` |
| Chapter 4.1 | `tab_naive` |
| Chapter 4.2 | `tab_estimators` |
| Table A1.2 | `tab_assumption_map` |
| Table A1.3 | `tab_event` |
| Table A1.4 | `tab_ladder` |
| Table A1.5 | `tab_variants`, `tab_variants_adjusted` |
| Table A1.6 | `tab_placebos`, `tab_placebos_pools`, `tab_pools` |
| Table A1.7 | `tab_sdid`, `tab_sdid_weights`, `tab_sdid_west`, `tab_sdid_west_weights` |
| Table A1.8 | `tab_inference` (Panel A), `tab_inference_ext`, `tab_placebos_ex1300` (Panel B) |
| Table A1.9 | `tab_placebos_pools`, `tab_scales` |
| Table A1.10 | `tab_event_bands`, `tab_event_nor` |
| Table A1.11 | `tab_treat2021`, `tab_wealth_realized` |
| Table A1.12 | `tab_calibration` |
| Table A1.13 | `tab_scale_sens` |
| Table A1.14 | `tab_margin_ladder` |
| Tables A2.1 and A2.2 | `tab_wealth_stats`, `tab_summary` |
| Figures A2.1 to A2.4 | `fig_y_main`, `fig_headline_vs_mainland`, `fig_y_h2`, `fig_wealth` |
| Figure A2.5 | `fig_sdid_west` |
| Figure A2.6 | `fig_startyear`, `tab_startyear` |
| Appendix A3 | `tab_west_wealthtax` |

Three further figures are written but not used in the thesis: `fig_event_main_bands`, `fig_event_h2_bands`, and `fig_sdid_west_ex2021`.

## Data

All raw data are publicly available. The files in `Data/Raw/` are the downloads from the sources below. Three small files hold values extracted or aggregated from such downloads: `oecd_isl_4200.csv` and `oecd_nordics_1300.csv` (query links in the two `_README.txt` files) and `brent_annual.csv` (annual means of the monthly series, computed by the author).

| File in `Data/Raw/` | Source | Accessed |
|---|---|---|
| `oecd_revenue_nordics_2010_2024.csv`, `oecd_revenue_oecd_all_2010_2024.csv` | Organisation for Economic Co-operation and Development [OECD], Global Revenue Statistics Database, Comparative Tables, OECD Data Explorer, https://data-explorer.oecd.org/ | June and July 2026 |
| `oecd_isl_4200.csv` | Same database, category 4200, Iceland | 29 September 2026 |
| `oecd_nordics_1300.csv` | Same database, category 1300, Nordic countries | 30 September 2026 |
| `oecd_west_1300.csv` | Same database, category 1300, Norway and the ten Western European donors | 20 October 2026 |
| `07022 Tax accounts ... .csv` | Statistics Norway [SSB], StatBank table 07022, https://www.ssb.no/en/statbank/table/07022?sq=30125327 | July 2026 |
| `07391 Paid and distributed taxes ... .csv` | SSB, StatBank table 07391, https://www.ssb.no/en/statbank/table/07391?sq=30125328 | July 2026 |
| `08815 Taxable wealth ... .csv` | Skatteetaten and SSB, StatBank table 08815, https://www.ssb.no/en/statbank/table/08815?sq=30125330 | July 2026 |
| `09189 Final expenditure ... .csv` | SSB, StatBank table 09189, https://www.ssb.no/en/statbank/table/09189?sq=30125338 | July 2026 |
| `10722 General government ... .csv` | SSB, StatBank table 10722, https://www.ssb.no/en/statbank/table/10722?sq=30125335 | July 2026 |
| `wdi_population.csv`, `wdi_inflation.csv`, `wdi_gdp_per_capita.csv` | World Bank, World Development Indicators [WDI], series SP.POP.TOTL, FP.CPI.TOTL.ZG, and NY.GDP.PCAP.KD, https://databank.worldbank.org/source/world-development-indicators | June 2026 |
| `brent_annual.csv` | International Monetary Fund [IMF], Global Price of Brent Crude, series POILBREUSDM, retrieved from Federal Reserve Economic Data [FRED], Federal Reserve Bank of St. Louis, https://fred.stlouisfed.org/series/POILBREUSDM | July 2026 |

The data remain subject to the terms of use of their providers. OECD, SSB, and World Bank data are published under the Creative Commons Attribution 4.0 licence [CC BY 4.0]. The Brent series is IMF data (Source: International Monetary Fund).

## Reproducibility

All stochastic procedures use seed 1892, the year the Norwegian wealth tax was introduced. The seed is set at the start of every section and before every stochastic block.

The script contains 46 verification checks. They test the panels against external anchor values and the estimates against control values and against each other. The script stops if one of them fails.

A run on a second system (R 4.4.3 on Linux) reproduced every table to more than ten decimal places, with one exception. The Webb wild-bootstrap p-values in `tab_main_did.csv` and `tab_variants.csv` depend on the platform and the package versions and can differ in the second or third decimal. The thesis reports them as a conventional reference only (Table A1.5, Chapter 3.3.1), and they do not enter the placebo-based inference.

## Use of AI tools

The artificial intelligence [AI] model Claude (Fable 5, Anthropic) was used to support the creation of the R script, as stated in the Declaration of Authorship of the thesis. The author reviewed and revised the code and takes full responsibility for it.
