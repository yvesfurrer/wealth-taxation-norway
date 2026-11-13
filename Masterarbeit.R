##############################################################################
# Masterarbeit.R
#
# The Effects of a Wealth Tax Increase on Tax Revenue: A Difference-in-
# Differences Analysis of the Norwegian 2022 Tax Reform
# Yves Furrer, University of Lucerne (PPE)
#
# HOW TO RUN:
# Keep the folder structure of the repository: this script in the
# project folder and all raw data files, with unchanged file names, in
# the subfolder Data/Raw.
##############################################################################


##############################################################################
# SETTINGS
##############################################################################

if (!dir.exists("Data/Raw"))
  stop("Data/Raw not found: set the working directory to the project folder (see HOW TO RUN).", call. = FALSE)

# Fixed line width, so that the TXT tables do not depend on the console.
options(width = 200)


##############################################################################
# PACKAGES
##############################################################################
# Versions used for the thesis: fixest 0.14.0, fwildclusterboot 0.14.3
# (GitHub, commit 336bb574), didimputation 0.5.1, synthdid 0.0.9 (GitHub).
# Installation:
#   install.packages(c("readr", "dplyr", "tidyr", "stringr", "ggplot2",
#                      "fixest", "didimputation", "remotes"))
#   remotes::install_github("s3alfisc/fwildclusterboot@336bb574")
#   remotes::install_github("synth-inference/synthdid")

PKGS <- c("readr", "dplyr", "tidyr", "stringr", "ggplot2", "fixest",
          "fwildclusterboot", "didimputation", "synthdid")
missing_pkgs <- PKGS[!vapply(PKGS, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs))
  stop("Missing packages: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
suppressPackageStartupMessages(for (p in PKGS) library(p, character.only = TRUE))


##############################################################################
# CONSTANTS
##############################################################################

SEED    <- 1892                                   # year the Norwegian wealth tax was introduced
YEARS   <- 2010:2024
NORDICS <- c("NOR", "SWE", "DNK", "FIN", "ISL")
CONTROLS <- c("SWE", "DNK", "FIN", "ISL")         # Nordic controls (order of Tables A1.5, A1.6)
NORDIC4  <- sort(CONTROLS)                        # the same, alphabetically (comparison groups, SDID)
# Western European comparison group (Appendix A3, Table A3.1)
WEST_DONORS <- c("AUT", "BEL", "CHE", "DEU", "DNK", "FIN", "FRA", "ISL", "NLD", "SWE")
OECD_AGGREGATES <- c("FEDOECD", "OECD_REP", "UNIOECD")


##############################################################################
# EXTERNAL INPUTS (taken from the cited sources, not estimated)
##############################################################################

# Margins in percentage points of mainland GDP (Ch. 2.5)
DELTA1 <- 0.70   # H1: revenue of the net wealth tax
DELTA2 <- 0.27   # PIT mechanism check (Ch. 4.4): mechanical gross increase of the PIT by the package

# Quantified claim of the debate (Ch. 2.2, Table A1.15; Dellanna, 2023)
DEBATE_WEALTH_BN   <- 600     # wealth of the leavers, NOK bn (Neate, 2023, in Dellanna, 2023)
DEBATE_RATE        <- 0.011   # wealth tax rate applied to it
DEBATE_REV_2019_BN <- 16.1    # wealth tax revenue 2019, NOK bn (Dellanna, 2023)
DEBATE_RATE_UP     <- 0.10    # assumed yield of the rate increase: one tenth of the revenue
DEBATE_CLAIM_BN    <- DEBATE_WEALTH_BN * DEBATE_RATE - DEBATE_REV_2019_BN * DEBATE_RATE_UP   # NOK 4.99 bn

# Ex-ante estimates of the Ministry of Finance for the package measures,
# NOK bn, accrued (Norwegian Ministry of Finance, 2021b, p. 9; 2022b, ch. 1,
# Table 1.1; 2023, ch. 1, Table 1.1)
MOF_WEALTH <- c(`2022` = 4.040,           # wealth tax measures 2022 (2021b)
                `2023` = 2.435)           # wealth tax measures 2023 (2022b)
MOF_PIT    <- c(div_2022     = 3.245,     # dividend gross-up factor 1.44 -> 1.6 (2021b)
                div_2023     = 2.460,     # dividend gross-up factor 1.6 -> 1.72 (2022b)
                bracket_2023 = 3.875,     # bracket tax 2023 (2022b)
                bracket_2024 = 1.6)       # bracket tax 2024 (2023), rounded

# Ex-ante estimates of the non-package measures, NOK bn, accrued (Norwegian
# Ministry of Finance, 2022a; 2022b; 2023; Stortinget, 2022b, pp. 29, 31-32,
# 43, and 50). Summands per year: resource rent tax on hydropower, high-price
# contribution, employer's SSC, resource rent tax on aquaculture and on
# onshore wind. Derivation: Table A3.2.
NP_BN         <- c(`2022` = 11.2 + 4.0, `2023` = 11.2 + 12.0 + 7.7 + 2.15, `2024` = 11.2 + (7.7 - 1.82) + 2.15 + 0.15)
NP_EX_A112_BN <- c(`2022` = 4.0, `2023` = 12.0 + 7.7 + 2.15, `2024` = (7.7 - 1.82) + 2.15 + 0.15)

# Ex-ante net effect of all PIT measures per budget, NOK bn, accrued (Ch. 4.4;
# Norwegian Ministry of Finance, 2021b, p. 9; 2022b; 2023). The non-package
# PIT reliefs are the net effect minus the PIT increases of the package,
# cumulated over the budgets.
MOF_PIT_NET    <- c(`2022` = -0.4, `2023` = -1.2, `2024` = -0.4)
MOF_PIT_BUDGET <- c(`2022` = MOF_PIT[["div_2022"]],
                    `2023` = MOF_PIT[["div_2023"]] + MOF_PIT[["bracket_2023"]],
                    `2024` = MOF_PIT[["bracket_2024"]])
NP_PIT_CUT_BN  <- cumsum(MOF_PIT_BUDGET - MOF_PIT_NET)   # reliefs, cumulated: 3.645 / 11.18 / 13.18
NP_SYM_BN      <- NP_BN - NP_PIT_CUT_BN                  # non-package measures net: 11.555 / 21.87 / 6.2

# Register-based wealth tax loss from emigration, NOK bn per year (Ring, 2026, p. 3)
RING_WT_LOSS_BN <- 0.65

# Calibration (Section 9, Table A1.12)
CAL <- list(
  ret             = 0.05,    # pre-tax return (assumption, notes to Table A1.12)
  rate_pre        = 0.0085,  # wealth tax rate 2021 (Table A1.1)
  rate_post       = 0.011,   # rate of the second bracket from 2022 (Table A1.1)
  val_shares_pre  = 0.55,    # valuation of shares and business assets 2021 (Table A1.1)
  val_shares_post = 0.80,    # the same from 2023 (Table A1.1); other assets at 100 %
  elast_ret_lo    = 0.2,     # elasticity of taxable wealth with respect to the net-of-tax return
  elast_ret_hi    = 0.4,     #   (Jakobsen et al., 2020, p. 364: 0.2 moderately wealthy, 0.4 top 1 %)
  elast_rate_lo   = 0.09,    # elasticity with respect to the net-of-tax rate 1 - tau
  elast_rate_hi   = 0.27,    #   (Seim, 2017, pp. 395 and 405)
  migr_per_krone  = 0.24,    # migration: kroner lost per krone raised (Jakobsen et al., 2024, p. 36)
  ch_base_per_pp  = 0.43,    # subnational base response per percentage point of the rate (Bruelhart et al., 2022, p. 113)
  debate_lo       = 0.3,     # annual losses quantified in the debate, NOK bn (Ch. 2.2)
  debate_hi       = 1.1)


##############################################################################
# HELPER FUNCTIONS
##############################################################################

# --- Log (Output/results_summary.txt) and hard checks -----------------------
dir.create("Data/Processed", recursive = TRUE, showWarnings = FALSE)
dir.create("Output/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("Output/figures", recursive = TRUE, showWarnings = FALSE)
SUMMARY <- "Output/results_summary.txt"
cat(sprintf("Masterarbeit.R -- run started %s\n\n",
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")), file = SUMMARY)

log_add <- function(...) {                        # line to console and log
  txt <- paste0(...)
  cat(txt, "\n")
  cat(txt, "\n", sep = "", file = SUMMARY, append = TRUE)
  invisible(NULL)
}
w <- function(...) log_add(sprintf(...))          # formatted log line
section <- function(title) {
  log_add(strrep("=", 72))
  log_add(title, " -- ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  log_add(strrep("=", 72))
}

check_log <- character(0); check_fail <- FALSE
check <- function(id, pass, desc, detail = "") {  # hard verification check
  ok <- isTRUE(pass)
  line <- sprintf("CHECK %s [%s] %s | %s", id, if (ok) "PASS" else "FAIL", desc, detail)
  log_add(line)
  check_log <<- c(check_log, line)
  if (!ok) check_fail <<- TRUE
  invisible(ok)
}
stop_if_failed <- function(where) {
  if (check_fail) stop(where, ": verification check FAIL, see ", SUMMARY, call. = FALSE)
}

# --- Formatting, tables, figures ---------------------------------------------
fmt <- function(x, d = 3) formatC(x, format = "f", digits = d)
out_files <- character(0)

# Table as CSV (full precision) and readable TXT (rounded)
save_table <- function(df, name, digits = 2) {
  csv_path <- file.path("Output/tables", paste0(name, ".csv"))
  txt_path <- file.path("Output/tables", paste0(name, ".txt"))
  write_csv(df, csv_path)
  df_txt <- df |> mutate(across(where(is.numeric), \(x) round(x, digits)))
  writeLines(capture.output(print(as.data.frame(df_txt), row.names = FALSE)),
             txt_path)
  out_files <<- c(out_files, csv_path, txt_path)
  invisible(NULL)
}

# Figures: reform line between 2021 and 2022, common layout, PDF + PNG
REFORM_X <- 2021.5

thesis_theme <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
        axis.title.y = element_text(size = 10, margin = margin(r = 6)),
        axis.text = element_text(color = "grey30"),
        legend.position = "top",
        legend.title = element_blank(),
        plot.margin = margin(8, 12, 8, 8))

save_fig <- function(p, name, width = 8, height = 5) {
  for (ext in c("pdf", "png")) {
    path <- file.path("Output/figures", paste0(name, ".", ext))
    ggsave(path, p, width = width, height = height, dpi = 300)
    out_files <<- c(out_files, path)
  }
  invisible(NULL)
}

# --- Samples and estimation helpers (Sections 3-8) --------------------------
SAMPLE_DEF <- list(
  main   = list(label = "main: 2015-2024 excl. 2021, ISL2016 indicator", dummy = TRUE,
                keep = function(d) d$year >= 2015 & d$year != 2021,
                pre = 2015:2020, ref_year = 2020),
  full   = list(label = "full: 2010-2024, ISL2016 and DNK2014 indicators", dummy = TRUE,
                keep = function(d) rep(TRUE, nrow(d)),
                pre = 2010:2021, ref_year = 2021),
  noind  = list(label = "noind: 2010-2024, no indicators", dummy = FALSE,
                keep = function(d) rep(TRUE, nrow(d)),
                pre = 2010:2021, ref_year = 2021))
SAMPLES_MAIN <- c("main", "full")

# One-off indicators: ISL 2016 (bank winding-up) and DNK 2014 (capital pensions)
IND_VARS <- c("isl2016", "dnk2014")
add_ind <- function(d) d |> mutate(isl2016 = as.integer(iso == "ISL" & year == 2016),
                                   dnk2014 = as.integer(iso == "DNK" & year == 2014))
ind_of <- function(d) IND_VARS[vapply(IND_VARS, function(v) v %in% names(d) && any(d[[v]] == 1), logical(1))]
ind_rhs <- function(d) if (length(ind_of(d))) paste(ind_of(d), collapse = " + ") else "1"

prep_sample <- function(d, s) {
  d <- d[SAMPLE_DEF[[s]]$keep(d), ]
  d |> mutate(iso_f = factor(iso), year_f = factor(year)) |> add_ind()
}
rhs_of <- function(s, treat = "treat", d)
  if (SAMPLE_DEF[[s]]$dummy) c(treat, ind_of(d)) else treat
fml_of <- function(y, s, treat = "treat", d)
  as.formula(paste0(y, " ~ ", paste(rhs_of(s, treat, d), collapse = " + "),
                    " | iso_f + year_f"))

B_BOOT   <- 9999
MDES_A_T4 <- qt(0.975, 4) + qt(0.80, 4)   # = 3.72 (five clusters)

run_boot <- function(fit, type, param = "treat", B = B_BOOT) {
  set.seed(SEED)
  if (requireNamespace("dqrng", quietly = TRUE)) dqrng::dqset.seed(SEED)
  suppressWarnings(suppressMessages(
    fwildclusterboot::boottest(fit, param = param, clustid = "iso_f",
                               B = B, type = type)))
}
num1 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) < 1) NA_real_ else x[1]
}
bt_p <- function(bt) list(p = num1(bt$p_val),
                          B = num1(if (!is.null(bt$boot_iter)) bt$boot_iter else bt$B))

# feols fit whose call carries the data name as a symbol
fit_named <- function(fml, dname)
  eval(bquote(feols(.(fml), data = .(as.name(dname)), cluster = ~iso_f)))
# p: with cluster standard errors fixest uses the t distribution with G - 1
# degrees of freedom (column p_cluster, in the tables "t(G - 1) p-value")
coef_row <- function(fit, param = "treat") {
  ct <- coeftable(fit)
  list(coef = ct[param, "Estimate"], se = ct[param, "Std. Error"],
       p = ct[param, "Pr(>|t|)"], n = fit$nobs)
}

##############################################################################
##############################################################################
# SECTION 1: DATA (panels from the raw data, hard verification checks)
##############################################################################
##############################################################################

set.seed(SEED)
section("Section 1: Data")
n0 <- length(check_log)

# ============================================================================
# 1. Read OECD revenue files (comma-separated, UTF-8, interleaved label cols)
# ============================================================================

read_oecd <- function(path) {
  read_csv(path, show_col_types = FALSE, name_repair = "unique_quiet",
           progress = FALSE) |>
    select(REF_AREA, STANDARD_REVENUE, UNIT_MEASURE, TIME_PERIOD,
           OBS_VALUE, UNIT_MULT) |>
    filter(!REF_AREA %in% OECD_AGGREGATES,
           TIME_PERIOD %in% YEARS)
}

oecd_nord <- read_oecd("Data/Raw/oecd_revenue_nordics_2010_2024.csv")
oecd_all  <- read_oecd("Data/Raw/oecd_revenue_oecd_all_2010_2024.csv")

stopifnot(!anyDuplicated(oecd_nord[c("REF_AREA","STANDARD_REVENUE","UNIT_MEASURE","TIME_PERIOD")]),
          !anyDuplicated(oecd_all [c("REF_AREA","STANDARD_REVENUE","UNIT_MEASURE","TIME_PERIOD")]))

# Wide per country-year: pct (% of GDP) and xdc (levels in national currency).
# Missing T_4210 rows mean zero wealth tax revenue (treat as 0 in sums);
# Iceland is the exception and is patched from the 4200 level in build_panel().
oecd_wide <- function(df) {
  df |>
    mutate(cat = if_else(STANDARD_REVENUE == "_T", "T",
                         str_remove(STANDARD_REVENUE, "^T_")),
           value = if_else(UNIT_MEASURE == "XDC",
                           OBS_VALUE * 10^UNIT_MULT, OBS_VALUE),
           unit = if_else(UNIT_MEASURE == "XDC", "xdc", "pct")) |>
    select(iso = REF_AREA, year = TIME_PERIOD, unit, cat, value) |>
    pivot_wider(names_from = c(unit, cat), values_from = value,
                names_sep = "_") |>
    mutate(across(any_of(c("pct_4210", "xdc_4210")), \(x) coalesce(x, 0)))
}

wide_nord <- oecd_wide(oecd_nord)
wide_all  <- oecd_wide(oecd_all)

# --- Iceland: recurrent net wealth tax only reported at the 4200 level -----
isl_4200 <- read_csv("Data/Raw/oecd_isl_4200.csv", show_col_types = FALSE) |>
  transmute(iso, year = as.integer(year), pct_4200_isl = as.numeric(pct_4200))
stopifnot(nrow(isl_4200) == 15, all(isl_4200$iso == "ISL"),
          identical(isl_4200$year, YEARS), !anyNA(isl_4200$pct_4200_isl))

# --- OECD category 1300 (income taxes unallocable between 1100 and 1200) --
# Used for the control-side variant y_main_ex1300 (Danish pension yield tax).
nord_1300 <- read_csv("Data/Raw/oecd_nordics_1300.csv", show_col_types = FALSE) |>
  transmute(iso, year = as.integer(year), pct_1300 = as.numeric(pct_1300))
stopifnot(nrow(nord_1300) == 75, setequal(unique(nord_1300$iso), NORDICS),
          !anyNA(nord_1300$pct_1300))

# The same category for the Western European pool (Section 7, block 4c)
west_1300 <- read_csv("Data/Raw/oecd_west_1300.csv", show_col_types = FALSE,
                      col_types = cols(.default = col_character()), name_repair = "unique_quiet") |>
  transmute(iso = REF_AREA, year = as.integer(TIME_PERIOD), pct_1300 = as.numeric(OBS_VALUE))
stopifnot(nrow(west_1300) == 165, setequal(unique(west_1300$iso), c("NOR", WEST_DONORS)),
          !anyNA(west_1300$pct_1300), all(table(west_1300$iso) == 15))
ovl_1300 <- inner_join(nord_1300, west_1300, by = c("iso", "year"), suffix = c("_n", "_w"))
stopifnot(nrow(ovl_1300) == 75, max(abs(ovl_1300$pct_1300_n - ovl_1300$pct_1300_w)) < 1e-6)
oecd_1300 <- bind_rows(nord_1300, west_1300 |> filter(!iso %in% NORDICS)) |> arrange(iso, year)

# ============================================================================
# 2. Read Statistics Norway files (semicolon, windows-1252, title line -> skip)
# ============================================================================

# Semicolon-separated with '.' as decimal mark (e.g. "12304.0" in 07022), so
# read_delim with a plain locale: read_csv2 would treat '.' as grouping mark.
ssb_locale <- locale(encoding = "windows-1252", decimal_mark = ".")
SSB_NA <- c(".", "..", ":", "-", "")

read_ssb <- function(path) {
  read_delim(path, delim = ";", skip = 1, locale = ssb_locale, na = SSB_NA,
             show_col_types = FALSE, progress = FALSE)
}

# --- 10722: accrual taxes by type, NOK million ------------------------------
ssb_10722 <- read_ssb(paste0("Data/Raw/10722 General government. Taxes and social ",
                             "security contributions (NOK million), by type, year and sector.csv"))

ssb_row <- function(df, prefix) {
  row <- df |> filter(str_starts(.data[[names(df)[1]]], prefix))
  stopifnot(nrow(row) == 1)
  row |>
    pivot_longer(-1, names_to = "col", values_to = "value") |>
    mutate(year = as.integer(str_extract(col, "\\d{4}"))) |>
    filter(year %in% YEARS) |>
    select(year, value) |>
    arrange(year)
}

nor_ssb <- ssb_row(ssb_10722, "A12 ") |> rename(a12 = value) |>
  left_join(ssb_row(ssb_10722, "A121 ") |> rename(a121 = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A122 ") |> rename(a122 = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A23 ")  |> rename(a23  = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A112 ") |> rename(a112 = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A113 ") |> rename(a113 = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A1 ")   |> rename(a1   = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A2 ")   |> rename(a2   = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A3 ")   |> rename(a3   = value), by = "year") |>
  left_join(ssb_row(ssb_10722, "A4 ")   |> rename(a4   = value), by = "year")

# --- 09189: GDP total and Mainland Norway, current prices, NOK million ------
ssb_09189 <- read_ssb(paste0("Data/Raw/09189 Final expenditure and gross domestic ",
                             "product, by macroeconomic indicator and year.csv"))

gdp_row <- function(label) {
  row <- ssb_09189 |> filter(.data[[names(ssb_09189)[1]]] == label)
  stopifnot(nrow(row) == 1)
  row |>
    pivot_longer(-1, names_to = "col", values_to = "value") |>
    mutate(year = as.integer(str_extract(col, "^\\d{4}"))) |>
    filter(year %in% YEARS) |>
    select(year, value) |>
    arrange(year)
}

nor_gdp <- gdp_row("Gross domestic product, market values") |>
  rename(gdp_tot_ssb_mill = value) |>
  left_join(gdp_row("Gross domestic product Mainland Norway, market values") |>
              rename(gdp_ml_mill = value), by = "year")

nor_ssb <- left_join(nor_ssb, nor_gdp, by = "year")

# --- 07391 / 07022: cash-based tax accounts (crosschecks only) --------------
ssb_07391 <- read_ssb(paste0("Data/Raw/07391 Paid and distributed taxes ",
                             "(NOK million), by type, month and tax arrangement.csv"))
ssb_07022 <- read_ssb(paste0("Data/Raw/07022 Tax accounts. Accumulated figures ",
                             "(NOK million), by region, month and type of tax.csv"))

col_07391_dec22 <- grep("2022M12 Distributed taxes, total", names(ssb_07391),
                        value = TRUE, fixed = TRUE)
stopifnot(length(col_07391_dec22) == 1)
v_07391 <- function(rowlabel) {
  row <- ssb_07391 |> filter(.data[[names(ssb_07391)[1]]] == rowlabel)
  stopifnot(nrow(row) == 1)
  as.numeric(row[[col_07391_dec22]])
}
pet_cash_2022 <- v_07391("Ordinary taxes on extraction of petroleum") +
  v_07391("Special income tax on extraction of petroleum")
total_cash_2022_07391 <- v_07391("Distributed taxes, total")

col_07022_dec22 <- grep("2022M12 Distributed taxes, total", names(ssb_07022),
                        value = TRUE, fixed = TRUE)
stopifnot(length(col_07022_dec22) == 1)
total_cash_2022_07022 <- sum(as.numeric(ssb_07022[[col_07022_dec22]]), na.rm = TRUE)

# ============================================================================
# 3. Read World Bank WDI files (UTF-8 BOM, metadata lines before header)
# ============================================================================

read_wdi <- function(path, varname) {
  lines <- read_lines(path)
  hdr <- which(str_detect(lines, "^﻿?\"?Country Name"))[1]
  stopifnot(!is.na(hdr))
  read_csv(path, skip = hdr - 1, na = c("..", ""), show_col_types = FALSE,
           name_repair = "unique_quiet", progress = FALSE) |>
    filter(str_detect(`Country Code`, "^[A-Z]{3}$")) |>
    select(iso = `Country Code`, matches("^\\d{4}")) |>
    pivot_longer(-iso, names_to = "year", values_to = varname,
                 values_transform = as.numeric) |>
    mutate(year = as.integer(year)) |>
    filter(year %in% YEARS)
}

wdi_pop <- read_wdi("Data/Raw/wdi_population.csv", "pop")
wdi_inf <- read_wdi("Data/Raw/wdi_inflation.csv", "infl")
wdi_gpc <- read_wdi("Data/Raw/wdi_gdp_per_capita.csv", "gdppc_2015usd")

# CPI index from annual inflation rates, base 2015 = 100
wdi_cpi <- wdi_inf |>
  arrange(iso, year) |>
  group_by(iso) |>
  mutate(idx = cumprod(if_else(year == min(year), 1, 1 + infl / 100)),
         cpi_2015 = idx / idx[year == 2015] * 100) |>
  ungroup() |>
  select(iso, year, infl, cpi_2015)

wdi <- wdi_pop |>
  left_join(wdi_cpi, by = c("iso", "year")) |>
  left_join(wdi_gpc, by = c("iso", "year"))

# ============================================================================
# 4. Build panels (outcome definitions identical; mainland adjustment NOR only)
# ============================================================================

nor_adjust <- nor_ssb |>
  transmute(iso = "NOR", year,
            pet_a12     = a12 * 1e6,
            pet_a23     = a23 * 1e6,
            wealth_a113 = a113 * 1e6,
            rr_a112     = a112 * 1e6,
            gdp_ml      = gdp_ml_mill * 1e6,
            gdp_tot_ssb = gdp_tot_ssb_mill * 1e6)

build_panel <- function(wide) {
  wide |>
    left_join(nor_adjust, by = c("iso", "year")) |>
    left_join(wdi, by = c("iso", "year")) |>
    left_join(isl_4200, by = c("iso", "year")) |>
    left_join(oecd_1300, by = c("iso", "year")) |>
    mutate(
      gdp_oecd = xdc_T / (pct_T / 100),
      is_nor   = iso == "NOR",
      # individual net wealth tax, % of GDP: OECD 4210, Iceland from 4200
      pct_wealth_ind = if_else(iso == "ISL", coalesce(pct_4200_isl, 0), pct_4210),
      y_main = if_else(is_nor, (xdc_T - pet_a12) / gdp_ml * 100, pct_T),
      y_h2   = if_else(is_nor, (xdc_1100 + xdc_4210) / gdp_ml * 100,
                       pct_1100 + pct_wealth_ind),
      y_pit  = if_else(is_nor, xdc_1100 / gdp_ml * 100, pct_1100),
      y_cit  = if_else(is_nor, (xdc_1200 - pet_a12) / gdp_ml * 100, pct_1200),
      y_wealth    = pct_wealth_ind,
      y_wealth_ml = if_else(is_nor, xdc_4210 / gdp_ml * 100, NA_real_),
      y_main_grossgdp = pct_T,
      y_main_a23 = if_else(is_nor, (xdc_T - pet_a12 - pet_a23) / gdp_ml * 100,
                           pct_T),
      y_main_vintage = if_else(is_nor,
                               (xdc_T - pet_a12) /
                                 (gdp_oecd * (gdp_ml / gdp_tot_ssb)) * 100,
                               pct_T),
      y_main_pc = if_else(is_nor, xdc_T - pet_a12, xdc_T) / pop / (cpi_2015 / 100),
      y_main_lpc = log(y_main_pc),   # log real revenue per capita (Block 6)
      # control-side variant: OECD 1300 removed from every control series (zero for NOR)
      y_main_ex1300 = y_main - if_else(is_nor, 0, coalesce(pct_1300, 0)),
      treat = as.integer(iso == "NOR" & year >= 2022)
    ) |>
    select(iso, year, treat,
           y_main, y_h2, y_pit, y_cit, y_wealth, y_wealth_ml,
           y_main_grossgdp, y_main_a23, y_main_vintage, y_main_pc, y_main_lpc,
           y_main_ex1300,
           pct_T, pct_1100, pct_1200, pct_4210, pct_wealth_ind, pct_1300,
           xdc_T, xdc_1100, xdc_1200, xdc_4210,
           gdp_oecd, pet_a12, pet_a23, wealth_a113, rr_a112, gdp_ml, gdp_tot_ssb,
           pop, infl, cpi_2015, gdppc_2015usd) |>
    arrange(iso, year)
}

panel_nordics <- build_panel(wide_nord |> filter(iso %in% NORDICS))
panel_west    <- build_panel(wide_all |> filter(iso %in% c("NOR", WEST_DONORS)))

write_csv(panel_nordics, "Data/Processed/panel_nordics.csv")
write_csv(panel_west,    "Data/Processed/panel_west.csv")

# ============================================================================
# 5. Hard verification checks (all must PASS, ids 1.x)
# ============================================================================

nor <- panel_nordics |> filter(iso == "NOR")

# 1. Norway A12 anchors, NOK million, exact
a12_anchor <- c("2021" = 299222, "2022" = 882800, "2023" = 465111)
a12_obs <- setNames(nor_ssb$a12[match(as.integer(names(a12_anchor)), nor_ssb$year)],
                    names(a12_anchor))
check("1.1", desc = "Norway A12 2021/2022/2023 (NOK million, exact)",
      all(a12_obs == a12_anchor),
      paste(names(a12_obs), format(a12_obs, trim = TRUE), collapse = ", "))

# 1b. Norway A112 (bracket tax + ground rent) 2021/2022, NOK billion, +-0.3
a112_21 <- nor_ssb$a112[nor_ssb$year == 2021] / 1000
a112_22 <- nor_ssb$a112[nor_ssb$year == 2022] / 1000
check("1.1b", desc = "Norway A112 2021 ~ 99.0 / 2022 ~ 137.2 NOK bn (+-0.3)",
      abs(a112_21 - 99.0) <= 0.3 && abs(a112_22 - 137.2) <= 0.3,
      sprintf("%.1f / %.1f bn", a112_21, a112_22))

# 2. Norway Mainland GDP 2022, NOK million, exact
gdp_ml_2022 <- nor_ssb$gdp_ml_mill[nor_ssb$year == 2022]
check("1.2", desc = "Norway Mainland GDP 2022 = 3864878 NOK million (exact)",
      gdp_ml_2022 == 3864878, format(gdp_ml_2022))

# 3. Norway OECD _T 2022 in NOK billion, +-0.5
t_2022_bn <- nor$xdc_T[nor$year == 2022] / 1e9
check("1.3", desc = "Norway OECD _T 2022 = 2481.753 NOK bn (+-0.5)",
      abs(t_2022_bn - 2481.753) <= 0.5, sprintf("%.3f bn", t_2022_bn))

# 4. y_main Norway 2019-2024 anchors, +-0.05
ym_anchor <- c("2019" = 41.31, "2020" = 41.38, "2021" = 43.04,
               "2022" = 41.37, "2023" = 41.24, "2024" = 40.74)
ym_obs <- setNames(nor$y_main[match(as.integer(names(ym_anchor)), nor$year)],
                   names(ym_anchor))
check("1.4", desc = "y_main Norway 2019-2024 anchors (+-0.05)",
      all(abs(ym_obs - ym_anchor) <= 0.05),
      paste(names(ym_obs), sprintf("%.2f", ym_obs), collapse = ", "))

# 5. Basis consistency: sum(A1,A2,A3,A4) vs OECD _T for Norway, within 0.6%
basis <- nor_ssb |>
  mutate(ssb_sum = (a1 + a2 + a3 + a4) * 1e6) |>
  left_join(nor |> select(year, xdc_T), by = "year") |>
  mutate(rel_diff = abs(ssb_sum / xdc_T - 1))
check("1.5", desc = "sum(A1..A4) vs OECD _T Norway within 0.6% every year",
      all(basis$rel_diff < 0.006),
      sprintf("max rel diff %.3f%% (%d)", max(basis$rel_diff) * 100,
              basis$year[which.max(basis$rel_diff)]))

# 6. Cash crosscheck: 07391 Dec-2022 petroleum; 07022 region sum vs 07391
pass_6a <- abs(pet_cash_2022 - 713622) <= 1
pass_6b <- abs(total_cash_2022_07022 / total_cash_2022_07391 - 1) < 0.001
check("1.6", desc = "Cash crosscheck 07391 petroleum 2022 (+-1) & 07022 region sum (0.1%)",
      pass_6a && pass_6b,
      sprintf("petroleum %.0f; region sum %.0f vs 07391 %.0f (diff %.4f%%)",
              pet_cash_2022, total_cash_2022_07022, total_cash_2022_07391,
              abs(total_cash_2022_07022 / total_cash_2022_07391 - 1) * 100))

# 7. Nordics rows identical across the two OECD files
cmp <- full_join(
  oecd_nord |> filter(REF_AREA %in% NORDICS) |>
    select(REF_AREA, STANDARD_REVENUE, UNIT_MEASURE, TIME_PERIOD,
           v_nord = OBS_VALUE),
  oecd_all |> filter(REF_AREA %in% NORDICS) |>
    select(REF_AREA, STANDARD_REVENUE, UNIT_MEASURE, TIME_PERIOD,
           v_all = OBS_VALUE),
  by = c("REF_AREA", "STANDARD_REVENUE", "UNIT_MEASURE", "TIME_PERIOD"))
same_na  <- all(is.na(cmp$v_nord) == is.na(cmp$v_all))
max_diff <- max(abs(cmp$v_nord - cmp$v_all), na.rm = TRUE)
check("1.7", desc = "Nordics rows identical across OECD files (max abs diff < 0.001)",
      same_na && max_diff < 0.001,
      sprintf("%d matched rows, max abs diff %.6f", nrow(cmp), max_diff))

# 8. Panel dimensions and completeness (both panels)
west_out <- c("y_main", "y_h2", "y_pit")
check("1.8", desc = "panel_nordics: 75 rows (5x15), no NA in y_main, y_h2, treat; panel_west: 165 rows (11x15), no NA in y_main, y_h2, y_pit",
      nrow(panel_nordics) == 75 &&
        !anyNA(panel_nordics$y_main) &&
        !anyNA(panel_nordics$y_h2) &&
        !anyNA(panel_nordics$treat) &&
        nrow(panel_west) == 165 && n_distinct(panel_west$iso) == 11 &&
        !anyNA(panel_west[west_out]),
      sprintf("%d rows, NA counts y_main=%d y_h2=%d treat=%d; panel_west %d rows, %d countries, NA in outcomes %d",
              nrow(panel_nordics), sum(is.na(panel_nordics$y_main)),
              sum(is.na(panel_nordics$y_h2)), sum(is.na(panel_nordics$treat)),
              nrow(panel_west), n_distinct(panel_west$iso), sum(is.na(panel_west[west_out]))))

# 8b. Iceland 4200 patch applied to the H2 aggregate (2012: 0.502664 pp)
isl_2012 <- panel_nordics |> filter(iso == "ISL", year == 2012)
nor_chk  <- panel_nordics |> filter(iso == "NOR")
check("1.8b", desc = "Iceland H2 = PIT + 4200 wealth tax (2012: 0.502664); Norway y_h2 = PIT + 4210",
      abs(isl_2012$y_h2 - isl_2012$pct_1100 - 0.502664) < 1e-6 &&
        all(abs(nor_chk$y_wealth - nor_chk$pct_4210) < 1e-12),
      sprintf("ISL 2012 y_h2 - pct_1100 = %.6f", isl_2012$y_h2 - isl_2012$pct_1100))

# 8c. OECD 1300 joined and the ex-1300 control series built correctly
c13 <- panel_nordics |> select(iso, year, pct_1300, y_main, y_main_ex1300)
ex_expected <- c13$y_main - if_else(c13$iso == "NOR", 0, c13$pct_1300)
check("1.8c", desc = "OECD 1300: DNK 2022 0.394102, ISL 2010 1.629720, NOR/SWE/FIN zero; y_main_ex1300 = y_main - 1300 (controls)",
      !anyNA(c13$pct_1300) && !anyNA(c13$y_main_ex1300) &&
        abs(c13$pct_1300[c13$iso == "DNK" & c13$year == 2022] - 0.394102) < 1e-6 &&
        abs(c13$pct_1300[c13$iso == "ISL" & c13$year == 2010] - 1.629720) < 1e-6 &&
        all(c13$pct_1300[c13$iso %in% c("NOR", "SWE", "FIN")] == 0) &&
        all(abs(c13$y_main_ex1300 - ex_expected) < 1e-12),
      sprintf("DNK 2022 %.6f, ISL 2010 %.6f, DNK 2021 y_main %.3f -> ex1300 %.3f",
              c13$pct_1300[c13$iso == "DNK" & c13$year == 2022],
              c13$pct_1300[c13$iso == "ISL" & c13$year == 2010],
              c13$y_main[c13$iso == "DNK" & c13$year == 2021],
              c13$y_main_ex1300[c13$iso == "DNK" & c13$year == 2021]))

# 8d. OECD 1300 for the Western pool and the ex-1300 series for every donor
w13 <- panel_west |> select(iso, year, pct_1300, y_main, y_main_ex1300)
wx_expected <- w13$y_main - if_else(w13$iso == "NOR", 0, w13$pct_1300)
check("1.8d", desc = "OECD 1300 West: CHE 2022 1.023297, AUT 2022 0.304299, DNK 2022 0.394102, DEU/FRA/NLD/SWE/FIN/NOR zero; y_main_ex1300 complete for all 11 units",
      nrow(w13) == 165 && !anyNA(w13$pct_1300) && !anyNA(w13$y_main_ex1300) &&
        abs(w13$pct_1300[w13$iso == "CHE" & w13$year == 2022] - 1.023297) < 1e-6 &&
        abs(w13$pct_1300[w13$iso == "AUT" & w13$year == 2022] - 0.304299) < 1e-6 &&
        abs(w13$pct_1300[w13$iso == "DNK" & w13$year == 2022] - 0.394102) < 1e-6 &&
        all(w13$pct_1300[w13$iso %in% c("DEU", "FRA", "NLD", "SWE", "FIN", "NOR")] == 0) &&
        all(abs(w13$y_main_ex1300 - wx_expected) < 1e-12),
      sprintf("CHE 2022 %.6f, AUT 2022 %.6f, ISL 2022 %.6f, BEL 2022 %.6f; rows %d",
              w13$pct_1300[w13$iso == "CHE" & w13$year == 2022], w13$pct_1300[w13$iso == "AUT" & w13$year == 2022],
              w13$pct_1300[w13$iso == "ISL" & w13$year == 2022], w13$pct_1300[w13$iso == "BEL" & w13$year == 2022], nrow(w13)))

# ============================================================================
# 6. Summary in the log
# ============================================================================

chk1 <- check_log[seq_along(check_log) > n0]
log_add(strrep("-", 72))
log_add(sprintf("Checks passed: %d/%d", sum(grepl("[PASS]", chk1, fixed = TRUE)), length(chk1)))
log_add(sprintf("panel_nordics.csv : %d rows x %d cols (%s; %d-%d)",
                nrow(panel_nordics), ncol(panel_nordics),
                paste(sort(unique(panel_nordics$iso)), collapse = ","),
                min(panel_nordics$year), max(panel_nordics$year)))
log_add(sprintf("panel_west.csv    : %d rows x %d cols (%s; %d-%d)",
                nrow(panel_west), ncol(panel_west),
                paste(sort(unique(panel_west$iso)), collapse = ","),
                min(panel_west$year), max(panel_west$year)))
log_add("y_main by country (rounded to 2 decimals):")
ym_tab <- panel_nordics |>
  select(iso, year, y_main) |>
  mutate(y_main = round(y_main, 2)) |>
  pivot_wider(names_from = iso, values_from = y_main)
log_add(paste(capture.output(print(as.data.frame(ym_tab), row.names = FALSE)),
              collapse = "\n"))
log_add(sprintf("Norway y_wealth (OECD 4210, %% of GDP) 2019-2024: %s",
                paste(sprintf("%.2f", nor$y_wealth[nor$year >= 2019]),
                      collapse = " / ")))
log_add("")
stop_if_failed("Section 1")

# --- Panels for Sections 2-10 -------------------------------------------------
panel   <- read_csv("Data/Processed/panel_nordics.csv", show_col_types = FALSE)  # Norway + four Nordic controls
panel_o <- read_csv("Data/Processed/panel_west.csv",    show_col_types = FALSE)  # Norway + ten Western European donors
stopifnot(nrow(panel) == 75, nrow(panel_o) == 165,
          all(c("iso", "year", "treat", "y_main", "y_h2", "y_pit", "y_cit") %in% names(panel)))

##############################################################################
##############################################################################
# SECTION 2: DESCRIPTIVE STATISTICS (Figure A2.1-A2.4; Table A2.1, A2.2)
##############################################################################
##############################################################################

set.seed(SEED)
section("Section 2: Descriptive statistics")

CTRY <- c(NOR = "Norway", SWE = "Sweden", DNK = "Denmark",
          FIN = "Finland", ISL = "Iceland")
CTRY_WEST <- c(AUT = "Austria", BEL = "Belgium", CHE = "Switzerland",   # the six further donors of the
               DEU = "Germany", FRA = "France", NLD = "Netherlands")    # Western European group (Appendix A3)

panel_d <- panel |> mutate(country = CTRY[iso])   # with country names for figures and tables

# ============================================================================
# Figures
# ============================================================================

# Spread direct labels vertically so they do not overlap
spread_min_gap <- function(y, gap) {
  o <- order(y)
  ys <- y[o]
  if (length(ys) > 1) {
    for (i in 2:length(ys)) {
      if (ys[i] - ys[i - 1] < gap) ys[i] <- ys[i - 1] + gap
    }
  }
  y[o] <- ys
  y
}

# Multi-country trend figure: Norway black and thick, controls grey with
# direct labels at the right margin, dashed reform line between 2021 and 2022
plot_trends <- function(df, yvar, ylab) {
  df <- df |> select(iso, country, year, value = all_of(yvar))
  gap <- 0.055 * diff(range(df$value))   # minimum distance between the labels (figure height 3.4 in)
  ends <- df |>
    filter(year == 2024) |>
    mutate(lab_y = spread_min_gap(value, gap),
           col = if_else(iso == "NOR", "black", "grey40"),
           face = if_else(iso == "NOR", "bold", "plain"))
  ggplot(df, aes(year, value)) +
    geom_vline(xintercept = REFORM_X, linetype = "dashed",
               color = "grey40", linewidth = 0.4) +
    geom_line(data = df |> filter(iso != "NOR"), aes(group = iso),
              color = "grey65", linewidth = 0.5) +
    geom_line(data = df |> filter(iso == "NOR"),
              color = "black", linewidth = 1.1) +
    geom_text(data = ends, aes(x = 2024.3, y = lab_y, label = country),
              color = ends$col, fontface = ends$face,
              hjust = 0, size = 3.2) +
    scale_x_continuous(breaks = seq(2010, 2024, 2),
                       expand = expansion(mult = c(0.02, 0.16))) +
    labs(x = NULL, y = ylab) +
    thesis_theme
}

fig_y_main <- plot_trends(panel_d, "y_main",
                          "Tax revenue (% of GDP, mainland for Norway)")
save_fig(fig_y_main, "fig_y_main", width = 8, height = 3.4)

fig_y_h2 <- plot_trends(panel_d, "y_h2",
                        "PIT plus net wealth tax (% of GDP)")
save_fig(fig_y_h2, "fig_y_h2", width = 8, height = 3.4)

# Norway alone: net wealth tax revenue (OECD 4210), % of GDP
nor <- panel_d |> filter(iso == "NOR")
fig_wealth <- ggplot(nor, aes(year, y_wealth)) +
  geom_vline(xintercept = REFORM_X, linetype = "dashed",
             color = "grey40", linewidth = 0.4) +
  geom_line(color = "black", linewidth = 1.1) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = "% of GDP") +
  thesis_theme
save_fig(fig_wealth, "fig_wealth", width = 8, height = 3.2)

# Norway alone: OECD headline series vs mainland definition
SER_GROSS <- "Including petroleum / total GDP"
SER_ML    <- "Mainland definition"
nor_long <- nor |>
  select(year, y_main_grossgdp, y_main) |>
  pivot_longer(-year, names_to = "series") |>
  mutate(series = factor(if_else(series == "y_main", SER_ML, SER_GROSS),
                         levels = c(SER_GROSS, SER_ML)))
fig_headline_vs_mainland <- ggplot(nor_long, aes(year, value, color = series)) +
  geom_vline(xintercept = REFORM_X, linetype = "dashed",
             color = "grey40", linewidth = 0.4) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = setNames(c("grey55", "black"),
                                       c(SER_GROSS, SER_ML))) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(x = NULL, y = "Tax revenue (% of GDP)") +
  thesis_theme
save_fig(fig_headline_vs_mainland, "fig_headline_vs_mainland",
         width = 8, height = 3.4)

# Figure 4.1: both outcomes as deviations from the 2015-2020 mean, Norway
# black, Nordic controls grey, Western European pool as a band. Iceland's
# 2016 value is left out, as in the model.
DEV_LAB <- c(y_main = "(a) Mainland tax revenue (H1)", y_h2 = "(b) PIT plus net wealth tax (H2)")
dev_long <- panel_o |>
  mutate(country = coalesce(CTRY[iso], CTRY_WEST[iso])) |>
  select(iso, country, year, y_main, y_h2) |>
  pivot_longer(c(y_main, y_h2), names_to = "outcome") |>
  mutate(value = if_else(iso == "ISL" & year == 2016, NA_real_, value)) |>
  group_by(iso, outcome) |>
  mutate(dev = value - mean(value[year %in% 2015:2020], na.rm = TRUE)) |>
  ungroup() |>
  mutate(panel = factor(DEV_LAB[outcome], levels = DEV_LAB))
dev_band <- dev_long |> filter(iso %in% WEST_DONORS) |> group_by(panel, year) |>
  summarise(lo = min(dev, na.rm = TRUE), hi = max(dev, na.rm = TRUE), .groups = "drop")
dev_nor  <- dev_long |> filter(iso == "NOR")
dev_ctrl <- dev_long |> filter(iso %in% CONTROLS)
dev_span <- dev_long |> group_by(panel) |> summarise(span = diff(range(dev, na.rm = TRUE)), .groups = "drop")
dev_ends <- dev_long |> filter(iso %in% NORDICS, year == 2024) |> left_join(dev_span, by = "panel") |>
  group_by(panel) |> mutate(lab_y = spread_min_gap(dev, 0.07 * span[1])) |> ungroup() |>
  mutate(col = if_else(iso == "NOR", "black", "grey30"), face = if_else(iso == "NOR", "bold", "plain"))
BAND_LAB <- "Western European pool (10 donors), range"
fig_deviations <- ggplot() +
  geom_ribbon(data = dev_band, aes(x = year, ymin = lo, ymax = hi, fill = BAND_LAB), alpha = 0.25) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_vline(xintercept = REFORM_X, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_line(data = dev_ctrl, aes(year, dev, group = iso), color = "grey55", linewidth = 0.5) +
  geom_line(data = dev_nor, aes(year, dev), color = "black", linewidth = 1.1) +
  geom_text(data = dev_ends, aes(x = 2024.3, y = lab_y, label = country),
            color = dev_ends$col, fontface = dev_ends$face, hjust = 0, size = 3) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = "grey50") +
  scale_x_continuous(breaks = seq(2010, 2024, 2), expand = expansion(mult = c(0.02, 0.16))) +
  labs(x = NULL, y = "Deviation from the 2015-2020 mean (pp of GDP, mainland GDP for Norway)") +
  thesis_theme +
  theme(strip.text = element_text(hjust = 0, size = 10, face = "bold", color = "grey20"),
        panel.spacing.y = unit(12, "pt"))
save_fig(fig_deviations, "fig_deviations", width = 8, height = 7)

# ============================================================================
# Tables (CSV full precision + readable text version rounded to 2 decimals)
# ============================================================================

country_order <- c("Norway", "Denmark", "Finland", "Iceland", "Sweden", sort(unname(CTRY_WEST)))

# --- tab_summary: pre/post mean and SD per outcome and country --------------
tab_summary <- bind_rows(
    panel_d |> select(iso, country, year, y_main, y_h2, y_pit, y_cit),
    panel_o |> filter(iso %in% names(CTRY_WEST)) |>
      transmute(iso, country = CTRY_WEST[iso], year, y_main, y_h2, y_pit)) |>
  pivot_longer(c(y_main, y_h2, y_pit, y_cit), names_to = "outcome", values_drop_na = TRUE) |>
  mutate(group = if_else(iso %in% NORDICS, "Nordic", "Western Europe"),
         outcome = factor(outcome, levels = c("y_main", "y_h2", "y_pit", "y_cit")),
         country = factor(country, levels = country_order)) |>
  group_by(group, country, outcome) |>
  summarize(pre_mean   = mean(value[year <= 2021]),
            pre_sd     = sd(value[year <= 2021]),
            pre_mean_main = mean(value[year %in% 2015:2020]),   # pre-period of the main sample
            pre_sd_main   = sd(value[year %in% 2015:2020]),
            post_mean  = mean(value[year >= 2022]),
            post_sd    = sd(value[year >= 2022]),
            level_2021 = value[year == 2021],
            .groups = "drop") |>
  arrange(country, outcome)
save_table(tab_summary, "tab_summary")

# --- tab_wealth_stats: 08815 taxable net wealth and wealth tax, 2019-2024 ---
ssb_locale <- locale(encoding = "windows-1252", decimal_mark = ".")
ssb_08815 <- read_delim(paste0("Data/Raw/08815 Taxable wealth, debt and wealth ",
                               "tax for taxable persons with an amount, by tax ",
                               "components and year.csv"),
                        delim = ";", skip = 1, locale = ssb_locale,
                        na = c(".", "..", ":", "-", ""),
                        show_col_types = FALSE, progress = FALSE)

STATS_08815 <- c("Taxable persons with an amount"                    = "n_persons",
                 "Amount for taxable persons (NOK million)"          = "amount_mill_nok",
                 "Average among taxable persons with an amount (NOK)" = "avg_nok")

wealth_long <- ssb_08815 |>
  filter(.data[[names(ssb_08815)[1]]] %in% c("Taxable net wealth", "Wealth tax")) |>
  rename(component = 1) |>
  pivot_longer(-component, names_to = "col", values_to = "value") |>
  mutate(year = as.integer(str_sub(col, 1, 4)),
         statistic = str_trim(str_sub(col, 6))) |>
  filter(statistic %in% names(STATS_08815)) |>
  mutate(statistic = STATS_08815[statistic])

tab_wealth_stats <- wealth_long |>
  filter(year %in% 2019:2024) |>
  select(component, statistic, year, value) |>
  pivot_wider(names_from = year, values_from = value) |>
  arrange(component, match(statistic, STATS_08815))
save_table(tab_wealth_stats, "tab_wealth_stats", digits = 0)

# ============================================================================
# Log
# ============================================================================

cmp <- panel_d |>
  filter(year >= 2019) |>
  mutate(group = if_else(iso == "NOR", "nor", "ctrl")) |>
  group_by(year, group) |>
  summarize(y_main = mean(y_main), y_h2 = mean(y_h2), .groups = "drop") |>
  pivot_wider(names_from = group, values_from = c(y_main, y_h2)) |>
  select(year, y_main_NOR = y_main_nor, y_main_ctrl = y_main_ctrl,
         y_h2_NOR = y_h2_nor, y_h2_ctrl = y_h2_ctrl) |>
  mutate(across(-year, \(x) round(x, 2)))
log_add("Norway vs control mean (unweighted), 2019-2024:")
log_add(paste(capture.output(print(as.data.frame(cmp), row.names = FALSE)),
              collapse = "\n"))
# Standard deviation of the series 2022-2024 per unit (Ch. 3.3.1)
for (y in c("y_main", "y_h2")) {
  sdp <- tab_summary |> filter(outcome == y) |> arrange(post_sd)
  log_add(sprintf("SD of the series 2022-2024, %s, ascending: %s", y,
                  paste(sprintf("%s %.2f", sdp$country, sdp$post_sd), collapse = ", ")))
}

w_line <- function(comp, stat, label, fmt = "%.0f") {
  v <- wealth_long |>
    filter(component == comp, statistic == stat, year %in% 2021:2024) |>
    arrange(year)
  log_add(sprintf("08815 %s, %s 2021-2024: %s", comp, label,
                  paste(sprintf(fmt, v$value), collapse = " / ")))
}
w_line("Taxable net wealth", "n_persons", "persons")
w_line("Taxable net wealth", "avg_nok",   "average (NOK)")
w_line("Wealth tax",         "n_persons", "persons")
w_line("Wealth tax",         "avg_nok",   "average (NOK)")

log_add("Files written:")
for (f in out_files) log_add("  ", f)
log_add("")

##############################################################################
##############################################################################
# SECTION 3: TWFE DID NORDIC, WILD CLUSTER BOOTSTRAP, PLACEBOS, MDES (Table 4.1, A1.6)
##############################################################################
##############################################################################

# Samples (Sections 3-8):
#   main  : 2015-2024 without 2021, ISL2016 indicator (main specification)
#   full  : 2010-2024, indicators ISL2016 and DNK2014
#   noind : 2010-2024 without indicators (sample variant)
# MDES_a = (t_0.975 + t_0.80) x cluster SE, MDES_b = max |country placebo|.

set.seed(SEED)
section("Section 3: TWFE DiD, Nordic comparison group")

# Top-level data sets with fixed names: boottest re-evaluates fit$call$data,
# so the name in the call must resolve globally.
dat_main   <- prep_sample(panel, "main")
dat_full   <- prep_sample(panel, "full")
dat_noind  <- prep_sample(panel, "noind")
stopifnot(nrow(dat_main) == 45, nrow(dat_full) == 75, nrow(dat_noind) == 75,
          sum(dat_main$isl2016) == 1, sum(dat_main$dnk2014) == 0, sum(dat_full$dnk2014) == 1,
          !any(dat_main$year == 2021))

outcomes <- c("y_main", "y_h2", "y_pit", "y_cit")

# ---------------------------------------------------------------------------
# 1. TWFE per sample and outcome, bootstrap p for main and full
# ---------------------------------------------------------------------------

main_rows <- list()
for (s in names(SAMPLE_DEF)) for (y in outcomes) {
  fit <- fit_named(fml_of(y, s, d = get(paste0("dat_", s))), paste0("dat_", s))
  r   <- coef_row(fit)
  ew <- if (s %in% SAMPLES_MAIN) bt_p(run_boot(fit, "webb")) else list(p = NA_real_, B = NA_real_)
  main_rows[[paste(s, y)]] <- tibble(
    sample = s, outcome = y, coef = r$coef, se_cluster = r$se,
    p_cluster = r$p, p_webb = ew$p, B_webb = ew$B, n_obs = r$n, n_clusters = 5)
}
baseline <- bind_rows(main_rows)
stopifnot(nrow(baseline) == 12, !anyNA(baseline$coef),
          !anyNA(baseline$p_webb[baseline$sample %in% SAMPLES_MAIN]))

# Control values of the main estimates (regression test of the code)
bget <- function(s, y) baseline$coef[baseline$sample == s & baseline$outcome == y]
noind_ref <- c(y_main = 0.069, y_h2 = 0.339, y_pit = 0.112, y_cit = -0.090)
check("3.1", all(abs(sapply(names(noind_ref), \(y) bget("noind", y)) - noind_ref) < 0.002) &&
        abs(bget("full", "y_main") - (-0.282)) < 0.002 && abs(bget("main", "y_main") - 0.613) < 0.002,
      "TWFE control values: main 0.613, full -0.282, noind 0.069/0.339/0.112/-0.090 (tol 0.002)",
      sprintf("main %.3f, full %.3f, noind %s", bget("main", "y_main"), bget("full", "y_main"),
              paste(sprintf("%.3f", sapply(names(noind_ref), \(y) bget("noind", y))), collapse = "/")))
stop_if_failed("Section 3")

# ---------------------------------------------------------------------------
# 2. Country placebos (control g treated from 2022, NOR removed) per sample
#    -> MDES_b and Nordic rank p-values
# ---------------------------------------------------------------------------

placebo_country <- list()
for (s in names(SAMPLE_DEF)) {
  d0 <- get(paste0("dat_", s)) |> filter(iso != "NOR")
  for (g in CONTROLS) {
    d <- d0 |> mutate(treat_pl = as.integer(iso == g & year >= 2022),
                      iso_f = factor(iso), year_f = factor(year))
    for (y in outcomes) {
      r <- coef_row(feols(fml_of(y, s, "treat_pl", d), data = d, cluster = ~iso_f),
                    "treat_pl")
      placebo_country[[paste(s, g, y)]] <- tibble(
        type = "country", sample = s, placebo_unit = g, outcome = y,
        coef = r$coef, se_cluster = r$se, p_cluster = r$p, n_obs = r$n)
    }
  }
}
placebo_country <- bind_rows(placebo_country)
stopifnot(nrow(placebo_country) == 48)

mdes_tab <- placebo_country |>
  group_by(sample, outcome) |>
  summarise(mdes_b = max(abs(coef)), placebo_min = min(coef),
            placebo_max = max(coef), .groups = "drop")

rank_tab <- baseline |>
  select(sample, outcome, coef_nor = coef) |>
  left_join(placebo_country |> select(sample, outcome, pl = coef),
            by = c("sample", "outcome"), relationship = "many-to-many") |>
  group_by(sample, outcome, coef_nor) |>
  summarise(rank_nordic  = 1L + sum(abs(pl) >= abs(coef_nor)),
            p_rank_nordic = (1L + sum(abs(pl) >= abs(coef_nor))) / 5,
            p_loss_nordic = (1L + sum(pl <= coef_nor)) / 5,
            .groups = "drop") |>
  select(-coef_nor)

# ---------------------------------------------------------------------------
# 3. Timing placebos: pre-period only, fake treatment, all 5 countries
#    main: 2015-2020, fake from 2018; full/noind: 2010-2021, fake from 2016
# ---------------------------------------------------------------------------

timing_def <- list(main = list(last = 2020, from = 2018),
                   full = list(last = 2021, from = 2016),
                   noind = list(last = 2021, from = 2016))
placebo_timing <- list()
for (s in names(timing_def)) {
  td <- timing_def[[s]]
  d0 <- get(paste0("dat_", s)) |> filter(year <= td$last)
  for (g in c("NOR", CONTROLS)) {
    d <- d0 |> mutate(treat_pl = as.integer(iso == g & year >= td$from),
                      iso_f = factor(iso), year_f = factor(year))
    r <- coef_row(feols(fml_of("y_main", s, "treat_pl", d), data = d,
                        cluster = ~iso_f), "treat_pl")
    placebo_timing[[paste(s, g)]] <- tibble(
      type = "timing", sample = s, placebo_unit = g, outcome = "y_main",
      coef = r$coef, se_cluster = r$se, p_cluster = r$p, n_obs = r$n,
      note = sprintf("pre-period only (<= %d), fake treatment from %d", td$last, td$from))
  }
}
placebo_timing <- bind_rows(placebo_timing)

placebos <- bind_rows(placebo_country, placebo_timing)

# ---------------------------------------------------------------------------
# 4. Table 4.1 (Nordic rows): coefficients, bootstrap p, MDES_a/b, ranks
# ---------------------------------------------------------------------------

main_tab <- baseline |>
  left_join(mdes_tab, by = c("sample", "outcome")) |>
  left_join(rank_tab, by = c("sample", "outcome")) |>
  mutate(mdes_a = MDES_A_T4 * se_cluster,
         spec   = unname(sapply(sample, function(s) SAMPLE_DEF[[s]]$label))) |>
  select(sample, spec, outcome, coef, se_cluster, p_cluster,
         p_webb, B_webb, mdes_a, mdes_b,
         placebo_min, placebo_max, rank_nordic, p_rank_nordic, p_loss_nordic,
         n_obs, n_clusters) |>
  mutate(sample = factor(sample, levels = names(SAMPLE_DEF)),
         outcome = factor(outcome, levels = outcomes)) |>
  arrange(sample, outcome) |>
  mutate(sample = as.character(sample), outcome = as.character(outcome))

# ---------------------------------------------------------------------------
# Output: tables
# ---------------------------------------------------------------------------

write_csv(main_tab, "Output/tables/tab_main_did.csv")
write_csv(placebos, "Output/tables/tab_placebos.csv")

na_fmt <- function(x, d = 3) ifelse(is.na(x), "", fmt(x, d))
txt_main <- c(
  "tab_main_did -- TWFE DiD, FE iso + year, cluster iso (G=5)",
  "Samples: main = 2015-2024 excl. 2021 + ISL2016 indicator (Table 4.1);",
  "  full = 2010-2024 + ISL2016 and DNK2014 indicators (Table 4.1); noind = 2010-2024 without indicators (Tables A1.5, A1.6).",
  "Webb: 6-point wild cluster bootstrap, B = 9999, seed 1892.",
  "MDES_a = (t_0.975 + t_0.80, 4 df) x cluster SE = 3.72 x SE (80% power, 5% two-sided).",
  "MDES_b = max |country placebo| of the same sample and outcome (tab_placebos, type country).",
  "rank_nordic: rank of |coef| among the 4 country placebos (p = rank/5); p_loss: one-sided, placebos <= coef.",
  "",
  sprintf("%-7s %-7s %8s %8s %7s %7s %7s %7s %6s %6s %5s",
          "sample", "outcome", "coef", "se_cl", "p_clust", "p_webb",
          "MDES_a", "MDES_b", "rk_nor", "p_nor", "N"),
  apply(main_tab, 1, function(r) sprintf(
    "%-7s %-7s %8s %8s %7s %7s %7s %7s %6s %6s %5s",
    r[["sample"]], r[["outcome"]], fmt(as.numeric(r[["coef"]])),
    fmt(as.numeric(r[["se_cluster"]])), fmt(as.numeric(r[["p_cluster"]]), 4),
    na_fmt(as.numeric(r[["p_webb"]]), 4),
    fmt(as.numeric(r[["mdes_a"]])), fmt(as.numeric(r[["mdes_b"]])),
    r[["rank_nordic"]], fmt(as.numeric(r[["p_rank_nordic"]]), 2), r[["n_obs"]])))
writeLines(txt_main, "Output/tables/tab_main_did.txt")

txt_pl <- c(
  "tab_placebos -- placebo TWFE regressions (FE iso + year, cluster iso)",
  "country: treat assigned to control g from 2022, Norway excluded (G=4), per sample (main/full/noind).",
  "timing: pre-period only, fake treatment (main: 2015-2020 from 2018; full/noind: 2010-2021 from 2016).",
  "",
  sprintf("%-20s %-7s %-10s %-8s %9s %9s %8s %4s", "type", "sample", "unit",
          "outcome", "coef", "se_clust", "p_clust", "N"),
  apply(placebos, 1, function(r) sprintf(
    "%-20s %-7s %-10s %-8s %9s %9s %8s %4s", r[["type"]], r[["sample"]],
    r[["placebo_unit"]], r[["outcome"]], fmt(as.numeric(r[["coef"]])),
    fmt(as.numeric(r[["se_cluster"]])), fmt(as.numeric(r[["p_cluster"]]), 4),
    r[["n_obs"]])))
writeLines(txt_pl, "Output/tables/tab_placebos.txt")
# ---------------------------------------------------------------------------
# results_summary.txt
# ---------------------------------------------------------------------------

w("fwildclusterboot %s; Webb B=%d; MDES_a = 3.72 x cluster SE (t, 4 df)",
  as.character(utils::packageVersion("fwildclusterboot")), B_BOOT)
w("")
w("1) Table 4.1 (coef on treat, pp of GDP):")
for (i in seq_len(nrow(main_tab))) {
  b <- main_tab[i, ]
  w("   %-7s %-7s coef %7s  se %6s  p_clust %6s  p_webb %6s  MDES_a %5s  MDES_b %5s  rank %d/5 (p %.2f, p_loss %.2f)",
    b$sample, b$outcome, fmt(b$coef), fmt(b$se_cluster), fmt(b$p_cluster, 4),
    na_fmt(b$p_webb, 4), fmt(b$mdes_a, 2), fmt(b$mdes_b, 2),
    b$rank_nordic, b$p_rank_nordic, b$p_loss_nordic)
}
w("")
w("2) Country placebos (coef), by sample:")
pc_wide <- placebo_country |> select(sample, placebo_unit, outcome, coef) |>
  pivot_wider(names_from = outcome, values_from = coef)
for (i in seq_len(nrow(pc_wide))) {
  p <- pc_wide[i, ]
  w("   %-7s %-4s y_main %7s  y_h2 %7s  y_pit %7s  y_cit %7s", p$sample, p$placebo_unit,
    fmt(p$y_main), fmt(p$y_h2), fmt(p$y_pit), fmt(p$y_cit))
}
w("")
w("3) Timing placebos (y_main):")
for (i in seq_len(nrow(placebo_timing))) {
  p <- placebo_timing[i, ]
  w("   %-7s %-4s coef %7s  se %6s  p %6s  (%s)", p$sample, p$placebo_unit,
    fmt(p$coef), fmt(p$se_cluster), fmt(p$p_cluster, 4), p$note)
}
w("")
w("Files: tab_main_did, tab_placebos")
w("")

##############################################################################
##############################################################################
# SECTION 4: WESTERN EUROPEAN COMPARISON GROUP AND DESIGN-BASED INFERENCE (Table 4.1, A1.8, A1.9)
##############################################################################
##############################################################################

set.seed(SEED)
section("Section 4: Western European comparison group, design-based inference")

fml_tw <- function(y, treat, d) as.formula(paste0(y, " ~ ", paste(c(treat, ind_of(d)), collapse = " + "), " | iso_f + year_f"))
OUTC <- c("y_main", "y_h2", "y_pit")
POOLL <- c(nordic = "Nordic (4 donors)", west = "Western Europe (10 donors)")

complete_donors <- function(d_all, y) {
  n_years <- length(unique(d_all$year))
  d_all |> group_by(iso) |> summarise(ok = sum(!is.na(.data[[y]])) == n_years, .groups = "drop") |> filter(ok) |> pull(iso)
}
pool_donors <- function(pool, y, s) {
  d_all <- prep_sample(panel_o, s)
  comp <- complete_donors(d_all, y)
  cand <- switch(pool, nordic = NORDIC4, west = WEST_DONORS)
  sort(intersect(cand, comp))
}
pool_data <- function(pool, y, s) {
  donors <- pool_donors(pool, y, s)
  d <- prep_sample(panel_o, s) |> filter(iso %in% c(donors, "NOR")) |>
    mutate(iso_f = factor(iso), year_f = factor(year))
  list(d = d, donors = donors)
}
# Pre-period residual SD per unit (TWFE with indicators on the pre-period, all units of the pool incl. NOR)
unit_scales <- function(d, y, s) {
  pre <- d |> filter(year %in% SAMPLE_DEF[[s]]$pre) |> mutate(iso_f = factor(iso), year_f = factor(year))
  f <- feols(as.formula(paste0(y, " ~ ", ind_rhs(pre), " | iso_f + year_f")), data = pre)
  pre$res <- resid(f)
  pre |> filter(isl2016 == 0, dnk2014 == 0) |>                      # indicator observations have no residual
    group_by(iso) |> summarise(s = sd(res), n_pre = n(), .groups = "drop")
}

# ---------------------------------------------------------------------------
# 1.+2. TWFE and placebos per pool x sample x outcome
# ---------------------------------------------------------------------------
pool_rows <- list(); plac_rows <- list(); scale_rows <- list()
for (pool in names(POOLL)) for (s in SAMPLES_MAIN) for (y in OUTC) {
  pd <- pool_data(pool, y, s); d <- pd$d; donors <- pd$donors
  f_nor <- feols(fml_tw(y, "treat", d), data = d, cluster = ~iso_f)
  r_nor <- coef_row(f_nor)
  sc <- unit_scales(d, y, s)
  scale_rows[[paste(pool, s, y)]] <- sc |> mutate(pool = .env$pool, sample = .env$s, outcome = .env$y)
  pl <- bind_rows(lapply(donors, function(g) {
    dg <- d |> filter(iso != "NOR") |> mutate(treat_pl = as.integer(iso == g & year >= 2022))
    r <- coef_row(feols(fml_tw(y, "treat_pl", dg), data = dg, cluster = ~iso_f), "treat_pl")
    tibble(pool = pool, sample = s, outcome = y, unit = g, coef = r$coef, se_cluster = r$se, n_obs = r$n)
  })) |> left_join(sc |> select(unit = iso, s_pre = s), by = "unit") |>
    mutate(t_student = coef / s_pre, t_clse = coef / se_cluster)
  plac_rows[[paste(pool, s, y)]] <- pl
  s_nor <- sc$s[sc$iso == "NOR"]
  pool_rows[[paste(pool, s, y)]] <- tibble(
    pool = pool, sample = s, outcome = y, n_donors = length(donors), coef = r_nor$coef, se_cluster = r_nor$se,
    p_cluster = r_nor$p, n_obs = r_nor$n, s_pre_nor = s_nor, t_student = r_nor$coef / s_nor,
    rank_raw = 1L + sum(abs(pl$coef) >= abs(r_nor$coef)),
    rank_student = 1L + sum(abs(pl$t_student) >= abs(r_nor$coef / s_nor)),
    p_rank_raw = (1L + sum(abs(pl$coef) >= abs(r_nor$coef))) / (length(donors) + 1L),
    p_rank_student = (1L + sum(abs(pl$t_student) >= abs(r_nor$coef / s_nor))) / (length(donors) + 1L),
    p_loss_raw = (1L + sum(pl$coef <= r_nor$coef)) / (length(donors) + 1L),
    p_loss_student = (1L + sum(pl$t_student <= r_nor$coef / s_nor)) / (length(donors) + 1L),
    mdes_b_raw = max(abs(pl$coef)),
    mdes_a = (qt(0.975, length(donors)) + qt(0.80, length(donors))) * r_nor$se,   # G-1 = number of donors
    donors = paste(donors, collapse = " "))
}
tab_pools <- bind_rows(pool_rows); tab_plac <- bind_rows(plac_rows); tab_scales <- bind_rows(scale_rows)

# Reconciliation with Section 3
g2 <- function(s, y, col) main_tab[[col]][main_tab$sample == s & main_tab$outcome == y]
gp <- function(pool, s, y, col) tab_pools[[col]][tab_pools$pool == pool & tab_pools$sample == s & tab_pools$outcome == y]
check("4.5", abs(gp("nordic", "main", "y_main", "mdes_a") - g2("main", "y_main", "mdes_a")) < 1e-9 &&
        abs(gp("west", "main", "y_main", "mdes_a") / gp("west", "main", "y_main", "se_cluster") - (qt(0.975, 10) + qt(0.80, 10))) < 1e-9,
      "MDES_a uses G-1 df: Nordic (4 df) equals Section 3, West (10 df) = 3.11 x cluster SE",
      sprintf("West main y_main MDES_a %.3f (SE %.3f), factor %.3f", gp("west", "main", "y_main", "mdes_a"),
              gp("west", "main", "y_main", "se_cluster"), qt(0.975, 10) + qt(0.80, 10)))
check("4.1", abs(gp("nordic", "main", "y_main", "coef") - g2("main", "y_main", "coef")) < 1e-6 &&
        abs(gp("nordic", "full", "y_main", "coef") - g2("full", "y_main", "coef")) < 1e-6 &&
        abs(gp("west", "main", "y_main", "coef") - 0.177) < 0.002 && abs(gp("west", "full", "y_main", "coef") - (-0.695)) < 0.002,
      "Nordic TWFE = Section 3 (0.613 / -0.282); West (10 donors) = Table 4.1 (0.177 / -0.695)",
      sprintf("nordic %.3f/%.3f west %.3f/%.3f", gp("nordic", "main", "y_main", "coef"), gp("nordic", "full", "y_main", "coef"),
              gp("west", "main", "y_main", "coef"), gp("west", "full", "y_main", "coef")))
check("4.2", all(tab_pools$n_donors[tab_pools$pool == "west"] == 10) && all(tab_pools$n_donors[tab_pools$pool == "nordic"] == 4),
      "donor counts: West 10, Nordic 4",
      paste(tab_pools |> filter(sample == "main") |> transmute(k = paste(pool, outcome, n_donors)) |> pull(k), collapse = "; "))

# ---------------------------------------------------------------------------
# 3. Test inversion: confidence sets and non-inferiority p
#    H0: tau = tau0. Studentised statistic: (b_NOR - tau0)/s_NOR against b_g/s_g.
#    p_two = (1 + #|t_g| >= |t_NOR|)/(N+1); p_one (against a loss): (1 + #t_g >= t_NOR)/(N+1)
# ---------------------------------------------------------------------------
DELTA <- c(d1_wealth_tax = DELTA1, d2_pit_mech = DELTA2, mdes_1p75 = 1.75)   # 1.75 = MDES_b main y_main (Check 9.3)
GRID <- seq(-8, 8, by = 0.01)
delta_of <- function(y) if (y %in% c("y_main", "y_h2", "y_main_rrbound", "y_main_np", "y_main_npsym", "y_main_ex1300", "y_h2_relief")) DELTA[["d1_wealth_tax"]] else DELTA[["d2_pit_mech"]]
# Inversion of Norway's estimate b against the placebo statistics tg (sN = s_NOR studentised, 1 raw).
# Break-even: upward bias of b at which the rejection of a loss >= delta (10 %) is lost.
invert_one <- function(b, sN, tg, delta) {
  N <- length(tg)
  tN <- (b - GRID) / sN
  p_two <- sapply(tN, function(t) (1 + sum(abs(tg) >= abs(t))) / (N + 1))
  p_one <- sapply(tN, function(t) (1 + sum(tg >= t)) / (N + 1))
  cs <- function(alpha) { g <- GRID[p_two >= alpha]; if (length(g)) c(min(g), max(g)) else c(NA, NA) }
  lb <- function(alpha) { g <- GRID[p_one >= alpha]; if (length(g)) min(g) else NA }
  p_at <- function(dl) { t <- (b + dl) / sN; (1 + sum(tg >= t)) / (N + 1) }
  cs80 <- cs(0.20); cs90 <- cs(0.10); cs95 <- cs(0.05)
  tibble(n_donors = N, coef = b, p_min = 1 / (N + 1),
         rank = 1L + sum(abs(tg) >= abs(b / sN)),
         cs80_lo = cs80[1], cs80_hi = cs80[2], cs90_lo = cs90[1], cs90_hi = cs90[2], cs95_lo = cs95[1], cs95_hi = cs95[2],
         lb80 = lb(0.20), lb90 = lb(0.10), lb95 = lb(0.05),
         p_loss_d1 = p_at(DELTA[["d1_wealth_tax"]]), p_loss_d2 = p_at(DELTA[["d2_pit_mech"]]),
         p_loss_1p75 = p_at(DELTA[["mdes_1p75"]]), p_loss_0 = p_at(0),
         delta = delta, max_stat = max(tg), p_loss_delta = p_at(delta),
         breakeven = if (1 / (N + 1) < 0.10) b + delta - max(tg) * sN else NA_real_,   # only attainable with N >= 10
         rejected_10 = p_at(delta) < 0.10)
}
inv_rows <- list()
for (k in seq_len(nrow(tab_pools))) {
  pr <- tab_pools[k, ]; pl <- tab_plac |> filter(pool == pr$pool, sample == pr$sample, outcome == pr$outcome)
  for (method in c("student", "raw"))
    inv_rows[[paste(k, method)]] <- bind_cols(
      tibble(pool = pr$pool, sample = pr$sample, outcome = pr$outcome, method = method),
      invert_one(pr$coef, if (method == "student") pr$s_pre_nor else 1,
                 if (method == "student") pl$t_student else pl$coef, delta_of(pr$outcome)) |>
        mutate(s_pre_nor = pr$s_pre_nor, .after = coef))
}
tab_inference <- bind_rows(inv_rows)
stopifnot(with(tab_inference, all(is.na(breakeven) | ((breakeven > 0) == rejected_10))))

gi <- function(pool, s, y, m, col) tab_inference[[col]][tab_inference$pool == pool & tab_inference$sample == s & tab_inference$outcome == y & tab_inference$method == m]
check("4.3", abs(gi("west", "main", "y_main", "student", "p_min") - 1/11) < 1e-9 && gi("nordic", "main", "y_main", "student", "p_min") == 0.2 &&
        (is.na(gi("nordic", "main", "y_main", "student", "cs90_lo")) || gi("nordic", "main", "y_main", "student", "cs90_lo") <= -7.99),
      "p granularity: West 1/11, Nordic 1/5; 90% set unbounded on the Nordic pool",
      sprintf("West CS90 student [%.2f, %.2f] raw [%.2f, %.2f]; p(loss>=0.7) student %.3f raw %.3f", gi("west", "main", "y_main", "student", "cs90_lo"), gi("west", "main", "y_main", "student", "cs90_hi"),
              gi("west", "main", "y_main", "raw", "cs90_lo"), gi("west", "main", "y_main", "raw", "cs90_hi"), gi("west", "main", "y_main", "student", "p_loss_d1"), gi("west", "main", "y_main", "raw", "p_loss_d1")))

# ---------------------------------------------------------------------------
# 4. Wealth taxes of the Western European donors (Appendix A3)
# ---------------------------------------------------------------------------
tab_west_wealthtax <- panel_o |> filter(iso %in% c("FRA", "CHE")) |> group_by(iso) |>
  summarise(mean_2015_2017 = mean(pct_wealth_ind[year %in% 2015:2017]),
            mean_2018_2024 = mean(pct_wealth_ind[year %in% 2018:2024]),
            min_2010_2024 = min(pct_wealth_ind), max_2010_2024 = max(pct_wealth_ind), .groups = "drop")
gw <- function(iso, col) tab_west_wealthtax[[col]][tab_west_wealthtax$iso == iso]
check("4.4", abs(gw("FRA", "mean_2015_2017") - 0.224) < 0.002 && abs(gw("FRA", "mean_2018_2024") - 0.084) < 0.002,
      "France OECD 4210 in % of GDP, Appendix A3: 0.22 (2015-2017) -> 0.08 (2018-2024)",
      sprintf("FRA %.3f -> %.3f; CHE %.2f -> %.2f", gw("FRA", "mean_2015_2017"), gw("FRA", "mean_2018_2024"),
              gw("CHE", "mean_2015_2017"), gw("CHE", "mean_2018_2024")))

stop_if_failed("Section 4")
write_csv(tab_west_wealthtax, "Output/tables/tab_west_wealthtax.csv")
write_csv(tab_pools, "Output/tables/tab_pools.csv")
write_csv(tab_plac, "Output/tables/tab_placebos_pools.csv")
write_csv(tab_scales, "Output/tables/tab_scales.csv")
write_csv(tab_inference, "Output/tables/tab_inference.csv")

w("TWFE by pool (coef (cluster SE); studentised rank/N+1; 90%% CS student; 90%% CS raw (unbounded = [-8, 8]); p(loss>=d1=0.70) p(loss>=d2=0.27) p(loss>=1.75) student; break-even bias student/raw):")
for (k in seq_len(nrow(tab_pools))) {
  pr <- tab_pools[k, ]
  st <- tab_inference |> filter(pool == pr$pool, sample == pr$sample, outcome == pr$outcome, method == "student")
  rw <- tab_inference |> filter(pool == pr$pool, sample == pr$sample, outcome == pr$outcome, method == "raw")
  w("   %-6s %-4s %-6s %+6.3f (%.3f) s_NOR %.3f | rank %2d/%2d | CS90 st [%+.2f, %+.2f] lb90 %+.2f lb95 %+.2f | CS90 raw [%+.2f, %+.2f] | p d1 %.3f d2 %.3f 1.75 %.3f | MDES_a (G-1 df) %.2f | break-even (delta %.2f) st %+.3f raw %+.3f",
    pr$pool, pr$sample, pr$outcome, pr$coef, pr$se_cluster, pr$s_pre_nor, pr$rank_student, pr$n_donors + 1L,
    st$cs90_lo, st$cs90_hi, st$lb90, st$lb95, rw$cs90_lo, rw$cs90_hi, st$p_loss_d1, st$p_loss_d2, st$p_loss_1p75, pr$mdes_a,
    st$delta, st$breakeven, rw$breakeven)
}
w("Wealth taxes of the Western European donors (OECD 4210, %% of GDP; Appendix A3):")
for (k in seq_len(nrow(tab_west_wealthtax)))
  w("   %s  mean 2015-2017 %.2f  mean 2018-2024 %.2f  range 2010-2024 %.2f to %.2f", tab_west_wealthtax$iso[k],
    tab_west_wealthtax$mean_2015_2017[k], tab_west_wealthtax$mean_2018_2024[k],
    tab_west_wealthtax$min_2010_2024[k], tab_west_wealthtax$max_2010_2024[k])
w("Files: tab_pools, tab_placebos_pools, tab_scales, tab_inference, tab_west_wealthtax")
w("")

##############################################################################
##############################################################################
# SECTION 5: EVENT STUDY (BJS IMPUTATION), PRE-TREND TESTS, PLACEBO BANDS (Table A1.3, A1.10; Figure 4.3)
##############################################################################
##############################################################################

# Samples main (reference year 2020) and full (reference year 2021).
# 1. Year effects 2022-2024 by imputation (didimputation), check A: manual imputation.
# 2. Pre-period coefficients, joint Wald test with HC1, linear drift (check B: fixest).
# 3. Placebo bands for the year effects, Norway against both pools.
# 4. The same event study on the Western European pool.

set.seed(SEED)
section("Section 5: Event study and placebo bands")

stopifnot(exists("dat_main"), exists("dat_full"), exists("SAMPLE_DEF"))
for (s in SAMPLES_MAIN) {
  d <- get(paste0("dat_", s))
  d$g     <- ifelse(d$iso == "NOR", 2022, 0)
  d$nyear <- ifelse(d$iso == "NOR", d$year, 0)
  assign(paste0("dat_", s), d)
}

wald_hc1 <- function(fit, pat = "^nyear::") {
  idx <- grep(pat, names(coef(fit)))
  b   <- coef(fit)[idx]
  V   <- vcov(fit, vcov = "hetero")[idx, idx]
  stat <- as.numeric(t(b) %*% solve(V) %*% b)
  list(stat = stat, df = length(b), p = pchisq(stat, df = length(b), lower.tail = FALSE))
}

ev_rows  <- list()
ev_lines <- character(0)
eadd <- function(...) ev_lines <<- c(ev_lines, sprintf(...))

for (s in SAMPLES_MAIN) {
  d      <- get(paste0("dat_", s))
  sd     <- SAMPLE_DEF[[s]]
  untr   <- d |> filter(!(iso == "NOR" & year >= 2022))
  pre    <- d |> filter(year %in% sd$pre)
  pre$nor_trend <- as.integer(pre$iso == "NOR") * (pre$year - min(sd$pre))
  n_pre  <- length(sd$pre) - 1L           # pre-period coefficients (without the reference year)

  for (y in outcomes) {
    res <- did_imputation(data = d, yname = y, gname = "g", tname = "year",
                          idname = "iso", first_stage = as.formula(paste0("~ ", ind_rhs(d), " | iso + year")),
                          horizon = TRUE, pretrends = TRUE, cluster_var = "iso")
    res <- res |> mutate(et = as.numeric(as.character(term)), year = 2022 + et) |>
      arrange(year)
    post_r <- res |> filter(et >= 0)
    pre_r  <- res |> filter(et < 0)
    stopifnot(nrow(post_r) == 3, nrow(pre_r) == n_pre,
              all(is.finite(post_r$std.error)), all(is.finite(pre_r$std.error)))

    # Check A: imputation = manual imputation with a dummy
    f_man <- lm(as.formula(paste0(y, " ~ factor(iso) + factor(year) + ", ind_rhs(untr))), data = untr)
    np    <- d |> filter(iso == "NOR", year >= 2022) |> arrange(year)
    tau_m <- np[[y]] - predict(f_man, newdata = np)
    stopifnot(max(abs(post_r$estimate - tau_m)) < 1e-8)

    # fixest replication of the pre-period regression (reference = last pre-period year)
    f_pre <- feols(as.formula(paste0(
      y, " ~ ", ind_rhs(untr), " + i(nyear, ref = c(0, ", sd$ref_year, ")) | iso_f + year_f")),
      data = untr, cluster = ~iso_f)
    cb <- coef(f_pre)[grep("^nyear::", names(coef(f_pre)))]
    stopifnot(length(cb) == n_pre,
              max(abs(sort(as.numeric(cb)) - sort(pre_r$estimate))) < 1e-6)
    w_pre <- wald_hc1(f_pre)

    # linear drift in the pre-period
    f_dr  <- feols(as.formula(paste0(y, " ~ nor_trend + ", ind_rhs(pre), " | iso_f + year_f")),
                   data = pre, cluster = ~iso_f)
    ct_dr <- coeftable(f_dr)["nor_trend", ]
    ci_dr <- confint(f_dr)["nor_trend", ]

    ev_rows[[paste(s, y)]] <- bind_rows(
      tibble(pool = "nordic", sample = s, outcome = y, component = "pre", year = pre_r$year,
             estimate = pre_r$estimate, std_error = pre_r$std.error,
             ci_lo = pre_r$conf.low, ci_hi = pre_r$conf.high,
             stat = NA_real_, df = NA_real_, p_value = NA_real_,
             note = sprintf("BJS pre coef vs %d, untreated obs, indicators %s, SE cluster iso", sd$ref_year, ind_rhs(d))),
      tibble(pool = "nordic", sample = s, outcome = y, component = "post", year = post_r$year,
             estimate = post_r$estimate, std_error = post_r$std.error,
             ci_lo = post_r$conf.low, ci_hi = post_r$conf.high,
             stat = NA_real_, df = NA_real_, p_value = NA_real_,
             note = sprintf("imputation effect, indicators %s in first stage, SE cluster iso (didimputation)", ind_rhs(d))),
      tibble(pool = "nordic", sample = s, outcome = y, component = "pretrend_wald_hc1", year = NA_real_,
             estimate = NA_real_, std_error = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
             stat = w_pre$stat, df = w_pre$df, p_value = w_pre$p,
             note = "joint Wald all pre = 0, HC1 vcov (cluster-robust infeasible at G=5), chi2; over-rejects"),
      tibble(pool = "nordic", sample = s, outcome = y, component = "drift", year = NA_real_,
             estimate = ct_dr[["Estimate"]], std_error = ct_dr[["Std. Error"]],
             ci_lo = ci_dr[[1]], ci_hi = ci_dr[[2]],
             stat = NA_real_, df = NA_real_, p_value = ct_dr[["Pr(>|t|)"]],
             note = sprintf("NOR x (year-%d) trend, %d-%d, TWFE + indicators, cluster iso",
                            min(sd$pre), min(sd$pre), max(sd$pre))))

    eadd("   nordic %-5s %-7s 2022 %+6.3f [%+.2f, %+.2f]  2023 %+6.3f [%+.2f, %+.2f]  2024 %+6.3f [%+.2f, %+.2f] | avg %+.3f | drift %+.3f (p %.3f) | Wald-HC1 p %.4f",
         s, y, post_r$estimate[1], post_r$conf.low[1], post_r$conf.high[1],
         post_r$estimate[2], post_r$conf.low[2], post_r$conf.high[2],
         post_r$estimate[3], post_r$conf.low[3], post_r$conf.high[3],
         mean(post_r$estimate), ct_dr[["Estimate"]], ct_dr[["Pr(>|t|)"]], w_pre$p)
  }
}
# Western European pool (Table A1.3, Panel C): the same event study, outcomes y_main, y_h2, y_pit
for (s in SAMPLES_MAIN) for (y in OUTC) {
  sd   <- SAMPLE_DEF[[s]]
  d    <- pool_data("west", y, s)$d
  d$g     <- ifelse(d$iso == "NOR", 2022, 0)
  d$nyear <- ifelse(d$iso == "NOR", d$year, 0)
  untr <- d |> filter(!(iso == "NOR" & year >= 2022))
  pre  <- d |> filter(year %in% sd$pre)
  pre$nor_trend <- as.integer(pre$iso == "NOR") * (pre$year - min(sd$pre))
  n_pre <- length(sd$pre) - 1L
  res <- did_imputation(data = d, yname = y, gname = "g", tname = "year", idname = "iso",
                        first_stage = as.formula(paste0("~ ", ind_rhs(d), " | iso + year")),
                        horizon = TRUE, pretrends = TRUE, cluster_var = "iso") |>
    mutate(et = as.numeric(as.character(term)), year = 2022 + et) |> arrange(year)
  post_r <- res |> filter(et >= 0); pre_r <- res |> filter(et < 0)
  stopifnot(nrow(post_r) == 3, nrow(pre_r) == n_pre)
  f_pre <- feols(as.formula(paste0(y, " ~ ", ind_rhs(untr), " + i(nyear, ref = c(0, ", sd$ref_year, ")) | iso_f + year_f")),
                 data = untr, cluster = ~iso_f)
  cb <- coef(f_pre)[grep("^nyear::", names(coef(f_pre)))]
  stopifnot(length(cb) == n_pre, max(abs(sort(as.numeric(cb)) - sort(pre_r$estimate))) < 1e-6)
  w_pre <- wald_hc1(f_pre)
  f_dr  <- feols(as.formula(paste0(y, " ~ nor_trend + ", ind_rhs(pre), " | iso_f + year_f")), data = pre, cluster = ~iso_f)
  ct_dr <- coeftable(f_dr)["nor_trend", ]; ci_dr <- confint(f_dr)["nor_trend", ]
  ev_rows[[paste("west", s, y)]] <- bind_rows(
    tibble(pool = "west", sample = s, outcome = y, component = "pre", year = pre_r$year,
           estimate = pre_r$estimate, std_error = pre_r$std.error, ci_lo = pre_r$conf.low, ci_hi = pre_r$conf.high,
           stat = NA_real_, df = NA_real_, p_value = NA_real_,
           note = sprintf("BJS pre coef vs %d, West pool (10 donors), indicators %s, SE cluster iso", sd$ref_year, ind_rhs(d))),
    tibble(pool = "west", sample = s, outcome = y, component = "post", year = post_r$year,
           estimate = post_r$estimate, std_error = post_r$std.error, ci_lo = post_r$conf.low, ci_hi = post_r$conf.high,
           stat = NA_real_, df = NA_real_, p_value = NA_real_,
           note = sprintf("imputation effect, West pool, indicators %s in first stage, SE cluster iso (didimputation)", ind_rhs(d))),
    tibble(pool = "west", sample = s, outcome = y, component = "pretrend_wald_hc1", year = NA_real_,
           estimate = NA_real_, std_error = NA_real_, ci_lo = NA_real_, ci_hi = NA_real_,
           stat = w_pre$stat, df = w_pre$df, p_value = w_pre$p,
           note = "joint Wald all pre = 0, HC1 vcov, chi2; West pool; over-rejects"),
    tibble(pool = "west", sample = s, outcome = y, component = "drift", year = NA_real_,
           estimate = ct_dr[["Estimate"]], std_error = ct_dr[["Std. Error"]], ci_lo = ci_dr[[1]], ci_hi = ci_dr[[2]],
           stat = NA_real_, df = NA_real_, p_value = ct_dr[["Pr(>|t|)"]],
           note = sprintf("NOR x (year-%d) trend, %d-%d, West pool, TWFE + indicators, cluster iso", min(sd$pre), min(sd$pre), max(sd$pre))))
  eadd("   west   %-5s %-7s 2022 %+6.3f [%+.2f, %+.2f]  2023 %+6.3f [%+.2f, %+.2f]  2024 %+6.3f [%+.2f, %+.2f] | avg %+.3f | drift %+.3f (p %.3f) | Wald-HC1 p %.4f | pre: %s",
       s, y, post_r$estimate[1], post_r$conf.low[1], post_r$conf.high[1],
       post_r$estimate[2], post_r$conf.low[2], post_r$conf.high[2],
       post_r$estimate[3], post_r$conf.low[3], post_r$conf.high[3],
       mean(post_r$estimate), ct_dr[["Estimate"]], ct_dr[["Pr(>|t|)"]], w_pre$p,
       paste(sprintf("%d %+.2f", as.integer(pre_r$year), pre_r$estimate), collapse = " "))
}
tab_event <- bind_rows(ev_rows)

# Check C: mean of the year effects = TWFE coefficient of the same sample (Nordic: Section 3)
for (s in SAMPLES_MAIN) for (y in outcomes) {
  avg <- mean(tab_event$estimate[tab_event$pool == "nordic" & tab_event$sample == s & tab_event$outcome == y &
                                   tab_event$component == "post"])
  stopifnot(abs(avg - bget(s, y)) < 1e-6)
}
# Check 5.2: the year effects of the Western pool average to the TWFE coefficient of the Western pool (Section 4)
ev_w <- tab_event |> filter(pool == "west", component == "post") |> group_by(sample, outcome) |>
  summarise(avg = mean(estimate), .groups = "drop") |>
  inner_join(tab_pools |> filter(pool == "west") |> select(sample, outcome, coef), by = c("sample", "outcome"))
check("5.2", nrow(ev_w) == 6 && max(abs(ev_w$avg - ev_w$coef)) < 1e-6,
      "West pool: mean of the imputation year effects = TWFE coefficient of Section 4 (main and full, 3 outcomes)",
      sprintf("max diff %.2e; main y_main avg %+.3f, y_h2 %+.3f", max(abs(ev_w$avg - ev_w$coef)),
              ev_w$avg[ev_w$sample == "main" & ev_w$outcome == "y_main"], ev_w$avg[ev_w$sample == "main" & ev_w$outcome == "y_h2"]))

# ---------------------------------------------------------------------------
# Output
# ---------------------------------------------------------------------------

write_csv(tab_event, "Output/tables/tab_event.csv")
nn <- function(v, d = 3) ifelse(is.na(v), "", fmt(as.numeric(v), d))
txt <- c(
  "tab_event -- BJS imputation estimator (didimputation) + pre-trend tests; pool nordic (4 donors) and west (10 donors)",
  "Samples: main = 2015-2024 excl. 2021 (reference 2020), ISL2016 indicator; full = 2010-2024 (reference 2021), ISL2016 and DNK2014 indicators.",
  "Post effects: first stage iso + year FE + indicators on untreated obs, Norway imputed; SE cluster iso; 95% CI normal.",
  "Pre coefficients: Norway vs reference year from untreated obs (BJS separate pre-trend regression).",
  "Joint Wald (pre = 0): HC1 vcov, chi2; cluster-robust joint test infeasible at G=5; known to over-reject.",
  "Drift: NOR x (year - first pre-year) over the sample's pre-period, TWFE + indicators, cluster iso (t(G-1) CI).",
  "",
  sprintf("%-6s %-5s %-7s %-18s %5s %9s %8s %9s %9s %8s %3s %8s", "pool", "sample", "outcome", "component",
          "year", "estimate", "se", "ci_lo", "ci_hi", "stat", "df", "p"),
  apply(tab_event, 1, function(r) sprintf(
    "%-6s %-5s %-7s %-18s %5s %9s %8s %9s %9s %8s %3s %8s", r[["pool"]], r[["sample"]], r[["outcome"]],
    r[["component"]], ifelse(is.na(r[["year"]]), "", as.character(as.integer(r[["year"]]))),
    nn(r[["estimate"]]), nn(r[["std_error"]]), nn(r[["ci_lo"]]), nn(r[["ci_hi"]]),
    nn(r[["stat"]], 2), ifelse(is.na(r[["df"]]), "", as.character(as.integer(r[["df"]]))),
    nn(r[["p_value"]], 4))))
writeLines(txt, "Output/tables/tab_event.txt")


w("BJS imputation (didimputation %s), first stage iso + year FE + indicators;",
  as.character(utils::packageVersion("didimputation")))
w("main: reference 2020, pre 2015-2020; full: reference 2021, pre 2010-2021; pools nordic and west.")
w("Year effects (pp of GDP, 95%% CI), average, drift, Wald-HC1 p:")
for (l in ev_lines) log_add(l)
w("Files: tab_event")
w("")

# ---------------------------------------------------------------------------
# 3. Placebo bands for the year effects (main sample)
# ---------------------------------------------------------------------------
# TWFE on the untreated observations (unit g without its post-period years), effects = deviation from the prediction
event_unit <- function(d, y, g, ref_year) {
  d <- d |> mutate(iso_f = factor(iso), year_f = factor(year))
  untr <- d |> filter(!(iso == g & year >= 2022))
  f <- feols(as.formula(paste0(y, " ~ ", ind_rhs(untr), " | iso_f + year_f")), data = untr)
  pre <- untr |> filter(iso == g) |> mutate(estimate = resid(f)[untr$iso == g]) |> select(year, estimate)
  post <- d |> filter(iso == g, year >= 2022)
  post$estimate <- post[[y]] - predict(f, newdata = post)
  bind_rows(pre, post |> select(year, estimate)) |> mutate(unit = g, year = as.integer(year)) |> arrange(year)
}
band_rows <- list(); nor_ev_rows <- list()
for (pool in c("nordic", "west")) for (y in OUTC) {
  s <- "main"; pd <- pool_data(pool, y, s); d <- pd$d; donors <- pd$donors
  sc <- tab_scales |> filter(pool == .env$pool, sample == .env$s, outcome == .env$y)
  ev_nor <- event_unit(d, y, "NOR", SAMPLE_DEF[[s]]$ref_year) |> mutate(s_pre = sc$s[sc$iso == "NOR"])
  ev_pl <- bind_rows(lapply(donors, function(g)
    event_unit(d |> filter(iso != "NOR"), y, g, SAMPLE_DEF[[s]]$ref_year))) |>
    left_join(sc |> select(unit = iso, s_pre = s), by = "unit") |> mutate(t = estimate / s_pre)
  s_nor <- sc$s[sc$iso == "NOR"]
  bands <- ev_pl |> group_by(year) |>
    summarise(n = n(), raw_min = min(estimate), raw_max = max(estimate),
              st_min = min(t) * s_nor, st_max = max(t) * s_nor, .groups = "drop") |>
    mutate(pool = pool, outcome = y)
  band_rows[[paste(pool, y)]] <- bands
  nor_ev_rows[[paste(pool, y)]] <- ev_nor |> mutate(outcome = y, pool = pool)
}
tab_bands <- bind_rows(band_rows); nor_ev <- bind_rows(nor_ev_rows)
# Check: Norway's year effects (both pools) = imputation effects above
ev_post <- tab_event |> filter(sample == "main", component == "post")
chk <- nor_ev |> filter(year >= 2022) |> inner_join(ev_post |> select(pool, outcome, year, est_imp = estimate), by = c("pool", "outcome", "year"))
check("5.1", nrow(chk) == 18 && max(abs(chk$estimate - chk$est_imp)) < 1e-6,
      "Norway year effects (NOR x year TWFE) = imputation effects above, Nordic and West pool (single treated unit)",
      sprintf("max diff %.2e", max(abs(chk$estimate - chk$est_imp))))
stop_if_failed("Section 5")

plot_bands <- function(y, ylab) {
  ev <- nor_ev |> filter(outcome == y, pool == "nordic") |> mutate(period = ifelse(year >= 2022, "Treatment effect", "Pre-period residual"))
  bo <- tab_bands |> filter(outcome == y, pool == "west")
  bn <- tab_bands |> filter(outcome == y, pool == "nordic")
  ggplot() +
    geom_ribbon(data = bo, aes(x = year, ymin = st_min, ymax = st_max, fill = "Western European placebos (10 donors), range (studentised)"), alpha = 0.25) +
    geom_linerange(data = bn, aes(x = year, ymin = st_min, ymax = st_max, color = "Nordic placebos, range (studentised)"), linewidth = 4, alpha = 0.35) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = REFORM_X, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_point(data = ev, aes(year, estimate, shape = period), size = 2.4) +
    scale_fill_manual(values = "grey50") + scale_color_manual(values = "steelblue") +
    scale_shape_manual(values = c("Pre-period residual" = 1, "Treatment effect" = 16)) +
    scale_x_continuous(breaks = 2015:2024) + labs(x = NULL, y = ylab) + thesis_theme +
    theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0)) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2), shape = guide_legend(order = 3, nrow = 1))
}
save_fig(plot_bands("y_main", "Effect on tax revenue (pp of GDP, mainland for Norway)"), "fig_event_main_bands")
save_fig(plot_bands("y_h2", "Effect on PIT plus net wealth tax (pp of GDP)"), "fig_event_h2_bands")

# Figure 4.3: panel (a) y_main, panel (b) y_h2
plot_bands_both <- function() {
  lab <- c(y_main = "(a) Mainland tax revenue (H1)", y_h2 = "(b) PIT plus net wealth tax (H2)")
  SER <- c(nordic = "Norway against the Nordic controls", west = "Norway against the Western European pool")
  ev <- nor_ev |> filter(outcome %in% names(lab)) |>
    mutate(period = ifelse(year >= 2022, "treatment effect", "pre-period residual"),
           key = factor(paste0(unname(SER[pool]), ", ", period),
                        levels = c(paste0(SER["nordic"], ", pre-period residual"), paste0(SER["nordic"], ", treatment effect"),
                                   paste0(SER["west"], ", pre-period residual"), paste0(SER["west"], ", treatment effect"))),
           x = year + ifelse(pool == "nordic", -0.12, 0.12),
           panel = factor(unname(lab[outcome]), levels = unname(lab)))
  bands <- tab_bands |> filter(outcome %in% names(lab)) |> mutate(panel = factor(unname(lab[outcome]), levels = unname(lab)))
  bo <- bands |> filter(pool == "west")
  bn <- bands |> filter(pool == "nordic")
  ggplot() +
    geom_ribbon(data = bo, aes(x = year, ymin = st_min, ymax = st_max, fill = "Western European placebos (10 donors), range (studentised)"), alpha = 0.25) +
    geom_linerange(data = bn, aes(x = year, ymin = st_min, ymax = st_max, color = "Nordic placebos, range (studentised)"), linewidth = 4, alpha = 0.35) +
    geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
    geom_vline(xintercept = REFORM_X, linetype = "dashed", color = "grey40", linewidth = 0.4) +
    geom_point(data = ev, aes(x, estimate, shape = key), size = 2.4) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_fill_manual(values = "grey50") + scale_color_manual(values = "steelblue") +
    scale_shape_manual(values = setNames(c(1, 16, 2, 17), levels(ev$key))) +
    scale_x_continuous(breaks = 2015:2024) + labs(x = NULL, y = "Effect (pp of GDP, mainland GDP for Norway)") + thesis_theme +
    theme(legend.box = "vertical", legend.spacing.y = unit(0, "pt"), legend.margin = margin(0, 0, 0, 0),
          strip.text = element_text(hjust = 0, size = 10, face = "bold", color = "grey20"),
          panel.spacing.y = unit(12, "pt")) +
    guides(fill = guide_legend(order = 1), color = guide_legend(order = 2), shape = guide_legend(order = 3, nrow = 2))
}
save_fig(plot_bands_both(), "fig_event_bands_both", width = 8, height = 7.8)

write_csv(tab_bands, "Output/tables/tab_event_bands.csv")
write_csv(nor_ev, "Output/tables/tab_event_nor.csv")
w("Year-effect bands (main, y_main): year | NOR vs Nordic | Nordic range (st) | NOR vs West | West range (st) | West range (raw)")
for (yr in sort(unique(nor_ev$year[nor_ev$outcome == "y_main"]))) {
  e <- nor_ev |> filter(outcome == "y_main", pool == "nordic", year == yr); ew <- nor_ev |> filter(outcome == "y_main", pool == "west", year == yr)
  bn <- tab_bands |> filter(outcome == "y_main", pool == "nordic", year == yr)
  bo <- tab_bands |> filter(outcome == "y_main", pool == "west", year == yr)
  w("   %d %+6.3f | [%+.2f, %+.2f] | %+6.3f | [%+.2f, %+.2f] | [%+.2f, %+.2f]", yr, e$estimate, bn$st_min, bn$st_max, ew$estimate, bo$st_min, bo$st_max, bo$raw_min, bo$raw_max)
}
w("Files: tab_event_bands, tab_event_nor, fig_event_bands_both (Figure 4.3), fig_event_main_bands, fig_event_h2_bands")
w("")

##############################################################################
##############################################################################
# SECTION 6: SYNTHETIC DID ON BOTH COMPARISON GROUPS (Table A1.7; Figure A2.5)
##############################################################################
##############################################################################

# Panel variants per outcome:
#   full   : 2010-2024 (T0 = 12)              -> sdid, sc, did
#   ex2021 : 2010-2024 without 2021 (T0 = 11) -> sdid
#   main   : 2015-2024 without 2021 (T0 = 6)  -> sdid
# SE: placebo method (200 replications), CI = est +- 1.96 SE. synthdid runs without indicators.

set.seed(SEED)
section("Section 6: Synthetic DiD")

donor_sets <- list(nordic = list(y_main = NORDIC4, y_h2 = NORDIC4))
VARIANTS <- list(
  full   = list(keep = function(y) rep(TRUE, length(y)),   T0 = 12, label = "2010-2024"),
  ex2021 = list(keep = function(y) y != 2021,              T0 = 11, label = "2010-2024 excl. 2021"),
  main   = list(keep = function(y) y >= 2015 & y != 2021,  T0 = 6,  label = "2015-2024 excl. 2021 (main sample)"))

# Check 6.1: panel structure of the SDID input (Norway + four Nordic donors)
mis <- panel_o |> filter(iso %in% c(NORDIC4, "NOR")) |> group_by(iso) |>
  summarise(n = n(), na_m = sum(is.na(y_main)), na_h = sum(is.na(y_h2)), .groups = "drop")
check("6.1", nrow(mis) == 5 && all(mis$n == 15) && all(mis$na_m == 0) && all(mis$na_h == 0),
      "Nordic pool: NOR + 4 donors, 15 years each, no NA in y_main and y_h2",
      paste(sprintf("%s n=%d", mis$iso, mis$n), collapse = ", "))

# Check 6.2: Nordics identical in both panels, treatment pattern
cmp <- inner_join(panel |> select(iso, year, y_main, y_h2),
                  panel_o |> select(iso, year, y_main, y_h2),
                  by = c("iso", "year"), suffix = c("_n", "_o"))
maxdiff <- max(abs(cmp$y_main_n - cmp$y_main_o), abs(cmp$y_h2_n - cmp$y_h2_o))
check("6.2", nrow(cmp) == 75 && maxdiff < 1e-9 && sum(panel_o$treat) == 3,
      "Nordics identical in panel_nordics and panel_west (<1e-9); treat = NOR 2022-2024 only",
      sprintf("max abs diff %.2e; treated obs %d", maxdiff, sum(panel_o$treat)))

# ---------------------------------------------------------------------------
# Balanced matrices per outcome x pool x variant
# ---------------------------------------------------------------------------

outcomes2 <- c("y_main", "y_h2")
poolnames <- names(donor_sets)
setups <- list(); dim_ok <- TRUE; dim_txt <- character(0)
for (y in outcomes2) for (pl in poolnames) for (v in names(VARIANTS)) {
  donors <- donor_sets[[pl]][[y]]
  df <- panel_o |>
    filter(iso %in% c(donors, "NOR"), VARIANTS[[v]]$keep(year)) |>
    select(iso, year, all_of(y), treat) |>
    mutate(treat = as.integer(treat)) |> as.data.frame()
  n_t <- VARIANTS[[v]]$T0 + 3
  stopifnot(nrow(df) == n_t * (length(donors) + 1), !anyNA(df[[y]]))
  setup <- panel.matrices(df, unit = "iso", time = "year", outcome = y, treatment = "treat")
  key0 <- paste(y, pl, v, sep = "|")
  setups[[key0]] <- setup
  ok <- setup$N0 == length(donors) && setup$T0 == VARIANTS[[v]]$T0 &&
    all(dim(setup$Y) == c(length(donors) + 1, n_t))
  dim_ok <- dim_ok && ok
  dim_txt <- c(dim_txt, sprintf("%s N0=%d T0=%d", key0, setup$N0, setup$T0))
}
check("6.3", dim_ok, "balanced matrices for 2 outcomes x 3 variants, Nordic pool (N0 = 4, T0 = 12/11/6)",
      paste(dim_txt, collapse = "; "))

# ---------------------------------------------------------------------------
# Estimation
# ---------------------------------------------------------------------------

fit_est <- function(e, setup) switch(e,
  sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0),
  sc   = sc_estimate(setup$Y, setup$N0, setup$T0),
  did  = did_estimate(setup$Y, setup$N0, setup$T0))

set.seed(SEED)
est_all <- list(); res_rows <- list(); wt_rows <- list()
for (y in outcomes2) for (pl in poolnames) for (v in names(VARIANTS)) {
  key0   <- paste(y, pl, v, sep = "|")
  setup  <- setups[[key0]]
  donors <- donor_sets[[pl]][[y]]
  ests   <- if (v == "full") c("sdid", "sc", "did") else "sdid"
  for (e in ests) {
    est <- fit_est(e, setup)
    se  <- sqrt(as.numeric(vcov(est, method = "placebo")))
    tau <- as.numeric(est)
    key <- paste(key0, e, sep = "|")
    est_all[[key]] <- est
    res_rows[[key]] <- tibble(
      outcome = y, pool = pl, variant = v, years = VARIANTS[[v]]$label, estimator = e,
      estimate = tau, se_placebo = se, ci_lo = tau - 1.96 * se, ci_hi = tau + 1.96 * se,
      n_donors = length(donors), T0 = setup$T0,
      flag = "few donors, placebo SE unreliable")
    wt <- attr(est, "weights")
    omega <- as.numeric(wt$omega); lambda <- as.numeric(wt$lambda)
    if (!length(omega))  omega  <- rep(1 / setup$N0, setup$N0)
    if (!length(lambda)) lambda <- rep(1 / setup$T0, setup$T0)
    wt_rows[[key]] <- bind_rows(
      tibble(outcome = y, pool = pl, variant = v, estimator = e, type = "unit",
             name = rownames(setup$Y)[1:setup$N0], weight = omega),
      tibble(outcome = y, pool = pl, variant = v, estimator = e, type = "time",
             name = colnames(setup$Y)[1:setup$T0], weight = lambda))
  }
}
tab_sdid <- bind_rows(res_rows)
tab_wts  <- bind_rows(wt_rows)

# Check 6.4: did_estimate Nordic pool (2010-2024) = TWFE without indicators (noind)
g_did <- function(y) tab_sdid$estimate[tab_sdid$outcome == y & tab_sdid$pool == "nordic" &
                                          tab_sdid$variant == "full" & tab_sdid$estimator == "did"]
check("6.4", abs(g_did("y_main") - bget("noind", "y_main")) < 1e-6 &&
        abs(g_did("y_h2") - bget("noind", "y_h2")) < 1e-6,
      "did_estimate Nordic pool (2010-2024) equals TWFE without indicators of Section 3 (tol 1e-6)",
      sprintf("did y_main %.6f vs %.6f; y_h2 %.6f vs %.6f", g_did("y_main"),
              bget("noind", "y_main"), g_did("y_h2"), bget("noind", "y_h2")))

# Check 6.5: weights on the simplex
w_ok <- TRUE; w_txt <- character(0)
for (key in names(wt_rows)) {
  parts <- strsplit(key, "|", fixed = TRUE)[[1]]
  wdf <- wt_rows[[key]]
  om <- wdf$weight[wdf$type == "unit"]; la <- wdf$weight[wdf$type == "time"]
  ok <- min(om) >= -1e-8 && abs(sum(om) - 1) < 1e-6 && min(la) >= -1e-8 &&
    (if (parts[4] == "sc") abs(sum(la)) < 1e-8 else abs(sum(la) - 1) < 1e-6)
  w_ok <- w_ok && ok
  if (!ok) w_txt <- c(w_txt, key)
}
check("6.5", w_ok, sprintf("all %d estimations: weights nonnegative, omega sums 1, lambda sums 1 (sdid/did) or 0 (sc)", length(wt_rows)),
      ifelse(w_ok, "all simplex constraints satisfied", paste(w_txt, collapse = "; ")))

stop_if_failed("Section 6")

# ---------------------------------------------------------------------------
# Outputs
# ---------------------------------------------------------------------------

write_csv(tab_sdid, "Output/tables/tab_sdid.csv")
writeLines(c(
  "tab_sdid -- synthetic DiD (synthdid), synthetic control and DiD estimates, Nordic pool",
  "Treated: NOR from 2022. Variants: full = 2010-2024 (T0=12; sdid, sc, did); ex2021 = 2010-2024 without 2021 (T0=11; sdid);",
  "  main = 2015-2024 without 2021 (T0=6; sdid). SE: placebo method (200 reps, seed 1892); 95% CI normal.",
  "Pool: nordic = DNK, FIN, ISL, SWE (4 donors; placebo SE unreliable). Western European pool: tab_sdid_west.",
  "",
  sprintf("%-7s %-10s %-7s %-5s %9s %8s %9s %9s %3s %3s  %s", "outcome", "pool", "variant",
          "estim", "estimate", "se", "ci_lo", "ci_hi", "N0", "T0", "flag"),
  sprintf("%-7s %-10s %-7s %-5s %9s %8s %9s %9s %3d %3d  %s", tab_sdid$outcome, tab_sdid$pool,
          tab_sdid$variant, tab_sdid$estimator, fmt(tab_sdid$estimate), fmt(tab_sdid$se_placebo),
          fmt(tab_sdid$ci_lo), fmt(tab_sdid$ci_hi), tab_sdid$n_donors, tab_sdid$T0, tab_sdid$flag)),
  "Output/tables/tab_sdid.txt")

write_csv(tab_wts, "Output/tables/tab_sdid_weights.csv")
txt_wts <- c("tab_sdid_weights -- unit (omega) and time (lambda) weights per estimation (top 10, > 1e-4)", "")
for (key in names(wt_rows)) {
  parts <- strsplit(key, "|", fixed = TRUE)[[1]]
  for (ty in c("unit", "time")) {
    wdf <- wt_rows[[key]] |> filter(type == ty, weight > 1e-4) |> arrange(desc(weight)) |> head(10)
    txt_wts <- c(txt_wts, sprintf("%s | %s | %s | %s | %s: %s", parts[1], parts[2], parts[3], parts[4], ty,
                                  ifelse(nrow(wdf) == 0, "(all < 0.0001)",
                                         paste(sprintf("%s %s", wdf$name, fmt(wdf$weight, 4)), collapse = ", "))))
  }
}
writeLines(txt_wts, "Output/tables/tab_sdid_weights.txt")

top_str <- function(y, pl, v, ty, n) {
  wdf <- tab_wts |> filter(outcome == y, pool == pl, variant == v, estimator == "sdid", type == ty) |>
    arrange(desc(weight)) |> head(n)
  paste(sprintf("%s %.2f", wdf$name, wdf$weight), collapse = ", ")
}
w("synthdid %s; Nordic pool, N0 = %d; variants full / ex2021 / main",
  as.character(utils::packageVersion("synthdid")), length(NORDIC4))
w("1) Nordic pool, estimates (pp of GDP, placebo SE, 95%% CI):")
for (i in seq_len(nrow(tab_sdid))) {
  r <- tab_sdid[i, ]
  w("   %-7s %-10s %-7s %-5s %+8.3f  SE %6.3f  CI [%+.3f, %+.3f]%s", r$outcome, r$pool, r$variant,
    r$estimator, r$estimate, r$se_placebo, r$ci_lo, r$ci_hi,
    ifelse(r$flag != "", paste0("  [", r$flag, "]"), ""))
}
w("2) sdid weights (top-5 units | top-3 time):")
for (y in outcomes2) for (pl in poolnames) for (v in names(VARIANTS))
  w("   %-7s %-10s %-7s units: %s | time: %s", y, pl, v,
    top_str(y, pl, v, "unit", 5), top_str(y, pl, v, "time", 3))

# ---------------------------------------------------------------------------
# Western European group (full, ex2021, main), y_main and y_h2
# ---------------------------------------------------------------------------
sd_rows <- list(); wt_rows_we <- list(); est_we <- list(); set.seed(SEED)
for (y in c("y_main", "y_h2")) for (v in names(VARIANTS)) {
  donors <- intersect(WEST_DONORS, complete_donors(panel_o, y))
  df <- panel_o |> filter(iso %in% c(donors, "NOR"), VARIANTS[[v]]$keep(year)) |>
    select(iso, year, all_of(y), treat) |> mutate(treat = as.integer(treat)) |> as.data.frame()
  setup <- panel.matrices(df, unit = "iso", time = "year", outcome = y, treatment = "treat")
  ests <- if (v == "full") c("sdid", "sc", "did") else "sdid"
  for (e in ests) {
    est <- switch(e, sdid = synthdid_estimate(setup$Y, setup$N0, setup$T0), sc = sc_estimate(setup$Y, setup$N0, setup$T0),
                  did = did_estimate(setup$Y, setup$N0, setup$T0))
    se <- sqrt(as.numeric(vcov(est, method = "placebo"))); tau <- as.numeric(est)
    est_we[[paste(y, v, e)]] <- est
    sd_rows[[paste(y, v, e)]] <- tibble(outcome = y, pool = "west", variant = v, years = VARIANTS[[v]]$label, estimator = e,
                                        estimate = tau, se_placebo = se, ci_lo = tau - 1.96 * se, ci_hi = tau + 1.96 * se,
                                        n_donors = setup$N0, T0 = setup$T0)
    if (e == "sdid") {
      wt <- attr(est, "weights"); om <- as.numeric(wt$omega); la <- as.numeric(wt$lambda)
      names(om) <- rownames(setup$Y)[seq_len(setup$N0)]; names(la) <- colnames(setup$Y)[seq_len(setup$T0)]
      wt_rows_we[[paste(y, v)]] <- tibble(outcome = y, variant = v,
        units = paste(sprintf("%s %.2f", names(sort(om, decreasing = TRUE))[1:5], sort(om, decreasing = TRUE)[1:5]), collapse = ", "),
        time = paste(sprintf("%s %.2f", names(sort(la, decreasing = TRUE))[1:3], sort(la, decreasing = TRUE)[1:3]), collapse = ", "))
    }
  }
}
tab_sdid_we <- bind_rows(sd_rows); tab_sdid_we_wt <- bind_rows(wt_rows_we)
style_sdid <- function(p) p +
  labs(x = "Year", y = "Mainland tax revenue (percent of GDP)") +
  theme(legend.position = "top", legend.title = element_blank(), panel.grid.minor = element_blank(),
        axis.title.y = element_text(size = 10, margin = margin(r = 6)), axis.title.x = element_text(size = 10, margin = margin(t = 6)),
        axis.text = element_text(color = "grey30"), plot.margin = margin(8, 12, 8, 8))
save_fig(style_sdid(synthdid_plot(est_we[["y_main full sdid"]], treated.name = "Norway", control.name = "Synthetic control")), "fig_sdid_west", width = 8, height = 3.8)
save_fig(style_sdid(synthdid_plot(est_we[["y_main ex2021 sdid"]], treated.name = "Norway", control.name = "Synthetic control")), "fig_sdid_west_ex2021")
# All estimators (Ch. 4.2): TWFE (main, full; Section 4) and SDID, SC, DiD (full, ex2021, main) per comparison group
sdid_nordic <- tab_sdid |> select(outcome, pool, variant, estimator, estimate, se_placebo, ci_lo, ci_hi, n_donors, T0)
tab_estimators <- bind_rows(
  tab_pools |> filter(outcome %in% c("y_main", "y_h2")) |>
    transmute(outcome, pool, variant = sample, estimator = "twfe", estimate = coef, se = se_cluster,
              note = sprintf("studentised placebo rank %d of %d (p = %.2f); 90%% set [%s, %s]", rank_student, n_donors + 1L, p_rank_student,
                             fmt(sapply(seq_len(n()), function(i) gi(pool[i], variant[i], outcome[i], "student", "cs90_lo")), 2),
                             fmt(sapply(seq_len(n()), function(i) gi(pool[i], variant[i], outcome[i], "student", "cs90_hi")), 2))),
  bind_rows(sdid_nordic, tab_sdid_we |> select(-years)) |>
    transmute(outcome, pool, variant, estimator, estimate, se = se_placebo, note = sprintf("95%% CI [%.2f, %.2f], N0 = %d", ci_lo, ci_hi, n_donors))) |>
  filter(pool %in% c("nordic", "west")) |>
  mutate(pool = factor(pool, levels = c("nordic", "west")), outcome = factor(outcome, levels = c("y_main", "y_h2")),
         estimator = factor(estimator, levels = c("twfe", "sdid", "sc", "did")),
         variant = factor(variant, levels = c("main", "full", "ex2021"))) |>
  arrange(outcome, pool, estimator, variant) |> mutate(across(c(pool, outcome, estimator, variant), as.character))

write_csv(tab_sdid_we, "Output/tables/tab_sdid_west.csv")
write_csv(tab_sdid_we_wt, "Output/tables/tab_sdid_west_weights.csv")
write_csv(tab_estimators, "Output/tables/tab_estimators.csv")
w("2) Western European pool, estimates:"); for (k in seq_len(nrow(tab_sdid_we))) { v <- tab_sdid_we[k, ]
  w("   %-6s %-6s %-4s %+6.3f (%.3f) [%+.2f, %+.2f] N0 %d", v$outcome, v$variant, v$estimator, v$estimate, v$se_placebo, v$ci_lo, v$ci_hi, v$n_donors) }
w("Files: tab_sdid, tab_sdid_weights, tab_sdid_west, tab_sdid_west_weights, tab_estimators, fig_sdid_west, fig_sdid_west_ex2021")
w("")

##############################################################################
##############################################################################
# SECTION 7: ROBUSTNESS: LADDER, VARIANTS, BREAK-EVEN BIAS (Table A1.4, A1.5, A1.8 Panel B)
##############################################################################
##############################################################################


set.seed(SEED)
section("Section 7: Robustness")

# --- Brent: annual means of the FRED series POILBREUSDM (raw file shipped) --
brent <- read_csv("Data/Raw/brent_annual.csv", show_col_types = FALSE)
check("7.1", identical(as.integer(brent$year), 2010:2024) && !anyNA(brent$brent) &&
        all(brent$brent > 30 & brent$brent < 130),
      "Brent annual means 2010-2024 complete, plausible range", "source Data/Raw/brent_annual.csv")

panel6 <- panel |>
  left_join(brent |> select(year, brent), by = "year") |>
  mutate(lgdppc = log(gdppc_2015usd), lpop = log(pop),
         nor_lbrent = as.integer(iso == "NOR") * log(brent),
         nyear = ifelse(iso == "NOR", year, 0))
stopifnot(!anyNA(panel6$lgdppc), !anyNA(panel6$infl), !anyNA(panel6$lpop), !anyNA(panel6$nor_lbrent))

# --- A112 excess over the linear pre-trend, per pre-period -------------------
stopifnot("rr_a112" %in% names(panel6))
nor_a112 <- panel6 |> filter(iso == "NOR") |> transmute(year, a112_share = rr_a112 / gdp_ml * 100)
rr_excess <- function(pre_years) {
  f <- lm(a112_share ~ year, data = nor_a112 |> filter(year %in% pre_years))
  nor_a112 |> filter(year >= 2022) |>
    transmute(iso = "NOR", year, excess = a112_share - predict(f, newdata = data.frame(year = year)))
}
exc_full <- rr_excess(2010:2021); exc_main <- rr_excess(2015:2020)
check("7.2", all(abs(exc_full$excess - c(0.74, 0.29, -0.08)) <= 0.03),
      "A112 excess over 2010-2021 trend ~ +0.74/+0.29/-0.08 (control values, tol 0.03)",
      sprintf("full-trend %s; main-trend (2015-2020) %s",
              paste(sprintf("%+.2f", exc_full$excess), collapse = "/"),
              paste(sprintf("%+.2f", exc_main$excess), collapse = "/")))


add_variants <- function(d, exc) {
  d |> left_join(exc |> rename(rr_excess = excess), by = c("iso", "year")) |>
    mutate(rr_excess = coalesce(rr_excess, 0),
           adj_np  = if_else(iso == "NOR" & year >= 2022, NP_BN[as.character(year)] * 1e9 / gdp_ml * 100, 0),
           adj_max = if_else(iso == "NOR" & year >= 2022,
                             rr_excess + NP_EX_A112_BN[as.character(year)] * 1e9 / gdp_ml * 100, 0),
           adj_npsym  = if_else(iso == "NOR" & year >= 2022, NP_SYM_BN[as.character(year)] * 1e9 / gdp_ml * 100, 0),
           adj_relief = if_else(iso == "NOR" & year >= 2022, NP_PIT_CUT_BN[as.character(year)] * 1e9 / gdp_ml * 100, 0),
           y_main_rrbound = y_main - rr_excess,
           y_main_np      = y_main - adj_np,
           y_main_npsym   = y_main - adj_npsym,
           y_main_max     = y_main_ex1300 - adj_max,
           y_h2_relief    = y_h2 + adj_relief)
}
dat6_main   <- add_variants(prep_sample(panel6, "main"),   exc_full)   # trend 2010-2021 for all samples
dat6_full   <- add_variants(prep_sample(panel6, "full"),   exc_full)
dat6_noind  <- add_variants(prep_sample(panel6, "noind"),  exc_full)
stopifnot(sum(dat6_main$adj_np > 0) == 3, sum(dat6_full$adj_max != 0) == 3,
          sum(dat6_main$adj_relief > 0) == 3, all(dat6_main$adj_npsym <= dat6_main$adj_np))

run_twfe <- function(dat, yvar, s, extra = NULL) {
  rhs <- c(rhs_of(s, d = dat), extra)
  f  <- feols(as.formula(paste0(yvar, " ~ ", paste(rhs, collapse = " + "), " | iso_f + year_f")),
              data = dat, cluster = ~iso_f)
  ct <- coeftable(f)["treat", ]
  tibble(coef = ct[["Estimate"]], se = ct[["Std. Error"]], p = ct[["Pr(>|t|)"]], n = nobs(f))
}
run_webb <- function(fml, dname) {
  fit <- fit_named(fml, dname)
  bw  <- tryCatch(run_boot(fit, "webb"), error = function(e) NULL)
  if (is.null(bw)) NA_real_ else num1(bw$p_val)
}

# ---------------------------------------------------------------------------
# 1. Specification ladder
# ---------------------------------------------------------------------------
ladder_specs <- list("baseline" = NULL, "+ log GDP pc" = "lgdppc",
                     "+ inflation" = c("lgdppc", "infl"),
                     "+ log population" = c("lgdppc", "infl", "lpop"),
                     "+ NOR x log(Brent)" = c("lgdppc", "infl", "lpop", "nor_lbrent"))
tab_ladder <- bind_rows(lapply(SAMPLES_MAIN, function(s) bind_rows(lapply(c("y_main", "y_h2"), function(y)
  bind_rows(lapply(names(ladder_specs), function(sp)
    run_twfe(get(paste0("dat6_", s)), y, s, ladder_specs[[sp]]) |>
      mutate(sample = s, outcome = y, spec = sp, .before = 1)))))))
lb <- tab_ladder |> filter(spec == "baseline")
check("7.3", all(abs(lb$coef - mapply(bget, lb$sample, lb$outcome)) < 1e-6),
      "ladder baselines equal Table 4.1 (tol 1e-6)", sprintf("%d rows", nrow(tab_ladder)))

# ---------------------------------------------------------------------------
# 2. Outcome and confounder variants
# ---------------------------------------------------------------------------
ovars <- c("y_main_grossgdp", "y_main_a23", "y_main_vintage", "y_main_lpc", "y_main_rrbound",
           "y_main_ex1300", "y_main_np", "y_main_npsym", "y_main_max")
onotes <- c(y_main_grossgdp = "unadjusted OECD series: total revenue incl. petroleum / total GDP",
            y_main_a23 = "petroleum production taxes (A23) additionally subtracted",
            y_main_vintage = "vintage-consistent denominator (OECD-implied GDP x SSB mainland share)",
            y_main_lpc = "log real revenue per capita (national currency, CPI 2015=100); rel_pct = coef x 100",
            y_main_rrbound = "A112 above-trend excess subtracted from NOR 2022-2024 (linear trend fitted 2010-2021 for all samples; the six-point 2015-2020 trend is too unstable); over-corrects",
            y_main_ex1300 = "OECD 1300 (DNK pension yield tax, ISL item) removed from all control series",
            y_main_np = "MoF ex-ante non-package measures (NOK bn 2022/23/24: 15.20/33.05/19.38) subtracted from NOR; over-corrects",
            y_main_npsym = "symmetric: non-package measures net of the non-package PIT reliefs (NOK bn 2022/23/24: 11.56/21.87/6.20) subtracted from NOR",
            y_main_max = "ex-1300 controls + A112 excess + non-package outside A112 subtracted from NOR; maximal over-correction")
ovar_rows <- list()
for (s in names(SAMPLE_DEF)) for (v in ovars) {
  dname <- paste0("dat6_", s)
  r <- run_twfe(get(dname), v, s)
  pw <- if (s %in% SAMPLES_MAIN && v %in% c("y_main_ex1300", "y_main_np", "y_main_npsym", "y_main_max"))
    run_webb(fml_of(v, s, d = get(dname)), dname) else NA_real_
  ovar_rows[[paste(s, v)]] <- tibble(block = "outcome", sample = s, variant = v, outcome = v,
                                     coef = r$coef, se = r$se, p = r$p, n = r$n,
                                     rel_pct = ifelse(v == "y_main_lpc", r$coef * 100, NA_real_),
                                     p_webb = pw, note = onotes[[v]])
}
tab_ovar <- bind_rows(ovar_rows)
og <- function(s, v) tab_ovar$coef[tab_ovar$sample == s & tab_ovar$variant == v]

# ex-1300 placebos (MDES_b) and year effects per sample
pl_ex <- bind_rows(lapply(names(SAMPLE_DEF), function(s) bind_rows(lapply(CONTROLS, function(g) {
  d <- get(paste0("dat6_", s)) |> filter(iso != "NOR") |>
    mutate(treat_pl = as.integer(iso == g & year >= 2022), iso_f = factor(iso), year_f = factor(year))
  ct <- coeftable(feols(fml_of("y_main_ex1300", s, "treat_pl", d), data = d, cluster = ~iso_f))["treat_pl", ]
  tibble(type = "placebo_ex1300", sample = s, unit = g, outcome = "y_main_ex1300", year = NA_integer_,
         value = ct[["Estimate"]], se = ct[["Std. Error"]], p = ct[["Pr(>|t|)"]], n = nrow(d))
}))))
mdes_ex <- pl_ex |> group_by(sample) |> summarise(mdes_b_ex1300 = max(abs(value)), .groups = "drop")
ev_adj <- bind_rows(lapply(SAMPLES_MAIN, function(s) bind_rows(lapply(
  c("y_main_ex1300", "y_main_np", "y_main_npsym", "y_main_max", "y_main_rrbound"), function(v) {
    d <- get(paste0("dat6_", s)); untr <- d |> filter(!(iso == "NOR" & year >= 2022))
    f <- lm(as.formula(paste0(v, " ~ factor(iso) + factor(year) + ", ind_rhs(untr))), data = untr)
    np <- d |> filter(iso == "NOR", year >= 2022) |> arrange(year)
    tibble(type = "year_effect", sample = s, unit = "NOR", outcome = v, year = np$year,
           value = np[[v]] - predict(f, newdata = np), se = NA_real_, p = NA_real_, n = nrow(untr))
  }))))
tab_adj_detail <- bind_rows(pl_ex, ev_adj)

check("7.4", abs(og("noind", "y_main_rrbound") - (-0.25)) <= 0.05 &&
        abs(og("noind", "y_main_ex1300") - (-0.065)) <= 0.05 &&
        abs(og("noind", "y_main_np") - (-0.490)) <= 0.05 &&
        abs(og("noind", "y_main_max") - (-0.665)) <= 0.05 &&
        abs(mdes_ex$mdes_b_ex1300[mdes_ex$sample == "noind"] - 1.00) <= 0.05,
      "noind control values: rrbound -0.25, ex1300 -0.065, np -0.490, max -0.665, MDES_b(ex1300) 1.00 (tol 0.05)",
      sprintf("rrbound %+.3f, ex1300 %+.3f, np %+.3f, max %+.3f, MDES_b %.3f",
              og("noind", "y_main_rrbound"), og("noind", "y_main_ex1300"),
              og("noind", "y_main_np"), og("noind", "y_main_max"),
              mdes_ex$mdes_b_ex1300[mdes_ex$sample == "noind"]))

# ---------------------------------------------------------------------------
# 3. Sample variants
# ---------------------------------------------------------------------------
svar_rows <- list()
add_svar <- function(s, label, d, y, s_rhs = s, note = "") {
  r <- run_twfe(d, y, s_rhs)
  svar_rows[[paste(s, label, y)]] <<- tibble(block = "sample", sample = s, variant = label, outcome = y,
                                            coef = r$coef, se = r$se, p = r$p, n = r$n,
                                            rel_pct = NA_real_, p_webb = NA_real_, note = note)
}
for (y in c("y_main", "y_h2")) {
  add_svar("main", "excl 2024", dat6_main |> filter(year != 2024), y)
  add_svar("main", "incl 2021 (2015-2024)", dat6_full |> filter(year >= 2015), y,
           note = "2021 as pre-year")
  for (g in CONTROLS) add_svar("main", paste("LOO", g), dat6_main |> filter(iso != g), y)
  add_svar("main", "from 2010 (= full)", dat6_full, y)
  add_svar("main", "from 2010 excl 2021", dat6_full |> filter(year != 2021), y)
  add_svar("main", "no ISL indicator", dat6_main, y, s_rhs = "noind",
           note = "main sample without the ISL2016 indicator")
  add_svar("main", "noind (2010-2024, no indicators)", dat6_noind, y, s_rhs = "noind",
           note = "full sample without indicators")
  add_svar("full", "excl 2024", dat6_full |> filter(year != 2024), y)
  for (g in CONTROLS) add_svar("full", paste("LOO", g), dat6_full |> filter(iso != g), y)
}
tab_svar <- bind_rows(svar_rows)
sv <- function(s, lab, y) tab_svar$coef[tab_svar$sample == s & tab_svar$variant == lab & tab_svar$outcome == y]
check("7.5", abs(sv("main", "incl 2021 (2015-2024)", "y_main") - 0.387) < 0.002 &&
        abs(sv("main", "incl 2021 (2015-2024)", "y_h2") - 0.650) < 0.002 &&
        abs(sv("main", "noind (2010-2024, no indicators)", "y_main") - 0.069) < 0.002,
      "sample variant control values: 2015-2024 with 2021 0.387/0.650, noind 0.069",
      sprintf("incl2021 %+.3f/%+.3f, noind %+.3f", sv("main", "incl 2021 (2015-2024)", "y_main"),
              sv("main", "incl 2021 (2015-2024)", "y_h2"), sv("main", "noind (2010-2024, no indicators)", "y_main")))
tab_variants <- bind_rows(tab_ovar, tab_svar)

# ---------------------------------------------------------------------------
# 4. Break-even bias: corrected outcomes and pool without Sweden (Table A1.8, Panel B)
# ---------------------------------------------------------------------------
ext_rows <- list()
# a) Western European pool, main: y_main with the corrections of Table A1.5 subtracted from Norway's
#    post-period years (y_main_rrbound, y_main_np, y_main_npsym). Placebos and s_NOR as for y_main.
pdw  <- pool_data("west", "y_main", "main")
dw   <- add_variants(pdw$d, exc_full)
stopifnot(sum(dw$adj_np > 0) == 3, all(dw$y_main_np[dw$iso != "NOR"] == dw$y_main[dw$iso != "NOR"]))
pl_w <- tab_plac |> filter(pool == "west", sample == "main", outcome == "y_main")
pr_w <- tab_pools |> filter(pool == "west", sample == "main", outcome == "y_main")
pl_np <- vapply(pdw$donors, function(g) {
  dg <- dw |> filter(iso != "NOR") |> mutate(treat_pl = as.integer(iso == g & year >= 2022))
  coef_row(feols(fml_tw("y_main_np", "treat_pl", dg), data = dg, cluster = ~iso_f), "treat_pl")$coef
}, numeric(1))
sc_np <- unit_scales(dw, "y_main_np", "main")
b_np  <- coef_row(feols(fml_tw("y_main_np", "treat", dw), data = dw, cluster = ~iso_f))$coef
check("7.6", max(abs(pl_np - pl_w$coef[match(pdw$donors, pl_w$unit)])) < 1e-8 &&
        abs(sc_np$s[sc_np$iso == "NOR"] - pr_w$s_pre_nor) < 1e-8 &&
        abs((pr_w$coef - b_np) - mean(dw$adj_np[dw$iso == "NOR" & dw$year >= 2022])) < 1e-8,
      "Norway-only corrections leave the West placebos and s_NOR unchanged and shift the estimate one for one",
      sprintf("max |placebo diff| %.1e; s_NOR %.4f = %.4f; shift %.4f = mean adjustment %.4f",
              max(abs(pl_np - pl_w$coef[match(pdw$donors, pl_w$unit)])), sc_np$s[sc_np$iso == "NOR"], pr_w$s_pre_nor,
              pr_w$coef - b_np, mean(dw$adj_np[dw$iso == "NOR" & dw$year >= 2022])))
for (v in c("y_main_rrbound", "y_main_np", "y_main_npsym")) {
  b <- coef_row(feols(fml_tw(v, "treat", dw), data = dw, cluster = ~iso_f))$coef
  for (method in c("student", "raw"))
    ext_rows[[paste(v, method)]] <- bind_cols(
      tibble(pool = "west", variant = v, sample = "main", outcome = v, method = method,
             donors = paste(pdw$donors, collapse = " ")),
      invert_one(b, if (method == "student") pr_w$s_pre_nor else 1,
                 if (method == "student") pl_w$t_student else pl_w$coef, delta_of(v)) |>
        mutate(s_pre_nor = pr_w$s_pre_nor, .after = coef))
}
# a2) H2 with the non-package PIT reliefs added back (y_h2_relief). Placebos and s_NOR as for y_h2.
pdh  <- pool_data("west", "y_h2", "main")
dh   <- add_variants(pdh$d, exc_full)
stopifnot(setequal(pdh$donors, pdw$donors), sum(dh$adj_relief > 0) == 3)
pl_h <- tab_plac |> filter(pool == "west", sample == "main", outcome == "y_h2")
pr_h <- tab_pools |> filter(pool == "west", sample == "main", outcome == "y_h2")
b_h  <- coef_row(feols(fml_tw("y_h2_relief", "treat", dh), data = dh, cluster = ~iso_f))$coef
sc_h <- unit_scales(dh, "y_h2_relief", "main")
b_npsym <- coef_row(feols(fml_tw("y_main_npsym", "treat", dw), data = dw, cluster = ~iso_f))$coef
check("7.6b", abs(sc_h$s[sc_h$iso == "NOR"] - pr_h$s_pre_nor) < 1e-8 &&
        abs((b_h - pr_h$coef) - mean(dh$adj_relief[dh$iso == "NOR" & dh$year >= 2022])) < 1e-8 &&
        abs((pr_w$coef - b_npsym) - mean(dw$adj_npsym[dw$iso == "NOR" & dw$year >= 2022])) < 1e-8,
      "symmetric corrections: s_NOR unchanged, H2 rises by the mean relief adjustment, H1 falls by the mean net non-package adjustment",
      sprintf("H2 %+.4f -> %+.4f (mean relief %.4f pp); H1 %+.4f -> %+.4f (mean net non-package %.4f pp, gross %.4f)",
              pr_h$coef, b_h, mean(dh$adj_relief[dh$iso == "NOR" & dh$year >= 2022]),
              pr_w$coef, b_npsym, mean(dw$adj_npsym[dw$iso == "NOR" & dw$year >= 2022]),
              mean(dw$adj_np[dw$iso == "NOR" & dw$year >= 2022])))
for (method in c("student", "raw"))
  ext_rows[[paste("y_h2_relief", method)]] <- bind_cols(
    tibble(pool = "west", variant = "y_h2_relief", sample = "main", outcome = "y_h2_relief", method = method,
           donors = paste(pdh$donors, collapse = " ")),
    invert_one(b_h, if (method == "student") pr_h$s_pre_nor else 1,
               if (method == "student") pl_h$t_student else pl_h$coef, delta_of("y_h2_relief")) |>
      mutate(s_pre_nor = pr_h$s_pre_nor, .after = coef))
# b) Western European pool without Sweden and without France (nine placebos, smallest p-value 0.10)
EX_DONORS <- c(SWE = "without Sweden", FRA = "without France")
for (ex in names(EX_DONORS)) for (y in OUTC) {
  pd <- pool_data("west", y, "main")
  donors9 <- setdiff(pd$donors, ex)
  d9 <- pd$d |> filter(iso != ex) |> mutate(iso_f = factor(iso), year_f = factor(year))
  b9 <- coef_row(feols(fml_tw(y, "treat", d9), data = d9, cluster = ~iso_f))$coef
  sc9 <- unit_scales(d9, y, "main")
  pl9 <- bind_rows(lapply(donors9, function(g) {
    dg <- d9 |> filter(iso != "NOR") |> mutate(treat_pl = as.integer(iso == g & year >= 2022))
    tibble(unit = g, coef = coef_row(feols(fml_tw(y, "treat_pl", dg), data = dg, cluster = ~iso_f), "treat_pl")$coef)
  })) |> left_join(sc9 |> select(unit = iso, s_pre = s), by = "unit") |> mutate(t_student = coef / s_pre)
  s9 <- sc9$s[sc9$iso == "NOR"]
  for (method in c("student", "raw"))
    ext_rows[[paste0("ex", ex, " ", y, " ", method)]] <- bind_cols(
      tibble(pool = paste0("west_ex", ex), variant = EX_DONORS[[ex]], sample = "main", outcome = y, method = method,
             donors = paste(donors9, collapse = " ")),
      invert_one(b9, if (method == "student") s9 else 1,
                 if (method == "student") pl9$t_student else pl9$coef, delta_of(y)) |>
        mutate(s_pre_nor = s9, .after = coef))
}
# c) Western European pool with OECD 1300 removed from the control series (y_main_ex1300). The placebos
#    and the scales of all units change, Norway's series stays the same.
pdx <- pool_data("west", "y_main_ex1300", "main")
stopifnot(setequal(pdx$donors, pdw$donors))
dx  <- pdx$d
bx  <- coef_row(feols(fml_tw("y_main_ex1300", "treat", dx), data = dx, cluster = ~iso_f))$coef
scx <- unit_scales(dx, "y_main_ex1300", "main")
plx <- bind_rows(lapply(pdx$donors, function(g) {
  dg <- dx |> filter(iso != "NOR") |> mutate(treat_pl = as.integer(iso == g & year >= 2022))
  tibble(unit = g, coef = coef_row(feols(fml_tw("y_main_ex1300", "treat_pl", dg), data = dg, cluster = ~iso_f), "treat_pl")$coef)
})) |> left_join(scx |> select(unit = iso, s_pre = s), by = "unit") |> mutate(t_student = coef / s_pre)
sx <- scx$s[scx$iso == "NOR"]
for (method in c("student", "raw"))
  ext_rows[[paste("ex1300", method)]] <- bind_cols(
    tibble(pool = "west_ex1300", variant = "controls ex 1300", sample = "main", outcome = "y_main_ex1300", method = method,
           donors = paste(pdx$donors, collapse = " ")),
    invert_one(bx, if (method == "student") sx else 1,
               if (method == "student") plx$t_student else plx$coef, delta_of("y_main_ex1300")) |>
      mutate(s_pre_nor = sx, .after = coef))
tab_placebos_ex1300 <- plx |> mutate(pool = "west_ex1300", sample = "main", outcome = "y_main_ex1300", .before = 1) |>
  left_join(pl_w |> select(unit, coef_raw_series = coef, t_student_raw_series = t_student), by = "unit")
dnk_raw <- pl_w$coef[pl_w$unit == "DNK"]; dnk_ex <- plx$coef[plx$unit == "DNK"]
check("7.9", abs(dnk_ex) < abs(dnk_raw) && max(abs(plx$coef)) < max(abs(pl_w$coef)) && nrow(plx) == 10 && !anyNA(plx$t_student),
      "ex-1300 Western pool: Denmark's placebo and the largest |placebo| shrink against the raw control series; ten placebos",
      sprintf("DNK placebo %+.3f -> %+.3f; max |placebo| %.3f (%s) -> %.3f (%s); b_NOR %+.3f -> %+.3f; s_NOR %.4f -> %.4f",
              dnk_raw, dnk_ex, max(abs(pl_w$coef)), pl_w$unit[which.max(abs(pl_w$coef))],
              max(abs(plx$coef)), plx$unit[which.max(abs(plx$coef))], pr_w$coef, bx, pr_w$s_pre_nor, sx))

# d) Scale sensitivity of the studentised main analysis (Table A1.13): 90% set, smallest loss rejected
#    (delta_min = max(t_g) s_NOR - b), and p(loss >= Delta1) as a function of s_NOR. Break-even scale:
#    s* = (b + Delta1) / max(t_g).
scale_sens <- function(y) {
  pl <- tab_plac |> filter(pool == "west", sample == "main", outcome == y)
  pr <- tab_pools |> filter(pool == "west", sample == "main", outcome == y)
  n_pre <- tab_scales$n_pre[tab_scales$pool == "west" & tab_scales$sample == "main" & tab_scales$outcome == y & tab_scales$iso == "NOR"]
  stopifnot(nrow(pr) == 1, nrow(pl) == 10, length(n_pre) == 1, n_pre >= 4)
  tmax_two <- max(abs(pl$t_student)); tmax_one <- max(pl$t_student)
  s_ci <- pr$s_pre_nor * sqrt((n_pre - 1) / qchisq(c(0.95, 0.05), df = n_pre - 1))
  s_be <- (pr$coef + DELTA1) / tmax_one
  s_grid <- sort(unique(round(c(seq(0.10, 0.80, by = 0.05), pr$s_pre_nor, s_ci, s_be), 4)))
  tab <- tibble(outcome = y, s_nor = s_grid) |>
    mutate(cs90_lo = pr$coef - tmax_two * s_nor, cs90_hi = pr$coef + tmax_two * s_nor,
           delta_min = tmax_one * s_nor - pr$coef,
           p_loss_d1 = sapply(s_nor, function(sv) (1 + sum(pl$t_student >= (pr$coef + DELTA1) / sv)) / (length(pl$t_student) + 1)),
           rejected_10 = p_loss_d1 < 0.10,
           note = case_when(abs(s_nor - round(pr$s_pre_nor, 4)) < 1e-9 ~ "estimated s_NOR (Table 4.1)",
                            abs(s_nor - round(s_ci[1], 4)) < 1e-9 ~ "lower end of 90% chi-square interval",
                            abs(s_nor - round(s_ci[2], 4)) < 1e-9 ~ "upper end of 90% chi-square interval",
                            abs(s_nor - round(s_be, 4)) < 1e-9 ~ "break-even s (rejection of Delta1 lost above)",
                            TRUE ~ ""))
  list(tab = tab, coef = pr$coef, s = pr$s_pre_nor, n_pre = n_pre, tmax_two = tmax_two, tmax_one = tmax_one,
       s_ci = s_ci, s_be = s_be)
}
scale_sens_res <- lapply(c(y_main = "y_main", y_h2 = "y_h2"), scale_sens)
tab_scale_sens <- bind_rows(lapply(scale_sens_res, function(o) o$tab))
check("7.10", all(vapply(names(scale_sens_res), function(y) {
          o <- scale_sens_res[[y]]; tb <- o$tab; est <- tb$note == "estimated s_NOR (Table 4.1)"
          sum(est) == 1 && abs(tb$cs90_lo[est] - gi("west", "main", y, "student", "cs90_lo")) < 0.011 &&
            abs(tb$delta_min[est] - (DELTA1 - gi("west", "main", y, "student", "breakeven"))) < 1e-3 &&
            all(tb$rejected_10 == (tb$s_nor < o$s_be + 1e-9)) && o$s_ci[1] < o$s && o$s < o$s_ci[2] }, logical(1))) &&
        scale_sens_res$y_main$s_be < scale_sens_res$y_main$s_ci[2] && scale_sens_res$y_h2$s_be > scale_sens_res$y_h2$s_ci[2],
      "scale sensitivity (y_main, y_h2) reproduces the studentised 90% set (grid tolerance 0.01) and the smallest loss rejected at the estimated s_NOR, and the rejection switches exactly at the break-even s, which lies inside the 90% chi-square interval for y_main and above it for y_h2",
      paste(vapply(names(scale_sens_res), function(y) { o <- scale_sens_res[[y]]
        sprintf("%s: s_NOR %.4f (n_pre %d), chi-square 90%% interval %.3f to %.3f, break-even s %.3f, cs90_lo at interval ends %+.2f / %+.2f, delta_min at interval ends %+.2f / %+.2f",
                y, o$s, o$n_pre, o$s_ci[1], o$s_ci[2], o$s_be, o$coef - o$tmax_two * o$s_ci[1], o$coef - o$tmax_two * o$s_ci[2],
                o$tmax_one * o$s_ci[1] - o$coef, o$tmax_one * o$s_ci[2] - o$coef) }, character(1)), collapse = "; "))

tab_inference_ext <- bind_rows(ext_rows)
ge <- function(pool, y, m, col) tab_inference_ext[[col]][tab_inference_ext$pool == pool & tab_inference_ext$outcome == y & tab_inference_ext$method == m]
check("7.8", abs(gi("west", "main", "y_main", "student", "breakeven") - 0.433) < 0.005 &&
        abs(gi("west", "main", "y_pit", "student", "breakeven") - 0.045) < 0.005 &&
        abs(gi("west", "main", "y_h2", "student", "breakeven") - 0.712) < 0.005 &&
        gi("west", "main", "y_main", "student", "rejected_10") && !gi("west", "main", "y_main", "raw", "rejected_10") &&
        abs(ge("west", "y_main_np", "student", "p_loss_delta") - 5 / 11) < 1e-9 &&
        ge("west", "y_main_rrbound", "student", "rejected_10") && !ge("west", "y_main_np", "student", "rejected_10") &&
        abs(ge("west_exSWE", "y_pit", "student", "coef") - 0.078) < 0.005 &&
        all(!tab_inference_ext$rejected_10[tab_inference_ext$pool == "west_exSWE"]),
      "break-even West main (studentised): y_main 0.433, y_pit 0.045 (Delta2), y_h2 0.712 (Delta1); rrbound keeps, np loses the rejection (p 5/11); ex-SWE PIT 0.078, nothing rejected with nine placebos",
      sprintf("break-even %.3f/%.3f/%.3f; np p %.3f CS90 st [%+.2f, %+.2f] raw [%+.2f, %+.2f]; rrbound p %.3f; exSWE PIT %.3f p %.3f rank %d",
              gi("west", "main", "y_main", "student", "breakeven"), gi("west", "main", "y_pit", "student", "breakeven"),
              gi("west", "main", "y_h2", "student", "breakeven"), ge("west", "y_main_np", "student", "p_loss_delta"),
              ge("west", "y_main_np", "student", "cs90_lo"), ge("west", "y_main_np", "student", "cs90_hi"),
              ge("west", "y_main_np", "raw", "cs90_lo"), ge("west", "y_main_np", "raw", "cs90_hi"),
              ge("west", "y_main_rrbound", "student", "p_loss_delta"),
              ge("west_exSWE", "y_pit", "student", "coef"), ge("west_exSWE", "y_pit", "student", "p_loss_delta"),
              ge("west_exSWE", "y_pit", "student", "rank")))


# e) Margin ladder (Table A1.14, Figure 4.2): one-sided non-inferiority test of H1 and H2 on the Western
#    European pool (main sample) at the rungs 0.7 / 0.5 / 0.34 / 0.13 / 0. Smallest margin rejected:
#    delta_min = max(t_g) s_NOR - b.
pp_bn_2022    <- panel$gdp_ml[panel$iso == "NOR" & panel$year == 2022] / 1e9 / 100   # 1 pp of 2022 mainland GDP, NOK bn
margin_debate <- round(DEBATE_CLAIM_BN / pp_bn_2022, 2)
MARGINS <- c(DELTA1, 0.50, 0.34, margin_debate, 0.00)
MARGIN_LABELS <- c("Loss of the whole post-reform wealth tax (Delta1)",
                   "The wealth tax costs what it raised before the reform (2021)",
                   "The reform cost what it was meant to raise (mechanical yield, average 2022-2024)",
                   sprintf("The one circulated net-loss figure (NOK %.1f bn a year)", DEBATE_CLAIM_BN),
                   "Strict Laffer margin: behavioural loss equals mechanical gain")
MECH_YIELD <- MARGINS[3]
ladder_one <- function(b, sN, tg, margins) {
  N <- length(tg)
  bind_rows(lapply(margins, function(d) {
    stat <- (b + d) / sN
    tibble(margin = d, multiple = (d + MECH_YIELD) / MECH_YIELD, stat = stat,
           p = (1 + sum(tg >= stat)) / (N + 1),
           breakeven = b + d - max(tg) * sN, rejected_10 = (1 + sum(tg >= stat)) / (N + 1) < 0.10)
  }))
}
# Asymmetric bias (Ch. 4.2): increase in wealth tax revenue relative to 2021 minus the ex-ante estimate,
# pp of mainland GDP, mean 2022-2024 (check 8.2)
nor_wt <- panel |> filter(iso == "NOR", year >= 2021) |> transmute(year, pct = xdc_4210 / gdp_ml * 100)
asym_bias <- mean((nor_wt$pct[nor_wt$year >= 2022] - nor_wt$pct[nor_wt$year == 2021]) -
                    c(MOF_WEALTH[["2022"]], sum(MOF_WEALTH), sum(MOF_WEALTH)) / pp_bn_2022)
LADDER_SERIES <- c(h1 = "H1 main analysis", h1_rrbound = "H1, A112 excess subtracted",
                   h1_np = "H1, non-package measures subtracted",
                   h1_npsym = "H1, non-package measures net of the PIT reliefs subtracted",
                   h2 = "H2 main analysis", h2_bias = "H2, net of the asymmetric bias",
                   h2_relief = "H2, PIT reliefs added back, net of the asymmetric bias")
# ext: coefficient from block 4a/4a2 (tab_inference_ext) instead of Table 4.1
ladder_inputs <- list(
  h1         = list(outcome = "y_main",         ext = FALSE, shift = 0,          pool_y = "y_main"),
  h1_rrbound = list(outcome = "y_main_rrbound", ext = TRUE,  shift = 0,          pool_y = "y_main"),
  h1_np      = list(outcome = "y_main_np",      ext = TRUE,  shift = 0,          pool_y = "y_main"),
  h1_npsym   = list(outcome = "y_main_npsym",   ext = TRUE,  shift = 0,          pool_y = "y_main"),
  h2         = list(outcome = "y_h2",           ext = FALSE, shift = 0,          pool_y = "y_h2"),
  h2_bias    = list(outcome = "y_h2",           ext = FALSE, shift = -asym_bias, pool_y = "y_h2"),
  h2_relief  = list(outcome = "y_h2_relief",    ext = TRUE,  shift = -asym_bias, pool_y = "y_h2"))
ml_rows <- list()
for (k in names(ladder_inputs)) {
  li <- ladder_inputs[[k]]
  pr <- tab_pools |> filter(pool == "west", sample == "main", outcome == li$pool_y)
  pl <- tab_plac |> filter(pool == "west", sample == "main", outcome == li$pool_y)
  b  <- (if (li$ext) ge("west", li$outcome, "student", "coef") else pr$coef) + li$shift
  for (method in c("student", "raw")) {
    sN <- if (method == "student") pr$s_pre_nor else 1
    tg <- if (method == "student") pl$t_student else pl$coef
    ml_rows[[paste(k, method)]] <- ladder_one(b, sN, tg, MARGINS) |>
      mutate(series = k, series_label = LADDER_SERIES[[k]], pool = "west", sample = "main",
             outcome = li$outcome, method = method, label = MARGIN_LABELS,
             coef = b, s_nor = sN, max_t = max(tg), max_unit = pl$unit[which.max(tg)],
             delta_min = max(tg) * sN - b, .before = 1)
  }
}
tab_margin_ladder <- bind_rows(ml_rows)
gm <- function(k, m, d, col) tab_margin_ladder[[col]][tab_margin_ladder$series == k & tab_margin_ladder$method == m & abs(tab_margin_ladder$margin - d) < 1e-9]
check("7.11", abs(gm("h1", "student", DELTA1, "p") - gi("west", "main", "y_main", "student", "p_loss_d1")) < 1e-9 &&
        abs(gm("h2", "student", DELTA1, "breakeven") - gi("west", "main", "y_h2", "student", "breakeven")) < 1e-9 &&
        abs(gm("h1", "raw", DELTA1, "p") - gi("west", "main", "y_main", "raw", "p_loss_d1")) < 1e-9 &&
        abs(gm("h2", "raw", DELTA1, "breakeven") - gi("west", "main", "y_h2", "raw", "breakeven")) < 1e-9 &&
        abs(gm("h1", "student", 0.00, "p") - gi("west", "main", "y_main", "student", "p_loss_0")) < 1e-9 &&
        all(with(tab_margin_ladder |> filter(abs(margin - DELTA1) < 1e-9), abs((DELTA1 - delta_min) - breakeven) < 1e-9)),
      "margin ladder at Delta1 reproduces Table 4.1 (p and break-even, both readings) and delta_min = Delta1 - break-even",
      sprintf("delta_min studentised: H1 %+.3f (max t %s %.3f), H2 %+.3f (max t %s %.3f); unscaled: H1 %+.3f, H2 %+.3f",
              gm("h1", "student", DELTA1, "delta_min"), gm("h1", "student", DELTA1, "max_unit"), gm("h1", "student", DELTA1, "max_t"),
              gm("h2", "student", DELTA1, "delta_min"), gm("h2", "student", DELTA1, "max_unit"), gm("h2", "student", DELTA1, "max_t"),
              gm("h1", "raw", DELTA1, "delta_min"), gm("h2", "raw", DELTA1, "delta_min")))
check("7.12", abs(margin_debate - 0.13) < 1e-9 && abs(DEBATE_CLAIM_BN - 4.99) < 1e-9 &&
        abs(gm("h1_rrbound", "student", DELTA1, "breakeven") - ge("west", "y_main_rrbound", "student", "breakeven")) < 1e-9 &&
        abs(gm("h1_np", "student", DELTA1, "breakeven") - ge("west", "y_main_np", "student", "breakeven")) < 1e-9 &&
        abs(gm("h2_bias", "student", DELTA1, "delta_min") - (gm("h2", "student", DELTA1, "delta_min") + asym_bias)) < 1e-9 &&
        abs(gm("h1", "student", MECH_YIELD, "multiple") - 2) < 1e-9 && abs(gm("h1", "student", 0, "multiple") - 1) < 1e-9,
      "ladder: debate margin 0.13 (NOK 4.99 bn / 2022 mainland GDP per pp); corrected H1 rows reproduce the break-even of block 4a; H2 net of the bias shifts delta_min by the bias; multiples 2 at 0.34 and 1 at 0",
      sprintf("1 pp = NOK %.2f bn, debate margin %.4f -> %.2f; asymmetric bias %.4f; delta_min studentised: rrbound %+.3f, np %+.3f, H2 net of bias %+.3f",
              pp_bn_2022, DEBATE_CLAIM_BN / pp_bn_2022, margin_debate, asym_bias,
              gm("h1_rrbound", "student", DELTA1, "delta_min"), gm("h1_np", "student", DELTA1, "delta_min"),
              gm("h2_bias", "student", DELTA1, "delta_min")))
# Tolerance of the rejection of the circulated figure to a behavioural loss L in the wealth tax: the
# rejection holds as long as L < margin_debate - delta_min(H2 net of the bias).
wt_loss_tol       <- margin_debate - gm("h2_bias",   "student", DELTA1, "delta_min")
wt_loss_tol_rel   <- margin_debate - gm("h2_relief", "student", DELTA1, "delta_min")
wt_loss_claimed   <- DEBATE_WEALTH_BN * DEBATE_RATE / pp_bn_2022
check("7.13", abs(gm("h1_npsym", "student", DELTA1, "breakeven") - ge("west", "y_main_npsym", "student", "breakeven")) < 1e-9 &&
        abs(gm("h2_relief", "student", DELTA1, "delta_min") - (ge("west", "y_h2_relief", "student", "delta") - ge("west", "y_h2_relief", "student", "breakeven") + asym_bias)) < 1e-9 &&
        gm("h1_npsym", "student", DELTA1, "rejected_10") && !gm("h1_np", "student", DELTA1, "rejected_10") &&
        gm("h2_relief", "student", 0, "rejected_10") && !gm("h2_bias", "student", 0, "rejected_10") &&
        gm("h2_bias", "student", margin_debate, "rejected_10") &&
        wt_loss_tol > 0 && wt_loss_tol < wt_loss_claimed && wt_loss_tol_rel > wt_loss_claimed,
      "symmetric rows: H1 net of the reliefs keeps the rejection of Delta1 (the gross correction loses it); H2 with the reliefs rejects the strict Laffer margin again; the tolerance of the 0.13 rejection lies below the claimed wealth tax loss in the main analysis and above it with the reliefs",
      sprintf("H1 npsym break-even %+.3f (np %+.3f); H2 relief delta_min net of bias %+.3f (H2 net of bias %+.3f); tolerance L < %.3f pp (NOK %.1f bn, %.1f x Ring) main, < %.3f pp (NOK %.1f bn) with the reliefs; claimed wealth tax loss %.3f pp (NOK %.1f bn)",
              gm("h1_npsym", "student", DELTA1, "breakeven"), gm("h1_np", "student", DELTA1, "breakeven"),
              gm("h2_relief", "student", DELTA1, "delta_min"), gm("h2_bias", "student", DELTA1, "delta_min"),
              wt_loss_tol, wt_loss_tol * pp_bn_2022, wt_loss_tol * pp_bn_2022 / RING_WT_LOSS_BN,
              wt_loss_tol_rel, wt_loss_tol_rel * pp_bn_2022, wt_loss_claimed, wt_loss_claimed * pp_bn_2022))
write_csv(tab_margin_ladder, "Output/tables/tab_margin_ladder.csv")
writeLines(c("tab_margin_ladder -- Table A1.14 and Figure 4.2: one-sided non-inferiority test at five margins, West pool, main sample",
             "stat = (b + margin)/s_NOR against the signed placebo statistics; p = (1 + #{t_g >= stat})/(N + 1); break-even = b + margin - max(t_g) s_NOR;",
             "delta_min = max(t_g) s_NOR - b = smallest margin rejected at the 10 percent level (negative: every loss down to zero and beyond rejected);",
             "multiple = (margin + 0.34)/0.34 = behavioural loss the margin implies, in mechanical yields of the package.",
             sprintf("Series: %s. Asymmetric bias %.4f pp (Section 8, Panel B).", paste(sprintf("%s = %s", names(LADDER_SERIES), LADDER_SERIES), collapse = "; "), asym_bias), "",
             sprintf("%-10s %-8s %6s %8s %8s %6s %10s %8s %10s  %s", "series", "method", "margin", "multiple", "stat", "p", "breakeven", "rej_10", "delta_min", "label"),
             sprintf("%-10s %-8s %6.2f %8.1f %8.3f %6.3f %10.3f %8s %10.3f  %s", tab_margin_ladder$series, tab_margin_ladder$method,
                     tab_margin_ladder$margin, tab_margin_ladder$multiple, tab_margin_ladder$stat, tab_margin_ladder$p, tab_margin_ladder$breakeven,
                     tab_margin_ladder$rejected_10, tab_margin_ladder$delta_min, tab_margin_ladder$label)),
           "Output/tables/tab_margin_ladder.txt")
w("Margin ladder (Table A1.14, Figure 4.2; West main): series | method | margin (multiple of the mechanical yield) | p | break-even | rejected")
for (k in seq_len(nrow(tab_margin_ladder))) {
  r <- tab_margin_ladder[k, ]
  w("   %-10s %-7s %4.2f (%.1fx)  p %.3f  break-even %+.3f  %s", r$series, r$method, r$margin, r$multiple, r$p, r$breakeven,
    ifelse(r$rejected_10, "rejected", "not rejected"))
}
w("   smallest loss rejected (studentised): %s; (unscaled): H1 %+.3f, H2 %+.3f",
  paste(sprintf("%s %+.3f", names(LADDER_SERIES), sapply(names(LADDER_SERIES), \(k) gm(k, "student", DELTA1, "delta_min"))), collapse = ", "),
  gm("h1", "raw", DELTA1, "delta_min"), gm("h2", "raw", DELTA1, "delta_min"))
w("   asymmetric bias (H2): %.4f pp; 1 pp of 2022 mainland GDP = NOK %.2f bn; debate margin %.2f", asym_bias, pp_bn_2022, margin_debate)
w("   smallest loss rejected, NOK bn at 2022 values (studentised | unscaled): H1 %.1f | %.1f, H2 %.1f | %.1f, H2 net of bias %.1f, H2 reliefs added back net of bias %.1f",
  gm("h1", "student", DELTA1, "delta_min") * pp_bn_2022, gm("h1", "raw", DELTA1, "delta_min") * pp_bn_2022,
  gm("h2", "student", DELTA1, "delta_min") * pp_bn_2022, gm("h2", "raw", DELTA1, "delta_min") * pp_bn_2022,
  gm("h2_bias", "student", DELTA1, "delta_min") * pp_bn_2022, gm("h2_relief", "student", DELTA1, "delta_min") * pp_bn_2022)
w("   non-package PIT reliefs added back to H2 (NOK bn 2022/23/24: %s; mean %.3f pp): H2 %+.3f -> %+.3f, one-sided bound %+.3f, net of the asymmetric bias %+.3f",
  paste(sprintf("%.2f", NP_PIT_CUT_BN), collapse = "/"), mean(dh$adj_relief[dh$iso == "NOR" & dh$year >= 2022]),
  pr_h$coef, b_h, ge("west", "y_h2_relief", "student", "delta") - ge("west", "y_h2_relief", "student", "breakeven"),
  gm("h2_relief", "student", DELTA1, "delta_min"))
w("   rejection of the circulated figure (%.2f) survives a wealth tax behavioural loss of up to %.3f pp (NOK %.1f bn a year, %.1f x Ring 2026) in the main analysis and up to %.3f pp (NOK %.1f bn, %.1f x the loss the figure itself assumes, %.3f pp) with the reliefs added back",
  margin_debate, wt_loss_tol, wt_loss_tol * pp_bn_2022, wt_loss_tol * pp_bn_2022 / RING_WT_LOSS_BN,
  wt_loss_tol_rel, wt_loss_tol_rel * pp_bn_2022, wt_loss_tol_rel / wt_loss_claimed, wt_loss_claimed)

# Figure 4.2: smallest loss rejected per row, studentised (filled) and unscaled (hollow), with the five
# rungs of the ladder as reference lines
FIG_ROWS <- c(h1 = "H1 main analysis", h1_rrbound = "H1, A112 excess subtracted",
              h1_np = "H1, non-package measures subtracted",
              h1_npsym = "H1, non-package net of PIT reliefs subtracted",
              h2 = "H2 main analysis", h2_bias = "H2, net of the asymmetric bias",
              h2_relief = "H2, PIT reliefs added back, net of the bias")
stopifnot(identical(names(FIG_ROWS), names(LADDER_SERIES)))
bd <- tab_margin_ladder |> filter(abs(margin - DELTA1) < 1e-9) |>
  distinct(series, method, delta_min) |>
  filter(method == "student" | series %in% c("h1", "h2")) |>
  mutate(row = factor(FIG_ROWS[series], levels = rev(unname(FIG_ROWS))),
         statistic = ifelse(method == "student", "studentised (main analysis)", "unscaled (unscaled check)"),
         two = series %in% c("h1", "h2"),
         ypos = as.numeric(row) + ifelse(two, ifelse(method == "student", 0.16, -0.16), 0))
X_MAX <- 1.3; X_MIN <- -0.3
stopifnot(all(bd$delta_min > X_MIN), all(bd$delta_min < X_MAX))
bd_ref <- tibble(margin = MARGINS,
                 label = c("post-reform wealth tax (0.7)", "pre-reform wealth tax (0.5)",
                           "mechanical yield (0.34)", sprintf("circulated figure (%.2f)", margin_debate),
                           "strict Laffer margin (0)"))
fig_bounds <- ggplot(bd, aes(y = ypos)) +
  geom_vline(data = bd_ref, aes(xintercept = margin), linetype = "dotted", color = "grey45", linewidth = 0.45) +
  geom_segment(aes(x = delta_min, xend = X_MAX, yend = ypos, color = statistic, linewidth = statistic),
               alpha = 0.55, lineend = "butt") +
  geom_point(aes(x = delta_min, shape = statistic), size = 3, fill = "white", stroke = 0.9) +
  geom_text(aes(x = delta_min, label = gsub("-", "\u2212", sprintf("%.2f", delta_min)),
                vjust = ifelse(method == "student", -1.0, 2.1)), size = 2.9, color = "grey25") +
  scale_shape_manual(values = c("studentised (main analysis)" = 16, "unscaled (unscaled check)" = 21)) +
  scale_color_manual(values = c("studentised (main analysis)" = "grey30", "unscaled (unscaled check)" = "grey72")) +
  scale_linewidth_manual(values = c("studentised (main analysis)" = 1.6, "unscaled (unscaled check)" = 1.1)) +
  scale_y_continuous(breaks = seq_along(levels(bd$row)), labels = levels(bd$row),
                     expand = expansion(add = c(0.6, 0.9))) +
  scale_x_continuous(limits = c(X_MIN, X_MAX), breaks = seq(-0.2, X_MAX, 0.2), expand = expansion(mult = c(0, 0.01)),
                     sec.axis = dup_axis(breaks = bd_ref$margin, labels = bd_ref$label, name = NULL)) +
  labs(x = "Loss relative to the counterfactual (pp of mainland GDP)", y = NULL) +
  thesis_theme +
  theme(axis.text.x.top = element_text(angle = 45, hjust = 0, vjust = 0, size = 8.2, color = "grey20"),
        axis.ticks.x.top = element_blank(), panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 10, color = "grey20"),
        plot.margin = margin(8, 80, 8, 8)) +
  guides(linewidth = "none", color = "none")
save_fig(fig_bounds, "fig_bounds", width = 8, height = 4.7)
w("Files: tab_margin_ladder, fig_bounds (Figure 4.2)")


# ---------------------------------------------------------------------------
# 5. Start-year sensitivity (Figure A2.6)
# ---------------------------------------------------------------------------
START_YEARS <- 2010:2017
sy_rows <- list()
for (pool in c("nordic", "west")) for (y in c("y_main", "y_h2")) for (s0 in START_YEARS) {
  donors <- if (pool == "nordic") NORDIC4 else WEST_DONORS
  d <- panel_o |> filter(iso %in% c("NOR", donors), year >= s0, year != 2021) |>
    mutate(iso_f = factor(iso), year_f = factor(year)) |> add_ind()
  r <- coef_row(feols(fml_tw(y, "treat", d), data = d, cluster = ~iso_f))
  pl <- vapply(donors, function(g) {
    dg <- d |> filter(iso != "NOR") |> mutate(treat_pl = as.integer(iso == g & year >= 2022))
    coef_row(feols(fml_tw(y, "treat_pl", dg), data = dg, cluster = ~iso_f), "treat_pl")$coef
  }, numeric(1))
  sy_rows[[paste(pool, y, s0)]] <- tibble(
    pool = pool, outcome = y, start = s0, indicators = ind_rhs(d), coef = r$coef, se_cluster = r$se,
    n_obs = r$n, n_donors = length(donors), placebo_min = min(pl), placebo_max = max(pl),
    rank_raw = 1L + sum(abs(pl) >= abs(r$coef)))
}
tab_startyear <- bind_rows(sy_rows)
sy <- function(pool, y, s0) tab_startyear$coef[tab_startyear$pool == pool & tab_startyear$outcome == y & tab_startyear$start == s0]
check("7.7", abs(sy("nordic", "y_main", 2015) - bget("main", "y_main")) < 1e-9 &&
        abs(sy("nordic", "y_h2", 2015) - bget("main", "y_h2")) < 1e-9 &&
        abs(sy("west", "y_main", 2015) - gp("west", "main", "y_main", "coef")) < 1e-9 &&
        abs(sy("nordic", "y_main", 2010) - sv("main", "from 2010 excl 2021", "y_main")) < 1e-9,
      "start-year sensitivity: 2015 = main specification (both groups), 2010 = sample variant from 2010 excl. 2021",
      sprintf("Nordic y_main by start year: %s", paste(sprintf("%d %+.2f", START_YEARS,
                                                               sapply(START_YEARS, \(s0) sy("nordic", "y_main", s0))), collapse = ", ")))

SY_POOL <- c(nordic = "Nordic comparison group (4 donors)", west = "Western European comparison group (10 donors)")
SY_OUT  <- c(y_main = "Mainland tax revenue (H1)", y_h2 = "PIT plus net wealth tax (H2)")
sy_plot <- tab_startyear |>
  mutate(group = factor(SY_POOL[pool], levels = SY_POOL), panel = factor(SY_OUT[outcome], levels = SY_OUT),
         xpos = start + ifelse(pool == "nordic", -0.12, 0.12))
sy_margin <- tibble(panel = factor(SY_OUT, levels = SY_OUT), margin = -c(DELTA1, DELTA1))
fig_startyear <- ggplot(sy_plot, aes(xpos, coef, color = group, shape = group)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.4) +
  geom_hline(data = sy_margin, aes(yintercept = margin), linetype = "dotted", color = "grey30", linewidth = 0.5) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_linerange(aes(ymin = placebo_min, ymax = placebo_max), linewidth = 0.6, alpha = 0.45) +
  geom_point(size = 2.2) +
  facet_wrap(~panel, nrow = 1, scales = "free_y") +
  scale_color_manual(values = setNames(c("black", "grey55"), SY_POOL)) +
  scale_shape_manual(values = setNames(c(16, 17), SY_POOL)) +
  scale_x_continuous(breaks = START_YEARS) +
  labs(x = "First year of the pre-period", y = "Treatment effect (pp of GDP)") +
  thesis_theme + theme(legend.direction = "vertical", strip.text = element_text(size = 10, face = "bold"))
save_fig(fig_startyear, "fig_startyear", width = 9, height = 3.8)

stop_if_failed("Section 7")

# ---------------------------------------------------------------------------
# 5. Outputs
# ---------------------------------------------------------------------------
write_csv(tab_ladder, "Output/tables/tab_ladder.csv")
writeLines(c("tab_ladder -- specification ladder, TWFE FE iso+year + one-off indicators, cluster iso (G=5)",
             "Cumulative covariates; Brent enters as NOR x log(Brent) (collinear with year FE otherwise).", "",
             sprintf("%-5s %-7s %-18s %8s %8s %8s %4s", "sample", "outcome", "spec", "coef", "se", "p", "N"),
             sprintf("%-5s %-7s %-18s %8s %8s %8s %4d", tab_ladder$sample, tab_ladder$outcome, tab_ladder$spec,
                     fmt(tab_ladder$coef), fmt(tab_ladder$se), fmt(tab_ladder$p, 4), tab_ladder$n)),
           "Output/tables/tab_ladder.txt")
write_csv(tab_variants, "Output/tables/tab_variants.csv")
writeLines(c("tab_variants -- outcome, confounder and sample variants, TWFE (cluster iso)",
             "Samples: main = 2015-2024 excl. 2021 + ISL2016 indicator; full = 2010-2024 + ISL2016, DNK2014 indicators; noind = 2010-2024 without indicators.",
             "p_webb: Webb wild bootstrap p (B=9999) for the confounder variants (main, full).",
             sprintf("MDES_b(ex1300) = max |placebo|: %s.", paste(sprintf("%s %.3f", mdes_ex$sample, mdes_ex$mdes_b_ex1300), collapse = ", ")), "",
             sprintf("%-7s %-6s %-28s %-15s %8s %7s %7s %3s %7s %7s", "block", "sample", "variant", "outcome", "coef", "se", "p", "N", "rel_pct", "p_webb"),
             sprintf("%-7s %-6s %-28s %-15s %8s %7s %7s %3d %7s %7s", tab_variants$block, tab_variants$sample,
                     tab_variants$variant, tab_variants$outcome, fmt(tab_variants$coef), fmt(tab_variants$se),
                     fmt(tab_variants$p, 4), tab_variants$n,
                     ifelse(is.na(tab_variants$rel_pct), "", fmt(tab_variants$rel_pct, 2)),
                     ifelse(is.na(tab_variants$p_webb), "", fmt(tab_variants$p_webb, 4))),
             "", "notes:", sprintf("  %-28s %s", unique(tab_variants$variant[tab_variants$note != ""]),
                                   tab_variants$note[tab_variants$note != ""][!duplicated(tab_variants$variant[tab_variants$note != ""])])),
           "Output/tables/tab_variants.txt")
write_csv(tab_adj_detail, "Output/tables/tab_variants_adjusted.csv")
writeLines(c("tab_variants_adjusted -- ex-1300 placebos (MDES_b) and year effects of the confounder variants",
             "year_effect: manual BJS imputation (FE + indicators fitted on untreated obs), NOR 2022-2024.", "",
             sprintf("%-15s %-6s %-5s %-15s %5s %9s %8s %8s %4s", "type", "sample", "unit", "outcome", "year", "value", "se", "p", "N"),
             sprintf("%-15s %-6s %-5s %-15s %5s %9s %8s %8s %4d", tab_adj_detail$type, tab_adj_detail$sample,
                     tab_adj_detail$unit, tab_adj_detail$outcome,
                     ifelse(is.na(tab_adj_detail$year), "", as.character(tab_adj_detail$year)), fmt(tab_adj_detail$value),
                     ifelse(is.na(tab_adj_detail$se), "", fmt(tab_adj_detail$se)),
                     ifelse(is.na(tab_adj_detail$p), "", fmt(tab_adj_detail$p, 4)), tab_adj_detail$n)),
           "Output/tables/tab_variants_adjusted.txt")
write_csv(tab_startyear, "Output/tables/tab_startyear.csv")
write_csv(tab_inference_ext, "Output/tables/tab_inference_ext.csv")
write_csv(tab_placebos_ex1300, "Output/tables/tab_placebos_ex1300.csv")
write_csv(tab_scale_sens, "Output/tables/tab_scale_sens.csv")
writeLines(c("tab_scale_sens -- Table A1.13: studentised 90% set, smallest loss rejected (delta_min) and p(loss >= Delta1) on the Western European pool (main) as a function of s_NOR; Panel A y_main, Panel B y_h2",
             unlist(lapply(names(scale_sens_res), function(y) { o <- scale_sens_res[[y]]; tb <- o$tab
               c("", sprintf("%s: b_NOR %+.4f, max |t_g| %.3f (two-sided set), max t_g %.3f (one-sided loss test), n_pre %d, estimated s_NOR %.4f, chi-square 90%% interval for the true SD %.3f to %.3f, break-even s %.3f",
                             y, o$coef, o$tmax_two, o$tmax_one, o$n_pre, o$s, o$s_ci[1], o$s_ci[2], o$s_be), "",
                 sprintf("%7s %8s %8s %9s %9s %8s  %s", "s_NOR", "cs90_lo", "cs90_hi", "delta_min", "p_loss_d1", "rej_10", "note"),
                 sprintf("%7.4f %+8.3f %+8.3f %+9.3f %9.3f %8s  %s", tb$s_nor, tb$cs90_lo, tb$cs90_hi, tb$delta_min,
                         tb$p_loss_d1, tb$rejected_10, tb$note)) }))),
           "Output/tables/tab_scale_sens.txt")
writeLines(c("tab_inference_ext -- Table A1.8 Panel B: placebo inversion for the corrected outcomes (West, main), the West pool without Sweden, the West pool without France and the West pool with the ex-1300 control series",
             "Break-even = upward bias of Norway's estimate at which the rejection of a loss >= delta (10 %) is lost (positive = rejected).", "",
             sprintf("%-11s %-17s %-13s %-8s %2s %+8s %8s %7s %7s %+8s %+8s %+8s %+8s %6s %+9s",
                     "pool", "variant", "outcome", "method", "N", "coef", "s_NOR", "rank", "delta", "cs90_lo", "cs90_hi", "cs80_lo", "cs80_hi", "p_loss", "breakeven"),
             sprintf("%-11s %-17s %-13s %-8s %2d %+8.3f %8.3f %7d %7.2f %+8.2f %+8.2f %+8.2f %+8.2f %6.3f %+9.3f",
                     tab_inference_ext$pool, tab_inference_ext$variant, tab_inference_ext$outcome, tab_inference_ext$method,
                     tab_inference_ext$n_donors, tab_inference_ext$coef, tab_inference_ext$s_pre_nor, tab_inference_ext$rank,
                     tab_inference_ext$delta, tab_inference_ext$cs90_lo, tab_inference_ext$cs90_hi, tab_inference_ext$cs80_lo,
                     tab_inference_ext$cs80_hi, tab_inference_ext$p_loss_delta, tab_inference_ext$breakeven)),
           "Output/tables/tab_inference_ext.txt")

stab_line <- function(coefs, ps, base) {
  dev <- max(abs(coefs - base)); ok <- dev < 1.0 && all(ps > 0.05)
  sprintf("%s (max |dev.| %.2f pp, min p %.3f)", ifelse(ok, "stable: yes", "stable: no"), dev, min(ps))
}
w("1) Specification ladder:")
for (i in seq_len(nrow(tab_ladder))) { r <- tab_ladder[i, ]
  w("   %-5s %-7s %-18s %+8.3f  SE %6.3f  p %6.4f", r$sample, r$outcome, r$spec, r$coef, r$se, r$p) }
for (s in SAMPLES_MAIN) for (y in c("y_main", "y_h2")) { l <- tab_ladder |> filter(sample == s, outcome == y)
  w("   -> %s %s %s", s, y, stab_line(l$coef, l$p, bget(s, y))) }
w("2) Outcome and confounder variants:")
for (i in seq_len(nrow(tab_ovar))) { r <- tab_ovar[i, ]
  w("   %-6s %-15s %+8.3f  SE %6.3f  p %6.4f%s%s", r$sample, r$variant, r$coef, r$se, r$p,
    ifelse(is.na(r$p_webb), "", sprintf("  p_webb %.4f", r$p_webb)),
    ifelse(is.na(r$rel_pct), "", sprintf("  (%+.2f percent)", r$rel_pct))) }
w("   ex-1300 placebos: %s", paste(sprintf("%s/%s %+.3f", pl_ex$sample, pl_ex$unit, pl_ex$value), collapse = ", "))
w("   MDES_b(ex1300): %s", paste(sprintf("%s %.3f", mdes_ex$sample, mdes_ex$mdes_b_ex1300), collapse = ", "))
for (i in seq_len(nrow(ev_adj))) if (ev_adj$year[i] == 2022) { e3 <- ev_adj |> filter(sample == ev_adj$sample[i], outcome == ev_adj$outcome[i])
  w("   year effects %-5s %-15s %s", e3$sample[1], e3$outcome[1], paste(sprintf("%d %+.3f", e3$year, e3$value), collapse = "  ")) }
w("3) Sample variants:")
for (i in seq_len(nrow(tab_svar))) { r <- tab_svar[i, ]
  w("   %-5s %-7s %-28s %+8.3f  SE %6.3f  p %6.4f  N %2d", r$sample, r$outcome, r$variant, r$coef, r$se, r$p, r$n) }
for (s in SAMPLES_MAIN) for (y in c("y_main", "y_h2")) { l <- tab_svar |> filter(sample == s, outcome == y)
  w("   -> %s %s %s", s, y, stab_line(l$coef, l$p, bget(s, y))) }
w("4) Break-even bias (West, main; studentised | raw) and extended inversion:")
for (y in OUTC) w("   %-6s delta %.2f: break-even %+.3f | %+.3f  (rejected at 10 %%: %s | %s)", y, delta_of(y),
                   gi("west", "main", y, "student", "breakeven"), gi("west", "main", y, "raw", "breakeven"),
                   gi("west", "main", y, "student", "rejected_10"), gi("west", "main", y, "raw", "rejected_10"))
for (i in seq_len(nrow(tab_inference_ext))) { r <- tab_inference_ext[i, ]
  w("   %-11s %-17s %-13s %-8s N %d coef %+.3f rank %d/%d CS90 [%+.2f, %+.2f] p(loss>=%.2f) %.3f break-even %+.3f",
    r$pool, r$variant, r$outcome, r$method, r$n_donors, r$coef, r$rank, r$n_donors + 1L, r$cs90_lo, r$cs90_hi,
    r$delta, r$p_loss_delta, r$breakeven) }
w("   ex-1300 West placebos (raw series -> cleaned series; unit: coef | t_student): %s",
  paste(sprintf("%s %+.2f->%+.2f | %+.2f->%+.2f", tab_placebos_ex1300$unit, tab_placebos_ex1300$coef_raw_series, tab_placebos_ex1300$coef,
                tab_placebos_ex1300$t_student_raw_series, tab_placebos_ex1300$t_student), collapse = "; "))
invisible(lapply(names(scale_sens_res), function(y) { o <- scale_sens_res[[y]]
  w("   scale sensitivity (West main %s): s_NOR %.4f, chi-square 90%% interval %.3f to %.3f, break-even s %.3f; cs90_lo at the interval ends %+.2f / %+.2f; smallest loss rejected at the interval ends %+.2f / %+.2f",
    y, o$s, o$s_ci[1], o$s_ci[2], o$s_be, o$coef - o$tmax_two * o$s_ci[1], o$coef - o$tmax_two * o$s_ci[2],
    o$tmax_one * o$s_ci[1] - o$coef, o$tmax_one * o$s_ci[2] - o$coef) }))
w("5) Start-year sensitivity (coef; Nordic | West):")
for (y in c("y_main", "y_h2")) for (s0 in START_YEARS)
  w("   %-6s %d  Nordic %+6.3f [placebos %+.2f, %+.2f]  West %+6.3f [placebos %+.2f, %+.2f]", y, s0,
    sy("nordic", y, s0), tab_startyear$placebo_min[tab_startyear$pool == "nordic" & tab_startyear$outcome == y & tab_startyear$start == s0],
    tab_startyear$placebo_max[tab_startyear$pool == "nordic" & tab_startyear$outcome == y & tab_startyear$start == s0],
    sy("west", y, s0), tab_startyear$placebo_min[tab_startyear$pool == "west" & tab_startyear$outcome == y & tab_startyear$start == s0],
    tab_startyear$placebo_max[tab_startyear$pool == "west" & tab_startyear$outcome == y & tab_startyear$start == s0])
w("Files: tab_ladder, tab_variants, tab_variants_adjusted, tab_inference_ext, tab_startyear, fig_startyear")
w("")

##############################################################################
##############################################################################
# SECTION 8: TREATMENT FROM 2021, WEALTH TAX REALISED VS. EX ANTE, NAIVE 2X2 (Table A1.11)
##############################################################################
##############################################################################
# Panel A: treatment from 2021 (announcement), Nordic group, with country
# placebos. Panel B: wealth tax revenue (OECD 4210) relative to 2021 against
# the ex-ante estimates (MOF_WEALTH). Panel C: naive 2x2 comparison with
# country means (Iceland 2016 excluded).

set.seed(SEED)
section("Section 8: Treatment from 2021, realised vs. ex ante, naive 2x2")

var_rows <- list()
for (s in c("main", "full")) for (y in OUTC) {
  d <- panel |> filter(if (s == "main") year >= 2015 else TRUE) |>
    mutate(iso_f = factor(iso), year_f = factor(year), treat21 = as.integer(iso == "NOR" & year >= 2021)) |>
    add_ind()
  r <- coef_row(feols(fml_tw(y, "treat21", d), data = d, cluster = ~iso_f), "treat21")
  pl <- sapply(NORDIC4, function(g) {
    dg <- d |> filter(iso != "NOR") |> mutate(tp = as.integer(iso == g & year >= 2021))
    coef_row(feols(fml_tw(y, "tp", dg), data = dg, cluster = ~iso_f), "tp")$coef })
  var_rows[[paste(s, y)]] <- tibble(variant = "treatment from 2021 (announcement)", sample = if (s == "main") "2015-2024 (2021 treated)" else "2010-2024 (2021 treated)",
                                    outcome = y, coef = r$coef, se = r$se, p = r$p, n = r$n,
                                    rank_nordic = 1L + sum(abs(pl) >= abs(r$coef)), p_loss_nordic = (1L + sum(pl <= r$coef)) / 5,
                                    placebos = paste(sprintf("%s %.2f", NORDIC4, pl), collapse = ", "))
}
tab_treat2021 <- bind_rows(var_rows)

cm2 <- bind_rows(
  panel |> filter(!(iso == "ISL" & year == 2016), year %in% 2015:2020) |> group_by(iso) |> summarise(v = mean(y_main), .groups = "drop") |> mutate(per = "pre_main"),
  panel |> filter(year %in% 2010:2021) |> group_by(iso) |> summarise(v = mean(y_main), .groups = "drop") |> mutate(per = "pre_full"),
  panel |> filter(year >= 2022) |> group_by(iso) |> summarise(v = mean(y_main), .groups = "drop") |> mutate(per = "post")) |>
  pivot_wider(names_from = per, values_from = v)
naive <- bind_rows(
  tibble(window = "main 2015-2020 vs 2022-2024 (ISL 2016 excl.)", group = c("Norway", "Controls (mean of country means)"),
         pre = c(cm2$pre_main[cm2$iso == "NOR"], mean(cm2$pre_main[cm2$iso != "NOR"])),
         post = c(cm2$post[cm2$iso == "NOR"], mean(cm2$post[cm2$iso != "NOR"]))),
  tibble(window = "full 2010-2021 vs 2022-2024", group = c("Norway", "Controls (mean of country means)"),
         pre = c(cm2$pre_full[cm2$iso == "NOR"], mean(cm2$pre_full[cm2$iso != "NOR"])),
         post = c(cm2$post[cm2$iso == "NOR"], mean(cm2$post[cm2$iso != "NOR"])))) |>
  mutate(change = post - pre) |> group_by(window) |> mutate(did = change[group == "Norway"] - change[group != "Norway"]) |> ungroup()
check("8.1", abs(naive$change[2] - (-1.00)) < 0.02 && abs(naive$did[1] - 0.57) < 0.02,
      "naive 2x2 with country means: controls -1.00, DiD +0.57",
      sprintf("controls %.3f, DiD %.3f; full: controls %.3f, DiD %.3f", naive$change[2], naive$did[1], naive$change[4], naive$did[3]))

nor_w <- panel |> filter(iso == "NOR", year >= 2019) |>
  transmute(year, wealth_bn = xdc_4210 / 1e9, gdp_ml_bn = gdp_ml / 1e9, wealth_pct_ml = xdc_4210 / gdp_ml * 100, wealth_pct_oecd = y_wealth)
base21 <- nor_w |> filter(year == 2021)
mech_bn <- c(`2022` = MOF_WEALTH[["2022"]], `2023` = sum(MOF_WEALTH), `2024` = sum(MOF_WEALTH))   # MoF ex ante, 2022/2023 prices
wealth_tab <- nor_w |> filter(year >= 2022) |>
  mutate(change_bn = wealth_bn - base21$wealth_bn, change_pct_ml = wealth_pct_ml - base21$wealth_pct_ml,
         mech_bn = mech_bn[as.character(year)], mech_pct_ml_2022 = mech_bn / nor_w$gdp_ml_bn[nor_w$year == 2022] * 100,
         ratio_bn = change_bn / mech_bn, ratio_pct = change_pct_ml / mech_pct_ml_2022,
         base_growth_pct = (gdp_ml_bn / base21$gdp_ml_bn - 1) * 100)
check("8.2", abs(mean(wealth_tab$change_pct_ml - wealth_tab$mech_pct_ml_2022) - asym_bias) < 1e-9,
      "asymmetric bias of Section 7 = mean excess of the realised wealth tax increase over the ex-ante estimate (pp, Panel B)",
      sprintf("excess by year %s; mean %.4f", paste(sprintf("%d %+.3f", wealth_tab$year, wealth_tab$change_pct_ml - wealth_tab$mech_pct_ml_2022), collapse = ", "),
              mean(wealth_tab$change_pct_ml - wealth_tab$mech_pct_ml_2022)))

stop_if_failed("Section 8")
write_csv(tab_treat2021, "Output/tables/tab_treat2021.csv")
write_csv(wealth_tab, "Output/tables/tab_wealth_realized.csv")
write_csv(naive, "Output/tables/tab_naive.csv")

w("Treatment from 2021:"); for (k in seq_len(nrow(tab_treat2021))) { v <- tab_treat2021[k, ]
  w("   %-26s %-6s %+6.3f (%.3f) rank %d/5 p_loss %.2f | %s", v$sample, v$outcome, v$coef, v$se, v$rank_nordic, v$p_loss_nordic, v$placebos) }
w("Naive 2x2: main controls %.3f DiD %+.3f | full controls %.3f DiD %+.3f", naive$change[2], naive$did[1], naive$change[4], naive$did[3])
w("Wealth tax realised vs ex ante:"); for (k in seq_len(nrow(wealth_tab))) { v <- wealth_tab[k, ]
  w("   %d: NOK %.1f bn (+%.1f vs 2021) = %.3f%% ML GDP (+%.3f pp) | mech %.1f bn = %.3f pp | ratio NOK %.2f, pp %.2f | ML GDP +%.1f%% vs 2021",
    v$year, v$wealth_bn, v$change_bn, v$wealth_pct_ml, v$change_pct_ml, v$mech_bn, v$mech_pct_ml_2022, v$ratio_bn, v$ratio_pct, v$base_growth_pct) }
w("Files: tab_treat2021, tab_wealth_realized, tab_naive")
w("")

##############################################################################
##############################################################################
# SECTION 9: CALIBRATION (Table A1.12)
##############################################################################
##############################################################################
# Mechanical yield of the package against the behavioural losses implied by the elasticities of the
# literature. Percent = percent of 2022 mainland GDP (PIT measures: 2024). Inputs: EXTERNAL INPUTS.

set.seed(SEED)
section("Section 9: Calibration (Table A1.12)")

nor_ml <- panel |> filter(iso == "NOR") |> arrange(year)
gdp_bn <- setNames(nor_ml$gdp_ml / 1e9, nor_ml$year)              # mainland GDP, NOK bn
bn_pp  <- gdp_bn[["2022"]] / 100                                   # 1 pp of 2022 mainland GDP, NOK bn
pct22  <- function(bn) bn / gdp_bn[["2022"]] * 100

# 08815, 2022 (Table A2.1), NOK bn
w08815 <- function(comp) tab_wealth_stats[["2022"]][tab_wealth_stats$component == comp &
                                                       tab_wealth_stats$statistic == "amount_mill_nok"] / 1000
base22 <- w08815("Taxable net wealth")                             # taxable net wealth
rev22  <- w08815("Wealth tax")                                     # wealth tax revenue

# --- Mechanical yield -----------------------------------------------------------
mech_wealth <- sum(MOF_WEALTH)                                      # measures 2022 and 2023
mech_pit    <- sum(MOF_PIT)                                         # gross, before the reliefs
pit_pct     <- mech_pit / gdp_bn[["2024"]] * 100
wealth_pct  <- mech_wealth / gdp_bn[["2024"]] * 100                 # as pit_pct: 2024 mainland GDP
in_force    <- c(`2022` = MOF_WEALTH[["2022"]] + MOF_PIT[["div_2022"]],
                 `2023` = sum(MOF_WEALTH) + MOF_PIT[["div_2022"]] + MOF_PIT[["div_2023"]] +
                   MOF_PIT[["bracket_2023"]],
                 `2024` = mech_wealth + mech_pit)
pkg_pct     <- in_force / gdp_bn[names(in_force)] * 100             # package per year, % of mainland GDP of the year
pkg_range   <- c(mean(pkg_pct), pkg_pct[["2024"]])                  # average 2022-2024 until fully implemented

# --- Behavioural losses ---------------------------------------------------------
ntr <- function(rate, val) CAL$ret - rate * val                     # net return after wealth tax
ntr_shares <- c(ntr(CAL$rate_pre, CAL$val_shares_pre), ntr(CAL$rate_post, CAL$val_shares_post))
ntr_other  <- c(ntr(CAL$rate_pre, 1), ntr(CAL$rate_post, 1))
d_ntr      <- c(other = ntr_other[2] / ntr_other[1] - 1, shares = ntr_shares[2] / ntr_shares[1] - 1)
base_chg   <- c(CAL$elast_ret_lo * d_ntr[["other"]], CAL$elast_ret_hi * d_ntr[["shares"]])   # with respect to the net return
loss_base  <- -base_chg * rev22                                     # Jakobsen et al. (2020)
# Elasticity with respect to the net-of-tax rate 1 - tau (Seim, 2017): effective rate = rate x valuation
eff_shares <- c(CAL$rate_pre * CAL$val_shares_pre, CAL$rate_post * CAL$val_shares_post)
eff_other  <- c(CAL$rate_pre, CAL$rate_post)
d_ntax     <- c(other = (1 - eff_other[2]) / (1 - eff_other[1]) - 1, shares = (1 - eff_shares[2]) / (1 - eff_shares[1]) - 1)
base_chg_r <- c(CAL$elast_rate_lo * d_ntax[["other"]], CAL$elast_rate_hi * d_ntax[["shares"]])
loss_rate  <- -base_chg_r * rev22                                   # Seim (2017)
loss_migr  <- CAL$migr_per_krone * mech_wealth                      # Migration
loss_env   <- CAL$ch_base_per_pp * (CAL$rate_post - CAL$rate_pre) * 100 * rev22   # envelope
loss_cal     <- c(loss_rate[1], loss_base[2]) + loss_migr           # calibrated: smallest to largest national estimate
loss_cal_env <- loss_env + loss_migr                                # envelope plus migration

# --- Margins and bounds ---------------------------------------------------------
wt_nor <- nor_ml |> filter(year %in% 2022:2024) |>
  transmute(year, bn = xdc_4210 / 1e9, pct = xdc_4210 / gdp_ml * 100)
mdes_b_main <- main_tab$mdes_b[main_tab$sample == "main" & main_tab$outcome == "y_main"]
cs90_raw <- gi("west", "main", "y_main", "raw", "cs90_lo")
cs90_st  <- gi("west", "main", "y_main", "student", "cs90_lo")
lb90_st  <- gm("h1", "student", DELTA1, "delta_min")              # one-sided 90% bound (Section 7, Figure 4.2)

check("9.1", round(mean(wt_nor$pct), 1) == DELTA1,
      "Delta1 = mean net wealth tax revenue 2022-2024 in % of mainland GDP (rounded to 0.1)",
      sprintf("%.3f %%, NOK %.1f to %.1f bn", mean(wt_nor$pct), min(wt_nor$bn), max(wt_nor$bn)))
check("9.2", round(pit_pct, 2) == DELTA2,
      "Delta2 = gross mechanical PIT increase in % of 2024 mainland GDP (rounded to 0.01)",
      sprintf("NOK %.2f bn = %.4f %%", mech_pit, pit_pct))
check("9.3", round(mdes_b_main, 2) == DELTA[["mdes_1p75"]],
      "MDES_b (main, y_main) = 1.75, the loss threshold of Section 4",
      sprintf("%.4f", mdes_b_main))
check("9.4", abs(round(pkg_range[1], 2) - MARGINS[3]) < 1e-9 && abs(round(pkg_range[1], 2) - 0.34) < 1e-9,
      "ladder margin 0.34 (Table A1.14) = mechanical yield of the package, average 2022-2024 (rounded to 0.01)",
      sprintf("package average %.4f, fully implemented %.4f", pkg_range[1], pkg_range[2]))
check("9.5", abs(wealth_pct + pit_pct - pkg_pct[["2024"]]) < 1e-9,
      "Table A1.12: wealth tax and PIT measures in % of 2024 mainland GDP add up to the fully implemented package",
      sprintf("%.4f + %.4f = %.4f", wealth_pct, pit_pct, pkg_pct[["2024"]]))
stop_if_failed("Section 9")

# --- Table A1.12 --------------------------------------------------------------
f0 <- function(x) sprintf("%.0f", x); f1 <- function(x) sprintf("%.1f", x); f2 <- function(x) sprintf("%.2f", x)
rg <- function(f, lo, hi) paste0(f(lo), "-", f(hi))
cal_row <- function(block, component, basis, nok_lo, nok_hi, pct_lo, pct_hi, nok_txt, pct_txt)
  tibble(block, component, basis, nok_lo, nok_hi, pct_lo, pct_hi, nok_txt, pct_txt)
tab_calibration <- bind_rows(
  cal_row("Mechanical yield", "Wealth tax measures",
          "Ministry of Finance ex-ante estimates, 2022 and 2023 budgets",
          mech_wealth, mech_wealth, wealth_pct, wealth_pct,
          f1(mech_wealth), f2(wealth_pct)),
  cal_row("Mechanical yield", "PIT measures, gross",
          "Bracket tax and dividend tax, 2022 to 2024 budgets, net of reliefs about zero",
          mech_pit, mech_pit, pit_pct, pit_pct, f1(mech_pit), f2(pit_pct)),
  cal_row("Mechanical yield", "Package",
          "Average 2022-2024 to fully implemented (2024), percent of the year's mainland GDP",
          NA_real_, NA_real_, pkg_range[1], pkg_range[2], "", rg(f1, pkg_range[1], pkg_range[2])),
  cal_row("Behavioural loss", "Wealth tax base, elasticities with respect to the net-of-tax return",
          sprintf("Elasticity %s-%s (Jakobsen et al., 2020), net-of-tax return %s to %s percent, base %s to %s percent",
                  CAL$elast_ret_lo, CAL$elast_ret_hi, f0(100 * d_ntr[["other"]]), f0(100 * d_ntr[["shares"]]),
                  f0(100 * base_chg[1]), f0(100 * base_chg[2])),
          loss_base[1], loss_base[2], pct22(loss_base[1]), pct22(loss_base[2]),
          rg(f1, loss_base[1], loss_base[2]), rg(f2, pct22(loss_base[1]), pct22(loss_base[2]))),
  cal_row("Behavioural loss", "Wealth tax base, elasticities with respect to the net-of-tax rate",
          sprintf("Elasticity %s-%s (Seim, 2017), net-of-tax rate 1 - tau %.2f to %.2f percent, base %.2f to %.2f percent",
                  CAL$elast_rate_lo, CAL$elast_rate_hi, 100 * d_ntax[["other"]], 100 * d_ntax[["shares"]],
                  100 * base_chg_r[1], 100 * base_chg_r[2]),
          loss_rate[1], loss_rate[2], pct22(loss_rate[1]), pct22(loss_rate[2]),
          rg(f2, loss_rate[1], loss_rate[2]), rg(f2, pct22(loss_rate[1]), pct22(loss_rate[2]))),
  cal_row("Behavioural loss", "Migration, Swedish semi-elasticities",
          sprintf("%s kronor lost per krona raised, income taxes of leavers included", CAL$migr_per_krone),
          loss_migr, loss_migr, pct22(loss_migr), pct22(loss_migr), f1(loss_migr), f2(pct22(loss_migr))),
  cal_row("Behavioural loss", "Upper envelope: subnational base response",
          sprintf("%s percent of the base per percentage point (Switzerland), not applicable across a national border",
                  100 * CAL$ch_base_per_pp),
          loss_env, loss_env, pct22(loss_env), pct22(loss_env), f1(loss_env), f2(pct22(loss_env))),
  cal_row("Behavioural loss", "Taxes paid by the emigrants", "Annual tax payments of the emigrants reported in the debate (Chapter 2.2, Table A1.15)",
          CAL$debate_lo, CAL$debate_hi, pct22(CAL$debate_lo), pct22(CAL$debate_hi),
          rg(f1, CAL$debate_lo, CAL$debate_hi), rg(f2, pct22(CAL$debate_lo), pct22(CAL$debate_hi))),
  cal_row("Behavioural loss", "Calibrated total", "Smallest to largest national estimate plus migration, envelope in brackets",
          loss_cal[1], loss_cal[2], pct22(loss_cal[1]), pct22(loss_cal[2]),
          sprintf("%s (%s)", rg(f1, loss_cal[1], loss_cal[2]), f1(loss_cal_env)),
          sprintf("%s (%s)", rg(f2, pct22(loss_cal[1]), pct22(loss_cal[2])), f2(pct22(loss_cal_env)))),
  cal_row("Margins and bounds", "Delta1 (H1 and H2): revenue of the net wealth tax", "2022-2024 average",
          min(wt_nor$bn), max(wt_nor$bn), mean(wt_nor$pct), mean(wt_nor$pct),
          rg(f0, min(wt_nor$bn), max(wt_nor$bn)), f1(mean(wt_nor$pct))),
  cal_row("Margins and bounds", "Delta2 (PIT check): gross mechanical PIT increase", "As above",
          mech_pit, mech_pit, pit_pct, pit_pct, f1(mech_pit), f2(pit_pct)),
  cal_row("Margins and bounds", "Nordic detection threshold (MDES_b)",
          "Largest absolute Nordic placebo (Table A1.6)",
          mdes_b_main * bn_pp, mdes_b_main * bn_pp, mdes_b_main, mdes_b_main,
          f0(mdes_b_main * bn_pp), f2(mdes_b_main)),
  cal_row("Margins and bounds", "Two-sided 90 percent set, lower end, unscaled",
          "Western European pool, main sample (Table 4.1)",
          -cs90_raw * bn_pp, -cs90_raw * bn_pp, -cs90_raw, -cs90_raw, f0(-cs90_raw * bn_pp), f1(-cs90_raw)),
  cal_row("Margins and bounds", "Two-sided 90 percent set, lower end, studentised", "As above, placebos studentised",
          -cs90_st * bn_pp, -cs90_st * bn_pp, -cs90_st, -cs90_st, f0(-cs90_st * bn_pp), f1(-cs90_st)),
  cal_row("Margins and bounds", "Smallest loss rejected, one-sided, studentised",
          "One-sided 90 percent bound of the main analysis, Western European pool, main sample (Figure 4.2, Table A1.14)",
          lb90_st * bn_pp, lb90_st * bn_pp, lb90_st, lb90_st, f0(lb90_st * bn_pp), f2(lb90_st)))

write_csv(tab_calibration, "Output/tables/tab_calibration.csv")
writeLines(c(
  "tab_calibration -- Table A1.12: mechanical yield of the package against behavioural losses implied by the literature",
  sprintf("NOK billion per year; percent of mainland GDP 2022 (mechanical yields of the wealth tax and PIT measures: 2024). 1 pp = NOK %.1f bn at 2022 values.", bn_pp),
  "",
  sprintf("%-18s %-46s %-16s %s", "block", "component", "NOK bn", "% mainland GDP"),
  sprintf("%-18s %-46s %-16s %s", tab_calibration$block, tab_calibration$component,
          tab_calibration$nok_txt, tab_calibration$pct_txt),
  "", "Basis:",
  sprintf("  %-46s %s", tab_calibration$component, tab_calibration$basis)),
  "Output/tables/tab_calibration.txt")

w("Mainland GDP NOK bn: 2022 %.1f, 2023 %.1f, 2024 %.1f; 1 pp (2022) = NOK %.2f bn",
  gdp_bn[["2022"]], gdp_bn[["2023"]], gdp_bn[["2024"]], bn_pp)
w("08815 2022: taxable net wealth NOK %.1f bn, wealth tax NOK %.2f bn", base22, rev22)
w("Mechanical: wealth tax measures NOK %.3f bn (%.4f %% of 2024 GDP, %.4f %% of 2022 GDP as in Table A1.11), PIT gross NOK %.2f bn (%.4f %% of 2024 GDP)",
  mech_wealth, wealth_pct, pct22(mech_wealth), mech_pit, pit_pct)
w("Package by year (%% of the year's mainland GDP): %s; average %.3f, fully implemented %.3f",
  paste(sprintf("%s %.3f", names(pkg_pct), pkg_pct), collapse = ", "), pkg_range[1], pkg_range[2])
w("Net-of-tax return: shares %.2f -> %.2f %% (%+.1f %%), other assets %.2f -> %.2f %% (%+.1f %%)",
  100 * ntr_shares[1], 100 * ntr_shares[2], 100 * d_ntr[["shares"]],
  100 * ntr_other[1], 100 * ntr_other[2], 100 * d_ntr[["other"]])
w("Net-of-tax rate 1 - tau: shares %+.3f %%, other assets %+.3f %%", 100 * d_ntax[["shares"]], 100 * d_ntax[["other"]])
w("Base change (net-of-tax return, Jakobsen) %.2f to %.2f %%; (net-of-tax rate, Seim) %.3f to %.3f %%",
  100 * base_chg[1], 100 * base_chg[2], 100 * base_chg_r[1], 100 * base_chg_r[2])
w("Losses NOK bn: base (return) %.3f-%.3f, base (rate) %.4f-%.4f, migration %.3f, envelope %.3f, calibrated %.3f-%.3f (with envelope %.3f)",
  loss_base[1], loss_base[2], loss_rate[1], loss_rate[2], loss_migr, loss_env, loss_cal[1], loss_cal[2], loss_cal_env)
for (k in seq_len(nrow(tab_calibration)))
  w("   %-46s %-16s %s", tab_calibration$component[k], tab_calibration$nok_txt[k], tab_calibration$pct_txt[k])
w("Files: tab_calibration")
w("")

##############################################################################
##############################################################################
# SECTION 10: MAPPING OF ROBUSTNESS CHECKS TO ASSUMPTIONS (Table A1.2)
##############################################################################
##############################################################################
# Pure compilation, no estimation.

section("Section 10: Mapping of robustness checks to assumptions (Table A1.2)")

tab_assumption_map <- tribble(
  ~check, ~probes, ~reported_in,
  "Main specification: 2015-2024 excl. 2021, ISL2016 indicator", "(i) parallel trends (pre-period 2015-2020), (iv) anticipation (2021 excluded)", "Table 4.1",
  "Full sample 2010-2024 with ISL2016 and DNK2014 indicators", "(i) parallel trends: documented pre-2015 relative decline", "Table 4.1",
  "One-off indicators in all regressions (ISL2016, DNK2014)", "Measurement: one-off revenue (Icelandic bank resolution 2016, Danish capital pension restructuring 2014)", "Tables 4.1, A1.3 to A1.6, A1.8 to A1.11",
  "Specification ladder (covariates, NOR x log Brent)", "(i) robustness to observables (diagnostic only, bad controls)", "Table A1.4",
  "Outcome variants (gross GDP, A23, vintage, log per capita)", "Measurement: construction of the mainland outcome", "Table A1.5",
  "Resource rent bounding variant (A112 excess)", "(ii) no confounding treatments, revenue side", "Table A1.5",
  "Control-side variant: OECD 1300 removed (ex-1300)", "(ii) no confounding treatments, control side (DNK pension yield tax)", "Table A1.5",
  "Treated-side variant: non-package measures subtracted", "(ii) no confounding treatments, treated side (ex-ante bound, one-sided)", "Table A1.5",
  "Treated-side variant: non-package measures net of the PIT reliefs subtracted (symmetric)", "(ii) no confounding treatments, treated side (ex-ante, both directions)", "Table A1.5",
  "Maximal variant (ex-1300 + A112 excess + non-package)", "(ii) both sides, deliberate over-correction", "Table A1.5",
  "Sample variants: excl. 2024, incl. 2021, from 2010, start years 2010-2017, no indicators", "(i) and (iv), data status", "Table A1.5, Figure A2.6",
  "Leave-one-out controls", "(iii) no spillovers, no single-control dependence", "Table A1.5",
  "Country placebos, Nordic (per sample)", "Design-based inference, detection threshold MDES_b", "Table A1.6",
  "Country placebos, Western European pool (studentised)", "Design-based inference, confidence sets by test inversion", "Tables 4.1, A1.8, A1.9",
  "Timing placebos (fake 2018 in 2015-2020, fake 2016 in 2010-2021)", "(i) parallel trends, pre-period", "Table A1.6",
  "Event-study leads, Wald-HC1, linear drift", "(i) parallel trends, (iv) anticipation", "Table A1.3",
  "SDID on two donor pools, three panel variants (full, excl. 2021, main)", "(i) approximate parallel trends (reweighting), (iv) anticipation", "Table A1.7",
  "Western European pool: Northern and Western European OECD members, pre-2000 accession, no GDP distortion, no tax increase of its own", "Donor screening rule for (ii), control side", "Table A3.1",
  "Break-even bias for H1 and H2 at five margins (margin ladder), inversion for the corrected outcomes (A112 excess, non-package measures gross and net of the PIT reliefs, PIT reliefs added back to H2), for the pools without Sweden and without France and for the pool with the ex-1300 control series, and sensitivity of the studentised set to s_NOR", "(i) and (ii): size of the bias in Norway's post-period outcome that overturns each conclusion", "Tables A1.8, A1.13, A1.14, Figure 4.2")
write_csv(tab_assumption_map, "Output/tables/tab_assumption_map.csv")

writeLines(c("tab_assumption_map -- mapping of robustness checks -> assumptions (Table A1.2); (i) to (iv): assumptions from Chapter 3.4 in the order given there", "",
             sprintf("%-66s %-72s %s", "check", "probes", "reported_in"),
             sprintf("%-66s %-72s %s", tab_assumption_map$check, tab_assumption_map$probes, tab_assumption_map$reported_in)),
           "Output/tables/tab_assumption_map.txt")

w("Files: tab_assumption_map (Table A1.2)")
w("")


##############################################################################
# END
##############################################################################
section("End")
w("Checks: %d, of which FAIL: %d", length(check_log), sum(grepl("[FAIL]", check_log, fixed = TRUE)))
log_add("sessionInfo():")
log_add(paste(capture.output(sessionInfo()), collapse = "\n"))
cat("\nMasterarbeit.R: all 10 sections completed. Log: ", SUMMARY, "\n", sep = "")
