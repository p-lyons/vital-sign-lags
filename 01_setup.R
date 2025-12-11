
# 01_setup.R -------------------------------------------------------------------

# packages ---------------------------------------------------------------------

## list of required packages
packages = c(
  "arrow",
  "collapse",
  "geepack",
  "ggh4x",
  "ggplot2",
  "gtsummary",
  "here",
  "lubridate",
  "stringr",
  "tidytable"
)

## install missing packages
installed = packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

rm(packages, installed)

## load libraries
library(gtsummary)
library(tidytable)
library(collapse)
library(ggplot2)
library(stringr)
library(ggh4x)
library(here)

# directories ------------------------------------------------------------------

if (!dir.exists(here("tables")))  dir.create(here("tables"))
if (!dir.exists(here("figures"))) dir.create(here("figures"))
if (!dir.exists(here("output"))) dir.create(here("output"))

# helpers ----------------------------------------------------------------------

today  = format(Sys.Date(), "%y%m%d")
vitals = c("HR", "SBP", "RR", "SpO2", "Temperature")
vcolor = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

## outlier ranges
out =
  tidytable::tidytable(
    measure = c("hr", "sbp", "dbp", "rr", "spo2", "temp"),
    lo      = c(060,   090,   050,   010,   090,   096.0),
    hi      = c(120,   180,   110,   020,   100,   100.3)
  )