# vital-sign-lags
Code repository for "Sociotechnical factors underlying vital sign recording lags in the electronic health record"

Analysis of documentation lag times for vital signs in hospitalized patients.

## Overview

This project examines the time between when vital signs are taken and when they are recorded in the electronic health record.

## Repository Structure
```
├── 01_setup.R      # Environment setup, package installation, helper objects
├── 02_analysis.R   # Data processing, descriptive statistics, GEE models
├── 03_figures.R    # Visualization
├── clean/          # Processed data files (not included)
├── tables/         # Generated tables
├── figures/        # Generated figures
└── output/         # Model logs and other output
```

## Usage

Run scripts in order:
```r
source("01_setup.R")
source("02_analysis.R")
source("03_figures.R")
```

The setup script will install any missing packages automatically.

## Data Availability

The data underlying this study contain protected health information from electronic health records and cannot be shared publicly. Analysis code is provided for transparency and methodological review.

## Requirements

R 4.0+ with the following packages: arrow, collapse, geepack, ggh4x, ggplot2, gtsummary, here, lubridate, stringr, tidytable
