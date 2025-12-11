
# setup ------------------------------------------------------------------------

## libraries -------------------------------------------------------------------

library(gtsummary)
library(tidytable)
library(collapse)
library(ggplot2)
library(stringr)
library(ggh4x)
library(here)

## helpers ---------------------------------------------------------------------

today  = format(Sys.Date(), "%y%m%d")
vitals = c("HR", "SBP", "RR", "SpO2", "Temperature")
vcolor = c("#003f5c", "#58508d", "#bc5090", "#ff6361", "#ffa600")

### outlier ranges -------------------------------------------------------------

out =
  tibble::tibble(
    measure = c("hr", "sbp", "dbp", "rr", "spo2", "temp"),
    lo      = c(060,   090,   050,   010,   090,   096.0),
    hi      = c(120,   180,   110,   020,   100,   100.3)
  )

# initial data prep ------------------------------------------------------------

## load data -------------------------------------------------------------------

vitals_files = list.files(here("clean"), pattern = "^vitals_.*\\.parquet$", full.names = TRUE)
latest_file  = vitals_files[which.max(file.mtime(vitals_files))]

df =
  arrow::open_dataset(latest_file)  |>
  dplyr::filter(!is.na(value)) |>
  dplyr::filter(measure != "dbp") |>
  dplyr::collect()

## flag abnormal vital signs ---------------------------------------------------

df =
  join(df, out, how = "left", multiple = T) |>
  fmutate(
    abnormal = if_else(value < lo | value > hi, "Abnormal", "Normal Range"),
    measure  = case_when(
      measure %in% c("sbp", "rr", "hr") ~ toupper(measure),
      measure == "temp"                 ~ "Temperature",
      TRUE                              ~ "SpO2"
    ),
    measure = factor(measure, levels = vitals)
  ) |>
  fselect(-hi, -lo)

## time in hospital ------------------------------------------------------------

df =
  df |>
  fmutate(
    h_since_admit     = as.numeric(difftime(recrd_dttm, admit_dttm), "hours"),
    h_day             = as.integer(floor(h_since_admit/24)),
    rec_hour          = lubridate::hour(recrd_dttm),
    nightshift_01     = if_else(rec_hour %in% c(7:18), 0L, 1L),
    rec_date          = lubridate::date(recrd_dttm),
    shift_7a          = lubridate::ymd_hms(paste0(rec_date, " 07:00:00")),
    shift_7p          = lubridate::ymd_hms(paste0(rec_date, " 19:00:00")),
    tmrw_7a           = shift_7a + lubridate::ddays(1L),
    h_to_shift_change = case_when(
      rec_hour < 07 ~ as.numeric(difftime(recrd_dttm, shift_7a), "hours"),
      rec_hour < 19 ~ as.numeric(difftime(recrd_dttm, shift_7p), "hours"),
      TRUE          ~ as.numeric(difftime(recrd_dttm, tmrw_7a),  "hours")
    )
  ) |>
  fselect(-admit_dttm)

rm(out); gc()

# frequency of observations ----------------------------------------------------

## round by 10 minutes

freqs = 
  fselect(df, csn, h_day, taken_dttm) |>
  funique() |>
  fgroup_by(csn, h_day) |>
  fnobs() |>
  fmutate(taken_freq = if_else(taken_dttm > 24, 25L, taken_dttm)) |>
  fselect(-taken_dttm)

df = join(df, freqs, how = "left", multiple = T)

rm(freqs); gc()

## confirm no duplicates by taken time -----------------------------------------

# df |>
#   ftransform(one = 1L) |>
#   fgroup_by(csn, measure, taken_dttm) |>
#   fmutate(
#     n        = fsum(one),
#     val_diff = fmax(value) - fmin(value),
#     lag_diff = fmax(lag_min) - fmin(lag_min)
#   ) |>
#   fungroup() |>
#   fselect(-admit_dttm, -recrd_dttm, -one) |>
#   funique() |>
#   fsubset(n > 1)

# basic results ----------------------------------------------------------------

fnunique(df$csn) # n encounters
fnunique(df$mrn) # n patients
fselect(df, csn, taken_dttm) |> funique() |> fnobs() # n unique measure times
fselect(df, csn, recrd_dttm) |> funique() |> fnobs() # n unique recording times
fselect(df, csn, measure, recrd_dttm) |> funique() |> fgroup_by(measure) |> fnobs()

## batch recording - overall
df |>
  ftransform(one = 1L) |>
  fgroup_by(csn, measure, recrd_dttm) |>
  fmutate(n = fsum(one)) |>
  fungroup() |>
  fsubset(n > 1) |>
  fselect(csn, recrd_dttm, one) |>
  funique()

## batch recording - by vital sign
df |>
  ftransform(one = 1L) |>
  fgroup_by(csn, measure, recrd_dttm) |>
  fsummarize(n = fsum(one)) |>
  fsubset(n > 1) |>
  select(measure, n) |>
  tbl_summary(by = measure) >
  fungroup() |>
  fselect(measure, csn, recrd_dttm, one) |>
  funique() |>
  fgroup_by(measure) |>
  fsummarize(n = fsum(one))

# n taken times
n_taken =
  fselect(df, measure, csn, taken_dttm, abnormal) |>
  funique() |>
  fgroup_by(measure, abnormal) |>
  fsummarize(n_taken = fnobs(csn))

# n recording times
n_rec =
  fselect(df, measure, csn, recrd_dttm, abnormal) |>
  funique() |>
  fgroup_by(measure, abnormal) |>
  fsummarize(n_rec = fnobs(csn))

# n batch recordings
frq_batch =
  fgroup_by(df, csn, measure, recrd_dttm, abnormal) |>
  fnobs() |>
  fselect(csn, measure, abnormal, recrd_dttm, n = mrn) |>
  fsubset(n > 1) |>
  ftransform(one = 1L) |>
  fgroup_by(measure, abnormal) |>
  fsummarize(n_batched = fsum(one))

# n per batch
n_batch =
  fgroup_by(df, csn, measure, recrd_dttm, abnormal) |>
  fnobs() |>
  fsubset(taken_dttm > 1) |>
  fselect(csn, measure, abnormal, recrd_dttm, n_per_batch = taken_dttm)

# table 1: values and lag by measurement and abnormal --------------------------

table_1 =
  join(df, n_batch, how = "left", multiple = T) |>
  fselect(measure, abnormal, value, n_per_batch, lag_min) |>
  fmutate(lag_30 = if_else(lag_min > 30, 1L, 0L)) |>
  fmutate(lag_60 = if_else(lag_min > 60, 1L, 0L)) |>
  fmutate(lag_90 = if_else(lag_min > 90, 1L, 0L)) |>
  tbl_strata(
    strata = measure,
    .tbl_fun =
      ~.x |>
      tbl_summary(
        by        = abnormal,
        missing   = "no",
        statistic = n_per_batch ~ "{median} ({p25}-{p75})"
      ) |>
      add_p(),
    .header = "**{strata}**, N = {n}"
  ) |>
  as_gt()

n_taken
n_rec
frq_batch
n_batch
table_1

gt::gtsave(table_1, here("tables", paste0("table_01_", today, ".docx")))

rm(n_taken, n_rec, n_batch, frq_batch, out); gc()

## p-values for abnormal/normal lag comparisons by measure
fsubset(df, measure == "HR") |> select(abnormal, lag_min) |> tbl_summary(by = abnormal) |> add_p()
fsubset(df, measure == "RR") |> select(abnormal, lag_min) |> tbl_summary(by = abnormal) |> add_p()


# models -----------------------------------------------------------------------

## GEE -------------------------------------------------------------------------

### can't have zero as an outcome for a gamma model ----------------------------

df = mutate(df, lag_min  = if_else(lag_min == 0, 0.01, lag_min))
df = mutate(df, abnormal = qF(abnormal))

## create wave variable for AR(1) autocorrelation -- computationally overwhelming
# df = 
#   roworder(df, taken_dttm, recrd_dttm, measure, value) |>
#   mutate(wave = dense_rank(taken_dttm), .by = csn)

### drop unneeded columns to maximize efficiency (smaller df) ------------------

df = select(df, -ends_with("dttm"), -starts_with("shift"), -tmrw_7a)
gc()

### fit model ------------------------------------------------------------------

library(geepack)

model <- geeglm(
  lag_min ~ h_to_shift_change + measure + loc_cat + abnormal + nightshift_01 + taken_freq,
  id     = csn,
  family = Gamma(link = "log"),
  corstr = "exchangeable",
  data   = df
)

### fit autoregressive model (fails) -------------------------------------------
# model2 <- geeglm(
#   lag_min ~ h_to_shift_change + measure + loc_cat + abnormal + nightshift_01 + taken_freq, 
#   id     = csn, 
#   waves  = wave,
#   family = Gamma(link = "log"), 
#   corstr = "ar1", 
#   data   = df
# )

# broom::tidy(model2, exponentiate = T, conf.int = T)
# 
# gc()

### model results --------------------------------------------------------------

broom::tidy(model, exponentiate = T, conf.int = T)

#### coefficients,  robust SE, marginal effects --------------------------------

beta_X                     = coef(model)["h_to_shift_change"]
vcov_matrix                = vcov(model)  # This is already the robust vcov for GEE
se_beta_X                  = sqrt(vcov_matrix["h_to_shift_change", "h_to_shift_change"])
predicted_Y                = predict(model, type = "response")
marginal_effects           = beta_X * predicted_Y
se_marginal_effects        = se_beta_X * predicted_Y
average_marginal_effect    = fmean(marginal_effects)
se_average_marginal_effect = fmean(se_marginal_effects)

z_value  = 1.96
lower_ci = average_marginal_effect - z_value * se_average_marginal_effect
upper_ci = average_marginal_effect + z_value * se_average_marginal_effect

cat("Average Marginal Effect:", average_marginal_effect, "\n")
cat("95% CI:", lower_ci, "to", upper_ci, "\n")

# table 2: values and lag by measurement and location --------------------------

table_2 =
  select(df, loc_cat, measure, value, lag_min) |>
  ftransform(measure = as.character(measure), loc_cat = as.character(loc_cat)) |>
  tbl_strata(
    strata = loc_cat,
    .tbl_fun =
      ~.x |>
      tbl_summary(by = measure, missing = "no") |>
      add_n(),
    .header = "**{strata}**, N = {n}"
  ) |>
  as_flex_table()

# abnormal values and lag times ------------------------------------------------

## check basic numbers ---------------------------------------------------------

# what percent of abnormal values are delayed > 30 minutes
abnormal_lag_30 =
  fsubset(df, abnormal == "Abnormal") |>
  ftransform(
    n = 1L,
    l = if_else(lag_min > 30, 1L, 0L)
  ) |>
  fsummarize(
    a = fsum(l),
    p = fsum(l)/fsum(n)
  )

abnormal_lag_60 =
  fsubset(df, abnormal == "Abnormal") |>
  ftransform(
    n = 1L,
    l = if_else(lag_min > 60, 1L, 0L)
  ) |>
  fsummarize(
    a = fsum(l),
    p = fsum(l)/fsum(n)
  )

abnormal_lag_90 =
  fsubset(df, abnormal == "Abnormal") |>
  ftransform(
    n = 1L,
    l = if_else(lag_min > 90, 1L, 0L)
  ) |>
  fsummarize(
    a = fsum(l),
    p = fsum(l)/fsum(n)
  )

abnormal_lag_30
abnormal_lag_60
abnormal_lag_90

# and by vital sign, too
fsubset(df, abnormal == "Abnormal") |>
  ftransform(
    n = 1L,
    l = if_else(lag_min > 30, 1L, 0L)
  ) |>
  fgroup_by(measure) |>
  fsummarize(
    a = fsum(l),
    p = fsum(l)/fsum(n)
  )