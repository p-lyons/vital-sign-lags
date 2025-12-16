

# set up y axis scales ---------------------------------------------------------

scales = 
  list(
    scale_y_continuous(limits = c(0, 360), breaks = seq(0, 360, 90)),
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 60)),
    scale_y_continuous(limits = c(0, 180), breaks = seq(0, 180, 60))
  )

# make figure ------------------------------------------------------------------

figure_01 =
  df |>
  fmutate(
    loc_cat  = if_else(loc_cat == "wards", "Wards", loc_cat),
    loc_cat  = factor(loc_cat, levels = c("ED", "Wards", "ICU")),
    rec_hour = lubridate::hour(recrd_dttm),
    rec_hour = qF(rec_hour),
    one      = 1L
  ) |>
  fgroup_by(measure, rec_hour, loc_cat) |>
  fsummarize(
    lmean = fmean(lag_min),
    lsd   = fsd(lag_min),
    n     = fsum(one)
  ) |>
  fmutate(
    se = lsd/sqrt(n),
    lo = lmean - 1.96*se,
    hi = lmean + 1.96*se
  ) |>
  ggplot(aes(x = rec_hour, y = lmean, color = measure, group = measure)) +
  geom_pointrange(aes(ymin = lo, ymax = hi)) +
  geom_line() +
  theme_bw(base_size = 16) +
  #coord_cartesian(ylim = c(0, 240)) +
  #scale_y_continuous(breaks = seq(0, 240, 30)) +
  facet_wrap(~loc_cat, ncol = 1, scales = "free_y") +
  labs(x = "Recorded Hour", y = "Mean Lag Time (min)", color = "Vital Sign") +
  scale_color_manual(values = vcolor) +
  facetted_pos_scales(y = scales)

# save figure ------------------------------------------------------------------

ggsave(
  here("figures", paste0("lag_by_vs_hour_loc_", today, ".pdf")),
  width = 11, height = 8.5, units = "in"
)
