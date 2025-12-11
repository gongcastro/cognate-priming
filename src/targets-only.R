library(targets)
library(stringdist)
library(ggplot2)
library(brms)

options(
  mc.cores = 2,
  brms.backend = "cmdstanr",
  knitr.duplicate.label = "allow",
  cli.progress_bar_style = "dot"
)

tar_load_globals()

tar_load(c(
  stimuli,
  participants,
  vocabulary,
  gaze,
  attrition_trials,
  attrition_participants
))


time_subset <- c(0.00, 2.00)

d_g <- filter(
  gaze,
  phase == "Target-Distractor",
  timestamp >= time_subset[1],
  timestamp < time_subset[2]
) |>
  select(
    session_id,
    trial,
    phase,
    timestamp,
    is_gaze_target,
    is_gaze_distractor,
    trial_type
  )

d_p <- select(
  participants,
  child_id,
  list,
  location,
  test_language,
  version,
  session_id,
  age_group,
  age,
  lp
)

d_s <- stimuli |>
  unnest_wider(freq) |>
  unnest_wider(xsampa) |>
  mutate(target_lv = stringsim(xsampa_target, xsampa_t_target)) |>
  select(
    trial,
    test_language,
    version,
    trial,
    list,
    target,
    xsampa_target,
    xsampa_t_target,
    distractor,
    target_lv,
    freq = freq_target
  )

d_v <- rename_with(
  vocabulary,
  \(x) gsub("_prop", "", paste0("voc_", x)),
  matches("_prop")
)

d_at <- filter(attrition_trials, is_valid_trial) |>
  select(session_id, trial, samples, is_valid_trial)

d_ap <- filter(attrition_participants, is_valid_participant) |>
  select(session_id)

dat <- d_g |>
  inner_join(d_at) |>
  inner_join(d_ap) |>
  # aggregate across trials by participant, time bin and condition
  # see Chow et al. (2018)
  summarise(
    .sum_t = sum(is_gaze_target, na.rm = TRUE),
    .sum_d = sum(is_gaze_distractor, na.rm = TRUE),
    .ntrials = length(unique(trial)),
    .by = c(session_id, trial)
  ) |>
  inner_join(d_p) |>
  inner_join(d_v) |>
  inner_join(d_s) |>
  select(
    child_id,
    session_id,
    age_group,
    age,
    lp,
    voc_l1,
    voc_total,
    trial,
    target,
    xsampa_target,
    xsampa_t_target,
    target_lv,
    .sum_t,
    .sum_d
  ) |>
  # empirical logit with adjustment
  # see Barr et al. (2008)
  mutate(
    .nsamples = .sum_t + .sum_d,
    .prop = if_else(.nsamples == 0, NA_real_, .sum_t / .nsamples),
    .elog = if_else(
      .nsamples == 0,
      NA_real_,
      log((.sum_t + .5) / (.sum_d + .5))
    )
  ) |>
  filter(.nsamples > 0) |>
  mutate(across(c(.elog, .prop), \(x) zoo::na.locf(x, na.rm = TRUE))) |>
  arrange(desc(session_id)) |>
  mutate(
    across(c(.nsamples), as.integer),
    across(c(child_id, session_id), as.factor),
    across(
      c(age, matches("voc_")),
      \(x) scale(x, scale = TRUE)[, 1],
      .names = "{.col}_std"
    )
  ) |>
  select(
    child_id,
    session_id,
    age_group,
    age,
    lp,
    voc_l1,
    voc_total,
    target,
    xsampa_target,
    xsampa_t_target,
    target_lv,
    .sum_t,
    .sum_d,
    .prop,
    .elog,
    .nsamples,
    matches("_std")
  )

if (length(levels(dat$age_group)) > 1) {
  dat$lp <- factor(
    dat$lp,
    levels = c("Monolingual (English)", "Monolingual", "Bilingual")
  )
  contrasts(dat$lp) <- cbind(
    c(-5, 0.25, 0.25),
    c(0, -0.5, 0.5)
  )
}


if (length(levels(dat$age_group)) > 1) {
  dat$age_group <- factor(
    dat$age_group,
    levels = c("21 months", "25 months", "30 months")
  )
  contrasts(dat$age_group) <- cbind(
    c(-0.5, 0.25, 0.25),
    c(0, -0.5, 0.5)
  )
}


dat |>
  summarise(
    .elog = mean(.elog, na.rm = TRUE),
    .by = c(lp, target_lv)
  ) |>
  ggplot(aes(target_lv, .elog)) +
  facet_wrap(~lp) +
  geom_point() +
  geom_smooth(method = "lm")


# fit model
model_opts <- list(adapt_delta = 0.9, max_treedepth = 15)

fit <- brm(
  .elog ~ target_lv *
    lp +
    age_std +
    (1 + target_lv + age_std | child_id) +
    (1 + target_lv | child_id:session_id),
  data = dat,
  prior = prior(normal(0, 0.5), class = "Intercept") +
    prior(normal(0, 0.5), class = "b") +
    prior(exponential(6), class = "sd") +
    prior(lkj(6), class = "cor") +
    prior(exponential(6), class = "sigma"),
  iter = 1e3L,
  chains = 8L,
  cores = 8L,
  init = 0.1,
  file_refit = "on_change",
  file = file.path("out", "extra-model.rds"),
  seed = 1234,
  backend = "cmdstanr",
  control = model_opts,
  silent = 2,
)

fit_1 <- brm(
  .elog ~ target_lv *
    lp *
    age_std +
    (1 + target_lv + age_std | child_id) +
    (1 + target_lv | child_id:session_id),
  data = dat,
  prior = prior(normal(0, 0.5), class = "Intercept") +
    prior(normal(0, 0.5), class = "b") +
    prior(exponential(6), class = "sd") +
    prior(lkj(6), class = "cor") +
    prior(exponential(6), class = "sigma"),
  iter = 1e3L,
  chains = 8L,
  cores = 8L,
  init = 0.1,
  file_refit = "on_change",
  file = file.path("out", "extra-model-1.rds"),
  seed = 1234,
  backend = "cmdstanr",
  control = model_opts,
  silent = 2,
)

epreds <- expand_grid(
  lp = unique(dat$lp),
  target_lv = seq(0, 1, 0.1),
  age_std = c(-1, 0, 1)
) |>
  add_epred_draws(fit, re_formula = NA) |>
  mean_qi()

ggplot(epreds, aes(target_lv, .epred, colour = lp, fill = lp)) +
  facet_wrap(~age_std) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.35, linewidth = 0) +
  geom_line(linewidth = 3 / 4) +
  geom_point(
    data = summarise(dat, .elog = mean(.elog), .by = c(lp, target_lv)),
    aes(y = .elog)
  )


epreds <- expand_grid(
  lp = unique(dat$lp),
  target_lv = seq(0, 1, 0.1),
  age_std = c(-1, 0, 1)
) |>
  add_epred_draws(fit_1, re_formula = NA) |>
  mean_qi()

ggplot(epreds, aes(target_lv, .epred, colour = lp, fill = lp)) +
  facet_wrap(~age_std) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.35, linewidth = 0) +
  geom_line(linewidth = 3 / 4) +
  geom_point(
    data = summarise(dat, .elog = mean(.elog), .by = c(lp, target_lv)),
    aes(y = .elog)
  )

fit_2 <- brm(
  .elog ~ target_lv *
    lp *
    voc_l1_std +
    (1 + target_lv + voc_l1_std | child_id) +
    (1 + target_lv | child_id:session_id),
  data = dat,
  prior = prior(normal(0, 0.5), class = "Intercept") +
    prior(normal(0, 0.5), class = "b") +
    prior(exponential(6), class = "sd") +
    prior(lkj(6), class = "cor") +
    prior(exponential(6), class = "sigma"),
  iter = 1e3L,
  chains = 8L,
  cores = 8L,
  init = 0.1,
  file_refit = "on_change",
  file = file.path("out", "extra-model-2.rds"),
  seed = 1234,
  backend = "cmdstanr",
  control = model_opts,
  silent = 2,
)

epreds <- expand_grid(
  lp = unique(dat$lp),
  target_lv = seq(0, 1, 0.1),
  voc_l1_std = c(-1, 0, 1)
) |>
  add_epred_draws(fit_2, re_formula = NA) |>
  mean_qi()

ggplot(epreds, aes(target_lv, .epred, colour = lp, fill = lp)) +
  facet_wrap(~voc_l1_std) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.35, linewidth = 0) +
  geom_line(linewidth = 3 / 4) +
  geom_point(
    data = summarise(dat, .elog = mean(.elog), .by = c(lp, target_lv)),
    aes(y = .elog)
  )

fit_3 <- brm(
  .elog ~ target_lv *
    lp *
    voc_total_std +
    (1 + target_lv + voc_total_std | child_id) +
    (1 + target_lv | child_id:session_id),
  data = dat,
  prior = prior(normal(0, 0.5), class = "Intercept") +
    prior(normal(0, 0.5), class = "b") +
    prior(exponential(6), class = "sd") +
    prior(lkj(6), class = "cor") +
    prior(exponential(6), class = "sigma"),
  iter = 1e3L,
  chains = 8L,
  cores = 8L,
  init = 0.1,
  file_refit = "on_change",
  file = file.path("out", "extra-model-2.rds"),
  seed = 1234,
  backend = "cmdstanr",
  control = model_opts,
  silent = 2,
)

epreds <- expand_grid(
  lp = unique(dat$lp),
  target_lv = seq(0, 1, 0.1),
  voc_total_std = c(-1, 0, 1)
) |>
  add_epred_draws(fit_3, re_formula = NA) |>
  mean_qi()

ggplot(epreds, aes(target_lv, .epred, colour = lp, fill = lp)) +
  facet_wrap(~voc_total_std) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.35, linewidth = 0) +
  geom_line(linewidth = 3 / 4) +
  geom_point(
    data = summarise(dat, .elog = mean(.elog), .by = c(lp, target_lv)),
    aes(y = .elog)
  )

loos <- loo_compare(loo(fit), loo(fit_1), loo(fit_2), loo(fit_3))
