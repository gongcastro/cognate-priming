get_bvq <- function() {
  p <- bvq::bvq_participants()
  r <- bvq::bvq_responses(p)

  # get logs
  edu_dict <- c(
    "noeducation" = "No education",
    "primary" = "Primary",
    "secondary" = "Secondary",
    "complementary" = "Complementary",
    "vocational" = "Vocational",
    "university" = "University"
  )

  l <- bvq::bvq_logs(p, r) |>
    dplyr::filter(version %in% c("bvq-short", "bvq-lockdown")) |>
    # get maximum educational attainment of parents
    dplyr::mutate(edu_parent = apply(
      cbind(edu_parent1, edu_parent2), 1,
      \(x) max(x, na.rm = FALSE)
    ) |>
      factor(
        levels = names(edu_dict),
        labels = edu_dict
      )) |>
    dplyr::left_join(select(p, child_id, response_id, time),
      by = dplyr::join_by(child_id, response_id, time)
    ) |>
    dplyr::distinct(response_id, .keep_all = TRUE) |>
    dplyr::select(child_id, response_id, time, age, lp, version, dominance, edu_parent)

  v <- bvq::bvq_vocabulary(p, r, .scale = c("count", "prop")) |>
    dplyr::filter(type == "understands") |>
    dplyr::inner_join(l, by = join_by(child_id, response_id)) |>
    dplyr::filter(
      type == "understands",
      !(child_id %in% c(54487, 54840))
    ) |>
    dplyr::inner_join(l, by = dplyr::join_by(child_id, response_id, time)) |>
    dplyr::select(response_id, dplyr::matches("prop|count"), contents)

  bvq_data <- list(
    participants = p,
    responses = r,
    logs = l,
    vocabulary = v,
    pool = pool
  )

  saveRDS(bvq_data, file.path("data-raw", "stimuli", "bvq.rds"))
}
