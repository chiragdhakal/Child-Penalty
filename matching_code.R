if (!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

library(haven)
library(tidyverse)

# CENSUS 2021 CHILD-PENALTY PSEUDO EVENT STUDY

# This script follows the Child Penalty Atlas logic with Nepal Census 2021 only:
#   1. Parents provide post-birth observations at t = age of oldest child.
#   2. Married childless adults provide surrogate pre-birth observations.
#   3. Coarsened exact matching is implemented as exact strata weights.

data_dir <- "data"
out_dir <- file.path(data_dir, "matched")
fig_dir <- file.path(out_dir, "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

survey_year <- 2021
married_code <- 2

# Atlas baseline: age at first birth 20-45 and child-penalty horizon 0-10.
birth_age_min <- 20
birth_age_max <- 45
pre_event_times <- -5:-1
post_event_times <- 0:10
all_event_times <- c(pre_event_times, post_event_times)
min_event_cell_n <- 200

# The variables from the replication literature are age, education, marital status, and rural/urban status.
# Caste and province are Nepal-specific additions. 
match_vars <- c(
  "birth_age",
  "marital_status",
  "urban_rural",
  "education",
  "caste",
  "prov"
)

read_census_extract <- function(file_name) {
  read_dta(file.path(data_dir, file_name)) %>%
    mutate(across(where(is.labelled), zap_labels))
}

prepare_parent_pool <- function(data, sex_label) {
  if (sex_label == "mother") {
    data %>%
      transmute(
        person_id = paste0(hhid, "-", idcode0),
        sex = "Women",
        source_group = "parent",
        survey_year = survey_year,
        observed_age = as.integer(mother_age),
        firstchild_age = as.integer(firstchild_age),
        secondchild_age = as.integer(secondchild_age),
        firstchild_sex = as.integer(firstchild_sex),
        secondchild_sex = as.integer(secondchild_sex),
        event_time = firstchild_age,
        birth_age = observed_age - firstchild_age,
        birth_year = survey_year - firstchild_age,
        marital_status = as.integer(mother_marital_status),
        urban_rural = as.integer(urban_rural),
        education = as.integer(mother_education),
        caste = as.integer(mother_caste),
        prov = as.integer(prov),
        dist = as.integer(dist),
        employed = as.integer(mother_employed),
        census_weight = as.numeric(individual_wt)
      )
  } else if (sex_label == "father") {
    data %>%
      transmute(
        person_id = paste0(hhid, "-", idcode0),
        sex = "Men",
        source_group = "parent",
        survey_year = survey_year,
        observed_age = as.integer(father_age),
        firstchild_age = as.integer(firstchild_age),
        secondchild_age = as.integer(secondchild_age),
        firstchild_sex = as.integer(firstchild_sex),
        secondchild_sex = as.integer(secondchild_sex),
        event_time = firstchild_age,
        birth_age = observed_age - firstchild_age,
        birth_year = survey_year - firstchild_age,
        marital_status = as.integer(father_marital_status),
        urban_rural = as.integer(urban_rural),
        education = as.integer(father_education),
        caste = as.integer(father_caste),
        prov = as.integer(prov),
        dist = as.integer(dist),
        employed = as.integer(father_employed),
        census_weight = as.numeric(individual_wt)
      )
  } else {
    stop("sex_label must be either 'mother' or 'father'.")
  }
}

add_child_composition <- function(parent_data) {
  parent_data %>%
    mutate(
      first_child_label = case_when(
        firstchild_sex == 1 ~ "Son",
        firstchild_sex == 2 ~ "Daughter",
        TRUE ~ NA_character_
      ),
      second_child_label = case_when(
        secondchild_sex == 1 ~ "Son",
        secondchild_sex == 2 ~ "Daughter",
        TRUE ~ NA_character_
      ),
      parity_group = case_when(
        is.na(secondchild_age) ~ "One observed child",
        !is.na(secondchild_age) ~ "Two or more observed children",
        TRUE ~ NA_character_
      ),
      first_child_sex_group = case_when(
        !is.na(first_child_label) ~ paste("First child:", first_child_label),
        TRUE ~ NA_character_
      ),
      first_two_sex_group = case_when(
        !is.na(first_child_label) & !is.na(second_child_label) ~
          paste0("First two: ", first_child_label, "-", second_child_label),
        TRUE ~ NA_character_
      )
    )
}

prepare_childless_pool <- function(data, sex_label) {
  if (sex_label == "mother") {
    data %>%
      transmute(
        person_id = paste0(hhid, "-", idcode0),
        sex = "Women",
        source_group = "childless",
        survey_year = survey_year,
        observed_age = as.integer(childless_women_age),
        firstchild_age = NA_integer_,
        birth_year = NA_integer_,
        marital_status = as.integer(childless_women_marital_status),
        urban_rural = as.integer(urban_rural),
        education = as.integer(childless_women_education),
        caste = as.integer(childless_women_caste),
        prov = as.integer(prov),
        dist = as.integer(dist),
        employed = as.integer(childless_women_employed),
        census_weight = as.numeric(individual_wt)
      )
  } else if (sex_label == "father") {
    data %>%
      transmute(
        person_id = paste0(hhid, "-", idcode0),
        sex = "Men",
        source_group = "childless",
        survey_year = survey_year,
        observed_age = as.integer(childless_men_age),
        firstchild_age = NA_integer_,
        birth_year = NA_integer_,
        marital_status = as.integer(childless_men_marital_status),
        urban_rural = as.integer(urban_rural),
        education = as.integer(childless_men_education),
        caste = as.integer(childless_men_caste),
        prov = as.integer(prov),
        dist = as.integer(dist),
        employed = as.integer(childless_men_employed),
        census_weight = as.numeric(individual_wt)
      )
  } else {
    stop("sex_label must be either 'mother' or 'father'.")
  }
}

valid_for_matching <- function(data, vars = match_vars) {
  data %>%
    filter(
      marital_status == married_code,
      !is.na(employed),
      !is.na(census_weight),
      census_weight > 0,
      !if_any(all_of(vars), is.na)
    )
}

make_event_study_sample <- function(parent_data, childless_data, sex_label) {
  parents_post <- parent_data %>%
    filter(
      event_time %in% post_event_times,
      between(birth_age, birth_age_min, birth_age_max)
    ) %>%
    valid_for_matching() %>%
    mutate(
      event_time = as.integer(event_time),
      age = observed_age,
      pseudo_source = "Observed parent",
      analysis_weight = census_weight
    )

  parent_cells <- parents_post %>%
    group_by(across(all_of(match_vars))) %>%
    summarise(
      parent_cell_n = n(),
      parent_cell_weight = sum(census_weight),
      .groups = "drop"
    )

  childless_clean <- childless_data %>%
    filter(!is.na(observed_age)) %>%
    valid_for_matching(vars = setdiff(match_vars, "birth_age"))

  controls_pre_raw <- tidyr::expand_grid(
    childless_clean,
    event_time = pre_event_times
  ) %>%
    mutate(
      event_time = as.integer(event_time),
      age = observed_age,
      birth_age = observed_age - event_time,
      pseudo_source = "Matched childless surrogate"
    ) %>%
    filter(between(birth_age, birth_age_min, birth_age_max)) %>%
    filter(!if_any(all_of(match_vars), is.na))

  supported_cells <- controls_pre_raw %>%
    semi_join(parent_cells, by = match_vars) %>%
    distinct(event_time, across(all_of(match_vars))) %>%
    count(across(all_of(match_vars)), name = "n_supported_pre_times") %>%
    filter(n_supported_pre_times == length(pre_event_times))

  parent_cells_supported <- parent_cells %>%
    inner_join(supported_cells, by = match_vars)

  parents_post <- parents_post %>%
    inner_join(parent_cells_supported, by = match_vars)

  controls_pre <- controls_pre_raw %>%
    inner_join(parent_cells_supported, by = match_vars) %>%
    group_by(event_time, across(all_of(match_vars))) %>%
    mutate(
      control_cell_n = n(),
      control_cell_weight = sum(census_weight)
    ) %>%
    ungroup() %>%
    mutate(
      cem_weight = parent_cell_weight / control_cell_weight,
      analysis_weight = census_weight * cem_weight
    )

  common_cols <- c(
    "person_id", "sex", "pseudo_source", "survey_year", "event_time", "age",
    "observed_age", "firstchild_age", "birth_age", "birth_year",
    "marital_status", "urban_rural", "education", "caste", "prov", "dist",
    "employed", "census_weight", "analysis_weight", "parent_cell_n",
    "parent_cell_weight", "n_supported_pre_times"
  )

  parents_post <- parents_post %>%
    mutate(
      cem_weight = 1,
      control_cell_n = NA_integer_,
      control_cell_weight = NA_real_
    )

  event_data <- bind_rows(
    parents_post %>% select(all_of(common_cols), cem_weight, control_cell_n, control_cell_weight),
    controls_pre %>% select(all_of(common_cols), cem_weight, control_cell_n, control_cell_weight)
  ) %>%
    mutate(
      sample = sex_label,
      event_time = as.integer(event_time),
      age = as.integer(age),
      event_time_factor = fct_relevel(factor(event_time, levels = all_event_times), "-1"),
      age_factor = factor(age),
      strata_id = as.integer(factor(paste(!!!syms(match_vars), sep = "_")))
    )

  diagnostics <- tibble(
    sample = sex_label,
    post_parent_rows_before_support = nrow(parent_data %>% filter(event_time %in% post_event_times)),
    post_parent_rows_after_filters = nrow(parent_cells %>% right_join(parents_post, by = match_vars)),
    post_parent_rows_used = sum(event_data$pseudo_source == "Observed parent"),
    childless_rows_available = nrow(childless_clean),
    surrogate_rows_used = sum(event_data$pseudo_source == "Matched childless surrogate"),
    matched_strata = n_distinct(event_data$strata_id),
    weighted_parent_total = sum(event_data$analysis_weight[event_data$pseudo_source == "Observed parent"]),
    weighted_surrogate_total_each_pre_event = sum(event_data$analysis_weight[event_data$event_time == "-1"])
  )

  match_counts <- controls_pre %>%
    distinct(event_time, across(all_of(match_vars)), control_cell_n) %>%
    inner_join(parent_cells_supported, by = match_vars) %>%
    summarise(
      mean_controls_per_parent_cell = weighted.mean(control_cell_n, parent_cell_n),
      median_controls_per_parent_cell = median(control_cell_n),
      p90_controls_per_parent_cell = quantile(control_cell_n, 0.90),
      max_controls_per_parent_cell = max(control_cell_n),
      .by = event_time
    ) %>%
    mutate(sample = sex_label, .before = 1)

  list(
    event_data = event_data,
    diagnostics = diagnostics,
    match_counts = match_counts
  )
}

extract_event_coefficients <- function(model, event_levels) {
  coefs <- coef(model)

  if (requireNamespace("sandwich", quietly = TRUE)) {
    vcov_mat <- sandwich::vcovHC(model, type = "HC1")
  } else {
    vcov_mat <- vcov(model)
  }

  map_dfr(event_levels, function(event_time_value) {
    if (event_time_value == "-1") {
      tibble(
        event_time = as.integer(event_time_value),
        term = NA_character_,
        alpha = 0,
        se = 0
      )
    } else {
      term <- paste0("event_time_factor", event_time_value)
      tibble(
        event_time = as.integer(event_time_value),
        term = term,
        alpha = unname(coefs[term] %||% NA_real_),
        se = if (term %in% rownames(vcov_mat)) sqrt(vcov_mat[term, term]) else NA_real_
      )
    }
  })
}

estimate_event_study <- function(event_data, sex_label) {
  model <- lm(
    employed ~ event_time_factor + age_factor,
    data = event_data,
    weights = analysis_weight
  )

  coef_table <- extract_event_coefficients(model, levels(event_data$event_time_factor))

  counterfactuals <- event_data %>%
    mutate(
      fitted_value = fitted(model),
      event_time_int = event_time
    ) %>%
    left_join(
      coef_table %>% select(event_time_int = event_time, alpha),
      by = "event_time_int"
    ) %>%
    mutate(counterfactual_employment = fitted_value - alpha)

  counterfactual_means <- counterfactuals %>%
    summarise(
      counterfactual_employment = weighted.mean(counterfactual_employment, analysis_weight),
      observed_employment = weighted.mean(employed, analysis_weight),
      n = n(),
      weighted_n = sum(analysis_weight),
      .by = event_time_int
    ) %>%
    rename(event_time = event_time_int)

  estimates <- coef_table %>%
    left_join(counterfactual_means, by = "event_time") %>%
    mutate(
      sex = sex_label,
      scaled_impact = alpha / counterfactual_employment,
      scaled_se = se / counterfactual_employment,
      ci_low = scaled_impact - 1.96 * scaled_se,
      ci_high = scaled_impact + 1.96 * scaled_se,
      unscaled_ci_low = alpha - 1.96 * se,
      unscaled_ci_high = alpha + 1.96 * se
    ) %>%
    select(
      sex, event_time, alpha, se, unscaled_ci_low, unscaled_ci_high,
      counterfactual_employment, observed_employment,
      scaled_impact, scaled_se, ci_low, ci_high, n, weighted_n
    )

  list(model = model, estimates = estimates)
}

make_child_penalty <- function(event_estimates) {
  event_estimates %>%
    select(sex, event_time, alpha, counterfactual_employment, scaled_impact) %>%
    pivot_wider(
      names_from = sex,
      values_from = c(alpha, counterfactual_employment, scaled_impact)
    ) %>%
    mutate(
      unscaled_child_penalty = alpha_Men - alpha_Women,
      scaled_child_penalty = scaled_impact_Men - scaled_impact_Women
    )
}

summarise_child_penalty <- function(child_penalty_by_event, group_type = "Overall", group_value = "Overall") {
  post_scaled <- child_penalty_by_event$scaled_child_penalty[child_penalty_by_event$event_time %in% post_event_times]
  pre_scaled <- child_penalty_by_event$scaled_child_penalty[child_penalty_by_event$event_time %in% pre_event_times]
  post_unscaled <- child_penalty_by_event$unscaled_child_penalty[child_penalty_by_event$event_time %in% post_event_times]
  pre_unscaled <- child_penalty_by_event$unscaled_child_penalty[child_penalty_by_event$event_time %in% pre_event_times]

  tibble(
    group_type = group_type,
    group_value = group_value,
    post_events_used = sum(!is.na(post_scaled)),
    pre_events_used = sum(!is.na(pre_scaled)),
    min_event_cell_n = min_event_cell_n,
    measure = c(
      "scaled_child_penalty_0_10",
      "scaled_child_penalty_pre",
      "scaled_child_penalty_0_10_net_pre",
      "unscaled_child_penalty_0_10",
      "unscaled_child_penalty_pre",
      "unscaled_child_penalty_0_10_net_pre"
    ),
    value = c(
      mean(post_scaled, na.rm = TRUE),
      mean(pre_scaled, na.rm = TRUE),
      mean(post_scaled, na.rm = TRUE) - mean(pre_scaled, na.rm = TRUE),
      mean(post_unscaled, na.rm = TRUE),
      mean(pre_unscaled, na.rm = TRUE),
      mean(post_unscaled, na.rm = TRUE) - mean(pre_unscaled, na.rm = TRUE)
    )
  ) %>%
    mutate(value_percent = 100 * value)
}

estimate_child_penalty_for_sample <- function(women_event_data, men_event_data, group_type = "Overall", group_value = "Overall") {
  women_model <- estimate_event_study(women_event_data, "Women")
  men_model <- estimate_event_study(men_event_data, "Men")

  event_estimates <- bind_rows(
    women_model$estimates,
    men_model$estimates
  ) %>%
    mutate(group_type = group_type, group_value = group_value, .before = 1)

  event_support <- event_estimates %>%
    select(sex, event_time, n, weighted_n) %>%
    pivot_wider(
      names_from = sex,
      values_from = c(n, weighted_n)
    ) %>%
    mutate(
      min_event_n = pmin(n_Men, n_Women),
      event_cell_reliable = min_event_n >= min_event_cell_n
    )

  child_penalty_by_event <- make_child_penalty(event_estimates) %>%
    left_join(event_support, by = "event_time") %>%
    mutate(
      unscaled_child_penalty = if_else(event_cell_reliable, unscaled_child_penalty, NA_real_),
      scaled_child_penalty = if_else(event_cell_reliable, scaled_child_penalty, NA_real_)
    ) %>%
    mutate(group_type = group_type, group_value = group_value, .before = 1)

  child_penalty_summary <- summarise_child_penalty(
    child_penalty_by_event,
    group_type = group_type,
    group_value = group_value
  )

  list(
    women_model = women_model$model,
    men_model = men_model$model,
    event_estimates = event_estimates,
    child_penalty_by_event = child_penalty_by_event,
    child_penalty_summary = child_penalty_summary
  )
}

run_heterogeneity_group <- function(women_parents, men_parents, childless_women_pool, childless_men_pool, group_var, group_type) {
  group_values <- sort(unique(c(women_parents[[group_var]], men_parents[[group_var]])))
  group_values <- group_values[!is.na(group_values)]

  map(group_values, function(group_value) {
    women_subset <- women_parents %>% filter(.data[[group_var]] == group_value)
    men_subset <- men_parents %>% filter(.data[[group_var]] == group_value)

    women_event <- make_event_study_sample(
      parent_data = women_subset,
      childless_data = childless_women_pool,
      sex_label = "Women"
    )

    men_event <- make_event_study_sample(
      parent_data = men_subset,
      childless_data = childless_men_pool,
      sex_label = "Men"
    )

    penalty <- estimate_child_penalty_for_sample(
      women_event$event_data,
      men_event$event_data,
      group_type = group_type,
      group_value = group_value
    )

    diagnostics <- bind_rows(women_event$diagnostics, men_event$diagnostics) %>%
      mutate(group_type = group_type, group_value = group_value, .before = 1)

    match_counts <- bind_rows(women_event$match_counts, men_event$match_counts) %>%
      mutate(group_type = group_type, group_value = group_value, .before = 1)

    list(
      event_estimates = penalty$event_estimates,
      child_penalty_by_event = penalty$child_penalty_by_event,
      child_penalty_summary = penalty$child_penalty_summary,
      diagnostics = diagnostics,
      match_counts = match_counts
    )
  })
}

plot_heterogeneity_penalty <- function(child_penalty_by_event, group_type, file_stub) {
  plot_data <- child_penalty_by_event %>%
    filter(group_type == !!group_type)

  p <- ggplot(plot_data, aes(x = event_time, y = 100 * scaled_child_penalty)) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey55") +
    geom_vline(xintercept = -0.5, linewidth = 0.35, linetype = "dashed", colour = "grey45") +
    geom_line(linewidth = 0.8, colour = "#111111") +
    geom_point(size = 1.5, colour = "#111111") +
    facet_wrap(~ group_value) +
    scale_x_continuous(breaks = seq(-5, 10, by = 5)) +
    labs(
      title = paste("Child Penalty by", group_type),
      x = "Years relative to first child birth",
      y = "Men's effect minus women's effect (percentage points)"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )

  ggsave(file.path(fig_dir, paste0(file_stub, ".png")), p, width = 9, height = 5.5, dpi = 300)
  ggsave(file.path(fig_dir, paste0(file_stub, ".pdf")), p, width = 9, height = 5.5)

  p
}

plot_event_study <- function(event_estimates, child_penalty_summary) {
  penalty_label <- child_penalty_summary %>%
    filter(measure == "scaled_child_penalty_0_10_net_pre") %>%
    pull(value_percent) %>%
    round(1)

  p <- ggplot(
    event_estimates,
    aes(x = event_time, y = 100 * scaled_impact, colour = sex, fill = sex)
  ) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey55") +
    geom_vline(xintercept = -0.5, linewidth = 0.35, linetype = "dashed", colour = "grey45") +
    geom_ribbon(aes(ymin = 100 * ci_low, ymax = 100 * ci_high), alpha = 0.12, colour = NA) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.8) +
    scale_x_continuous(breaks = all_event_times) +
    scale_colour_manual(values = c("Men" = "#737373", "Women" = "#111111")) +
    scale_fill_manual(values = c("Men" = "#737373", "Women" = "#111111")) +
    labs(
      title = "Child Penalty Event Study, Nepal Census 2021",
      subtitle = paste0("Average scaled child penalty, t = 0 to 10 net of pre-trend: ", penalty_label, "%"),
      x = "Years relative to first child birth",
      y = "Effect on employment (% of counterfactual)",
      colour = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )

  ggsave(file.path(fig_dir, "child_penalty_event_study_nepal_2021.png"), p, width = 8.5, height = 5.2, dpi = 300)
  ggsave(file.path(fig_dir, "child_penalty_event_study_nepal_2021.pdf"), p, width = 8.5, height = 5.2)

  p
}

plot_child_penalty <- function(child_penalty_by_event) {
  p <- ggplot(child_penalty_by_event, aes(x = event_time, y = 100 * scaled_child_penalty)) +
    geom_hline(yintercept = 0, linewidth = 0.35, colour = "grey55") +
    geom_vline(xintercept = -0.5, linewidth = 0.35, linetype = "dashed", colour = "grey45") +
    geom_line(linewidth = 0.9, colour = "#111111") +
    geom_point(size = 1.8, colour = "#111111") +
    scale_x_continuous(breaks = all_event_times) +
    labs(
      title = "Motherhood Penalty Relative to Fatherhood",
      x = "Years relative to first child birth",
      y = "Men's effect minus women's effect (percentage points)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title.position = "plot"
    )

  ggsave(file.path(fig_dir, "child_penalty_gap_nepal_2021.png"), p, width = 8.5, height = 5.2, dpi = 300)
  ggsave(file.path(fig_dir, "child_penalty_gap_nepal_2021.pdf"), p, width = 8.5, height = 5.2)

  p
}

childless_men <- read_census_extract("childless_men.dta")
childless_women <- read_census_extract("childless_women.dta")
respondent_father <- read_census_extract("respondent_father.dta")
respondent_mother <- read_census_extract("respondent_mother.dta")

prepared_mothers <- prepare_parent_pool(respondent_mother, "mother") %>%
  add_child_composition()
prepared_fathers <- prepare_parent_pool(respondent_father, "father") %>%
  add_child_composition()
prepared_childless_women <- prepare_childless_pool(childless_women, "mother")
prepared_childless_men <- prepare_childless_pool(childless_men, "father")

mother_event <- make_event_study_sample(
  parent_data = prepared_mothers,
  childless_data = prepared_childless_women,
  sex_label = "Women"
)

father_event <- make_event_study_sample(
  parent_data = prepared_fathers,
  childless_data = prepared_childless_men,
  sex_label = "Men"
)

event_study_data_2021 <- bind_rows(
  mother_event$event_data,
  father_event$event_data
)

match_diagnostics_2021 <- bind_rows(
  mother_event$diagnostics,
  father_event$diagnostics
)

match_counts_2021 <- bind_rows(
  mother_event$match_counts,
  father_event$match_counts
)

overall_penalty <- estimate_child_penalty_for_sample(
  mother_event$event_data,
  father_event$event_data
)

event_estimates_2021 <- overall_penalty$event_estimates
child_penalty_by_event_2021 <- overall_penalty$child_penalty_by_event
child_penalty_summary_2021 <- overall_penalty$child_penalty_summary

heterogeneity_results <- c(
  run_heterogeneity_group(
    women_parents = prepared_mothers,
    men_parents = prepared_fathers,
    childless_women_pool = prepared_childless_women,
    childless_men_pool = prepared_childless_men,
    group_var = "parity_group",
    group_type = "Parity"
  ),
  run_heterogeneity_group(
    women_parents = prepared_mothers,
    men_parents = prepared_fathers,
    childless_women_pool = prepared_childless_women,
    childless_men_pool = prepared_childless_men,
    group_var = "first_child_sex_group",
    group_type = "First child sex"
  ),
  run_heterogeneity_group(
    women_parents = prepared_mothers,
    men_parents = prepared_fathers,
    childless_women_pool = prepared_childless_women,
    childless_men_pool = prepared_childless_men,
    group_var = "first_two_sex_group",
    group_type = "First two child sex sequence"
  )
)

heterogeneity_event_estimates_2021 <- map_dfr(heterogeneity_results, "event_estimates")
heterogeneity_child_penalty_by_event_2021 <- map_dfr(heterogeneity_results, "child_penalty_by_event")
heterogeneity_child_penalty_summary_2021 <- map_dfr(heterogeneity_results, "child_penalty_summary")
heterogeneity_diagnostics_2021 <- map_dfr(heterogeneity_results, "diagnostics")
heterogeneity_match_counts_2021 <- map_dfr(heterogeneity_results, "match_counts")

event_plot <- plot_event_study(event_estimates_2021, child_penalty_summary_2021)
penalty_plot <- plot_child_penalty(child_penalty_by_event_2021)
parity_plot <- plot_heterogeneity_penalty(
  heterogeneity_child_penalty_by_event_2021,
  "Parity",
  "child_penalty_by_parity_nepal_2021"
)
first_child_sex_plot <- plot_heterogeneity_penalty(
  heterogeneity_child_penalty_by_event_2021,
  "First child sex",
  "child_penalty_by_first_child_sex_nepal_2021"
)
first_two_sex_plot <- plot_heterogeneity_penalty(
  heterogeneity_child_penalty_by_event_2021,
  "First two child sex sequence",
  "child_penalty_by_first_two_child_sex_nepal_2021"
)

write_dta(event_study_data_2021, file.path(out_dir, "event_study_data_2021.dta"))
write_csv(match_diagnostics_2021, file.path(out_dir, "match_diagnostics_2021.csv"))
write_csv(match_counts_2021, file.path(out_dir, "match_counts_2021.csv"))
write_csv(event_estimates_2021, file.path(out_dir, "event_estimates_2021.csv"))
write_csv(child_penalty_by_event_2021, file.path(out_dir, "child_penalty_by_event_2021.csv"))
write_csv(child_penalty_summary_2021, file.path(out_dir, "child_penalty_summary_2021.csv"))
write_csv(heterogeneity_event_estimates_2021, file.path(out_dir, "heterogeneity_event_estimates_2021.csv"))
write_csv(heterogeneity_child_penalty_by_event_2021, file.path(out_dir, "heterogeneity_child_penalty_by_event_2021.csv"))
write_csv(heterogeneity_child_penalty_summary_2021, file.path(out_dir, "heterogeneity_child_penalty_summary_2021.csv"))
write_csv(heterogeneity_diagnostics_2021, file.path(out_dir, "heterogeneity_diagnostics_2021.csv"))
write_csv(heterogeneity_match_counts_2021, file.path(out_dir, "heterogeneity_match_counts_2021.csv"))

saveRDS(overall_penalty$women_model, file.path(out_dir, "event_model_women_2021.rds"))
saveRDS(overall_penalty$men_model, file.path(out_dir, "event_model_men_2021.rds"))

print(match_diagnostics_2021)
print(child_penalty_summary_2021)
print(
  heterogeneity_child_penalty_summary_2021 %>%
    filter(measure == "scaled_child_penalty_0_10_net_pre") %>%
    select(group_type, group_value, value_percent)
)
