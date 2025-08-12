get_pin("field_types") |>
  sbt(point == "temporal"
      # & field == "practice state or us territory"
      ) |>
  field_type_col() |>
  collapse::mtt(
    alias = ifelse(title == "Quality Payment Program Experience", "quality_payment", alias),
    type = cheapr::case(
      field %in_% c("practice state or us territory") ~ "state",
      field %in_% c("years in medicare") ~ "years",
    .default = type
    ))




qpp <- build(endpoint("quality_payment"), query(practice_state_or_us_territory = "GA"))@base

url <- map(qpp, function(x) {
  gremove(x, "/stats") |>
    greplace("size=1", "size=500")
})

qpp_ <- purrr::imap(url, function(x, i) {
  fastplyr::as_tbl(
    x |>
      httr2::request() |>
      httr2::req_error(is_error = ~ FALSE) |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE, check_type = FALSE)
  )
}) |>
  purrr::list_rbind(names_to = "year")

tfbin <- function(x) {
  # cheapr::val_match(x, "True" ~ 1L, "False" ~ 0L, .default = NA_integer_)
  cheapr::val_match(x, "True" ~ TRUE, "False" ~ FALSE, .default = NA)
}

over <- set_names(qpp_, clean_names(names(qpp_))) |>
  map_na_if() |>
  mtt(
    acr(
      c(
        dual_eligibility_ratio,
        final_score,
        payment_adjustment_percentage,
        complex_patient_bonus,
        quality_category_score,
        quality_category_weight,
        quality_improvement_score,
        quality_measure_score_1,
        quality_measure_score_2,
        quality_measure_score_3,
        quality_measure_score_4,
        quality_measure_score_5,
        quality_measure_score_6,
        quality_measure_score_7,
        quality_measure_score_9,
        quality_measure_score_10,
        quality_measure_score_11,
        promoting_interoperability_pi_category_score,
        promoting_interoperability_pi_category_weight,
        improvement_activities_ia_category_weight,
        cost_category_score,
        cost_category_weight,
        cost_measure_achievement_points_1,
        cost_measure_achievement_points_2,
        cost_measure_achievement_points_3,
        cost_measure_achievement_points_4,
        cost_measure_achievement_points_5,
        cost_measure_achievement_points_6,
        cost_measure_achievement_points_7,
        cost_measure_achievement_points_8,
        cost_measure_achievement_points_9,
        cost_measure_achievement_points_10,
        cost_measure_achievement_points_11,
        cost_measure_achievement_points_12,
        cost_measure_achievement_points_13,
        cost_measure_achievement_points_14,
        cost_measure_achievement_points_15,
        cost_measure_achievement_points_16,
        cost_measure_achievement_points_17,
        cost_measure_achievement_points_18,
        cost_measure_achievement_points_19,
        cost_measure_achievement_points_20,
        cost_measure_achievement_points_21,
        cost_measure_achievement_points_22,
        quality_improvement_bonus,
        cost_score,
        cost_measure_score_1,
        cost_measure_score_2
      ),
      as.numeric),
    acr(
      c(
        year,
        practice_size,
        years_in_medicare,
        npi,
        medicare_patients,
        allowed_charges,
        services,
        improvement_activities_ia_category_score,
        small_practice_bonus,
        quality_measure_score_8,
        quality_measure_id_11,
        pi_measure_score_1,
        pi_measure_score_2,
        pi_measure_score_3,
        pi_measure_score_4,
        pi_measure_score_5,
        pi_measure_score_6,
        pi_measure_score_7,
        pi_measure_score_8,
        pi_measure_score_9,
        pi_measure_score_10,
        pi_measure_score_11,
        ia_measure_score_1,
        ia_measure_score_2,
        ia_measure_score_3,
        ia_measure_score_4,
        cost_improvement_score,
        ia_score
        ),
      as.integer),
    acr(
      c(
        non_reporting,
        opted_into_mips,
        small_practice_status,
        rural_status,
        health_professional_shortage_area_status,
        ambulatory_surgical_center_based_status,
        hospital_based_status,
        non_patient_facing_status,
        facility_based_status,
        safety_net_status,
        extreme_uncontrollable_circumstance_euc,
        received_facility_score,
        quality_reweighting_euc,
        pi_reweighting_euc,
        pi_reweighting_hardship_exception,
        pi_reweighting_special_status_or_clinician_type,
        ia_reweighting_euc,
        ia_credit,
        cost_reweighting_euc,
        engaged,
        small_practitioner,
        rural_clinician,
        hpsa_clinician,
        ambulatory_surgical_center,
        hospital_based_clinician,
        non_patient_facing,
        facility_based,
        extreme_hardship,
        extreme_hardship_quality,
        quality_bonus,
        extreme_hardship_pi,
        pi_hardship,
        pi_reweighting,
        pi_bonus,
        extreme_hardship_ia,
        ia_study,
        extreme_hardship_cost
        ),
      tfbin),
    acr(
      c(
        reporting_option,
        participation_option,
        participation_type
      ),
    cheapr::as_factor)
    ) |>
  # dplyr::glimpse() |>
  cheapr::overview()

over$numeric |>
  fastplyr::as_tbl() |>
  print(n = 100)

over$logical |> fastplyr::as_tbl()

over$categorical |>
  fastplyr::as_tbl() |>
  sbt(is.na(n_levels) & n_unique > 1 & n_unique < 6) |>
  print(n = 200)
  _$col |>
  paste0(",") |>
  cat(sep = "\n")
