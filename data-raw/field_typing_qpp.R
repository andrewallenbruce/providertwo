field_types <- get_pin("field_types")

qpp_enums <- list(
  reporting_option = c("APM Performance Pathway", "Traditional MIPS"),
  participation_option = c("APM Entity", "Group", "Individual"),
  participation_type = c("MIPS APM", "Group", "Individual"),
  clinician_type = c(
    'Doctor of Medicine',
    'Nurse Practitioner',
    'Physician Assistant',
    'Anesthesiologist Assistant',
    'Clinical Social Worker',
    'Doctor of Optometry',
    'Registered Dietician/Nutrition Professional',
    'Physical Therapist',
    'Certified Registered Nurse Anesthetist',
    'Certified Nurse-Midwife',
    'Clinical Psychologist',
    'Occupational Therapist',
    'Qualified Audiologist',
    'Doctor of Dental Medicine/Doctor of Dental Surgery'
  ),
  clinician_specialty = c(
    'Emergency Medicine',
    'Neurosurgery',
    'Nurse Practitioner',
    'Ophthalmology',
    'Physician Assistant',
    'Hospitalist',
    'Anesthesiologist Assistant',
    'Cardiology',
    'Pathology',
    'Urology',
    'Neurology',
    'Critical Care (Intensivists)',
    'General Surgery',
    'Internal Medicine',
    'Otolaryngology',
    'Family Practice',
    'Anesthesiology',
    'Interventional Cardiology',
    'Orthopedic Surgery',
    'Licensed Clinical Social Worker',
    'Nephrology',
    'Obstetrics/Gynecology',
    'Hospice and Palliative Care',
    'Dermatology',
    'Physical Medicine and Rehabilitation',
    'Interventional Pain Management',
    'Surgical Oncology',
    'Endocrinology',
    'Optometry',
    'Thoracic Surgery',
    'Registered Dietician/Nutrition Professional',
    'Pulmonary Disease',
    'Diagnostic Radiology',
    'Physical Therapist in Private Practice',
    'Certified Registered Nurse Anesthetist (CRNA)',
    'Cardiac Electrophysiology',
    'Allergy/Immunology',
    'Certified Nurse Midwife',
    'Interventional Radiology',
    'Clinical Psychologist',
    'Podiatry',
    'Psychiatry',
    'Hematology/Oncology',
    'Advanced heart failure and transplant cardiology',
    'Colorectal Surgery (Formerly Proctology)',
    'Gastroenterology',
    'Infectious Disease',
    'Preventive Medicine',
    'Radiation Oncology',
    'Hematopoietic cell transplantation and cellular therapy',
    'Occupational Therapist in Private Practice',
    'Audiologist (Billing Independently)',
    'Vascular Surgery',
    'Sports Medicine',
    'Rheumatology',
    'Oral Surgery (Dentists Only)',
    'Gynecological/Oncology',
    'Pain Management',
    'Cardiac Surgery',
    'Maxillofacial Surgery',
    'Medical Oncology',
    'Geriatric Medicine',
    'Hematology',
    'Pediatric Medicine',
    'Sleep Medicine',
    'Hand Surgery',
    'Nuclear Medicine',
    'Missing',
    'General Practice',
    'Plastic and Reconstructive Surgery',
    'Ambulatory Surgical Center',
    'Independent Diagnostic Testing Facility (IDTF)',
    'Chiropractic',
    'Undetermined',
    'Single or Multispecialty Clinic or Group Practice'
  )
)

# field_types <- field_types[which_(field_types$point %in_% "current" & field_types$alias %in_% "qppe", invert = TRUE), ]

field_types |>
  field_type_col() |>
  sbt(alias == "qppe" & is.na(type)) |>
  fcount(field) |>
  roworder(-N) |>
  collapse::mtt(
    type = cheapr::case(
      # COUNTS
      field %in_% c("practice size") ~ "practice_size",
      field %in_% c("medicare patients") ~ "patients",
      field %in_% c("services") ~ "services",
      field %in_% c("allowed charges") ~ "allowed_charges",
      # FLAGS
      field %in_% c("opted into mips") ~ "opt_into_mips",
      # NUMERICS
      field %in_% c("final score") ~ "final_score",
      field %in_% c("payment adjustment percentage") ~ "pay_adj",
      field %in_% c("dual_eligibility_ratio") ~ "dual_ratio",
    .default = type)
  )

qppe_names <- field_types |> sbt(alias == "qppe") |> slt(field, year)

qppe_by_year <- qppe_names |> rsplit(~year)


field_list <- fastplyr::list_tidy(
  all = reduce(qppe_by_year, intersect),
  `2023` = setdiff(qppe_by_year$`2023`, all),
  `2022` = setdiff(qppe_by_year$`2022`, all),
  `2021` = setdiff(qppe_by_year$`2021`, all),
  `2020` = setdiff(qppe_by_year$`2020`, all),
  `2019` = setdiff(qppe_by_year$`2019`, all),
  `2018` = setdiff(qppe_by_year$`2018`, all),
  `2017` = setdiff(qppe_by_year$`2017`, all)
) |>
  map(\(x) kit::psort(x, nThread = 4L))

qpp <- build(endpoint("qppe"), query(practice_state_or_us_territory = any_of(c("GA", "NY"))))@base

url <- map(qpp, function(x) {
  gremove(x, "/stats") |>
    greplace("size=1", "size=500") |>
    greplace("offset=0", "offset=10000")
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

qpp_ <- set_names(qpp_, clean_names(names2(qpp_))) |>
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
    )





qpp_ |>
  fcount(clinician_specialty) |>
  _$clinician_specialty |>
  glue::single_quote() |>
  paste0(", ") |>
  cat(sep = "\n")

over <- qpp_ |> cheapr::overview()

over$numeric |>
  fastplyr::as_tbl() |>
  print(n = 100)

over$logical |>
  fastplyr::as_tbl() |>
  print(n = 100)

over$categorical |>
  fastplyr::as_tbl() |>
  print(n = 100)
  sbt(is.na(n_levels) & n_unique > 1 & n_unique < 6) |>
  print(n = 200)
  _$col |>
  paste0(",") |>
  cat(sep = "\n")
