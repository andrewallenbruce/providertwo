#' @autoglobal
#' @noRd
na_if <- function(x, y = "") {
  vctrs::vec_slice(x, vctrs::vec_in(x, y, needles_arg = "x", haystack_arg = "y")) <- NA
  x
}

#' @autoglobal
#' @noRd
map_na_if <- function(i) {
  modify_if(i, is.character, function(x)
    na_if(x, y = ""))
}

#' @autoglobal
#' @noRd
make_address <- function(a1, a2) {
  cheapr_if_else(!is_na(a2), paste(a1, a2), a1)
}

#' @autoglobal
#' @noRd
clean_names <- function(x) {
  gremove(greplace(tolower(x), "\\s|-", "_"), "\\(|\\)")
}

#' @autoglobal
#' @noRd
set_clean <- function(i, x) {
  set_names(i, clean_names(x))
}

#' @autoglobal
#' @noRd
fmt_entity <- function(x, type = c("int", "chr")) {

  type <- match.arg(type, c("int", "chr"))

  switch(
    type,
    int = val_match(x, 1 ~ "I", 2 ~ "O"),
    chr = val_match(x, "NPI-1" ~ "I", "NPI-2" ~ "O")
  ) |>
    factor_()
}

#' @autoglobal
#' @noRd
underscore <- function(x) {
  greplace(x, "___owner$", "_owner")
}

#' @autoglobal
#' @noRd
charbin <- function(x) {
  val_match(x, "N" ~ 0L, "Y" ~ 1L, .default = NA_integer_)
}

#' @autoglobal
#' @noRd
charprop <- function(x) {
  case(x == "0" ~ 0, is_na(x) ~ NA_real_, .default = as.double(x) / 100)
}

#' @noRd
#' @autoglobal
care_names <- function(x, call = caller_env()) {
  switch(
    x,
    enrollees = c(
      NPI                = "npi",
      PECOS_ASCT_CNTL_ID = "pac",
      ENRLMT_ID          = "enid",
      PROVIDER_TYPE_CD   = "specialty_code",
      PROVIDER_TYPE_DESC = "specialty_description",
      STATE_CD           = "state",
      FIRST_NAME         = "first_name",
      MDL_NAME           = "middle_name",
      LAST_NAME          = "last_name",
      ORG_NAME           = "org_name"
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}

#' @noRd
#' @autoglobal
pro_names <- function(x, call = caller_env()) {
  switch(
    x,
    affiliations = c(
      ind_pac_id                                 = "pac",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      facility_affiliations_certification_number = "ccn_facility",
      facility_type_certification_number         = "ccn_primary"
    ),
    clinicians = c(
      ind_pac_id                                 = "pac",
      ind_enrl_id                                = "enid",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      suff                                       = "suffix_name",
      cred                                       = "credential",
      gndr                                       = "gender",
      grd_yr                                     = "grad_year",
      med_sch                                    = "med_school",
      pri_spec                                   = "spec_prim",
      sec_spec_all                               = "spec_sec",
      telehlth                                   = "telehealth",
      org_pac_id                                 = "org_pac",
      num_org_mem                                = "org_members",
      citytown                                   = "city",
      state                                      = "state",
      zip_code                                   = "zip",
      telephone_number                           = "phone"
    ),
    utilization = c(
      ind_pac_id                                 = "pac",
      provider_last_name                         = "last_name",
      provider_first_name                        = "first_name",
      provider_middle_name                       = "middle_name",
      profile_display_indicator                  = "display"
    ),
    cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  )
}
