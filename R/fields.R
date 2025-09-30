#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {
  if (length(ls) == 0) cat("<empty>\n")

  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(names(ls)), ls), sep = "\n")

  invisible(ls)
}

# test_aliases(the$aka$open)
# test_aliases(the$aka$care)
# test_aliases(the$aka$caid)
# test_aliases(the$aka$prov)
# test_aliases(the$aka$hgov)
#' Test Aliases
#' @param name description
#' @autoglobal
#' @noRd
test_aliases <- function(x) {

  all_alias <- purrr::list_flatten(x, name_spec = "{inner}")

  all_names <- rlang::names2(all_alias) |> sort()

  errors <- purrr::map_dfr(
    all_names, function(x) {
      cheapr::new_df(
        alias = x,
        title = tryCatch(
          alias_lookup(x)$title,
          error = function(e)
            NA_character_
        ),
        identifier = tryCatch(
          alias_lookup(x)$identifier,
          error = function(e)
            NA_character_
        )
      )
    }
  ) |>
    fastplyr::as_tbl() |>
    collapse::sbt(is.na(identifier)) |>
    _$alias

  print_list(
    all_alias[errors],
    prefix = paste0(cli::symbol$record, " ")
  )
}

# alias_column(the$clog$care$temporal, the$aka$care$temporal)
#' @autoglobal
#' @noRd
alias_column <- function(df, aka, default = "NA") {

  default <- match.arg(default, c("NA", "alias"))

  to_col <- function(aka) {
    code_def  <- glue::glue(",\n .default = {default})")

    string <- paste0(
      "gdetect(title, ",
      "{glue::single_quote(unname(x))}) ~ ",
      "{glue::single_quote(names(x))}"
    )

    glue::as_glue("cheapr::case(\n") +
      glue::glue(string, x = aka) |>
      glue::glue_collapse(sep = ",\n") +
      code_def
  }

  collapse::mtt(
    df,
    alias = to_col(aka) |>
      rlang::parse_expr() |>
      rlang::eval_bare()
  ) |>
    collapse::colorder(alias)
}

#' Field Dictionary
#' @examples
#' dictionary()
#' @returns A data frame
#' @autoglobal
#' @export
dictionary <- function() {
  get_pin("dicts")
}

#' Field Table
#' @examples
#' field_table()
#' @returns A data frame
#' @autoglobal
#' @export
field_table <- function() {

  collapse::mtt(
    get_pin("field_types"),
    type = NULL,
    field = rm_nonascii(field),
    type = cheapr::case(
      field %in_% FIELD$first_name ~ "first_name",
      field %in_% FIELD$middle_name ~ "middle_name",
      field %in_% FIELD$last_name ~ "last_name",
      field %in_% FIELD$suffix ~ "suffix",
      field %in_% FIELD$org_name ~ "org_name",
      field %in_% FIELD$facility ~ "facility",
      field %in_% FIELD$dba_name ~ "dba_name",
      field %in_% FIELD$hospital_name ~ "hospital_name",
      field %in_% FIELD$brand_name ~ "brand_name",
      field %in_% FIELD$specialty ~ "specialty",
      field %in_% FIELD$specialty_code ~ "specialty_code",
      field %in_% FIELD$enid_ind ~ "enid_ind",
      field %in_% FIELD$enid_org ~ "enid_org",
      field %in_% FIELD$pac_ind ~ "pac_ind",
      field %in_% FIELD$pac_org ~ "pac_org",
      field %in_% FIELD$pac_own ~ "pac_own",
      field %in_% FIELD$hcpcs ~ "hcpcs",
      field %in_% FIELD$hcpcs_desc ~ "hcpcs_desc",
      field %in_% FIELD$npi ~ "npi",
      field %in_% FIELD$npi_prescriber ~ "npi_prescriber",
      field %in_% FIELD$npi_supply ~ "npi_supply",
      field %in_% FIELD$npi_render ~ "npi_render",
      field %in_% FIELD$npi_refer ~ "npi_refer",
      field %in_% FIELD$npi_seller ~ "npi_seller",
      field %in_% FIELD$npi_buyer ~ "npi_buyer",
      field %in_% FIELD$npi_entity ~ "npi_entity",
      field %in_% FIELD$multi_npi ~ "multi_npi",
      field %in_% FIELD$drg ~ "drg",
      field %in_% FIELD$drg_desc ~ "drg_desc",
      field %in_% FIELD$ccn ~ "ccn",
      field %in_% FIELD$ccn_alt ~ "ccn_alt",
      field %in_% FIELD$ccn_buy ~ "ccn_buyer",
      field %in_% FIELD$ccn_sell ~ "ccn_seller",
      field %in_% FIELD$ccn_render ~ "ccn_render",
      field %in_% FIELD$year ~ "year",
      field %in_% FIELD$years ~ "years",
      field %in_% FIELD$city ~ "city",
      field %in_% FIELD$city_own ~ "city_own",
      field %in_% FIELD$city_refer ~ "city_refer",
      field %in_% FIELD$city_render ~ "city_render",
      field %in_% FIELD$city_supply ~ "city_supply",
      field %in_% FIELD$city_scribe ~ "city_scribe",
      field %in_% FIELD$county ~ "county",
      field %in_% FIELD$country ~ "country",
      field %in_% FIELD$phone ~ "phone",
      field %in_% FIELD$zip ~ "zip",
      field %in_% FIELD$address ~ "address",
      field %in_% FIELD$address_2 ~ "address_2",
      field %in_% FIELD$state ~ "state",
      field %in_% FIELD$state_ind ~ "state_ind",
      field %in_% FIELD$state_org ~ "state_org",
      field %in_% FIELD$work_date ~ "work_date",
      field %in_% FIELD$start_date ~ "start_date",
      field %in_% FIELD$end_date ~ "end_date",

      field %in_% c("effective_date") ~ "effective_date",
      field %in_% c("termination_date") ~ "termination_date",
      field %in_% c("processing_date") ~ "process_date",
      field %in_% c("fda_approval_date") ~ "fda_approval_date",
      field %in_% c("market_date") ~ "market_date",
      field %in_% c("measure_date_range") ~ "measure_date_range",
      field %in_% c("measure_abbreviation") ~ "measure_abb",
      field %in_% c("as_of_date") ~ "asof_date",
      field %in_% c("INCORPORATION DATE") ~ "incorp_date",

      field %in_% c("month") ~ "month",
      field %in_% c("quarter") ~ "quarter",

      field %in_% c("ndc") ~ "ndc",
      field %in_% c("ndc_description") ~ "ndc_description",
      field %in_% c("package_size") ~ "pkg_size",
      field %in_% c("product_name") ~ "product_name",
      field %in_% c("product_code") ~ "product_code",
      field %in_% c("labeler_code") ~ "labeler_code",
      field %in_% c("labeler_name") ~ "labeler_name",
      field %in_% c("explanation_code") ~ "explanation_code",

      field %in_% c("suppression_used") ~ "suppressed",
      field %in_% c("number_of_prescriptions") ~ "prescriptions",
      field %in_% c("classification_for_rate_setting") ~ "rate_class",
      field %in_% c("state_rate") ~ "rate_state",
      field %in_% c("rateper1000beneficiaries") ~ "rate_bene",
      field %in_% c("population") ~ "population",

      field %in_% c("nadac_per_unit") ~ "nadac_per_unit",
      field %in_% c("pricing_unit") ~ "pricing_unit",
      field %in_% c("unit_type") ~ "unit_type",
      field %in_% c("drug_type") ~ "drug_type",
      field %in_% c("pharmacy_type_indicator") ~ "pharmacy_type",

      field %in_% c("units_reimbursed") ~ "units_reimbursed",
      field %in_% c("total_amount_reimbursed") ~ "total_reimbursed",
      field %in_% c("medicaid_amount_reimbursed") ~ "medicaid_reimbursed",
      field %in_% c("non_medicaid_amount_reimbursed") ~ "non_medicaid_amount_reimbursed",

      field %in_% c("corresponding_generic_drug_nadac_per_unit") ~ "generic_nadac_per_unit",
      field %in_% c("corresponding_generic_drug_effective_date") ~ "generic_effective_date",
      field %in_% c("utilization_type") ~ "utilization_type",

      field %in_% c("score") ~ "score",
      field %in_% c("otc") ~ "otc",
      field %in_% c("cms_region") ~ "region",
      field %in_% c("network") ~ "network",
      field %in_% c("facility_id") ~ "facility_id",

      field %in_% c("measure_name") ~ "measure_name",
      field %in_% c("measure_id") ~ "measure_id",
      field %in_% c("measure_code") ~ "measure_code",

      field %in_% c("number_of_completed_surveys") ~ "surveys_complete",
      field %in_% c("number_of_beneficiaries") ~ "beneficiaries",
      field %in_% c("count_of_enrollees") ~ "enrollees",
      field %in_% c("servicecount") ~ "services",
      field %in_% c("number_of_states_reporting") ~ "states",
      field %in_% c("Number of Beds") ~ "beds",

      field %in_% c("Place_Of_Srvc") ~ "pos",

      field %in_% c("median") ~ "median",
      field %in_% c("location") ~ "location",

      .default = NA_character_
    ) |>
      rm_nonascii()
  )
}

#' @autoglobal
#' @noRd
dict_field <- function() {
  field_table() |>
  collapse::slt(point, alias, year, field, type) |>
    collapse::sbt(!is.na(type) & !is.na(alias)) |>
    collapse::rsplit( ~ point, keep.by = FALSE) |>
    purrr::map(function(x)
      rm_all_na(x) |>
        fastplyr::f_nest_by(.by = "alias") |>
          fastplyr::f_ungroup()) |>
    purrr::list_rbind()
}
