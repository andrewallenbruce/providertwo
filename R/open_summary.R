#' @include S7_open.R
NULL

#' @autoglobal
#' @noRd
perform_bare <- new_generic("perform_bare", "x", function(x) {
  S7_dispatch()
})

method(perform_bare, open_endpoint) <- function(x) {
  prop(x, "identifier") |>
    request() |>
    req_url_query(
      count   = "false",
      format  = "json",
      keys    = "true",
      results = "true",
      rowIds  = "false",
      schema  = "false",
      offset  = 0L,
      limit   = 500L
    ) |>
    req_perform() |>
    resp_body_string() |>
    fparse() |>
    _[["results"]]
}

#' @autoglobal
#' @noRd
openDashboard <- new_class(
  name       = "openDashboard",
  package    = NULL,
  properties = list(response = class_list)
)

#' @autoglobal
#' @noRd
openNational <- new_class(
  name       = "openNational",
  package    = NULL,
  properties = list(response = class_list)
)

#' @autoglobal
#' @noRd
tidyup <- new_generic("tidyup", "x", function(x) {
  S7_dispatch()
})

method(tidyup, openDashboard) <- function(x) {

  x <- prop(x, "response") |>
    mtt(
      metric = data_metrics,
      type   = case(
        grepl("^Total Number of", metric, perl = TRUE)              ~ "count",
        grepl("^Total Dollar Amount of", metric, perl = TRUE)       ~ "dollar",
        grepl("^Total Dollar Amount Invested", metric, perl = TRUE) ~ "invest",
        grepl("^Total value of interest", metric, perl = TRUE)      ~ "value"
      ),
      data_metrics         = NULL,
      dashboard_row_number = NULL
    ) |>
    rsplit(~ type)

  x$count <-  mtt(
    x$count,
    metric = gsub("^Total Number of ", "", metric, perl = TRUE),
    metric = gsub("^all ", "", metric, perl = TRUE),
    type   = cheapr_if_else(
      grepl("^Payment Records", metric, perl = TRUE),
      "record",
      "entity"
    )
  ) |>
    rsplit( ~ type)

  x$count$entity <- rnm(
    x$count$entity,
    `2017` = py_2017,
    `2018` = py_2018,
    `2019` = py_2019,
    `2020` = py_2020,
    `2021` = py_2021,
    `2022` = py_2022,
    `2023` = py_2023,
    ALL = total
  ) |>
    mtt(
      metric = gsub(
        "Companies Making Payments \\(AM/GPO Making Payment\\)",
        "Companies [AM/GPO] Making Payments",
        metric,
        perl = TRUE
      ),
      metric = gsub(
        "Physician Covered Recipients with associated payment records",
        "Physicians with Payment Records",
        metric,
        perl = TRUE
      ),
      metric = gsub(
        "Non-Physician Practitioner Covered Recipients with associated payment records",
        "NPPs with Payment Records",
        metric,
        perl = TRUE
      ),
      metric = gsub(
        "Teaching Hospitals with associated payment records",
        "Teaching Hospitals with Payment Records",
        metric,
        perl = TRUE
      )
    ) |>
    pivot(ids = "metric", names = list("year", "count")) |>
    roworder(metric, -year) |>
    colorder(year) |>
    mtt(count = as.integer(count)) |>
    as_tbl()

  x$count$record <- rnm(
    x$count$record,
    `2017` = py_2017,
    `2018` = py_2018,
    `2019` = py_2019,
    `2020` = py_2020,
    `2021` = py_2021,
    `2022` = py_2022,
    `2023` = py_2023,
    ALL = total
  ) |>
    mtt(
      type = case(
        grepl("General Payments", metric, perl = TRUE) ~ "General",
        grepl("Research Payments", metric, perl = TRUE) ~ "Research",
        grepl("Ownership Payments", metric, perl = TRUE) ~ "Ownership",
        .default = "All"
      ),
      status = case(
        grepl("Disputed", metric, perl = TRUE) ~ "Disputed",
        grepl("Undisputed", metric, perl = TRUE) ~ "Undisputed"
      ),
      entity = case(
        grepl("- Covered Recipient", metric, perl = TRUE) ~ "Covered Recipient",
        grepl("- Non-Covered Recipient", metric, perl = TRUE) ~ "Non-Covered Recipient",
        grepl("- Physician or Non-Covered Recipient", metric, perl = TRUE) ~ "Physician or Non-Covered Recipient",
        grepl(
          "- Non-Physician Practitioner or Non-Covered Recipient",
          metric,
          perl = TRUE
        ) ~ "Non-Physician Practitioner or Non-Covered Recipient",
        grepl(" to Physicians", metric, perl = TRUE) ~ "Physician",
        grepl(" to Non-Physician Practitioners", metric, perl = TRUE) ~ "Non-Physician Practitioner",
        grepl(" to Teaching Hospitals", metric, perl = TRUE) ~ "Teaching Hospital",
        grepl(" - Non-Covered Recipient Entity", metric, perl = TRUE) ~ "Non-Covered Recipient Entity",
        grepl(" - Non-Covered Recipient Individual", metric, perl = TRUE) ~ "Non-Covered Recipient Individual"
      ),
      metric = NULL
    ) |>
    pivot(ids = c("type", "status", "entity"),
          names = list("year", "count")) |>
    roworder(type, -year) |>
    colorder(year, type, entity) |>
    mtt(count = as.integer(count)) |>
    as_tbl()

  x$invest <- mtt(
    x$invest,
    metric = gsub(
      "^Total Dollar Amount Invested of all Payment Records ",
      "",
      metric,
      perl = TRUE
    ),
    metric = gsub("^-\\s", "", metric, perl = TRUE)
  )

  x$value <- mtt(
    x$value,
    metric = gsub("^Total value of interest of all Payment Records ", "", metric, perl = TRUE),
    metric = gsub("^-\\s", "", metric, perl = TRUE)
  )

  x$dollar <- mtt(
    x$dollar,
    metric = gsub("^Total Dollar Amount of ", "", metric, perl = TRUE),
    metric = gsub("^all ", "", metric, perl = TRUE)
  )
  x
}

method(tidyup, openNational) <- function(x) {

  x <- prop(x, "response") |>
    map_na_if() |>
    mtt(recipient_type = clean_names(recipient_type)) |>
    rsplit( ~ recipient_type)

  x$covered_recipient_physician <- fcompute(
    x$covered_recipient_physician,
    year            = factor_(program_year),
    payment_type    = factor_(payment_type),
    recipient_type  = "Physician",
    recipient_n     = as.integer(total_number_of_physicians),
    payment_sum     = as.numeric(total_payment_amount_physician),
    payment_mean    = as.numeric(mean_total_payment_amount_physician),
    payment_median  = as.numeric(median_total_payment_amount_physician),
    payments_n      = as.integer(total_payment_count_physician),
    payments_mean   = as.double(mean_total_payment_count_physician),
    payments_median = as.double(median_total_payment_count_physician)
  )

  x$covered_recipient_non_physician_practitioner <- fcompute(
    x$covered_recipient_non_physician_practitioner,
    year            = factor_(program_year),
    payment_type    = factor_(payment_type),
    recipient_type  = "Non-Physician Practitioner",
    recipient_n     = as.integer(total_number_of_non_physician_practitioners),
    payment_sum     = as.numeric(total_payment_amount_non_physician_practitioner),
    payment_mean    = as.numeric(mean_total_payment_amount_non_physician_practitioner),
    payment_median  = as.numeric(median_total_payment_amount_non_physician_practitioner),
    payments_n      = as.integer(total_payment_count_non_physician_practitioner),
    payments_mean   = as.double(mean_total_payment_count_non_physician_practitioner),
    payments_median = as.double(median_total_payment_count_non_physician_practitioner)
  )

  x |>
    rowbind() |>
    mtt(recipient_type = factor_(recipient_type)) |>
    roworder(recipient_type, payment_type) |>
    as_tbl()
}

#' Open Payments Summaries
#' @name open_summary
#' @returns A `<tibble>`
#' @examples
#' open_dashboard()
#' open_national()
NULL

#' @autoglobal
#' @export
#' @rdname open_summary
open_dashboard <- function() {
  openDashboard(
    response = open_endpoint("SUMM_dashboard") |>
      perform_bare()) |>
    tidyup()
}

#' @autoglobal
#' @export
#' @rdname open_summary
open_national <- function() {
  openNational(
    response = open_endpoint("SUMM_nation_all") |>
      perform_bare()) |>
    tidyup()
}

#' @autoglobal
#' @rdname open_summary
#' @noRd
open_dictionary <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    get_elem("data", DF.as.list = TRUE) |>
    get_elem("title|describedBy$", regex = TRUE) |>
    map(\(x) x[not_null(names(x))])

  new_df(name     = get_elem(x, "title") |> delist(),
         download = get_elem(x, "describedBy") |> delist()) |>
    mtt(
      year = as.integer(stri_extract_all_regex(name, "[0-9]{4}")),
      name = cheapr_if_else(is_na(year), name, stri_extract_all_regex(name, "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)")),
      year = cheapr_if_else(is_na(year), fmax(year), year)
    ) |>
    sbt(year == fmax(year), -year) |>
    _[["download"]] |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(
      \(resp)
      resp_body_string(resp) |>
        fparse(query = "/data") |>
        _[["fields"]] |>
        map_na_if() |>
        as_tbl() |>
        mtt(
          description = stri_trans_general(description, "latin-ascii"),
          description = gsub("[\n\"']", "", description, perl = TRUE),
          description = gsub("[\\\\]", "-", description, perl = TRUE),
          description = stri_trim_both(stri_replace_all_regex(description, "\\s+", " ")),
          title = NULL
          )
      ) |>
    set_clean(
      get_elem(x, "title") |>
        stri_extract_all_regex(
          "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)|Covered Recipient Profile Supplement"
          ) |>
        delist() |>
        funique()
      )
}
