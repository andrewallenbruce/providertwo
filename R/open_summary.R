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
#' open_dictionary()
NULL

#' @autoglobal
#' @export
#' @rdname open_summary
open_dashboard <- function() {
  openDashboard(
    response = openMain("dashboard") |>
      new_request() |>
      req_perform() |>
      resp_body_string() |>
      fparse() |>
      _[["results"]]
  ) |>
    tidyup()
}

#' @autoglobal
#' @export
#' @rdname open_summary
open_national <- function() {
  openNational(
    response = openMain("national_total") |>
      new_request() |>
      req_perform() |>
      resp_body_string() |>
      fparse() |>
      _[["results"]]
  ) |>
    tidyup()
}

#' @autoglobal
#' @export
#' @rdname open_summary
open_dictionary <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids")
  x <- get_elem(x, "data", DF.as.list = TRUE)
  x <- get_elem(x, "title|describedBy$", regex = TRUE)
  x <- map(x, \(x) x[not_null(names(x))])
  x <- new_df(name = delist(get_elem(x, "title")), dictionary = delist(get_elem(x, "describedBy"))) |>
    mtt(year = as.integer(stri_extract_all_regex(name, "[0-9]{4}")),
        name = cheapr_if_else(na(year), name, stri_extract_all_regex(name, "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)")),
        year = cheapr_if_else(na(year), fmax(year), year)) |>
    sbt(year == fmax(year), -year)

  get_dict  <- \(x) perform_simple_request(x) |> _[["data"]] |> _[["fields"]]
  research  <- get_dict(x$dictionary[[1]])
  ownership <- get_dict(x$dictionary[[2]])
  general   <- get_dict(x$dictionary[[3]])
  covered   <- get_dict(x$dictionary[[4]])

  list(
    `Research Payment Data` = new_tbl(
      field       = research$name,
      description = research$description,
      type        = research$type,
      maxlength   = research$constraints$maxLength,
      pattern     = research$constraints$pattern,
      max         = research$constraints$maximum),
    `Ownership Payment Data` = new_tbl(
      field       = ownership$name,
      description = ownership$description,
      type        = ownership$type,
      maxlength   = ownership$constraints$maxLength,
      pattern     = ownership$constraints$pattern,
      max         = ownership$constraints$maximum),
    `General Payment Data` = new_tbl(
      field       = general$name,
      description = general$description,
      type        = general$type,
      maxlength   = general$constraints$maxLength,
      pattern     = general$constraints$pattern,
      max         = general$constraints$maximum),
    `Covered Recipient Profile Supplement` = new_tbl(
      field       = covered$name,
      description = covered$description,
      type        = covered$type,
      maxlength   = covered$constraints$maxLength,
      pattern     = covered$constraints$pattern,
      max         = covered$constraints$maximum))
}
