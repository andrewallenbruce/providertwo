options(fastplyr.inform = FALSE)

#' @autoglobal
#' @noRd
`%0%` <- function(x, y) {
  if (is_empty(x)) y else x
}

#' @autoglobal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @autoglobal
#' @noRd
`%|||%` <- function(x, y) {
  if (!is.null(x)) y else NULL
}

#' @autoglobal
#' @noRd
`%|%` <- function(x, y) {
  if (is.na(x)) y else x
}

#' @autoglobal
#' @noRd
names_map <- function(x, f, ..., .nm = x) {
  map(.x = x, .f = f, ...) |>
    set_names(nm = .nm)
}

#' @autoglobal
#' @noRd
fibble <- function(...) {
  fastplyr::new_tbl(...)
}

#' @autoglobal
#' @noRd
as_fibble <- function(x) {
  fastplyr::as_tbl(x)
}

#' @autoglobal
#' @noRd
flist <- function(...) {
  fastplyr::list_tidy(...)
}

#' @autoglobal
#' @noRd
ffill <- function(x,
                  ...,
                  by = NULL,
                  cols = NULL,
                  direction = "forwards",
                  limit = Inf,
                  new_names = "{.col}") {
  fastplyr::f_fill(
    data = x,
    ...,
    .by = by,
    .cols = cols,
    .direction = direction,
    .fill_limit = limit,
    .new_names = new_names
  )
}

#' @autoglobal
#' @noRd
fnest <- function(x,
                  ...,
                  add  = FALSE,
                  by   = NULL,
                  cols = NULL) {
  fastplyr::f_nest_by(
    data = x,
    ...,
    .add = add,
    .by = by,
    .cols = cols) |>
    fastplyr::f_ungroup() |>
    collapse::rnm(endpoints = "data")
}

#' @autoglobal
#' @noRd
yank <- function(x, ..., .def = NULL) {
  pluck(x, 1, ..., .default = .def)
}

`yank<-` <- function(x, value) {
  pluck(x, 1) <- value
  x
}

#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE, ci = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n,
                    case_insensitive = ci)
}

#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE, ci = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n, ci = ci))
}

#' @autoglobal
#' @noRd
ss_title <- function(x, re, ...) {
  subset_detect(i = x, j = title, p = re, ...)
}

#' @autoglobal
#' @noRd
extract_year <- function(x) {
  as.integer(stri_extract_first_regex(x, "[12]{1}[0-9]{3}"))
}

#' @autoglobal
#' @noRd
this_year <- function() {
  as.numeric(substr(Sys.Date(), 1, 4))
}

#' @autoglobal
#' @noRd
qmatch <- function(a, b) {
  collapse::fmatch(x = a, table = b, nomatch = 0L, overid = 2)
}

# state_recode(c("GA", "FL"))
# state_recode(c("Georgia", "Florida"), "abbr")
#' @autoglobal
#' @noRd
state_recode <- function(x, to = "full") {

  states <- switch(
    match.arg(to, c("full", "abbr")),
    full = set_names(state.abb, state.name),
    abbr = set_names(state.name, state.abb))

  names2(states)[qmatch(x, states)]
}

#' @autoglobal
#' @noRd
date_year <- function(x) {
  as.integer(substr(x, 1, 4))
}

#' @autoglobal
#' @noRd
gdetect <- function(str, pt, ...) {
  grepl(x       = str,
        pattern = pt,
        perl    = TRUE,
        ...)
}

#' @autoglobal
#' @noRd
greplace <- function(str, pt, rp, ...) {
  gsub(
    x           = str,
    pattern     = pt,
    replacement = rp,
    perl        = TRUE,
    ...
  )
}

#' @autoglobal
#' @noRd
gremove <- function(str, pt, ...) {
  gsub(
    x           = str,
    pattern     = pt,
    replacement = "",
    perl        = TRUE,
    ...
  )
}

#' @autoglobal
#' @noRd
gextract <- function(x, pt, n = FALSE, ...) {
  stri_extract_first_regex(
    str = x,
    pattern = pt,
    ...
  )
}

#' @autoglobal
#' @noRd
rm_nonascii <- function(x) {
  gremove(x, "[^\x20-\x7E]")
}

#' @autoglobal
#' @noRd
rm_space <- function(x) {
  greplace(x, "  ", " ")
}

#' @autoglobal
#' @noRd
rm_quotes <- function(x) {
  gremove(x, "[\"']")
}

#' @autoglobal
#' @noRd
join_on_title <- function(a, b) {
  join(
    x = a,
    y = b,
    on = "title",
    verbose = 0,
    multiple = TRUE
  )
}

#' @autoglobal
#' @noRd
str_look <- function(pattern, look) {
  switch(
    match.arg(look, c("ahead", "behind")),
    ahead  = glue("(?<={pattern}).*$"),
    behind = glue("^.*(?={pattern})")
  ) |>
    as.character()
}

#' @autoglobal
#' @noRd
str_look_detect <- function(x, pattern, look) {
  str_look(pattern, look) |>
    grepl(x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_replace <- function(x, pattern, look, replacement) {
  str_look(pattern, look) |>
    gsub(replacement = replacement, x, perl = TRUE)
}

#' @autoglobal
#' @noRd
str_look_remove <- function(x, pattern, look) {
  str_look_replace(x, pattern, look, replacement = "")
}

#' @autoglobal
#' @noRd
str_look_extract <- function(x, pattern, look) {
  gextract(x, pattern = str_look(pattern, look))
}

#' @autoglobal
#' @noRd
delist <- function(x) {
  unlist(x, use.names = FALSE)
}

#' @autoglobal
#' @noRd
as_date <- function(x, ..., fmt = "%Y-%m-%d") {
  as.Date(x, ..., format = fmt)
  }

#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {
  if (length(ls) == 0) cat("<empty>\n")

  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(names(ls)), ls), sep = "\n")

  invisible(ls)
}

#' ISO 8601 Recurring Time Intervals
#' @source [DCAT Schema: accrualPeriodicity](https://resources.data.gov/resources/dcat-us/#accrualPeriodicity)
#' @param x `<chr>` vector of ISO8601 recurrence rules
#' @returns `<chr>` vector of human-readable recurrence rule descriptions
#' @examplesIf rlang::is_interactive()
#' accrualPeriodicity = c(
#'   "R/PT1S",   "R/PT1H",  "R/P1D", "R/P3.5D",
#'   "R/P0.33W", "R/P0.5W", "R/P1W", "R/P2W",
#'   "R/P0.33M", "R/P0.5M", "R/P1M", "R/P2M",
#'   "R/P3M",    "R/P4M",   "R/P6M", "R/P1Y",
#'   "R/P2Y",    "R/P3Y",   "R/P4Y", "R/P10Y")
#' fmt_periodicity(accrualPeriodicity)
#' @section References:
#' - [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' - [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#' - [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#' @autoglobal
#' @keywords internal
#' @noRd
fmt_periodicity <- function(x) {
  val_match(
    x,
    "R/P10Y"   ~ "Decennially [R/P10Y]",
    "R/P4Y"    ~ "Quadrennially [R/P4Y]",
    "R/P3Y"    ~ "Triennially [R/P3Y]",
    "R/P2Y"    ~ "Biennially [R/P2Y]",
    "R/P1Y"    ~ "Annually [R/P1Y]",
    "R/P6M"    ~ "Biannually [R/P6M]",
    "R/P4M"    ~ "Triannually [R/P4M]",
    "R/P3M"    ~ "Quarterly [R/P3M]",
    "R/P2M"    ~ "Bimonthly [R/P2M]",
    "R/P1M"    ~ "Monthly [R/P1M]",
    "R/P0.5M"  ~ "Biweekly [R/P0.5M]",
    "R/P2W"    ~ "Biweekly [R/P2W]",
    "R/P0.33M" ~ "Three Times a Month [R/P0.33M]",
    "R/P1W"    ~ "Weekly [R/P1W]",
    "R/P0.5W"  ~ "Twice a Week [R/P0.5W]",
    "R/P3.5D"  ~ "Twice a Week [R/P3.5D]",
    "R/P0.33W" ~ "Three Times a Week [R/P0.33W]",
    "R/P1D"    ~ "Daily [R/P1D]",
    "R/PT1H"   ~ "Hourly [R/PT1H]",
    "R/PT1S"   ~ "Continuously [R/PT1S]",
    .default = x
  )
}

#' @autoglobal
#' @noRd
roxy8601 <- function(x) {
  val_match(
    x,
    "R/P10Y"   ~ "Decennially (R/P10Y)",
    "R/P4Y"    ~ "Quadrennially (R/P4Y)",
    "R/P3Y"    ~ "Triennially (R/P3Y)",
    "R/P2Y"    ~ "Biennially (R/P2Y)",
    "R/P1Y"    ~ "Annually (R/P1Y)",
    "R/P6M"    ~ "Biannually (R/P6M)",
    "R/P4M"    ~ "Triannually (R/P4M)",
    "R/P3M"    ~ "Quarterly (R/P3M)",
    "R/P2M"    ~ "Bimonthly (R/P2M)",
    "R/P1M"    ~ "Monthly (R/P1M)",
    "R/P0.5M"  ~ "Biweekly (R/P0.5M)",
    "R/P2W"    ~ "Biweekly (R/P2W)",
    "R/P0.33M" ~ "Three Times a Month (R/P0.33M)",
    "R/P1W"    ~ "Weekly (R/P1W)",
    "R/P0.5W"  ~ "Twice a Week (R/P0.5W)",
    "R/P3.5D"  ~ "Twice a Week (R/P3.5D)",
    "R/P0.33W" ~ "Three Times a Week (R/P0.33W)",
    "R/P1D"    ~ "Daily (R/P1D)",
    "R/PT1H"   ~ "Hourly (R/PT1H)",
    "R/PT1S"   ~ "Continuously (R/PT1S)",
    .default   = "Unknown"
  )
}

#' @autoglobal
#' @keywords internal
#' @noRd
care_types <- function(x) {
  switch(
    x,
    single = list(
      "ACO REACH Aligned Beneficiaries",
      "ACO REACH Eligible Beneficiaries",
      "ACO REACH Financial and Quality Results",
      "ACO REACH Providers",
      "COVID-19 Nursing Home Data",
      "CPC Initiative - Participating Primary Care Practices",
      "Comprehensive Care for Joint Replacement Model: Metropolitan Statistical Areas",
      "Innovation Center Data and Reports",
      "Innovation Center Innovation Advisors",
      "Innovation Center Milestones and Updates",
      "Innovation Center Model Awardees",
      "Innovation Center Model Participants",
      "Innovation Center Model Summary Information",
      "Innovation Center Webinars and Forums",
      "Market Saturation & Utilization Core-Based Statistical Areas",
      "Market Saturation & Utilization State-County",
      "Medicaid Managed Care",
      "Medicaid Opioid Prescribing Rates - by Geography",
      "Medicaid Spending by Drug",
      "Medicare Advantage Geographic Variation - National & State",
      "Medicare Clinical Laboratory Fee Schedule Private Payer Rates and Volumes",
      "Medicare Demonstrations",
      "Medicare Diabetes Prevention Program",
      "Medicare Geographic Variation - by National, State & County",
      "Medicare Monthly Enrollment",
      "Medicare Part B Discarded Drug Units",
      "Medicare Part B Spending by Drug",
      "Medicare Part D Opioid Prescribing Rates - by Geography",
      "Medicare Part D Spending by Drug",
      "Medicare Post-Acute Care and Hospice - by Geography & Provider",
      "Medicare Telehealth Trends",
      "Restructured BETOS Classification System",
      "Strong Start Awardees"
    ),
    multi = list(
      "Provider of Services File - Clinical Laboratories",
      "Provider of Services File - Hospital & Non-Hospital Facilities",
      "Medicare Fee-for-Service Comprehensive Error Rate Testing",
      "Physician/Supplier Procedure Summary",
      "Accountable Care Organization Participants",
      "Accountable Care Organizations",
      "Hospital Provider Cost Report",
      "Skilled Nursing Facility Cost Report",
      "Medicare Part D Prescribers - by Geography and Drug",
      "Medicare Part D Prescribers - by Provider",
      "Medicare Part D Prescribers - by Provider and Drug",
      "Medicare Physician & Other Practitioners - by Geography and Service",
      "Medicare Physician & Other Practitioners - by Provider",
      "Medicare Physician & Other Practitioners - by Provider and Service",
      "Performance Year Financial and Quality Results",
      "Deficit Reduction Act Hospital-Acquired Condition Measures",
      "Medicare Inpatient Hospitals - by Geography and Service",
      "Medicare Inpatient Hospitals - by Provider",
      "Medicare Inpatient Hospitals - by Provider and Service",
      "Accountable Care Organization Skilled Nursing Facility Affiliates",
      "Hospital Service Area",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Geography and Service",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Referring Provider and Service",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Supplier",
      "Medicare Durable Medical Equipment, Devices & Supplies - by Supplier and Service",
      "County-level Aggregate Expenditure and Risk Score Data on Assignable Beneficiaries",
      "Medicare Outpatient Hospitals - by Geography and Service",
      "Medicare Outpatient Hospitals - by Provider and Service",
      "Number of Accountable Care Organization Assigned Beneficiaries by County",
      "Payroll Based Journal Daily Non-Nurse Staffing",
      "Payroll Based Journal Daily Nurse Staffing",
      "Minimum Data Set Frequency",
      "Quality Payment Program Experience",
      "Medicare Dialysis Facilities",
      "Payroll Based Journal Employee Detail Nursing Home Staffing",
      "Pioneer ACO Model",
      "REACH ACOs",
      "Hospital All Owners",
      "Hospital Enrollments",
      "Medicare Provider and Supplier Taxonomy Crosswalk",
      "Revalidation Clinic Group Practice Reassignment",
      "Revalidation Due Date List",
      "Revalidation Reassignment List",
      "Skilled Nursing Facility All Owners",
      "Skilled Nursing Facility Enrollments",
      "Value Modifier",
      "End-Stage Renal Disease Facility Aggregation Group Performance",
      "Federally Qualified Health Center All Owners",
      "Federally Qualified Health Center Enrollments",
      "Fiscal Intermediary Shared System Attending and Rendering",
      "Home Health Agency All Owners",
      "Home Health Agency Cost Report",
      "Home Health Agency Enrollments",
      "Home Infusion Therapy Providers",
      "Hospice All Owners",
      "Hospice Enrollments",
      "Hospital Change of Ownership",
      "Hospital Change of Ownership - Owner Information",
      "Managing Clinician Aggregation Group Performance",
      "Medicare Fee-For-Service Public Provider Enrollment",
      "Nursing Home Affiliated Entity Performance Measures",
      "Opioid Treatment Program Providers",
      "Opt Out Affidavits",
      "Order and Referring",
      "Pending Initial Logging and Tracking Non Physicians",
      "Pending Initial Logging and Tracking Physicians",
      "Provider of Services File - Internet Quality Improvement and Evaluation System - Home Health Agency, Ambulatory Surgical Center, and Hospice Providers",
      "Rural Health Clinic All Owners",
      "Rural Health Clinic Enrollments",
      "Skilled Nursing Facility Change of Ownership",
      "Skilled Nursing Facility Change of Ownership - Owner Information",
      "Advance Investment Payment Spend Plan",
      "Agency for Healthcare Research and Quality (AHRQ) Patient Safety Indicator 11 (PSI-11) Measure Rates",
      "Facility-Level Minimum Data Set Frequency",
      "Hospital Price Transparency Enforcement Activities and Outcomes",
      "Long-Term Care Facility Characteristics",
      "Medicare COVID-19 Hospitalization Trends",
      "Public Reporting of Missing Digital Contact Information"
    ),
    cli::cli_abort(c("x" = "No matches found for {.val {x}}."), call = call)
  ) |>
    unlist(use.names = FALSE)
}
