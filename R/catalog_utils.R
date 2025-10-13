options(fastplyr.inform = FALSE)

#' @autoglobal
#' @noRd
open_description <- function(title_col, desc_col) {
  kit::nswitch(
    title_col,
    "General Payment Data",
    "All general (non-research, non-ownership related) payments from the program year",
    "Ownership Payment Data",
    "All ownership and investment payments from the program year",
    "Research Payment Data",
    "All research-related payments from the program year",
    default = desc_col,
    nThread = 4L
  )
}

#' @autoglobal
#' @noRd
caid_title <- function(x) {
  kit::nif(
    gdetect(x, "Child and Adult Health Care Quality Measures"), "Child and Adult Health Care Quality Measures",
    gdetect(x, "2[0-9]{3} Manage"), "Managed Care Programs by State",
    gdetect(x, "NADAC \\(National Average Drug Acquisition Cost\\)"), "NADAC",
    gdetect(x, "State Drug Utilization Data"), "State Drug Utilization Data",
    gdetect(x, "Pricing Comparison"), "Pricing Comparison for Blood Disorder Treatments",
    gdetect(x, "Product Data for Newly Reported"), "Product Data for Newly Reported Drugs in the Medicaid Drug Rebate Program",
    default = x
  )
}

#' @autoglobal
#' @noRd
to_str <- function(x) {
  purrr::map_chr(x, \(i) toString(unlist(i, use.names = FALSE), width = NULL))
}

#' @autoglobal
#' @noRd
caid_download <- function(x) {

  download <- get_caid(x) |>
    collapse::slt(title, distribution) |>
    collapse::sbt(
      grep(
        "test|coreset|scorecard|category_tiles|auto",
        title,
        ignore.case = TRUE,
        perl = TRUE,
        invert = TRUE
      )
    ) |>
    collapse::roworder(title) |>
    collapse::mtt(
      distribution = purrr::map(
        distribution,
        \(x) collapse::get_elem(
          x,
          "^title$|^downloadURL$",
          DF.as.list = TRUE,
          regex = TRUE
        )
      ),
      is_chr = purrr::map_lgl(distribution, \(x) is.character(x))
    ) |>
    collapse::rsplit( ~ is_chr, use.names = FALSE) |>
    rlang::set_names(c("multi", "single"))

  purrr::imap(download, function(x, i) {
    switch(
      i,
      single = collapse::slt(collapse::mtt(x, download = to_str(distribution)), title, download),
      multi = collapse::mtt(
        x,
        name     = to_str(collapse::get_elem(distribution, "title")),
        download = to_str(collapse::get_elem(distribution, "downloadURL")),
        name     = kit::iif(name == "CSV", title, name, nThread = 4L),
        name     = kit::iif(title == name, NA_character_, name, nThread = 4L)
      ) |>
        collapse::slt(title, download)
    )
  }) |>
    purrr::list_rbind() |>
    collapse::roworder(title)
}

#' @autoglobal
#' @noRd
prov_download <- function(x) {
  get_prov(x) |>
    get_distribution() |>
    collapse::get_elem("^downloadURL", regex = TRUE, DF.as.list = TRUE) |>
    unlist(use.names = FALSE)
}

#' @autoglobal
#' @noRd
open_download <- function(x) {
  get_open(x) |>
    get_distribution(DF.as.list = TRUE) |>
    collapse::get_elem("downloadURL", DF.as.list = TRUE) |>
    unlist(use.names = FALSE)
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
clean_title <- function(x) {
  rm_nonascii(x) |>
    rm_quotes() |>
    trimws() |>
    rm_space()
}

#' @autoglobal
#' @noRd
f_nest <- function(x,
                  ...,
                  add  = FALSE,
                  by   = NULL,
                  cols = NULL) {
  fastplyr::f_nest_by(
    .data = x,
    ...,
    .add = add,
    .by = by,
    .cols = cols) |>
    fastplyr::f_ungroup() |>
    collapse::rnm(endpoints = "data")
}

#' @autoglobal
#' @noRd
get_care <- function(x) {
  collapse::get_elem(x, "care")
}

#' @autoglobal
#' @noRd
get_prov <- function(x) {
  collapse::get_elem(x, "prov")
}

#' @autoglobal
#' @noRd
get_caid <- function(x) {
  collapse::get_elem(x, "caid")
}

#' @autoglobal
#' @noRd
get_open <- function(x) {
  collapse::get_elem(x, "open")
}

#' @autoglobal
#' @noRd
get_hgov <- function(x) {
  collapse::get_elem(x, "hgov")
}

#' @autoglobal
#' @noRd
get_distribution <- function(x, ...) {
  collapse::get_elem(x, "distribution", ...)
}

#' @autoglobal
#' @noRd
uuid_from_url <- function(x) {
  stringi::stri_extract(
    x,
    regex = paste(
      "(?:[0-9a-fA-F]){8}",
      "(?:[0-9a-fA-F]){4}",
      "(?:[0-9a-fA-F]){4}",
      "(?:[0-9a-fA-F]){4}",
      "(?:[0-9a-fA-F]){12}",
      sep = "-?"
    )
  )
}

#' @autoglobal
#' @noRd
fmt_contactpoint <- function(x) {
  x <- collapse::get_elem(x, "contactPoint")

  glue::glue("{rlang::names2(x)} ({x})",
             x = collapse::get_elem(x, "^has", regex = TRUE) |>
               unlist(use.names = FALSE) |>
               rlang::set_names(
                 collapse::get_elem(x, "fn") |>
                   unlist(use.names = FALSE))) |>
    as.character()
}

#' @autoglobal
#' @noRd
fmt_temporal <- function(x) {
  greplace(x, "/", paste0(" ", cli::symbol$bullet, " "))
}

#' ISO 8601 Recurring Time Intervals
#' @section References:
#' - [DCAT Schema: accrualPeriodicity](https://resources.data.gov/resources/dcat-us/#accrualPeriodicity)
#' - [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601)
#' - [ISO 8601 Repeating_intervals](https://en.wikipedia.org/wiki/ISO_8601#Repeating_intervals)
#' - [Recurring Time Intervals](https://sentenz.github.io/convention/convention/iso-8601/#19-recurring-time-intervals)
#' @autoglobal
#' @noRd
fmt_periodicity <- function(x) {
  cheapr::val_match(
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
