#' Public Provider Enrollment Dataset Object
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<S7_class>` object
#'
#' @examples
#' public_Dataset(dataset = "Public Provider Enrollment")
#' public_Dataset(dataset = "Hospital Enrollments")
#'
#' @autoglobal
#'
#' @export
public_Dataset <- \(dataset) {

  if (!exists(".__public")) .__public <<- Catalog_public()

  a <- c(
    as.list(sbt(gelm(.__public, "dataset"), sf_detect(title, dataset))),
    as.list(sbt(gelm(.__public, "distribution"), sf_detect(title, dataset))[1, 5]))

  Dataset(
    type               = a[["type"]],
    accessLevel        = a[["accessLevel"]],
    accrualPeriodicity = a[["accrualPeriodicity"]],
    bureauCode         = a[["bureauCode"]],
    contactPoint       = dataset_contactPoint(type = gelm(a[["contactPoint"]], "type"), fn = gelm(a[["contactPoint"]], "fn"), hasEmail= gelm(a[["contactPoint"]], "hasEmail")),
    describedBy        = a[["describedBy"]],
    description        = a[["description"]],
    identifier         = public_Identifier(url = a[["identifier"]]),
    keyword            = a[["keyword"]],
    landingPage        = a[["landingPage"]],
    modified           = a[["modified"]],
    programCode        = a[["programCode"]],
    publisher          = dataset_Publisher(type = gelm(a[["publisher"]], "type"), name = gelm(a[["publisher"]], "name")),
    references         = a[["references"]],
    temporal           = a[["temporal"]],
    title              = a[["title"]],
    resourcesAPI       = dataset_Resources(url = a[["resourcesAPI"]])
  )

}

#' Public Provider Enrollment Distribution Object
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<S7_class>` object
#'
#' @examples
#' public_Distribution(dataset = "Public Provider Enrollment")
#'
#' @autoglobal
#'
#' @export
public_Distribution <- \(dataset) {

  if (!exists(".__public")) .__public <<- Catalog_public()

  a <- c(
    as.list(sbt(.__public[["dataset"]], sf_detect(title, dataset))),
    list(distribution = sbt(.__public[["distribution"]], sf_detect(title, dataset))))

  Distribution(
    type               = a[["type"]],
    accessLevel        = a[["accessLevel"]],
    accrualPeriodicity = a[["accrualPeriodicity"]],
    bureauCode         = a[["bureauCode"]],
    contactPoint       = dataset_contactPoint(type = gelm(a[["contactPoint"]], "type"), fn = gelm(a[["contactPoint"]], "fn"), hasEmail= gelm(a[["contactPoint"]], "hasEmail")),
    describedBy        = a[["describedBy"]],
    description        = a[["description"]],
    keyword            = a[["keyword"]],
    landingPage        = a[["landingPage"]],
    programCode        = a[["programCode"]],
    publisher          = dataset_Publisher(type = gelm(a[["publisher"]], "type"), name = gelm(a[["publisher"]], "name")),
    references         = a[["references"]],
    title              = a[["title"]],
    distributions      = a[["distribution"]]
  )

}

#' Medicare Provider Enrollees
#'
#' Individual & Organizational Enrollment-level Data
#' on Providers Actively Approved to Bill Medicare.
#'
#' @section Accrual Periodicity:
#'    * `r roxy8601("R/P3M")`
#'
#' @section Links:
#'    * [Enrollment API](https://data.cms.gov/provider-characteristics/medicare-provider-supplier-enrollment/medicare-fee-for-service-public-provider-enrollment)
#'    * [Enrollment Data Dictionary](https://data.cms.gov/resources/medicare-fee-for-service-public-provider-enrollment-data-dictionary)
#'
#' @param npi `<chr>` 10-digit Individual NPI
#'
#' @param pac `<chr>` 10-digit PECOS Associate Control (PAC) ID
#'
#' @param enid `<chr>` 15-digit Medicare Enrollment ID
#'
#' @param spec_code `<chr>` Enrollment specialty code
#'
#' @param spec_desc `<chr>` Enrollment specialty description
#'
#' @param state `<chr>` Enrollment state abbreviation
#'
#' @param first,middle,last `<chr>` Individual provider's name
#'
#' @param org `<chr>` Organizational provider's name
#'
#' @param gender `<chr>` Individual provider's gender:
#'
#'    * `"F"` (Female)
#'    * `"M"` (Male)
#'    * `"9"` (Unknown/Organization)
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' enrollees(enid = "I20040309000221")
#'
#' enrollees(npi = "1417918293", spec_code = "14-41")
#'
#' enrollees(pac = "2860305554", gender = "9")
#'
#' enrollees(state = "GA", gender = "F")
#'
#' @autoglobal
#'
#' @export
enrollees <- function(npi       = NULL,
                      pac       = NULL,
                      enid      = NULL,
                      spec_code = NULL,
                      spec_desc = NULL,
                      first     = NULL,
                      middle    = NULL,
                      last      = NULL,
                      org       = NULL,
                      state     = NULL,
                      gender    = NULL) {

  args <- list2(
    "NPI"                = npi,
    "PECOS_ASCT_CNTL_ID" = pac,
    "ENRLMT_ID"          = enid,
    "PROVIDER_TYPE_CD"   = spec_code,
    "PROVIDER_TYPE_DESC" = spec_desc,
    "STATE_CD"           = state,
    "FIRST_NAME"         = first,
    "MDL_NAME"           = middle,
    "LAST_NAME"          = last,
    "ORG_NAME"           = org,
    "GNDR_SW"            = gender)

  api <- public_Dataset("Public Provider Enrollment")

  nobs <- req_url_query(api@identifier@request, !!!format_query(args), size = 5000) |>
    req_url_path_append("stats") |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE) |>
    gelm("found_rows")

  is_complete <- \(resp) length(resp_body_json(resp)$data) < 5000

  resp <- req_url_query(api@identifier@request, !!!format_query(args), size = 5000) |>
    req_perform_iterative(
    next_req = iterate_with_offset(
      "offset",
      start = 0,
      offset = 5000,
      resp_complete = is_complete))


  cat(format(api@title), "\n")

  utils::formatUL(
    label  = "==>",
    offset = 2,
    c(paste0("Periodicity: ", format(api@accrualPeriodicity)),
      paste0("Last Modified:       ", format(api@modified)))
    ) |>
    writeLines()

  cat("\n")

  map(resp, \(x)
      resp_body_string(x) |>
        fparse() |>
        _[["data"]] |>
        qTBL()) |>
    rowbind() |>
    setNames(names(args))

  # qTBL(resp[["data"]]) |>
  #   setNames(names(args))

}
