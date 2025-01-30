#' Populate a public_Dataset class object
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
    accrualPeriodicity = a[["accrualPeriodicity"]],
    contactPoint       = ContactPoint(type = gelm(a[["contactPoint"]], "type"), fn = gelm(a[["contactPoint"]], "fn"), hasEmail= gelm(a[["contactPoint"]], "hasEmail")),
    describedBy        = a[["describedBy"]],
    description        = a[["description"]],
    identifier         = Identifier(url = a[["identifier"]]),
    keyword            = a[["keyword"]],
    landingPage        = a[["landingPage"]],
    modified           = a[["modified"]],
    references         = a[["references"]],
    temporal           = a[["temporal"]],
    title              = a[["title"]],
    resourcesAPI       = Resources(url = a[["resourcesAPI"]])
  )

}

#' Populate a public_Distribution class object
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
    accrualPeriodicity = a[["accrualPeriodicity"]],
    contactPoint       = ContactPoint(type = gelm(a[["contactPoint"]], "type"), fn = gelm(a[["contactPoint"]], "fn"), hasEmail= gelm(a[["contactPoint"]], "hasEmail")),
    describedBy        = a[["describedBy"]],
    description        = a[["description"]],
    keyword            = a[["keyword"]],
    landingPage        = a[["landingPage"]],
    references         = a[["references"]],
    title              = a[["title"]],
    distributions      = a[["distribution"]]
  )

}
