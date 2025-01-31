#' Load Public API `Dataset`
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<Dataset>` object
#'
#' @examples
#' public_Dataset("Public Provider Enrollment")
#'
#' public_Dataset("Hospital Enrollments")
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
    contactPoint       = ContactPoint(type     = gelm(a[["contactPoint"]], "type"),
                                      fn       = gelm(a[["contactPoint"]], "fn"),
                                      hasEmail = gelm(a[["contactPoint"]], "hasEmail")),
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

#' Load Public API `Distribution`
#'
#' @param dataset `<chr>` dataset title
#'
#' @returns `<Distribution>` object
#'
#' @examples
#' public_Distribution("Public Provider Enrollment")
#'
#' public_Distribution("Quality Payment Program Experience")
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
    contactPoint       = ContactPoint(type     = gelm(a[["contactPoint"]], "type"),
                                      fn       = gelm(a[["contactPoint"]], "fn"),
                                      hasEmail = gelm(a[["contactPoint"]], "hasEmail")),
    describedBy        = a[["describedBy"]],
    description        = a[["description"]],
    keyword            = a[["keyword"]],
    landingPage        = a[["landingPage"]],
    references         = a[["references"]],
    title              = a[["title"]],
    distributions      = a[["distribution"]]
  )

}
