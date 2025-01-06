.onLoad <- function(libname, pkgname) {
  .__public <<- public_dataset()
}

.onUnload <- function(libpath) {
  remove(".__public", envir = .GlobalEnv)
}

#' Public CMS Dataset Object
#'
#' @autoglobal
#'
#' @returns description
#'
#' @keywords internal
#'
#' @export
public_dataset <- \() {

  dataset <- RcppSimdJson::fload("https://data.cms.gov/data.json")

  dataset <- collapse::qTBL(dataset[["dataset"]])

  distribution <- collapse::fselect(dataset, distribution) |>
    tidyr::unnest(distribution)

  list(
    dataset = collapse::fselect(dataset, -distribution) |> remove_all_na(),
    latest  = collapse::fsubset(distribution, description %==% "latest") |> remove_all_na(),
    api     = collapse::fsubset(distribution, not_na(format) & na(description)) |> remove_all_na(),
    csv     = collapse::fsubset(distribution, mediaType %==% "text/csv") |> remove_all_na()
  )
}
