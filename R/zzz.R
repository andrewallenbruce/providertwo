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
#' @returns `<list>` of `<tibbles>`: `dataset`, `latest`, `api`, `csv`
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

#' Filter Public Catalog
#'
#' @param endpoint `<chr>` API endpoint; options are `"dataset"` (default),
#'   `"latest"`, `"api"`, `"csv"`
#'
#' @param title `<chr>` dataset title to search for; use `NULL` to return all
#'
#' @returns `<tibble>` of search results
#'
#' @examples
#' public_filter(
#'   endpoint = "latest",
#'   title = "Medicare Fee-For-Service Public Provider Enrollment : 2024-09-01")
#'
#' public_filter(
#'   endpoint = "dataset",
#'   title = "Medicare Fee-For-Service  Public Provider Enrollment")
#'
#' @autoglobal
#'
#' @export
public_filter <- function(endpoint = c("dataset", "latest", "api", "csv"),
                          title = NULL) {

  endpoint <- match.arg(endpoint)

  if (!exists(".__public")) .__public <- public_dataset()

  x <- .__public[[endpoint]]

  search_in(x, "title", title)
}

#' Format API Queries
#'
#' @param args `<list>` of `<chr>` arguments
#'
#' @returns `<list>` of formatted query `<expr>`
#'
#' @examples
#' format_query(list("NPI" = "1417918293", "PECOS_ASCT_CNTL_ID" = NULL))
#'
#' @autoglobal
#'
#' @export
format_query <- \(args) {

  query <- glue::glue(
    '
  "filter[fID{fID}][path]" = "{PATH}",
  "filter[fID{fID}][operator]" = "{OPERATOR}",
  "filter[fID{fID}][value]" = "{VALUE}"
  ',
    fID = seq_along(args),
    PATH = names(args),
    OPERATOR = "=",
    VALUE = args
  ) |>
    glue::glue_collapse(sep = ",\n")

  glue::glue('c({query})') |>
    rlang::parse_expr() |>
    rlang::eval_bare()
}
