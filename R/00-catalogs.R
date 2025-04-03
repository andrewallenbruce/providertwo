rlang::on_load(
  catalog <<- list(
    care = catalog_care(),
    pro  = catalog_pro(),
    open = catalog_open()
  ))

#' @autoglobal
#' @noRd
catalogs <- function() {
  list(
    care = catalog_care(),
    pro  = catalog_pro(),
    open = catalog_open()
  )
}
