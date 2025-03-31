rlang::on_load(
  catalog <<- list(
    main = catalog_main(),
    pro  = catalog_provider(),
    open = catalog_open()
  ))

#' API Catalogs
#' @returns `<list>` of API catalogs
#' @autoglobal
#' @noRd
catalogs <- function() {
  list(
    main = catalog_main(),
    pro  = catalog_provider(),
    open = catalog_open()
  )
}
