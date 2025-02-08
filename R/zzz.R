.__public <- .__provider <- NULL

.onLoad <- function(lib, pkg) {

  .__public   <<- Catalog_public()
  .__provider <<- Catalog_provider()

  S7::methods_register()
}

.onUnload <- function(libpath) {
  remove(
    list = c(
      ".__public",
      ".__provider"
    ),
    envir = .GlobalEnv)
}
