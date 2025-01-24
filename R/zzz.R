.__public <- .__provider <- NULL

.onLoad <- function(libname, pkgname, ...) {

  .__public   <<- public_dataset()
  .__provider <<- provider_dataset()

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
