.__public <- NULL

.onLoad <- function(libname, pkgname, ...) {

  .__public <<- public_dataset()

  # debugme::debugme()

  S7::methods_register()
}

.onUnload <- function(libpath) {
  remove(".__public", envir = .GlobalEnv)
}
