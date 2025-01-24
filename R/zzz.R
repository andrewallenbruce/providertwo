.__public <- NULL

.onLoad <- function(libname, pkgname, ...) {

  .__public <<- public_dataset()

  # run_on_load()

  S7::methods_register()
}

.onUnload <- function(libpath) {
  remove(".__public", envir = .GlobalEnv)
}
