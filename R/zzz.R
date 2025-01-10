.onLoad <- function(libname, pkgname) {

  debugme::debugme()

  .__public <<- public_dataset()
}

.onUnload <- function(libpath) {
  remove(".__public", envir = .GlobalEnv)
}
