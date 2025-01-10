.onLoad <- function(libname, pkgname) {

  debugme::debugme()

  "!DEBUG Loading public_dataset"
  .__public <<- public_dataset()
}

.onUnload <- function(libpath) {
  remove(".__public", envir = .GlobalEnv)
}
