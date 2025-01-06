.onLoad <- function(libname, pkgname) {
  .__public <<- public_dataset()
}

.onUnload <- function(libpath) {
  remove(".__public", envir = .GlobalEnv)
}
