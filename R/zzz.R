.onLoad <- function(libname, pkgname) {

  S7::methods_register()
  catalogs <<- memoise::memoise(catalogs)
  endpoint <<- memoise::memoise(endpoint)
  rlang::run_on_load()

}

the      <- new.env(parent = emptyenv())
the$clog <- catalogs()
