.onLoad <- function(libname, pkgname) {

  S7::methods_register()
  catalogs <<- memoise::memoise(catalogs)

}

the         <- new.env(parent = emptyenv())
the$catalog <- catalogs()
