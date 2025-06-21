.onLoad <- function(libname, pkgname) {

  S7::methods_register()
  catalogs <<- memoise::memoise(catalogs)

  # list_resources <<- memoise::memoise(list_resources)
  # base_request   <<- memoise::memoise(base_request)

}
