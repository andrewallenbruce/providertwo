.onLoad <- function(libname, pkgname) {

  catalog_provider <<- memoise::memoise(catalog_provider)
  catalog_open     <<- memoise::memoise(catalog_open)
  catalog_main     <<- memoise::memoise(catalog_main)
  open_dictionary  <<- memoise::memoise(open_dictionary)

  S7::methods_register()
}
