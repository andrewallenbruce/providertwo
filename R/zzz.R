.onLoad <- function(libname, pkgname) {

  S7::methods_register()

  # list_resources <<- memoise::memoise(list_resources)
  # base_request   <<- memoise::memoise(base_request)
  # quick          <<- memoise::memoise(quick)

}
