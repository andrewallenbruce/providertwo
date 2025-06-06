.onLoad <- function(libname, pkgname) {

  S7::methods_register()

  opts <- options()
  options(
    nthreads = 4,
    fastplyr.inform = FALSE)
  on.exit(opts)

  # list_resources <<- memoise::memoise(list_resources)
  # base_request   <<- memoise::memoise(base_request)
  # quick          <<- memoise::memoise(quick)

}
