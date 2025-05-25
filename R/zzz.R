.onLoad <- function(libname, pkgname) {

  S7::methods_register()

  oldopts <- options()
  options(nthreads = 4)
  on.exit(oldopts)

  # list_resources <<- memoise::memoise(list_resources)
  # base_request   <<- memoise::memoise(base_request)
  # quick          <<- memoise::memoise(quick)

}
