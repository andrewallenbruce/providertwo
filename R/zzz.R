.onLoad <- function(libname, pkgname) {

  rlang::run_on_load()

  catalogs <<- memoise::memoise(catalogs)
  MainCurrent <<- memoise::memoise(MainCurrent)
  openCurr <<- memoise::memoise(openCurr)
  proCurr <<- memoise::memoise(proCurr)

  S7::methods_register()

}

# .onUnload <- function(libpath) {
#
#   remove(
#     list = c("CATALOG"),
#     envir = .GlobalEnv
#   )
#
# }


