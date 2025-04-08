.onLoad <- function(libname, pkgname) {

  catalogs      <<- memoise::memoise(catalogs)
  careMain      <<- memoise::memoise(careMain)
  careTemp      <<- memoise::memoise(careTemp)
  careTempGroup <<- memoise::memoise(careTempGroup)
  openMain      <<- memoise::memoise(openMain)
  proMain       <<- memoise::memoise(proMain)

  rlang::run_on_load()

  S7::methods_register()

  open_dashboard <<- memoise::memoise(open_dashboard)
  open_national  <<- memoise::memoise(open_national)

}

# .onUnload <- function(libpath) {
#
#   remove(
#     list = c("CATALOG"),
#     envir = .GlobalEnv
#   )
#
# }


