.onLoad <- function(libname, pkgname) {

  rlang::run_on_load()
  S7::methods_register()

  catalogs      <<- memoise::memoise(catalogs)

  careMain      <<- memoise::memoise(careMain)
  careGroup     <<- memoise::memoise(careGroup)
  careTemp      <<- memoise::memoise(careTemp)
  careTempGroup <<- memoise::memoise(careTempGroup)

  pro_endpoint  <<- memoise::memoise(pro_endpoint)
  pro_group     <<- memoise::memoise(pro_group)

  openMain      <<- memoise::memoise(openMain)
  openGroup     <<- memoise::memoise(openGroup)
  openTemp      <<- memoise::memoise(openTemp)
  openTempGroup <<- memoise::memoise(openTempGroup)

  caid_endpoint <<- memoise::memoise(caid_endpoint)
  caid_group    <<- memoise::memoise(caid_group)

  open_dashboard <<- memoise::memoise(open_dashboard)
  open_national  <<- memoise::memoise(open_national)
  quick          <<- memoise::memoise(quick)

}

# .onUnload <- function(libpath) {
#
#   remove(
#     list = c("CATALOG"),
#     envir = .GlobalEnv
#   )
#
# }


