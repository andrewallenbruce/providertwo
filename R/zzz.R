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

  open_endpoint  <<- memoise::memoise(open_endpoint)
  open_group     <<- memoise::memoise(open_group)
  open_temporal  <<- memoise::memoise(open_temporal)
  open_troup     <<- memoise::memoise(open_troup)

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


