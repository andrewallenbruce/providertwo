source(here::here("data-raw", "pins_internal.R"))

dir <- glue::as_glue(here::here("data-raw", "catalogs"))

paths <- fs::dir_ls(dir)

catalog_raw <- RcppSimdJson::fload(paths) |>
  rlang::set_names(c("hgov", "caid", "care", "open", "prov"))

pin_update(
  catalog_raw,
  name = "catalog_raw",
  title = "catalog_raw",
  description = "catalog_raw")
