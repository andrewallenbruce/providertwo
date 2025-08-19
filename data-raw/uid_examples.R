source(here::here("data-raw", "pins_internal.R"))

uid <- list(
  npi = sort.int(collapse::funique(c(npi_ex$a, npi_ex$k, npi_ex$v)))
)

pin_update(
  uid,
  name = "uid",
  title = "uid",
  description = "Provider ID examples")
