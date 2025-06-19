library(httr2)
library(purrr)
library(RcppSimdJson)
library(fastplyr)
library(cheapr)
library(cli)
library(collapse)
library(fs)
library(glue)
library(kit)
library(rlang)
library(S7)
library(stringi)
library(vctrs)
as_date = providertwo:::as_date
as_fibble = providertwo:::as_fibble
fibble = providertwo:::fibble
ffill = providertwo:::ffill
fnest = providertwo:::fnest
ifelse_ = providertwo:::ifelse_
fmt_temporal = providertwo:::fmt_temporal
fmt_periodicity = providertwo:::fmt_periodicity
fmt_contactpoint = providertwo:::fmt_contactpoint
gremove = providertwo:::gremove
greplace = providertwo:::greplace
rm_nonascii = providertwo:::rm_nonascii
rm_space = providertwo:::rm_space
rm_quotes = providertwo:::rm_quotes
get_distribution = providertwo:::get_distribution
delist = providertwo:::delist
ss_title = providertwo:::ss_title
extract_year = providertwo:::extract_year
gdetect = providertwo:::gdetect
gextract = providertwo:::gextract
join_on_title = providertwo:::join_on_title
str_look_remove = providertwo:::str_look_remove
care_types = providertwo:::care_types
starts_with_ = providertwo:::starts_with_
in_ = providertwo:::in_


switch2 <- function(x, ..., nomatch = NA) {
  res <- vector("list", length(x))
  for (ii in seq_along(x))
    res[[ii]] <- switch(x[ii], ..., nomatch)
  simplify2array(res)
}

args = list(state = c("GA", "MD"),
           last_name = "SMITH",
           npi = 1234567890,
           PECOS = NULL)


# Formulas of the form
# `first_name ~ starts_with("Andr")`
# will:
#    * have no name attribute
#    * return TRUE for is_formula(x[[i]])

x <- list(
  first_name ~ starts_with_("Andr"),
  last_name ~ contains_("Jason"),
  state = ~in_(c("CA", "GA", "NY")),
  npi = 1234567890
)

x1 <- x[names(x) == ""][[1]]
x1 <- rlang::f_rhs(x1) |>
  rlang::eval_bare() |>
  purrr::list_assign(
    filter = rlang::as_name(rlang::f_lhs(x1)))

x[names(x) == ""] <- list(unlist(x1))
names(x)[1] <- x1[[1]]

x

x2 <- x[map_lgl(x, rlang::is_formula)]

rlang::f_rhs(x2[[1]]) |>
  rlang::eval_bare() |>
  purrr::list_assign(
    filter = names(x2))

rlang::is_formula(frm$state, lhs = FALSE)
rlang::is_bare_formula(frm$state, scoped = TRUE, lhs = FALSE)
rlang::f_rhs(frm)

