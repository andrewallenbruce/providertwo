# state_recode(c("GA", "FL"))
# state_recode(c("Georgia", "Florida"), "abbr")
#' @autoglobal
#' @noRd
state_recode <- function(x, to = "full") {

  states <- switch(
    match.arg(to, c("full", "abbr")),
    full = rlang::set_names(state.abb, state.name),
    abbr = rlang::set_names(state.name, state.abb))

  rlang::names2(states)[qmatch(x, states)]
}

#' @autoglobal
#' @noRd
rm_all_na <- function(x) {
  cheapr::na_rm(x[, !cheapr::col_all_na(x)])
}

#' @autoglobal
#' @noRd
roundup <- function(x, d = 2) {
  d  <- 10^d
  z  <- abs(x) * d
  z  <- z + 0.5 + sqrt(.Machine[["double.eps"]])
  z  <- trunc(z)
  z  <- z / d
  z * sign(x)
}

#' @autoglobal
#' @noRd
fmt_int <- function(x) {
  if (x >= 1e6) {
    glue::glue("{roundup(x / 1e6, 1)} M")
  } else if (x >= 1e3) {
    glue::glue("{roundup(x / 1e3, 1)} K")
  } else {
    glue::as_glue(x)
  }
}

#' @autoglobal
#' @noRd
fmt_sum <- function(x) {
  collapse::fsum(x) |> fmt_int()
}

#' @autoglobal
#' @noRd
fmt_num <- function(x) prettyNum(x, big.mark = ",")

#' @autoglobal
#' @noRd
na_if <- function(x, y = "") {
  vctrs::vec_slice(x, vctrs::vec_in(x, y, needles_arg = "x", haystack_arg = "y")) <- NA
  x
}

#' @autoglobal
#' @noRd
map_na_if <- function(i) {
  purrr::modify_if(i, is.character, function(x) na_if(x, y = ""))
}

#' @autoglobal
#' @noRd
make_address <- function(a1, a2) {
  ifelse(!is.na(a2), paste(a1, a2), a1)
}

#' @autoglobal
#' @noRd
clean_names <- function(x) {
  # Convert to lowercase
  x <- tolower(x)
  # Replace dash with underscore
  x <- gsub("-", "_", x, perl = TRUE)
  # Replace space with underscore
  x <- gsub(" ", "_", x, perl = TRUE)
  # Remove dash
  # x <- gsub("-", "", x, perl = TRUE)
  # Remove parentheses
  x <- gsub("\\(|\\)", "", x, perl = TRUE)
  # Remove multiple underscores
  x <- gsub("__", "_", x, perl = TRUE)

  return(x)
}

#' @autoglobal
#' @noRd
set_clean <- function(i, x) {
  rlang::set_names(i, clean_names(x))
}

#' @autoglobal
#' @noRd
fmt_entity <- function(x, type = c("int", "chr")) {
  switch(
    match.arg(type, c("int", "chr")),
    int = val_match(x, 1 ~ 1L, 2 ~ 2L, .default = NA_integer_),
    chr = val_match(x, "NPI-1" ~ 1L, "NPI-2" ~ 2L, .default = NA_integer_),
  )
}

#' @autoglobal
#' @noRd
underscore <- function(x) {
  greplace(x, "___owner$", "_owner")
}

#' @autoglobal
#' @noRd
charbin <- function(x) {
  val_match(x, "N" ~ 0L, "Y" ~ 1L, .default = NA_integer_)
}

#' @autoglobal
#' @noRd
charprop <- function(x) {
  case(x == "0" ~ 0, is_na(x) ~ NA_real_, .default = as.double(x) / 100)
}
