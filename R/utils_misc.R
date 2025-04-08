#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n)
}

#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n))
}

#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}

#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {
  if (length(ls) == 0) cat("<empty>\n")

  if (length(names(ls)) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(names(ls)), ls), sep = "\n")

  invisible(ls)
}

#' @noRd
empty <- function(x) vec_is_empty(x)

#' @autoglobal
#' @noRd
if_empty_null <- function(x) if (empty(x)) NULL else x

#' @autoglobal
#' @noRd
na_if <- function(x, y = "") {
  vctrs::vec_slice(x, vec_in(x, y, needles_arg = "x", haystack_arg = "y")) <- NA
  x
}

#' @noRd
yank <- function(x) x[[1]]

#' @noRd
na <- function(x) is_na(x)

#' @noRd
not_na <- function(x) !is_na(x)

#' @noRd
delist <- function(x) unlist(x, use.names = FALSE)

#' @noRd
null <- function(x) is.null(x)

#' @noRd
not_null <- function(x) !is.null(x)

#' @noRd
true <- function(x) isTRUE(x)

#' @noRd
false <- function(x) isFALSE(x)

#' @noRd
as_date <- function(x, ..., fmt = "%Y-%m-%d") as.Date(x, ..., format = fmt)

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
  if (x >= 1e6) return(paste0(round(x / 1e6, 1), "M"))
  if (x >= 1e3) return(paste0(round(x / 1e3, 0), "K"))
  as.character(x)
}
