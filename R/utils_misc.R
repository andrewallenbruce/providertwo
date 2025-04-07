#' Detect by Regex
#'
#' @param x `<chr>` vector to search
#' @param p `<chr>` regular expression pattern
#' @param n `<lgl>` negate
#'
#' @returns `<lgl>` logical vector
#'
#' @autoglobal
#' @noRd
pdetect <- function(x, p, n = FALSE) {
  stri_detect_regex(str     = x,
                    pattern = p,
                    negate  = n)
}

#' Subset by Regex
#'
#' @param i `<data.frame>` to search
#' @param j `<chr>` vector to detect
#' @param p `<chr>` regular expression pattern
#' @param n `<lgl>` negate
#'
#' @returns `<data.frame>` subsetted data.frame
#'
#' @autoglobal
#' @noRd
subset_detect <- function(i, j, p, n = FALSE) {
  sbt(i, pdetect(x = i[[ensym(j)]], p = p, n = n))
}

#' Select by Alias
#' @autoglobal
#' @noRd
select_alias <- function(x, alias) {
  subset_detect(x, title, alias)
}

#' Check if a property is empty
#'
#' @param obj `<S7_object>` to check
#' @param nm `<chr>` property name
#'
#' @returns `<lgl>` `TRUE` if empty, `FALSE` otherwise
#'
#' @autoglobal
#' @noRd
prop_empty <- function(obj, nm) {
  check_is_S7(obj)
  empty(prop(obj, nm))
}

#' Print a named list
#'
#' @param ls     `<list>` to print
#' @param prefix `<chr>` to prepend to each line
#'
#' @returns `<list>` invisibly
#'
#' @examplesIf rlang::is_interactive()
#' print_list(list(a = 1, b = 2, c = 3))
#'
#' @autoglobal
#' @noRd
print_list <- function(ls, prefix = "") {

  if (length(ls) == 0) cat("<empty>\n")

  ns <- names(ls)

  if (length(ns) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(ns), ls), sep = "\n")

  invisible(ls)
}
