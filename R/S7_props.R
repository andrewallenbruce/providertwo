#' `npi` argument class
#'
#' @param input `<chr>` npi
#'
#' @returns An S7 `<arg_npi>` object.
#'
#' @examples
#' arg_npi("1225701881")
#'
#' kind <- c(
#' 1225701881,
#' 1174270805,
#' 1235702796,
#' 1962116806,
#' 1013647569,
#' 1306500665,
#' 1982296737,
#' 1083295638,
#' 1841967825,
#' 1891390084,
#' 1275117269,
#' 1992338701,
#' 1891355863,
#' 1548743511,
#' 1023473279,
#' 1861857013,
#' 1689182859,
#' 1982059275)
#'
#' arg_npi(kind)
#'
#' @autoglobal
#' @rdname args
#' @export
arg_npi <- new_class(
  name    = "arg_npi",
  package = NULL,
  properties = list(
    input = new_property(
      class = NULL | class_character | class_numeric,
      setter = function(self, value) {
        self@input <- as_chr(value)
        self},
      getter = function(self) { as_chr(self@input) })),
      validator = function(self) {
        if (not_null(self@input)) {
          assert_nchars(self@input, 10)
          assert_digits(self@input)
          assert_luhn(self@input)
          }
        }
  )

# props_state <- new_property(
#   class = null_character,
#   validator = function(value) {
#     if (not_null(value)) {
#       if (!is_character(value)) "`state` must be a character vector"
#       if (any(sf_chars(value) != 2L)) "`state` must be 2 characters long"
#       if (any(!value %in% state.abb)) {
#         paste(
#           "Invalid state(s) entered:",
#           paste0(
#             value[
#               which_(value %in% state.abb, invert = TRUE)],
#             collapse = ", "))
#       }
#     }
#   }
# )
