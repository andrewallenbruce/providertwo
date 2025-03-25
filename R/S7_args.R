#' `npi` argument class
#'
#' @param x `<chr>` vector of npis
#'
#' @returns An S7 `<arg_npi>` object.
#'
#' @examples
#' arg_npi("1225701881")
#'
#' arg_npi(1225701881)
#'
#' try(arg_npi(122570188))
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
    x = new_property(
      class = NULL | class_character | class_numeric,
      setter = function(self, value) {
        self@x <- as_chr(value)
        self
        },
      getter = function(self) as_chr(self@x)
      )
    ),
      validator = function(self) {
        if (not_null(self@x)) {
          assert_nchars(self@x, 10L, "npi")
          assert_digits(self@x)
          assert_luhn(self@x)
          }
        }
  )

#' `state` argument class
#'
#' @param x `<chr>` vector of state abbreviations
#'
#' @returns An S7 `<arg_state>` object.
#'
#' @examples
#' arg_state("CA")
#'
#' try(arg_state("C"))
#'
#' try(arg_state("AA"))
#'
#' # Allow duplicates?
#' arg_state(c("CA", "CA"))
#'
#' @autoglobal
#' @rdname args
#' @export
arg_state <- new_class(
  name    = "arg_state",
  package = NULL,
  properties = list(
    x = new_property(class = NULL | class_character)),
  validator = function(self) {
    if (not_null(self@x)) {
      assert_nchars(self@x, 2L, "state")
      assert_choices(self@x, state.abb, "state")
    }
  }
)
