#' @name npi_ex
#' @title Example NPIs
#' @keywords internal
#' @export
npi_ex <- c(1225701881, 1174270805, 1235702796, 1962116806, 1013647569, 1306500665, 1982296737, 1083295638, 1841967825, 1891390084, 1275117269, 1992338701, 1891355863, 1548743511, 1023473279, 1861857013, 1689182859, 1982059275)

#' `npi` argument class
#'
#' @param x `<chr>` vector of npis
#'
#' @returns An S7 `<arg_npi>` object.
#'
#' @examples
#' arg_npi("1225701881")
#' arg_npi(1225701881)
#' arg_npi(npi_ex)
#' arg_npi(NULL)
#' arg_npi(NA_character_)
#'
#' try(arg_npi(122570188))
#' try(arg_npi("12257O1881"))
#' try(arg_npi(1225701882))
#' @autoglobal
#' @rdname args
#' @export
arg_npi <- new_class(
  name    = "arg_npi",
  package = NULL,
  properties = list(
    x = new_property(
      class = NULL | class_character,
      setter = function(self, value) {
        self@x <- as_chr(value[which_not_na(value)])
        self
      },
      getter = function(self) {
        as_chr(self@x[which_not_na(self@x)])
      }
    ),
    op = new_property(
      class = class_character,
      default = "=",
      getter = function(self)
        if (length(self@x) > 1) "IN" else "=")
  ),
  validator = function(self) {
    if (not_null(self@x) && length(self@x) > 0) {
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
#' arg_state(state.abb[1])
#'
#' arg_state(state.abb)
#'
#' arg_state(c(NULL, NULL))
#'
#' arg_state(NA_character_)
#'
#' @autoglobal
#' @rdname args
#' @export
arg_state <- new_class(
  name    = "arg_state",
  package = NULL,
  properties = list(
    x = new_property(
      class = NULL | class_character,
      setter = function(self, value) {
        self@x <- as_chr(value[which_not_na(value)])
        self
      },
      getter = function(self) {
        as_chr(self@x[which_not_na(self@x)])
      }
    ),
    op = new_property(
      class = class_character,
      default = "=",
      getter = function(self)
        if (length(self@x) > 1) "IN" else "=")
  ),
  validator = function(self) {
    if (not_null(self@x) && length(self@x) > 0) {
      assert_nchars(self@x, 2L, "state")
      assert_choices(self@x, state.abb, "state")
    }
  }
)
