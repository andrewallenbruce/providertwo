#' @name arguments
#' @title Argument Classes
#' @param x `<chr>` input vector
#' @returns An S7 `<arg>` object.
#' @examplesIf interactive()
#' arg_npi("1225701881")
#' arg_npi(1225701881)
#' arg_npi(npi_ex$k)
#' arg_npi(NULL)
#' arg_npi(NA_character_)
#'
#' try(arg_npi(122570188))
#' try(arg_npi("12257O1881"))
#' try(arg_npi(1225701882))
#'
#' arg_state("CA")
#' arg_state(state.abb[1])
#' arg_state(state.abb)
#' arg_state(NA_character_)
#' arg_state(c(NULL, NULL))
#'
#' # Allow duplicates?
#' arg_state(c("CA", "CA"))
#' try(arg_state("C"))
#' try(arg_state("AA"))
#' @noRd
NULL

#' @autoglobal
#' @rdname arguments
#' @noRd
arg_npi <- new_class(
  name    = "arg_npi",
  package = NULL,
  properties = list(
    x = new_property(
      class = new_union(NULL, class_character),
      setter = function(self, value) {
        self@x <- as.character(value[which_not_na(value)])
        self
      },
      getter = function(self) {
        as.character(self@x[which_not_na(self@x)])
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

#' @autoglobal
#' @rdname arguments
#' @noRd
arg_state <- new_class(
  name    = "arg_state",
  package = NULL,
  properties = list(
    x = new_property(
      class = new_union(NULL, class_character),
      setter = function(self, value) {
        self@x <- as.character(value[which_not_na(value)])
        self
      },
      getter = function(self) {
        as.character(self@x[which_not_na(self@x)])
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
