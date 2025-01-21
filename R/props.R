null_numeric    <- new_union(NULL, class_numeric)
null_integer    <- new_union(NULL, class_integer)
null_character  <- new_union(NULL, class_character)
null_char_int   <- new_union(NULL, class_character, class_integer)
null_list       <- new_union(NULL, class_list)
null_data.frame <- new_union(NULL, class_data.frame)
null_dbl_Date   <- new_union(NULL, class_double, class_Date)

# .npi <- new_property(
#   class = null_character,
#   validator = function(value) {
#     if (not_null(value)) {
#       if (!is_character(value))   "`npi` must be a character vector"
#       if (sf_chars(value) != 10L) "`npi` must be 10 characters long"
#       if (!sf_detect(value, "^[0-9]{1,10}$")) "`npi` must be all digits"
#       if (!luhn_check(value))     "`npi` must pass Luhn algorithm"
#       if (!sf_sub(value, 1, 1) %in% c("1", "2")) "`npi` must start with 1 or 2"
#     }
#   }
# )
#
# class_arg <- new_class(
#   name = "class_arg",
#   properties = list(
#     input = class_any,
#     field = class_character
#     )
# )
#
# class_arg(input = .npi, field = "NPI")
