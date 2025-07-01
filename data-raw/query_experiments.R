q2 <- list(
  `conditions[0][property]` = "state",
  `conditions[0][operator]` = "IN",
  `conditions[0][value][1]` = "CA",
  `conditions[0][value][2]` = "GA",
  `conditions[0][value][3]` = "NY")

# encodeString(x, quote = '"')

x <- list(
  first_name  ~ starts_with_("Andr"),
  last_name   ~ contains_("J"),
  state       = ~ in_(c("CA", "GA", "NY")),
  country     = in_(c("CA", "GA", "NY")),
  state_owner = c("GA", "MD"),
  npi         = npi_ex$k,
  npi_owner   = npi_ex$k[1],
  ccn         = "01256",
  pac         = NULL
)

x
x[map_lgl(x, is_modifier)]

rlang::is_function(contains_)

is_unnamed_formula(x)

x[is_named_rhs(x)]
!is_unnamed_formula(x) & !is_named_formula(x) & is_length_one(x)
!is_unnamed_formula(x) & !is_named_formula(x) & is_length_two(x)

check_names_unique(x)
check_unnamed_formulas(x)
convert_named_formula(x)

x = list(
  state = ~ in_(c("GA", "MD"), negate = TRUE),
  state2 = ~ in_(c("GA", "MD")),
  last_name = "SMITH",
  npi = 1234567890,
  PECOS = NULL
)

generate_query(x)
query_cli(x)
query_formatter(x)

`if`(TRUE, 1234567890, c("GA", "MD"))

# in_(c("GA", "MD"), negate = TRUE)
nf <- x[is_named_rhs(x)]
map(nf, \(x) call_args(x))


rlang::call_args_names(nf$state)

rlang::call_args(nf$state)[!have_name(rlang::call_args(nf$state))]
rlang::call_args(nf$state)[have_name(rlang::call_args(nf$state))]

rlang::call_name(nf$state)
enexpr(c("GA", "MD"))
