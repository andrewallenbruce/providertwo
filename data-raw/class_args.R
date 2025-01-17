class_arg <- new_class(
  "class_arg",
  properties = list(
    label = new_property(
      class_character,
      setter = function(self, value) {
        self@label <- value
        self
        }),
    alias = new_property(
      class_character,
      setter = function(self, value) {
        self@alias <- value
        self
      }),
    choices = new_property(
      null_character,
      setter = function(self, value) {
        self@choices <- value
        self
      }),
    operators = new_property(
      class_character,
      default = "=",
      setter = function(self, value) {
        self@operators <- value
        self
        }),
    entered = new_property(
      null_character,
      setter = function(self, value) {
        self@entered <- value
        self
        })
    ),
  validator = function(self) {
    if (length(self@label) != 1L) "@label must be length 1"
    if (length(self@alias) != 1L) "@alias must be length 1"
    if (any(!self@operators %in% c("=", ">=", "<=", ">", "<", "<>", "STARTS_WITH", "ENDS_WITH", "CONTAINS", "IN", "NOT IN", "BETWEEN", "NOT BETWEEN"))) "@operators invalid"
  }
)


arg_npi <- class_arg(
  label = "npi",
  alias = "NPI",
  operators = c("=", "IN", "NOT IN", "STARTS_WITH", "ENDS_WITH"))

arg_pac <- class_arg(
  label = "pac",
  alias = "PECOS_ASCT_CNTL_ID",
  operators = c("=", "IN", "NOT IN", "STARTS_WITH", "ENDS_WITH"))

arg_enid <- class_arg(
  label = "enid",
  alias = "ENRLMT_ID",
  operators = c("=", "IN", "NOT IN", "STARTS_WITH", "ENDS_WITH"))

arg_state <- class_arg(
  label = "state",
  alias = "STATE_CD",
  operators = c("=", "IN", "NOT IN", "STARTS_WITH", "ENDS_WITH"),
  choices = state.abb)

arg_firstname <- class_arg(
  label = "first",
  alias = "FIRST_NAME",
  operators = c("=", "IN", "NOT IN", "STARTS_WITH", "ENDS_WITH"))

arg_gender <- class_arg(
  label = "gender",
  alias = "GNDR_SW",
  operators = c("=", "IN", "NOT IN", "STARTS_WITH", "ENDS_WITH"),
  choices = c("M", "F", "9"))

list2(
  "NPI"                = npi,
  "PECOS_ASCT_CNTL_ID" = pac,
  "ENRLMT_ID"          = enid,
  "PROVIDER_TYPE_CD"   = spec_code,
  "PROVIDER_TYPE_DESC" = spec_desc,
  "STATE_CD"           = state,
  "FIRST_NAME"         = first,
  "MDL_NAME"           = middle,
  "LAST_NAME"          = last,
  "ORG_NAME"           = org,
  "GNDR_SW"            = gender)

class_args <- new_class(
  "class_args",
  properties = list(args = class_list))

list(
  npi = arg_npi,
  pac = arg_pac,
  enid = arg_enid,
  state = arg_state,
  first = arg_firstname,
  gender = arg_gender)

args@
  #' enrollees(enid = "I20040309000221")
  #'
  #' enrollees(npi = "1417918293", spec_code = "14-41")
  #'
  #' enrollees(pac = "2860305554", gender = "9")


try(class_args(
  id     = 1234567890,
  state  = "ZZ",
  number = "300.12"))
