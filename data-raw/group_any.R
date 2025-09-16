class_ids <- quo(alias$care$temporal$quality_payment) |>
  as_label() |>
  strsplit("$", fixed = TRUE) |>
  pluck(1) |>
  _[2:4]

class_fn <- class_ids[1:2] |> paste0(collapse = "_") |> as_function()

paste0(
  "select_alias(.catalog$",
  stringi::stri_sub(paste0(class_ids[1:2], collapse = "$"), to = -5),
  ", '",
  as_name(alias$care$temporal$quality_payment),
  "')"
       ) |>
  parse_expr() |>
  eval_bare()

x <- select_care_group("fqhc")

care_endpoint("care_enrollees")
hgov_temporal("qhp_quality_ratings")

aka <- c("care_enrollees", "qhp_quality_ratings")
cls <- c("care_endpoint", "hgov_temporal")

class_group(
  group = uuid::UUIDgenerate(use.time = TRUE, output = "string"),
  members = set_names(map2(aka, cls, \(x, y) as_function(y)(x)), aka)
)

call_quo <- as.list(quote(care_endpoint("care_enrollees")))

c(
  .aka$endpoint$ahqr_psi11,
  .aka$endpoint$aip_plan
  ) |>
  map(\(alias) select_endpoint(alias))

x <- new_group(c("qhp_bus_rule_variables", "managed_care_share"))

x@members$managed_care_share@dimensions@fields |>
  cheapr::list_as_df()
x@members$qhp_bus_rule_variables@dimensions@fields

x@members$managed_care_share@dimensions |>
  props(names = c("limit", "rows", "pages")) |>
  cheapr::list_as_df() |>
  t()
