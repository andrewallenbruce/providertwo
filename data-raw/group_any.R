class_group(
  group = "group",
  members = list(
    enrollees = care_endpoint("care_enrollees"),
    qhp_quality = hgov_temporal("qhp_quality_ratings")
    )
)

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
