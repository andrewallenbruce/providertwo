"Managed Long Term Services and Supports (MLTSS) Enrollees"

"Medicaid Financial Management Data - Data Dictionary 12-11-2024"

x <- caid_endpoint("managed_longterm")@metadata$dictionary |>
  request() |>
  perform_simple() |>
  _$data

list(
  title = x$title,
  dictionary = list(glue('{x$fields$name}: "{x$fields$description}"') |> glue_collapse(sep = ""))
) |>
  yaml::as.yaml() |>
  cat()

glue("{x$fields$name}: {x$fields$description}") |> glue_collapse(sep = "\n\n")



yaml::write_yaml()
