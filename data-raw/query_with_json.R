x <- query(city = starts_with("BIRMINGHAM"))


x@input$city |> rlang::eval_tidy()


x@params$city <- "Atlanta"

x

x <- query(
  or(first_name = starts_with("And"), middle_name = "J"),
  last_name   = contains("J"),
  state       = any_of(c("CA", "GA", "NY")),
  city        = not_equal(c("Atlanta", "Los Angeles")),
  state_own   = c("GA", "MD"),
  npi         = npi_ex$k,
  ccn         = "01256",
  rate        = between(0.45, 0.67),
  year        = 2014:2025)

is_group <- rlang::names2(x@params) == "" & purrr::map_lgl(x@params, rlang::is_symbolic)

names(x@input[which_(is_group)]) <- paste0("g", seq_along(which_(is_group)))
names(x@params[which_(purrr::map_lgl(x@params, rlang::is_symbolic))]) <- paste0("g", seq_along(which_(purrr::map_lgl(x@params, rlang::is_symbolic))))

# works
request("https://data.cms.gov/provider-data/api/1/datastore/query/4pq5-n9py/0?limit=5") |>
  req_body_json(
    list(
      properties = c("state", "overall_rating"),
      conditions = list(
        list(
          property = "overall_rating",
          operator = ">",
          value = "3"
        )
      )
    )
  ) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  _$results |>
  as_fibble() |>
  head()


json_ex <- '{
    "properties": [
        "state",
        "overall_rating"
    ],
    "conditions": [
        {
            "property": "overall_rating",
            "operator": ">",
            "value": "3"
        }
    ]
}'

jsonify::as.json(json_ex) |> jsonify::from_json() |> jsonify::to_json()
