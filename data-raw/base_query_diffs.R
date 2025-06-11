caid_list$endpoint[["unwin_sbm"]]

aka <- list(
    endpoint = list_combine(
      care_list$endpoint,
      pro_list$endpoint,
      open_list$endpoint,
      caid_list$endpoint,
      hgov_list$endpoint
    ),
    temporal = list_combine(
      care_list$temporal,
      open_list$temporal,
      caid_list$temporal,
      hgov_list$temporal
    )
  )

aka$endpoint$ahqr_psi11

select_alias(the$catalogs$care$end, aka$endpoint$ahqr_psi11)

`%|||%` <- function(x, y) {
  if (!is.null(x)) y else NULL
}

quick("care_dialysis")
quick("managed_longterm")
quick("hgov_ab_reg_comp")
quick("profile_covered")
quick("asc_facility")

x <- care_endpoint("care_dialysis") |> quick_query_()
caid_endpoint("managed_longterm")@identifier |> quick_query_()
hgov_endpoint("hgov_ab_reg_comp")
open_endpoint("profile_covered")
prov_endpoint("asc_facility")@identifier

caid_temporal("nadac_year")@endpoints$identifier[1]

list(count = "true", results = "true", offset = 0L, limit = 1L)

# CARE ENDPOINT:
#    == Query `stats` for results count
request("https://data.cms.gov/data-api/v1/dataset/9887a515-7552-4693-bf58-735c77af46d7/data") |>
  req_url_path_append("stats") |>
  req_url_query(offset = 0L, size = 1L) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  unlist()
#    == Output is a data.frame (with column names)
request("https://data.cms.gov/data-api/v1/dataset/9887a515-7552-4693-bf58-735c77af46d7/data") |>
  req_url_query(offset = 0L, size = 1L) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  fastplyr::as_tbl()


# CARE TEMPORAL:
#   == Query `stats` to get results count
request("https://data.cms.gov/data-api/v1/dataset/9887a515-7552-4693-bf58-735c77af46d7/data-viewer") |>
  req_url_path_append("stats") |>
  req_url_query(offset = 0L, size = 1L) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  get_elem("data")
#    == Outputs a matrix (NO column names)
#    == x$meta$headers path contains column names
request("https://data.cms.gov/data-api/v1/dataset/9887a515-7552-4693-bf58-735c77af46d7/data-viewer") |>
  req_url_query(offset = 0L, size = 1L) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  get_elem("data")

# CAID ENDPOINT:
#   - Set `count = "true"` to get results count
request("https://data.medicaid.gov/api/1/datastore/query/5394bcab-c748-5e4b-af07-b5bf77ed3aa3/0") |>
  req_url_query(offset = 0L, limit = 1L, count = "true") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  get_elem("count")
#   - Output is a data.frame (with column names)
request("https://data.medicaid.gov/api/1/datastore/query/5394bcab-c748-5e4b-af07-b5bf77ed3aa3/0") |>
  req_url_query(offset = 0L, limit = 1L, count = "false") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  get_elem("results")
# CAID TEMPORAL: EXACTLY THE SAME AS ENDPOINT

# PRO ENDPOINT: SAME
request("https://data.cms.gov/provider-data/api/1/datastore/query/4jcv-atw7/0") |>
  req_url_query(offset = 0L, limit = 1L, count = "true") |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  get_elem("count")
