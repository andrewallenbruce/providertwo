caid_list$endpoint[["unwin_sbm"]]

aka$endpoint$ahqr_psi11

select_alias(the$catalogs$care$end, aka$endpoint$ahqr_psi11)


care_temporal("quality_payment") |> query_nresults()
open_temporal("payment_general") |> query_nresults()
caid_temporal("healthcare_quality") |> query_nresults()
hgov_temporal("hgov_mlr") |> query_nresults()

care_endpoint("care_enrollees") |> query_nresults()
prov_endpoint("pdc_affiliations") |> query_nresults()

quick("care_dialysis")
quick("managed_longterm")
quick("hgov_ab_reg_comp")
quick("profile_covered")
quick("asc_facility")

care_endpoint("care_dialysis") |> quick_query_()
caid_endpoint("managed_longterm")@identifier |> quick_query_()
hgov_endpoint("hgov_ab_reg_comp")
open_endpoint("profile_covered")
prov_endpoint("asc_facility")@identifier

caid_temporal("nadac_year")@endpoints$identifier[1]

new_collection("caid_demographics") |> query_nresults()

list(count = "true", results = "true", offset = 0L, limit = 1L)

# CARE ENDPOINT:
#    == Query `stats` for results count
request("https://data.cms.gov/data-api/v1/dataset/2457ea29-fc82-48b0-86ec-3b0755de7515/data-viewer") |>
  req_url_path_append("stats") |>
  req_url_query(offset = 0L, size = 1L) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  _$data |>
  unlist()
#    WRONG == Output is a data.frame (with column names)
#    == Outputs a matrix (NO column names)
#    == x$meta$headers path contains column names
request("https://data.cms.gov/data-api/v1/dataset/2457ea29-fc82-48b0-86ec-3b0755de7515/data-viewer") |>
  req_url_query(offset = 0L, size = 1L) |>
  req_perform() |>
  resp_body_json(simplifyVector = TRUE) |>
  get_elem(c("data", "headers"))


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

ex <- paste0(
  "https://data.cms.gov/",
  "data-api/v1/dataset/",
  "0e57f57d-0acc-4c9c-8f8c-973e3f4a3c4b/",
  "data-viewer?",
  # "data-viewer/stats?",
  paste(
    "offset=0",
    "size=200",


    "filter[0][condition][path]=hcpcs_cd",
    "filter[0][condition][operator]==",
    "filter[0][condition][value]=80047",
    "filter[0][condition][memberOf]=2",

    "filter[2][group][conjunction]=AND",  # found 235/967129
    # "filter[2][group][conjunction]=OR", # found 821635/967129

    "filter[1][condition][path]=VOL_TXT",
    "filter[1][condition][operator]=<",
    "filter[1][condition][value]=207",
    "filter[1][condition][memberOf]=2",


    sep = "&"
  )) |>
  request() |>
  perform_simple()

paste0(
  "https://data.cms.gov/",
  "provider-data/api/1/",
  "datastore/query/",
  # "27ea-46a8", # pdc_affiliations
  "mj5m-pzi6", # pdc_clinicians
  "/0?",
  paste(
    "keys=true",
    "rowIds=false",
    "schema=false",
    "count=true",
    "results=true",
    # "results=false",
    "offset=0",
    "limit=100",

    "conditions[0][property]=num_org_mem",
    "conditions[0][operator]=between",
    "conditions[0][value][1]=1721",
    "conditions[0][value][2]=1722",


    "conditions[0][property]=state",
    "conditions[0][operator]=NOT+IN",
    "conditions[0][value][1]=ID",
    "conditions[0][value][2]=IL",
    "conditions[0][value][3]=IN",
    "conditions[0][value][4]=IA",

    sep = "&"
  )
) |>
  request() |>
  perform_simple() |>
  _$count
