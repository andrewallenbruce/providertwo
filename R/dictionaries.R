#' @autoglobal
#' @noRd
open_dictionary <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    get_elem("data", DF.as.list = TRUE) |>
    get_elem("title|describedBy$", regex = TRUE) |>
    map(\(x) x[not_null(names(x))])

  new_df(name     = get_elem(x, "title") |> delist(),
         download = get_elem(x, "describedBy") |> delist()) |>
    mtt(
      year = as.integer(stri_extract_all_regex(name, "[0-9]{4}")),
      name = cheapr_if_else(is_na(year), name, stri_extract_all_regex(name, "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)")),
      year = cheapr_if_else(is_na(year), fmax(year), year)
    ) |>
    sbt(year == fmax(year), -year) |>
    _[["download"]] |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(
      \(resp)
      resp_body_string(resp) |>
        fparse(query = "/data") |>
        _[["fields"]] |>
        map_na_if() |>
        as_tbl() |>
        mtt(
          description = stri_trans_general(description, "latin-ascii"),
          description = gsub("[\n\"']", "", description, perl = TRUE),
          description = gsub("[\\\\]", "-", description, perl = TRUE),
          description = stri_trim_both(stri_replace_all_regex(description, "\\s+", " ")),
          title = NULL
        )
    ) |>
    set_clean(
      get_elem(x, "title") |>
        stri_extract_all_regex(
          "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)|Covered Recipient Profile Supplement"
        ) |>
        delist() |>
        funique()
    )
}

#' @autoglobal
#' @noRd
caid_dictionary <- function() {

  x <- fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    as_tbl() |>
    slt(distribution)

  x <- rowbind(x$distribution, fill = TRUE)

  get_elem(x$data, "describedBy") |>
    unlist(use.names = FALSE) |>
    funique() |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(resp) {
      x <- resp_body_string(resp) |>
        fparse() |>
        _[["data"]]

      new_tbl(
        title       = x$title,
        field       = x$fields$name,
        description = remove_non_ascii(gsub("\r\n", " ", x$fields$description))
        ) |>
        map_na_if()
      }) |>
    list_rbind()
}
