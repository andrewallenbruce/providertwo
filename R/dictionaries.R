#' @autoglobal
#' @noRd
open_dictionary <- function() {

  x <- fload("https://openpaymentsdata.cms.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    get_elem("data", DF.as.list = TRUE) |>
    get_elem("title|describedBy$", regex = TRUE) |>
    map(\(x) x[!is.null(names(x))])

  new_df(
    name = get_elem(x, "title") |> delist(),
    download = get_elem(x, "describedBy") |> delist()) |>
    mtt(
      year = extract_year(name),
      name = ifelse(
        is_na(year),
        name,
        stri_extract_all_regex(
          name,
          "^.*(?=\\s.\\sDetailed Dataset [0-9]{4} Reporting Year)"
          )
        ),
        year = ifelse(
          is_na(year),
          fmax(year),
          year)
      ) |>
    sbt(year == fmax(year), -year) |>
    get_elem("download") |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    resps_successes() |>
    map(function(resp)
      parse_string(resp, query = "/data") |>
        get_elem("fields") |>
        map_na_if() |>
        as_fibble() |>
        mtt(description = stri_trans_general(description, "latin-ascii"),
            description = gremove(description, "[\n\"']"),
            description = greplace(description, "[\\\\]", "-"),
            description = stri_trim_both(greplace(description, "\\s+", " ")),
            description = rm_nonascii(description))
      ) |>
    set_names(
      get_elem(x, "title") |>
        stri_extract_all_regex(
          paste0(
            "^.*(?=\\s.\\sDetailed Dataset ",
            "[0-9]{4} Reporting Year)",
            "|Covered Recipient Profile Supplement")) |>
        unlist(use.names = FALSE) |>
        funique()
    ) |>
    list_rbind(names_to = "endpoint") |>
    slt(endpoint, field = name, description, format, constraints, title)
}

#' @autoglobal
#' @noRd
caid_dictionary <- function() {

  fload("https://data.medicaid.gov/api/1/metastore/schemas/dataset/items?show-reference-ids") |>
    get_elem("distribution") |>
    rowbind(fill = TRUE) |>
    get_elem("data") |>
    get_elem("describedBy") |>
    unlist(use.names = FALSE) |>
    funique() |>
    map(request) |>
    req_perform_parallel(on_error = "continue") |>
    map(function(resp) {

      x <- parse_string(resp, query = "/data")

      fibble(
        endpoint    = x$title,
        field       = x$fields$name,
        title       = x$fields$title,
        description = rm_nonascii(greplace(x$fields$description, "\r\n", " ")),
        format      = x$fields$format) |>
        map_na_if()
      }
  ) |>
    list_rbind()
}
