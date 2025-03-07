<!-- NEWS.md is maintained by https://cynkra.github.io/fledge, do not edit -->

# providertwo 0.0.0.9093 (2025-03-07)

* added `get_resources()` helper


# providertwo 0.0.0.9092 (2025-03-07)

* added several new helpers


# providertwo 0.0.0.9091 (2025-03-06)

* Testing memoise


# providertwo 0.0.0.9090 (2025-03-04)

* renaming some things


# providertwo 0.0.0.9089 (2025-03-04)

* Added Open Payments group subsetters


# providertwo 0.0.0.9088 (2025-03-04)

* New Open Payments Catalog internal structure
* New `open_dictionary()`
* Removed `tidyr` from Suggests
* Moved `RcppSimdJson` to Imports


# providertwo 0.0.0.9087 (2025-02-20)

* work on print methods


# providertwo 0.0.0.9086 (2025-02-19)

* Quick draft for NLM's NPI Registry API


# providertwo 0.0.0.9085 (2025-02-19)

* refactored `load_openpayments()` a bit
* organizing files


# providertwo 0.0.0.9084 (2025-02-19)

* added `seq_along0()` for query format indices that must start at 0
* moved `limit` parameter functionality from endpoint functions to internal `perform_request` methods


# providertwo 0.0.0.9083 (2025-02-18)

* added `cli_n_results_requests()`
* switched `abort` conditions from `rlang` to `cli`
* bug: `affiliations()` not processing multiple-call requests


# providertwo 0.0.0.9082 (2025-02-18)

* experimenting with `fn_fmls()` for queries


# providertwo 0.0.0.9081 (2025-02-17)

* added `open_Dataset()` draft


# providertwo 0.0.0.9080 (2025-02-16)

* More openpayments work


# providertwo 0.0.0.9079 (2025-02-16)

* `load_openpayments()` pruning


# providertwo 0.0.0.9078 (2025-02-15)

* work on class print methods


# providertwo 0.0.0.9077 (2025-02-13)

* added `load_openpayments()`


# providertwo 0.0.0.9076 (2025-02-09)

* class clean up


# providertwo 0.0.0.9075 (2025-02-08)

* `affiliations()` initial implementation
* API differentiation of request/query/parse functions


# providertwo 0.0.0.9074 (2025-02-08)

* simplified catalog loadings


# providertwo 0.0.0.9073 (2025-02-08)

* fixed bug in `Catalog_provider`
* `provider_Dataset` draft


# providertwo 0.0.0.9072 (2025-02-08)

* tidying


# providertwo 0.0.0.9071 (2025-02-08)

* renamed & simplified classes
* `lookups` regex differentiation
* `httr2` utils
* reworked `public_Dataset/Distribution` selection functionality


# providertwo 0.0.0.9070 (2025-02-07)

* Don't `@export` S7 methods. Use `S7::methods_register` in `.onLoad` call instead.


# providertwo 0.0.0.9069 (2025-02-07)

* rudimentary print method for `Dataset` class


# providertwo 0.0.0.9068 (2025-02-07)

* renaming API-specific methods


# providertwo 0.0.0.9067 (2025-02-04)

* fixed bug in `public_Distribution` (keeps first `dataset` row only, if more than one is returned)


# providertwo 0.0.0.9066 (2025-02-04)

* renaming


# providertwo 0.0.0.9065 (2025-02-03)

* added `get_nrows`


# providertwo 0.0.0.9064 (2025-02-03)

* fixed bug in `get_fields` (removed `ifelse()` call)


# providertwo 0.0.0.9063 (2025-01-31)

* testing using function name when loading dataset/distribution


# providertwo 0.0.0.9062 (2025-01-31)

* removed all `collapse::qTBL` calls


# providertwo 0.0.0.9061 (2025-01-31)

* replace `collapse::qTBL` calls with `fastplyr::as_tbl`


# providertwo 0.0.0.9060 (2025-01-31)

* added function name to dataset title lookup


# providertwo 0.0.0.9059 (2025-01-30)

* added rate limit checks


# providertwo 0.0.0.9058 (2025-01-30)

* initial draft: `request` -> `response` -> `return` flow


# providertwo 0.0.0.9057 (2025-01-30)

* tweaks to classes


# providertwo 0.0.0.9056 (2025-01-30)

* `offset_length()` added
* utilities exported as internal functions
* `hasURL` property removed from `contactPoint` class


# providertwo 0.0.0.9055 (2025-01-30)

* Simplified catalog loading


# providertwo 0.0.0.9054 (2025-01-28)

* added `is_complete_with_limit()` function factory for use with `iterate_with_offset()`
* `vna_if()` renamed to `map_na_if()`
* added `as_datetime()` helper to parse _RFC 3339_ ISO datetimes


# providertwo 0.0.0.9053 (2025-01-27)

* corrected `offset_sequence()` behavior


# providertwo 0.0.0.9052 (2025-01-26)

* added `start` parameter to `offset_sequence()`


# providertwo 0.0.0.9051 (2025-01-25)

* `offset_sequence()` now begins at `limit`, not `0`


# providertwo 0.0.0.9050 (2025-01-25)

* `Catalog_openpayments()` draft


# providertwo 0.0.0.9049 (2025-01-25)

* `offset_sequence()` now works
* lookups now internal


# providertwo 0.0.0.9048 (2025-01-25)

* removed constants from initial `.onLoad`


# providertwo 0.0.0.9047 (2025-01-25)

* `enrollees()` work


# providertwo 0.0.0.9046 (2025-01-25)

* bug fixed?
* `req_perform_iterative()`/`iterate_with_offset()` use in `enrollees()`


# providertwo 0.0.0.9045 (2025-01-24)

* Bug in `as_Dataset` - `dataset_Resources`


# providertwo 0.0.0.9044 (2025-01-24)

* added `provider_catalog` methods


# providertwo 0.0.0.9043 (2025-01-24)

* draft of `provider_dataset`


# providertwo 0.0.0.9042 (2025-01-24)

* added `Distribution` class, `as_Distribution`, and `handle_na()` helper


# providertwo 0.0.0.9041 (2025-01-24)

* `as_Dataset` draft


# providertwo 0.0.0.9040 (2025-01-23)

* changed structure of `query` helpers


# providertwo 0.0.0.9039 (2025-01-23)

* renamed `Resources` class to `dataset_Resources`


# providertwo 0.0.0.9038 (2025-01-23)

* shortened class names


# providertwo 0.0.0.9037 (2025-01-23)

* `class_API` renamed to `Dataset`


# providertwo 0.0.0.9036 (2025-01-22)

* args default `props`


# providertwo 0.0.0.9035 (2025-01-22)

* `query_helpers` first draft
* bumped `cheapr` version to 1.0.0


# providertwo 0.0.0.9034 (2025-01-21)

* added computed `fields` property to `class_Identifier`


# providertwo 0.0.0.9033 (2025-01-21)

* `luhn_check()` working again


# providertwo 0.0.0.9032 (2025-01-21)

* added `class_contactPoint()`, `class_publisher()`
* expanded `class_API()`


# providertwo 0.0.0.9031 (2025-01-20)

* fixed bug in `class_Resources()`


# providertwo 0.0.0.9030 (2025-01-19)

* imported standalone `purrr`, type checks
* removed `prettyunits` in favor of `rlang::parsebytes()`
* added `luhn_check()` for NPIs


# providertwo 0.0.0.9029 (2025-01-18)

* bump dependency versions (`collapse`, `httr2`, `S7`, `testthat`)


# providertwo 0.0.0.9028 (2025-01-17)

* `enrollee_API()` rework


# providertwo 0.0.0.9027 (2025-01-17)

* `class_args()` draft


# providertwo 0.0.0.9026 (2025-01-16)

* updated classes


# providertwo 0.0.0.9025 (2025-01-13)

* `offset_sequence()` rewrite


# providertwo 0.0.0.9024 (2025-01-11)

* `class_Resources()` draft


# providertwo 0.0.0.9023 (2025-01-11)

* `class_Identifier()` draft


# providertwo 0.0.0.9022 (2025-01-11)

* `enrolleeAPI()` class draft


# providertwo 0.0.0.9021 (2025-01-11)

* centralized `importFrom` calls
* general tidying
* Reverted dev `S7` and `httr2` dependencies


# providertwo 0.0.0.9020 (2025-01-11)

* removed `terse()` wrapper


# providertwo 0.0.0.9019 (2025-01-11)

* `class_API()` working again


# providertwo 0.0.0.9018 (2025-01-11)

* `class_API()` draft


# providertwo 0.0.0.9017 (2025-01-10)

* clean up


# providertwo 0.0.0.9016 (2025-01-10)

* fiddling with things


# providertwo 0.0.0.9015 (2025-01-10)

* gettin sendy


# providertwo 0.0.0.9014 (2025-01-10)

* class sketch


# providertwo 0.0.0.9013 (2025-01-09)

* tweaking initial `.onLoad`
* tweaked readme


# providertwo 0.0.0.9012 (2025-01-09)

* added Code of Conduct
* rebuilt Readme
* added pkgdown site


# providertwo 0.0.0.9011 (2025-01-06)

* clean up `NEWS` bullets
* further refinement of `enrollees()`


# providertwo 0.0.0.9010 (2025-01-06)

* moved `.onLoad` hooks to `aaa.R`


# providertwo 0.0.0.9009 (2025-01-06)

* minor alteration to `public_filter()`


# providertwo 0.0.0.9008 (2025-01-06)

* renamed `providers2()` to `enrollees()`


# providertwo 0.0.0.9007 (2025-01-06)

* `providers2()` draft


# providertwo 0.0.0.9006 (2025-01-06)

* `public_filter()` internal added


# providertwo 0.0.0.9005 (2025-01-06)

* `.onLoad` API call added


# providertwo 0.0.0.9004 (2025-01-02)

* removed `replace_fixed()`
* updated `use-standalone-helpers`


# providertwo 0.0.0.9003 (2025-01-01)

* Added lookups `program_code()` and `bureau_code()`


# providertwo 0.0.0.9002 (2025-01-01)

* Added utility functions, DCAT Schema `pins`


# providertwo 0.0.0.9001 (2024-12-27)

* Added `use-standalone-helpers`


# providertwo 0.0.0.9000 (2024-12-27)

* Initial commit
