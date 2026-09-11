# utils.ninsoc 0.0.0.9005

* Argument validation is now standardized with `checkmate`.
* `compress_data` is now much faster on large tables: it skips columns that are
  already at their minimal type (`integer`/`logical`/`complex`) and runs the
  per-column conversion in parallel via `purrr::in_parallel()` (`mirai` backend)
  when the data is large enough. Parallelism is automatic, produces an identical
  result, and falls back to sequential when `mirai`/`carrier` are unavailable.
  Tunable with options `utils.ninsoc.parallel`, `utils.ninsoc.workers`, and
  `utils.ninsoc.parallel_threshold`.
* `compress_data` gains an `exclude` argument to preserve selected variables
  without compression.
* Rename function `rename_to_pnadc_original_case` to `pnadc_original_vars`.
* Rename function `srvyr_pnadc_design_lowcase` to `pnadc_design_lowcase`.
* Add `label_values` to package exports.
* Remove functions `from_parquet_to_parquet`, `from_fst_to_parquet`, and `from_parquet_to_fst` (moved to `dev/.obsoleto/`).
* Remove `tictoc` from package dependencies.
* Remove unnecessary `bit64` import so the package installs in clean project libraries.
* Make `uncomment_sas_code` an internal function (no longer exported).
* Refactoring: replace broad `@import` directives with explicit `package::function()` calls throughout.

# utils.ninsoc 0.0.0.9004

* Rename functions: 'rename_to_pnadc_original_case' and 'srvyr_pnadc_design_lowcase'.

# utils.ninsoc 0.0.0.9003

* Add 'pnadc_design' function.

# utils.ninsoc 0.0.0.9002

* Remove 'tic_msg_fun' function.
* Remove survey, srvyr packages imports. Add 'stringr' package to import.
* Rename function 'get_original_pnadc_varnames' to 'rename_vars_pnadc_original_case' and correct bugs.

# utils.ninsoc 0.0.0.9001

* Depends R (>= 4.1) elimites maggrit import.
* Add argument 'exclude' to 'compress_arrow' function.
* Default parameter int64 = FALSE in 'compress_arrow' function.
* Deprecated 'tic_msg_fun' function.
* Function 'parse_sas_input_code' now returns original variables case.
* Remove 'locale' parameter from 'sas_input_dict' and 'parse_sas_input_code' functions. Instead of this, define a new parameter 'encoding' with no defaults. Function readr::guess_encoding will try to guess the file encoding.
* Add 'get_original_pnadc_varnames' function.
* Add 'pnadc_design_posest' and 'pnadc_design' function.

# utils.ninsoc 0.0.0.9000

* Initial release.
