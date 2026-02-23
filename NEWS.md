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
