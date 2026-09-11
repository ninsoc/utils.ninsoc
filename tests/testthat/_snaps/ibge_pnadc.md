# pnadc_original_vars validates x

    Code
      pnadc_original_vars(list(x = 1))
    Condition
      Error in `pnadc_original_vars()`:
      ! Assertion on 'x' failed: Must be of type 'data.frame', not 'list'.

# pnadc_design_lowcase validates data_pnadc

    Code
      pnadc_design_lowcase(data.frame(upa = 1, id_domicilio = 1))
    Condition
      Error in `pnadc_design_lowcase()`:
      ! Assertion on 'data_pnadc' failed: Must be a tibble, not data.frame.

