# parse_sas_input_code validates its arguments

    Code
      parse_sas_input_code()
    Condition
      Error in `parse_sas_input_code()`:
      ! Argument 'sas_input_file' is missing, with no default.

---

    Code
      parse_sas_input_code(1)
    Condition
      Error in `parse_sas_input_code()`:
      ! Assertion on 'sas_input_file' failed: Must be of type 'string', not 'double'.

---

    Code
      parse_sas_input_code("does-not-exist.sas")
    Condition
      Error in `parse_sas_input_code()`:
      ! Assertion on 'sas_input_file' failed: File does not exist: 'does-not-exist.sas'.

---

    Code
      parse_sas_input_code(sas_file, beginline = "1")
    Condition
      Error in `parse_sas_input_code()`:
      ! Assertion on 'beginline' failed: Must be of type 'number', not 'character'.

---

    Code
      parse_sas_input_code(sas_file, lrecl = "1")
    Condition
      Error in `parse_sas_input_code()`:
      ! Assertion on 'lrecl' failed: Must be of type 'number' (or 'NULL'), not 'character'.

---

    Code
      parse_sas_input_code(sas_file, encoding = 1)
    Condition
      Error in `parse_sas_input_code()`:
      ! Assertion on 'encoding' failed: Must be of type 'string' (or 'NULL'), not 'double'.

# uncomment_sas_code validates its arguments

    Code
      utils.ninsoc:::uncomment_sas_code(1, "/*", "*/")
    Condition
      Error in `utils.ninsoc:::uncomment_sas_code()`:
      ! Assertion on 'SASinput' failed: Must be of type 'character', not 'double'.

---

    Code
      utils.ninsoc:::uncomment_sas_code("input value 1;", 1, "*/")
    Condition
      Error in `utils.ninsoc:::uncomment_sas_code()`:
      ! Assertion on 'starting.comment' failed: Must be of type 'string', not 'double'.

---

    Code
      utils.ninsoc:::uncomment_sas_code("input value 1;", "/*", 1)
    Condition
      Error in `utils.ninsoc:::uncomment_sas_code()`:
      ! Assertion on 'ending.comment' failed: Must be of type 'string', not 'double'.

# sas_input_dict validates its arguments

    Code
      sas_input_dict()
    Condition
      Error in `sas_input_dict()`:
      ! Argument 'sas_input_file' is missing, with no default.

---

    Code
      sas_input_dict(1)
    Condition
      Error in `sas_input_dict()`:
      ! Assertion on 'sas_input_file' failed: Must be of type 'string', not 'double'.

---

    Code
      sas_input_dict("does-not-exist.sas")
    Condition
      Error in `sas_input_dict()`:
      ! Assertion on 'sas_input_file' failed: File does not exist: 'does-not-exist.sas'.

---

    Code
      sas_input_dict(sas_file, file_ext = 1)
    Condition
      Error in `sas_input_dict()`:
      ! Assertion on 'file_ext' failed: Must be of type 'string', not 'double'.

---

    Code
      sas_input_dict(sas_file, encoding = 1)
    Condition
      Error in `sas_input_dict()`:
      ! Assertion on 'encoding' failed: Must be of type 'string' (or 'NULL'), not 'double'.

