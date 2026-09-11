# data frame metadata validates its input

    Code
      df_variables(list(value = 1))
    Condition
      Error in `df_variables()`:
      ! Assertion on 'x' failed: Must be of type 'data.frame', not 'list'.

# file metadata validates paths and extensions

    Code
      fst_variables(1)
    Condition
      Error in `fst_variables()`:
      ! Assertion on 'file' failed: Must be of type 'string', not 'double'.

---

    Code
      fst_variables(rep(fst_file, 2))
    Condition
      Error in `fst_variables()`:
      ! Assertion on 'file' failed: Must have length 1.

---

    Code
      fst_variables(txt_file)
    Condition
      Error in `fst_variables()`:
      ! Assertion on 'file' failed: File extension must be in {'fst'} (case insensitive), but file name is '<tempdir>/utils-ninsoc-validation.txt'.

---

    Code
      fst_variables(missing_fst)
    Condition
      Error in `fst_variables()`:
      ! Assertion on 'file' failed: File does not exist: '<tempdir>/utils-ninsoc-validation.fst'.

---

    Code
      pq_variables(1)
    Condition
      Error in `pq_variables()`:
      ! Assertion on 'file' failed: Must be of type 'string', not 'double'.

---

    Code
      pq_variables(rep(parquet_file, 2))
    Condition
      Error in `pq_variables()`:
      ! Assertion on 'file' failed: Must have length 1.

---

    Code
      pq_variables(txt_file)
    Condition
      Error in `pq_variables()`:
      ! Assertion on 'file' failed: File extension must be in {'parquet'} (case insensitive), but file name is '<tempdir>/utils-ninsoc-validation.txt'.

---

    Code
      pq_variables(missing_parquet)
    Condition
      Error in `pq_variables()`:
      ! Assertion on 'file' failed: File does not exist: '<tempdir>/utils-ninsoc-validation.parquet'.

# compression helpers validate their inputs

    Code
      compress_needs_convert(environment())
    Condition
      Error in `compress_needs_convert()`:
      ! Assertion on 'col' failed: Must be of type 'vector', not 'environment'.

---

    Code
      compress_should_parallelize("3", 2)
    Condition
      Error in `compress_should_parallelize()`:
      ! Assertion on 'n_rows' failed: Must be of type 'number', not 'character'.

---

    Code
      compress_should_parallelize(3, "2")
    Condition
      Error in `compress_should_parallelize()`:
      ! Assertion on 'n_cols' failed: Must be of type 'number', not 'character'.

---

    Code
      compress_convert(1:3, 3)
    Condition
      Error in `compress_convert()`:
      ! Assertion on 'cols' failed: Must be of type 'list', not 'integer'.

---

    Code
      compress_convert(list(1:3), "3")
    Condition
      Error in `compress_convert()`:
      ! Assertion on 'n_rows' failed: Must be of type 'number', not 'character'.

# data frame compression validates its inputs

    Code
      compress_data(list(value = 1))
    Condition
      Error in `compress_data()`:
      ! Assertion on 'x' failed: Must be of type 'data.frame', not 'list'.

# Arrow compression validates its inputs

    Code
      compress_arrow(list(value = 1))
    Condition
      Error:
      ! Assertion on 'x' failed: One of the following must apply:
       * checkmate::check_data_frame(x): Must be of type 'data.frame', not
       * 'list'
       * checkmate::check_class(x): Must inherit from class 'ArrowTabular',
       * but has class 'list'.

---

    Code
      compress_arrow(df, int64 = 1)
    Condition
      Error in `compress_arrow()`:
      ! Assertion on 'int64' failed: Must be of type 'logical flag', not 'double'.

---

    Code
      compress_arrow(df, exclude = 1)
    Condition
      Error in `compress_arrow()`:
      ! Assertion on 'exclude' failed: Must be of type 'character' (or 'NULL'), not 'double'.

---

    Code
      compress_arrow(df, exclude = "missing")
    Condition
      Error in `compress_arrow()`:
      ! Assertion on 'exclude' failed: Must be a subset of {'value'}, but has additional elements {'missing'}.

# chunk sizing validates its inputs

    Code
      optimal_chunk_size(list(value = 1))
    Condition
      Error:
      ! Assertion on 'x' failed: One of the following must apply:
       * checkmate::check_data_frame(x): Must be of type 'data.frame', not
       * 'list'
       * checkmate::check_class(x): Must inherit from class 'ArrowTabular',
       * but has class 'list'.

---

    Code
      optimal_chunk_size(df, chunk_size_bytes = "1")
    Condition
      Error in `optimal_chunk_size()`:
      ! Assertion on 'chunk_size_bytes' failed: Must be of type 'number', not 'character'.

# Arrow casting validates its inputs

    Code
      cast_arrow_dtype(data.frame(value = 1:2), value, arrow::int8())
    Condition
      Error in `cast_arrow_dtype()`:
      ! Assertion on 'arrow_table' failed: Must inherit from class 'ArrowTabular', but has class 'data.frame'.

---

    Code
      cast_arrow_dtype(arrow_table, value, "int8")
    Condition
      Error in `cast_arrow_dtype()`:
      ! Assertion on 'data_type' failed: Must inherit from class 'ArrowObject'/'DataType', but has class 'character'.

---

    Code
      cast_arrow_dtype(arrow_table, missing, arrow::int8())
    Condition
      Error in `cast_arrow_dtype()`:
      ! Assertion on 'var_name' failed: Must be element of set {'value'}, but is 'missing'.

