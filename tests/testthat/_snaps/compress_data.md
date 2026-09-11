# compress_data validates exclude

    Code
      compress_data(df, exclude = 1)
    Condition
      Error in `compress_data()`:
      ! Assertion on 'exclude' failed: Must be of type 'character' (or 'NULL'), not 'double'.

---

    Code
      compress_data(df, exclude = "missing")
    Condition
      Error in `compress_data()`:
      ! Assertion on 'exclude' failed: Must be a subset of {'value'}, but has additional elements {'missing'}.

