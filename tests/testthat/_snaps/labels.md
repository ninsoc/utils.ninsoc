# label_values validates x

    Code
      label_values()
    Condition
      Error in `label_values()`:
      ! Argument 'x' is missing, with no default.

---

    Code
      label_values(new.env(), c(`1` = "one"))
    Condition
      Error in `label_values()`:
      ! Assertion on 'x' failed: Must be of type 'vector', not 'environment'.

---

    Code
      label_values(matrix(1:4, nrow = 2), c(`1` = "one"))
    Condition
      Error in `label_values()`:
      ! Assertion on 'dim(x)' failed: Must be NULL.

# label_values validates format

    Code
      label_values(1:2)
    Condition
      Error in `label_values()`:
      ! Argument 'format' is missing, with no default.

---

    Code
      label_values(1:2, new.env())
    Condition
      Error in `label_values()`:
      ! Assertion on 'format' failed: Must be of type 'vector', not 'environment'.

---

    Code
      label_values(1:2, dimensional_format)
    Condition
      Error in `label_values()`:
      ! Assertion on 'dim(format)' failed: Must be NULL.

---

    Code
      label_values(1:2, c("one", "two"))
    Condition
      Error in `label_values()`:
      ! Assertion on 'format' failed: Must have names.

---

    Code
      label_values(1:2, c(`1` = "one", `1` = "another one"))
    Condition
      Error in `label_values()`:
      ! Assertion on 'format' failed: Must have unique names, but element 2 is duplicated.

# label_values validates na

    Code
      label_values(1:2, c(`1` = "one", `2` = "two"), na = 1)
    Condition
      Error in `label_values()`:
      ! Assertion on 'na' failed: Must be of type 'string', not 'double'.

---

    Code
      label_values(1:2, c(`1` = "one", `2` = "two"), na = c("NA", ""))
    Condition
      Error in `label_values()`:
      ! Assertion on 'na' failed: Must have length 1.

