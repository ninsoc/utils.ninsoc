check_package <- function() {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' is required to run the package check.", call. = FALSE)
  }

  if (.Platform$OS.type == "windows") {
    default_temp <- "C:/rt"
    check_temp <- Sys.getenv("UTILS_NINSOC_TMPDIR", unset = default_temp)

    dir.create(check_temp, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(check_temp)) {
      stop(sprintf("Unable to create the temporary directory '%s'.", check_temp), call. = FALSE)
    }

    write_probe <- tempfile("utils-ninsoc-", tmpdir = check_temp)
    can_write <- suppressWarnings(file.create(write_probe))
    if (isTRUE(can_write)) {
      unlink(write_probe)
    } else {
      stop(sprintf("The temporary directory '%s' is not writable.", check_temp), call. = FALSE)
    }

    check_temp <- normalizePath(check_temp, winslash = "/", mustWork = TRUE)
    Sys.setenv(TMPDIR = check_temp, TMP = check_temp, TEMP = check_temp)
    message("Using temporary directory: ", check_temp)
  }

  devtools::check(args = "--no-build-vignettes", error_on = "note")
}

check_package()
