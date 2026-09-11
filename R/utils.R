#' Get Variable Names and Types from a Data Frame
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Extracts variable names and their simplified data types from a data frame.
#'
#' @param x A `data.frame`.
#'
#' @return Returns a \code{data.frame} containing the following columns:
#' \describe{
#'   \item{var}{Variable name}
#'   \item{type}{Variable type (`integer`, `double`, `Date`, `character`, `logical`, `factor`, `datetime`)}
#' }
#'
#' Additional attributes include:
#' - `"num_rows"`: Number of rows in `x`
#'
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' df_variables(srvdata)
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link{fst_variables}}
df_variables <- function(x) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  checkmate::assert_data_frame(x)

  class_map <- lapply(as.data.frame(utils::head(x, 0)), class)

  vars_class <- list()
  for (iter in seq_along(class_map)) {
    item <- class_map[[iter]]
    last_item <- item[length(item)] # Em alguns casos a variável tem mais de uma classe
    nome_item <- names(class_map)[iter]

    vars_class[[nome_item]] <- last_item
  }

  meta <- dplyr::tibble(var = names(vars_class), type = as.character(vars_class))

  # O uso do .data$type é para não aparecer a mensagem abaixo no 'check' do pacote:
  # no visible binding for global variable 'type'
  meta <- meta |>
    dplyr::mutate(
      type = dplyr::case_when(
        .data$type == "numeric" ~ "double",
        .data$type %in% c("POSIXt", "POSIXct") ~ "datetime",
        TRUE ~ .data$type
      )
    )

  attr(meta, "num_rows") <- nrow(x)

  return(meta)
}


#' Variables Names and Types of a \code{fst} File
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Return the name and type of each \code{fst} file columns.
#'
#' @param file Path to a \code{fst} file.
#'
#' @return Returns a [tibble][tibble::tibble-package] containing the following columns:
#' \describe{
#'   \item{var}{Column name}
#'   \item{type}{Column type (`integer`, `double`, `Date`, `character`, `logical`, `factor`, `datetime`)}
#' }
#'
#' @export
#'
#' @examples
#' fst_variables(system.file("extdata", "srvdata.fst", package = "utils.ninsoc"))
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link{df_variables}}, \code{\link{pq_variables}}
fst_variables <- function(file) {
  checkmate::assert_string(file)
  checkmate::assert_file_exists(file, extension = "fst")

  vars_class <- fst::metadata_fst(file)
  meta_txt <- utils::capture.output(vars_class)
  meta_parsed <- stringr::str_match(meta_txt, "'(.+)'\\s*:\\s*(.+)")

  meta_df <- dplyr::tibble(
    var = meta_parsed[4:nrow(meta_parsed), 2],
    type = meta_parsed[4:nrow(meta_parsed), 3]
  ) |>
    dplyr::filter(!is.na(.data$var)) |> # remove header or malformed
    dplyr::mutate(
      type = dplyr::case_when(
        .data$type == "IDate" ~ "Date",
        .data$type %in% c("POSIXt", "POSIXct") ~ "datetime",
        TRUE ~ .data$type
      )
    )

  attr(meta_df, "path") <- basename(file)
  attr(meta_df, "data_type") <- "fst"
  attr(meta_df, "num_rows") <- vars_class$nrOfRows

  return(meta_df)
}


#' Variables Names and Types of a \code{parquet} File
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Return the name and type of each \code{parquet} file columns.
#'
#' @param file Path to a \code{parquet} file.
#'
#' @return Returns a [tibble][tibble::tibble-package] containing the following columns:
#' \describe{
#'   \item{var}{Column name}
#'   \item{type}{Data Frame column type (`integer`, `double`, `Date`, `character`, `logical`, `factor`, `datetime`)}
#'   \item{pq_type}{Parquet column type}
#' }
#'
#' @export
#'
#' @examples
#' pq_variables(system.file("extdata", "srvdata.parquet", package = "utils.ninsoc"))
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link{df_variables}}, \code{\link{fst_variables}}
pq_variables <- function(file) {
  if (missing(file)) {
    stop("Argument 'file' is missing, with no default.")
  }

  checkmate::assert_string(file)
  checkmate::assert_file_exists(file, extension = "parquet")

  pq <- arrow::ParquetFileReader$create(file)
  pq_schema <- pq$GetSchema()

  vars_class <- pq_schema$ToString() |>
    stringr::str_split("\\n", simplify = TRUE) |>
    stringr::str_split_fixed(":", 2)

  meta <- tibble::tibble(var = vars_class[, 1], type = vars_class[, 2]) |>
    dplyr::inner_join(tibble::tibble(var = pq_schema$names), by = "var") |>
    dplyr::mutate(type = stringr::str_squish(.data$type)) |>
    dplyr::mutate(pq_type = .data$type) |>
    dplyr::mutate(
      type = dplyr::case_when(
        .data$type == "bool" ~ "logical",
        .data$type == "int8" ~ "integer",
        .data$type == "int16" ~ "integer",
        .data$type == "int32" ~ "integer",
        .data$type == "int64" ~ "double",
        .data$type == "float" ~ "double",
        .data$type == "string" ~ "character",
        .data$type == "large_string" ~ "character",
        stringr::str_detect(.data$type, "^dictionary") ~ "factor",
        .data$type == "date32[day]" ~ "Date",
        stringr::str_detect(.data$type, "^timestamp") ~ "datetime",
        TRUE ~ .data$type
      )
    )

  attr(meta, "path") <- basename(file)
  attr(meta, "data_type") <- "parquet"
  attr(meta, "num_rows") <- pq$num_rows

  return(meta)
}


# Internal helpers for compress_data() --------------------------------------

# TRUE if a column still needs type.convert(). integer/logical/complex are exact
# no-ops under type.convert (verified), so they are skipped.
compress_needs_convert <- function(col) {
  checkmate::assert_vector(col)

  cl <- class(col)
  !(cl[length(cl)] %in% c("integer", "logical", "complex"))
}

# Number of parallel workers to use (physical cores by default).
compress_workers <- function() {
  n <- getOption("utils.ninsoc.workers", NULL)
  if (is.null(n)) {
    n <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
    if (is.na(n)) {
      n <- tryCatch(parallel::detectCores(), error = function(e) 1L)
    }
    if (is.na(n)) n <- 1L
  }
  max(1L, as.integer(n))
}

# TRUE if a mirai daemon pool is already set up (so we reuse it, not clobber it).
compress_daemons_active <- function() {
  tryCatch(isTRUE(as.integer(mirai::status()[["connections"]]) > 0L), error = function(e) FALSE)
}

# Decide whether parallelizing the per-column conversion is worthwhile.
compress_should_parallelize <- function(n_rows, n_cols) {
  checkmate::assert_number(n_rows)
  checkmate::assert_number(n_cols)

  if (!isTRUE(getOption("utils.ninsoc.parallel", TRUE))) {
    return(FALSE)
  }
  if (n_cols < 2L || compress_workers() < 2L) {
    return(FALSE)
  }
  if (!requireNamespace("mirai", quietly = TRUE)) {
    return(FALSE)
  }
  if (!requireNamespace("carrier", quietly = TRUE)) {
    return(FALSE)
  }
  if (!("in_parallel" %in% getNamespaceExports("purrr"))) {
    return(FALSE)
  }
  # Cost proxy: paralleliza quando o trabalho amortiza o dispatch e a eventual
  # inicialização (~1s) dos workers. Vide benchmarks (scripts de análise).
  threshold <- getOption("utils.ninsoc.parallel_threshold", 1e6)
  (as.numeric(n_rows) * n_cols) >= threshold
}

# Convert a list of columns with type.convert(), serial or parallel.
compress_convert <- function(cols, n_rows) {
  checkmate::assert_list(cols)
  checkmate::assert_number(n_rows)

  if (!compress_should_parallelize(n_rows, length(cols))) {
    return(purrr::map(cols, function(col) {
      utils::type.convert(col, as.is = TRUE, numerals = "no.loss")
    }))
  }

  # Reutiliza um pool de daemons já existente; caso contrário cria um pool
  # temporário e o encerra ao final (sem alterar a configuração do usuário).
  if (!compress_daemons_active()) {
    mirai::daemons(min(length(cols), compress_workers()))
    on.exit(mirai::daemons(0), add = TRUE)
  }

  purrr::map(
    cols,
    purrr::in_parallel(function(col) utils::type.convert(col, as.is = TRUE, numerals = "no.loss"))
  )
}


#' Compress Data.Frame Variable's Data Types
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Convert each \code{data.frame} column to the most memory saving data type.
#' This function doesn't change date or datetime columns, but it can convert
#' a string column into a numeric one.
#'
#' Columns that are already at their minimal type (\code{integer},
#' \code{logical}, \code{complex}) are skipped. For large tables the per-column
#' conversion runs in parallel via \code{purrr::in_parallel()} (\code{mirai}
#' backend), which is typically 3-5x faster. Parallelism is automatic and can be
#' tuned with the options below; it gracefully falls back to sequential
#' execution when \code{mirai}/\code{carrier} are not installed.
#'
#' @param x A \code{data.frame} object.
#' @param exclude Vector containing variable's names exclusion list. Those
#'   variables will not be compressed.
#' @return Returns a compressed \code{data.frame}, identical to the sequential
#'   result (parallelism never changes the output).
#'
#' @section Options:
#' \describe{
#'   \item{\code{utils.ninsoc.parallel}}{Set to \code{FALSE} to force sequential
#'     execution. Default \code{TRUE}.}
#'   \item{\code{utils.ninsoc.workers}}{Number of parallel workers. Default: the
#'     number of physical cores.}
#'   \item{\code{utils.ninsoc.parallel_threshold}}{Minimum work
#'     (\code{nrow * n_columns_to_convert}) to trigger parallelism. Default
#'     \code{1e6}.}
#' }
#' For repeated calls, set up a persistent pool once with
#' \code{mirai::daemons()} to avoid per-call worker startup; \code{compress_data}
#' reuses an existing pool and only creates a temporary one when none is set.
#'
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' dplyr::glimpse(srvdata)
#' dplyr::glimpse(compress_data(srvdata))
#'
#' @author Fabio M. Vaz
compress_data <- function(x, exclude = NULL) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  checkmate::assert_data_frame(x)
  checkmate::assert_character(exclude, null.ok = TRUE)
  checkmate::assert_subset(exclude, names(x))

  # O uso do .data$type é para não aparecer a mensagem abaixo no 'check' do pacote:
  # no visible binding for global variable 'type'

  vars_type <- df_variables(x)

  # Identifica quais variáveis são do tipo Date e POSIXct e quais não são

  var_names <- vars_type |> dplyr::pull(.data$var)

  datetime_vars <- dplyr::filter(
    vars_type,
    .data$type %in% c("Date", "IDate", "POSIXt", "POSIXct")
  ) |>
    dplyr::pull(.data$var)

  non_datetime_vars <- dplyr::filter(vars_type, !(.data$var %in% datetime_vars)) |>
    dplyr::pull(.data$var)

  # Separa o data.frame em duas partes, Date e non-Date

  df_datetime_vars <- dplyr::select(x, dplyr::all_of(datetime_vars))
  df_non_datetime_vars <- dplyr::select(x, dplyr::all_of(non_datetime_vars))

  # Otimiza as variáveis
  # De forma geral, qualquer número inteiro
  # com até 15 dígitos pode ser armazenado sem perda de precisão como "numeric".
  # type.convert("900719925474099", as.is = TRUE, numerals = "no.loss")

  # Nesse processo, as variáveis Date acabam sendo convertidas para character.
  # Por isso eu tenho que dividir o data.frame
  # em duas partes (variáveis Date e variáveis não-Date), otimizar somente uma parte
  # e recombinar as colunas mantendo a ordem original das variáveis.

  # As colunas integer/logical/complex já estão no tipo mínimo e type.convert é
  # um no-op nelas (verificado empiricamente), então são ignoradas. Somente
  # character, double e factor precisam ser processadas -- esse é o trabalho
  # pesado. Quando o volume de dados justifica, a conversão roda em paralelo via
  # purrr::in_parallel (backend mirai), 3-5x mais rápido em tabelas grandes.
  # Controlável pelas options 'utils.ninsoc.parallel', 'utils.ninsoc.workers' e
  # 'utils.ninsoc.parallel_threshold'.
  cols <- as.list(df_non_datetime_vars)
  to_convert <- which(
    vapply(cols, compress_needs_convert, logical(1)) & !(names(cols) %in% exclude)
  )

  if (length(to_convert) > 0) {
    cols[to_convert] <- compress_convert(cols[to_convert], nrow(x))
  }

  df_non_datetime_vars <- tibble::as_tibble(cols)

  re <- dplyr::bind_cols(df_datetime_vars, df_non_datetime_vars) |>
    dplyr::select(dplyr::all_of(var_names))

  return(re)
}


#' Compress Arrow Table Variable's Data Types
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Convert each \code{Arrow Table} column to the most memory saving data type.
#' This function doesn't change strings, date or datetime columns.
#'
#' @param x A \code{data.frame} or an \code{Arrow Table}.
#' @param int64 If \code{TRUE} then big integers could be converted to 64bit integers.
#'   Otherwise it would be converted to double precision (float64).
#' @param exclude Vector containing variable's names exclusion list. Those variables will not be
#'   compressed.
#' @return Returns an \code{Arrow Table} with columns converted to
#' the most memory saving data type.
#'
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' dplyr::glimpse(arrow::Table$create(srvdata))
#' dplyr::glimpse(compress_arrow(srvdata))
#'
#' @author Fabio M. Vaz
compress_arrow <- function(x, int64 = FALSE, exclude = NULL) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  checkmate::assert(
    checkmate::check_data_frame(x),
    checkmate::check_class(x, "ArrowTabular"),
    .var.name = "x"
  )
  checkmate::assert_flag(int64)
  checkmate::assert_character(exclude, null.ok = TRUE)
  checkmate::assert_subset(exclude, names(x))

  bkp_options <- options(arrow.use_threads = TRUE)
  on.exit(options(bkp_options), add = TRUE)

  # Converte o input para um Arrow Table
  if ("data.frame" %in% class(x)) {
    pq_table <- arrow::Table$create(x)
  } else if ("ArrowTabular" %in% class(x)) {
    pq_table <- x
  }

  pq_schema <- pq_table$schema
  col_names <- pq_table$ColumnNames()

  # pq_first_line = pq_table[1, ]$to_data_frame()
  #
  # col_types = purrr::map_chr(
  #   col_names,
  #   function(x) class(pull(pq_first_line[, x]))[1]
  # )
  #
  # rm(pq_first_line)
  #
  # numeric_cols = col_names[which(col_types %in% c("integer", "numeric"))]

  # Parquet Primitive Types
  # https://parquet.apache.org/documentation/latest/

  # BOOLEAN: 1 bit boolean
  # INT32: 32 bit signed ints
  # INT64: 64 bit signed ints
  # INT96: 96 bit signed ints
  # FLOAT: IEEE 32-bit floating point values
  # DOUBLE: IEEE 64-bit floating point values
  # BYTE_ARRAY: arbitrarily long byte arrays.

  vars_class <- pq_schema$ToString() |>
    stringr::str_split("\\n", simplify = TRUE) |>
    stringr::str_split_fixed(":", 2)

  meta <- tibble::tibble(var = vars_class[, 1], type = vars_class[, 2]) |>
    dplyr::inner_join(tibble::tibble(var = pq_schema$names), by = "var") |>
    dplyr::mutate(type = stringr::str_squish(.data$type)) |>
    dplyr::mutate(pq_type = .data$type) |>
    dplyr::mutate(
      type = dplyr::case_when(
        .data$type == "bool" ~ "logical",
        .data$type == "int8" ~ "integer",
        .data$type == "int16" ~ "integer",
        .data$type == "int32" ~ "integer",
        .data$type == "int64" ~ "double",
        .data$type == "float" ~ "double",
        .data$type == "string" ~ "character",
        .data$type == "large_string" ~ "character",
        stringr::str_detect(.data$type, "^dictionary") ~ "factor",
        .data$type == "date32[day]" ~ "Date",
        stringr::str_detect(.data$type, "^timestamp") ~ "datetime",
        TRUE ~ .data$type
      )
    )

  numeric_cols <- dplyr::pull(meta[which(meta$type %in% c("integer", "double")), "var"])

  for (column in numeric_cols) {
    if (column %in% exclude) {
      next
    }

    # cat(paste("Executando coluna", column, "\n"))

    # column = numeric_cols[11] # FIXME: COMENTAR ESSA LINHA. Usada somente para teste.

    # Coleta os dados da coluna
    # col_data = srvdata |>
    #   select(!!column) |>
    #   filter(across(all_of(column), function(x) !is.na(x))) |>
    #   pull()

    col_data <- pq_table$GetColumnByName(column)
    col_data <- col_data[!is.na(col_data)]

    # Se a coluna é composta somente por NULLs, ignorar
    if (col_data$length() == 0) {
      rm(col_data)
      gc()
      next
    }

    # Indice da coluna baseado em zero
    idx <- which(col_names == column) - 1

    # Verifica se é número inteiro

    # 2147483647 # max integer in R
    # 9007199254740991 (ou seria 9007199254740994?) # max.noloss in R
    # 9223372036854775807 # max integer64 in MonetDB

    # Como o número é maior que max integer, eu uso floor ao invés de converter para integer
    # floor(90071992547409.1) == 90071992547409.1
    # floor(922337203685477.1) == 922337203685477.1
    if (class(col_data[1]$as_vector()) %in% c("integer", "integer64")) {
      ind_integer <- TRUE
    } else {
      ind_integer <- all(arrow::call_function("floor", col_data) == col_data)$as_vector()
    }

    if (ind_integer == TRUE) {
      # Se for inteiro

      min_value <- min(col_data)$as_vector()
      max_value <- max(col_data)$as_vector()

      if (min_value >= -127 & max_value <= 127) {
        pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::int8())) # TINYINT
      } else if (min_value >= -32767 & max_value <= 32767) {
        pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::int16())) # SMALLINT
      } else if (min_value >= -2147483647 & max_value <= 2147483647) {
        pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::int32())) # INTEGER
      } else {
        if (int64 == TRUE) {
          pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::int64())) # BIGINT
        } else {
          pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::float64())) # DOUBLE PRECISION
        }
      }
    } else if (ind_integer == FALSE) {
      # Se não for inteiro

      # The IEEE-754 basic 32-bit binary floating-point format
      # only guarantees that six significant decimal digits will survive a round-trip conversion
      ind_float32 <- all(nchar(as.character(col_data)) <= 6)

      if (ind_float32 == TRUE) {
        pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::float32())) # REAL
      } else {
        pq_schema <- pq_schema$SetField(idx, arrow::field(column, arrow::float64())) # DOUBLE PRECISION
      }
    }

    rm(col_data)
    gc()
  }

  # Altera o schema da tabela
  pq_table <- pq_table$cast(pq_schema)

  return(pq_table)
}


#' Define an Optimal Chunk Size for a Parquet file
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Define an optimal chunk size (in number of rows) given a max memory partition.
#' This can be used, for example, to define the chunk size of a parquet file.
#' Chunk sizes can be defined using the formula \code{chunk_size_bytes = size*(1024^unit)},
#' where 'unit' can be 1 = Kb, 2 = Mb, 3 = Gb and so on.
#'
#' @param x A \code{data.frame} or an \code{Arrow Table}.
#' @param chunk_size_bytes Expected memory size (in bytes) for each chunk. Default 500Mb.
#'
#' @return Returns the optimal number of rows for each chunk.
#'
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' utils::object.size(srvdata)
#' nrow(srvdata)
#' srvdata_size_by_four = as.numeric(utils::object.size(srvdata))/4
#' optimal_chunk_size(srvdata, chunk_size_bytes = srvdata_size_by_four)
#'
#' @author Fabio M. Vaz
optimal_chunk_size <- function(x, chunk_size_bytes = 500 * (1024^2)) {
  # FIXME: uma forma mais rápida de ver o tamanho da tabela é
  # capturar a classe das variáveis e imputar o espaço utilizado
  # por cada tipo de dado.

  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  checkmate::assert(
    checkmate::check_data_frame(x),
    checkmate::check_class(x, "ArrowTabular"),
    .var.name = "x"
  )
  checkmate::assert_number(chunk_size_bytes)

  if ("data.frame" %in% class(x)) {
    size_bytes <- as.numeric(utils::object.size(x))
  } else if ("ArrowTabular" %in% class(x)) {
    pct_sample <- min(1, 42.3 * exp(-0.86 * log10(x$num_rows)))
    sample_data <- x[1:as.integer(x$num_rows * pct_sample), ]
    sample_data <- sample_data$to_data_frame()
    size_bytes <- as.numeric(utils::object.size(sample_data)) / pct_sample
    rm(sample_data)
  }

  chunk_parts <- ceiling(size_bytes / chunk_size_bytes)
  chunk_size_rows <- as.integer(ceiling(nrow(x) / chunk_parts))

  return(chunk_size_rows)
}


#' Cast an Arrow Table variable to Another Data Type
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Change the data type of an Arrow Table column.
#'
#' @param arrow_table An \code{Arrow Table}.
#' @param var_name Variable which would be casted to another data type.
#' @param data_type An \code{Arrow} data type.
#'
#' @return An \code{Arrow Table}.
#'
#' @importFrom rlang quo as_name
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' srvdata_arrow = arrow::arrow_table(srvdata)
#' cast_arrow_dtype(srvdata_arrow, Died, arrow::int16())
#'
#' @author Fabio M. Vaz
cast_arrow_dtype <- function(arrow_table, var_name, data_type) {
  # Usando non-standard evaluation
  quo_var_name <- quo({{ var_name }})
  var_name <- as_name(quo_var_name)

  checkmate::assert_class(arrow_table, "ArrowTabular")
  checkmate::assert_multi_class(data_type, c("ArrowObject", "DataType"))

  # Informações das colunas
  pq_schema <- arrow_table$schema
  col_names <- arrow_table$ColumnNames()

  checkmate::assert_choice(var_name, col_names)

  # Indice da coluna baseado em zero
  column <- var_name
  idx <- which(col_names == column) - 1
  pq_schema <- pq_schema$SetField(idx, arrow::field(column, data_type))

  # Altera o schema da tabela
  arrow_table <- arrow_table$cast(pq_schema)

  return(arrow_table)
}
