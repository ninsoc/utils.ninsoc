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
#' @importFrom utils head
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' df_variables(srvdata)
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link{fst_variables}}
df_variables = function(x) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  if (!inherits(x, "data.frame")) {
    stop("Argument 'x' must be a data.frame.")
  }

  map = lapply(as.data.frame(head(x, 0)), class)

  vars_class = list()
  for (iter in 1:length(map)) {
    item = map[[iter]]
    last_item = item[length(item)] # Em alguns casos a variável tem mais de uma classe
    nome_item = names(map)[iter]

    vars_class[[nome_item]] = last_item
  }

  meta = dplyr::tibble(var = names(vars_class), type = as.character(vars_class))

  # O uso do .data$type é para não aparecer a mensagem abaixo no 'check' do pacote:
  # no visible binding for global variable 'type'
  meta = meta |>
    dplyr::mutate(
      type = dplyr::case_when(
        .data$type == "numeric" ~ "double",
        .data$type %in% c("POSIXt", "POSIXct") ~ "datetime",
        TRUE ~ .data$type
      )
    )

  attr(meta, "num_rows") = nrow(x)

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
#' @import assertthat
#' @import dplyr
#' @importFrom fst metadata_fst
#' @importFrom utils capture.output
#' @importFrom stringr str_match
#' @export
#'
#' @examples
#' fst_variables(system.file("extdata", "srvdata.fst", package = "utils.ninsoc"))
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link{df_variables}}, \code{\link{pq_variables}}
fst_variables = function(file) {
  assertthat::assert_that(
    assertthat::is.string(file),
    assertthat::has_extension(tolower(file), "fst"),
    file.exists(file)
  )

  vars_class = fst::metadata_fst(file)
  meta_txt <- utils::capture.output(vars_class)
  meta_parsed <- stringr::str_match(meta_txt, "'(.+)'\\s*:\\s*(.+)")

  meta_df =
    dplyr::tibble(
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

  attr(meta_df, "path") = basename(file)
  attr(meta_df, "data_type") = "fst"
  attr(meta_df, "num_rows") = vars_class$nrOfRows

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
#' @import assertthat
#' @import dplyr
#' @importFrom arrow ParquetFileReader
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_squish
#' @importFrom stringr str_detect
#' @export
#'
#' @examples
#' pq_variables(system.file("extdata", "srvdata.parquet", package = "utils.ninsoc"))
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link{df_variables}}, \code{\link{fst_variables}}
pq_variables = function(file) {
  if (missing(file)) {
    stop("Argument 'file' is missing, with no default.")
  }

  if (!assertthat::is.string(file)) {
    stop("Argument 'file' must be a character string.")
  }

  if (!assertthat::has_extension(tolower(file), "parquet")) {
    stop("Filename must end with an 'parquet' extension.")
  }

  if (!file.exists(file)) {
    stop(paste0("File '", file, "' does not exist."))
  }

  pq = arrow::ParquetFileReader$create(file)
  pq_schema = pq$GetSchema()

  vars_class = pq_schema$ToString() |>
    stringr::str_split("\\n", simplify = TRUE) |>
    stringr::str_split_fixed(":", 2)

  meta = tibble::tibble(var = vars_class[, 1], type = vars_class[, 2]) |>
    dplyr::inner_join(tibble(var = pq_schema$names), by = "var") |>
    dplyr::mutate(type = stringr::str_squish(.data$type)) |>
    dplyr::mutate(pq_type = .data$type) |>
    dplyr::mutate(
      type = case_when(
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

  attr(meta, "path") = basename(file)
  attr(meta, "data_type") = "parquet"
  attr(meta, "num_rows") = pq$num_rows

  return(meta)
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
#' @param x A \code{data.frame} object.
#' @return Returns a compressed \code{data.frame}.
#'
#' @import dplyr
#' @importFrom purrr map
#' @importFrom utils type.convert
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' dplyr::glimpse(srvdata)
#' dplyr::glimpse(compress_data(srvdata))
#'
#' @author Fabio M. Vaz
compress_data = function(x) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  if (!("data.frame" %in% class(x))) {
    stop("Argument 'x' must be of a data.frame class or equivalent.")
  }

  # O uso do .data$type é para não aparecer a mensagem abaixo no 'check' do pacote:
  # no visible binding for global variable 'type'

  vars_type = df_variables(x)

  # Converte variáveis POSIXct para Date

  # POSIXct_vars = filter(vars_type, .data$type %in% c("POSIXt", "POSIXct")) |>
  #   pull(.data$var)

  # if (length(POSIXct_vars) > 0) {
  #   x = x |>
  #     mutate(across(
  #       all_of(POSIXct_vars), as.Date
  #     ))
  # }

  # Identifica quais variáveis são do tipo Date e POSIXct e quais não são

  var_names = vars_type |> dplyr::pull(.data$var)

  datetime_vars =
    filter(
      vars_type,
      .data$type %in% c("Date", "IDate", "POSIXt", "POSIXct")
    ) |>
    dplyr::pull(.data$var)

  non_datetime_vars =
    filter(vars_type, !(.data$var %in% datetime_vars)) |>
    dplyr::pull(.data$var)

  # Separa o data.frame em duas partes, Date e non-Date

  df_datetime_vars = dplyr::select(x, all_of(datetime_vars))
  df_non_datetime_vars = dplyr::select(x, all_of(non_datetime_vars))

  # Otimiza as variáveis
  # O maior número inteiro que pode ser armazenado como "numeric" sem perda de
  # precisão é 9007199254740991. De forma geral, qualquer número inteiro
  # com até 15 dígitos pode ser armazenado sem perda de precisão como "numeric".
  # type.convert("9007199254740993", as.is = TRUE)
  # type.convert("9007199254740993", as.is = TRUE, numerals = "warn.loss")
  # type.convert("9007199254740993", as.is = TRUE, numerals = "no.loss")

  # Nesse processo, as variáveis Date acabam sendo convertidas para character.
  # Por isso eu tenho que dividir o data.frame
  # em duas partes (variáveis Date e variáveis não-Date), otimizar somente uma parte
  # e recombinar as colunas mantendo a ordem original das variáveis.

  # o lapply pode ser substituído pelo purrr::map, que por sua vez pode ser
  # substituído pelo furrr:future_map, que é equivalente ao purrr mas que
  # roda em paralelo. Vide https://davisvaughan.github.io/furrr/

  df_non_datetime_vars = tibble::as_tibble(
    purrr::map(
      df_non_datetime_vars,
      function(x) utils::type.convert(x, as.is = TRUE, numerals = "no.loss")
    )
  )

  re = dplyr::bind_cols(df_datetime_vars, df_non_datetime_vars) |>
    dplyr::select(all_of(var_names))

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
#' @import dplyr
#' @import arrow
#' @importFrom stringr str_split
#' @importFrom stringr str_split_fixed
#' @importFrom stringr str_squish
#' @importFrom stringr str_detect
#' @importFrom bit64 as.integer64
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' dplyr::glimpse(arrow::Table$create(srvdata))
#' dplyr::glimpse(compress_arrow(srvdata))
#'
#' @author Fabio M. Vaz
compress_arrow = function(x, int64 = FALSE, exclude = NULL) {
  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  if (!("data.frame" %in% class(x)) & !("ArrowTabular" %in% class(x))) {
    stop("Argument 'x' must be an Arrow Table or a data.frame.")
  }

  if (!(int64 %in% c(TRUE, FALSE))) {
    stop("Argument 'int64' must be TRUE or FALSE.")
  }

  if (!(is.null(exclude) | is.character(exclude))) {
    stop("Argument 'exclude' must be a character vector.")
  }

  set_diff_vars = setdiff(exclude, names(x))
  if (length(set_diff_vars) != 0) {
    stop(paste0("There is no '", set_diff_vars[1], "' variable in data.frame."))
  }

  bkp_options = options(arrow.use_threads = TRUE)
  on.exit(options(bkp_options), add = TRUE)

  # Converte o input para um Arrow Table
  if ("data.frame" %in% class(x)) {
    pq_table = arrow::Table$create(x)
  } else if ("ArrowTabular" %in% class(x)) {
    pq_table = x
  }

  pq_schema = pq_table$schema
  col_names = pq_table$ColumnNames()

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

  vars_class = pq_schema$ToString() |>
    stringr::str_split("\\n", simplify = TRUE) |>
    stringr::str_split_fixed(":", 2)

  meta = tibble::tibble(var = vars_class[, 1], type = vars_class[, 2]) |>
    dplyr::inner_join(tibble(var = pq_schema$names), by = "var") |>
    dplyr::mutate(type = stringr::str_squish(.data$type)) |>
    dplyr::mutate(pq_type = .data$type) |>
    dplyr::mutate(
      type = case_when(
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

  numeric_cols = dplyr::pull(meta[which(meta$type %in% c("integer", "double")), "var"])

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

    col_data = pq_table$GetColumnByName(column)
    col_data = col_data[!is.na(col_data)]

    # Se a coluna é composta somente por NULLs, ignorar
    if (col_data$length() == 0) {
      rm(col_data)
      gc()
      next
    }

    # Indice da coluna baseado em zero
    idx = which(col_names == column) - 1

    # Verifica se é número inteiro

    # 2147483647 # max integer in R
    # 9007199254740991 (ou seria 9007199254740994?) # max.noloss in R
    # 9223372036854775807 # max integer64 in MonetDB

    # Como o número é maior que max integer, eu uso floor ao invés de converter para integer
    # floor(90071992547409.1) == 90071992547409.1
    # floor(922337203685477.1) == 922337203685477.1
    # col_data = c(9007199254740991, 90071992547409.1)
    # ind_integer = all(as.character(bit64::as.integer64(col_data)) == as.character(col_data))

    if (class(col_data[1]$as_vector()) %in% c("integer", "integer64")) {
      ind_integer = TRUE
    } else {
      ind_integer = all(
        arrow::call_function("floor", col_data) == col_data
      )$as_vector()
    }

    if (ind_integer == TRUE) {
      # Se for inteiro

      min_value = min(col_data)$as_vector()
      max_value = max(col_data)$as_vector()

      if (min_value >= -127 & max_value <= 127) {
        pq_schema = pq_schema$SetField(idx, arrow::field(column, arrow::int8())) # TINYINT
      } else if (min_value >= -32767 & max_value <= 32767) {
        pq_schema = pq_schema$SetField(
          idx,
          arrow::field(column, arrow::int16())
        ) # SMALLINT
      } else if (min_value >= -2147483647 & max_value <= 2147483647) {
        pq_schema = pq_schema$SetField(
          idx,
          arrow::field(column, arrow::int32())
        ) # INTEGER
      } else {
        if (int64 == TRUE) {
          pq_schema = pq_schema$SetField(
            idx,
            arrow::field(column, arrow::int64())
          ) # BIGINT
        } else {
          pq_schema = pq_schema$SetField(
            idx,
            arrow::field(column, arrow::float64())
          ) # DOUBLE PRECISION
        }
      }
    } else if (ind_integer == FALSE) {
      # Se não for inteiro

      # The IEEE-754 basic 32-bit binary floating-point format
      # only guarantees that six significant decimal digits will survive a round-trip conversion
      ind_float32 = all(nchar(as.character(col_data)) <= 6)

      if (ind_float32 == TRUE) {
        pq_schema = pq_schema$SetField(
          idx,
          arrow::field(column, arrow::float32())
        ) # REAL
      } else {
        pq_schema = pq_schema$SetField(
          idx,
          arrow::field(column, arrow::float64())
        ) # DOUBLE PRECISION
      }
    }

    rm(col_data)
    gc()
  }

  # Altera o schema da tabela
  pq_table = pq_table$cast(pq_schema)

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
#' @import assertthat
#' @importFrom utils object.size
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
optimal_chunk_size = function(x, chunk_size_bytes = 500 * (1024^2)) {
  # FIXME: uma forma mais rápida de ver o tamanho da tabela é
  # capturar a classe das variáveis e imputar o espaço utilizado
  # por cada tipo de dado.

  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  if (!("data.frame" %in% class(x)) & !("ArrowTabular" %in% class(x))) {
    stop("Argument 'x' must be an Arrow Table or a data.frame.")
  }

  if (!assertthat::is.number(chunk_size_bytes)) {
    stop("Argument 'chunk_size_bytes' must be numeric.")
  }

  if ("data.frame" %in% class(x)) {
    size_bytes = as.numeric(utils::object.size(x))
  } else if ("ArrowTabular" %in% class(x)) {
    pct_sample = min(1, 42.3 * exp(-0.86 * log10(x$num_rows)))
    sample_data = x[1:as.integer(x$num_rows * pct_sample), ]
    sample_data = sample_data$to_data_frame()
    size_bytes = as.numeric(utils::object.size(sample_data)) / pct_sample
    rm(sample_data)
  }

  chunk_parts = ceiling(size_bytes / chunk_size_bytes)
  chunk_size_rows = as.integer(ceiling(nrow(x) / chunk_parts))

  return(chunk_size_rows)
}


# FIXME: Arrumar a seguinte mensagem quando importa rlang em DESCRIPTION
# Warning: replacing previous import 'assertthat::has_name' by 'rlang::has_name' when loading 'utils.ninsoc'
# Warning: replacing previous import 'arrow::string' by 'rlang::string' when loading 'utils.ninsoc'

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
#' @import assertthat
#' @importFrom rlang quo
#' @importFrom rlang as_name
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' srvdata_arrow = arrow::arrow_table(srvdata)
#' cast_arrow_dtype(srvdata_arrow, Died, arrow::int16())
#'
#' @author Fabio M. Vaz
cast_arrow_dtype = function(arrow_table, var_name, data_type) {
  # Usando non-standard evaluation
  quo_var_name = rlang::quo({{ var_name }})
  var_name = rlang::as_name(quo_var_name)

  if (!("ArrowTabular" %in% class(arrow_table))) {
    stop("Argument 'arrow_table' must be an Arrow Table.")
  }

  if (!("ArrowObject" %in% class(data_type) & "DataType" %in% class(data_type))) {
    stop("Argument 'data_type' must be an Arrow DataType.")
  }

  # Informações das colunas
  pq_schema = arrow_table$schema
  col_names = arrow_table$ColumnNames()

  if (!(var_name %in% col_names)) {
    stop(paste0("Column '", var_name, "' does not exists."))
  }

  # Indice da coluna baseado em zero
  column = var_name
  idx = which(col_names == column) - 1
  pq_schema = pq_schema$SetField(idx, arrow::field(column, data_type))

  # Altera o schema da tabela
  arrow_table = arrow_table$cast(pq_schema)

  return(arrow_table)
}
