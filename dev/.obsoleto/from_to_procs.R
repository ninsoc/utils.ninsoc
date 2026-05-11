# Diminui o número de núcleos utilizados para input/output de dados, para evitar
# algumas situações em que o Arrow trava ao gravar os arquivos.

# arrow::arrow_info()
# arrow::list_compute_functions()
# arrow::io_thread_count()
# arrow::cpu_count()

# set_io_thread_count(as.integer(0.5 * parallel::detectCores()))
# set_cpu_count(as.integer(0.5 * parallel::detectCores()))

# options(arrow.use_threads = FALSE)

#' Função para criar arquivos Parquet com novos parâmetros
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Função para criar arquivos Parquet com novos parâmetros
#' a partir de arquivos Parquet existentes.
#'
#' @param file_list Lista de arquivos a serem convertidos.
#' @param output_path Diretório onde os arquivos convertidos serão gravados.
#' @param replace Opção para sobrescrever os arquivos existentes.
#' @param compress_arrow Opção para otimizar os tipos de dados numéricos da tabela.
#' @param chunk_size Número de linhas em cada partição da tabela. Se não for
#'   especificada, será calculado um número ótimo de linhas.
#' @param int64 If \code{TRUE} then big integers could be converted to 64bit integers.
#'   Otherwise it would be converted to double precision (float64).
#'
#' @return Returns a list containing the converted file names.
#'
#' @import tictoc
#' @importFrom arrow read_parquet
#' @importFrom arrow write_parquet
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_c str_glue
#' @export
#'
#' @author Fabio M. Vaz
from_parquet_to_parquet = function(
  file_list = NULL,
  output_path = NULL,
  replace = TRUE,
  compress_arrow = TRUE,
  chunk_size = NULL,
  int64 = TRUE
) {
  if (missing(file_list)) {
    stop("Argument 'file_list' is missing, with no default.")
  }

  if (missing(output_path)) {
    stop("Argument 'output_path' is missing, with no default.")
  }

  if (!(replace %in% c(TRUE, FALSE))) {
    stop("Argument 'replace' must be true or false.")
  }

  if (!(compress_arrow %in% c(TRUE, FALSE))) {
    stop("Argument 'compress_arrow' must be true or false.")
  }

  if (!(int64 %in% c(TRUE, FALSE))) {
    stop("Argument 'int64' must be TRUE or FALSE")
  }

  bkp_options = options(arrow.use_threads = FALSE)

  re = list()
  for (file_path in file_list) {
    # file_path = file_list[1] # Para teste!

    file_name = basename(file_path)
    file_name_sans_ext = tools::file_path_sans_ext(file_name)
    out_file_name = stringr::str_c(file_name_sans_ext, ".parquet")

    if (replace == FALSE) {
      print(stringr::str_glue("Pulando o arquivo {file_name}"))
      if (file.exists(file.path(output_path, out_file_name))) {
        next
      }
    }

    msg = stringr::str_glue("Convertendo {file_name} para {out_file_name}")
    print(msg)
    tictoc::tic(msg)

    dados = arrow::read_parquet(file_path, as_data_frame = FALSE)

    if (is.null(chunk_size)) {
      chunk_size_choice = optimal_chunk_size(dados)
    } else {
      chunk_size_choice = chunk_size
    }

    if (compress_arrow == TRUE) {
      dados_arrow = compress_arrow(dados, int64 = int64)
    } else {
      dados_arrow = dados
    }

    rm(dados)
    gc()

    arrow::write_parquet(
      x = dados_arrow,
      sink = file.path(output_path, out_file_name),
      chunk_size = chunk_size_choice,
      coerce_timestamps = "ms",
      allow_truncated_timestamps = TRUE,
      compression = "gzip",
      compression_level = 9
    )

    re[[file_name]] = out_file_name

    tictoc::toc()
    rm(dados_arrow)
    gc()
  }

  on.exit(options(bkp_options), add = TRUE)

  return(invisible(re))
}


#' Função para criar arquivos FST a partir de arquivos Parquet
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Função para criar arquivos FST a partir de arquivos Parquet existentes.
#'
#' @param file_list Lista de arquivos a serem convertidos.
#' @param output_path Diretório onde os arquivos convertidos serão gravados.
#' @param replace Opção para sobrescrever os arquivos existentes.
#' @param compress_arrow Opção para otimizar os tipos de dados numéricos da tabela.
#' @param chunk_size Número de linhas em cada partição da tabela. Se não for
#'   especificada, será calculado um número ótimo de linhas.
#' @param int64 If \code{TRUE} then big integers could be converted to 64bit integers.
#'   Otherwise it would be converted to double precision (float64).
#'
#' @return Returns a list containing the converted file names.
#'
#' @import tictoc
#' @importFrom fst read_fst
#' @importFrom arrow write_parquet
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_c str_glue
#' @export
#'
#' @author Fabio M. Vaz
from_fst_to_parquet = function(
  file_list = NULL,
  output_path = NULL,
  replace = TRUE,
  compress_arrow = TRUE,
  chunk_size = NULL,
  int64 = TRUE
) {
  if (missing(file_list)) {
    stop("Argument 'file_list' is missing, with no default.")
  }

  if (missing(output_path)) {
    stop("Argument 'output_path' is missing, with no default.")
  }

  if (!(replace %in% c(TRUE, FALSE))) {
    stop("Argument 'replace' must be true or false.")
  }

  if (!(compress_arrow %in% c(TRUE, FALSE))) {
    stop("Argument 'compress_arrow' must be true or false.")
  }

  if (!(int64 %in% c(TRUE, FALSE))) {
    stop("Argument 'int64' must be TRUE or FALSE")
  }

  bkp_options = options(arrow.use_threads = FALSE)

  re = list()
  for (file_path in file_list) {
    # file_path = file_list[1] # Para teste!

    file_name = basename(file_path)
    file_name_sans_ext = tools::file_path_sans_ext(file_name)
    out_file_name = stringr::str_c(file_name_sans_ext, ".parquet")

    if (replace == FALSE) {
      if (file.exists(file.path(output_path, out_file_name))) {
        print(stringr::str_glue("Pulando o arquivo {file_name}"))
        next
      }
    }

    msg = stringr::str_glue("Convertendo {file_name} para {out_file_name}")
    print(msg)
    tictoc::tic(msg)

    dados = fst::read_fst(file_path)

    if (is.null(chunk_size)) {
      chunk_size_choice = optimal_chunk_size(dados)
    } else {
      chunk_size_choice = chunk_size
    }

    if (compress_arrow == TRUE) {
      dados_arrow = compress_arrow(dados, int64 = int64)
    } else {
      dados_arrow = dados
    }

    rm(dados)
    gc()

    arrow::write_parquet(
      x = dados_arrow,
      sink = file.path(output_path, out_file_name),
      chunk_size = chunk_size_choice,
      coerce_timestamps = "ms",
      allow_truncated_timestamps = TRUE,
      compression = "gzip",
      compression_level = 9
    )

    re[[file_name]] = out_file_name

    tictoc::toc()
    rm(dados_arrow)
    gc()
  }

  on.exit(options(bkp_options), add = TRUE)

  return(invisible(re))
}


#' Função para criar arquivos Parquet a partir de arquivos FST
#'
#' @description
#'`r lifecycle::badge("experimental")`
#'
#' Função para criar arquivos Parquet a partir de arquivos FST existentes.
#'
#' @param file_list Lista de arquivos a serem convertidos.
#' @param output_path Diretório onde os arquivos convertidos serão gravados.
#' @param replace Opção para sobrescrever os arquivos existentes.
#'
#' @return Returns a list containing the converted file names.
#'
#' @import tictoc
#' @importFrom fst write_fst
#' @importFrom arrow read_parquet
#' @importFrom tools file_path_sans_ext
#' @importFrom stringr str_c str_glue
#' @export
#'
#' @author Fabio M. Vaz
from_parquet_to_fst = function(
  file_list = NULL,
  output_path = NULL,
  replace = TRUE
) {
  if (missing(file_list)) {
    stop("Argument 'file_list' is missing, with no default.")
  }

  if (missing(output_path)) {
    stop("Argument 'output_path' is missing, with no default.")
  }

  if (!(replace %in% c(TRUE, FALSE))) {
    stop("Argument 'replace' must be true or false.")
  }

  bkp_options = options(arrow.use_threads = FALSE)

  re = list()
  for (file_path in file_list) {
    # file_path = file_list[1] # Para teste!

    file_name = basename(file_path)
    file_name_sans_ext = tools::file_path_sans_ext(file_name)
    out_file_name = stringr::str_c(file_name_sans_ext, ".fst")

    if (replace == FALSE) {
      if (file.exists(file.path(output_path, out_file_name))) {
        print(stringr::str_glue("Pulando o arquivo {file_name}"))
        next
      }
    }

    msg = stringr::str_glue("Convertendo {file_name} para {out_file_name}")
    print(msg)
    tictoc::tic(msg)

    dados = arrow::read_parquet(file_path)

    fst::write_fst(
      x = dados,
      path = file.path(output_path, out_file_name),
      compress = 100
    )

    re[[file_name]] = out_file_name

    tictoc::toc()
    rm(dados)
    gc()
  }

  on.exit(options(bkp_options), add = TRUE)

  return(invisible(re))
}
