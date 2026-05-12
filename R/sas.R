#' Create a Data Dictionary from a SAS Input Code
#'
#' @description
#' Create a \code{data.frame} with columns name, type and widths of a SAS file
#' retrieved from a SAS input code.
#'
#' @details
#' This function cannot handle overlapping columns.
#' Essa função funciona para a maioria, mas não para todas as situações.
#' Por exemplo, a função falha quando a leitura de um arquivo (INPUT) é interrompida
#' por um "@;" para posteriormente ser retomada por outro comando INPUT. Nesse caso
#' o codigo de importação SAS tem que ser editado previamente para que a função funcione.
#'
#' @param sas_input_file Path to a SAS imput code file.
#' @param beginline Line number in SAS import instructions where the INPUT statement begins.  If the word INPUT appears before the actual INPUT block, the function will return an error.
#' @param lrecl LRECL option from SAS code.  Only necessary if the width of the ASCII file is longer than the actual columns containing data (if the file contains empty space on the right side).
#' @param encoding File encoding (defaults to "cp1252"). See example.
#' @return This function returns a \code{data.frame} including columns:
#' \describe{
#'  \item{varname}{The name of the variable field}
#'  \item{width}{The width of the field}
#'  \item{char}{A logical flag indicating a character field if TRUE and numeric if FALSE}
#'  \item{divisor}{A fraction to later be multiplied by numeric fields containing decimal points}
#'  \item{start}{Field start position}
#'  \item{end}{Field ending position}
#' }
#'
#' @export
#'
#' @seealso \code{\link{sas_input_dict}}
#'
#' @source
#' This function is an adaptation of the parse.SAScii function from package SAScii
#' made by Anthony Joseph Damico (https://cran.r-project.org/package=SAScii).
#'
#' @author Fabio M. Vaz
parse_sas_input_code = function(
  sas_input_file,
  beginline = 1,
  lrecl = NULL,
  encoding = NULL
) {
  # NOTE: a função parse_sas_input_code funciona para a maioria, mas não para todas as situações.
  # Por exemplo, a função falha quando a leitura de um arquivo (INPUT) é interrompida
  # por um "@;" para posteriormente ser retomada por outro comando INPUT. Nesse caso
  # o codigo de importação SAS tem que ser editado previamente para que a função funcione.

  if (missing(sas_input_file)) {
    stop("Argument 'sas_input_file' is missing, with no default.")
  }

  if (!assertthat::is.string(sas_input_file)) {
    stop("Argument 'sas_input_file' must be a string.")
  }

  if (!file.exists(sas_input_file)) {
    stop(paste0("File '", sas_input_file, "' does not exist."))
  }

  if (!(is.null(lrecl) || assertthat::is.number(lrecl))) {
    stop("Argument 'lrecl' must be either NULL or a number.")
  }

  if (!(is.null(encoding) | assertthat::is.string(encoding))) {
    stop("Argument 'encoding' must be a string.")
  }

  # Define the file encoding

  if (is.null(encoding)) {
    choice_encoding = "CP1252"
    if (readr::guess_encoding(sas_input_file)[1, ] |> dplyr::pull(encoding) == "UTF-8") {
      choice_encoding = "UTF-8"
    }
    locale = readr::locale(encoding = choice_encoding)
  } else {
    locale = readr::locale(encoding = encoding)
  }

  # and now actually pull the entire file into R, line-by-line
  SASinput = readr::read_lines(sas_input_file, locale = locale)

  # remove all tab characters
  SASinput = gsub("\t", " ", SASinput)

  # if the SAS code includes more than one INPUT, start at the user-specified beginline
  SASinput = SASinput[beginline:length(SASinput)]

  # remote all /* and */ from the code
  SASinput = uncomment_sas_code(SASinput, "/*", "*/")

  # remote all * and ; from the code
  SASinput = uncomment_sas_code(SASinput, "*", ";")

  # find the first line with the word INPUT in it, which is where the ASCII variable locations occur.

  # lines that start with input
  firstline = grep("input", SASinput, ignore.case = TRUE)[1]

  # find the first semicolon ending that input line
  semicolon_loc = grep(";", SASinput)
  lastline = min(semicolon_loc[semicolon_loc > firstline])

  # isolate the Fixed-Width File (FWF) input lines
  FWFlines = SASinput[firstline:lastline]

  # remove the word input from the first line
  input_word = unlist(gregexpr("input", FWFlines[1], ignore.case = TRUE))
  FWFlines[1] = substr(FWFlines[1], input_word + 5, nchar(FWFlines[1]))

  # remove the semicolon from the last line
  semicolon = unlist(gregexpr(";", FWFlines[length(FWFlines)], fixed = TRUE))
  FWFlines[length(FWFlines)] = substr(
    FWFlines[length(FWFlines)],
    1,
    semicolon - 1
  )

  # put a space in front of all dollar signs
  for (i in 1:length(FWFlines)) FWFlines[i] = gsub("$", " $ ", FWFlines[i], fixed = TRUE)
  for (i in 1:length(FWFlines)) FWFlines[i] = gsub("-", " - ", FWFlines[i], fixed = TRUE)

  # remove all fully-blank lines
  FWFlines = FWFlines[which(gsub(" ", "", FWFlines) != "")]

  # break apart all FWF lines
  splited_lines = strsplit(FWFlines, " ", perl = TRUE)

  # initiate massive character vector
  SAS.input.lines = NULL

  for (i in 1:length(splited_lines)) {
    # throw out all splits that are empty
    splited_lines[[i]] = gsub("-", " ", splited_lines[[i]])
    splited_lines[[i]] = splited_lines[[i]][which(
      gsub(" ", "", splited_lines[[i]]) != ""
    )]

    # and then combine everything into one huge character vector
    SAS.input.lines = c(SAS.input.lines, splited_lines[[i]])
  }

  # create FWF structure file (x)
  x = data.frame(NULL)

  i <- j <- 1

  # pull out the second, third, and fourth elements after input line
  elements_2_4 = SAS.input.lines[2:4]
  # remove dollar signs for this test, they don't count
  elements_2_4 = elements_2_4[elements_2_4 != "$"]

  # figure out from the first line if the numbers are widths of each column
  # or if they're the actual location on the file.
  # look at the first line -- how many non $ numerics are there before you hit
  # the second variable name?
  widths_not_places =
    (length(elements_2_4) == 2 & # if there was a dollar sign, the length will be two
      is.na(as.numeric(as.character(elements_2_4[2]))))

  # look for any @ symbols in the input lines!
  if (sum(grepl("@", SAS.input.lines)) > 0) {
    # if the input lines appear to contain @START VARNAME FORMAT then use this block:

    # cycle through entire character vector
    while (i < length(SAS.input.lines)) {
      start.point = as.numeric(gsub("@", "", SAS.input.lines[i], fixed = TRUE))

      # skip the first time:
      if (i > 1) {
        # if there's room between the current start point and the previous width, add some empty space
        if (x[j - 1, "start"] + x[j - 1, "width"] < start.point) {
          # this creates a negative width
          x[j, "width"] = (x[j - 1, "start"] + x[j - 1, "width"]) - start.point
          j = j + 1
        }
      }

      # set first word to variable name
      x[j, "start"] = start.point
      x[j, "varname"] = SAS.input.lines[i + 1]

      # if there's a dollar sign between second word and the format, record that this is of type character
      if (SAS.input.lines[i + 2] == "$") {
        x[j, "char"] = TRUE
        i = i + 1
      } else {
        x[j, "char"] = FALSE
      }

      # remove leading f's and char's
      for (k in c("f", "char")) {
        SAS.input.lines[i + 2] = gsub(
          k,
          "",
          SAS.input.lines[i + 2],
          fixed = TRUE
        )
      }

      # if the length has a period, split it
      if (grepl(".", SAS.input.lines[i + 2], fixed = TRUE)) {
        period = unlist(gregexpr(".", SAS.input.lines[i + 2], fixed = TRUE))
        x[j, "width"] = as.numeric(substr(
          SAS.input.lines[i + 2],
          1,
          period - 1
        ))
        divisor = substr(
          SAS.input.lines[i + 2],
          period + 1,
          nchar(SAS.input.lines[i + 2])
        )
      } else {
        x[j, "width"] = as.numeric(SAS.input.lines[i + 2])
        divisor = ""
      }

      if (divisor != "") {
        x[j, "divisor"] = 1 / 10^as.numeric(divisor)
      } else {
        x[j, "divisor"] = 1
      }

      i = i + 3
      j = j + 1
    }
  } else if (widths_not_places) {
    # if the input lines appear to contain VARNAME LENGTH then use this block:

    # cycle through entire character vector
    while (i < length(SAS.input.lines)) {
      # set first word to variable name
      x[j, "varname"] = SAS.input.lines[i]

      # if there's a dollar sign between first word and the first number, record that this is of type character
      if (SAS.input.lines[i + 1] == "$") {
        x[j, "width"] = as.numeric(SAS.input.lines[i + 2])
        x[j, "char"] = TRUE
        i = i + 3

        # otherwise record that it's type numeric
      } else {
        x[j, "width"] = as.numeric(SAS.input.lines[i + 1])
        x[j, "char"] = FALSE
        i = i + 2
      }

      # search for a divisor
      if (grepl(".", SAS.input.lines[i], fixed = TRUE)) {
        period = unlist(gregexpr(".", SAS.input.lines[i], fixed = TRUE))

        divisor = substr(
          SAS.input.lines[i],
          period + 1,
          nchar(SAS.input.lines[i])
        )

        x[j, "divisor"] = 1 / 10^as.numeric(divisor)
        i = i + 1
      } else {
        x[j, "divisor"] = 1
      }

      # jump to the next row of x
      j = j + 1
    }
  } else {
    # if the input lines appear to contain VARNAME #START - #END then use this block:

    # cycle through entire character vector
    while (i < length(SAS.input.lines)) {
      # set first word to variable name
      x[j, "varname"] = SAS.input.lines[i]

      # if there's a dollar sign between first word and the first number, record that this is of type character
      if (SAS.input.lines[i + 1] == "$") {
        x[j, "start"] = SAS.input.lines[i + 2]

        # check if the width was one number or two..
        if (
          is.na(as.numeric(SAS.input.lines[i + 3])) | # #if it isn't numeric...
            grepl(".", SAS.input.lines[i + 3], fixed = TRUE)
        ) {
          # or if it contains a period...
          # then it's moved too far because the width was a single digit..
          x[j, "end"] = x[j, "start"]

          # and it should move back one
          i = i - 1
        } else {
          # otherwise, if it's a character string,
          x[j, "end"] = SAS.input.lines[i + 3]
        }

        x[j, "char"] = TRUE
        i = i + 4
      } else {
        # otherwise record that it's type numeric

        x[j, "start"] = SAS.input.lines[i + 1]

        # check if the width was one number or two..
        if (
          is.na(as.numeric(SAS.input.lines[i + 2])) | # if it isn't numeric..
            grepl(".", SAS.input.lines[i + 2], fixed = TRUE)
        ) {
          # or if it contains a period..

          # then it's moved too far because the width was a single digit..
          x[j, "end"] = x[j, "start"]

          # and it should move back one
          i = i - 1
        } else {
          # otherwise, if it's a character string,
          x[j, "end"] = SAS.input.lines[i + 2]
        }

        x[j, "char"] = FALSE
        i = i + 3
      }

      # search for a divisor
      if (grepl(".", SAS.input.lines[i], fixed = TRUE)) {
        period = unlist(gregexpr(".", SAS.input.lines[i], fixed = TRUE))

        divisor = substr(
          SAS.input.lines[i],
          period + 1,
          nchar(SAS.input.lines[i])
        )

        x[j, "divisor"] = 1 / 10^as.numeric(divisor)

        i = i + 1
      } else {
        x[j, "divisor"] = 1
      }

      # BUT if we're on the second row already..
      if (j > 1) {
        # IF current row's start > previous row's end + 1
        if (as.numeric(x[j, "start"]) > as.numeric(x[j - 1, "end"]) + 1) {
          # then you need to add in some blank space!
          x = rbind(x[1:(j - 1), ], NA, x[j, ])

          # add one to j, since you've added a row
          j = j + 1

          # and add a negative
          x[j - 1, "start"] = as.numeric(x[j - 2, "end"]) + 1
          x[j - 1, "end"] = as.numeric(x[j, "start"]) - 1
        }
      }

      # jump to the next row of x
      j = j + 1
    }

    # the width should be the end position minus the beginning position, plus one
    x = dplyr::mutate(x, width = as.numeric(.data$end) - as.numeric(.data$start) + 1)

    # if there's no variable name, it should be a negative.
    x[is.na(x[, "varname"]), "width"] = (-1 * x[is.na(x[, "varname"]), "width"])
  }

  # finally, if the final logical record length is specified by the user..
  if (!is.null(lrecl)) {
    # if it's the same as the sum of the widths already in x, do nothing (specifying it was unnecessary)

    # if it's less than the sum of the absolute values of current widths..
    if (lrecl < sum(abs(x$width)))
      stop(
        "specified logical record length (lrecl) parameter is shorter than the SAS columns constructed"
      )

    # if it's more than the sum of the absolute value of the current widths..
    if (lrecl > sum(abs(x$width))) {
      # blank space containing the difference should be added onto the tail of x
      length.of.blank.record.to.add.to.end = (lrecl - sum(abs(x$width)))

      x[nrow(x) + 1, "width"] = -length.of.blank.record.to.add.to.end
    }
  }

  # Start column
  if (!("start" %in% names(x))) {
    x[1, "start"] = 1
    x[2:nrow(x), "start"] = x[1, "start"] + cumsum(x[1:(nrow(x) - 1), "width"])
  }

  # End column
  if (!("end" %in% names(x))) {
    x = x |>
      dplyr::mutate(end = .data$start + .data$width - 1)
  }

  x = tibble::as_tibble(x)

  return(x)
}


#' Uncomment SAS Code
#'
#' @description
#' Internal function used to remove comments from SAS code.
#'
#' @source
#' This function is an adaptation of the SAS.uncomment function from package
#' SAScii made by Anthony Joseph Damico (https://cran.r-project.org/package=SAScii).
#'
#' @param SASinput A SAS code splited in a character vector.
#' @param starting.comment Comment start string.
#' @param ending.comment Comment ending string.
#' @return Character vector with SAS code without comments.
#'
#' @keywords internal
#' @noRd
uncomment_sas_code = function(SASinput, starting.comment, ending.comment) {
  if (!assertthat::is.string(starting.comment)) {
    stop("Argument 'starting.comment' must be a string.")
  }

  if (!assertthat::is.string(ending.comment)) {
    stop("Argument 'ending.comment' must be a string.")
  }

  # remove /* */
  for (i in 1:length(SASinput)) {
    # test if the line contains a slash_asterisk (or any opening comment character)
    slash_asterisk = unlist(gregexpr(
      starting.comment,
      SASinput[i],
      fixed = TRUE
    ))

    # test if the line contains a asterisk_slash (or any closing comment character)
    asterisk_slash = unlist(gregexpr(ending.comment, SASinput[i], fixed = TRUE))

    # only if the line contains an slash_asterisk
    if (!(-1 %in% slash_asterisk)) {
      # if there's a closing asterisk_slash on that line
      if (max(asterisk_slash) > min(slash_asterisk)) {
        SASinput[i] = sub(
          substr(SASinput[i], slash_asterisk[1], asterisk_slash[1] + 1),
          "",
          SASinput[i],
          fixed = TRUE
        )

        # and re-do the line just in case there's more than one comment!!
        i = i - 1
      } else {
        # delete the rest of the line
        SASinput[i] = sub(
          substr(SASinput[i], slash_asterisk[1], nchar(SASinput[i])),
          "",
          SASinput[i],
          fixed = TRUE
        )

        # start a new counter
        j = i

        # keep going until you find a asterisk_slash
        while (max(asterisk_slash) < 0) {
          j = j + 1

          # look for asterisk_slash again
          asterisk_slash = unlist(gregexpr(
            ending.comment,
            SASinput[j],
            fixed = TRUE
          ))

          # if the asterisk_slash doesn't exist, delete the whole line
          if (max(asterisk_slash) < 0) SASinput[j] = "" else
            # otherwise delete until the asterisk_slash
            SASinput[j] = sub(
              substr(SASinput[j], 1, asterisk_slash[1] + 1),
              "",
              SASinput[j],
              fixed = TRUE
            )
        }
      }
    }
  }

  return(SASinput)
}


#' List of Data Dictionaries from a SAS Input Code when more than one Input is present in the same
#' Input Code.
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This script iterate over a SAS input code, identify each input file, parse
#' the input code for each file and return a list contaning a data dictionary
#' for each file.
#'
#' @param sas_input_file Caminho completo para o script de leitura SAS.
#' @param file_ext Extensão dos arquivos de dados (normalmente "txt" ou "dat").
#' @param encoding Codificação do arquivo.
#'
#' @return This function returns a list whose entry is the filename and whose
#' value is a \code{data.frame} with the following columns:
#' \describe{
#'  \item{varname}{The name of the variable field}
#'  \item{width}{The width of the field}
#'  \item{char}{A logical flag indicating a character field if TRUE and numeric if FALSE}
#'  \item{divisor}{A fraction to later be multiplied by numeric fields containing decimal points}
#'  \item{start}{Field start position}
#'  \item{end}{Field ending position}
#' }
#'
#' @export
#'
#' @seealso \code{\link{parse_sas_input_code}}
#'
#' @author Fabio M. Vaz
sas_input_dict = function(sas_input_file, file_ext = "txt", encoding = NULL) {
  # NOTE: a função 'sas_input_dict' funciona para a maioria, mas não para todas as situações.
  # Por exemplo, a função falha quando a leitura de um arquivo (INPUT) é interrompida
  # por um "@;" para posteriormente ser retomada por outro comando INPUT. Nesse caso,
  # o codigo de importação SAS tem que ser editado previamente para que a função funcione corretamente.

  if (missing(sas_input_file)) {
    stop("Argument 'sas_input_file' is missing, with no default.")
  }

  if (!assertthat::is.string(sas_input_file)) {
    stop("Argument 'sas_input_file' must be a string.")
  }

  if (!file.exists(sas_input_file)) {
    stop(paste0("File '", sas_input_file, "' does not exist."))
  }

  if (!assertthat::is.string(file_ext)) {
    stop("Argument 'file_ext' must be a string.")
  }

  if (!(is.null(encoding) | assertthat::is.string(encoding))) {
    stop("Argument 'encoding' must be a string.")
  }

  # Define the file encoding

  if (is.null(encoding)) {
    choice_encoding = "CP1252"
    if (readr::guess_encoding(sas_input_file)[1, ] |> dplyr::pull(encoding) == "UTF-8") {
      choice_encoding = "UTF-8"
    }
    locale = readr::locale(encoding = choice_encoding)
  } else {
    locale = readr::locale(encoding = encoding)
  }

  # read SAS input file
  SASinput = readr::read_lines(sas_input_file, locale = locale)

  # remote all /* and */ from the code
  SASinput = uncomment_sas_code(SASinput, "/*", "*/")

  # remote all * and ; from the code
  SASinput = uncomment_sas_code(SASinput, "*", ";")

  # Lista os arquivos de dados e a linha onde se inicia o programa de leitura do SAS
  vec_data_files = c()

  for (line in 1:length(SASinput)) {
    line_content = as.character(SASinput[line])
    ext_regex = stringr::regex(paste0("\\.", file_ext), ignore_case = TRUE)
    if (!is.na(stringr::str_match(line_content, ext_regex))) {
      file_regex = stringr::regex(
        paste0("\\b\\w+?\\b\\.", file_ext),
        ignore_case = TRUE
      )
      vec_data_files[tolower(stringr::str_match(
        line_content,
        file_regex
      ))] = line
    }
  }

  # Cria uma lista com os dicionários de leitura
  dict_leitura = lapply(vec_data_files, function(x) {
    parse_sas_input_code(
      sas_input_file,
      beginline = x[],
      encoding = choice_encoding
    )
  })

  # Adiciona a coluna de 'start' e 'end'
  for (filename in names(dict_leitura)) {
    # Nome da tabela (nome do arquivo, sem extensão)
    tabela = tools::file_path_sans_ext(filename) |> tolower()

    # Dicionário de dados do arquivo
    dict = dict_leitura[filename][[1]]

    # Coluna inicial
    if (!("start" %in% names(dict))) {
      dict[1, "start"] = 1
      dict[2:nrow(dict), "start"] = dict[1, "start"] +
        cumsum(dict[1:(nrow(dict) - 1), "width"])
    }

    # Coluna final
    if (!("end" %in% names(dict))) {
      dict = dict |>
        dplyr::mutate(end = .data$start + .data$width - 1)
    }

    dict_leitura[filename][[1]] = dplyr::as_tibble(dict)
  }

  return(dict_leitura)
}
