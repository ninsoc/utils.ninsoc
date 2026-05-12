#' Label Values
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Create a factor variable using a named vector as a value-description pair.
#'
#' @details
#' The 'factor' function in R is weird because it asks you to inform both factor
#' levels and labels as separated vectors. This can be a problem for factors with
#' many levels, as the chance of errors arrising from different vector dimensions
#' or misaligment between labels and values is righer. The function 'label_values'
#' try to resolve this by asking the user to inform instead a value-description
#' format that can be made by specifying a named vector.
#'
#' @param x Vector to be transformed.
#' @param format Named vector structured as c('value' = 'description').
#' @param na String to be interpreted as missing values.
#' @return Factor variable.
#'
#' @export
#'
#' @examples
#' srvdata = readRDS(system.file("extdata", "srvdata.rds", package = "utils.ninsoc"))
#' table(srvdata$Died)
#' fmt_death = c("TRUE" = "Dead", "FALSE" = "Alive")
#' srvdata$Died = label_values(srvdata$Died, fmt_death)
#' table(srvdata$Died)
#'
#' @author Fabio M. Vaz
#' @seealso \code{\link[base]{factor}}
label_values = function(x, format, na = "NA") {
  # https://regexr.com/
  # (".+?") = ("?.+?"?)(,?\n)
  # $2 = $1$3

  if (missing(x)) {
    stop("Argument 'x' is missing, with no default.")
  }

  if (missing(format)) {
    stop("Argument 'format' is missing, with no default.")
  }

  if (!is.null(dim(x)[2])) {
    stop("Argument 'x' needs to be a vector.")
  }

  if (!is.null(dim(x)[2])) {
    stop("Argument 'format' needs to be a vector.")
  }

  if (is.null(names(format))) {
    stop("Argument 'format' needs to be a named vector.")
  }

  if (!assertthat::is.string(na)) {
    stop("Argument 'na' must be a character string.")
  }

  descr = c(format)
  valor = names(format)

  if (anyDuplicated(valor)) {
    stop("There are duplicated values in 'format'.")
  }

  # Substitui os missings
  valor = sub(na, NA, valor)

  # Transforma em fator
  re = factor(x, levels = valor, labels = descr, exclude = NULL)

  return(re)
}
