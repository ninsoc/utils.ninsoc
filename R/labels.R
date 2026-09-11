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

  checkmate::assert_vector(x)
  checkmate::assert_null(dim(x), .var.name = "dim(x)")

  checkmate::assert_vector(format)
  checkmate::assert_null(dim(format), .var.name = "dim(format)")
  checkmate::assert_vector(format, names = "unique")

  checkmate::assert_string(na)

  descr = c(format)
  valor = names(format)

  # Substitui os missings
  valor = sub(na, NA, valor)

  # Transforma em fator
  re = factor(x, levels = valor, labels = descr, exclude = NULL)

  return(re)
}
