#' Return a \code{data.frame} with variables renamed to original PNADC case
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' You know what it does.
#'
#' @param x A \code{data.frame}.
#' @return Return the original \code{data.frame} with variable renamed acording PNADC input scripts.
#'
#' @export
#'
#' @import dplyr
#' @importFrom rlang !! :=
#' @importFrom stringr regex str_detect
#'
#' @author Fabio M. Vaz
rename_to_pnadc_original_case = function(x) {
  if (!("data.frame" %in% class(x))) {
    stop("Argument 'x' must be of a data.frame class or equivalent.")
  }

  regex_pnadc_vars = stringr::regex(
    r"(\b(?:v|vd|vi|vdi|s|sd)\d+?[A-Z]?\d{0,2}\b)",
    ignore_case = TRUE
  )
  no_regex_pnadc_vars = c(
    "ID_DOMICILIO",
    "Ano",
    "Trimestre",
    "UF",
    "Capital",
    "RM_RIDE",
    "UPA",
    "Estrato",
    "posest",
    "posest_sxi"
  )
  no_regex_pnadc_vars_upcase = toupper(no_regex_pnadc_vars)

  df_names = names(x)

  for (varname in df_names) {
    varname_upcase = toupper(varname)

    if (stringr::str_detect(varname, regex_pnadc_vars)) {
      pnadc_varname = varname_upcase
      x = rename(x, !!pnadc_varname := !!varname)
    } else if (varname_upcase %in% toupper(no_regex_pnadc_vars)) {
      pnadc_varname = no_regex_pnadc_vars[
        varname_upcase == toupper(no_regex_pnadc_vars)
      ]
      x = rename(x, !!pnadc_varname := !!varname)
    }
  }

  return(x)
}


#' Cria um objeto de survey.design da PNADC Trimestral ou Anual
#' @description This function creates PNADC survey object with its sample design for analysis using \code{survey} package functions.
#' @param data_pnadc A tibble of PNADC microdata read with \code{read_pnadc} function.
#' @return An object of class \code{survey.design} or \code{svyrep.design} with the data from PNADC and its sample design.
#'
#' @importFrom survey svydesign postStratify svrepdesign calibrate
#' @importFrom srvyr as_survey
#' @export
#'
#' @author Fabio M. Vaz
#' @note
#' Essa função foi adaptada da função 'pnadc_design' do pacote "PNADcIBGE".
srvyr_pnadc_design_lowcase <- function(data_pnadc) {
  if (!("tbl_df" %in% class(data_pnadc))) {
    stop(
      "The microdata object is not of the tibble class or sample design was already defined for microdata"
    )
  }

  if (
    all(
      c(
        "upa",
        "id_domicilio",
        "estrato",
        "v1027",
        "v1028",
        "v1029",
        "v1033",
        "posest",
        "posest_sxi"
      ) %in%
        names(data_pnadc)
    ) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1030",
          "v1031",
          "v1032",
          "v1034",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      ) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1027",
          "v1028",
          "v1029",
          "posest"
        ) %in%
          names(data_pnadc)
      ) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1030",
          "v1031",
          "v1032",
          "posest"
        ) %in%
          names(data_pnadc)
      ) |
      all(c("upa", "id_domicilio", "estrato", "v1028") %in% names(data_pnadc)) |
      all(c("upa", "id_domicilio", "estrato", "v1032") %in% names(data_pnadc)) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1035",
          "v1036",
          "v1037",
          "v1038",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      ) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1039",
          "v1040",
          "v1041",
          "v1042",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      ) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1035",
          "v1036",
          "v1037",
          "posest"
        ) %in%
          names(data_pnadc)
      ) |
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1039",
          "v1040",
          "v1041",
          "posest"
        ) %in%
          names(data_pnadc)
      ) |
      all(c("upa", "id_domicilio", "estrato", "v1036") %in% names(data_pnadc)) |
      all(c("upa", "id_domicilio", "estrato", "v1040") %in% names(data_pnadc))
  ) {
    options(survey.lonely.psu = "adjust")
    options(survey.adjust.domain.lonely = TRUE)

    if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1027",
          "v1028",
          "v1029",
          "v1033",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC Trimestral - pós-estratificação geográfica e por sexo e faixa etária")

      if (all(sprintf("v1028%03d", seq(1:200)) %in% names(data_pnadc))) {
        # log_info("Usando pesos bootstrap")

        data_posterior <- survey::svrepdesign(
          data = data_pnadc,
          weight = ~v1028,
          type = "bootstrap",
          repweights = "v1028[0-9]+",
          mse = TRUE,
          replicates = length(sprintf("v1028%03d", seq(1:200))),
          df = length(sprintf("v1028%03d", seq(1:200)))
        )
      } else {
        # log_info("Usando calibragem")

        data_prior <- survey::svydesign(
          ids = ~ upa + id_domicilio,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1027,
          nest = TRUE
        )
        popc.types <- data.frame(
          posest = as.character(unique(data_pnadc$posest)),
          Freq = as.numeric(unique(data_pnadc$v1029))
        )
        popc.types <- popc.types[order(popc.types$posest), ]
        popi.types <- data.frame(
          posest_sxi = as.character(unique(data_pnadc$posest_sxi)),
          Freq = as.numeric(unique(data_pnadc$v1033))
        )
        popi.types <- popi.types[order(popi.types$posest_sxi), ]
        # pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
        # data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        data_posterior <- survey::calibrate(
          design = data_prior,
          formula = list(~posest, ~posest_sxi),
          pop = list(popc.types, popi.types),
          calfun = "raking",
          aggregate.stage = 2,
          bounds = c(0.2, 5),
          multicore = TRUE
        )
      }
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1030",
          "v1031",
          "v1032",
          "v1034",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC Anual - pós-estratificação geográfica e por sexo e faixa etária")

      if (all(sprintf("v1032%03d", seq(1:200)) %in% names(data_pnadc))) {
        # log_info("Usando pesos bootstrap")

        data_posterior <- survey::svrepdesign(
          data = data_pnadc,
          weight = ~v1032,
          type = "bootstrap",
          repweights = "v1032[0-9]+",
          mse = TRUE,
          replicates = length(sprintf("v1032%03d", seq(1:200))),
          df = length(sprintf("v1032%03d", seq(1:200)))
        )
      } else {
        # log_info("Usando calibragem")

        data_prior <- survey::svydesign(
          ids = ~ upa + id_domicilio,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1031,
          nest = TRUE
        )
        popc.types <- data.frame(
          posest = as.character(unique(data_pnadc$posest)),
          Freq = as.numeric(unique(data_pnadc$v1030))
        )
        popc.types <- popc.types[order(popc.types$posest), ]
        popi.types <- data.frame(
          posest_sxi = as.character(unique(data_pnadc$posest_sxi)),
          Freq = as.numeric(unique(data_pnadc$v1034))
        )
        popi.types <- popi.types[order(popi.types$posest_sxi), ]
        # pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
        # data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        data_posterior <- survey::calibrate(
          design = data_prior,
          formula = list(~posest, ~posest_sxi),
          pop = list(popc.types, popi.types),
          calfun = "raking",
          aggregate.stage = 2,
          bounds = c(0.2, 5),
          multicore = TRUE
        )
      }
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1027",
          "v1028",
          "v1029",
          "posest"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC Trimestral - somente pós-estratificação geográfica")

      # data_prior <- survey::svydesign(ids=~upa, strata=~estrato, data=data_pnadc, weights=~v1027, nest=TRUE)
      # popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$v1029)))
      # popc.types <- popc.types[order(popc.types$posest),]
      # data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)

      # creating desing object w/o poststratification
      data_prior <-
        survey::svydesign(
          ids = ~upa,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1027,
          nest = TRUE
        )

      # defining total for poststratification
      popc.types <- data.frame(
        posest = as.character(unique(data_pnadc$posest)),
        Freq = as.numeric(unique(data_pnadc$v1029))
      )

      # order data by post strata
      popc.types <- (popc.types[order(popc.types$posest), ])

      # creating final desing object w/ poststratification
      data_posterior <- survey::postStratify(
        design = data_prior,
        strata = ~posest,
        population = popc.types
      )
    } else if (all(c("upa", "id_domicilio", "estrato", "v1028") %in% names(data_pnadc))) {
      # log_info("PNADC Trimestral - não pós-estratificado")

      # data_posterior <- survey::svydesign(ids=~upa, strata=~estrato, data=data_pnadc, weights=~v1028, nest=TRUE)

      # creating desing object w/o poststratification
      data_posterior <-
        survey::svydesign(
          ids = ~upa,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1028,
          nest = TRUE
        )
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1030",
          "v1031",
          "v1032",
          "posest"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC Anual - somente pós-estratificação geográfica")

      # data_prior <- survey::svydesign(ids=~upa, strata=~estrato, data=data_pnadc, weights=~v1031, nest=TRUE)
      # popc.types <- data.frame(posest=as.character(unique(data_pnadc$posest)), Freq=as.numeric(unique(data_pnadc$v1030)))
      # popc.types <- popc.types[order(popc.types$posest),]
      # data_posterior <- survey::postStratify(design=data_prior, strata=~posest, population=popc.types)

      # creating desing object w/o poststratification
      data_prior <-
        survey::svydesign(
          ids = ~upa,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1031,
          nest = TRUE
        )

      # defining total for poststratification
      popc.types <- data.frame(
        posest = as.character(unique(data_pnadc$posest)),
        Freq = as.numeric(unique(data_pnadc$v1030))
      )

      # order data by post strata
      popc.types <- (popc.types[order(popc.types$posest), ])

      # creating final desing object w/ poststratification
      data_posterior <- survey::postStratify(
        design = data_prior,
        strata = ~posest,
        population = popc.types
      )
    } else if (all(c("upa", "id_domicilio", "estrato", "v1032") %in% names(data_pnadc))) {
      # log_info("PNADC Anual - não pós-estratificado")

      # data_posterior <- survey::svydesign(ids=~upa, strata=~estrato, data=data_pnadc, weights=~v1032, nest=TRUE)

      # creating desing object w/o poststratification
      data_posterior <-
        survey::svydesign(
          ids = ~upa,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1032,
          nest = TRUE
        )
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1035",
          "v1036",
          "v1037",
          "v1038",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC módulos individuais trimestrais")

      if (all(sprintf("v1036%03d", seq(1:200)) %in% names(data_pnadc))) {
        # log_info("Usando pesos bootstrap")

        data_posterior <- survey::svrepdesign(
          data = data_pnadc,
          weight = ~v1036,
          type = "bootstrap",
          repweights = "v1036[0-9]+",
          mse = TRUE,
          replicates = length(sprintf("v1036%03d", seq(1:200))),
          df = length(sprintf("v1036%03d", seq(1:200)))
        )
      } else {
        # log_info("Usando calibragem")

        data_prior <- survey::svydesign(
          ids = ~ upa + id_domicilio,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1035,
          nest = TRUE
        )
        popc.types <- data.frame(
          posest = as.character(unique(data_pnadc$posest)),
          Freq = as.numeric(unique(data_pnadc$v1037))
        )
        popc.types <- popc.types[order(popc.types$posest), ]
        popi.types <- data.frame(
          posest_sxi = as.character(unique(data_pnadc$posest_sxi)),
          Freq = as.numeric(unique(data_pnadc$v1038))
        )
        popi.types <- popi.types[order(popi.types$posest_sxi), ]
        # pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
        # data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        data_posterior <- survey::calibrate(
          design = data_prior,
          formula = list(~posest, ~posest_sxi),
          pop = list(popc.types, popi.types),
          calfun = "raking",
          aggregate.stage = 2,
          bounds = c(0.2, 5),
          multicore = TRUE
        )
      }
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1039",
          "v1040",
          "v1041",
          "v1042",
          "posest",
          "posest_sxi"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC módulos individuais anuais")

      if (all(sprintf("V1040%03d", seq(1:200)) %in% names(data_pnadc))) {
        # log_info("Usando pesos bootstrap")

        data_posterior <- survey::svrepdesign(
          data = data_pnadc,
          weight = ~v1040,
          type = "bootstrap",
          repweights = "v1040[0-9]+",
          mse = TRUE,
          replicates = length(sprintf("v1040%03d", seq(1:200))),
          df = length(sprintf("v1040%03d", seq(1:200)))
        )
      } else {
        # log_info("Usando calibragem")

        data_prior <- survey::svydesign(
          ids = ~ upa + id_domicilio,
          strata = ~estrato,
          data = data_pnadc,
          weights = ~v1039,
          nest = TRUE
        )
        popc.types <- data.frame(
          posest = as.character(unique(data_pnadc$posest)),
          Freq = as.numeric(unique(data_pnadc$v1041))
        )
        popc.types <- popc.types[order(popc.types$posest), ]
        popi.types <- data.frame(
          posest_sxi = as.character(unique(data_pnadc$posest_sxi)),
          Freq = as.numeric(unique(data_pnadc$v1042))
        )
        popi.types <- popi.types[order(popi.types$posest_sxi), ]
        # pop.rake.calib <- c(sum(popc.types$Freq), popc.types$Freq[-1], popi.types$Freq[-1])
        # data_posterior <- survey::calibrate(design=data_prior, formula=~posest+posest_sxi, pop=pop.rake.calib, calfun="raking", aggregate.stage=2, bounds=c(0.2,5), multicore=TRUE)
        data_posterior <- survey::calibrate(
          design = data_prior,
          formula = list(~posest, ~posest_sxi),
          pop = list(popc.types, popi.types),
          calfun = "raking",
          aggregate.stage = 2,
          bounds = c(0.2, 5),
          multicore = TRUE
        )
      }
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1035",
          "v1036",
          "v1037",
          "posest"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC módulos individuais trimestrais - pós-estratificação geográfica")

      data_prior <- survey::svydesign(
        ids = ~upa,
        strata = ~estrato,
        data = data_pnadc,
        weights = ~v1035,
        nest = TRUE
      )
      popc.types <- data.frame(
        posest = as.character(unique(data_pnadc$posest)),
        Freq = as.numeric(unique(data_pnadc$v1037))
      )
      popc.types <- popc.types[order(popc.types$posest), ]
      data_posterior <- survey::postStratify(
        design = data_prior,
        strata = ~posest,
        population = popc.types
      )
    } else if (all(c("upa", "id_domicilio", "estrato", "v1036") %in% names(data_pnadc))) {
      # log_info("PNADC módulos individuais trimestrais - não pós-estratificado")

      data_posterior <- survey::svydesign(
        ids = ~upa,
        strata = ~estrato,
        data = data_pnadc,
        weights = ~v1036,
        nest = TRUE
      )
    } else if (
      all(
        c(
          "upa",
          "id_domicilio",
          "estrato",
          "v1039",
          "v1040",
          "v1041",
          "posest"
        ) %in%
          names(data_pnadc)
      )
    ) {
      # log_info("PNADC módulos individuais anuais - pós-estratificação geográfica")

      data_prior <- survey::svydesign(
        ids = ~upa,
        strata = ~estrato,
        data = data_pnadc,
        weights = ~v1039,
        nest = TRUE
      )
      popc.types <- data.frame(
        posest = as.character(unique(data_pnadc$posest)),
        Freq = as.numeric(unique(data_pnadc$v1041))
      )
      popc.types <- popc.types[order(popc.types$posest), ]
      data_posterior <- survey::postStratify(
        design = data_prior,
        strata = ~posest,
        population = popc.types
      )
    } else if (all(c("upa", "id_domicilio", "estrato", "v1040") %in% names(data_pnadc))) {
      # log_info("PNADC módulos individuais anuais - não pós-estratificado")

      data_posterior <- survey::svydesign(
        ids = ~upa,
        strata = ~estrato,
        data = data_pnadc,
        weights = ~v1040,
        nest = TRUE
      )
    }
  } else {
    message("Weight variables required for sample design are missing.")
    data_posterior <- data_pnadc
  }

  # log_info("Aplicando a função srvyr::as_survey() no object 'survey'")
  data_posterior = srvyr::as_survey(data_posterior)

  return(data_posterior)
}
