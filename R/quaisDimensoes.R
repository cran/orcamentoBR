#' List of Members in a Brazilian Federal Budget Dimension.
#'
#' @description
#' This function lists all members in a Brazilian Federal Budget dimension for a given year.
#'
#' @param exercicio A number. Indicates the year to which the extracted data refers. (Indica o ano a que se refere a extração)
#' @param dimensao A string. The dimension to be listed. (A dimensão a ser relacionada)
#'  Valid values (Valores válidos): \link{dimensoes}
#' @param ignoreSecureCertificate A Boolean. If the parameter is set to \code{TRUE},
#'   the download of SIOP data will proceed while ignoring the secure certificate.
#'   (Se o parâmetro estiver definido como \code{TRUE}, o download dos dados
#'   do SIOP será realizado ignorando o certificado seguro.)
#'
#' @export
#'
#' @returns The dataframe contains the members in a dimension. Both code and description are provided.
#'
#' @examples
#' \dontrun{quaisMembros(2023, dimensao = "Funcao")}
#'
quaisMembros <- function(exercicio = .last_year()
                           , dimensao
                         , ignoreSecureCertificate = FALSE) {
  .dimensoes <- .carregar_dataframe_dimensoes()
  if (!is.character(dimensao))
    stop(paste("Error. dimensao should be a string!\ndimensao deve ser uma string!"
               , .dimensoes$param[2:17]))

  if (sum(.dimensoes$param[2:17] == dimensao) == 0)
    stop(paste("Error [dimensao] does not contain a valid value!\nValid values (valores v\u00E1lidos):"
               , paste(.dimensoes$param[2:17], collapse = ", ")))

  query <- .constroiSIOPqueryDimensoes(exercicio = exercicio, dimensao = dimensao[1])

  xurl <- .formaURLSIOP(query)

  x <- .downloadSIOP(xurl, ignoreSecureCertificate)

  x <- .json2df(x)

  names(x) <- paste0(dimensao, c("_cod", "_desc"))

  return(x)
}

#' List of Members in a Brazilian Federal Budget Dimension.
#'
#' @description
#' This function calls [quaisMembros].
#'
#' @param exercicio A number. Indicates the year to which the extracted data refers.
#' @param dimensao A string. The dimension to be listed.
#'   Valid values: [dimensoes]
#' @param ignoreSecureCertificate A Boolean. If the parameter is set to \code{TRUE},
#'   the download of SIOP data will proceed while ignoring the secure certificate.
#'   (Se o parâmetro estiver definido como \code{TRUE}, o download dos dados
#'   do SIOP será realizado ignorando o certificado seguro.)
#'
#' @returns The dataframe contains the members in a dimension. Both code and description are provided.
#' @export
#'
#' @examples
#' \dontrun{whichMembers(2023, dimensao = "Funcao")}
#'
whichMembers <- function(exercicio = .last_year(), dimensao
                         , ignoreSecureCertificate = FALSE) {
  return(quaisMembros(exercicio = exercicio, dimensao = dimensao
                      , ignoreSecureCertificate = ignoreSecureCertificate))
}
