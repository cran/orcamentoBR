#' Download expenditure data from the Brazilian federal budget.
#'
#' @description
#' This function downloads expenditure data from the Brazilian federal budget.
#' It resorts on the SIOP API. For more information on the Brazilian Budget,
#' please refer to the Budget Technical Manual, available in Portuguese at \href{https://www1.siop.planejamento.gov.br/mto/lib/exe/fetch.php/mto2025:mto2025.pdf}{Manual Técnico do Orçamento}.
#'
#' PT-BR: Essa função faz o download dos dados da despesa do orçamento da União do Brasil.
#' Os dados são baixados por meio da API do SIOP. Para mais informação sobre o
#' orçamento brasileiro, não deixe de consultar o \href{https://www1.siop.planejamento.gov.br/mto/lib/exe/fetch.php/mto2025:mto2025.pdf}{Manual Técnico do Orçamento}.
#'
#' @param exercicio A number. Indicates the year to which the extracted data refers. (Indica o ano a que se refere a extração)
#'
#' The following parameters are Boolean values. If a parameter is set to \code{TRUE},
#'  the returned dataframe will be detailed according to the respective dimension.
#'  If a parameter is set to \code{FALSE}, the figures will be aggregated.
#'
#'  (Os parâmetros que seguem são valores booleanos. Se um parâmetro estiver
#'  definido como \code{TRUE}, o dataframe retornado será detalhado de acordo com a
#'  respectiva dimensão. Se um parâmetro estiver definido como \code{FALSE},
#'  os valores serão agregados.)
#'
#' * Dimensions (Nível de detalhamento)
#'
#' QUALITATIVE CLASSIFICATION (CLASSIFICAÇÃO QUALITATIVA)
#'
#' Classification by Budget segment (Classificação por Esfera)
#'
#' @param Esfera Budget segment (Esfera Orçamentária)
#'
#' Institutional Classification (Classificação Institucional)
#'
#' @param Orgao Agency (Órgão)
#' @param UO Budgetary Unit (Unidade Orçamentária)
#'
#' Functional Classification (Classificação Funcional)
#'
#' @param Funcao Function (Função)
#' @param Subfuncao Subfunction (Subfunção)
#'
#' Programmatic Structure (Classificação Programática)
#'
#' @param Programa Program (Programa)
#' @param Acao Activity (Ação)
#' @param PlanoOrcamentario  Budget Plan (Plano orçamentárip)
#' @param Subtitulo Subtitle (Subtítulo)
#'
#' QUANTITATIVE CLASSIFICATION (CLASSIFICAÇÃO QUANTITATIVA)
#'
#' Detailed Financial Programming (Componentes da programação financeira)
#'
#' @param CategoriaEconomica Economic Category (categoria Econômica)
#' @param GND Nature of Expenditure (Grupo Natureza da Despesa)
#' @param ModalidadeAplicacao Application Mode (Modalidade de Aplicação)
#' @param ElementoDespesa Expenditure Element (Elemento da Despesa)
#'
#' @param Fonte Fonte de Recursos (Source of Funds)
#' @param IdUso Resources used as a counterpart Identifier (Identificador de recursos utilizado como contrapartida)
#' @param ResultadoPrimario Primary Result Identifier (Idenfificador de efeito sobre o resultado primário da União)
#'
#' The following parameters are Boolean values. If a parameter is set to \code{TRUE},
#'  the returned dataframe will include the respective figure. By default, all
#'  metrics are set to \code{TRUE}.
#'
#'  (Os seguintes parâmetros são valores booleanos. Se um parâmetro estiver
#'  definido como \code{TRUE}, o dataframe retornado incluirá a respectiva métrica.
#'  Todas as métricas são definidas como TRUE por padrão.)
#'
#' @param valorPLOA Annual Budget Bill value (Valor do PLOA)
#' @param valorLOA Annual Budget Law value - LOA (Valor da LOA)
#' @param valorLOAmaisCredito LOA value plus additional credits - Total authorized value (Valor da LOA adicionado de créditos adicionais)
#' @param valorEmpenhado Committed value (Valor empenhado)
#' @param valorLiquidado Verified value (Valor liquidado)
#' @param valorPago Paid value (Valor pago)
#'
#' @param incluiDescricoes A Boolean. If the parameter is set to \code{TRUE},
#'  the returned dataframe will include a column describing each selected dimension.
#'  If the parameter is set to \code{FALSE}, only the dimension codes will be returned.
#'  The default value is \code{TRUE}. (Se o parâmetro estiver definido como \code{TRUE},
#'  o dataframe retornado incluirá uma coluna descrevendo cada dimensão
#'  selecionada. Se o parâmetro estiver definido como \code{FALSE}, apenas os códigos
#'  das dimensões serão retornados. O valor padrão é \code{TRUE}.)
#'
#' @param detalheMaximo A Boolean. If the parameter is set to \code{TRUE}, the returned
#'  dataframe will be disaggregated across all available dimensions, overriding
#'  any dimension choices made by other parameters. If the parameter is set to
#'  \code{FALSE}, only the selected dimensions will be returned. The default value is \code{FALSE}.
#'  (Se o parâmetro estiver definido como \code{TRUE}, o dataframe retornado será
#'  desagregado em todas as dimensões disponíveis, substituindo qualquer escolha
#'  de dimensão feita por outros parâmetros. Se o parâmetro estiver definido como
#'  \code{FALSE}, apenas as dimensões selecionadas serão retornadas. O valor padrão é
#'  \code{FALSE}.)
#'
#' @param ignoreSecureCertificate A Boolean. If the parameter is set to \code{TRUE},
#'   the download of SIOP data will proceed while ignoring the secure certificate.
#'   (Se o parâmetro estiver definido como \code{TRUE}, o download dos dados
#'   do SIOP será realizado ignorando o certificado seguro.)
#' @param timeout An Integer. Milliseconds to server timeout. Values less than 1000 are
#'  ignored by SIOP server. (Milisegundos até o timeout do servidor. Valores
#'  menores que 1000 são ignorados pelo servidor.)
#' @param print_url A Boolean. If the parameter is set to \code{TRUE}, the function
#'  prints the constructed url before reaching the SIOP endpooint. (Se o parâmetro estiver
#'  definido como \code{TRUE}, a função exibe a url utilizanda antes de acessar o endpoint.)
#'
#' @returns The dataframe contains the expenditure figures. The columns are based
#'  on the choices made in the parameters.
#'
#'  PT-BR: O dataframe contém os valores de
#'  despesa. As colunas são baseadas nas escolhas feitas nos parâmetros.
#' @export
#'
#' @examples
#' \dontrun{despesaDetalhada(2020, UO = "73901", valorPLOA = FALSE, detalheMaximo = TRUE)}
#'
#' \dontrun{despesaDetalhada(exercicio = 2023, ResultadoPrimario = "6")}
#'
despesaDetalhada <-
  function(
    exercicio = .last_year()
    , Esfera = FALSE

    , Orgao = FALSE
    , UO = FALSE

    , Funcao = FALSE
    , Subfuncao = FALSE

    , Programa = FALSE
    , Acao = FALSE
    , PlanoOrcamentario = FALSE
    , Subtitulo = FALSE

    , CategoriaEconomica  = FALSE
    , GND = FALSE
    , ModalidadeAplicacao = FALSE
    , ElementoDespesa = FALSE

    , Fonte = FALSE
    , IdUso = FALSE
    , ResultadoPrimario = FALSE

    , valorPLOA = TRUE
    , valorLOA = TRUE
    , valorLOAmaisCredito = TRUE
    , valorEmpenhado = TRUE
    , valorLiquidado = TRUE
    , valorPago = TRUE

    , incluiDescricoes = TRUE

    , detalheMaximo = FALSE

    , ignoreSecureCertificate = FALSE
    , timeout = 0
    , print_url = FALSE
  ) {

    # Captura os argumentos passados na chamada da funcao
    args <- as.list(match.call()[-1])

    # Define os parametros mencionados explicitamente como TRUE, se n\u00E3o tiverem valores atribuidos
    .dimensoes <- .carregar_dataframe_dimensoes()
    params <- .dimensoes$param[2:17]

    if (detalheMaximo)
      for (param in params) {
        assign(param , TRUE)
      }

    if (grepl("^20\\d{2}$", exercicio)) {
      exercicio <- as.numeric(exercicio)
      if (exercicio < 2000 | exercicio > as.numeric(format(Sys.Date(), "%Y")))
        stop("Erro: exercicio deve ser um n\u00FAmero entre 2000 e o ano atual!")
    }
    else stop("Erro: exercicio deve ser um n\u00FAmero! E um n\u00FAmero entre 2000 e o ano atual!")


    query <- .constroiSIOPqueryDetalheAnual(
      exercicio = exercicio, Esfera = Esfera, Orgao = Orgao, UO = UO
      , Funcao = Funcao, Subfuncao = Subfuncao
      , Programa = Programa, Acao = Acao, PlanoOrcamentario = PlanoOrcamentario
      , Subtitulo = Subtitulo
      , CategoriaEconomica  = CategoriaEconomica, GND = GND
      , ModalidadeAplicacao = ModalidadeAplicacao, ElementoDespesa = ElementoDespesa
      , Fonte = Fonte, IdUso = IdUso, ResultadoPrimario = ResultadoPrimario
      , valorPLOA = valorPLOA, valorLOA = valorLOA, valorLOAmaisCredito = valorLOAmaisCredito
      , valorEmpenhado = valorEmpenhado, valorLiquidado = valorLiquidado
      , valorPago = valorPago
      , incluiDescricoes = incluiDescricoes
      , detalheMaximo = detalheMaximo
      , timeout = timeout
      , print_url = print_url
    )

    if (!is.integer(timeout)) timeout <- 0
    xurl <- .formaURLSIOP(query, timeout)

    if (print_url) cat(xurl, "\n")

    x <- .downloadSIOP(xurl, ignoreSecureCertificate)

    x <- .json2df(x)


    # Transforma valores em numeric
    if(!is.null(x$ploa))      x$ploa      <- as.numeric(x$ploa)
    if(!is.null(x$loa ))      x$loa       <- as.numeric(x$loa)
    if(!is.null(x$loa_mais_credito))  x$loa_mais_credito <- as.numeric(x$loa_mais_credito)
    if(!is.null(x$empenhado)) x$empenhado <- as.numeric(x$empenhado)
    if(!is.null(x$liquidado)) x$liquidado <- as.numeric(x$liquidado)
    if(!is.null(x$pago))      x$pago      <- as.numeric(x$pago)


    # Renomear as colunas
    novos_nomes <- sapply(names(x), function(nome) {
      if (nome == "codExercicio") return("exercicio")
      if (startsWith(nome, "cod")) {
        var <- substr(nome, 4, nchar(nome))
        novo_nome <- paste0(var, "_cod")
        return(novo_nome)
      } else if (startsWith(nome, "desc")) {
        var <- substr(nome, 5, nchar(nome))
        novo_nome <- paste0(var, "_desc")
        return(novo_nome)
      } else {
        return(nome)
      }
    })
    names(x) <- novos_nomes

    return(x)
  }

#' Download expenditure data from the Brazilian federal budget.
#'
#' @description
#' This function calls \code{\link[=despesaDetalhada]{despesaDetalhada()}}.
#'
#' @inheritParams despesaDetalhada
#'
#' @returns The dataframe contains the expenditure figures. The columns are based
#'  on the choices made in the parameters.
#' @export
#'
detailedExpenditure <- function(
    exercicio, Esfera, Orgao, UO, Funcao, Subfuncao
    , Programa, Acao, PlanoOrcamentario, Subtitulo
    , CategoriaEconomica, GND, ModalidadeAplicacao, ElementoDespesa
    , Fonte, IdUso, ResultadoPrimario
    , valorPLOA, valorLOA, valorLOAmaisCredito, valorEmpenhado, valorLiquidado
    , valorPago, incluiDescricoes, detalheMaximo, ignoreSecureCertificate
    , timeout, print_url) {
  return(despesaDetalhada(
    exercicio = exercicio, Esfera = Esfera, Orgao = Orgao, UO = UO
    , Funcao = Funcao, Subfuncao = Subfuncao, Programa = Programa
    , Acao = Acao, Subtitulo = Subtitulo, PlanoOrcamentario = PlanoOrcamentario
    , Fonte = Fonte, CategoriaEconomica = CategoriaEconomica, GND = GND
    , ModalidadeAplicacao = ModalidadeAplicacao, IdUso = IdUso
    , ResultadoPrimario = ResultadoPrimario, ElementoDespesa = ElementoDespesa
    , incluiDescricoes = incluiDescricoes
    , ignoreSecureCertificate = ignoreSecureCertificate
    , timeout = timeout, print_url = print_url
  ))
}
