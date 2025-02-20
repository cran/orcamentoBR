#' Build the SPARQL to query the SIOP according to the annual breakdown.
#'
#' This function builds the SPARQL query to query the SIOP according to the annual breakdown.
#'
#' @inheritParams despesaDetalhada
#'
#' @returns A string. A SPARQL query.
#'
#' @noRd
.constroiSIOPqueryDetalheAnual <- function(
    exercicio, Esfera, Orgao, UO, Funcao, Subfuncao
    , Programa, Acao, PlanoOrcamentario, Subtitulo
    , CategoriaEconomica, GND, ModalidadeAplicacao, ElementoDespesa
    , Fonte, IdUso, ResultadoPrimario
    , valorPLOA, valorLOA, valorLOAmaisCredito, valorEmpenhado, valorLiquidado
    , valorPago, incluiDescricoes, detalheMaximo) {

  # Funcao auxiliar para adicionar trechos de codigo
  escreve_descritores <-
    function(query
             , Esfera, Orgao, UO, Funcao, Subfuncao
             , Programa, Acao, Subtitulo, PlanoOrcamentario
             , Fonte, CategoriaEconomica, GND, ModalidadeAplicacao
             , IdUso, ResultadoPrimario, ElementoDespesa
             , incluiDescricoes) {
      if (Esfera != FALSE) { query <- paste(query, "?codEsfera")
      if (incluiDescricoes) query <- paste(query, "?descEsfera")
      }
      if (Orgao != FALSE) { query <- paste(query, "?codOrgao")
      if (incluiDescricoes) query <- paste(query, "?descOrgao")
      }
      if (UO != FALSE) { query <- paste(query, "?codUO")
      if (incluiDescricoes) query <- paste(query, "?descUO")
      }
      if (Funcao != FALSE) { query <- paste(query, "?codFuncao")
      if (incluiDescricoes) query <- paste(query, "?descFuncao")
      }
      if (Subfuncao != FALSE) { query <- paste(query, "?codSubfuncao")
      if (incluiDescricoes) query <- paste(query, "?descSubfuncao")
      }
      if (Programa != FALSE) { query <- paste(query, "?codPrograma")
      if (incluiDescricoes) query <- paste(query, "?descPrograma")
      }
      if (Acao != FALSE) { query <- paste(query, "?codAcao")
      if (incluiDescricoes) query <- paste(query, "?descAcao")
      }
      if (Subtitulo != FALSE) { query <- paste(query, "?codSubtitulo")
      if (incluiDescricoes) query <- paste(query, "?descSubtitulo")
      }
      if (PlanoOrcamentario != FALSE) { query <- paste(query, "?codPlanoOrcamentario")
      if (incluiDescricoes) query <- paste(query, "?descPlanoOrcamentario")
      }
      if (Fonte != FALSE) { query <- paste(query, "?codFonte")
      if (incluiDescricoes) query <- paste(query, "?descFonte")
      }
      if (CategoriaEconomica != FALSE) { query <- paste(query, "?codCategoriaEconomica")
      if (incluiDescricoes) query <- paste(query, "?descCategoriaEconomica")
      }
      if (GND != FALSE) { query <- paste(query, "?codGND")
      if (incluiDescricoes) query <- paste(query, "?descGND")
      }
      if (ModalidadeAplicacao != FALSE) { query <- paste(query, "?codModalidadeAplicacao")
      if (incluiDescricoes) query <- paste(query, "?descModalidadeAplicacao")
      }
      if (IdUso != FALSE) { query <- paste(query, "?codIdUso")
      if (incluiDescricoes) query <- paste(query, "?descIdUso")
      }
      if (ResultadoPrimario != FALSE) { query <- paste(query, "?codResultadoPrimario")
      if (incluiDescricoes) query <- paste(query, "?descResultadoPrimario")
      }
      if (ElementoDespesa != FALSE) { query <- paste(query, "?codElementoDespesa")
      if (incluiDescricoes) query <- paste(query, "?descElementoDespesa")
      }
      return(query)
    }

  query <- "SELECT ?codExercicio" #"?data"

  query <- escreve_descritores(
    query = query
    , Esfera = Esfera, Orgao = Orgao, UO = UO
    , Funcao = Funcao, Subfuncao = Subfuncao, Programa = Programa
    , Acao = Acao, Subtitulo = Subtitulo, PlanoOrcamentario = PlanoOrcamentario
    , Fonte = Fonte, CategoriaEconomica = CategoriaEconomica, GND = GND
    , ModalidadeAplicacao = ModalidadeAplicacao, IdUso = IdUso
    , ResultadoPrimario = ResultadoPrimario, ElementoDespesa = ElementoDespesa
    , incluiDescricoes = incluiDescricoes)

  if (valorPLOA)      query <- paste(query, "(sum(?val1) as ?ploa)")
  if (valorLOA)  query <- paste(query, "(sum(?val2) as ?loa)")
  if (valorLOAmaisCredito)  query <- paste(query, "(sum(?val3) as ?loa_mais_credito)")
  if (valorEmpenhado)       query <- paste(query, "(sum(?val4) as ?empenhado)")
  if (valorLiquidado)       query <- paste(query, "(sum(?val5) as ?liquidado)")
  if (valorPago)            query <- paste(query, "(sum(?val6) as ?pago)")

  query <- paste(query, "WHERE { GRAPH <")
  query <- paste0(query, "http://orcamento.dados.gov.br/", exercicio, "/>")
  query <- paste(query
                 , "{
       ?i                    loa:temExercicio            ?exercicio .
       ?exercicio            loa:dataUltimaAtualizacao   ?data .
       ?exercicio            loa:identificador           ?codExercicio ."
  )

  if (Esfera != FALSE) {
    query <- paste(query,
                   "?i                    loa:temEsfera               ?esfera .
       ?esfera               loa:codigo                  ?codEsfera .")
    if (incluiDescricoes) query <- paste(query, "?esfera               rdfs:label                  ?descEsfera .")
  }

  if (Orgao != FALSE) {
    query <- paste(query,
                   "?UO                   loa:temOrgao                ?orgao .
   ?orgao                loa:codigo                  ?codOrgao .")
    if (incluiDescricoes) query <- paste(query, "?orgao                rdfs:label                  ?descOrgao .")
  }

  if (UO != FALSE) {
    query <- paste(query,
                   "?i                    loa:temUnidadeOrcamentaria  ?UO .
   ?UO                   loa:codigo                  ?codUO .")
    if (incluiDescricoes) query <- paste(query, "?UO                   rdfs:label                  ?descUO .")
  }

  if (Funcao != FALSE) {
    query <- paste(query,
                   "?i                    loa:temFuncao               ?funcao .
   ?funcao               loa:codigo                  ?codFuncao .")
    if (incluiDescricoes) query <- paste(query, "?funcao               rdfs:label                  ?descFuncao .")
  }

  if (Subfuncao != FALSE) {
    query <- paste(query,
                   "?i                    loa:temSubfuncao            ?subfuncao .
   ?subfuncao            loa:codigo                  ?codSubfuncao .")
    if (incluiDescricoes) query <- paste(query, "?subfuncao            rdfs:label                  ?descSubfuncao .")
  }

  if (Programa != FALSE) {
    query <- paste(query,
                   "?i                    loa:temPrograma             ?programa .
   ?programa             loa:codigo                  ?codPrograma .")
    if (incluiDescricoes) query <- paste(query, "?programa             rdfs:label                  ?descPrograma .")
  }

  if (Acao != FALSE) {
    query <- paste(query,
                   "?i                    loa:temAcao                 ?acao .
   ?acao                 loa:codigo                  ?codAcao .")
    if (incluiDescricoes) query <- paste(query, "?acao                 rdfs:label                  ?descAcao .")
  }

  if (Subtitulo != FALSE) {
    query <- paste(query,
                   "?i                    loa:temSubtitulo            ?subtitulo .
   ?subtitulo            loa:codigo                  ?codSubtitulo .")
    if (incluiDescricoes) query <- paste(query, "?subtitulo            rdfs:label                  ?descSubtitulo .")
  }

  if (PlanoOrcamentario != FALSE) {
    query <- paste(query,
                   "?i                    loa:temPlanoOrcamentario    ?planoorcamentario .
   ?planoorcamentario    loa:codigo                  ?codPlanoOrcamentario .")
    if (incluiDescricoes) query <- paste(query, "?planoorcamentario    rdfs:label                  ?descPlanoOrcamentario .")
  }

  if (Fonte != FALSE) {
    query <- paste(query,
                   "?i                    loa:temFonteRecursos        ?fonte .
   ?fonte                loa:codigo                  ?codFonte .")
    if (incluiDescricoes) query <- paste(query, "?fonte                rdfs:label                  ?descFonte .")
  }

  if (CategoriaEconomica != FALSE) {
    query <- paste(query,
                   "?i                    loa:temCategoriaEconomica   ?categoriaeconomica .
   ?categoriaeconomica   loa:codigo                  ?codCategoriaEconomica .")
    if (incluiDescricoes) query <- paste(query, "?categoriaeconomica   rdfs:label                  ?descCategoriaEconomica .")
  }

  if (GND != FALSE) {
    query <- paste(query,
                   "?i                    loa:temGND                  ?GND .
   ?GND                  loa:codigo                  ?codGND .")
    if (incluiDescricoes) query <- paste(query, "?GND                  rdfs:label                  ?descGND .")
  }

  if (ModalidadeAplicacao != FALSE) {
    query <- paste(query,
                   "?i                    loa:temModalidadeAplicacao  ?modalidadeaplicacao .
   ?modalidadeaplicacao  loa:codigo                  ?codModalidadeAplicacao .")
    if (incluiDescricoes) query <- paste(query, "?modalidadeaplicacao  rdfs:label                  ?descModalidadeAplicacao .")
  }

  if (IdUso != FALSE) {
    query <- paste(query,
                   "?i                    loa:temIdentificadorUso     ?iduso .
   ?iduso                loa:codigo                  ?codIdUso .")
    if (incluiDescricoes) query <- paste(query, "OPTIONAL  {?iduso     rdfs:label                  ?descIdUso }.")
  }

  if (ResultadoPrimario != FALSE) {
    query <- paste(query,
                   "?i                    loa:temResultadoPrimario    ?resultadoprimario .
   ?resultadoprimario    loa:codigo                  ?codResultadoPrimario .")
    if (incluiDescricoes) query <- paste(query, "OPTIONAL {?resultadoprimario rdfs:label ?descResultadoPrimario }.")
  }

  if (ElementoDespesa != FALSE) {
    query <- paste(query,
                   "?i                    loa:temElementoDespesa      ?elementoDespesa .
   ?elementoDespesa      loa:codigo                  ?codElementoDespesa .")
    if (incluiDescricoes) query <- paste(query, "?elementoDespesa      rdfs:label                  ?descElementoDespesa .")
  }

  # FILTROS

  if (is.character(Esfera))
    query <- paste0(query, ' ?esfera               loa:codigo "', Esfera ,'" . ')
  if (is.character(Orgao))
    query <- paste0(query, ' ?orgao                loa:codigo "', Orgao ,'" . ')
  if (is.character(UO))
    query <- paste0(query, ' ?UO                   loa:codigo "', UO ,'" . ')
  if (is.character(Funcao)) query <-
    paste(query, ' ?funcao               loa:codigo  "', Funcao ,'" . ')
  if (is.character(Subfuncao))
    query <- paste0(query, ' ?subfuncao            loa:codigo  "', Subfuncao ,'" . ')
  if (is.character(Programa))
    query <- paste0(query, ' ?programa             loa:codigo  "', Programa ,'" . ')
  if (is.character(Acao))
    query <- paste0(query, ' ?acao                 loa:codigo  "', Acao ,'" . ')
  if (is.character(Subtitulo))
    query <- paste0(query, ' ?subtitulo            loa:codigo  "', Subtitulo ,'" . ')
  if (is.character(PlanoOrcamentario))
    query <- paste0(query, ' ?planoorcamentario    loa:codigo  "', PlanoOrcamentario ,'" . ')
  if (is.character(Fonte))
    query <- paste0(query, ' ?fonte                loa:codigo  "', Fonte ,'" . ')
  if (is.character(CategoriaEconomica))
    query <- paste0(query, ' ?categoriaeconomica   loa:codigo  "', CategoriaEconomica ,'" . ')
  if (is.character(GND))
    query <- paste0(query, ' ?GND                  loa:codigo  "', GND ,'" . ')
  if (is.character(ModalidadeAplicacao))
    query <- paste0(query, ' ?modalidadeaplicacao  loa:codigo  "', ModalidadeAplicacao ,'" . ')
  if (is.character(IdUso))
    query <- paste0(query, ' ?iduso                loa:codigo  "', IdUso ,'" .')
  if (is.character(ResultadoPrimario))
    query <- paste0(query, ' ?resultadoprimario    loa:codigo  "', ResultadoPrimario ,'" . ')
  if (is.character(ElementoDespesa))
    query <- paste0(query, ' ?elementoDespesa      loa:codigo  "', ElementoDespesa ,'" . ')

  ######

  if (valorPLOA)      query <- paste(query, "?i loa:valorProjetoLei     ?val1 .")
  if (valorLOA)  query <- paste(query, "?i loa:valorDotacaoInicial ?val2 .")
  if (valorLOAmaisCredito)  query <- paste(query, "?i loa:valorLeiMaisCredito ?val3 .")
  if (valorEmpenhado)       query <- paste(query, "?i loa:valorEmpenhado      ?val4 .")
  if (valorLiquidado)       query <- paste(query, "?i loa:valorLiquidado      ?val5 .")
  if (valorPago)            query <- paste(query, "?i loa:valorPago           ?val6 .")

  query <- paste(query, "} } GROUP BY ?codExercicio")

  query <- escreve_descritores(
    query = query
    , Esfera = Esfera, Orgao = Orgao, UO = UO
    , Funcao = Funcao, Subfuncao = Subfuncao, Programa = Programa
    , Acao = Acao, Subtitulo = Subtitulo, PlanoOrcamentario = PlanoOrcamentario
    , Fonte = Fonte, CategoriaEconomica = CategoriaEconomica, GND = GND
    , ModalidadeAplicacao = ModalidadeAplicacao, IdUso = IdUso
    , ResultadoPrimario = ResultadoPrimario, ElementoDespesa = ElementoDespesa
    , incluiDescricoes = incluiDescricoes)

  #query <- paste(query, "?data")

  return(query)
}


#' Build the SPARQL to query the SIOP according to the annual breakdown.
#'
#' This function builds the SPARQL query to query the SIOP according to the annual breakdown.
#'
#' @inheritParams quaisMembros
#'
#' @returns A string. A SPARQL query.
#'
#' @noRd
.constroiSIOPqueryDimensoes <- function(exercicio, dimensao) {
  .dimensoes <- .carregar_dataframe_dimensoes()
  query <-
    paste0(
      "SELECT ?cod ?desc WHERE { GRAPH <http://orcamento.dados.gov.br/",exercicio,"/> { "
      , "?i loa:", .dimensoes[which(.dimensoes$param == dimensao),]$var," ?dimensao . "
      , "?dimensao loa:codigo ?cod . "
      , "?dimensao rdfs:label ?desc . } } GROUP BY ?cod ?desc ORDER BY ?cod"
      )

  return(query)
}

#' Build the URL to query the SIOP endpoint.
#'
#' This function builds the URL to query the SIOP endpoint.
#'
#' @param query A string. The SPARQL query.
#'
#' @returns A string. The URL.
#'
#' @noRd
.formaURLSIOP <- function(query) {
  query <- gsub("\n", " ", query) # Remove newline characters
  query <- gsub("\\s+", " ", query)
  query <- utils::URLencode(query, TRUE, TRUE)
  url <-
    paste0(
      "https://www1.siop.planejamento.gov.br/sparql/?default-graph-uri=&query="
      , query
      , "&format=application%2Fsparql-results%2B"
      ,"json"
      , "&timeout=0&debug=on")
  return(url)
}

#' Query the SIOP API
#'
#' This function queries the SIOP API
#'
#' @param url_completo A string. The URL to query the SIOP API.
#'
#' @returns A string containing the SIOP response in JSON format.
#'
#' @noRd
.downloadSIOP <- function(url_completo, ignoreSecureCertificate) {

  if (!ignoreSecureCertificate) {
    response <- tryCatch({
      cert_path <- system.file("certs", "fullchainsiop.pem", package = "orcamentoBR")
      httr::set_config(httr::config(cainfo = cert_path))
      httr::GET(url_completo)
    }, error = function(e) {
      stop("Error using the certificate.\nErro ao usar o certificado: ", e$message)
    })

  } else { #ignoreSecureCertificate
    response <- tryCatch( {
        httr::set_config(httr::config(ssl_verifypeer = FALSE))
        httr::GET(url_completo)
        }, error = function(e) {
          stop("Error acessing the endpoint.\nErro de acesso ao endpoint.")
        }
     )
  }

  if (response$status_code != 200)
    stop(paste("Error in downloading from endpoint. Error code:"
               , response$status_code))

  text.content <- httr::content(response, "text", encoding = "UTF-8")
  text.content <- gsub("^\\s*\\n*\\s*\\{", "{", text.content)

  # Verificar se a resposta esta em formato JSON
  if (grepl("^\\{", text.content)) {
    json_data <- jsonlite::fromJSON(text.content)
    return(json_data)
  } else {
    stop("The response is not in JSON format.")
  }
}

#' Convert JSON to a dataframe
#'
#' This function converts a JSON-formatted string into a dataframe.
#'
#' @param json_data A string containing the SIOP response in JSON format.
#'
#' @returns A dataframe.
#'
#' @noRd
.json2df <- function(json_data) {
  df <- json_data$results$bindings
  df <- data.frame(lapply(df, function(col) col$value))
  return(df)
}

#' Return last year
#'
#' This function returns last year.
#'
#' @returns An integer.
#'
#' @noRd
.last_year <- function() {
  return(as.integer(format(Sys.Date(), "%Y")) - 1)
}

#' Get dimensoes
#'
#' This function builds the service dataframe dimensoes.
#'
#' @returns A dataframe.
#'
#' @noRd
.carregar_dataframe_dimensoes <- function() {
  dimensoes <-
    data.frame(
      param = c("exercicio", "Esfera", "Orgao", "UO", "Funcao", "Subfuncao", "Programa", "Acao",
                "PlanoOrcamentario", "Subtitulo", "CategoriaEconomica", "GND", "ModalidadeAplicacao",
                "ElementoDespesa", "Fonte", "IdUso", "ResultadoPrimario")
      , var = c("temExercicio", "temEsfera", "temOrgao", "temUnidadeOrcamentaria", "temFuncao",
                "temSubfuncao", "temPrograma", "temAcao", "temPlanoOrcamentario", "temSubtitulo",
                "temCategoriaEconomica", "temGND", "temModalidadeAplicacao", "temElementoDespesa",
                "temFonteRecursos", "temIdentificadorUso", "temResultadoPrimario")
      )

  return(dimensoes)
}
