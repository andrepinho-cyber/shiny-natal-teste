# R/filtro.R
# Introdução a Aplicativos Shiny · Prof. André Pinho · UFRN
# Aula 15: módulo "filtro" (o período e o resumo, na barra lateral); PASSO 1.
# Recebe os dados (um data frame comum) e DEVOLVE os dias do período
# (um reactive), que os outros módulos recebem.

## ---- filtro_ui ----
# A parte da ui: duas peças soltas, juntas num tagList().
filtroUI <- function(id) {
  ns <- NS(id)
  tagList(
    dateRangeInput(
      inputId = ns("periodo"),
      label = "Período",
      start = "2025-01-01",
      end = "2025-03-31",
      format = "dd/mm/yyyy",
      language = "pt-BR",
      separator = " a "
    ),
    textOutput(ns("resumo"))
  ) # fim do tagList
}

## ---- filtro_server ----
# A parte do server: recebe os dados e devolve o reactive dias.
filtroServer <- function(id, dados) {
  moduleServer(id, function(input, output, session) {
    dias <- reactive({
      req(input$periodo[1], input$periodo[2])
      dentro <- dados[["data"]] >= input$periodo[1] &
        dados[["data"]] <= input$periodo[2]
      selecionados <- dados[dentro, ]
## ---- filtro_validate ----
      validate(need(
        nrow(selecionados) > 0,
        paste0("Não há dias no período escolhido. Use datas de ",
               format(min(dados[["data"]]), "%d/%m/%Y"), " a ",
               format(max(dados[["data"]]), "%d/%m/%Y"),
               ", com a inicial antes da final.")
      ))
      validate(need(
        any(!is.na(selecionados[["temp_media"]])),
        "O período não tem registro de temperatura."
      ))
      selecionados
    })
## ---- filtro_resumo ----
    output$resumo <- renderText({
      sem_registro <- sum(is.na(dias()[["temp_media"]]))
      paste0(nrow(dias()), " dias no período; ",
             sem_registro, " sem registro de temperatura.")
    })
    # O reactive sai do módulo SEM parênteses.
    return(dias)
  }) # fim do moduleServer
}
