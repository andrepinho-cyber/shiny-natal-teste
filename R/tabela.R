# R/tabela.R
# Introdução a Aplicativos Shiny · Prof. André Pinho · UFRN
# Aula 15: módulo "tabela" (os dias do período no DT); PASSO 2.
# Recebe os dias (um reactive) e DEVOLVE as linhas selecionadas (outro
# reactive), que o módulo do gráfico recebe.

## ---- tabela_ui ----
tabelaUI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Os dias do período (°C e mm)"),
    DTOutput(ns("tabela"))
  ) # fim do card
}

## ---- tabela_server ----
# dias chega sem parênteses; aqui dentro, dias() é o valor de agora.
tabelaServer <- function(id, dias) {
  moduleServer(id, function(input, output, session) {
    output$tabela <- renderDT({
      tabela <- data.frame(
        Data = dias()[["data"]],
        Média = dias()[["temp_media"]],
        Máxima = dias()[["temp_max"]],
        Mínima = dias()[["temp_min"]],
        Chuva = dias()[["precipitacao"]]
      )
      datatable(
        tabela,
        rownames = FALSE,
        class = "compact nowrap",
        options = list(pageLength = 10)
      ) |>
        formatRound(columns = 2:5, digits = 1,
                    dec.mark = ",", mark = ".")
    })
## ---- tabela_retorno ----
    # PASSO 4: a seleção é desta tabela e sai num reactive.
    return(reactive(input$tabela_rows_selected))
  }) # fim do moduleServer
}
