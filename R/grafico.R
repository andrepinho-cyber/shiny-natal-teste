# R/grafico.R
# Introdução a Aplicativos Shiny · Prof. André Pinho · UFRN
# Aula 15: módulo "gráfico" (as temperaturas diárias no plotly); PASSO 3.
# Recebe os dias e as linhas selecionadas na tabela (dois reactives).
# A escolha das temperaturas só serve ao gráfico: fica dentro do módulo.

## ---- grafico_ui ----
graficoUI <- function(id) {
  ns <- NS(id)
  card(
    full_screen = TRUE,
    card_header("Temperaturas diárias"),
    checkboxGroupInput(
      inputId = ns("variaveis"),
      label = NULL,
      choices = temperaturas,
      selected = "temp_media",
      inline = TRUE
    ),
    plotlyOutput(ns("grafico"))
  ) # fim do card
}

## ---- grafico_server ----
graficoServer <- function(id, dias, marcadas) {
  moduleServer(id, function(input, output, session) {
    output$grafico <- renderPlotly({
      req(input$variaveis)
      longo <- empilhar(dias(), input$variaveis)
      longo[["dica"]] <- paste0(
        format(longo[["dia"]], "%d/%m/%Y"), "<br>",
        longo[["temperatura"]], ": ", virgula(longo[["valor"]]), " °C"
      )
      grafico <- ggplot(longo, aes(
        x = dia, y = valor, color = temperatura,
        group = temperatura, text = dica
      )) +
        geom_line(na.rm = TRUE) +
        scale_y_continuous(labels = rotulo_virgula) +
        scale_x_date(date_labels = "%d/%m/%y") +
        labs(x = NULL, y = "Temperatura (°C)", color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom")
## ---- grafico_marcadas ----
      # PASSO 4: a seleção chega pelo argumento marcadas.
      if (length(marcadas()) > 0) {
        dias_marcados <- dias()[["data"]][marcadas()]
        pontos <- longo[longo[["dia"]] %in% dias_marcados, ]
        grafico <- grafico +
          geom_point(data = pontos, aes(color = "Seleção na tabela"),
                     size = 3, na.rm = TRUE)
      }
      ggplotly(grafico, tooltip = "text")
    })
  }) # fim do moduleServer
}
