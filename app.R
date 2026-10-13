# app.R
# Introdução a Aplicativos Shiny · Prof. André Pinho · UFRN
# Aula 15: Módulos II e app em vários arquivos
#
# App "Temperaturas e chuva em Natal": o app da Aula 13, reorganizado em
# três módulos que trocam dados (filtro, tabela e gráfico) e em vários
# arquivos. Esta pasta é o app:
#   app.R                     configuração, dados, ui e server do app
#   R/utilidades.R            funções e constantes dos módulos
#   R/filtro.R                módulo do período (devolve os dias)
#   R/tabela.R                módulo da tabela (devolve a seleção)
#   R/grafico.R               módulo do gráfico
#   inmet_natal_diario.csv    os dados
# O shiny lê sozinho os arquivos da pasta R/, antes deste app.R.
# Solução do roteiro: PASSOS 1 a 5 feitos.
# Como usar: abra este app.R no RStudio e clique em "Run App".
# Porta fixa da aula: 4015.

## ---- app_configuracao ----
options(shiny.port = 4015)        # porta fixa da Aula 15
options(shiny.autoreload = TRUE)  # salvar o arquivo recarrega o app
library(shiny)
library(bslib)
library(ggplot2)
library(DT)       # tabelas interativas
library(plotly)   # gráficos interativos

## ---- app_dados ----
# Os dados são do app: lidos aqui e entregues ao módulo do filtro.
natal <- read.csv("inmet_natal_diario.csv", fileEncoding = "UTF-8")
natal[["data"]] <- as.Date(natal[["data"]])

## ---- app_caixas ----
# Três caixas de valor, lado a lado (Aula 12).
caixas <- layout_columns(
  fill = FALSE,
  value_box(
    title = "Temperatura média",
    value = textOutput("media"),
    showcase = icon("temperature-half"),
    theme = "primary"
  ),
  value_box(
    title = "Maior máxima",
    value = textOutput("maxima"),
    textOutput("dia_maxima"),
    showcase = icon("temperature-arrow-up"),
    theme = "primary"
  ),
  value_box(
    title = "Dias com chuva (1 mm ou mais)",
    value = textOutput("chuva"),
    textOutput("dias_registro"),
    showcase = icon("cloud-rain"),
    theme = "primary"
  )
) # fim do layout_columns das caixas

## ---- app_sobre ----
# A página "Sobre os dados", num cartão (Aula 12).
sobre <- card(
  card_header("Sobre os dados"),
  p(strong("Fonte:"), "INMET, dados históricos anuais,",
    "estação automática de Natal (A304)."),
  p(paste0("Período: ", format(min(natal[["data"]]), "%d/%m/%Y"),
           " a ", format(max(natal[["data"]]), "%d/%m/%Y"), ".")),
  p("Dia com menos de 18 horas válidas fica sem valor."),
  p("Dia com chuva: pelo menos 1 mm de precipitação no dia.")
) # fim do card

## ---- app_ui ----
# Cada módulo entra na ui pela função nomeUI(), com o seu id.
ui <- page_navbar(
  title = "Temperaturas e chuva em Natal",
  id = "pagina",
  theme = bs_theme(preset = "cerulean"),
  sidebar = sidebar(width = 300, filtroUI("filtro")),
  nav_panel(
    "Painel",
    caixas,
    layout_columns(
      col_widths = c(6, 6),
      graficoUI("grafico"),
      tabelaUI("tabela")
    )
  ),
  nav_panel("Sobre os dados", sobre)
)

## ---- app_server ----
server <- function(input, output, session) {
  # Os reactives passam de um módulo a outro SEM parênteses.
  dias <- filtroServer("filtro", dados = natal)
  marcadas <- tabelaServer("tabela", dias = dias)
  graficoServer("grafico", dias = dias, marcadas = marcadas)

## ---- app_server_caixas ----
  # As caixas usam o reactive devolvido pelo filtro, com parênteses.
  output$media <- renderText({
    paste(virgula(mean(dias()[["temp_media"]], na.rm = TRUE)), "°C")
  })
  output$maxima <- renderText({
    paste(virgula(max(dias()[["temp_max"]], na.rm = TRUE)), "°C")
  })
  output$dia_maxima <- renderText({
    quando <- which.max(dias()[["temp_max"]])
    paste("em", format(dias()[["data"]][quando], "%d/%m/%Y"))
  })
  output$chuva <- renderText({
    chuva <- dias()[["precipitacao"]]
    paste(sum(chuva >= limiar_chuva, na.rm = TRUE), "dias")
  })
  output$dias_registro <- renderText({
    paste("de", sum(!is.na(dias()[["precipitacao"]])),
          "dias com registro de chuva")
  })
} # fim do server

## ---- app ----
shinyApp(ui, server)
