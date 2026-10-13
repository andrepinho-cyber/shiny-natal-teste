# R/utilidades.R
# Introdução a Aplicativos Shiny · Prof. André Pinho · UFRN
# Aula 15: as funções e constantes que os módulos usam.
# Arquivo da pasta R/ do app: o shiny o lê sozinho, antes do app.R.

## ---- utilidades ----
# Rótulo na tela = nome da coluna nos dados.
temperaturas <- c(
  "Média" = "temp_media",
  "Máxima" = "temp_max",
  "Mínima" = "temp_min"
)
# Dia com chuva: pelo menos 1 mm (escolha do app).
limiar_chuva <- 1
# Número com vírgula decimal, para textos e tabelas.
virgula <- function(x, casas = 1) {
  formatC(x, format = "f", digits = casas,
          decimal.mark = ",", big.mark = ".")
}
# Vírgula decimal no eixo dos gráficos.
rotulo_virgula <- scales::label_number(decimal.mark = ",")
# Formato longo para o ggplot2: uma linha por dia e temperatura.
empilhar <- function(dados, colunas) {
  partes <- lapply(colunas, function(v) data.frame(
    dia = dados[["data"]],
    temperatura = names(temperaturas)[temperaturas == v],
    valor = dados[[v]]
  ))
  do.call(rbind, partes)
}
