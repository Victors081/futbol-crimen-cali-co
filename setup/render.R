
source(file = "setup/functions.R")

## Render Quarto Report

quarto_render_move(
  input = "reports/01-calidad_de_datos.qmd",
  output_file = "reports/01-calidad_de_datos.html",
  output_dir = "reports/html"
)

quarto_render_move(
  input = "reports/02-analisis_exploratorio_de_datos.qmd",
  output_file = "reports/02-analisis_exploratorio_de_datos.html",
  output_dir = "reports/html"
)

quarto_render_move(
  input = "reports/03-analisis_econometrico.qmd",
  output_file = "reports/03-analisis_econometrico.html",
  output_dir = "reports/html"
)