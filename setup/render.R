
source(file = "setup/functions.R")

## Render Quarto Report

in_files <- list.files(
  path = "reports/",
  pattern = ".qmd",
  full.names = TRUE
)

out_files <- gsub(
  pattern = ".qmd",
  replacement = ".html",
  x = in_files
)


quarto_render_move(
  input = in_files,
  output_file = out_files,
  output_dir = "reports/html"
)