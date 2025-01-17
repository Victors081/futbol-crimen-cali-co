read_excel_allsheets <- function(filename, tibble = TRUE) {
  # I prefer straight tibbles
  # but if you like data.frames
  # then just pass tibble = FALSE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
