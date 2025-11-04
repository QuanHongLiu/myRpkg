wb <- openxlsx::createWorkbook()

style <- openxlsx::createStyle(fontName = font, fontSize = size)

if (!is.list(x) | is.data.frame(x)) {
  x <- as.data.frame(x)
  x <- list(Sheet1 = x)
} else if (inherits(x, c("descrTable", "createTable"))) {
  prefix_file <- sub("\\.xlsx$", "", file)
  x$descr <- apply(x$descr, c(1, 2), function(para) gsub("±", " ± ", para))
  compareGroups::export2csv(x, file = paste0(prefix_file,'.csv'))
  x <- read_csv(file = paste0(prefix_file,'.csv'))
  file.remove(paste0(prefix_file,'.csv'))
  x <- list(Sheet1 = x)
}


for (nm in names(x)) {
  openxlsx::addWorksheet(wb, sheetName = nm)
  data <- x[[nm]]
  openxlsx::writeData(wb, sheet = nm, x = data, ...)
  openxlsx::addStyle(wb, nm, style = style, rows = 1:(nrow(data)+1), cols = 1:ncol(data), gridExpand = TRUE)
  openxlsx::setRowHeights(wb, nm, rows = 1:(nrow(data)+1), heights = row_height)
  if (auto_width) {
    openxlsx::setColWidths(wb, nm, cols = 1:ncol(data), widths = "auto")
  }
}
openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
