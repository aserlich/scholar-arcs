## Zero-install entry point: shiny::runApp("path/to/scholar-arcs")
## Sources R/ directly, so no package installation is required.

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT)
  library(dplyr); library(tidyr); library(purrr); library(tibble); library(rlang)
  library(ggplot2); library(ggrepel); library(scales)
  library(httr2); library(jsonlite)
})

# Identify yourself to OpenAlex (the "polite pool" is faster and more stable).
if (is.null(getOption("scholararcs.mailto")))
  options(scholararcs.mailto = Sys.getenv("SCHOLARARCS_MAILTO",
                                          unset = "scholararcs@example.org"))

for (f in list.files(file.path(dirname(sys.frame(1)$ofile %||% "."), "R"),
                     pattern = "\\.R$", full.names = TRUE)) source(f)

shinyApp(ui = arc_ui, server = arc_server)
