#' Launch the citation-arcs app
#'
#' @param source which backend to preselect. "scholar" only works when running
#'   locally -- see fetch_scholar.R for why that is a hard limit rather than a
#'   default.
#' @param mailto contact address sent to OpenAlex for the polite pool.
#' @export
run_app <- function(source = c("openalex", "scholar"), mailto = NULL, ...) {
  source <- match.arg(source)
  if (!is.null(mailto)) options(scholararcs.mailto = mailto)
  if (identical(source, "scholar")) {
    r <- gs_block_reason()
    if (!is.null(r)) stop(r, call. = FALSE)
  }
  options(scholararcs.default_backend = source)
  shiny::shinyApp(ui = arc_ui, server = arc_server, ...)
}
