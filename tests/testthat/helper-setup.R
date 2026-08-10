## Source the package's R/ directly so tests run without installing.
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(tibble)
  library(rlang); library(ggplot2)
})
`%||%` <- rlang::`%||%`

.pkg_root <- local({
  d <- normalizePath(".", mustWork = FALSE)
  for (i in 1:5) {
    if (file.exists(file.path(d, "DESCRIPTION"))) return(d)
    d <- dirname(d)
  }
  normalizePath("../..", mustWork = FALSE)
})
for (f in list.files(file.path(.pkg_root, "R"), pattern = "\\.R$", full.names = TRUE)) {
  if (!grepl("app_(ui|server)\\.R$|run_app\\.R$", f)) sys.source(f, envir = globalenv())
}

# A small synthetic record exercising every property the series builder must have:
# a preprint cited before publication, a paper whose only citations land in the
# partial current year, a zero-citation paper, and two sources that disagree.
fixture <- function(this_year = as.integer(format(Sys.Date(), "%Y"))) {
  papers <- tibble::tibble(
    paper_id = c("p_old", "p_pre", "p_new", "p_zero"),
    title    = c("Old paper", "Preprinted paper", "Brand new", "Uncited"),
    venue    = c("Journal A", "Journal B", "Journal A", "Journal C"),
    venue_id = c("S1", "S2", "S1", "S3"),
    pub_year = c(this_year - 6L, this_year - 3L, this_year - 1L, this_year - 2L),
    total    = c(30L, 12L, 2L, 0L))
  counts <- dplyr::bind_rows(
    tibble::tibble(paper_id = "p_old", year = (this_year - 6L):(this_year - 1L),
                   cites = c(1, 4, 8, 9, 5, 3), source = "scholar"),
    # cited two years before it was published
    tibble::tibble(paper_id = "p_pre", year = (this_year - 5L):(this_year - 1L),
                   cites = c(1, 2, 3, 4, 2), source = "scholar"),
    # only citation lands in the partial current year
    tibble::tibble(paper_id = "p_new", year = this_year, cites = 2, source = "scholar"),
    tibble::tibble(paper_id = "p_zero", year = this_year - 2L, cites = 0, source = "scholar"),
    # openalex disagrees on level and reaches one year further back on p_old
    tibble::tibble(paper_id = "p_old", year = (this_year - 7L):(this_year - 1L),
                   cites = c(2, 1, 3, 5, 6, 3, 2), source = "openalex"),
    tibble::tibble(paper_id = "p_pre", year = (this_year - 4L):(this_year - 1L),
                   cites = c(1, 2, 2, 1), source = "openalex"))
  list(papers = papers, counts = counts, this_year = this_year)
}
