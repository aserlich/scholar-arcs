## Google Scholar backend. LOCAL ONLY -- and that is a hard constraint, not a
## default that can be flipped.
##
## Scholar has no API. The `scholar` package scrapes HTML, Google's terms
## disallow automated access, and Google blocks datacenter IPs: a hosted app
## serves every user from one address and gets CAPTCHA'd within minutes. A
## browser-side app cannot do it either, because Scholar sends no CORS headers so
## the browser refuses to read the response. Neither is a throttling problem.
##
## Also verified: get_scholar_id() no longer resolves anything at all -- every
## name lookup returns "No Scholar ID found", even from a residential IP. Users
## must paste the `user=` value from their own profile URL.

GS_THROTTLE_SEC <- 8
GS_MAX_RETRY    <- 3L

#' Is the Scholar backend usable here?
#'
#' False when the package is missing or when running somewhere hosted. Shiny
#' Server / Connect / shinyapps.io all set SHINY_SERVER_VERSION.
#' @export
gs_available <- function() {
  if (!requireNamespace("scholar", quietly = TRUE)) return(FALSE)
  if (isTRUE(getOption("scholararcs.force_local", FALSE))) return(TRUE)
  !nzchar(Sys.getenv("SHINY_SERVER_VERSION"))
}

gs_block_reason <- function() {
  if (!requireNamespace("scholar", quietly = TRUE))
    return("The 'scholar' package is not installed. install.packages(\"scholar\")")
  if (nzchar(Sys.getenv("SHINY_SERVER_VERSION")))
    return(paste("Google Scholar cannot be used from a hosted app: it has no API,",
                 "and Google blocks server IP addresses. Run this app locally to use it."))
  NULL
}

.gs_cache_dir <- function() {
  d <- getOption("scholararcs.cache", file.path(tools::R_user_dir("scholararcs", "cache")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Publication list for a Scholar profile
#'
#' @param gs_id the `user=` value from a Scholar profile URL
#' @export
gs_publications <- function(gs_id, cache = TRUE) {
  stopifnot(gs_available())
  f <- file.path(.gs_cache_dir(), paste0("pubs-", gs_id, ".rds"))
  if (cache && file.exists(f)) return(readRDS(f))
  p <- tryCatch(scholar::get_publications(gs_id), error = function(e) NULL)
  if (is.null(p) || !nrow(p))
    stop("Scholar returned nothing for '", gs_id, "'. Either the id is wrong or ",
         "Google is blocking this address; try again in a few minutes.", call. = FALSE)
  p <- tibble::as_tibble(p) |>
    dplyr::transmute(
      paper_id = as.character(.data$pubid),
      title    = as.character(.data$title),
      venue    = dplyr::na_if(as.character(.data$journal), ""),
      pub_year = suppressWarnings(as.integer(.data$year)),
      total    = as.integer(.data$cites))
  if (cache) saveRDS(p, f)
  p
}

#' Per-year citation history for each publication
#'
#' One scraped request per paper at an 8-second throttle, so roughly seven
#' minutes for fifty papers. Cached to disk per (profile, paper) because a
#' blocked run part-way through is common and should not cost the whole fetch.
#'
#' @param progress optional function(i, n, title)
#' @export
gs_year_counts <- function(gs_id, papers, cache = TRUE, progress = NULL,
                           throttle = GS_THROTTLE_SEC) {
  stopifnot(gs_available())
  f <- file.path(.gs_cache_dir(), paste0("hist-", gs_id, ".rds"))
  have <- if (cache && file.exists(f)) readRDS(f) else list()

  blocked <- character()
  for (i in seq_len(nrow(papers))) {
    pid <- papers$paper_id[i]
    if (!is.null(have[[pid]])) next
    if (is.function(progress)) progress(i, nrow(papers), papers$title[i])
    h <- NULL
    for (k in seq_len(GS_MAX_RETRY)) {
      h <- tryCatch(scholar::get_article_cite_history(gs_id, pid), error = function(e) NULL)
      if (!is.null(h)) break
      Sys.sleep(throttle * k)     # backoff; a NULL here is almost always a block
    }
    if (is.null(h)) { blocked <- c(blocked, pid); next }
    have[[pid]] <- if (nrow(h))
      tibble::tibble(year = as.integer(h$year), cites = as.numeric(h$cites))
    else
      # Zero-citation paper: a real observation anchored at publication, not a miss.
      tibble::tibble(year = papers$pub_year[i], cites = 0)
    if (cache) saveRDS(have, f)
    if (i < nrow(papers)) Sys.sleep(throttle)
  }

  out <- purrr::map_dfr(names(have), ~ dplyr::mutate(have[[.x]], paper_id = .x, source = "scholar"))
  if (length(blocked))
    attr(out, "blocked") <- blocked
  out
}

#' Fetch a whole Scholar profile into the shape series.R wants
#' @export
gs_fetch <- function(gs_id, cache = TRUE, progress = NULL) {
  papers <- gs_publications(gs_id, cache = cache)
  counts <- gs_year_counts(gs_id, papers, cache = cache, progress = progress)
  list(counts = dplyr::select(counts, "paper_id", "year", "cites", "source"),
       papers = papers,
       blocked = attr(counts, "blocked"))
}
