## OpenAlex backend. Works from anywhere -- a real API, CORS-enabled, no scraping.
## This is the default because the Scholar backend cannot run on a server at all.

OA_BASE <- "https://api.openalex.org"

# OpenAlex asks for a contact address; in return you get the "polite pool", which
# has materially better rate limits and stability. Overridable so users of the
# package identify as themselves.
oa_mailto <- function() {
  getOption("scholararcs.mailto", Sys.getenv("SCHOLARARCS_MAILTO",
                                             unset = "scholararcs@example.org"))
}

# `counts_by_year` on a work object does not go back further than this. Anything
# published before it needs the (slower) per-work deep fetch or its early years
# are silently missing -- verified: a 2005 paper loses 63 of 212 citations.
OA_COUNTS_FLOOR <- 2012L

# ---- transport ---------------------------------------------------------------
# Back off on OpenAlex's own rate-limit headers rather than guessing an interval.
.oa_get <- function(path, query = list(), tries = 3L) {
  req <- httr2::request(OA_BASE) |>
    httr2::req_url_path_append(path) |>
    httr2::req_url_query(!!!c(query, list(mailto = oa_mailto()))) |>
    httr2::req_user_agent(sprintf("scholararcs (%s)", oa_mailto())) |>
    httr2::req_retry(
      max_tries = tries,
      is_transient = function(resp) httr2::resp_status(resp) %in% c(429, 500, 503),
      after = function(resp) {
        ra <- httr2::resp_header(resp, "Retry-After")
        if (!is.null(ra)) suppressWarnings(as.numeric(ra)) else 2
      }
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)
  if (httr2::resp_status(resp) >= 400) return(NULL)
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

.oa_id <- function(x) if (is.null(x)) NA_character_ else sub("^https://openalex\\.org/", "", x)

# ---- author resolution -------------------------------------------------------

#' Resolve an ORCID to an OpenAlex author record
#'
#' Uses the filter form rather than `authors/https://orcid.org/{id}`. That variant
#' embeds a whole URL inside the request path, and anything that normalises the
#' "//" mangles it — the filter form has no such trap.
#'
#' @return a one-row author tibble, or a length-1 character giving the reason:
#'   "malformed", "notfound", or "unreachable". Callers must distinguish them:
#'   reporting a network failure as "no such ORCID" sends people off checking an
#'   identifier that was never the problem.
#' @export
oa_resolve_orcid <- function(orcid) {
  orcid <- sub("^https?://orcid\\.org/", "", trimws(orcid))
  if (!grepl("^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$", orcid))
    return("malformed")
  r <- .oa_get("authors", list(filter = paste0("orcid:", orcid), per_page = 1))
  if (is.null(r$results)) return("unreachable")
  if (!length(r$results)) return("notfound")
  .oa_author_row(r$results[[1]])
}

#' Search OpenAlex authors by name
#'
#' Returns candidates for a human to choose from -- deliberately not a single
#' best match. OpenAlex merges distinct researchers into one record often enough
#' that auto-picking the top hit is wrong: searching one political scientist's
#' name returns a profile with 72 works at a children's hospital.
#' @export
oa_search_authors <- function(name, n = 10L) {
  r <- .oa_get("authors", list(search = name, per_page = n))
  if (is.null(r$results) || !length(r$results)) return(.oa_author_row(NULL))
  do.call(rbind, lapply(r$results, .oa_author_row))
}

#' Fetch a single OpenAlex author by id
#' @export
oa_author <- function(author_id) {
  a <- .oa_get(paste0("authors/", .oa_id(author_id)))
  if (is.null(a$id)) return(NULL)
  .oa_author_row(a)
}

.oa_author_row <- function(a) {
  if (is.null(a)) return(tibble::tibble(
    id = character(), name = character(), orcid = character(),
    institution = character(), works_count = integer(), cited_by_count = integer()))
  inst <- a$last_known_institutions
  tibble::tibble(
    id             = .oa_id(a$id),
    name           = a$display_name %||% NA_character_,
    orcid          = sub("^https?://orcid\\.org/", "", a$orcid %||% NA_character_),
    institution    = if (length(inst)) (inst[[1]]$display_name %||% NA_character_) else NA_character_,
    works_count    = as.integer(a$works_count %||% NA),
    cited_by_count = as.integer(a$cited_by_count %||% NA)
  )
}

# ---- works -------------------------------------------------------------------

#' All journal articles for an OpenAlex author, with per-year citation counts
#'
#' One paged request returns everything including `counts_by_year`, so a whole
#' author is a single round trip in the common case.
#'
#' @param types OpenAlex work types to keep. Defaults to journal articles only --
#'   including preprints double-counts, since OpenAlex often holds both versions.
#' @export
oa_works <- function(author_id, types = "article", max_pages = 20L) {
  author_id <- .oa_id(author_id)
  out <- list(); cursor <- "*"; page <- 0L
  repeat {
    page <- page + 1L
    r <- .oa_get("works", list(
      filter = sprintf("author.id:%s,type:%s", author_id, paste(types, collapse = "|")),
      select = paste("id", "display_name", "publication_year", "cited_by_count",
                     "counts_by_year", "primary_location", "type", sep = ","),
      per_page = 200, cursor = cursor))
    if (is.null(r$results) || !length(r$results)) break
    out <- c(out, r$results)
    cursor <- r$meta$next_cursor %||% NULL
    if (is.null(cursor) || page >= max_pages) break
  }
  if (!length(out)) return(.oa_empty_works())

  purrr::map_dfr(out, function(w) {
    src <- w$primary_location$source
    cby <- w$counts_by_year %||% list()
    tibble::tibble(
      work_id   = .oa_id(w$id),
      title     = w$display_name %||% NA_character_,
      pub_year  = as.integer(w$publication_year %||% NA),
      total     = as.integer(w$cited_by_count %||% 0L),
      venue     = src$display_name %||% NA_character_,
      venue_id  = .oa_id(src$id),
      counts    = list(if (length(cby))
        tibble::tibble(year  = vapply(cby, function(x) as.integer(x$year), integer(1)),
                       cites = vapply(cby, function(x) as.numeric(x$cited_by_count), numeric(1)))
        else tibble::tibble(year = integer(), cites = numeric()))
    )
  })
}

.oa_empty_works <- function() tibble::tibble(
  work_id = character(), title = character(), pub_year = integer(),
  total = integer(), venue = character(), venue_id = character(), counts = list())

#' Full-history per-year citation counts for one work
#'
#' Grouping the citing works by publication year has no 2012 floor, unlike the
#' `counts_by_year` field. Two calls per work, so this is used only for papers
#' old enough to need it.
#' @export
oa_deep_year_counts <- function(work_id) {
  r <- .oa_get("works", list(filter = sprintf("cites:%s", .oa_id(work_id)),
                             group_by = "publication_year"))
  g <- r$group_by %||% list()
  if (!length(g)) return(tibble::tibble(year = integer(), cites = numeric()))
  tibble::tibble(
    year  = suppressWarnings(as.integer(vapply(g, function(x) x$key, character(1)))),
    cites = vapply(g, function(x) as.numeric(x$count), numeric(1))
  ) |>
    dplyr::filter(!is.na(.data$year)) |>
    dplyr::arrange(.data$year)
}

#' Which works need the deep fetch to have complete early history
#'
#' A work needs it when it was published before the counts_by_year floor AND its
#' per-year counts do not already reconcile with its reported total.
oa_needs_deep <- function(works, floor = OA_COUNTS_FLOOR) {
  vapply(seq_len(nrow(works)), function(i) {
    cy <- works$counts[[i]]
    isTRUE(works$pub_year[i] < floor) && sum(cy$cites) < works$total[i]
  }, logical(1))
}

#' Fill in full history for old works, in place
#'
#' @param progress optional function(i, n, title) for a Shiny progress bar.
oa_backfill_deep <- function(works, progress = NULL) {
  idx <- which(oa_needs_deep(works))
  if (!length(idx)) return(works)
  for (k in seq_along(idx)) {
    i <- idx[k]
    if (is.function(progress)) progress(k, length(idx), works$title[i])
    d <- oa_deep_year_counts(works$work_id[i])
    if (nrow(d)) works$counts[[i]] <- d
  }
  works
}

# ---- to the shape series.R wants ---------------------------------------------
# Long: one row per work x year, plus a per-work lookup of pub_year/venue.
oa_to_long <- function(works, source = "openalex") {
  long <- purrr::map_dfr(seq_len(nrow(works)), function(i) {
    cy <- works$counts[[i]]
    if (!nrow(cy))
      # A resolved work with zero citations is a real observation, not a miss.
      cy <- tibble::tibble(year = works$pub_year[i], cites = 0)
    cy |> dplyr::mutate(paper_id = works$work_id[i], source = source)
  })
  list(
    counts = dplyr::select(long, "paper_id", "year", "cites", "source"),
    papers = dplyr::transmute(works, paper_id = .data$work_id, .data$title,
                              .data$pub_year, .data$venue, .data$venue_id, .data$total)
  )
}
