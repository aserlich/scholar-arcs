## Venue grouping. Auto-tiers so something useful renders immediately, but every
## assignment is overridable -- the automatic tiers encode an impact hierarchy,
## and that is a claim the user should be able to reject.

# Percentile cutpoints for the default tiers, on 2-year mean citedness computed
# WITHIN the author's own field. A biologist must not be ranked against the
# citation norms of political science.
ARC_TIER_CUTS   <- c(0.90, 0.75, 0.50)
ARC_TIER_LABELS <- c("Top decile", "Top quartile", "Upper half", "Rest")

#' Fetch journal-level stats for a set of OpenAlex source ids
#'
#' Batched -- OpenAlex accepts an OR-joined id filter, so 50 venues is one call.
oa_source_stats <- function(source_ids) {
  ids <- unique(stats::na.omit(source_ids))
  if (!length(ids)) return(tibble::tibble(venue_id = character(), citedness = numeric(),
                                          h_index = integer(), venue_type = character()))
  chunks <- split(ids, ceiling(seq_along(ids) / 50))
  purrr::map_dfr(chunks, function(ch) {
    r <- .oa_get("sources", list(filter = paste0("ids.openalex:", paste(ch, collapse = "|")),
                                 select = "id,display_name,type,summary_stats", per_page = 200))
    if (is.null(r$results)) return(NULL)
    purrr::map_dfr(r$results, function(s) tibble::tibble(
      venue_id   = .oa_id(s$id),
      venue      = s$display_name %||% NA_character_,
      venue_type = s$type %||% NA_character_,
      citedness  = as.numeric(s$summary_stats$`2yr_mean_citedness` %||% NA),
      h_index    = as.integer(s$summary_stats$h_index %||% NA)))
  })
}

#' The author's dominant OpenAlex topics, which define the reference area
#'
#' Topics rather than field or subfield. `field` tops out at "Social Sciences",
#' which pools political science with education; `subfield` is not a valid filter
#' on the sources endpoint. Topics are, and they are also the narrowest level that
#' still describes "journals someone in my area might publish in".
oa_author_topics <- function(author_id, n_works = 100L, n_topics = 5L) {
  r <- .oa_get("works", list(filter = sprintf("author.id:%s,type:article", .oa_id(author_id)),
                             select = "primary_topic", per_page = n_works))
  if (is.null(r$results)) return(NULL)
  ids <- purrr::map_chr(r$results, ~ .oa_id(.x$primary_topic$id))
  nms <- purrr::map_chr(r$results, ~ (.x$primary_topic$display_name %||% NA_character_))
  ok <- !is.na(ids)
  if (!any(ok)) return(NULL)
  tab <- sort(table(ids[ok]), decreasing = TRUE)
  top <- names(tab)[seq_len(min(n_topics, length(tab)))]
  list(ids = top, names = unique(nms[ok][match(top, ids[ok])]))
}

#' Citedness distribution of JOURNALS in the author's topic area
#'
#' Venue-weighted over real journals, which is what "top decile journal" means to
#' a reader. Two alternatives were tried and are both wrong:
#'
#'  * Text-searching source names for the field returns mostly-obscure titles
#'    (median citedness 0.27), against which nearly any real venue clears the 90th
#'    percentile -- it put 27 of 35 test papers in the "top decile".
#'  * Sampling articles instead of journals inherits whatever the works endpoint
#'    orders by. Unsorted it returns the most-cited papers (median 255 citations),
#'    which pushes cutpoints so high that APSR lands in "rest"; with `sample=` it
#'    returns OpenAlex's uncited long tail (median 0 citations), which drops them
#'    so low that everything is top decile.
#'
#' `works_count:>50` excludes dormant and one-issue titles. Sorting by output
#' biases the sample toward active journals, which makes the tiers conservative --
#' documented rather than corrected, because "compared to journals that actually
#' publish in my area" is the defensible reading.
oa_topic_journal_citedness <- function(topic_ids, n = 200L) {
  if (!length(topic_ids)) return(numeric())
  r <- .oa_get("sources", list(
    filter = sprintf("topics.id:%s,type:journal,works_count:>50,summary_stats.2yr_mean_citedness:>0",
                     paste(topic_ids, collapse = "|")),
    select = "display_name,summary_stats", per_page = min(n, 200L),
    sort = "works_count:desc"))
  if (is.null(r$results) || !length(r$results)) return(numeric())
  v <- purrr::map_dbl(r$results, ~ as.numeric(.x$summary_stats$`2yr_mean_citedness` %||% NA))
  v[!is.na(v)]
}

#' Auto-tier the venues in a paper set
#'
#' @param papers from oa_to_long()$papers -- needs venue and venue_id
#' @param author_id used to pick the reference field
#' @return data.frame(venue, group, citedness, n_papers) ready for citation_series()
#' @export
tier_venues <- function(papers, author_id = NULL, cuts = ARC_TIER_CUTS,
                        labels = ARC_TIER_LABELS) {
  vs <- papers |>
    dplyr::filter(!is.na(.data$venue)) |>
    dplyr::count(.data$venue, .data$venue_id, name = "n_papers")
  if (!nrow(vs)) return(tibble::tibble(venue = character(), group = character(),
                                       citedness = numeric(), n_papers = integer()))

  st <- oa_source_stats(vs$venue_id)
  vs <- dplyr::left_join(vs, dplyr::select(st, "venue_id", "citedness"), by = "venue_id")

  # Reference distribution: what papers in the author's own subfield actually get
  # published in. Falls back to the spread of their own venues, which is weaker
  # (self-referential) but never wrong-footed.
  ref <- NULL; ref_label <- "your own venues"
  if (!is.null(author_id)) {
    tp <- oa_author_topics(author_id)
    if (!is.null(tp)) {
      ref <- oa_topic_journal_citedness(tp$ids)
      if (length(ref) >= 20)
        ref_label <- paste0("journals in: ", paste(utils::head(tp$names, 3), collapse = "; "))
    }
  }
  if (length(ref) < 20) { ref <- stats::na.omit(vs$citedness); ref_label <- "your own venues" }

  brk <- if (length(ref) >= 4) stats::quantile(ref, cuts, na.rm = TRUE) else rep(NA_real_, length(cuts))

  vs$group <- vapply(vs$citedness, function(c) {
    if (is.na(c) || all(is.na(brk))) return("Unclassified")
    labels[which(c >= brk)[1] %||% length(labels)]
  }, character(1))
  # `which(...)[1]` is NA when the value clears no cut -> lowest tier.
  vs$group[is.na(vs$group)] <- labels[length(labels)]

  out <- vs |>
    dplyr::mutate(group = factor(.data$group,
                                 levels = intersect(c(labels, "Unclassified"), unique(.data$group)))) |>
    dplyr::arrange(.data$group, dplyr::desc(.data$n_papers)) |>
    dplyr::select("venue", "group", "citedness", "n_papers")

  # Carried so the UI can say what the tiers are relative to -- the cutpoints are
  # meaningless without it, and "top decile" reads as absolute if unlabelled.
  attr(out, "reference")   <- ref_label
  attr(out, "cutpoints")   <- brk
  attr(out, "reference_n") <- length(ref)
  out
}

# ---- mappings on disk --------------------------------------------------------

#' @export
save_mapping <- function(mapping, path) {
  jsonlite::write_json(list(
    generated = as.character(Sys.Date()),
    groups    = as.character(unique(mapping$group)),
    venues    = stats::setNames(as.list(as.character(mapping$group)),
                                as.character(mapping$venue))
  ), path, auto_unbox = TRUE, pretty = TRUE)
  invisible(path)
}

#' @export
load_mapping <- function(path) {
  j <- jsonlite::read_json(path, simplifyVector = FALSE)
  v <- j$venues %||% list()
  tibble::tibble(venue = names(v), group = unlist(v, use.names = FALSE)) |>
    dplyr::mutate(group = factor(.data$group,
                                 levels = unlist(j$groups %||% unique(.data$group))))
}

#' Presets shipped with the package
#' @export
list_presets <- function() {
  d <- system.file("presets", package = "scholararcs")
  if (!nzchar(d)) d <- file.path("inst", "presets")
  f <- list.files(d, pattern = "\\.json$", full.names = TRUE)
  stats::setNames(f, sub("\\.json$", "", basename(f)))
}
