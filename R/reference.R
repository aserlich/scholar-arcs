## Field reference distribution -- the feature that turns "is 24% a lot?" into an
## answerable question. Building this by hand cost ten hand-collected Scholar
## profile ids and an afternoon; here it is one call per sampled author.
##
## Comparability is the whole game. Concentration depends heavily on how many
## papers someone has, so the sample is matched on career length and output
## before anything is compared.

#' Sample comparable authors from the same topic area
#'
#' @param topic_ids from oa_author_topics()$ids
#' @param works_count the focal author's article count; peers are drawn from a
#'   band around it, because top-1 share falls mechanically as output rises
#' @param n how many peers to sample
oa_sample_peers <- function(topic_ids, works_count, n = 25L, band = 0.6,
                            exclude = NULL) {
  lo <- max(5L, floor(works_count * (1 - band)))
  hi <- ceiling(works_count * (1 + band))
  r <- .oa_get("authors", list(
    filter = sprintf("topics.id:%s,works_count:>%d,works_count:<%d",
                     paste(topic_ids, collapse = "|"), lo, hi),
    select = "id,display_name,works_count,cited_by_count",
    per_page = min(n * 2L, 200L), sort = "cited_by_count:desc"))
  if (is.null(r$results) || !length(r$results)) return(tibble::tibble())
  out <- purrr::map_dfr(r$results, ~ tibble::tibble(
    id = .oa_id(.x$id), name = .x$display_name %||% NA_character_,
    works_count = as.integer(.x$works_count %||% NA)))
  out <- dplyr::filter(out, !.data$id %in% (exclude %||% character()))
  utils::head(out, n)
}

#' Concentration distribution across a peer sample
#'
#' One works call per peer, so this is the slow part of the app; results are
#' cached by topic set.
#' @param progress optional function(i, n, name)
#' @export
reference_distribution <- function(topic_ids, works_count, n = 25L,
                                   exclude = NULL, progress = NULL) {
  peers <- oa_sample_peers(topic_ids, works_count, n = n, exclude = exclude)
  if (!nrow(peers)) return(NULL)

  rows <- purrr::map_dfr(seq_len(nrow(peers)), function(i) {
    if (is.function(progress)) progress(i, nrow(peers), peers$name[i])
    w <- oa_works(peers$id[i])
    if (!nrow(w) || sum(w$total) == 0) return(NULL)
    cs <- concentration_stats(w$total)
    if (is.null(cs)) return(NULL)
    dplyr::mutate(cs, peer_id = peers$id[i], peer_name = peers$name[i], .before = 1)
  })
  if (!nrow(rows)) return(NULL)
  rows
}

#' Compare one author's concentration against a peer distribution
#'
#' Deliberately returns the peer n and range alongside every figure. A percentile
#' with no visible sample size behind it invites over-reading, which is exactly
#' the failure this feature exists to prevent.
#' @export
compare_to_reference <- function(mine, reference) {
  if (is.null(reference) || !nrow(reference)) return(NULL)
  metrics <- c(top1_pct = "Top paper % of citations",
               top3_pct = "Top 3 papers % of citations",
               median_cites = "Median citations per paper",
               gini = "Gini (citation inequality)")
  purrr::map_dfr(names(metrics), function(m) {
    loc <- locate_in(mine[[m]], reference[[m]])
    tibble::tibble(
      metric      = unname(metrics[m]),
      you         = round(mine[[m]], 2),
      peer_median = round(stats::median(reference[[m]], na.rm = TRUE), 2),
      peer_min    = round(min(reference[[m]], na.rm = TRUE), 2),
      peer_max    = round(max(reference[[m]], na.rm = TRUE), 2),
      percentile  = round(loc$pct),
      rank_of     = sprintf("%d of %d", loc$rank, loc$n)
    )
  })
}
