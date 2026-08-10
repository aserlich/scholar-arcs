## Concentration statistics.
##
## The motivating question was "one paper is a quarter of my citations -- is that
## a lot?", which turns out to be unanswerable without a comparison group: the
## peer median was 23%. So every figure here is reported next to its reference
## distribution, never alone. See reference.R.

#' Concentration of an author's citations
#'
#' @param x either a series from citation_series(), or a bare numeric vector of
#'   per-paper citation totals.
#' @export
concentration_stats <- function(x) {
  v <- if (is.numeric(x)) x else series_finals(x)$total
  v <- v[!is.na(v)]
  if (!length(v)) return(NULL)
  s <- sort(v, decreasing = TRUE)
  tot <- sum(s)
  share <- function(k) if (tot > 0) 100 * sum(utils::head(s, k)) / tot else NA_real_
  tibble::tibble(
    n_papers   = length(s),
    total      = tot,
    top1       = s[1],
    top1_pct   = share(1),
    top3_pct   = share(3),
    top5_pct   = share(5),
    median_cites = stats::median(s),
    under5_pct = 100 * mean(s < 5),
    zero_pct   = 100 * mean(s == 0),
    # Gini over the citation distribution: 0 = every paper equal, 1 = one paper
    # holds everything. Reported because top-1 share is sensitive to a single
    # outlier in a way a whole-distribution measure is not.
    gini       = .gini(s)
  )
}

.gini <- function(v) {
  v <- sort(v[!is.na(v)])
  n <- length(v)
  if (n < 2 || sum(v) == 0) return(NA_real_)
  sum((2 * seq_len(n) - n - 1) * v) / (n * sum(v))
}

#' Where a value sits in a reference distribution
#'
#' Returns the percentile plus a plain-language rank, e.g. "5th of 10".
locate_in <- function(value, reference) {
  r <- reference[!is.na(reference)]
  if (!length(r) || is.na(value)) return(list(pct = NA_real_, rank = NA_integer_, n = length(r)))
  list(
    pct  = 100 * mean(r <= value),
    rank = as.integer(rank(c(value, r))[1]),
    n    = length(r) + 1L
  )
}
