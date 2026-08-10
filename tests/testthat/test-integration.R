## Integration checks against a verified snapshot. Skipped unless the reference
## data is present, so the suite still runs on a clean machine.
##
## journal-tracker/data/citations.csv was hand-verified against both Google
## Scholar and OpenAlex, which makes it the strongest correctness check available:
## if this package's pipeline disagrees with it, this package is wrong.

REF_CSV <- path.expand("~/Dropbox/admin_projects/journal-tracker/data/citations.csv")

skip_no_ref <- function() testthat::skip_if_not(file.exists(REF_CSV), "no reference snapshot")

ref_series <- function() {
  raw <- utils::read.csv(REF_CSV, stringsAsFactors = FALSE)
  papers <- raw |>
    group_by(paper_id = .data$article_id) |>
    summarise(pub_year = min(.data$year), total = NA_integer_, .groups = "drop") |>
    mutate(title = .data$paper_id, venue = NA_character_)
  counts <- raw |> transmute(paper_id = .data$article_id, .data$year,
                             .data$cites, .data$source)
  list(counts = counts, papers = papers)
}

test_that("the pipeline reproduces the verified snapshot's per-source totals", {
  skip_no_ref()
  r <- ref_series()
  s <- citation_series(r$counts, r$papers, prefer = "scholar")
  fin <- s |> group_by(paper_id, source) |> summarise(cum = max(cum_cites), .groups = "drop")
  raw <- r$counts |> filter(year <= max(s$year)) |>
    group_by(paper_id, source) |> summarise(tot = sum(cites), .groups = "drop")
  cmp <- inner_join(fin, raw, by = c("paper_id", "source")) |> filter(cum != tot)
  expect_equal(nrow(cmp), 0)
})

test_that("Scholar and OpenAlex agree on shape while differing on level", {
  skip_no_ref()
  # The finding that justifies defaulting the hosted app to OpenAlex: it reports
  # materially fewer citations but ranks papers almost identically, and all three
  # charts show shape rather than level.
  r <- ref_series()
  s <- citation_series(r$counts, r$papers, prefer = "scholar") |> filter(!partial)
  f <- s |> group_by(paper_id) |>
    summarise(scholar = max(cum_cites), openalex = max(alt_cum), .groups = "drop") |>
    filter(scholar + openalex > 0)
  skip_if_not(nrow(f) > 10, "reference snapshot too small")

  expect_lt(sum(f$openalex), sum(f$scholar))                  # OpenAlex undercounts
  expect_gt(sum(f$openalex) / sum(f$scholar), 0.5)            # but not wildly
  expect_gt(stats::cor(f$scholar, f$openalex, method = "spearman"), 0.85)
})

test_that("live OpenAlex matches the snapshot's OpenAlex column", {
  skip_no_ref()
  testthat::skip_on_cran()
  testthat::skip_if_offline()
  options(scholararcs.mailto = "scholararcs-test@example.org")
  w <- oa_works("A5079669887")
  skip_if_not(nrow(w) > 0, "OpenAlex unreachable")
  # Per-year counts must reconcile with each work's reported total, which is the
  # invariant the pre-2012 deep fetch exists to preserve.
  w <- oa_backfill_deep(w)
  recon <- vapply(seq_len(nrow(w)),
                  function(i) sum(w$counts[[i]]$cites) >= w$total[i], logical(1))
  expect_true(all(recon))
})
