## The series builder. Each of these guards a bug that produced a plausible-looking
## but wrong chart rather than an error.

test_that("cumulative citations never decrease", {
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  bad <- s |> group_by(paper_id) |> summarise(ok = all(diff(cum_cites) >= 0)) |> filter(!ok)
  expect_equal(nrow(bad), 0)
})

test_that("every published paper appears, including uncited ones", {
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  expect_setequal(unique(s$paper_id), f$papers$paper_id)
  fin <- series_finals(s)
  expect_equal(fin$total[fin$paper_id == "p_zero"], 0)
})

test_that("a paper cited only in the partial year is retained, not dropped", {
  # Cutting the partial year outright removed such papers entirely; keeping it
  # unmarked made every curve look like it plateaued.
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  expect_true("p_new" %in% s$paper_id)
  expect_true(any(s$partial[s$paper_id == "p_new"]))
})

test_that("pre-publication citations keep a negative offset", {
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  pre <- s |> filter(paper_id == "p_pre", years_since_pub < 0)
  expect_gt(nrow(pre), 0)
  expect_true(all(pre$year < pre$pub_year))
  expect_gt(sum(pre$cites), 0)          # they carry real citations, not zeros
})

test_that("the year grid is contiguous and starts no later than publication", {
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  chk <- s |> group_by(paper_id) |>
    summarise(contig = identical(sort(year), seq(min(year), max(year))),
              starts_by_pub = min(year) <= first(pub_year)) |>
    filter(!contig | !starts_by_pub)
  expect_equal(nrow(chk), 0)
})

test_that("the partial flag marks exactly the current year", {
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  expect_true(all(s$partial == (s$year >= f$this_year)))
})

test_that("totals reconcile with the source that drives the curve", {
  f <- fixture(); s <- citation_series(f$counts, f$papers, prefer = "scholar")
  fin <- s |> group_by(paper_id, source) |> summarise(cum = max(cum_cites), .groups = "drop")
  raw <- f$counts |> filter(year <= f$this_year) |>
    group_by(paper_id, source) |> summarise(raw = sum(cites), .groups = "drop")
  cmp <- inner_join(fin, raw, by = c("paper_id", "source")) |> filter(cum != raw)
  expect_equal(nrow(cmp), 0)
})

test_that("the alternate-source curve keeps its own earlier years", {
  # The grid must span BOTH sources: openalex reaches a year further back on
  # p_old, and a grid cut to the preferred source silently dropped it.
  f <- fixture(); s <- citation_series(f$counts, f$papers, prefer = "scholar")
  alt <- s |> filter(paper_id == "p_old") |> summarise(m = max(alt_cum)) |> pull(m)
  oa  <- f$counts |> filter(paper_id == "p_old", source == "openalex") |>
    summarise(s = sum(cites)) |> pull(s)
  expect_equal(alt, oa)
  expect_lte(min(s$year[s$paper_id == "p_old"]), f$this_year - 7L)
})

test_that("a single source yields NA rather than a fake comparison", {
  f <- fixture()
  one <- filter(f$counts, source == "scholar")
  s <- citation_series(one, f$papers)
  expect_true(all(is.na(s$alt_cum)))
})

test_that("unmapped venues fall into Other rather than vanishing", {
  f <- fixture()
  m <- tibble(venue = "Journal A", group = "Tier 1")
  s <- citation_series(f$counts, f$papers, groups = m)
  expect_true("Other" %in% levels(s$group))
  expect_setequal(unique(s$paper_id), f$papers$paper_id)
})
