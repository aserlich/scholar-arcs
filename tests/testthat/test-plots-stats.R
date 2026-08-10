# Find layers by what they contain, not by position -- index-based lookups break
# silently whenever a layer is added.
layer_data_all <- function(p) {
  d <- lapply(p$layers, function(l) l$data)
  d[vapply(d, function(x) is.data.frame(x) && nrow(x) > 0, logical(1))]
}
layers_with <- function(p, col) {
  Filter(function(x) col %in% names(x), layer_data_all(p))
}

test_that("the arc chart excludes the partial year", {
  # A part-year count drawn as an annual rate fakes a simultaneous collapse.
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  p <- plot_annual(s)
  ls <- layers_with(p, "partial")
  expect_gt(length(ls), 0)
  expect_false(any(vapply(ls, function(x) any(x$partial), logical(1))))
})

test_that("the cumulative charts keep the partial year but mark it", {
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  for (p in list(plot_aligned(s), plot_calendar(s))) {
    ls <- layers_with(p, "partial")
    expect_true(any(vapply(ls, function(x) any(x$partial), logical(1))))
  }
})

test_that("single-observation papers get a point so they are visible", {
  # p_new has one complete-year row; a line through one point draws nothing, so
  # without the endpoint markers it disappears from the chart entirely.
  f <- fixture(); s <- citation_series(f$counts, f$papers)
  pt_layers <- function(p) Filter(
    function(l) inherits(l$geom, "GeomPoint") && is.data.frame(l$data) && nrow(l$data),
    p$layers)
  for (p in list(plot_calendar(s), plot_aligned(s))) {
    ids <- unlist(lapply(pt_layers(p), function(l) l$data$paper_id))
    expect_true("p_new" %in% ids)
  }
})

test_that("the group median is evaluated from age zero, not the negative tail", {
  # Starting the running n >= min_n check in the negative tail -- where only the
  # preprinted papers exist -- killed every median line.
  f <- fixture()
  m <- tibble(venue = c("Journal A", "Journal B", "Journal C"), group = "One group")
  s <- citation_series(f$counts, f$papers, groups = m)
  med <- .arc_median(s, cum_cites, min_n = 2L)
  expect_gt(nrow(med), 0)
  expect_true(all(med$years_since_pub >= 0))
})

test_that("the palette is capped and warns rather than inventing hues", {
  expect_length(arc_palette(factor(letters[1:4])), 4)
  expect_warning(arc_palette(factor(letters[1:9])), "more than")
  expect_false(anyDuplicated(arc_palette(factor(letters[1:8]))) > 0)
})

test_that("concentration statistics are arithmetically right", {
  cs <- concentration_stats(c(50, 30, 10, 10))
  expect_equal(cs$total, 100)
  expect_equal(cs$top1_pct, 50)
  expect_equal(cs$top3_pct, 90)
  expect_equal(cs$median_cites, 20)
  expect_equal(cs$under5_pct, 0)

  # perfect equality -> gini 0; total concentration -> approaching 1
  expect_equal(.gini(rep(10, 5)), 0)
  expect_gt(.gini(c(100, 0, 0, 0, 0)), 0.7)
})

test_that("locate_in reports position and sample size together", {
  l <- locate_in(21, c(8, 14, 15, 23, 30, 33, 45))
  expect_equal(l$rank, 4L)
  expect_equal(l$n, 8L)
  expect_true(l$pct > 0 && l$pct < 100)
})
