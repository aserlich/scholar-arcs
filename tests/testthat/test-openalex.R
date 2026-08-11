test_that("oa_resolve_orcid separates malformed / notfound / unreachable", {
  # The distinction is the point: a network blip reported as "no such ORCID"
  # sends users off checking an identifier that was never wrong.
  expect_identical(oa_resolve_orcid("not-an-orcid"), "malformed")
  expect_identical(oa_resolve_orcid("1234"), "malformed")

  testthat::skip_on_cran(); testthat::skip_if_offline()
  options(scholararcs.mailto = "scholararcs-test@example.org")
  ok <- oa_resolve_orcid("0000-0001-6571-9081")
  skip_if(is.character(ok), "OpenAlex unreachable")
  expect_s3_class(ok, "tbl_df")
  expect_equal(nrow(ok), 1)
  expect_true(nzchar(ok$id))

  # Well-formed but unattached ORCIDs must come back as notfound, not an error.
  expect_identical(oa_resolve_orcid("0000-0002-0000-0000"), "notfound")
})

test_that("the orcid lookup does not embed a URL in the request path", {
  # Guards the regression: authors/https://orcid.org/{id} breaks wherever "//"
  # gets normalised. Keep it on the filter form.
  # Comments are stripped first: the docs deliberately name the bad form in
  # order to warn about it, and matching prose would fail on the warning itself.
  src <- readLines(file.path(.pkg_root, "R", "fetch_openalex.R"))
  code <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("authors/https://orcid", code, fixed = TRUE)))
})
