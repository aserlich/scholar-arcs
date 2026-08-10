## The per-paper yearly series. Backend-agnostic: it takes long counts plus a
## paper lookup and knows nothing about where they came from.
##
## Three properties here are easy to get wrong and each one silently corrupts the
## charts rather than erroring:
##
##  1. Citations dated BEFORE a paper's publication year are real -- they cite the
##     working paper or preprint. They keep their negative offset. Folding them
##     into year 0 both inflates the publication-year count and erases the fact
##     that the preprint was accumulating.
##  2. The current calendar year is PARTIAL. Cutting it drops any paper published
##     this year entirely; keeping it unmarked makes every curve look like it
##     plateaued at the same instant. So it is carried and flagged.
##  3. The year grid must span every source present, not just the preferred one,
##     or the comparison curve loses its earliest years without complaint.

#' Build the per-paper yearly citation series
#'
#' @param counts long: paper_id, year, cites, source
#' @param papers lookup: paper_id, pub_year, venue (+ any extra columns kept)
#' @param groups optional data.frame(venue, group) mapping; unmapped -> "Other"
#' @param prefer which source drives the curve when more than one is present
#' @param through_year last calendar year to include; defaults to the current one
#' @return one row per paper x year with cites, cum_cites, years_since_pub,
#'   partial, alt_cum (the non-preferred source, NA when there is only one)
#' @export
citation_series <- function(counts, papers, groups = NULL,
                            prefer = NULL, through_year = NULL) {
  stopifnot(all(c("paper_id", "year", "cites", "source") %in% names(counts)))
  stopifnot(all(c("paper_id", "pub_year") %in% names(papers)))
  if (!nrow(counts)) stop("no citation data", call. = FALSE)

  this_year     <- as.integer(format(Sys.Date(), "%Y"))
  last_year     <- through_year %||% this_year
  last_complete <- min(last_year, this_year - 1L)

  srcs   <- unique(counts$source)
  prefer <- prefer %||% if ("scholar" %in% srcs) "scholar" else srcs[1]
  other  <- setdiff(srcs, prefer)

  papers <- papers |>
    dplyr::filter(!is.na(.data$pub_year)) |>
    dplyr::mutate(group = .assign_group(.data$venue %||% NA_character_, groups))

  counts <- dplyr::filter(counts, .data$paper_id %in% papers$paper_id)

  # Papers we have ANY data for, regardless of when those citations landed. Kept
  # separate from the in-range counts: a paper whose only citations fall in the
  # current partial year has been fetched successfully and belongs on the chart
  # as a flat line, not dropped.
  fetched <- dplyr::distinct(counts, .data$paper_id, .data$source)

  have <- counts |>
    dplyr::filter(.data$year <= last_year) |>
    dplyr::group_by(.data$paper_id, .data$source, .data$year) |>
    dplyr::summarise(cites = sum(.data$cites), .groups = "drop")

  src_pick <- fetched |>
    dplyr::group_by(.data$paper_id) |>
    dplyr::summarise(use = if (prefer %in% .data$source) prefer else .data$source[1],
                     .groups = "drop")

  # Across ALL sources -- see note 3 above.
  first_yr <- have |>
    dplyr::group_by(.data$paper_id) |>
    dplyr::summarise(first_cite = min(.data$year), .groups = "drop")

  grid <- papers |>
    dplyr::filter(.data$paper_id %in% fetched$paper_id) |>
    dplyr::left_join(first_yr, by = "paper_id") |>
    dplyr::mutate(start = pmin(.data$pub_year, dplyr::coalesce(.data$first_cite, .data$pub_year)),
                  year = purrr::map2(.data$start, last_year, ~ seq.int(.x, .y))) |>
    tidyr::unnest_longer("year") |>
    dplyr::mutate(year = as.integer(.data$year)) |>
    dplyr::select(-"first_cite", -"start")

  main <- grid |>
    dplyr::left_join(src_pick, by = "paper_id") |>
    dplyr::left_join(have, by = c("paper_id", "year", "use" = "source")) |>
    dplyr::mutate(cites = dplyr::coalesce(.data$cites, 0)) |>
    dplyr::arrange(.data$paper_id, .data$year) |>
    dplyr::group_by(.data$paper_id) |>
    dplyr::mutate(cum_cites = cumsum(.data$cites)) |>
    dplyr::ungroup() |>
    dplyr::rename(source = "use")

  if (length(other)) {
    alt <- grid |>
      dplyr::select("paper_id", "year") |>
      dplyr::left_join(dplyr::filter(have, .data$source == other[1]),
                       by = c("paper_id", "year")) |>
      dplyr::mutate(cites = dplyr::coalesce(.data$cites, 0)) |>
      dplyr::arrange(.data$paper_id, .data$year) |>
      dplyr::group_by(.data$paper_id) |>
      dplyr::mutate(alt_cum = cumsum(.data$cites)) |>
      dplyr::ungroup() |>
      dplyr::select("paper_id", "year", "alt_cum")
    main <- dplyr::left_join(main, alt, by = c("paper_id", "year"))
  } else {
    main$alt_cum <- NA_real_
  }

  main |>
    dplyr::mutate(
      years_since_pub = .data$year - .data$pub_year,   # negative = pre-publication
      partial         = .data$year > last_complete,
      label           = .short_label(.data$title %||% .data$paper_id,
                                     .data$pub_year, .data$venue %||% NA_character_)
    )
}

#' Collapse a series to one row per paper
#' @export
series_finals <- function(series) {
  series |>
    dplyr::group_by(.data$paper_id, .data$label, .data$title, .data$venue,
                    .data$group, .data$pub_year) |>
    dplyr::summarise(
      total          = max(.data$cum_cites),
      total_complete = max(c(.data$cum_cites[!.data$partial], 0)),
      last_year      = max(.data$year),
      .groups = "drop"
    )
}

# ---- helpers -----------------------------------------------------------------
.assign_group <- function(venue, groups) {
  if (is.null(groups) || !nrow(groups)) return(factor(rep("All papers", length(venue))))
  lut <- stats::setNames(as.character(groups$group), as.character(groups$venue))
  g <- unname(lut[as.character(venue)])
  g[is.na(g)] <- "Other"
  factor(g, levels = c(unique(as.character(groups$group)), "Other")) |> droplevels()
}

.short_label <- function(title, year, venue) {
  t <- ifelse(is.na(title), "?", substr(title, 1, 40))
  ifelse(is.na(venue), sprintf("%s (%s)", t, year), sprintf("%s · %s", t, venue))
}
