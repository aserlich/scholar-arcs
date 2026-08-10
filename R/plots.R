## The three charts. Shared by the app and by the PNG export so they cannot drift.

## Palette note. These eight hues in this order are a validated categorical set:
## they clear the adjacent-pair CVD gate (worst protanopia dE 9.1, normal-vision
## 19.6) on a white surface. They do NOT clear the ALL-PAIRS gate past three
## slots -- with six on screen, green vs orange collapses to dE 3.2 under
## protanopia. In a multi-line chart any two lines can end up side by side, which
## is the all-pairs case. That is why the aligned and arc charts FACET by group:
## one group coloured per panel against grey context, so position reinforces
## colour. Do not "simplify" them into a single panel.
##
## Slots 3, 4 and 5 sit below 3:1 contrast on white, so every chart using them
## ships direct labels or the table view alongside.
ARC_HUES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
              "#e87ba4", "#008300", "#4a3aa7", "#e34948")

#' Categorical palette for n groups
#'
#' Caps at eight. Beyond that a ninth hue would have to be invented, which breaks
#' the validation above -- fold the tail into "Other" instead.
#' @export
arc_palette <- function(groups) {
  g <- if (is.factor(groups)) levels(groups) else unique(as.character(groups))
  if (length(g) > length(ARC_HUES))
    warning("more than ", length(ARC_HUES), " groups; extra groups reuse hues and stop being distinguishable",
            call. = FALSE)
  stats::setNames(rep(ARC_HUES, length.out = length(g)), g)
}

.arc_theme <- function(base = 11) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#e1e0d9", linewidth = 0.3),
      strip.text  = ggplot2::element_text(face = "bold", size = 9, hjust = 0),
      plot.title  = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 8, colour = "#52514e"),
      plot.caption  = ggplot2::element_text(size = 7, colour = "#898781"),
      plot.margin   = ggplot2::margin(8, 14, 6, 8))
}

# Group median, restricted to ages where at least `min_n` papers still contribute.
# Evaluated FROM AGE 0 FORWARD: the negative tail holds only the few papers with
# preprint citations, so running the check from there kills every median line.
.arc_median <- function(s, value, min_n = 3L) {
  s |>
    dplyr::filter(.data$years_since_pub >= 0) |>
    dplyr::group_by(.data$group, .data$years_since_pub) |>
    dplyr::summarise(m = stats::median({{ value }}),
                     n = dplyr::n_distinct(.data$paper_id), .groups = "drop") |>
    dplyr::arrange(.data$group, .data$years_since_pub) |>
    dplyr::group_by(.data$group) |>
    dplyr::filter(dplyr::cumall(.data$n >= min_n)) |>
    dplyr::ungroup()
}

# Solid body (complete years), the dashed bridge into the partial year, and
# endpoint markers. The markers are load-bearing, not decoration: a paper with a
# single observation -- anything published this year, or last year with the
# current year partial -- draws no line at all without them.
.arc_layers <- function(s, xvar) {
  x <- rlang::ensym(xvar)
  list(
    solid  = dplyr::filter(s, !.data$partial),
    bridge = s |> dplyr::group_by(.data$paper_id) |>
      dplyr::filter(any(.data$partial)) |>
      dplyr::filter(.data$years_since_pub >=
                      max(c(.data$years_since_pub[!.data$partial], -Inf))) |>
      dplyr::ungroup(),
    ends_solid = s |> dplyr::filter(!.data$partial) |> dplyr::group_by(.data$paper_id) |>
      dplyr::slice_max(!!x, n = 1, with_ties = FALSE) |> dplyr::ungroup(),
    ends_part  = s |> dplyr::filter(.data$partial) |> dplyr::group_by(.data$paper_id) |>
      dplyr::slice_max(!!x, n = 1, with_ties = FALSE) |> dplyr::ungroup()
  )
}

.maybe_facet <- function(s) {
  if (dplyr::n_distinct(s$group) <= 1) NULL
  else ggplot2::facet_wrap(~group, ncol = 3)
}

# ---- 1. Calendar time --------------------------------------------------------

#' Cumulative citations in calendar time
#' @export
plot_calendar <- function(series, label_n = 7, base = 11, caption = NULL) {
  L   <- .arc_layers(series, year)
  fin <- series_finals(series)
  labs_df <- fin |> dplyr::slice_max(.data$total, n = label_n) |>
    dplyr::transmute(x = .data$last_year, y = .data$total, .data$label)

  ggplot2::ggplot(mapping = ggplot2::aes(.data$year, .data$cum_cites,
                                         group = .data$paper_id, colour = .data$group)) +
    ggplot2::geom_line(data = L$solid, linewidth = 0.7, alpha = 0.9) +
    ggplot2::geom_line(data = L$bridge, linewidth = 0.7, alpha = 0.9, linetype = "22") +
    ggplot2::geom_point(data = L$ends_solid, size = 1.2) +
    ggplot2::geom_point(data = L$ends_part, size = 1.5, shape = 21, fill = "white", stroke = 0.7) +
    ggrepel::geom_text_repel(
      data = labs_df, mapping = ggplot2::aes(.data$x, .data$y, label = .data$label),
      inherit.aes = FALSE, size = 2.6, colour = "#52514e", hjust = 0, direction = "y",
      nudge_x = 0.45, segment.colour = "#c3c2b7", segment.size = 0.3,
      min.segment.length = 0, box.padding = 0.25, max.overlaps = Inf, seed = 1) +
    ggplot2::scale_colour_manual(values = arc_palette(series$group), name = NULL, drop = FALSE) +
    # Right expansion holds the direct labels; breaks pinned to the data range so
    # the axis does not tick out into that label gutter.
    ggplot2::scale_x_continuous(
      breaks = seq(min(series$year), max(series$year), by = 2),
      expand = ggplot2::expansion(mult = c(0.01, 0.28))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::guides(colour = ggplot2::guide_legend(nrow = 1,
      override.aes = list(linewidth = 1.6))) +
    ggplot2::labs(title = "Cumulative citations, calendar time",
      subtitle = "One line per paper · dashed tail + hollow point = partial current year",
      x = NULL, y = "Cumulative citations", caption = caption) +
    .arc_theme(base) +
    ggplot2::theme(legend.position = "top",
                   legend.text = ggplot2::element_text(size = 8))
}

# ---- 2. Aligned on publication ----------------------------------------------

#' Cumulative citations aligned on each paper's publication year
#' @export
plot_aligned <- function(series, min_n = 3L, base = 11, caption = NULL) {
  L   <- .arc_layers(series, years_since_pub)
  med <- .arc_median(series, .data$cum_cites, min_n)
  ctx <- series |> dplyr::filter(!.data$partial) |>
    dplyr::select("paper_id", "years_since_pub", "cum_cites")

  ggplot2::ggplot(mapping = ggplot2::aes(.data$years_since_pub, .data$cum_cites,
                                         group = .data$paper_id, colour = .data$group)) +
    ggplot2::geom_line(data = ctx, mapping = ggplot2::aes(.data$years_since_pub,
      .data$cum_cites, group = .data$paper_id), inherit.aes = FALSE,
      colour = "#e1e0d9", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c3c2b7", linewidth = 0.4) +
    ggplot2::geom_line(data = L$solid, linewidth = 0.7) +
    ggplot2::geom_line(data = L$bridge, linewidth = 0.7, linetype = "22") +
    ggplot2::geom_point(data = L$ends_solid, size = 1.1) +
    ggplot2::geom_point(data = L$ends_part, size = 1.4, shape = 21, fill = "white", stroke = 0.6) +
    ggplot2::geom_line(data = med, mapping = ggplot2::aes(.data$years_since_pub, .data$m),
      inherit.aes = FALSE, colour = "#0b0b0b", linewidth = 0.9, linetype = "22") +
    .maybe_facet(series) +
    ggplot2::scale_colour_manual(values = arc_palette(series$group), guide = "none", drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::labs(title = "Cumulative citations by years since publication",
      subtitle = paste0("Grey = all papers · coloured = this group · dashed black = group median while n ≥ ",
                        min_n, " · x < 0 = cited before publication"),
      x = "Years since publication", y = "Cumulative citations", caption = caption) +
    .arc_theme(base)
}

# ---- 3. The arc --------------------------------------------------------------

#' Citations per year -- the rate, not the running total
#'
#' The partial current year is dropped entirely here. A part-year count plotted
#' as an annual rate fakes a simultaneous collapse across every paper.
#' @export
plot_annual <- function(series, min_n = 3L, base = 11, caption = NULL) {
  s   <- dplyr::filter(series, !.data$partial)
  med <- .arc_median(s, .data$cites, min_n)
  ctx <- dplyr::select(s, "paper_id", "years_since_pub", "cites")
  pk  <- med |> dplyr::group_by(.data$group) |>
    dplyr::slice_max(.data$m, n = 1, with_ties = FALSE) |>
    dplyr::filter(.data$m > 0) |> dplyr::ungroup()
  singles <- s |> dplyr::group_by(.data$paper_id) |>
    dplyr::filter(dplyr::n() == 1) |> dplyr::ungroup()

  ggplot2::ggplot(mapping = ggplot2::aes(.data$years_since_pub, .data$cites,
                                         group = .data$paper_id, colour = .data$group)) +
    ggplot2::geom_line(data = ctx, mapping = ggplot2::aes(.data$years_since_pub,
      .data$cites, group = .data$paper_id), inherit.aes = FALSE,
      colour = "#e1e0d9", linewidth = 0.35) +
    ggplot2::geom_vline(xintercept = 0, colour = "#c3c2b7", linewidth = 0.4) +
    ggplot2::geom_line(data = s, linewidth = 0.7) +
    ggplot2::geom_point(data = singles, size = 1.1) +
    ggplot2::geom_line(data = med, mapping = ggplot2::aes(.data$years_since_pub, .data$m),
      inherit.aes = FALSE, colour = "#0b0b0b", linewidth = 0.9, linetype = "22") +
    ggplot2::geom_point(data = pk, mapping = ggplot2::aes(.data$years_since_pub, .data$m),
      inherit.aes = FALSE, colour = "#0b0b0b", size = 1.8) +
    .maybe_facet(series) +
    ggplot2::scale_colour_manual(values = arc_palette(series$group), guide = "none", drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::labs(title = "The arc: citations per year",
      subtitle = paste0("Annual rate, not cumulative · dashed black = group median while n ≥ ",
                        min_n, " (dot = its peak) · partial current year excluded"),
      x = "Years since publication", y = "Citations that year", caption = caption) +
    .arc_theme(base)
}
