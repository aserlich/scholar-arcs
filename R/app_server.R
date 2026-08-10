arc_server <- function(input, output, session) {

  st <- shiny::reactiveValues(
    author = NULL, papers = NULL, counts = NULL, tiers = NULL,
    mapping = NULL, msg = NULL, ref = NULL, topics = NULL)

  say <- function(text, type = "info") st$msg <- list(text = text, type = type)

  output$status <- shiny::renderUI({
    m <- st$msg; if (is.null(m)) return(NULL)
    cls <- switch(m$type, error = "alert-danger", warn = "alert-warning", "alert-info")
    shiny::div(class = paste("alert", cls, "py-2"), shiny::HTML(m$text))
  })

  output$backend_note <- shiny::renderUI({
    if (input$backend != "scholar") return(NULL)
    r <- gs_block_reason()
    if (is.null(r)) shiny::div(class = "small text-success", "Running locally — Scholar available.")
    else shiny::div(class = "small text-danger", r)
  })

  # ---- identity ------------------------------------------------------------
  shiny::observeEvent(input$go_orcid, {
    shiny::req(nzchar(input$orcid))
    a <- oa_resolve_orcid(input$orcid)
    if (is.null(a) || !nrow(a)) return(say("No OpenAlex author for that ORCID.", "error"))
    st$author <- a
    load_openalex(a$id[1])
  })

  shiny::observeEvent(input$go_name, {
    shiny::req(nzchar(input$name))
    c <- oa_search_authors(input$name, 8L)
    if (!nrow(c)) return(say("No authors matched that name.", "error"))
    st$candidates <- c
    output$candidates <- shiny::renderUI({
      shiny::tagList(
        shiny::helpText("Pick your profile. Check the work count and institution — OpenAlex ",
                        "sometimes merges two people into one record."),
        shiny::radioButtons("cand", NULL, choiceNames = lapply(seq_len(nrow(c)), function(i)
          shiny::HTML(sprintf("<b>%s</b><br><span class='small text-muted'>%s · %d works · %d cites</span>",
                              c$name[i], c$institution[i] %|NA|% "unknown institution",
                              c$works_count[i], c$cited_by_count[i]))),
          choiceValues = c$id, selected = character(0)),
        shiny::actionButton("pick", "Use this profile", class = "btn-primary btn-sm"))
    })
  })

  shiny::observeEvent(input$pick, {
    shiny::req(input$cand)
    st$author <- dplyr::filter(st$candidates, .data$id == input$cand)
    load_openalex(input$cand)
  })

  load_openalex <- function(author_id) {
    shiny::withProgress(message = "Fetching from OpenAlex", value = 0.1, {
      w <- oa_works(author_id)
      if (!nrow(w)) return(say("That profile has no journal articles in OpenAlex.", "error"))
      shiny::setProgress(0.4, detail = "checking pre-2012 coverage")
      nd <- sum(oa_needs_deep(w))
      if (nd > 0) {
        w <- oa_backfill_deep(w, progress = function(i, n, t)
          shiny::setProgress(0.4 + 0.4 * i / n, detail = sprintf("older paper %d/%d", i, n)))
      }
      L <- oa_to_long(w)
      st$papers <- L$papers; st$counts <- L$counts
      shiny::setProgress(0.85, detail = "tiering venues")
      st$tiers   <- tier_venues(L$papers, author_id)
      st$mapping <- dplyr::select(st$tiers, "venue", "group")
      tp <- oa_author_topics(author_id); st$topics <- tp
      say(sprintf("%d articles, %d citations.%s", nrow(w), sum(w$total),
                  if (nd) sprintf(" Backfilled %d pre-2012 paper(s).", nd) else ""))
    })
  }

  shiny::observeEvent(input$go_gs, {
    shiny::req(nzchar(input$gs_id))
    r <- gs_block_reason()
    if (!is.null(r)) return(say(r, "error"))
    shiny::withProgress(message = "Fetching from Google Scholar", value = 0, {
      res <- try(gs_fetch(input$gs_id, progress = function(i, n, t)
        shiny::setProgress(i / n, detail = sprintf("%d/%d · %s", i, n, substr(t, 1, 40)))),
        silent = TRUE)
      if (inherits(res, "try-error")) return(say(conditionMessage(attr(res, "condition")), "error"))
      st$papers <- res$papers; st$counts <- res$counts
      st$tiers <- NULL; st$mapping <- NULL      # no venue ids from Scholar -> no auto-tier
      say(sprintf("%d entries, %d citations.%s", nrow(res$papers), sum(res$papers$total),
                  if (length(res$blocked))
                    sprintf(" <b>%d blocked</b> — re-run to retry; cached progress is kept.",
                            length(res$blocked)) else ""),
          if (length(res$blocked)) "warn" else "info")
    })
  })

  # ---- derived -------------------------------------------------------------
  series <- shiny::reactive({
    shiny::req(st$counts, st$papers)
    citation_series(st$counts, st$papers, groups = st$mapping)
  })

  output$tier_reference <- shiny::renderText({
    if (is.null(st$tiers)) return("Scholar gives no venue ids, so grouping must be set by hand.")
    sprintf("Tiers relative to %s (n = %d).",
            attr(st$tiers, "reference") %||% "?", attr(st$tiers, "reference_n") %||% 0L)
  })

  # ---- charts --------------------------------------------------------------
  output$p_calendar <- shiny::renderPlot(plot_calendar(series()), res = 110)
  output$p_aligned  <- shiny::renderPlot(plot_aligned(series()),  res = 110)
  output$p_annual   <- shiny::renderPlot(plot_annual(series()),   res = 110)

  # ---- concentration -------------------------------------------------------
  conc <- shiny::reactive(concentration_stats(series()))

  output$conc_cards <- shiny::renderUI({
    c <- conc(); shiny::req(c)
    tile <- function(v, l) shiny::div(class = "col",
      shiny::div(class = "fs-3 fw-bold text-primary", v),
      shiny::div(class = "small text-uppercase text-muted", l))
    shiny::div(class = "row row-cols-auto g-4 mb-3",
      tile(c$n_papers, "papers"), tile(c$total, "citations"),
      tile(sprintf("%.0f%%", c$top1_pct), "top paper share"),
      tile(sprintf("%.0f%%", c$top3_pct), "top 3 share"),
      tile(c$median_cites, "median per paper"),
      tile(sprintf("%.2f", c$gini), "gini"))
  })

  shiny::observeEvent(input$go_ref, {
    shiny::req(st$topics, st$papers)
    shiny::withProgress(message = "Sampling peers in your area", value = 0, {
      r <- reference_distribution(
        st$topics$ids, nrow(st$papers),
        exclude = st$author$id,
        progress = function(i, n, nm) shiny::setProgress(i / n, detail = sprintf("%d/%d", i, n)))
      st$ref <- r
      if (is.null(r)) say("Could not assemble a comparable peer sample.", "warn")
    })
  })

  output$ref_table <- DT::renderDT({
    shiny::req(st$ref, conc())
    cmp <- compare_to_reference(conc(), st$ref)
    DT::datatable(cmp, rownames = FALSE, options = list(dom = "t", ordering = FALSE),
                  caption = sprintf("Against %d sampled authors in the same topics with a similar number of papers.",
                                    nrow(st$ref)))
  })

  # ---- tables & downloads --------------------------------------------------
  output$papers_table <- DT::renderDT({
    f <- series_finals(series())
    DT::datatable(
      f |> dplyr::arrange(dplyr::desc(.data$total)) |>
        dplyr::transmute(Paper = .data$title, Venue = .data$venue, Group = .data$group,
                         Published = .data$pub_year, Citations = .data$total),
      rownames = FALSE, options = list(pageLength = 25))
  })

  output$dl_csv <- shiny::downloadHandler(
    filename = function() "citation-series.csv",
    content = function(f) utils::write.csv(series(), f, row.names = FALSE))

  output$dl_png <- shiny::downloadHandler(
    filename = function() "citation-arcs.zip",
    content = function(f) {
      d <- tempfile(); dir.create(d)
      ps <- list(calendar = plot_calendar(series()), aligned = plot_aligned(series()),
                 arc = plot_annual(series()))
      fs <- vapply(names(ps), function(n) {
        p <- file.path(d, paste0("citations-", n, ".png"))
        ggplot2::ggsave(p, ps[[n]], width = 12, height = 7, dpi = 200, bg = "white")
        p }, character(1))
      utils::zip(f, fs, flags = "-j9X")
    })
}

`%|NA|%` <- function(a, b) if (is.na(a)) b else a
