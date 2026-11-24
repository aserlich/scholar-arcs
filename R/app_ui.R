## UI. The limitations are surfaced in the interface, not buried in a README --
## every one of them was found the hard way and each can silently mislead.

arc_ui <- function(request) {
  bslib::page_sidebar(
    title = "Citation arcs",
    theme = bslib::bs_theme(version = 5, primary = "#2a78d6"),

    sidebar = bslib::sidebar(
      width = 340,

      shiny::radioButtons("backend", "Data source",
        choices = c("OpenAlex (works anywhere)" = "openalex",
                    "Google Scholar (local only)" = "scholar"),
        selected = getOption("scholararcs.default_backend", "openalex")),
      shiny::uiOutput("backend_note"),

      shiny::conditionalPanel("input.backend == 'openalex'",
        shiny::textInput("orcid", "ORCID", placeholder = "0000-0001-6571-9081"),
        shiny::actionButton("go_orcid", "Look up", class = "btn-primary btn-sm"),
        shiny::hr(),
        shiny::textInput("name", "…or search by name", placeholder = "Jane Researcher"),
        shiny::actionButton("go_name", "Search", class = "btn-sm"),
        shiny::uiOutput("candidates")),

      shiny::conditionalPanel("input.backend == 'scholar'",
        shiny::textInput("gs_id", "Scholar profile id", placeholder = "h8n98PQAAAAJ"),
        shiny::helpText(shiny::HTML(
          "The <code>user=</code> value in your profile URL. Name lookup no longer works &mdash; ",
          "Google blocks it. Expect roughly 8 seconds per paper.")),
        shiny::actionButton("go_gs", "Fetch", class = "btn-primary btn-sm")),

      shiny::hr(),
      shiny::selectInput("preset", "Venue grouping",
        choices = c("Auto-tier by journal impact" = "auto"), selected = "auto"),
      shiny::helpText(shiny::textOutput("tier_reference", inline = TRUE)),
      shiny::actionButton("edit_tiers", "Edit groups…", class = "btn-sm"),

      shiny::hr(),
      shiny::downloadButton("dl_csv", "Data (CSV)", class = "btn-sm"),
      shiny::downloadButton("dl_png", "Charts (PNG)", class = "btn-sm")
    ),

    shiny::uiOutput("status"),

    bslib::navset_card_tab(
      bslib::nav_panel("Calendar time",
        shiny::plotOutput("p_calendar", height = "560px"),
        shiny::helpText("Dominated by vintage: an older paper has had more years to accumulate. ",
                        "Mostly a reminder that total citations largely measure age.")),
      bslib::nav_panel("Aligned on publication",
        shiny::plotOutput("p_aligned", height = "560px"),
        shiny::helpText("Every paper restarted at its own year zero. The fair comparison. ",
                        "x < 0 means the paper was cited before publication, as a preprint.")),
      bslib::nav_panel("The arc",
        shiny::plotOutput("p_annual", height = "560px"),
        shiny::helpText("Citations per year rather than the running total — the only one of ",
                        "the three that can show when a paper actually peaked.")),
      bslib::nav_panel("Concentration",
        shiny::uiOutput("conc_cards"),
        shiny::actionButton("go_ref", "Compare against peers in my area", class = "btn-sm"),
        shiny::helpText("Samples authors in the same topics with a similar number of papers. ",
                        "One request per peer, so this takes a moment."),
        DT::DTOutput("ref_table")),
      bslib::nav_panel("Papers", DT::DTOutput("papers_table"))
    ),

    shiny::tags$footer(class = "text-muted small mt-3", shiny::HTML(
      "<b>Read with care.</b> OpenAlex sometimes merges two researchers into one profile &mdash; ",
      "check the work count and institution. Scholar profiles are curated differently by ",
      "different people, so any per-paper median partly measures that choice. OpenAlex ",
      "per-year counts start at 2012; older papers are backfilled more slowly. Groups holding ",
      "one or two papers produce medians over one or two papers."))
  )
}
