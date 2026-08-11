# scholar-arcs

Three views of a researcher's citation record:

1. **Calendar time** — cumulative citations, one line per paper. Mostly measures age.
2. **Aligned on publication** — every paper restarted at its own year zero, faceted by venue tier.
   The fair comparison.
3. **The arc** — citations *per year*, which is the only one of the three that can show when a
   paper actually peaked.

## Run it

You need R (4.1 or newer). Nothing else — no server, no account, no API key.

**Quickest — no installation.** Clone and point Shiny at the folder:

```sh
git clone https://github.com/aserlich/scholar-arcs.git
```
```r
install.packages(c("shiny", "bslib", "DT", "dplyr", "tidyr", "purrr", "tibble",
                   "rlang", "ggplot2", "ggrepel", "scales", "jsonlite", "httr2"))
shiny::runApp("scholar-arcs")
```

**Or install it as a package**, which also gives you the functions on their own:

```r
# install.packages("remotes")
remotes::install_github("aserlich/scholar-arcs")

scholararcs::run_app(mailto = "you@example.edu")
```

`mailto` puts you in OpenAlex's "polite pool" — faster and more stable, and it is how OpenAlex asks
to be identified. Set it once with `options(scholararcs.mailto = "you@example.edu")` or the
`SCHOLARARCS_MAILTO` environment variable.

Then enter your **ORCID**, or search your name and pick your profile from the candidates.

## Use Google Scholar instead

Google Scholar reports substantially higher citation counts than OpenAlex — roughly 1.5× on the
record this was built against — because it indexes preprints, book chapters, theses and non-English
work that OpenAlex misses. Those are the numbers most people recognise.

**It only works locally.** Scholar has no API; the `scholar` package scrapes the HTML, and Google
blocks datacenter IP addresses, so a hosted copy of this app is useless for it. On your own machine
from your own connection it is fine.

```r
install.packages("scholar")          # not installed by default

scholararcs::run_app(source = "scholar")
```

Then paste your **profile id** — the `user=` value from your Scholar URL:

```
https://scholar.google.com/citations?user=h8n98PQAAAAJ
                                          ^^^^^^^^^^^^
```

Name lookup is not offered because Google no longer allows it — `get_scholar_id()` returns nothing
for every name, even from a residential connection.

**Expect it to be slow.** Scholar is fetched one paper at a time behind an 8-second throttle, so a
50-paper profile takes about seven minutes. Progress is cached to disk as it goes, so if Google
starts blocking part-way through you can re-run and it picks up where it stopped rather than
starting over.

**One thing Scholar cannot do here:** it returns journal *names* but no venue identifiers, so
automatic tiering is unavailable on that backend. Group your venues by hand, or load a saved
mapping — `inst/presets/political-science-6.json` is an example.

## Why two backends

Google Scholar has no API. Scraping it from a server gets the IP banned, and a browser-side app
cannot do it either because Scholar sends no CORS headers. Neither is a throttling problem. So the
Scholar backend only works when you run the app on your own machine, and the hosted build refuses
it outright.

OpenAlex is the default because it works anywhere. It reports fewer citations than Scholar — about
68% on the record this was built against — but the two agree closely on *shape* (Pearson r = 0.97,
Spearman 0.94), and all three charts show shape rather than level.

## Known limitations, stated up front

- **OpenAlex merges people.** A name search can return one profile covering two different
  researchers. Check the work count and institution before trusting a result.
- **Scholar profiles are curated.** Some list only journal articles, others include working papers,
  talks and software. Any per-paper median partly measures that choice, not citation performance.
- **OpenAlex `counts_by_year` starts at 2012.** Papers older than that need a slower per-work fetch
  or their early years are missing.
- Small tiers give medians over one or two papers. The app draws them; read them accordingly.
