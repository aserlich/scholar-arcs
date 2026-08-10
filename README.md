# scholar-arcs

Three views of a researcher's citation record:

1. **Calendar time** — cumulative citations, one line per paper. Mostly measures age.
2. **Aligned on publication** — every paper restarted at its own year zero, faceted by venue tier.
   The fair comparison.
3. **The arc** — citations *per year*, which is the only one of the three that can show when a
   paper actually peaked.

## Run it

No installation needed — point Shiny at the folder:

```r
shiny::runApp("~/Dropbox/admin_projects/scholar-arcs")
```

Or install it once and use the package interface:

```sh
R CMD INSTALL scholar-arcs      # from the parent directory
```
```r
scholararcs::run_app(mailto = "you@example.edu")   # OpenAlex, works anywhere
scholararcs::run_app(source = "scholar")           # Google Scholar, LOCAL ONLY
```

`mailto` puts you in OpenAlex's "polite pool", which is faster and more stable. Set it once with
`options(scholararcs.mailto = "you@example.edu")` or the `SCHOLARARCS_MAILTO` environment variable.

**Identifiers.** OpenAlex takes an ORCID, or a name search that shows candidate profiles to pick
from. Google Scholar takes the `user=` value out of your profile URL — name lookup no longer works,
because Google blocks it.

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
