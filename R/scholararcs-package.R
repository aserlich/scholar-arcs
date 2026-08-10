#' @keywords internal
#' @aliases scholararcs-package
#' @importFrom rlang .data %||%
#' @importFrom stats median quantile setNames na.omit
#' @importFrom utils head write.csv zip
"_PACKAGE"

## Package-level notes worth keeping next to the code:
##
## Two backends exist because Google Scholar has no API. It bans server IP
## addresses and sends no CORS headers, so it cannot be reached from a hosted app
## or from a browser -- only from a user's own machine. OpenAlex is the default
## everywhere else. See R/fetch_scholar.R.
##
## OpenAlex reports fewer citations than Scholar (about two thirds on the record
## this was developed against) but ranks papers almost identically. All three
## charts encode shape rather than level, so the default costs little.
NULL
