#' Effect table for a replicated factorial fit
#'
#' Returns a tidy table of Yates-scaled effect estimates from a `"repfact_fit"` object,
#' including 95% confidence intervals.
#'
#' @param x A `"repfact_fit"` object returned by [fit_rep_factorial()].
#'
#' @return A data.frame with columns:
#' \describe{
#'   \item{term}{Effect/term name (e.g., `Milk`, `Milk:Packaging`, `grand.mean`).}
#'   \item{estimate}{Yates-scaled effect estimate (non-intercepts are `2 * coefficient`).}
#'   \item{ci_low}{Lower 95% CI bound.}
#'   \item{ci_high}{Upper 95% CI bound.}
#' }
#'
#' @examples
#' dat <- read.csv(system.file("extdata","coffee.csv", package = "repfactoR"))
#' fit <- fit_rep_factorial(
#'   toydata,
#'   response = "Overall_liking",
#'   factors  = c("Milk", "Packaging", "Species")
#' )
#' effects_table(fit)
#'
#' @export
effects_table <- function(x) {
  stopifnot(inherits(x, "repfact_fit"))

  eff <- x$effects
  nm <- names(eff)

  out <- data.frame(
    term = nm,
    estimate = as.numeric(eff),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  if (!is.null(x$effects_ci)) {
    ci <- as.data.frame(x$effects_ci, stringsAsFactors = FALSE)
    names(ci) <- c("ci_low", "ci_high")
    ci$term <- rownames(x$effects_ci)

    out <- merge(out, ci, by = "term", all.x = TRUE, sort = FALSE)
    out <- out[match(nm, out$term), ]
  }

  out
}
