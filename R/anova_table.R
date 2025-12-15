?fit_rep_factorial#' ANOVA-style coefficient summary for a replicated factorial fit
#'
#' Provides coefficient estimates from the fitted linear model (SE, t, p-value) and adds
#' an `effect_estimate` column where non-intercept terms are scaled by 2 to match the
#' factorial **effect** scale (Yates scaling).
#'
#' @param x A `"repfact_fit"` object returned by [fit_rep_factorial()].
#'
#' @return A data.frame with columns `term`, `estimate`, `std.error`, `t.value`, `p.value`,
#' and `effect_estimate`.
#'
#' @examples
#' dat <- read.csv(system.file("extdata","coffee.csv", package = "repfactoR"))
#' fit <- fit_rep_factorial(
#'   toydata,
#'   response = "Overall_liking",
#'   factors  = c("Milk", "Packaging", "Species")
#' )
#' anova_table(fit)
#'
#' @export
anova_table <- function(x) {
  stopifnot(inherits(x, "repfact_fit"))

  # `summary()` is a base generic; avoid `stats::summary()` because `summary`
  # is not exported from the stats namespace.
  sm <- summary(x$fit)
  co <- sm$coefficients

  out <- data.frame(
    term = rownames(co),
    estimate = co[, 1],
    std.error = co[, 2],
    t.value = co[, 3],
    p.value = co[, 4],
    stringsAsFactors = FALSE,
    row.names = NULL,
    check.names = FALSE
  )

  out$effect_estimate <- out$estimate
  idx <- out$term != "(Intercept)"
  out$effect_estimate[idx] <- 2 * out$estimate[idx]

  out$term[out$term == "(Intercept)"] <- "grand.mean"
  out
}
