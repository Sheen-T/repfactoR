#' Fit a replicated 2^k factorial model with all interactions
#'
#' Fits a full factorial linear model (main effects + all interactions) for a design with
#' two-level factors. Factors are expected to be coded as \eqn{-1} and \eqn{+1}.
#' If a factor is coded as \eqn{0/1} or as two text levels, the function attempts to
#' recode it to \eqn{-1/+1}.
#'
#' For balanced \eqn{2^k} designs using \eqn{-1/+1} coding, each non-intercept coefficient
#' from the fitted model corresponds to half of the classical factorial *effect*. This
#' function therefore reports **Yates-scaled effects** as:
#' \deqn{\widehat{\text{effect}} = 2 \times \widehat{\beta}}
#' for each non-intercept term.
#'
#' Replication (multiple observations per design point) provides an estimate of pure error,
#' enabling standard errors, confidence intervals, and t-tests in the usual linear model summary.
#'
#' @param data A data.frame containing the response column and factor columns.
#' @param response Character scalar; the name of the response column.
#' @param factors Character vector; names of the two-level factor columns.
#'
#' @return An object of class `"repfact_fit"`: a list with components
#' \describe{
#'   \item{data}{The input data with factor columns standardized to \eqn{-1/+1} factors.}
#'   \item{formula}{The model formula used (e.g., `y ~ A*B*C`).}
#'   \item{fit}{The underlying `lm` fit.}
#'   \item{effects}{Named numeric vector of Yates-scaled effect estimates. The intercept is labeled `grand.mean`.}
#'   \item{effects_ci}{A two-column matrix of 95% confidence intervals for effects (scaled consistently).}
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
fit_rep_factorial <- function(data, response, factors) {
  stopifnot(is.data.frame(data), length(response) == 1, is.character(response))
  stopifnot(is.character(factors), length(factors) >= 1)

  # Local helper (kept inside this function so exported surface stays minimal)
  ensure_pm1 <- function(df, facs) {
    out <- df
    for (f in facs) {
      x <- out[[f]]

      # Coerce character to factor
      if (is.character(x)) x <- factor(x)

      if (is.factor(x)) {
        lev <- levels(x)

        # If levels are already "-1" and "1", standardize to numeric then factor(-1,1)
        if (all(lev %in% c("-1", "1"))) {
          xnum <- as.integer(as.character(x))
          x <- factor(xnum, levels = c(-1, 1))
        } else if (length(lev) == 2) {
          # Map first level -> -1, second level -> +1
          x <- factor(ifelse(x == lev[1], -1, 1), levels = c(-1, 1))
        } else {
          stop(sprintf("Factor '%s' must have exactly 2 levels (found %d).", f, length(lev)))
        }
      } else if (is.numeric(x) || is.integer(x) || is.logical(x)) {
        xnum <- as.numeric(x)
        u <- sort(unique(xnum[!is.na(xnum)]))
        if (all(u %in% c(-1, 1))) {
          x <- factor(ifelse(xnum < 0, -1, 1), levels = c(-1, 1))
        } else if (all(u %in% c(0, 1))) {
          x <- factor(ifelse(xnum == 0, -1, 1), levels = c(-1, 1))
        } else {
          stop(sprintf("Numeric factor '%s' must be coded -1/+1 or 0/1.", f))
        }
      } else {
        stop(sprintf("Unsupported type for factor '%s'.", f))
      }

      out[[f]] <- x
    }
    out
  }

  df <- ensure_pm1(data, factors)

  rhs <- paste(factors, collapse = "*")
  fmla <- stats::as.formula(paste(response, "~", rhs))
  fit <- stats::lm(fmla, data = df)

  cf <- stats::coef(fit)
  effects <- cf
  if ("(Intercept)" %in% names(effects)) {
    idx <- names(effects) != "(Intercept)"
    effects[idx] <- 2 * effects[idx]
    names(effects)[names(effects) == "(Intercept)"] <- "grand.mean"
  }

  ci <- stats::confint(fit)
  # Scale non-intercept CI by 2; rename intercept to grand.mean
  ci_eff <- ci
  if ("(Intercept)" %in% rownames(ci_eff)) {
    idx <- rownames(ci_eff) != "(Intercept)"
    ci_eff[idx, ] <- 2 * ci_eff[idx, , drop = FALSE]
    rownames(ci_eff)[rownames(ci_eff) == "(Intercept)"] <- "grand.mean"
  } else {
    ci_eff <- 2 * ci_eff
  }

  res <- list(
    data = df,
    formula = fmla,
    fit = fit,
    effects = effects,
    effects_ci = ci_eff
  )
  class(res) <- "repfact_fit"
  res
}
