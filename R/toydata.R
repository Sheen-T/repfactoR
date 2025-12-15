#' Toy coffee preference dataset (replicated 2^3 factorial)
#'
#' A small teaching dataset used to demonstrate analysis of a replicated \eqn{2^3} factorial design.
#' The response is an overall liking score for coffee, measured for 16 respondents. The three
#' factors (\code{Milk}, \code{Packaging}, \code{Species}) each take two levels and are coded as
#' \eqn{-1} (low) and \eqn{+1} (high).
#'
#' @format A data frame with 16 rows and 5 variables:
#' \describe{
#'   \item{Respondent}{Integer respondent id.}
#'   \item{Overall_liking}{Numeric response (overall preference score).}
#'   \item{Milk}{Two-level factor coded \eqn{-1/+1}.}
#'   \item{Packaging}{Two-level factor coded \eqn{-1/+1}.}
#'   \item{Species}{Two-level factor coded \eqn{-1/+1}.}
#' }
#'
#' @details
#' This dataset mimics a replicated factorial design: multiple respondents provide responses
#' across the 8 design points (all combinations of three two-level factors). Replication enables
#' estimation of residual variance (pure error) and standard inference for effects.
#'
#' @examples
#' data(toydata)
#' head(toydata)
#'
#' @export
toydata <- data.frame(
  Respondent = 1:16,
  Overall_liking = c(39, 32, 28, 30, 17, 22, 20, 9, 40, 33, 24, 28, 14, 21, 17, 8),
  Milk = factor(c( 1,-1, 1, 1,-1,-1, 1,-1, 1,-1, 1, 1,-1,-1, 1,-1), levels = c(-1, 1)),
  Packaging = factor(c( 1, 1,-1, 1,-1, 1,-1,-1, 1, 1,-1, 1,-1, 1,-1,-1), levels = c(-1, 1)),
  Species = factor(c( 1, 1, 1, 1, 1,-1,-1,-1, 1, 1, 1,-1, 1,-1,-1,-1), levels = c(-1, 1))
)

toydata
