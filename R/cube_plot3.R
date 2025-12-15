#' Cube plot for a 3-factor replicated factorial fit
#'
#' Draws a simple cube-style visualization for designs with exactly three two-level factors.
#' The plot shows the 8 model-predicted means at the vertices corresponding to the \eqn{(-1,+1)^3}
#' factor combinations. This function does not require a specialized design object.
#'
#' @param x A `"repfact_fit"` object returned by [fit_rep_factorial()].
#'
#' @return Invisibly returns a data.frame of the 8 factor combinations and predicted means.
#'
#' @examples
#' dat <- read.csv(system.file("extdata","coffee.csv", package = "repfactoR"))
#' fit <- fit_rep_factorial(
#'   toydata,
#'   response = "Overall_liking",
#'   factors  = c("Milk", "Packaging", "Species")
#' )
#' cube_plot3(fit)
#'
#' @export
cube_plot3 <- function(x) {
  stopifnot(inherits(x, "repfact_fit"))

  factors <- all.vars(stats::terms(x$formula))[-1]
  if (length(factors) != 3) stop("cube_plot3 requires exactly 3 factors.")

  lev <- c(-1, 1)
  grid_list <- stats::setNames(rep(list(lev), 3), factors)
  nd <- expand.grid(grid_list, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  # Match factor structure used in the fitted data (coded -1/+1 as factors)
  for (f in factors) nd[[f]] <- factor(nd[[f]], levels = c(-1, 1))

  nd$pred <- as.numeric(stats::predict(x$fit, newdata = nd))

  A <- factors[1]; B <- factors[2]; C <- factors[3]

  # Map to 2D cube layout: front square = C=-1, back square = C=+1 (shifted)
  shift <- 0.35
  vertex_xy <- function(a, b, depth) {
    x0 <- ifelse(a == -1, 0, 1)
    y0 <- ifelse(b == -1, 0, 1)
    if (depth == 1) { x0 <- x0 + shift; y0 <- y0 + shift }
    c(x0, y0)
  }

  graphics::plot(
    NA, xlim = c(-0.1, 1.55), ylim = c(-0.1, 1.55),
    axes = FALSE, xlab = "", ylab = "",
    main = "Cube Plot (model-predicted means)"
  )

  sq1 <- matrix(c(0,0, 1,0, 1,1, 0,1, 0,0), ncol = 2, byrow = TRUE)
  sq2 <- sq1 + shift

  graphics::lines(sq1[,1], sq1[,2])
  graphics::lines(sq2[,1], sq2[,2])
  for (i in 1:4) graphics::lines(c(sq1[i,1], sq2[i,1]), c(sq1[i,2], sq2[i,2]))

  # Use ASCII-only labels for portability under R CMD check
  graphics::text(0.5, -0.08, paste0(A, ": -1 -> +1"))
  graphics::text(-0.08, 0.5, paste0(B, ": -1 -> +1"), srt = 90)
  graphics::text(1.25, 1.35, paste0(C, ": -1 (front) / +1 (back)"))

  for (i in seq_len(nrow(nd))) {
    a <- as.integer(as.character(nd[[A]][i]))
    b <- as.integer(as.character(nd[[B]][i]))
    cval <- as.integer(as.character(nd[[C]][i]))
    depth <- ifelse(cval == -1, 0, 1)

    xy <- vertex_xy(a, b, depth)
    graphics::points(xy[1], xy[2], pch = 19)
    graphics::text(
      xy[1], xy[2],
      labels = format(round(nd$pred[i], 2), nsmall = 2),
      pos = 3, cex = 0.8
    )
  }

  invisible(nd)
}
