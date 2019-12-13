#' @title Christmas card 2008.
#'
#' @description Christmas card 2008: a random Christmas tree.
#'
#' @param year Year to be printed. Default is \code{2009}.
#' @param language Language to be used in the card. One of \code{c("english", "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no seed).
#' @return A Christmas card plot including a random tree.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmas2008tree()
#' xmas2008tree(year = 2020, language = "catalan")
#' xmas2008tree(year = 2020, language = "spanish", seed = 666)
#' }
#' @export


xmas2008tree <- function(year = 2009, language = c("english", "spanish", "catalan"), seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "language":
  language <- match.arg(language)
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed))
    set.seed(seed)

  r <- 0.6
  q <- 0.6
  t <- 0.05
  n <- 500
  if (!is.null(seed)) set.seed(seed)
  newwindow()
  Sys.sleep(0.5)
  x <- rnorm(4 * n, 0, q * (1 + r + r^2))
  y <- rnorm(4 * n, 1 + r + r^2, q * (1 + r + r^2))
  plot(x[1:n], y[1:n], pch = 8, lwd = 1, cex = 0.5, col = rainbow(180)[90], xlim = c(-1.5, 1.5), ylim = c(-r^2, 3 * (1 + r + r^2)), asp = 0.6, axes = F, xlab = "", ylab = "")
  Sys.sleep(1)
  x1 <- (r^2 / 4) * c(-1, 1, 1, -1) + rnorm(4, 0, t / 2)
  y1 <- 6 * (r^2 / 4) *c(0, 0, -1, -1) + rnorm(4, 0, t / 2)
  polygon(x1, y1, border = NA, col = "brown")
  x2 <- c(-1, 1, 0, r, 0, r^2, 0, -r^2, 0, -r, 0, -1) + rnorm(12, 0, t)
  y2 <- 2*c(0, 0, 1, 1, 1 + r, 1 + r, 1 + r + r^2, 1 + r, 1 + r, 1, 1, 0) + rnorm(12, 0, t)
  Sys.sleep(1)
  polygon(x2, y2, border = NA, col = rainbow(180)[50])
  Sys.sleep(1)
  mess <- switch(language,
                 english = "Happy ",
                 spanish = "Feliz ",
                 catalan = "Bon ")
  text(0, 1.2 * max(y2), paste0(mess, year, "!"), col = floor(runif(1, 2, 8)), cex = 2.5, font = 2)
  Sys.sleep(0.5)
  lines(x[( n +1):(4 * n)], y[(n + 1):(4 * n)], type = "p", pch = 8, lwd = 1, cex = 0.6, col = "white")
 }
