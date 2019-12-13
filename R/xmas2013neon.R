#' @title Christmas card 2013.
#'
#' @description Christmas card 2013: a neon sign.
#'
#' @param language Language to be used in the card. One of \code{c("english", "spanish", "catalan")}. Default is \code{"english"}.
#' @param nflash Number of flashes shown in the neon sign. Default is 30.
#' @param pause Pause, in seconds, between two consecutive flashes shown in the neon sign. Default is 0.1.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no seed).
#' @return A Christmas card plot including a message and then a simulations of a neon sign.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmas2013neon()
#' xmas2013neon(language = "catalan", nflash = 10, pause = 0.3)
#' xmas2013neon(language = "spanish", nflash = 8, pause = 1)
#' xmas2013neon(nflash = 100, pause = 0.02)
#' }
#' @export

xmas2013neon <- function(language = c("english", "spanish", "catalan"), nflash = 30, pause = 0.1, seed = NULL) {
  # "language":
  language <- match.arg(language)
  # "nflash":
  if (!inherits(nflash, c("numeric", "integer")) || nflash < 1 || length(nflash) != 1L)
    stop("'nflash' must be a positive number")
  # "pause":
  if (!inherits(pause, c("numeric", "integer")) || pause < 0 || length(pause) != 1L)
    stop("'pause' must be a positive number")
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)
  d <- expand.grid(x = 1:20, y = 1:9)
  d$front <- 0
  r3 <- d$y == 3 & d$x %in% c(4:6, 8:10, 12:14, 17)
  r4 <- d$y == 4 & d$x %in% c(4, 8, 10, 13, 17)
  r5 <- d$y == 5 & d$x %in% c(4:6, 8, 10, 13, 15:17)
  r6 <- d$y == 6 & d$x %in% c(6, 8, 10, 12:13, 15, 17)
  r7 <- d$y == 7 & d$x %in% c(4:6, 8:10, 13, 15, 17)
  r <- r3 | r4 | r5 | r6 | r7
  d$front[r] <- 1
  newwindow()
  plot(c(1, 20), c(1, 9), type = "n", axes = F, xlab = "", ylab = "")
  # message:
  ms <- switch(language,
                 english = c("I ", "wish ", "you ", "a ", "fun..."),
                 spanish = c("Te ", "deseo ", "un ", "gran..."),
                 catalan = c("Et ", "desitjo ", "un ", "gran..."))
  lms <- length(ms)
  x <- runif(lms, min = 3, max = 18)
  y <- runif(lms, min = 2, max = 8)
  col.text <- sample(c("red", "green", "yellow", "blue", "brown"), lms)
  for (i in 1:lms)
   {
   	Sys.sleep(0.3)
    points(x[i], y[i], pch = 19, cex = 20, col = "cyan")
   	Sys.sleep(0.5)
   	text(x[i], y[i], ms[i], col = col.text[i], cex = 3, font = 1)
  }
  Sys.sleep(1.5)
  for (i in 1:nflash)
   {
   	cols <- sample(c(2:4, 6:7), 2)
    col <- cols[1] + (cols[2] - cols[1]) * d$front
    pchf <- sample(c(15, 19), 1)
    pchb <- sample(c(3, 4, 8), 1)
    pch <- pchb + (pchf - pchb) * d$front
    plot(d$x, d$y, col = col, pch = pch, cex = 4.5, axes = F, xlab = "", ylab = "")
    Sys.sleep(pause)
   }
}


# Usage:
#
# nflash: number of flashes
# pause: elapsed time between two pictures (seconds)
#
# Examples:
#

