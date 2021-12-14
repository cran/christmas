#' @title Christmas flag.
#'
#' @description Christmas flag (2015 card). A flag with a Christmas message.
#'
#' @param year Year to be printed. Default is \code{2016}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot including a message in a flag.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmasflag()
#' xmasflag(year = 2020, language = "catalan")
#' xmasflag(year = 2020, language = "spanish")
#' }
#' @export

xmasflag <- function(year = 2016,
                     language = c("english", "spanish", "catalan"),
                     seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "language":
  language <- match.arg(language)
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)
  # Background:
  newwindow()
  xmin <- -10
  xmax <- 6
  ymin <- -5
  ymax <- 10
  op <- par(family = "HersheySerif")
  on.exit(par(op))
  #  par(family = "HersheySerif")
  op
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", asp = 1, axes = F, xlab = "", ylab = "")
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, ymax, ymax, ymin), border = NA, col = "azure2")
  snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
  Sys.sleep(1)
  polygon(c(-4, 4, 4, -4), c(1.6, 1.6, 8, 8), border = "forestgreen", col = "forestgreen", lwd = 2)
  snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
  n <- 100
  a1 <- 0
  an <- 10 * 360 + 90
  B <- (an - a1) / (n - 1)
  A <- a1 - B
  as <- A + B * (1:n)
  l1 <- 0.5
  ln <- 5
  B <- (ln - l1) / (n - 1)
  A <- l1 - B
  ls <- A + B * (1:n)
  t <- 0.03
  Sys.sleep(1)
  sapply(1:(n - 1), FUN = function(i) { bar(theta = as[i], l = ls[i], col = "forestgreen", lwd = 7)
                                        Sys.sleep(t)
                                        bar(theta = as[i], l = ls[i], col = "azure2", lwd = 8)
                                        snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax) })
  bar(theta = as[n], l = ls[n], col = "forestgreen", lwd = 7)
  Sys.sleep(t)
  snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
  polygon(c(-4, 4, 4, -4), c(1.6, 1.6, 8, 8), border = "forestgreen", col = "white", lwd = 7)
  snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
  t <- 0.2
  year <- unlist(strsplit(as.character(year), ""))
  year <- c(year, "!")
  d <- 0.8
  x1 <- 0
  y1 <- 6
  x2 <- -1.5
  y2 <- 4
  Sys.sleep(1)
  mess <- switch(language,
                 english = "HAPPY...",
                 spanish = "FELIZ...",
                 catalan = "BON...")
  text(x1, y1, mess, cex = 2.5, font = 2, col = "forestgreen")
  for (i in 1:length(year))
   {
    Sys.sleep(t)
    snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
    text(x2 + (i - 1) * d, y2, year[i], cex = 2.5, font = 2, col = "forestgreen")
   }
  snow(x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
}
