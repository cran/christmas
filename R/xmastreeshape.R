#' @title Christmas shaped tree.
#'
#' @description Christmas trees (2017 card) with different shapes.
#'
#' @param year Year to be printed. Default is \code{2018}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param shape The shape of the tree. One of \code{c("piramidal", "oval",
#'   "vshaped", "round", "columnar")}. Default is \code{"piramidal"}.
#' @param nballs The number of balls in the tree. Default is 15.
#' @param ballscolor The colors to be used for the balls in the tree. It must be
#'   a vector with names of colors included in \code{colors()}, or \code{NULL}
#'   (default). If \code{NULL}, then colors are randomly selected.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot including a christmas tree.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmastreeshape()
#' xmastreeshape(shape = "oval", language = "catalan", ballscolor = "blue")
#' xmastreeshape(shape = "vshaped", nballs = 15, ballscolor = c("sienna2", "yellow2", "tomato"),
#'               seed = 1111)
#' xmastreeshape(shape = "round", language = "spanish")
#' xmastreeshape(shape = "columnar", nballs = 20, ballscolor = "red")
#' }
#' @export

xmastreeshape <- function(year = 2018,
                          language = c("english", "spanish", "catalan"),
                          shape = c("piramidal", "oval", "vshaped", "round", "columnar"),
                          nballs = 15,
                          ballscolor = NULL,
                          seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "language":
  language <- match.arg(language)
  # "shape":
  shape <- match.arg(shape)
  # "nballs":
  if (!inherits(nballs, c("numeric", "integer")) || length(nballs) != 1L || nballs <= 0)
    stop("'nballs' must be a non negative number")
  # "ballscolor":
  if(!is.null(ballscolor) & (is.na(ballscolor) || !all(ballscolor %in% colors())))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed))
    set.seed(seed)

  # Background:
  newwindow()
  xmin <- -3
  xmax <- 3
  ymin <- -2
  ymax <- 4
  np <- 1000
  u <- runif(np, xmin, xmax)
  v <- runif(np, ymin, ymax)
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", asp = 1, axes = F, xlab = "", ylab = "")
  h <- - 0.5
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, h, h, ymin), border = NA, col = "azure2")
  polygon(c(xmin, xmin, xmax, xmax), c(h, ymax, ymax, h), border = NA, col = "darkblue")
  d <- (xmax - xmin) / 100
  x0 <- seq(xmin + d, xmax - d, by = 0.01)
  lines(x0, h + rnorm(length(x0), 0, 0.05), type = "l", lwd = 3, col = "blue4")
  ##############
  plottrunk()
  ##############
  if (is.null(ballscolor))
   ballscolor <- c("cornflowerblue", "blue", "darkgoldenrod1", "darkmagenta", "yellow", "violet", "red", "darkorchid1", "gold1")
  switch(shape,
         piramidal = plotTreePiramidal(nballs = nballs, ballscolor = ballscolor),
         oval = plotTreeOval(nballs = nballs, ballscolor = ballscolor),
         vshaped = plotTreeVshaped(nballs = nballs, ballscolor = ballscolor),
         round = plotTreeRound(nballs = nballs, ballscolor = ballscolor),
         columnar = plotTreeColumnar(nballs = nballs, ballscolor = ballscolor))
  ##############
  for(i in 1:40)
   {
    snow(np = 20, x0 = xmin, x1 = xmax, y0 = ymin, y1 = ymax)
    Sys.sleep(0.1)
   }
  ##############
  Sys.sleep(0.5)
  mess <- switch(language,
                 english = "Happy ",
                 spanish = "Feliz ",
                 catalan = "Bon ")
  myvfont <- c("serif", "bold")
  text(x = 0, y = 3, labels = paste0(mess, year, "!"), vfont = myvfont, cex = 2, col = "red")
  ##############
  Sys.sleep(1)
  text(x = 0, y = 2.1, labels = "R", srt = 15, vfont = myvfont, cex = 3, col = "gold")
 }
