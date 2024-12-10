


#' @title Fractal Christmas tree.
#'
#' @description The tree fractal structure was adapted from
#'   (https://blogs.sas.com/content/iml/2012/12/14/a-fractal-christmas-tree.html).
#'   A preliminary translation to R was written by Alejandra Cabana (UAB).
#'
#' @param year Year to be printed. Default is \code{2025}.
#' @param language Language to be used in the card. One of \code{c("english", "spanish", "catalan")}. Default is \code{"english"}.
#' @return A Christmas card plot including a fractal Christmas tree.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmasfractaltree()
#' }
#' @export

xmasfractaltree <- function(year = 2025,
                            language = c("english", "spanish", "catalan")) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "language":
  language <- match.arg(language)

  # set the seed:
  set.seed(10)

  ### generate the first tree:
  treematrix1 <- fractaltree(npoints = 1e5)
  xtree1 <- 5 * treematrix1[1, ]
  ytree1 <- treematrix1[2, ]
  rm(treematrix1)

  ### generate the second tree:
  treematrix2 <- fractaltree(npoints = 1e5)
  xtree2 <- 5 * treematrix2[1, ]
  ytree2 <- treematrix2[2, ]
  rm(treematrix2)

  ### generate the third tree:
  treematrix3 <- fractaltree(npoints = 1e5)
  xtree3 <- 5 * treematrix3[1, ]
  ytree3 <- treematrix3[2, ]
  rm(treematrix3)

  # plot limits (increase tree width to fit with asp = 1):
  newwindow()
  xr <- range(xtree1, xtree2, xtree2) * 1.2
  yr <- range(ytree1, ytree2, ytree2) * c(1, 1.5)

  # set font type for the tree R:
  myvfont <- c("serif", "bold")

  # background:
  plot(xr, yr, type = "n", xlim = xr, ylim = yr, asp = 1, axes = FALSE, xlab = "", ylab = "")
  h0 <- 5
  polygon(c(xr[1], xr[1], xr[2], xr[2]), c(yr[1], h0, h0, yr[1]), border = NA, col = "azure2")
  polygon(c(xr[1], xr[1], xr[2], xr[2]), c(h0, yr[2], yr[2], h0), border = NA, col = "darkblue")

  # stars:
  u <- runif(500, xr[1], xr[2])
  v <- runif(500, yr[1] + 1.5 * h0, yr[2])
  points(u, v, pch = 8, lwd = 1, cex = 0.1, col = rainbow(180)[90])
  u <- runif(50, xr[1], xr[2])
  v <- runif(50, yr[1] + 1.1 * h0, yr[1] + 1.5 * h0)
  points(u, v, pch = 8, lwd = 1, cex = 0.1, col = rainbow(180)[90])

  # skyline:
  xaux <- seq(xr[1], xr[2], by = 0.01)
  lines(xaux, rnorm(length(xaux), h0, 0.05), type = "l", lwd = 3, col = "blue4")

  ### plot tree:
  greencol <- c("darkgreen", "darkolivegreen", "forestgreen")

  ### tree body 1:
  y1 <- 3
  xtree <- xtree1
  ytree <- ytree1 + y1
  points(x = xtree, y = ytree, col = greencol, cex = 0.15, pch = 18)
  rm(xtree, ytree)

  ### tree body 2:
  y2 <- 3.3
  xtree <- xtree2
  ytree <- ytree2 + y2
  points(x = xtree, y = ytree, col = greencol, cex = 0.15, pch = 18)
  rm(xtree, ytree)

  ### tree body 3:
  y3 <- 3.5
  xtree <- xtree3
  ytree <- ytree3 + y3
  points(x = xtree, y = ytree, col = greencol, cex = 0.15, pch = 18)

  # trunk:
  x <- c(-0.4, 0.4, 0, -0.4) + rnorm(4, 0, 0.015)
  ymaxtrunk <- max(ytree1, ytree2, ytree3) - 0.2
  y <- c(2.5, 2.5, ymaxtrunk, 2.5) + rnorm(4, 0, 0.015)
  polygon(x, y, border = NA, col = "brown")
  x <- runif(9, -0.28, 0.28)
  y <- runif(9, 2.8, 3.2)
  lines(x, y, type = "p", pch = "|", lwd = 2, col = "orangered4")
  # trunk basis:
  xaux <- seq(-0.4, 0.4, by = 0.01)
  lines(xaux, rnorm(length(xaux), 2.5, 0.05), type = "l", lwd = 3, col = "azure2")

  # add some green over the trunk:
  # green minimum height:
  ygmin <- min(ytree1, ytree2, ytree3) + 3.5
  ygmax <- ymaxtrunk
  xgmax <- 0.4 * (ygmax - ygmin) / (ygmax - 2.5)

  ngreen <- 15000
  xgreen <- runif(n = ngreen, min = -xgmax, max = xgmax)
  ygreen <- runif(n = ngreen, min = ygmin, max = ygmax)
  points(x = xgreen, y = ygreen, cex = 0.15, pch = 16:18, col = greencol)

  ### R tree:
  text(x = 0, y = max(ytree1, ytree2, ytree3) + 3.3, labels = "R", srt = 15, vfont = myvfont, cex = 3, col = "gold")

  ### tree balls:
  balls <- 80
  # ball colors:
  colors <- sample(x = c("maroon2", "maroon1", "maroon3", "maroon4"),
                   size = balls,
                   replace = TRUE)
  # ball positions:
  xball <- xtree
  xball <- xball + (1 - 2 * (xball > 0)) * 0.05
  yball <- ytree - 0.1
  tokeep <- yball > 4
  xball <- xball[tokeep]
  yball <- yball[tokeep]
  # location index:
  ballspoints <- sample(length(xball), balls)
  xball <- xball[ballspoints]
  yball <- yball[ballspoints]

  # ball sizes:
  cexmin <- 0.7
  cexmax <- 1.3
  ymin <- min(yball)
  ymax <- max(yball)
  b <- (cexmin - cexmax) / (ymax - ymin)
  a <- cexmin - b * ymax
  cexball <- a + b * yball
  # plot balls:
  points(x = xball, y = yball, col = colors, cex = cexball, pch = 19)

  # snow:
  u <- runif(400, xr[1], xr[2])
  v <- runif(400, yr[1], yr[2])
  points(u, v, pch = 8, cex = c(0.2, 0.3, 0.4), col = "white")

  # message:
  mess <- switch(language,
                 english = "Happy",
                 spanish = "Feliz",
                 catalan = "Bon")
  mess <- paste(mess, year)
  textcex <- 3
  Sys.sleep(0.4)
  text(x = 0, y = h <- (min(ytree) + yr[1]) / 2, mess, cex = textcex, font = 2, vfont = myvfont, col = "maroon4")
}
