#' @title Christmas Galton card.
#'
#' @description Christmas Galton card (2018). A Christmas tree in the Galton
#'   board. This is an adaptation of \code{quincunx()} in package
#'   \code{animation}, which simulates the Galton board
#'   (\url{https://en.wikipedia.org/wiki/Bean_machine}).
#'
#' @param year Year to be printed. Default is \code{2019}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param balls The number of balls in the board. Default is 240.
#' @param layers The number of layers in the board. Default is 15.
#' @param onlyBoard Logical. If \code{FALSE} (default), an empirical Christmas
#'   normal tree estimated from data is added to the histogram.
#' @param treeballs The number of balls to be added to the Christmas normal
#'   tree. Default is 15.
#' @param time Elapsed time, in seconds, between two consecutive balls in the
#'   board. Default is 0.02.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas tree in a Galton board.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmasgalton()
#' # more balls, less layers:
#' xmasgalton(balls = 300, layers = 9)
#' # no balls in the gaussian christmas tree:
#' xmasgalton(balls = 400, layers = 11, time = 0.005, treeballs = 0)
#' }
#' @export
#' @import animation

xmasgalton <- function(year = 2019,
                       language = c("english", "spanish", "catalan"),
                       balls = 240,
                       layers = 15,
                       onlyBoard = FALSE,
                       treeballs = 15,
                       time = 0.02,
                       seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "language":
  language <- match.arg(language)
  # "balls":
  if (!inherits(balls, c("numeric", "integer")) || length(balls) != 1L || balls <= 0)
    stop("'balls' must be a positive number")
  # "layers":
  if (!inherits(layers, c("numeric", "integer")) || length(layers) != 1L || layers <= 0)
    stop("'layers' must be a positive number")
  # "treeballs":
  if (!inherits(treeballs, c("numeric", "integer")) || length(treeballs) != 1L || treeballs == 0)
    stop("'treeballs' must be a non negative number")
  # "time":
  if (!inherits(time, c("numeric", "integer")) || length(time) != 1L || time <= 0)
    stop("'time' must be a positive number")
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)

   pch.balls <- 19
   cex.balls <- 2
   pch.layers <- 2
   col.balls <- rainbow(balls)
   nsim <- 10^6
   nmax <- balls + layers - 2
   newwindow()
   oopt <- ani.options(nmax = ifelse(interactive(), balls + layers - 2, 2), interval = time)
   op <- par(mar = c(1, 0.1, 0.1, 0.1), mfrow = c(2, 1))
   on.exit(par(op))
   layerx <- layery <- NULL
   for (i in 1:layers) {
   	layerx <- c(layerx, seq(0.5 * (i + 1), layers - 0.5 * (i - 1), 1))
   	layery <- c(layery, rep(i, layers - i + 1))
    }
    ballx <- bally <- matrix(nrow = balls, ncol = nmax)
    finalx <- numeric(balls)
    for (i in 1:balls) {
        ballx[i, i] <- (1 + layers) / 2
        if (layers > 2) {
            tmp <- rbinom(layers - 2, 1, 0.5) * 2 - 1
            ballx[i, i + 1:(layers - 2)] <- cumsum(tmp) * 0.5 + (1 + layers) / 2
        }
        bally[i, (i - 1) + 1:(layers - 1)] <- (layers - 1):1
        finalx[i] <- ballx[i, i + layers - 2]
    }
   rgx <- c(1, layers)
   rgy <- c(0, max(table(finalx)))
   for (i in 1:ani.options("nmax")) {
   	dev.hold()
    plot(1:layers, type = "n", ann = FALSE, axes = FALSE)
    points(layerx, layery, pch = pch.layers)
    points(ballx[, i], bally[, i], pch = pch.balls, col = col.balls, cex = cex.balls)
    par(bty = "u")
    if (i < layers - 1)
     plot.new()
        else {
            hist(finalx[1:(i - layers + 2)], breaks = 1:layers, col = "forestgreen",
                xlim = rgx, ylim = rgy, main = "", xlab = "",
                ylab = "", ann = FALSE, axes = FALSE)
        }
        ani.pause()
    }
   if (!onlyBoard) {
   invisible(c(table(finalx)))
   freq <- table(finalx)
   samp <- rep(as.numeric(names(freq)), freq)
   m <- mean(samp)
   s <- sd(samp)
   dens <- density(rnorm(nsim, m, s))
   dens$y <- dens$y * max(freq) / max(dens$y)
   Sys.sleep(1)
   lines(dens, col = "white", lwd = 2)
   if (treeballs > 0) {
   	xy <- data.frame(x = dens$x, y = dens$y)
    tokeep <- xy$y >= min(dens$y) + 0.15 * (max(dens$y) - min(dens$y))
    xy <- xy[tokeep, ]
    nxy <- dim(xy)[1]
    treeballs <- min(treeballs, nxy)
    d <- floor((nxy - 1) / (treeballs - 1))
    wb <- 1 + (0:(d + 1)) * (treeballs - 1)
    xy <- xy[wb, ]
    sizes <- runif(treeballs, 2, 3)
    for (i in 1:treeballs) {
      Sys.sleep(0.2)
      points(xy$x[i], xy$y[i], col = "red", pch = 19, cex = sizes[i])
    }
   }
  ypos <- min(dens$y) + 0.7 * (max(dens$y) - min(dens$y))
  myvfont <- c("serif", "bold")
  Sys.sleep(1)
  xw <- (1 + layers) / 2
  mycex <- 3 * xw / 8
  mess <- switch(language,
                 english = "Happy",
                 spanish = "Feliz",
                 catalan = "Bon")
  text(0.875 * xw / 2, ypos, mess, col = "forestgreen", cex = mycex, vfont = myvfont)
  Sys.sleep(1)
  text(2 * xw - 0.875 * xw / 2, ypos, paste0(year, "!"), col = "forestgreen", cex = mycex, vfont = myvfont)
  }
 }
