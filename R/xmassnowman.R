#' @title Christmas snowman.
#'
#' @description Christmas snowman (2014 card).
#'
#' @param year Year to be printed. Default is \code{2015}.
#' @param language Language to be used in the card. One of \code{c("english", "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no seed).
#' @return A Christmas card plot including a snowman.
#' @author Jose Barrera-Gomez.
#' @examples
 #' \donttest{
#' xmassnowman(year = 2015, language = "catalan")
#' }
#' @export

xmassnowman <- function(year = 2015,
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
  r <- 0.6
  t <- 0.4
  # background:
  newwindow()
  Sys.sleep(0.5 * t)
  xmin <- -10
  xmax <- 6
  ymin <- -5
  ymax <- 10
  np <- 2000
  u <- runif(np, xmin, xmax)
  v <- runif(np, ymin, ymax)
  op <- par(family = "HersheySerif")
  on.exit(par(op))
  #  par(family = "HersheySerif")
  op
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", asp = 1, axes = F, xlab = "", ylab = "")
  h <- ymin + (ymax - ymin) / 3
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, h, h, ymin), border = NA, col = "azure2")
  polygon(c(xmin, xmin, xmax, xmax), c(h, ymax, ymax, h), border = NA, col = "darkblue")
  d <- (xmax - xmin) / 100
  x0 <- seq(xmin + d, xmax - d, by = 0.01)
  lines(x0, h + rnorm(length(x0), 0, 0.05), type = "l", lwd = 3, col = "blue4")
  # body:
  Sys.sleep(t)
  theta <- seq(0, 2 * pi, 0.05)
  r <- 2.5
  ns <- 200
  x <- jitter(1.1 * r * cos(theta), ns)
  y <- jitter(r * sin(theta), ns)
  polygon(x, y, border = "gray", col = "white")
  h <- -2.2
  Sys.sleep(t)
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, h, h, ymin), border = NA, col = "azure2")
  lines(x0, h + rnorm(length(x0), 0, 0.05), type = "l", lwd = 3, col = "azure2")
  # head:
  Sys.sleep(t)
  r <- 1.65
  x <- jitter(1.1 * r * cos(theta), ns)
  y <- jitter(3.35 + r * sin(theta), ns)
  polygon(x, y, border = "gray", col = "white")
  h <- -2.2
  Sys.sleep(t)
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, h, h, ymin), border = NA, col = "azure2")
  # scarf:
  d <- 0.25
  theta <- seq(5 * pi / 4 - d, 7 * pi / 4 + d, 0.05)
  r1 <- 2
  r2 <- 1.8
  x <- r1 * cos(theta)
  y1 <- 3 + r1 * sin(theta)
  y2 <- 3.7 + r2 * sin(theta)
  ns <- 15
  Sys.sleep(t)
  polygon(jitter(c(x, -x), ns), jitter(c(y1, y2), ns), border = NA, col = "red")
  l <- 20
  t <- seq(0, 1, length.out = l)
  x1 <- 0.2 + t
  x2 <- rep(1.2, l)
  x3 <- 1.2 - t
  x4 <- rep(0.2, l)
  x <- c(x1, x2, x3, x4)
  y1 <- 0.8 + 0.2 * t
  y2 <- 1 + 1.4 * t
  y3 <- 2.4 - 0.2 * t
  y4 <- 2.2 - 1.4 * t
  y <- c(y1, y2, y3, y4)
  ns <- 5
  Sys.sleep(t)
  polygon(jitter(x, ns), jitter(y, ns), border = NA, col = "red")
  # scarf ties:
  x <- c(0, 0.2, 0.5, 0.7)
  y <- c(0, 0.9, 1, -0.3)
  Sys.sleep(t)
  polygon(x, y, border = NA, col = "red")
  x <- c(0.8, 0.9, 1.2, 1.4)
  y <- c(-0.5, 1, 1.1, -0.7)
  Sys.sleep(t)
  polygon(x, y, border = NA, col = "red")
  # buttons:
  theta <- seq(0, 2 * pi, 0.05)
  r <- 0.25
  x <- -0.6 + r * cos(theta)
  y <- -1.1 + r * sin(theta)
  h <- 0.8
  Sys.sleep(0.7 * t)
  polygon(x, y, border = "gray", col = "black")
  Sys.sleep(0.7 * t)
  polygon(x + 0.05, y + h, border = "gray", col = "black")
  Sys.sleep(0.7 * t)
  polygon(x + 0.15, y + 2 * h, border = "gray", col = "black")
  # hat:
  x <- c(-1.9, 2, 2.02, -1.88)
  y <- c(4.5, 4.0, 4.2, 4.7)
  Sys.sleep(t)
  polygon(x, y, col = "black")
  x <- c(-1.4, 1.6, 1.8, -0.9)
  y <- c(4.6, 4.2, 6, 6.3)
  Sys.sleep(t)
  polygon(x, y, col = "black")
  x <- c(-1.45, 1.67, 1.7, -1.3)
  y <- c(4.6, 4.2, 4.7, 5.1)
  Sys.sleep(t)
  polygon(x, y, col = "red")
  # right arm:
  Sys.sleep(t)
  x <- c(-2.5, -5)
  y <- c(1, 2)
  lines(x, y, lwd = 6, col = "darkorange4")
  x <- c(-4.2, -4.6)
  y <- c(1.75, 2.2)
  lines(x, y, lwd = 6, col = "darkorange4")
  x <- c(-4.2, -4.7)
  y <- c(1.75, 1.6)
  lines(x, y, lwd = 6, col = "darkorange4")
  # left arm:
  Sys.sleep(t)
  x <- c(2, 4)
  y <- c(0.5, 1.5)
  lines(x, y, lwd = 6, col = "darkorange4")
  x <- c(3.5, 3.6)
  y <- c(1.3, 1.7)
  lines(x, y, lwd = 6, col = "darkorange4")
  x <- c(3.5, 3.65)
  y <- c(1.25, 1.1)
  lines(x, y, lwd = 6, col = "darkorange4")
  # mouth:
  Sys.sleep(t)
  n <- 8
  theta <- seq(210, 320, length.out = n) * pi / 180
  r <- 0.75
  x <- -0.1 + r * cos(theta)
  y <- 3.25 + r * sin(theta)
  points(x, y, pch = 19, cex = 0.3)
  # nose:
  Sys.sleep(t)
  x0 <- -0.1
  y0 <- 3.2
  points(x0, y0, pch = 19, cex = 2, col = "orange")
  x <- c(-1.5, x0, x0)
  d <- 0.28
  y <- c(3, y0 - d, y0 + d)
  polygon(x, y, border = NA, col = "orange")
  segments(-0.2, 3.3, -0.1, 3, lwd = 1, col = "darkorange4")
  segments(-0.4, 3.2, -0.3, 3.1, lwd = 1, col = "darkorange4")
  segments(-0.7, 3.1, -0.65, 3, lwd = 1, col = "darkorange4")
  segments(-1, 3.1, -0.95, 3, lwd = 1, col = "darkorange4")
  # eyes:
  Sys.sleep(t)
  points(-0.5, 3.8, pch = 19, cex = 1.5)
  points(-0.6, 3.8, pch = 19, cex = 0.4, col = "white")
  Sys.sleep(0.4 * t)
  points(0.4, 3.7, pch = 19, cex = 1.5)
  points(0.3, 3.7, pch = 19, cex = 0.4, col = "white")
  # snow:
  Sys.sleep(t)
  np <- 500
  points(u[1:np], v[1:np], pch = 8, lwd = 1, cex = 0.1, col = rainbow(180)[90])
  # message:
  mess <- switch(language,
                 english = "HAPPY...",
                 spanish = "FELIZ...",
                 catalan = "BON...")
  year <- unlist(strsplit(paste0(year, "!"), ""))
  t <- 1
  Sys.sleep(t)
  x0 <- switch(language,
               english = -4.4,
               spanish = -4.4,
               catalan = -3.8)
  x0 <- -4.4
  h <- -4
  text(x0, h, mess, cex = 2.5, font = 2, col = "forestgreen")
  d <- 0.8
  for (i in 1:length(year)) {
    Sys.sleep(0.5 * t)
    text(x0 + 4 + (i - 1) * d, h, year[i], cex = 2.5, font = 2, col = "forestgreen")
  }
}
