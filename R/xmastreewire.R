#' @title Wire Christmas tree.
#'
#' @description A random wire Christmas tree (2021 card).
#'
#' @param year Year to be printed. Default is \code{2022}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot including a random wire tree.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmastreewire(year = 2020, language = "catalan", seed = 666)
#' }
#' @export

xmastreewire <- function (year = 2022,
                          language = c("english", "spanish", "catalan"),
                          seed = NULL) {
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  language <- match.arg(language)
  if (!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed))
    set.seed(seed)
  r <- 0.6
  t <- 0.8
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
  op
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", asp = 1, axes = F,
       xlab = "", ylab = "")
  h <- ymin + (ymax - ymin)/3
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, h, h, ymin), border = NA,
          col = "azure2")
  polygon(c(xmin, xmin, xmax, xmax), c(h, ymax, ymax, h), border = NA,
          col = "darkblue")
  d <- (xmax - xmin)/100
  x0 <- seq(xmin + d, xmax - d, by = 0.01)
  lines(x0, h + rnorm(length(x0), 0, 0.05), type = "l", lwd = 3,
        col = "blue4")
  Sys.sleep(t)
  h <- -2.2
  polygon(c(xmin, xmin, xmax, xmax), c(ymin, h, h, ymin), border = NA,
          col = "azure2")
  Sys.sleep(t)
  ### tronco:
  n <- 150
  xmint <- -0.6
  xmaxt <- -xmint
  ymint <- -1.5
  ymaxt <- 0.8
  x <- runif(n, xmint, xmaxt)
  y <- runif(n, ymint, ymaxt)
  lines(x, y, col = "darkorange4")
  Sys.sleep(t)
  ### copa:
  n <- 400
  fx <- 6
  xminc <- fx * xmint
  xmaxc <- fx * xmaxt
  yminc <- ymaxt
  fy <- 2.7
  ymaxc <- yminc + fy * (ymaxt - ymint)
  x <- runif(n, min = xminc, max = xmaxc)
  k <- ymaxc
  h <- ymaxc - yminc
  m <- h / xmaxc
  y <- (k - m * abs(x)) * runif(n, 0.9, 1.1)
  nto0 <- round(n / (1 + sqrt(1 + (h / m)^2)))
  to0 <- sample(1:n, size = nto0)
  y[to0] <- yminc
  y <- y + runif(n, -0.3, 0.3)
  lines(x, y, col = "forestgreen")

  Sys.sleep(t)
  np <- 500
  points(u[1:np], v[1:np], pch = 8, lwd = 1, cex = 0.1, col = rainbow(180)[90])
  mess <- switch(language, english = "HAPPY", spanish = "FELIZ",
                 catalan = "BON")
  year <- unlist(strsplit(paste0(year, "!"), ""))
  t <- 1
  Sys.sleep(t)
  x0 <- switch(language, english = -7.4, spanish = -7.4, catalan = -6.8)
  x0year <- switch(language, english = 3.1, spanish = 2.6, catalan = 2.4)
  h <- -4
  text(x0, h, mess, cex = 2.5, font = 2, col = "forestgreen")
  d <- 0.8
  for (i in 1:length(year)) {
    Sys.sleep(0.5 * t)
    text(x0 + x0year + (i - 1) * d, h, year[i], cex = 2.5, font = 2,
         col = "forestgreen")
  }
  Sys.sleep(1.5)
  myvfont <- c("serif", "bold")
  text(x = 0, y = ymaxc + 0.5, labels = "R", srt = 15, vfont = myvfont,
       cex = 4, col = "gold")
}





