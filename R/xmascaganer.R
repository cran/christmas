#' @title Caganer.
#'
#' @description A Catalan 'caganer' (2009 card). Caganer is a figurine depicted
#'   in the act of defecation appearing in nativity scenes in Catalonia and
#'   neighbouring areas with Catalan culture such as Andorra, Valencia, and
#'   Northern Catalonia (in southern France). It is most popular and widespread
#'   in these areas, but can also be found in other areas of Spain (Murcia),
#'   Portugal, and southern Italy (Naples). Further details can be found at
#'   \url{https://en.wikipedia.org/wiki/Caganer}. This caganer won the annual
#'   contest of sustainable caganers at CREAL (now ISGlobal,
#'   \url{https://www.isglobal.org/en/}) in 2009.
#'
#' @param year Year to be printed. Default is \code{2010}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot with a caganer.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmascaganer()
#' }
#' @export

xmascaganer <- function(year = 2010,
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

  t <- 0.3

  # Background:
  newwindow()
  Sys.sleep(0.5 * t)
  u <- runif(2000, -6, 9)
  v <- runif(2000, -4, 10)
  plot(u, v, type = "n", xlim = c(-6, 9), ylim = c(-4, 10), asp = 1, axes = F, xlab = "", ylab = "")
  polygon(c(-6, -6, 9, 9), c(-4, 1, 1, -4), border = NA, col = "azure2")
  polygon(c(-6, -6, 9, 9), c(1, 10, 10, 1), border = NA, col = "darkblue")
  lines(u[1:500], v[1:500], type = "p", pch = 8, lwd = 1, cex = 0.1, col = rainbow(180)[90])
  x <- seq(-6, 9, by = 0.01)
  lines(x, rnorm(length(x), 1, 0.05), type = "l", lwd = 3, col = "blue4")
  # Body:
  Sys.sleep(t)
  a <- seq(0, 2 * pi, 0.05)
  x0 <-       cos(a)
  y0 <- 0.5 * sin(a)
  theta <- 75 * pi / 180
  x <- -0.3 + x0 * cos(theta) - y0 * sin(theta)
  y <-  0.5 + x0 * sin(theta) + y0 * cos(theta)
  polygon(x, y, border = NA, col = "white")
  # Head:
  Sys.sleep(t)
  x <- 0.3 + 0.5 * cos(a)
  y <- 1.6 + 0.5 * sin(a)
  polygon(x, y, border = NA, col = "wheat3")
  # Noise:
  Sys.sleep(t)
  x <- 0.7 + 0.1 * cos(a)
  y <- 1.3 + 0.1 * sin(a)
  polygon(x, y, border = NA, col = "wheat3")
  # Eyes:
  Sys.sleep(t)
  x <- 0.6 + 0.05 * cos(a)
  y <- 1.5 + 0.05 * sin(a)
  polygon(x, y, border = NA, col = "black")
  x <- 0.7 + 0.05 * cos(a)
  y <- 1.5 + 0.05 * sin(a)
  polygon(x, y, border = NA, col = "black")
  # Curra:
  Sys.sleep(t)
  d <- seq(0, pi / 2)
  x <-       0.3 * cos(d)
  y <- 1.3 + 0.3 * sin(d)
  lines(x, y, lwd = 6)
  # Barretina:
  Sys.sleep(t)
  c <- seq(pi / 4, 5 * pi / 4, 0.05)
  x <- 0.10 + 0.77 * cos(c)
  y <- 1.65 + 0.52 * sin(c)
  polygon(x, y, border = NA, col = "red")
  polygon(c(-0.15, 0.75, 0.60, -0.25), c( 1.40, 1.90, 2.05,  1.50), border = NA, col = "black")
  # Legs:
  Sys.sleep(t)
  x0 <- 0.9 * cos(a)
  y0 <- 0.4 * sin(a)
  theta <- 30 * pi / 180
  x <- x0 * cos(theta) - y0 * sin(theta)
  y <- x0 * sin(theta) + y0 * cos(theta)
  polygon(x, y, col = "black")
  # Culo:
  Sys.sleep(t)
  x <- -0.55 + 0.35 * cos(a)
  y <- -0.34 + 0.35 * sin(a)
  polygon(x, y, border = NA, col = "wheat3")
  # Legs:
  Sys.sleep(t)
  x0 <-       cos(a)
  y0 <- 0.3 * sin(a)
  theta <- 70 * pi / 180
  x <-  0.42 + x0 * cos(theta) - y0 * sin(theta)
  y <- -0.45 + x0 * sin(theta) + y0 * cos(theta)
  polygon(x, y, col = "black")
  # ...legs:
  Sys.sleep(t)
  b <- seq(0.98 * pi, 1.37 * pi, 0.05)
  x <- 0.65 * cos(b)
  y <- 0.65 * sin(b)
  polygon(x, y, border = NA, col = "black")
  # Feet:
  Sys.sleep(t)
  x <-  0.4 + 0.50 * cos(a)
  y <- -1.3 + 0.15 * sin(a)
  polygon(x, y, border = NA, col = "tan4")
  # Belt:
  Sys.sleep(t)
  polygon(c(-0.85, 0.25, 0.25, -0.85), c(-0.20, 0.50, 0.70,  0.15), border = "NA", col = "red")
  # ...legs:
  Sys.sleep(t)
  polygon(c(-0.31, 0.27, 0.25, -0.60), c(-0.61, 0.22, 0.50, -0.05), border = "NA", col = "black")
  # Arms:
  Sys.sleep(t)
  polygon(c(0.10, -0.25, -0.6, -0.3), c(0.25,  0.10,  0.5,  0.9), border = "NA", col = "white")
  polygon(c(0, -0.05, -0.30, 0.10), c(0, -0.10,  0.15, 0.28), border = "NA", col = "white")
  x <- 0.50 + 0.2 * cos(a)
  y <- 0.08 + 0.2 * sin(a)
  Sys.sleep(t)
  polygon(x, y, border = NA, col = "wheat3")
  polygon(c(-0.05, 0.08, 0.40,  0.40), c(-0.10, 0.28, 0.28, -0.10), border = "NA", col = "white")
  # Ca...:
  Sys.sleep(t)
  x <- -0.5 + 0.30 * cos(a)
  y <- -1.3 + 0.15 * sin(a)
  polygon(x, y, border = NA, col = "darkorange4")
  # ...ga...:
  Sys.sleep(t)
  x <- -0.50 + 0.2 * cos(a)
  y <- -1.15 + 0.1 * sin(a)
  polygon(x, y, border = NA, col = "darkorange4")
  # ...da:
  Sys.sleep(t)
  x0 <- 0.13 * cos(a)
  y0 <- 0.07 * sin(a)
  theta <- 70
  x <- -0.5 + x0 * cos(theta * pi / 180) - y0 * sin(theta * pi / 180)
  y <- -1.0 + x0 * sin(theta * pi / 180) + y0 * cos(theta * pi / 180)
  polygon(x, y, border = NA, col = "darkorange4")
  # Trunk:
  Sys.sleep(t)
  x <- c(-2.4, -1.6, -1.6, -2.4) + rnorm(4, 0, 0.02)
  y <- c( 2.5,  2.5, -1.2, -1.2) + rnorm(4, 0, 0.02)
  polygon(x, y, border = NA, col = "brown")
  x <- runif(9, -2.3, -1.7)
  y <- runif(9, -0.5,  2.2)
  lines(x, y, type = "p", pch = "|", cex = 1.4, col = "orangered4")
  x <- seq(-2.4, -1.6, by = 0.01)
  lines(x, rnorm(length(x), -1.2, 0.05), col = "azure2")
  Sys.sleep(t)
  # Tree 1:
  x <- c(-5.0, 1.0, -2)
  y <- c( 2.5, 2.5,  6)
  polygon(x, y, border = NA, col = "forestgreen")
  t <- seq(0, 1, by = 0.01)
  x <- -5 + 6 * t
  y <-  6 - 7 * abs(t - 0.5)
  y <- y + rnorm(length(y), 0, 0.05)
  lines(x, y, col = "forestgreen", lwd = 6)
  lines(x, rnorm(length(x), 2.7, 0.1), col = "white", lwd = 15)
  # Tree 2:
  Sys.sleep(t)
  x <- c(-4.0, 0.0, -2)
  y <- c( 4.5, 4.5,  7)
  polygon(x, y, border = NA, col = "forestgreen")
  x <- -4 + 4 * t
  y <-  7 - 5 * abs(t - 0.5)
  y <- y + rnorm(length(y), 0, 0.05)
  lines(x, y, col = "forestgreen", lwd = 6)
  lines(x, rnorm(length(x), 4.7, 0.1), col = "white", lwd = 15)
  # Tree 3:
  Sys.sleep(t)
  x <- c(-3.5, -0.5, -2)
  y <- c( 6.0,  6.0,  8)
  polygon(x, y, border = NA, col = "forestgreen")
  x <- -3.5 + 3 * t
  y <-  8.0 - 4 * abs(t - 0.5)
  y <- y + rnorm(length(y), 0, 0.05)
  lines(x, y, col = "forestgreen", lwd = 6)
  lines(x, rnorm(length(x), 6.2, 0.1), col = "white", lwd = 15)
  # Text:
  Sys.sleep(t)
  xmess <- 4.8 + (-2:2) * 1.27
  xyear <- 4.8 + (-2:2)

  if (language == "catalan")
    xmess <- xmess[2:4]
  x <- c(xmess, xyear)

  ymess <- c(8.0, 7.6, 8.0, 7.6, 7.7)
  if (language == "catalan")
    ymess <- ymess[2:4]
  yyear <- c(6.3, 5.7, 6.3, 5.7, 6.2)
  y <- c(ymess, yyear)

  mess <- switch(language, english = "HAPPY", spanish = "FELIZ",
                 catalan = "BON")
  messsplit <- unlist(strsplit(mess, ""))
  yearsplit <- unlist(strsplit(paste0(year, "!"), ""))
  ms <- c(messsplit, yearsplit)
  col.text <- c(rep("red", length(messsplit)),
                rep("forestgreen", length(yearsplit)))
  mida <- 4 #c(rep(4, 6), rep(5, 3), 4, 1)
  for (i in 1:11)
    text(x[i], y[i], ms[i], col = col.text[i],
         cex = mida, #mida[i],
         font = 1)
  # More snow:
  Sys.sleep(t)
  lines(u[501:2000], v[501:2000], type = "p", pch = 8, lwd = 1, cex = 0.05, col = "white")
}
