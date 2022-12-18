#' @title Christmas regression model.
#'
#' @description A plot of the Christmas regression model (2019 card).
#'
#' @param year Year to be printed. Default is \code{2020}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param time Total time, in seconds, for the card generation. Default is 12.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return An illustration of the Christmas regression model.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmasregression()
#' xmasregression(year = 2021, language = "catalan", time = 2)
#' }
#' @export

xmasregression <- function(year = 2020,
                           language = c("english", "spanish", "catalan"),
                           time = 4,
                           seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "language":
  language <- match.arg(language)
  # "time":
  if (!inherits(time, c("numeric", "integer")) || length(time) != 1L || time <= 0)
    stop("'time' must be a positive number")
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)

  t <- time / 26

  # data (sleep = 0):
  if (!is.null(seed)) set.seed(seed)
  dm1 <- paste(12, 20:31, sep = "-")
  dm2 <- paste("01", paste0(0, 1:8), sep = "-")
  dm <- c(dm1, dm2)
  nd <- length(dm)
  rr <- runif(n = nd, min = 0.999, 1.001)
  rr[dm == "12-24"] <- rr[dm == "12-24"] + 0.03
  rr[dm == "12-25"] <- rr[dm == "12-25"] + 0.07
  rr[dm == "12-26"] <- rr[dm == "12-26"] + 0.05
  rr[dm == "12-31"] <- rr[dm == "12-31"] + 0.1
  rr[dm == "01-01"] <- rr[dm == "01-01"] + 0.12
  rr[dm == "01-06"] <- rr[dm == "01-06"] + 0.07
  d <- as.Date(c(paste(year - 1, dm1, sep = "-"), paste(year, dm2, sep = "-")))
  # canvas (sleep = 0):
  newwindow()
  op <- par(las = 1, cex.main = 1.5, col.main = "blue")
  on.exit(par(op))
  #  par(las = 1, cex.main = 1.5, col.main = "blue")
  op
  xl <- switch(language,
               english = "Date",
               spanish = "Fecha",
               catalan = "Data")
  yl <- switch(language,
               english = "Relative risk",
               spanish = "Riesgo relativo",
               catalan = "Risc relatiu")
  plot(d, rr, xlab = xl, ylab = yl, type = "n", ylim = c(1, 1.2), xaxt = "n")
  axis(1, at = d, labels = FALSE)
  text(d, par("usr")[3] - 0.01, labels = format(d, "%d/%m/%y"), srt = 45, adj = 1, xpd = TRUE, cex = 0.7)
  # data points (sleep = 8.5 * t):
  Sys.sleep(0.5 * t)
  for (i in 1:length(d)) {
    Sys.sleep(0.4 * t)
    points(d[i], rr[i], col = "gray", pch = 19)
  }
  # model name (sleep = 1.5 * t)::
  Sys.sleep(1.5 * t)
  myvfont <- c("serif", "bold")
  text(mean(d), 1.22, labels = "chRistmas Regression model", col = "blue", xpd = TRUE, cex = 1.4, vfont = myvfont)
  # fit model (sleep = 4.5 * t):
  Sys.sleep(1.5 * t)
  sels <- list(c("12-23", "12-24", "12-25", "12-26", "12-27"),
               c("12-30", "12-31", "01-01", "01-02"),
               c("01-05", "01-06", "01-07"))
  for (i in 1:length(sels)) {
    sel <- dm %in% sels[[i]]
    d2 <- d[sel]
    rr2 <- rr[sel]
    rr2[rr2 < 1] <- 1
    polygon(d2, rr2, border = NULL, col = "forestgreen")
    Sys.sleep(t)
  }
  # fitted values (sleep = 8 * t):
  sel0 <- dm %in% c("12-24", "12-25", "12-26", "12-31", "01-01", "01-06")
  d0 <- d[sel0]
  rr0 <- rr[sel0]
  for (i in 1:length(sel0)) {
    points(d0[i], rr0[i], pch = 19, col = "red", cex = 3)
    Sys.sleep(0.4 * t)
  }
  # snowflakes (sleep = 0):
  for (i in 1:2) {
    fac <- runif(n = length(d0), min = 0.1, max = 0.9)
    points(d0, fac * (rr0 - 1) + 1, pch = 8, cex = 1.2 + (i - 1) * 0.6, col = "white")
  }
  # message (sleep = 3 * t):
  Sys.sleep(2 * t)
  y <- 1.17
  mess <- switch(language,
                 english = "Happy ",
                 spanish = "Feliz ",
                 catalan = "Bon ")
  messaux <- paste0(mess, year, "!")
  text(x = as.Date(paste(year - 1, "12-28", sep = "-")), y = y, labels = messaux, vfont = myvfont, cex = 2.5, col = "red")
  Sys.sleep(2 * t)
  lab <- switch(language,
                 english = "   (with R)",
                 spanish = "   (con R)",
                 catalan = "   (amb R)")
  text(x = as.Date(paste(year, "01-01", sep = "-")), y = y, labels = lab, vfont = myvfont, cex = 1.3, col = "gold")
}
