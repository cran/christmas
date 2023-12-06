#' @title Christmas helix.
#'
#' @description Christmas helix (2023 card). A helix with a Christmas message.
#'
#' @param year Year to be printed. Default is \code{2024}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @param totalsecs Approximate total time, in seconds, to create the card.
#'   Default is \code{NULL} (and it is internally set).
#' @return A Christmas card plot including a message in a helix.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmashelix(year = 2024, language = "catalan", totalsecs = 1)
#' }
#' @export

xmashelix <- function(year = 2024,
                      language = c("english", "spanish", "catalan"),
                      totalsecs = NULL,
                      seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")

  # "language":
  language <- match.arg(language)

  # "totalsecs":
  if(!is.null(totalsecs) & (is.na(totalsecs) || !is(totalsecs, "numeric") || length(totalsecs) != 1L || totalsecs <= 0 || totalsecs > 20))
    stop("If not NULL, 'totalsecs' must be a positive number not higher than 20.")

  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")

  if (!is.null(seed))
    set.seed(seed)

  # some graphical parameters:
  xmin <- -30
  xmax <- 30
  ymin <- -30
  ymax <- 30
  nv <- 4 # number of laps
  a <- 1
  b <- 1
  l <- 80

  # time factor scaling:
  tf <- 1
  if (!is.null(totalsecs))
    tf <- totalsecs / (1.5 + 0.03 * l)

  mycols <- c("cornflowerblue",
              "aquamarine4",
              "blue",
              "blueviolet",
              "deepskyblue4",
              "darkgreen",
              "darkmagenta",
              "darkorchid",
              "darkviolet",
              "darkslategrey",
              "hotpink",
              "mediumseagreen",
              "midnightblue",
              "orangered",
              "red",
              "seagreen4")

  rancols <- sample(x = mycols, size = 2)

  ### message 1:
  mess1 <- switch(language,
                  english = "Peace",
                  spanish = "Paz",
                  catalan = "Pau")

  ### message 2:
  mess2 <- switch(language,
                  english = "I wish you a very happy",
                  spanish = "Te deseo mucha felicidad para el",
                  catalan = "Et desitjo molta felicitat per al")
  mess2 <- paste(mess2, year)

  ### message 3:
  mess3 <- switch(language,
                  english = "and peace",
                  spanish = "y paz",
                  catalan = "i pau")

  theta <- seq(from = 0, to = 2 * pi * nv, length.out = l)
  r <- a + b * theta
  x <- r * cos(theta)
  y <- r * sin(theta)
  x[l] <- 0
  y[l] <- 0

  newwindow()
  op <- par(family = "HersheySerif")
  on.exit(par(op))
  op

  plot(c(xmin, xmax), c(ymin - 0.3 * (ymax - ymin), ymax), type = "n", asp = 1, axes = F, xlab = "", ylab = "")
  cexmin <- 0.05
  cexmax <- 3
  cexs <- seq(from = cexmin, to = cexmax, length.out = l)
  cexs[l] <- 2 * cexs[l]

  colors <- rainbow(n = l - 1)

  v <- floor(theta / (2 * pi))
  thetareduxdegrees <- 180 * (theta - 2 * pi * v) / pi

  for (i in 1:(l - 1)) {
    text(x[i], y[i], mess1, cex = cexs[i], srt = thetareduxdegrees[i],
         col = colors[i])
    Sys.sleep(0.03 * tf)
  }

  # I wish you...:
  Sys.sleep(0.5 * tf)
  text(x = (xmin + xmax) / 2,
       y = ymin - 0.15 * (ymax - ymin),
       mess2, col = rancols[2], font = 2, cex = 1.8)

  # ... and peace:
  Sys.sleep(tf)
  text(x = (xmin + xmax) / 2,
       y = ymin - 0.24 * (ymax - ymin),
       mess3,
       cex = 2,
       srt = thetareduxdegrees[l],
       col = rancols[1])
}
