#' @title Christmas message.
#'
#' @description A random repetition of a message (2010 card). Random allocation
#'   of repetitions of the Christmas message.
#'
#' @param year Year to be printed. Default is \code{2011}.
#' @param language Language to be used in the card. One of \code{c("english",
#'   "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot including the repetition of the same message
#'   randomly allocated.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmastext()
#' xmastext(year = 2020, language = "spanish", seed = 666)
#' }
#' @export


xmastext <- function(year = 2011,
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
  totalTime <- 12
  minTime <- 0.005
  maxTime <- 0.5
  mess <- switch(language,
                 english = "Happy ",
                 spanish = "Feliz ",
                 catalan = "Bon ")
  mess <- paste0(mess, year, "!")
  mess0 <- unlist(strsplit(mess, ""))
  mess0 <- mess0[mess0 != " "]
  colors <- rainbow(40)[11:17]
  l <- length(mess0)
  vf <- c("script", "bold")
  r <- totalTime / (totalTime + maxTime - minTime)
  A <- maxTime / r
  k <- round(1 + log(minTime / maxTime) / log(r))
  newwindow()
  op <- par(mar = c(0, 0, 0, 0))
  on.exit(par(op))
  #par(mar = c(0, 0, 0, 0))
  op
  plot(1, ann = F, type = "n", axes = F)
  for (i in 1:k)
   {
    messperm <- paste(sample(mess0, l), collapse = " ")
    text(runif(1, 0.6, 1.4), runif(1, 0.8, 1.2), messperm, srt = runif(1, 0, 360), col = sample(colors, 1), cex = 6 * (k - i) / k, vfont = vf)
    Sys.sleep(A * r^i)
   }
  text(1, 1.05, mess, srt = 0, col = "red", cex = 6, vfont = vf)
  text(1, 0.65, "Jose & R", srt = 0, col = "black", cex = 1.5, vfont = vf)
 }
