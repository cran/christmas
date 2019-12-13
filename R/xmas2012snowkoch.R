#' @title Christmas card 2012.
#'
#' @description Christmas card 2012: Koch snowflakes (https://en.wikipedia.org/wiki/Koch_snowflake) are used to print the Christmas message in three languages (English, Catalan and Spanish).
#'
#' @param year Year to be printed. Default is \code{2013}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no seed).
#' @return A Christmas card plot including Koch snowflakes.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmas2012snowkoch()
#' }
#' @export

xmas2012snowkoch <- function(year = 2013, seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)
  mcex <- 1.8
  nsnowflakes <- 40
  # Snowflakes color:
  colors <- rainbow(n = 100, start = 0.5, end = 0.65)
  message <- c("Feliz", "Bon any", "Happy")
  messagecol <- sample(c("red", "forestgreen", "white"), size = 3, replace = FALSE)
  newwindow()
  plot(c(-10, 10), c(-10, 10), type = "n", asp = 1, axes = F, xlab = "", ylab = "")
  centers <- koch(niter = 1, size = 9, rotate = runif(1, 0, pi / 3))
  for (i in 1:nsnowflakes)
    polygon(randomkoch(), border = NA, col = colors[sample(1:100)])
  for (i in 1:3) {
    Sys.sleep(1)
    polygon(koch(size = 8, centerX = centers[i, 1], centerY = centers[i, 2], rotate = runif(1, 0, pi/3)), border = "blue", col = colors[sample(1:20)], lwd = 2)
   }
  for (i in 1:3) {
    Sys.sleep(1)
    text(centers[i, 1], centers[i, 2] + 0.6, message[i], col = messagecol[i], cex = mcex, family = "serif")
    text(centers[i, 1], centers[i, 2] - 0.6, paste(year, "!", sep = ""), col = messagecol[i], cex = mcex, family = "serif")
   }
 }
