#' @title Christmas risks.
#'
#' @description Christmas risks (2011 card). Some epidemiological jokes about
#'   potential effects of Christmas on health.
#'
#' @param year Year to be printed. Default is \code{2012}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot including boxplots and OR estimates.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmashealth()
#' }
#' @export

xmashealth <- function(year = 2012, seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)

  nDaysX <- 20
  nDaysNoX <- 365 - nDaysX
  nYears <- 100
  christmas <- c(rep(1, nYears * nDaysX), rep(0, nYears * nDaysNoX))
  X <- data.frame(christmas)
  X$inlaws <- simulateBinary(p0 = 0.2, or = 1.8, christmas = christmas)
  X$financial <- simulateBinary(p0 = 0.1, or = 1.4, christmas = christmas)
  X$noise <- simulateBinary(p0 = 0.1, or = 1.2, christmas = christmas)
  labs <- c("In-laws\n visits", "Domestic\n financial\n crisis", "Harmful levels\n of children's\n noise at home")
  ORs <- t(sapply(names(X)[-1], FUN = function(y) getOR(x = y, data = X)))
  newwindow()
  op <- par(mfrow = c(1, 2), las = 1, oma = c(0, 0, 2, 0))
  on.exit(par(op))
  op
  mu0 <- 115
  mu1 <- 165
  mu <- data.frame(mu = mu0 + (mu1 - mu0) * christmas)
  X$ldl <- apply(mu, 1, FUN = function(x) rnorm(n = 1, mean = x, sd = 10))
  boxplot(ldl ~ christmas, data = X, xlab = "Christmas", ylab = "", ylim = c(0, 2 * mu1), col = c("forestgreen", "red"), main = "LDL cholesterol (mg/dl)", xaxt = "n")
  axis(1, at = c(1, 2), labels = c("Before", "After"), cex = 0.5)
  xmin <- 0.8
  xmax <- max(ORs[, 3])
  plot(ORs[, 1], 1:3, type = "p", xlim = c(xmin, xmax), ylim = c(0, 4), pch = 19, xlab = "OR", ylab = "", yaxt = "n", main = "Other Christmas risks", cex = 1.3, col = "blue")
  axis(2, at = 1:3, labels = labs, cex.axis = 0.8)
  abline(v = 1, lty = 2, col = "blue", lwd = 2)
  segments(ORs[, 2], 1:3, ORs[, 3], 1:3, lwd = 3, col = "blue")
  tit <- paste0("I wish you a statistically non significant chRistmas and a happy ",
                year, "!")
  title(tit, outer = TRUE, cex.main = 1.3, col.main = "forestgreen")
}
