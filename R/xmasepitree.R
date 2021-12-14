#' @title Christmas epitree.
#'
#' @description A statistical/epidemiological Christmas tree (2016 card). This
#'   tree is built using a random subset of statistical and epidemiological
#'   words.
#'
#' @param year Year to be printed. Default is \code{2017}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no
#'   seed).
#' @return A Christmas card plot including a tree made of nice words.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmasepitree()
#' xmasepitree(year = 2020)
#' }
#' @export

xmasepitree <- function(year = 2017, seed = NULL) {
  # "year":
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  # "seed":
  if(!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed)) set.seed(seed)
  # you can add new words (don't matter if there are duplicates).
  # there are only few words with more than 12 characters...
  words <- c("bias", "mean", "mode", "rate", "anova",
             "median", "average", "estimate", "confidence",
             "coefficient", "correlation", "autocorrelation",
             "distribution", "frequency", "table", "normal",
             "plot", "boxplot", "density", "random", "data",
             "simulation", "categorical", "analysis", "cluster",
             "sample", "error", "confounding", "interaction",
             "variable", "covariance", "database", "quantile",
             "statistics", "descriptive", "effect", "design",
             "study", "smoothing", "parameter", "variance",
             "exponential", "logarithm", "test", "gaussian",
             "logistic", "poisson", "gamma", "lognormal",
             "unit", "hypothesis", "additive", "percentage",
             "increment", "range", "concordance", "linear",
             "curve", "likelihood", "method", "model", "lag",
             "relationship", "adjusted", "fitted", "observed",
             "expected", "link", "logit", "measure", "odds",
             "absolute", "dispersion", "trend", "matched",
             "stratum", "mixed", "multiple", "negative",
             "positive", "null", "missing", "symmetry", "set",
             "interpolation", "risk", "probability", "order",
             "projection", "outlier", "permutation", "power",
             "function", "geometric", "proportional", "sum",
             "qualitative", "quantitative", "regression",
             "residual", "significance", "relative", "level",
             "outcome", "independent", "scatterplot", "value",
             "subset", "standard", "standardised", "series",
             "stratified", "matrix", "weight", "transformation",
             "population", "base", "uniform", "bootstrap",
             "causal", "conditional", "continuous", "coverage",
             "homocedasticity", "marginal", "crude", "binomial",
             "multinomial", "univariate", "multivariate",
             "observational", "unbiased", "interval",
             "extrapolation", "standardization")
  # data.frame of words:
  words <- unique(words)
  nc <- sapply(words, nchar)
  dd <- data.frame(word = words, nc)
  ord <- with(dd, order(word, nc))
  dd <- dd[ord, ]
  rownames(dd) <- NULL
  # number of words by number of characters:
  ncs <- sort(unique(dd$nc))
  # number of lengths:
  lncs <- length(ncs)
  # select tree:
  selwords <- sapply(ncs, FUN = function(x) getword(data = dd, numchars = x))
  # plot tree:
  newwindow()
  xmin <- -4
  xmax <- 4
  ymin <- 0
  ymax <- max(ncs) + 6
  myvfont <- c("serif", "bold")
  plot(c(xmin, xmax), c(ymin, ymax), type = "n", axes = FALSE, xlab = "", ylab = "")
  # log:
  loglabels <- c("EPIDEMIOLOGY", "STATISTICS")
  Sys.sleep(1)
  for (i in 1:length(loglabels)) {
   	text(x = 0, y = ymin + 1 + i, labels = loglabels[i], vfont = myvfont, cex = 1.7, col = "brown")
    Sys.sleep(1)
  }
  for (i in length(selwords):1) {
   	text(x = 0, y = ymax - ncs[i] - 2, labels = selwords[i], vfont = myvfont, cex = 2, col = "forestgreen")
   	Sys.sleep(0.3)
  }
  Sys.sleep(0.7)
  text(x = 0, y = ymax - 0.5, labels = paste0("HAPPY ", year, "!"), vfont = myvfont, cex = 4, col = "red")
  Sys.sleep(1.5)
  text(x = 0, y = max(ncs) + 2.5, labels = "R", srt = 15, vfont = myvfont, cex = 6, col = "gold")
}
