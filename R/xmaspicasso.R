#' @title Picasso's pigeon.
#'
#' @description Approximation to Picasso's pigeon (2022 card).
#'
#' @param year Year to be printed. Default is \code{2023}.
#' @param language Language to be used in the card. One of \code{c("english", "spanish", "catalan")}. Default is \code{"english"}.
#' @param seed Seed for reproducibility of the card. Default is \code{NULL} (no seed).
#' @return A Christmas card plot including an approximation to Picasso's pigeon.
#' @author Jose Barrera-Gomez.
#' @examples
#' \donttest{
#' xmaspicasso()
#' xmaspicasso(year = 2023, language = "catalan")
#' }
#' @export

xmaspicasso <- function (year = 2023,
                         language = c("english", "spanish", "catalan"),
                         seed = NULL) {
  if (!inherits(year, c("numeric", "integer")) || length(year) != 1L)
    stop("'year' must be a number")
  language <- match.arg(language)
  if (!is.null(seed) & (is.na(seed) || !is(seed, "numeric")))
    stop("'seed' must be numeric or NULL")
  if (!is.null(seed))
    set.seed(seed)

  rx <- 3.5
  ry <- 3

  newwindow()
  op <- par(family = "HersheySerif")
  on.exit(par(op))
  op

  cols0 <- c("red", "orange", "yellow", "forestgreen", "blue", "violet")
  cols0 <- sample(cols0)
  cols <- rep(cols0, c(1, 1, 10, 8, 9, 1))

  ### clist
  clist = list(c01 = list(x = c(34, 37, 39, 43, 46, 51, 53, 57),
                          y = c(34.5, 35.9, 37, 41, 44, 44.5, 44, 43),
                          type = "loess", col = cols[1]),
               c02 = list(x = c(57, 56, 54.5, 53, 52, 51 ,49.5, 48, 44, 39.5),
                          y = c(43, 42.5, 41, 40, 34, 29, 25.5,23, 18, 15),
                          type = "loess", col = cols[2]),
               c03 = list(x = c(44, 41, 38, 30, 20, 16, 10, 7, 3, 2.5),
                          y = c(35, 38.5, 41, 46, 50, 51, 51.5, 51.5, 51, 50.4),
                          type = "poly", col = cols[3]),
               c04 = list(x = c(2.5, 4, 5, 7, 10),
                          y = c(50.4, 49, 48.6, 48, 47),
                          type = "poly", col = cols[4]),
               c05 = list(x = c(10, 8, 7, 5, 3.3),
                          y = c(47, 46.6, 46.5, 45.8, 44.7),
                          type = "poly", col = cols[5]),
               c06 = list(x = c(3.3, 5, 7, 10, 11.4),
                          y = c(44.7, 43.5, 43,  42.8, 42.3),
                          type = "poly", col = cols[6]),
               c07 = list(x = c(11.4, 9.5, 7, 5, 4, 3.3),
                          y = c(42.3, 41.5, 40.5, 39.5, 39, 38.2),
                          type = "poly", col = cols[7]),
               c08 = list(x = c(3.3, 5:9),
                          y = c(38.2, rep(37.5, 4), 37.4),
                          type = "poly", col = cols[8]),
               c09 = list(x = c(9, 8.7, 8, 7.6, 7.7),
                          y = c(37.4, 37, 36.6, 35.7, 35.5),
                          type = "poly", col = cols[9]),
               c10 = list(x = c(7.7, 8, 10, 11, 12, 12.7),
                          y = c(35.5, 34.8, 34.2, 34, 33.8, 33.5),
                          type = "poly", col = cols[10]),
               c11 = list(x = c(12.7, 12.8, 12.3, 12, 11.7),
                          y = c(33.5, 33.5, 32, 31.2, 30.5),
                          type = "poly", col = cols[11]),
               c12 = list(x = c(11.7, 12:23),
                          y = c(30.5, 30, 29, 28.8, 28.6, 28.6, 28.8, 28.9, 29, 29.3, 29.5, 29.8, 29.9),
                          type = "poly2", col = cols[12]),
               c13 = list(x = c(2 * (0:13), 27),
                          y = c(25, 24, 23.9, 24.1, 24.8, 24.8, 25.3, 25.8, 26.4, 27:29, 30.5, 33, 35),
                          type = "poly", col = cols[13]),
               c14 = list(x = c(0:5, 6.7),
                          y = c(25, 22, 21, 20.7, 20.7, 20.8, 21),
                          type = "loess", col = cols[14]),
               c15 = list(x = 3:7,
                          y = c(16, 18:20, 20.8),
                          type = "poly2", col = cols[15]),
               c16 = list(x = 3:11,
                          y = c(16, 15.1, 15.2, 14.4, 16, 16.5, 17.2, 17.9, 18.5),
                          type = "poly2", col = cols[16]),
               c17 = list(x = 7:11,
                          y = c(11.3, 14.5, 16, 17.4, 18.5),
                          type = "poly2", col = cols[17]),
               c18 = list(x = 7:12,
                          y = c(11.3, 11.4, 12:15),
                          type = "poly2", col = cols[18]),
               c19 = list(x = c(11, 11.5, 12),
                          y = c(10, 13.5, 15),
                          type = "poly2", col = cols[19]),
               c20 = list(x = 11:21,
                          y = c(10, 9, 9.6, 10.2, 11.2, 12.1, 13.3, 14.8, 16.2, 18, 19),
                          type = "poly2", col = cols[20]),
               c21 = list(x = c(23.7, 24, 24.8),
                          y = c(21.6, 22.2, 23.2),
                          type = "poly2", col = cols[21]),
               c22 = list(x = c(23.7, 24.6, 25),
                          y = c(21.6, 21.5, 21.7),
                          type = "poly2", col = cols[22]),
               c23 = list(x = c(21.2, 22:25),
                          y = c(16, 18, 19, 20.3, 21.7),
                          type = "poly2", col = cols[23]),
               c24 = list(x = c(21.2, 22, 23, 24.3),
                          y = c(16, 15.6, 16, 16.6),
                          type = "poly2", col = cols[24]),
               c25 = list(x = c(17.5, 18:23, 24.3),
                          y = c(5.7, 7.7, 9.5, 11, 11.4, 12.7, 13.9, 16.6),
                          type = "poly2", col = cols[25]),
               c26 = list(x = c(0.5 + 17:26),
                          y = c(5.9, 6, 6.5, 7.2, 8, 8.6, 9.4, 10.2, 10.9, 11.5),
                          type = "poly2", col = cols[26]),
               c27 = list(x = c(20.8, 21:25, 26.5),
                          y = c(4.8, 5, 6.2, 7.5, 8.7, 10, 11.8),
                          type = "poly2", col = cols[27]),
               c28 = list(x = c(21, 23, 26, 30, 33, 35, 37, 40, 41),
                          y = c(3.9, 4, 5, 7, 9, 11, 13, 17, 21) + 1,
                          type = "poly", col = cols[28]),
               c29 = list(x = c(36, 39, 40, 40.9, 41),
                          y = c(31, 29, 27, 24, 21),
                          type = "poly2", col = cols[29]))

  plot(x = c(0, rx * 60), y = c(0, ry * 55), type = "n", asp = 1, axes = F,
       xlab = "", ylab = "")

  ### bird:
  for (i in 1:length(clist))
    drawbird(x = clist[[i]]$x, y = clist[[i]]$y, type = as.character(clist[[i]]$type),
             col = as.character(clist[[i]]$col), lwd = 4)

  ### o:
  points(rx * 50.3, ry* 42, cex = 3, lwd = 4, col = cols[30])
  points(rx * 50.3, ry* 42, pch = 19, col = cols[30])

  ### p
  lines(x = rx * c(53:57),
        y = ry * c(42.4, 42.8, 43, 43.2, 43),
        lwd = 4, col = cols[30])
  lines(x = rx * c(55, 55, 55, 55.2, 55.3, 55.3),
        y = ry * c(42.5, 42, 41, 40, 39, 37.6),
        lwd = 4, col = "green")
  lines(x = rx * c(56, 55.4, 55.2, 55),
        y = ry * c(47, 46.5, 45.5, 44.1),
        lwd = 4, col = "green")
  ### R:
  myvfont <- c("serif", "bold")
  textcex <- 2.5
  text(x = rx * 56.5, y = ry * 48, labels = "R", srt = 15, vfont = myvfont,
       cex = textcex, col = "green")

  ### mess:
  mess <- switch(language, english = "HAPPY...", spanish = "FELIZ...",
                 catalan = "BON...")
  year <- unlist(strsplit(paste0(year, "!"), ""))
  x0 <- rx * 43
  h <- ry * 7
  auxcols <- c("cornflowerblue", cols0[cols0 != "yellow"])
  auxcols <- sample(auxcols)
  text(x0, h, mess, cex = textcex, font = 2, col = auxcols[1])
  d2 <- rx * (10.5 -  2.1 * (language == "catalan"))
  d <- rx * 2.7
  Sys.sleep(0.5)
  for (i in 1:length(year)) {
    text(x0 + d2 + (i - 1) * d, h, year[i], cex = textcex, font = 2,
         col = auxcols[i + 1])
  }
}
