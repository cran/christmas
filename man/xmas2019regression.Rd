% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xmas2019regression.R
\name{xmas2019regression}
\alias{xmas2019regression}
\title{Christmas card 2019.}
\usage{
xmas2019regression(year = 2020, language = c("english", "spanish",
  "catalan"), time = 12, seed = NULL)
}
\arguments{
\item{year}{Year to be printed. Default is \code{2020}.}

\item{language}{Language to be used in the card. One of \code{c("english", "spanish", "catalan")}. Default is \code{"english"}.}

\item{time}{Total time, in seconds, for the card generation. Default is 12.}

\item{seed}{Seed for reproducibility of the card. Default is \code{NULL} (no seed).}
}
\value{
An illustration of the Christmas regression model.
}
\description{
Christmas card 2019: a Christmas regression model.
}
\examples{
\donttest{
xmas2019regression()
xmas2019regression(year = 2021, language = "catalan", time = 5)
}
}
\author{
Jose Barrera-Gomez.
}
