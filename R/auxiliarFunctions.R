#' @import grDevices graphics methods stats

################################################################################
################################################################################
###
###  auxiliar functions for all xmas
###
################################################################################
################################################################################
newwindow <- function() {
  if (interactive()) {
    sc <- switch(Sys.info()[['sysname']],
                 Windows = "windows",
                 Linux  = "x11",
                 Darwin = "quartz")
    eval(parse(text = paste(sc, "(width = 10, height = 7)", sep = "")))
    } else {
      dev.new()
    }
}

################################################################################
################################################################################
###
###  auxiliar functions for xmas 2011
###
################################################################################
################################################################################

simulateBinary <- function(p0, or, christmas) {
  p1 <- 1 / (1 + (1 - p0) / (p0 * or))
  p <- data.frame(p = p0 + (p1 - p0) * christmas)
  res <- apply(p, 1, FUN = function(x) rbinom(n = 1, size = 1, prob = x))
  return(res)
}

getOR <- function(x, alpha = 0.05, data) {
  mod <- glm(data[[x]] ~ data$christmas, family = binomial)
  logOR <- summary(mod)$coefficients[2, 1]
  logSE <- summary(mod)$coefficients[2, 2]
  res <- exp(logOR + c(0, qnorm(1 - alpha / 2) * logSE * c(-1, 1)))
  return(res)
}

################################################################################
################################################################################
###
###  auxiliar functions for xmas 2012
###
################################################################################
################################################################################

# Function for create a rotation matrix:
rotationMatrix <- function(theta) {
  res <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
                nrow = 2, ncol = 2)
  return(res)
}

# Function for rotate a vector:
rotation <- function(v, theta) {
  R <- rotationMatrix(theta)
  res <- as.vector(R %*% v)
  return(res)
}

# Function for standardize a Koch's snowflake:
standardKoch <- function(pos) {
  n <- dim(pos)[1]
  m <- colMeans(pos)
  pos <- pos - rep(1, n) %*% t(m)
  d <- max(pos[, 1]) - min(pos[, 1])
  res <- pos / d
  return(res)
}

# Function for drawing a Koch's snowflake (adapted from 'alphahull' library):
koch <- function(side = 3, niter = 5, centerX = 0, centerY = 0, size = 1, rotate = 0) {
  npoints <- 3 * 4^(niter - 1)
  ver <- matrix(nrow = npoints, ncol = 2)
  index <- c(1, npoints / 3 + 1, 2 * npoints / 3 + 1)
  ver[1, ] <- c(-side / 2, 0)
  ver[index[3], ] <- c(side / 2, 0)
  ver[index[2], ] <- ver[1, ] + rotation(ver[index[3], ] - ver[1, ], -pi / 3)
  if (niter >= 2) {
    for (k in 2:niter) {
      npoints2 <- 3 * 4^(niter - k)
      index1 <- index + npoints2 / 3
      index2 <- index + npoints2
      index3 <- index1 + npoints2 / 3
      aux <- diff(rbind(ver[!is.na(ver[, 1]), ], ver[1, ])) / 3
      ver[index1, ] <- ver[index, ] + aux
      ver[index2, ] <- ver[index, ] + 2 * aux
      aux1 <- matrix(c(cos(-pi/3), sin(-pi/3)), nrow = length(index1), ncol = 2, byrow = TRUE)
      aux2 <- matrix(c(-sin(-pi/3), cos(-pi/3)), nrow = length(index1), ncol = 2, byrow = TRUE)
      v <- ver[index2, ] - ver[index1, ]
      ver[index3, ] <- ver[index1, ] + cbind(apply(aux1 * v, 1, sum), apply(aux2 * v, 1, sum))
      index <- sort(c(index, index1, index2, index3))
    }
  }
  ver <- size * standardKoch(ver)
  ver <- t(apply(ver, 1, FUN = function (x) rotation(x, rotate)))
  ver[, 1] <- ver[, 1] + centerX
  ver[, 2] <- ver[, 2] + centerY
  return(ver)
}

# Function for randomize a Koch's snowflake:
randomkoch <- function() {
  r <- runif(1, 0, 7)
  phi <- runif(1, 0, 2 * pi)
  res <- koch(side = 3, niter = 5, centerX = r * cos(phi), centerY = r * sin(phi),
              size = 0.1 + 0.3 * r, rotate = runif(1, 0, pi / 3))
  return(res)
}

################################################################################
################################################################################
###
###  auxiliar functions for xmas 2015
###
################################################################################
################################################################################

# bar function:
bar <- function(theta = 45, l = 10, center = c(-4, -1), col = "darkorange4", lwd = 6) {
  theta <- theta * pi / 180
  pos1 <- center - (l / 2) * c(cos(theta), sin(theta))
  pos2 <- center + (l / 2) * c(cos(theta), sin(theta))
  xs <- center[1] + c(-1, 1) * (l / 2) * cos(theta)
  ys <- center[2] + c(-1, 1) * (l / 2) * sin(theta)
  lines(xs, ys, lwd = lwd, col = col)
}

# snow function:
snow <- function(np = 10, x0, x1, y0, y1) {
  u <- runif(np, x0, x1)
  v <- runif(np, y0, y1)
  points(u, v, pch = 8, lwd = 1, cex = 0.1, col = rainbow(180)[90])
}


################################################################################
################################################################################
###
###  auxiliar functions for xmas 2016
###
################################################################################
################################################################################

# function to randomly get a word of given length:
getword <- function(data, numchars) {
  w <- data$word[data$nc == numchars]
  res <- as.character(sample(w, 1))
  return(res)
}

################################################################################
################################################################################
###
###  auxiliar functions for xmas 2017
###
################################################################################
################################################################################

plottrunk <- function() {
  jf <- 3
  b <- 0.2
  h <- 1
  np <- 10
  b1 <- seq(b / 1.5, -b / 1.5, length.out = np)
  b2 <- seq(-b / 1.5, -b / 2, length.out = np)
  b2 <- jitter(b2, factor = jf)
  b3 <- seq(-b / 2, b / 2, length.out = np)
  b4 <- seq(b / 2, b / 1.5, length.out = np)
  b4 <- jitter(b4, factor = jf)
  h1 <- rep(-h, np)
  h1 <- jitter(h1, factor = jf / 4)
  h2 <- seq(-h, 1, length.out = np)
  h3 <- rep(1, np)
  h4 <- h2[np:1]
  polygon(c(b1, b2, b3, b4), c(h1, h2, h3, h4), col = "brown", border = NA)
  x0 <- runif(4, -b / 2, b / 2)
  y0 <- runif(4, 0.6 - h , 0.6)
  lines(x0, y0, type = "p", pch = "|", cex = 2, col = "orangered4")
}

########################

plotBalls <- function(x, y, nballs, ballscolor) {
  if (length(x) > nballs) {
    tosel <- sample(1:length(x), size = nballs)
    x <- x[tosel]
    y <- y[tosel]
  }
  tballs <- 3 / nballs
  for (i in 1:nballs) {
    Sys.sleep(tballs)
    points(x[i], y[i], col = sample(ballscolor, size = 1), pch = 19,
           cex = runif(n = 1, 0.8, 1.5))
  }
  Sys.sleep(1)
}

########################

plotTreePiramidal <- function(nballs, ballscolor) {
  jf <- 10
  x1 <- seq(-1, 1, length.out = 100)
  y1 <- 2 * (1 - abs(x1))
  x2 <- -x1
  y2 <- rep(0, length(x2))
  x <- c(x1, x2)
  y <- c(y1, y2)
  polygon(jitter(x, factor = jf), jitter(y, factor = jf), col = "forestgreen",
          border = NA)
  ##############
  if (nballs > 0) {
    areafraction = 1 / 2
    n <- round(4 * nballs / areafraction)
    xb <- runif(n, -1, 1)
    yb <- runif(n, 0, 2)
    tosel <- yb <= 2 * (1 - abs(xb))
    plotBalls(x = xb[tosel], y = yb[tosel], nballs, ballscolor = ballscolor)
  }
}


########################

plotTreeOval <- function(nballs, ballscolor) {
  jf <- 60
  a <- 0.7
  b <- 1
  cx <- 0
  cy <- 1
  phi <- seq(0, 2 * pi, length.out = 40)
  x <- cx + a * cos(phi)
  y <- cy + b * sin(phi)
  polygon(jitter(x, factor = jf), jitter(y, factor = jf), col = "forestgreen", border = NA)
  ##############
  if (nballs > 0) {
    areafraction = pi / 4
    n <- round(4 * nballs / areafraction)
    xb <- runif(n, cx - a, cx + a)
    yb <- runif(n, cy - b, cy + b)
    tosel <- ((xb - cx) / a)^2 + ((yb - cy) / b)^2 <= 1
    plotBalls(x = xb[tosel], y = yb[tosel], nballs, ballscolor = ballscolor)
  }
}


########################

plotTreeVshaped <- function(nballs, ballscolor) {
  jf <- 10
  phi <- pi / 3
  x <- seq(-1, 1, length.out = 100)
  y <- abs(x * tan(phi))
  x <- jitter(x, factor = jf)
  y <- jitter(y, factor = jf)
  w <- seq(phi, pi - phi, length.out = 50)
  x2 <- cos(w) / cos(phi)
  y2 <- sin(w) / cos(phi)
  x2 <- jitter(x2, factor = jf)
  y2 <- jitter(y2, factor = jf)
  polygon(c(x, x2), c(y, y2), col = "forestgreen", border = NA)
  ##############
  if (nballs > 0) {
    areafraction = (pi - 2 * phi) / (8 * (cos(phi))^2)
    n <- round(4 * nballs / areafraction)
    xb <- runif(n, -1, 1)
    yb <- runif(n, 0, 2)
    tosel <- (yb <= tan(phi)) & (yb >= abs(xb * tan(phi)))
    plotBalls(x = xb[tosel], y = yb[tosel], nballs, ballscolor = ballscolor)
  }
}


########################

plotTreeRound <- function(nballs, ballscolor) {
  jf <- 40
  cx <- 0
  cy <- 1
  r <- 1
  phi <- seq(0, 2 * pi, length.out = 40)
  x <- cx + r * cos(phi)
  y <- cy + r * sin(phi)
  polygon(jitter(x, factor = jf), jitter(y, factor = jf), col = "forestgreen",
          border = NA)
  ##############
  if (nballs > 0) {
    areafraction = pi / 4
    n <- round(4 * nballs / areafraction)
    xb <- runif(n, cx - r, cx + r)
    yb <- runif(n, cy - r, cy + r)
    tosel <- (xb - cx)^2 + (yb - cy)^2 <= r^2
    plotBalls(x = xb[tosel], y = yb[tosel], nballs, ballscolor = ballscolor)
  }
}


########################

plotTreeColumnar <- function(nballs, ballscolor) {
  jf <- 10
  xmin <- -0.5
  xmax <- -xmin
  ymin <- 0
  ymax <- 2
  np <- 50
  x1 <- seq(xmax, xmin, length.out = np)
  y1 <- rep(ymin, np)
  x2 <- rep(xmin, round((ymax - ymin) / (xmax - xmin) * np))
  y2 <- seq(ymin, ymax, length.out = length(x2))
  x3 <- -x1
  y3 <- rep(ymax, length(x3))
  x4 <- rep(xmax, length(x2))
  y4 <- seq(ymax, ymin, length.out = length(x4))
  x <- c(x1, x2, x3, x4)
  y <- c(y1, y2, y3, y4)
  polygon(jitter(x, factor = jf), jitter(y, factor = jf), col = "forestgreen",
          border = NA)
  ##############
  if (nballs > 0) {
    xb <- runif(nballs, xmin, xmax)
    yb <- runif(nballs, ymin, ymax)
    plotBalls(x = xb, y = yb, nballs, ballscolor = ballscolor)
  }
}


################################################################################
################################################################################
###
###  auxiliar functions for xmaspicasso
###
################################################################################
################################################################################

drawbird <- function(x, y, type = c("loess", "poly", "poly2"), col = "red",
                     lwd = 4) {
  rx <- 3.5
  ry <- 3
  x <- rx * x
  y <- ry * y
  dd <- data.frame(x, y)
  l <- length(x)
  w <- rep(1, l)
  w[c(1, l)] <- 3
  dd$w <- w
  mod <- switch(type,
                loess = loess(y ~ x, data = dd, weights = w),
                poly = lm(y ~ x + I(x^2) + I(x^3), weights = w, data = dd),
                poly2 = lm(y ~ x + I(x^2), weights = w, data = dd))
  xv <- seq(min(x), max(x), length.out = 50)
  pred <- predict(mod, newdata = data.frame(x = xv))
  lines(xv, pred, col = col, lwd = lwd)
}

