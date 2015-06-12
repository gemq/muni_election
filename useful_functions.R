checkVar <- function(x){
  typeof <- typeof(x)
  mode <- mode(x)
  class <- class(x)
  dimensions <- dim(x)
  result <- list(typeof=typeof, mode=mode, class=class, dimensions=dimensions)
  return(result)
}

stats <- function(x){
  list(mean = mean(x), sdev = sd(x), range = range(x), variance = var(x))
}

norm <- function(n=1000, mu=0, sig=1, ...){
  x <- rnorm(n, mu, sig)
  y <- dnorm(x, mu, sig)
  plot(x, y, ...)
}

