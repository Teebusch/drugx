prettyRange <- function(x) {
  r <- range(x)
  c(floor(r[1]), ceiling(r[2]))
}