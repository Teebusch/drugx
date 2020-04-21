pretty_range <- function(x) {
  r <- range(x)
  c(floor(r[1]), ceiling(r[2]))
}

between_range <- function(x, r) {
  between(x, r[1], r[2])
}
