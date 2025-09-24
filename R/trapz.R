trapz <- function(x, y) {
  sum(diff(x) * (utils::head(y, -1) + utils::tail(y, -1)) / 2)
}
