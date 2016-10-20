### coverHandling.R ###

unif_length_cover <- function(lower, upper, n_int = 30, overlap = .1) {
  int_length <- (upper - lower)/n_int
  step <- int_length * overlap / 2
  data.frame(
    lower = lower + (1:n_int - 1)*int_length - step,
    upper = lower + (1:n_int)*int_length + step
  )
}

unif_weight_cover <- function(var, n_int = 30, overlap = .1) {
  if (n_int > length(var)) {
    stop("the default or selected size of n_int must exceed the length of var")
  }
  lower <- quantile(var, probs = 0:n_int / n_int)
  int_length <- lower[-1] - lower[-(n_int + 1)]
  step <- int_length * overlap / 2
  data.frame(
    lower = lower[-(n_int + 1)] - step,
    upper = lower[-1] + step
  )
}

assign_cover <- function(var, cover) {
  li <- lapply(1:nrow(cover), function(i) {
    which(cover$lower[i] <= var & var <= cover$upper[i])
  })
  li[sapply(li, length) > 0]
}

identify_cover <- function(var, cover) {
  li <- lapply(1:length(var), function(i) {
    which(cover$lower <= var[i] & var[i] <= cover$upper)
  })
  li[sapply(li, length) > 0]
}
