tda_filter <- function(data,
                       n_int = 30,
                       overlap = .1,
                       cover_type = c("unif_length", "unif_weight"),
                       filter_stats = NULL,
                       data_stats = NULL) {
  # for the moment only svd supported....
  allowed_type <- c("unif_length", "unif_weight")
  cover_type <- cover_type[1]
  if (!(cover_type %in% c("unif_length", "unif_weight"))) {
    stop("'cover_type' must be one of the currently allowed type; see function help.")
  }
  is_numeric <- sapply(1:ncol(data), function(j) is.numeric(data[[j]]))
  out <- svds(as.matrix(data[ ,is_numeric]), k = 1)$v
  var <- as.matrix(data[ ,is_numeric]) %*% out
  cover <- switch(
    EXPR = cover_type,
    "unif_length" = unif_length_cover(min(var), max(var), n_int = n_int, overlap = overlap),
    "unif_weight" = unif_weight_cover(var, n_int = n_int, overlap = overlap)
  )
  li <- list(
    groups = assign_cover(var, cover),
    cover = cover, filter = data.frame(svd = var),
    stats = data.frame(count = sapply(filter$groups, length))
  )
  attr(li, "class") <- "tda_filter"
  li
}

