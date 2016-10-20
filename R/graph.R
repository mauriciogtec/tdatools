tda_edges <- function(tda_filter) {
  groups <- tda_filter$groups
  n <- length(groups)
  info <- data.frame()
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      inter <- sum(groups[[i]] %in%  groups[[j]])
      info <- rbind(info, data.frame(
        source = i,
        target = j,
        connected = (inter > 0),
        percent_source = inter / length(groups[[i]]),
        percent_target = inter / length(groups[[j]])
      ))
    }
  }
  info
}

tda_nodes <- function(tda_filter) {
  n <- length(tda_filter$groups)
  count <- sapply(tda_filter$groups, length)
  size <- count / sum(count)
  info <- data.frame(
    node = 1:n,
    size = size
  )
  if(!is.null(tda_filter$filter_stats)) {
    info <- cbind(info, tda_filter$data_stats)
  }
  if(!is.null(tda_filter$data_stats)) {
    info <- cbind(info, tda_filter$data_stats)
  }
  info
}
