cluster <- function(filter, data, control = list(k = 3)) {
  # for the moment only dbscan suported... intend to change this...
  numeric_cols <- sapply(1:ncol(data), function(j) is.numeric(data[[j]]))
  cluster_sublists <- lapply(filter$groups, function(idx) {
    k <- min(control$k, length(idx))
    knn <- kNNdist(data[idx, numeric_cols], k = k)
    eps <- as.numeric(apply(knn, 2, mean))[k]
    cl <- dbscan(data[idx, numeric_cols], minPts = k, eps = eps)
    clusts <- unique(cl$cluster[cl$cluster != 0])
    noise <- which(cl$cluster == 0)
    li <- list()
    for (i in noise) {
      li <- c(li, list(i))
    }
    for (i in clusts) {
      li <- c(li, list(idx[which(cl$cluster == clusts[i])]))
    }
    li
  })
  filter$groups <- do.call("c", cluster_sublists)
  filter$stats <- data.frame(
    id = 1:length(filter$groups),
    count = sapply(filter$groups, length)
  )
  filter
}

