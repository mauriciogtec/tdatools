add_stat <- function(filter, funs, data, rm_incomplete = TRUE) {
  if (!is.list(funs)) {
    funs <- list(funs)
  }
  if (data[[1]][1] == "filter") {
    data <- filter$filter
  }
  group_list <- filter$groups
  outer_list <- lapply(1:length(funs),  function(i) {
    fun_entry <- funs[[i]]
    fun_name <- names(funs)[i]
    if (is.null(fun_name)) {
      fun_name <- paste0("fun", i)
    } else {
      if (fun_name == "") {
        fun_name <- paste0("fun", i)
      }
    }
    inner_list <- lapply(group_list, function(idx) {
      if (!is.list(fun_entry)) {
        fun <- fun_entry
        target <- names(as.data.frame(data))
      } else {
        fun <- fun_entry[[1]]
        if (is.integer(fun_entry[[2]])) {
          target <- names(as.data.frame(data))[fun_entry[[2]]]
        } else {
          target <- fun_entry[[2]]
        }
      }
      d <- data %>%
        as.data.frame() %>%
        slice(idx) %>%
        summarise_each_(funs(fun), target)
      names(d) <- paste(target, gsub(" ", "_", fun_name), sep = "_")
      d
    })
    do.call("rbind", inner_list)
  })
  d <- do.call("cbind", outer_list)
  if (rm_incomplete) {
    incomplete <- apply(d, 2, function(x) any(is.na(x)))
    d <- d[ ,!as.logical(incomplete), drop = FALSE]
  }
  if (nrow(filter$stats) > 0) {
    filter$stats <- cbind(filter$stats, d)
  } else {
    filter$stats <- d
  }
  filter
}

