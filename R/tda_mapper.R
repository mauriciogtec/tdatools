#' @import htmlwidgets
#' @param nodes data frame with node data
#' @param links data frame with link data
#' @param height numeric height of graph
#' @param width numeric width of graph
#' @param size_var integer index or character name nodes that will be used as node size variable
#' @param color_var integer index or character name nodes that will be used as node size variable
# tda_mapper <- function(nodes,
#                       edges,
#                       size_var = 2,
#                       color_var = 3,
#                       height = NULL,
#                       width = NULL) {
#   # clean data format
#   nodes_clean <- data.frame(
#     id = nodes[ ,1],
#     size = nodes[ ,size_var],
#     color = nodes[ ,color_var]
#   )
#
#   # clean link/edges format
#   edges$value <- mapply(max, edges$percent_source, edges$percent_target)
#   edges_clean <- edges[edges$connected , c("source", "target", "value")]
#
#   # create data
#   x <- list(
#     "nodes" = nodes_clean,
#     "links" = edges_clean
#   )
#
#   # create options
#   options = list()
#
#   # create widget
#   htmlwidgets::createWidget(
#     name = "tda_mapper",
#     x = x,
#     width = width,
#     height = height,
#     htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
#     package = "tdatools"
#   )
# }

tda_mapper <- function(filter, color="id", size="count", info = NULL, height = NULL, width = NULL) {
  # links
  links <- edge_info(filter)
  links <- links[links$connected, ]
  links <- data.frame(
    source = links$source,
    target = links$target,
    value = mapply(max, links$percent_source, links$percent_target)
  )

  # nodes
  nodes <- data.frame(
    id = filter$stats$id,
    size = filter$stats[ ,size],
    color = filter$stats[ ,color]
  )

  # info
  if(!is.null(info)) {
    if (is.numeric(info)) {
      info <- names(filter$stats)[info]
    }
    dinfo <- filter$stats[ ,c("id", info)]
  } else {
    dinfo <- data.frame(
      id = filter$stats$id,
      rel_size = percent(filter$stats$count / sum(filter$stats$count))
    )
  }
  tabd<- function(d) {
    out <- "<table>"
    for (i in 1:nrow(d)) {
      out <- paste0(out, "<tr>")
      for (j in 1:ncol(d)) {
        out <- paste0(out, "<td>",d[i,j],"</td>")
      }
      out <- paste0(out, "</tr>")
    }
    out <- paste0(out, "</table>")
    out
  }
  nodes$info <- apply(dinfo, 1, function(row) {
    tabd(data.frame(c("id", info), row))
  })


  # create data
  x <- list(
    "nodes" = nodes,
    "links" = links
  )

  # create options
  options = list()

  # create widget
  htmlwidgets::createWidget(
    name = "tda_mapper",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
    package = "tdatools"
  )
}

#' @rdname tdamapper-shiny
#' @export
tda_mapperOutput <- function(outputId, width = "100%", height = "500px") {
  shinyWidgetOutput(outputId, "tda_mapper", width, height,
                    package = "tdatools")
}

#' @rdname tdamapper-shiny
#' @export
renderTda_mapper <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, tdamapperOutput, env, quoted = TRUE)
}
