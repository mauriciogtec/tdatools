#' @import htmlwidgets
#' @param nodes data frame with node data
#' @param links data frame with link data
#' @param height numeric height of graph
#' @param width numeric width of graph
#' @param size_var integer index or character name nodes that will be used as node size variable
#' @param color_var integer index or character name nodes that will be used as node size variable
tdamapper <- function(nodes,
                      edges,
                      size_var = 2,
                      color_var = 3,
                      height = NULL,
                      width = NULL) {
  # clean data format
  nodes_clean <- data.frame(
    id = nodes[ ,1],
    size = nodes[ ,size_var],
    color = nodes[ ,color_var]
  )

  # clean link/edges format
  edges$value <- mapply(max, edges$percent_source, edges$percent_target)
  edges_clean <- edges[edges$connected , c("source", "target", "value")]

  # create data
  x <- list(
    "nodes" = nodes_clean,
    "links" = edges_clean
  )

  # create options
  options = list()

  # create widget
  htmlwidgets::createWidget(
    name = "tdamapper",
    x = x,
    width = width,
    height = height,
    htmlwidgets::sizingPolicy(padding = 10, browser.fill = TRUE),
    package = "tdamapper"
  )
}

#' @rdname tdamapper-shiny
#' @export
tdamapperOutput <- function(outputId, width = "100%", height = "500px") {
  shinyWidgetOutput(outputId, "tdamapper", width, height,
                    package = "tdamapper")
}

#' @rdname tdamapper-shiny
#' @export
renderTdamapper <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, tdamapperOutput, env, quoted = TRUE)
}
