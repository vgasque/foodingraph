#' Save graph
#'
#' Save the graph generated from \code{graph_from_matrix()} or \code{graph_from_links_nodes()}.
#'
#' @param graph : the graph
#' @param filename (optional) : the name of the file. Default is the igraph unique ID in a .png format
#' @param width (optional) : width of the image in cm. Default is 25 cm
#' @param height (optional) : height of the image in cm. Default is 20 cm
#' @param dpi (optional) : the resolution of the image in dpi. Default is 300
#' @param ... : other parameters to pass to the \code{ggsave} ggplot2 function
#' @examples
#' adj_matrix <- cor(iris[,-5])
#' legend <- data.frame(name = colnames(iris[,-5]),
#'                      title = colnames(iris[,-5]))
#' graph_iris <- graph_from_matrix(adj_matrix, legend, main_title = "Iris graph")
#' save_graph(graph_iris)
#' @importFrom ggplot2 ggsave
#' @importFrom igraph graph_id
#' @export
save_graph <- function(graph,
                       filename = NULL,
                       width = 25,
                       height = 20,
                       dpi = 300,
                       ...) {

  if (is.null(graph$igraph) || is.null(graph$net)) {
    stop("Use graphs created by graph_from_matrix() or graph_from_links_nodes(). Or use ggsave()")
  }

  # Is filename is not given, uses the igraph ID
  filename <- ifelse(is.null(filename),
                     paste0("graph_", graph_id(graph$igraph), ".png"),
                     filename)

  ggsave(filename, graph$net, width = width, height = height, units = "cm", dpi = dpi, ...)
}
