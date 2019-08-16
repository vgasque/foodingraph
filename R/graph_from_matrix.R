#' Display a graph from an adjacency matrix
#'
#' Given an adjacency matrix and a legend, displays the graph.
#' This is a shortcut function, rather than using \code{links_nodes_from_mat()}
#' and \code{graph_from_links_nodes()}.
#'
#' @param adjacency_matrix : a matrix of size n x n, each element being an number
#'  explaining the relationship (coefficient, information)
#'  between two variables given in the column and row names
#'  /!\ As this code is to draw undirected graphs, only the lower triangular part of
#'   association matrix is used to extract the information
#' @param legend : a data frame of columns in order :
#'   + name (str) : name of the node in the association matrix (e.g. CRUDSAL_cat)
#'   + title (str) : name of the node (e.g. Raw vegetables)
#'   + family (factor, str), optional : the family the node belongs to (e.g. Vegetables)
#' @param threshold (numeric) : a number defining the minimal threshold. If the weights are
#'  less than this threshold, they will be set to 0. Default is 0.
#' @param abs_threshold (bool) : should the threshold keep negative values (e.g. if abs_threshold
#'  is set to TRUE, and threshold is set to 0.1, all weights between -0.1 and 0.1 will be set to 0).
#'  Default is TRUE.
#' @param filter_nodes (bool) : should the variables not in the association matrix be displayed on the graph?
#'  CAREFUL : if set to TRUE (default), be sure to have the same colors in the family legend
#'  of the graphs (a fixed palette can be set using the \code{generate_family_palette()} func.).
#'  Default is TRUE.
#' @param ... parameters passed to \code{\link{graph_from_links_nodes}}
#' @examples
#' adj_matrix <- cor(iris[,-5])
#' legend <- data.frame(name = colnames(iris[,-5]),
#'                      title = colnames(iris[,-5]))
#' graph_from_matrix(adj_matrix, legend, main_title = "Iris graph")
#' @seealso \code{\link{graph_from_links_nodes}}
#' @export
graph_from_matrix <- function(adjacency_matrix,
                              legend,
                              threshold = 0,
                              abs_threshold = T,
                              filter_nodes = T,
                              ...) {

  # Extract the links and nodes from the adjacency matrix
  network_link_nodes <- links_nodes_from_mat(adjacency_matrix = adjacency_matrix,
                                            legend = legend,
                                            threshold = threshold,
                                            abs_threshold = abs_threshold,
                                            filter_nodes = filter_nodes)

  # Display the graph
  graph_from_links_nodes(network_link_nodes, ...)
}
