#' Display a graph from a list of links and nodes
#'
#' Given a list of links and nodes (e.g. from extract_links_nodes func)
#' Uses igraph and ggraph to display the network plots
#' Must have the proper structure OR use \code{extract_links_nodes()}, which automatically returns
#' this structure when given an adjacency matrix and its legend (see documentation for this function)
#' network_data should be a list of 2 : edges, nodes
#' For edges (data.frame) : from, to, weight, width, sign (of the width: neg/pos)
#' For nodes (data.frame) : name, title, family, family_color (optional)
#'
#' @param network_data (list of two) : links, nodes with the proper structure
#' @param main_title (string, optional) : the title of the network
#' @param ret (bool, default TRUE) : should the network graph and degree table be returned or directly displayed
#' @param node_label_title (bool, default F) : should the node labels be the names or title column?
#'    (e.g. names : CRUDSAL_cat, title : Raw vegetables)
#' @param family_palette (list of key = value) : the keys are the family codes (from family column in the legend),
#'  and the values are the corresponding colors. Can be generated using the \code{generate_family_palette()} func.
#'  USEFUL if there is a need to compare multiple graphs of the same families, so the color is consistent.
#'  If NULL (default), the palette will be automatically generated using viridis
#' @param layout (chr) : the layout to be used to construct the graph
#' @param remove_null (bool) : should the nodes with 0 connections (degree 0) be removed from the graph.
#'  default is TRUE.
#' @param alpha_edge (bool) : should the edges have a transparent scale? In addition to the width scale.
#' @param edge_width_range : range of the edges width. (default is 0.2 to 2)
#' @param edge_alpha_range : if \code{alpha_edge} is TRUE, the range of the alpha values (between 0 and 1).
#' Default is 0.3 to 1.
#' @param node_label_size : the size of the node labels. Default is 3.
#' @param legend_label_size : the size of the legend labels. Default is 10.
#' @param ... : other parameters to pass to ggraph `create_layout`
#' @examples
#' adj_matrix <- cor(iris[,-5])
#' legend <- data.frame(name = colnames(iris[,-5]),
#'                      title = colnames(iris[,-5]))
#' graph_iris <- links_nodes_from_mat(adj_matrix, legend)
#' graph_from_links_nodes(graph_iris, main_title = "Iris graph")
#' @seealso \code{\link{graph_from_matrix}}
#' @importFrom dplyr arrange desc
#' @importFrom viridis scale_fill_viridis
#' @importFrom utils txtProgressBar setTxtProgressBar capture.output
#' @importFrom stats quantile
#' @importFrom rlang .data
#' @import ggplot2
#' @import igraph
#' @import ggraph
#' @export
graph_from_links_nodes <- function(network_data,
                                   main_title = "",
                                   ret = T,
                                   node_label_title = T,
                                   family_palette = NULL,
                                   layout = "nicely",
                                   remove_null = T,
                                   alpha_edge = T,
                                   edge_width_range = c(0.2,2),
                                   edge_alpha_range = c(0.3, 1),
                                   node_label_size = 3,
                                   legend_label_size  = 10,
                                   ...) {

  if (nrow(network_data$links) == 0) {
    stop("No edges. Can't produce a graph")
  }

  # Create the igraph object
  network_igraph <- graph_from_data_frame(d = network_data$links,
                                          vertices = network_data$nodes,
                                          directed = F)

  # To compute node degrees and use it to set node size
  # (node degree is the number of edges linked to each node)
  deg <- degree(network_igraph)
  V(network_igraph)$size <- deg

  # Create a data frame of two colums : the node name and its degrees
  deg_table <- data.frame(
    name = V(network_igraph)$title,
    degrees = V(network_igraph)$size
  )
  deg_table <- arrange(deg_table, desc(.data$degrees))

  # To remove unconnected nodes
  if (remove_null == T) {
    network_igraph <- delete_vertices(network_igraph, deg == 0)
  }

  if (alpha_edge == T) {
    # Set the alpha value
    E(network_igraph)$alpha <- E(network_igraph)$width
  }


  # Display either the node IDs (name) or title on the graph
  if (node_label_title == T) {
    node_label <- V(network_igraph)$title
  } else {
    node_label <- V(network_igraph)$name
  }

  # For the graph layout : the method to display graph edges and nodes in the plot
  network_layout <- create_layout(network_igraph, layout=layout, ...)

  # Display the graph
  net <- ggraph(network_layout)

  # Edges
  if (alpha_edge == T) {
    net <- net +
      geom_edge_link(aes(width = .data$width, color = .data$sign, alpha = .data$alpha)) +
      scale_edge_alpha(name = "Absolute weight", range = edge_alpha_range)
  } else {
    net <- net +
      geom_edge_link(aes(width = .data$width, color = .data$sign))
  }

  # Edges color and legend
  net <- net +
    scale_edge_color_manual(name = "Edge sign",
                            values = c("Negative" = "#00558a", "Positive" = "#910101")) +
    scale_edge_width(name = "Absolute weight", range = edge_width_range)

  # Nodes
  if (is.null(V(network_igraph)$family)) {
    net <- net +
      geom_node_point(aes(size = .data$size))
  } else {
    net <- net +
      geom_node_point(aes(fill = .data$family, size = .data$size), color = "black", shape = 21)

    # Nodes color and legend
    if (is.null(family_palette)) {
      net <- net +
        scale_fill_viridis(discrete = T, name = "Food families")
    } else {
      net <- net +
        scale_fill_manual(values = family_palette, name = "Food families")
    }
  }

  # Node labels
  net <- net +
    geom_node_text(aes(label = node_label), size = node_label_size, repel = T) +
    labs(size = "Node degree") +

    # Legend order
    guides(
      fill = guide_legend(order = 1),
      size = guide_legend(order = 2),
      color = guide_legend(order = 3),
      alpha = guide_legend(order = 4),
      width = guide_legend(order = 5)
    ) +

    # Main theme
    ggtitle(main_title)  +
    theme_graph(base_family = "sans",
                base_size = legend_label_size,
                title_size = 13,
                plot_margin = margin(15, 15, 15, 15)
    )

  list(
      igraph = network_igraph,
      net =  net,
      deg = deg_table
  )
}
