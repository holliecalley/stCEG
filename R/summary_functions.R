#' Summary of an Event Tree
#'
#' This function provides a summary of an object of class `event_tree`. It outputs key information about the nodes and edges of the event tree, including the number of nodes and edges, unique levels and labels, and the labels of the edges (with newlines replaced by spaces).
#'
#' @param object An object of class `event_tree`. This object should have a list structure with `eventtree` containing a nested list where `x` is a dataframe with `nodes` and `edges` attributes.
#' 
#' @return Prints a summary of the event tree to the console, including:
#'   - The number of nodes and the unique node levels
#'   - The number of edges and the unique labels in the edges
#'   - The labels of the edges, with newlines replaced by spaces
#' 
#' @examples
#' # Example of using the summary function with an event_tree object
#' summary(event_tree_object)
#' 
#' @export
summary.event_tree <- function(object) {
  # Ensure the object is of class 'event_tree'
  if (!inherits(object, "event_tree")) {
    stop("Object must be of class 'event_tree'")
  }
  
  # Extract the event tree structure
  object <- object$eventtree$x
  
  # Summarize the nodes
  cat("Summary of Nodes:\n")
  cat("Number of nodes:", nrow(object$nodes), "\n")
  cat("Unique node levels:", length(unique(object$nodes$level)), "\n")
  
  # Summarize the edges
  cat("\nSummary of Edges:\n")
  cat("Number of edges:", nrow(object$edges), "\n")
  cat("Unique labels in edges:", length(unique(object$edges$label1)), "\n")
  
  # Replace newlines with spaces in edge labels before printing  # Use newline to separate labels
  edge_labels <- gsub("\n", " ", unique(object$edges$label1))
  edge_labels <- paste(edge_labels, collapse = "\n") # Replace newlines with spaces in the labels themselves
  
  # Print each label on a new line
  cat("Edge labels:\n", edge_labels)
  
}



summary.staged_tree <- function(object, ...) {
  nodes <- object$stagedtree$x$nodes
  edges <- object$stagedtree$x$edges
  
  # Basic summary counts
  num_nodes <- nrow(nodes)
  num_edges <- nrow(edges)
  
  # Level range
  min_level <- min(nodes$level, na.rm = TRUE)
  max_level <- max(nodes$level, na.rm = TRUE)
  
  # Colour count
  color_counts <- table(nodes$color)
  
  # White nodes not in min or max level
  num_uncoloured_middle <- sum(
    nodes$color == "#FFFFFF" &
      !(nodes$level %in% c(min_level, max_level))
  )
  
  cat("Summary of Staged Tree Object\n")
  cat("=============================\n")
  cat("Total nodes:", num_nodes, "\n")
  cat("Total edges:", num_edges, "\n")
  cat("Nodes left to be coloured:", num_uncoloured_middle, "\n")
  
  # Load crayon
  if (!requireNamespace("crayon", quietly = TRUE)) {
    cat("Install the 'crayon' package to view colored output.\n")
  } else {
    hex_to_style <- function(hex) {
      rgb <- grDevices::col2rgb(hex)
      crayon::make_style(rgb, bg = TRUE)
    }
    
    cat("\nNode colour counts:\n")
    for (hex in names(color_counts)) {
      style <- hex_to_style(hex)
      cat(style(sprintf("     ")), " ", hex, sprintf(" (%d nodes)\n", color_counts[[hex]]))
    }
  }
  
  if (!is.null(object$priortable)) {
    cat("\nPrior Table:\n")
    print(object$priortable)
  }
  
}


# Example usage
summary(homicides_AHC)


