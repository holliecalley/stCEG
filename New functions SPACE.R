library(leaflet)
library(viridis)
library(sf)  # For handling shapefiles
library(dplyr)

calculate_path_products <- function(nodes_df, edges_df, root_node = "w0") {
  
  # Initialize a list to store paths and products
  paths_list <- list()
  
  # Find all paths from root node (could use graph traversal here)
  traverse_paths <- function(node, path, product) {
    # Find outgoing edges from current node
    next_edges <- edges_df[edges_df$from == node, ]
    
    if (nrow(next_edges) == 0) {
      # If no outgoing edges, it's a terminal node, save the path and product
      paths_list <<- append(paths_list, list(list(path = path, product = product)))
    } else {
      # Otherwise, traverse the next nodes
      for (i in 1:nrow(next_edges)) {
        # Access the posteriormean from the edge
        posteriormean <- as.numeric(next_edges$posterior_mean[i])
        
        # Append the label1 (from edges_df) to the path
        new_path <- c(path, next_edges$label1[i])
        
        # Continue traversal with the new node
        traverse_paths(next_edges$to[i], new_path, product * posteriormean)
      }
    }
  }
  
  # Start traversal from the root node
  traverse_paths(root_node, path = character(0), product = 1)
  
  # Convert list of paths and products into a data frame
  path_df <- do.call(rbind, lapply(paths_list, function(x) data.frame(path = paste(x$path, collapse = " -> "), product = x$product)))
  
  return(path_df)
}

calculate_conditional_prob <- function(path_df, unique_values, selected_indices, last_group) {
  # Ensure unique_values is a character vector
  unique_values <- as.character(unique_values)
  
  # Group values by their assigned index
  grouped_conditions <- split(unique_values, selected_indices)
  
  # Step 1: Filter paths directly based on AND/OR logic
  condition_paths <- path_df[sapply(path_df$path, function(path) {
    path_components <- unlist(strsplit(path, " -> "))  # Convert path to vector
    
    # Apply AND/OR logic for each group
    all(sapply(grouped_conditions, function(group) {
      if (length(group) == 1) {
        group %in% path_components  # AND condition: must be present
      } else {
        any(group %in% path_components)  # OR condition: at least one must be present
      }
    }))
  }), , drop = FALSE]  # Prevent accidental list conversion
  
  # If no matching paths exist, return 0 probability
  if (nrow(condition_paths) == 0) {
    print(paste("P(", last_group, "|", paste(unique_values, collapse = ", "), ") = 0"))
    return(0)
  }
  
  # Step 2: Compute joint probability P(last_group and unique_values)
  joint_prob <- sum(condition_paths$product[sapply(condition_paths$path, function(path) {
    last_group %in% unlist(strsplit(path, " -> "))
  })])
  
  # Step 3: Compute marginal probability P(unique_values)
  marginal_prob <- sum(condition_paths$product)
  
  # Step 4: Compute conditional probability P(last_group | unique_values)
  conditional_prob <- ifelse(marginal_prob > 0, joint_prob / marginal_prob, 0)
  
  print(paste("P(", last_group, "|", paste(unique_values, collapse = ", "), ") = ", conditional_prob, sep = ""))
  return(conditional_prob)
}


calculate_area_probabilities <- function(path_df, unique_values, selected_indices, last_group, shapefile_vals) {
  area_probs <- list()  # Store probabilities for each area
  
  # Loop over each area in shapefile_vals
  for (area in shapefile_vals) {
    # Filter paths for the current area
    area_paths <- path_df[sapply(path_df$path, function(path) {
      area %in% unlist(strsplit(path, " -> "))  # Ensure area is in path
    }), , drop = FALSE]
    
    # If no paths exist for this area, assign probability 0
    if (nrow(area_paths) == 0) {
      area_probs[[area]] <- NA
    } else {
      # Compute probability for the given area
      area_probs[[area]] <- calculate_conditional_prob(area_paths, unique_values, selected_indices, last_group)
    }
  }
  
  return(area_probs)  # Return a named list of probabilities
}

# Define the function
generate_CEG_map <- function(shapefile, ceg_object, conditionals = unique(ceg_object$x$edges$label1), colour_by = NULL, color_palette = "viridis") {
  
  nodes <- ceg_object$ceg$x$nodes
  edges <- ceg_object$ceg$x$edges
  
  
  # If start_label1 is NULL, set it to the first label1 with max level
  if (is.null(colour_by)) {
    max_level <- max(edges$level, na.rm = TRUE)
    max_level_rows <- edges[edges$level == max_level, ]
    colour_by <- max_level_rows$label1[1]  # First label1 with max level
  }
  
  # Step 1: Load Shapefile Data
  shape_data <- shapefile
  shape_data <- st_transform(shape_data, crs = 4326)
  # Step 2: Calculate Path Products (Assume your function is loaded)
  path_df <- calculate_path_products(nodes, edges)
  
  get_levels_from_conditionals <- function(conditionals, edges_df) {
    levels <- edges_df %>%
      filter(label1 %in% conditionals) %>%  # Filter for given label1 values
      group_by(label1) %>%
      slice(1) %>%  # Take the first row for each label1
      ungroup() %>%
      mutate(label1 = factor(label1, levels = conditionals)) %>%  # Maintain original order
      arrange(label1) %>%  # Arrange by the factor to keep input order
      pull(level)  # Extract the level column
    
    return(levels) # Return levels in the correct order
  }
  
  selected_indices <- get_levels_from_conditionals(conditionals, edges)
  
  # Step 3: Calculate Area Probabilities 
  area_probs <- calculate_area_probabilities(
    path_df, conditionals, selected_indices, colour_by, shape_data[[1]]
  )
  
  # Convert area probabilities to a named vector
  area_probs_vec <- unlist(area_probs)
  
  # Match areas and assign probabilities to shapefile data
  shape_data$area_probs <- area_probs_vec[shape_data[[1]]]
  
  # Step 4: Assign Colors Based on Area Probabilities
  assign_colors <- function(probs, palette_name) {
    color_func <- colorNumeric(viridis(100, option = palette_name), domain = c(1, 0))
    
    sapply(probs, function(prob) {
      if (is.na(prob)) "#FFFFFF" else color_func(prob)  # White for NA values
    })
  }
  
  # Assign colors based on area probabilities
  shape_data$color_assignment <- assign_colors(shape_data$area_probs, color_palette)
  
  # Step 5: Generate the Leaflet Map
  leaflet(data = shape_data) %>%
    addTiles() %>%
    addPolygons(
      layerId = shape_data[[1]],  
      fillColor = shape_data$color_assignment,
      color = "black",
      weight = 1,
      highlightOptions = highlightOptions(
        weight = 1, fillOpacity = 0.7, bringToFront = TRUE
      ),
      opacity = 1, fillOpacity = 0.7,
      label = ~paste0(as.character(shape_data[[1]]), ": ", round(shape_data$area_probs, 3))
    ) %>%
    addLegend(
      pal = colorNumeric(viridis(100, option = color_palette), domain = c(0, 1)), 
      values = c(0, 1),
      title = "Probability", position = "bottomright",
      labFormat = labelFormat(transform = function(x) round(x, 2))
    )
}


create_reduced_CEG <- function(ceg_object, start_labels, level_separation = 1200, node_distance = 300) {
  
  extract_floret <- function(nodes, edges, start_label1) {
    # Find all 'from' edges associated with the start_label1
    start_edges <- edges[edges$label1 == start_label1, ]
    
    if (nrow(start_edges) == 0) {
      return(list(nodes = data.frame(), edges = data.frame()))
    }
    
    # Initialize sets for floret nodes and edges
    floret_nodes <- list()
    floret_edges <- data.frame()
    visited_nodes <- c()
    
    # Recursive function to traverse and collect connected nodes and edges
    collect_floret <- function(current_node) {
      # Find edges starting from the current node
      outgoing_edges <- edges[edges$from == current_node, ]
      
      # Eliminate edges leading into or before the starting node
      outgoing_edges <- outgoing_edges[outgoing_edges$to != current_node, ]
      
      # Add the new edges to the floret set
      if (nrow(outgoing_edges) > 0) {
        floret_edges <<- rbind(floret_edges, outgoing_edges)
      }
      
      # Get the 'to' nodes from these edges
      to_nodes <- outgoing_edges$to
      
      # Add new nodes to the floret set if not already added
      new_nodes <- to_nodes[!to_nodes %in% visited_nodes]
      visited_nodes <<- c(visited_nodes, new_nodes)
      
      floret_nodes <<- c(floret_nodes, setNames(as.list(new_nodes), new_nodes))
      
      # Recursively process each new node
      for (node in new_nodes) {
        collect_floret(node)
      }
    }
    
    # Process all starting edges
    for (i in seq_len(nrow(start_edges))) {
      start_node <- start_edges$to[i]
      
      if (!start_node %in% visited_nodes) {
        visited_nodes <- c(visited_nodes, start_node)
        floret_nodes <- c(floret_nodes, setNames(list(start_node), start_node))
        collect_floret(start_node)
      }
    }
    
    # Filter the nodes dataframe to include only nodes in the floret
    floret_nodes_df <- nodes[nodes$id %in% names(floret_nodes), ]
    
    list(nodes = floret_nodes_df, edges = floret_edges)
  }
  

  
  extract_florets <- function(ceg_object, start_labels) {
    nodes <- ceg_object$ceg$x$nodes
    edges <- ceg_object$ceg$x$edges
    # Initialize sets for floret nodes and edges
    all_floret_nodes <- data.frame()
    all_floret_edges <- data.frame()
    
    for (start_label in start_labels) {
      floret <- extract_floret(nodes, edges, start_label)
      
      # Combine results
      all_floret_nodes <- rbind(all_floret_nodes, floret$nodes)
      all_floret_edges <- rbind(all_floret_edges, floret$edges)
    }
    
    # Ensure unique nodes and edges
    all_floret_nodes <- unique(all_floret_nodes)
    all_floret_edges <- unique(all_floret_edges)
    
    list(nodes = all_floret_nodes, edges = all_floret_edges)
  }
  
  reduced_data <- extract_florets(ceg_object, start_labels)
  
  # Generate the visNetwork visualization
  network_plot <- visNetwork(nodes = reduced_data$nodes, edges = reduced_data$edges) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = level_separation) %>%
    visNodes(scaling = list(min = 10, max = 10), font = list(vadjust = -170), fixed = TRUE) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
    visOptions(
      manipulation = list(
        enabled = FALSE,
        addEdgeCols = FALSE,
        addNodeCols = FALSE,
        editEdgeCols = FALSE,
        editNodeCols = c("color"),
        multiselect = TRUE
      ),
      nodesIdSelection = FALSE
    ) %>%
    visInteraction(
      dragNodes = TRUE,
      multiselect = TRUE, 
      navigationButtons = TRUE
    ) %>%
    visPhysics(hierarchicalRepulsion = list(nodeDistance = node_distance), stabilization = TRUE) %>%
    visEvents(
      selectNode = "function(params) { /* Node selection code */ }",
      deselectNode = "function(params) { /* Deselect code */ }"
    ) %>%
    visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }") %>%
    visEvents(
      selectNode = "function(params) {
        var selectedNodeIds = params.nodes; // Array of selected node IDs

        // Store the original colours of the edges
        var edges = this.body.data.edges.get();
        edges.forEach(function(edge) {
          if (edge.originalcolour === undefined) {
            edge.originalcolour = edge.color; // Store the original edge colour
          }
          if (edge.originalFontcolour === undefined) {
            edge.originalFontcolour = (edge.font && edge.font.color) || '#000000'; // Store the original label colour
          }
        });

        // Reset all edges to their original colours
        this.body.data.edges.update(edges.map(function(edge) {
          edge.color = edge.originalcolour || '#000000'; // Reset to original or default black
          edge.font = { colour: edge.originalFontcolour || '#000000' }; // Reset to original or default black
          return edge;
        }));

        // Highlight edges based on selected nodes
        selectedNodeIds.forEach(function(selectedNodeId) {
          // Highlight edges going into the selected node (blue)
          var incomingEdges = this.body.data.edges.get({
            filter: function(edge) {
              return edge.to === selectedNodeId;
            }
          });
          incomingEdges.forEach(function(edge) {
            edge.color = '#0000FF'; // Set colour to blue
            edge.font = { color: '#0000FF' }; // Set label colour to blue
          });
          this.body.data.edges.update(incomingEdges);

          // Highlight edges going out from the selected node (red)
          var outgoingEdges = this.body.data.edges.get({
            filter: function(edge) {
              return edge.from === selectedNodeId;
            }
          });
          outgoingEdges.forEach(function(edge) {
            edge.color = '#FF0000'; // Set colour to red
            edge.font = { color: '#FF0000' }; // Set label colour to red
          });
          this.body.data.edges.update(outgoingEdges);
        }, this); // Bind `this` to the function to access visNetwork context

        // Redraw network to apply changes
        this.redraw();
      }",
      deselectNode = "function(params) {
        // When deselecting, reset all edges to their original colours
        var edges = this.body.data.edges.get();
        this.body.data.edges.update(edges.map(function(edge) {
          edge.color = edge.originalcolour || '#000000'; // Reset to original or default black
          edge.font = { color: edge.originalFontcolour || '#000000' }; // Reset to original or default black
          return edge;
        }));
        this.redraw();
      }"
    )%>%
    visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")
  
  # Print the network plot
  print(network_plot)
}


