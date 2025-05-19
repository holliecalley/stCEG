#' Create a Chain Event Graph (CEG)
#'
#' This function generates a Chain Event Graph (CEG) from a staged tree object and a prior table.
#' It allows for node contraction and mapping, adjusts edge and node attributes,
#' and visualizes the graph with specific customisations for labels, colour, and node arrangement.
#'
#' @param staged_tree_obj A staged tree object containing nodes and edges. It should have the following structure:
#' \itemize{
#'   \item `staged_tree_obj$x$nodes` : A data frame of node attributes with columns such as `id`, `level`, `colour`, `label`, etc.
#'   \item `staged_tree_obj$x$edges` : A data frame of edge attributes with columns such as `from`, `to`, `label1`, `label2`, `label3`, etc.
#' }
#' @param prior_table A data frame containing prior information for each stage, including `Colour`, `Level`, `Stage`, and other relevant columns.
#' @param level_separation Numeric. The level separation value for hierarchical layout in the visualised graph. Default is 1200.
#' @param node_distance Numeric. The node distance value for hierarchical layout in the visualised graph. Default is 400.
#' @param label A character string specifying the type of label to display on edges. Options include:
#'   - `"posterior"`: Uses posterior label information.
#'   - `"posterior_mean"`: Uses posterior mean label information.
#'   - `"none"`: No labels on edges.
#'   Default is `"posterior_mean"`.
#' @param view_table Logical. Whether to display the summary table of the aggregated CEG data in the console. Default is `FALSE`.
#'
#' @return A `visNetwork` object representing the Chain Event Graph, including contracted nodes and updated edges.
#'
#' @details This function processes the staged tree and prior table, contracts nodes based on connected nodes,
#' creates aggregated edge summaries, computes posterior and prior mean values, and visualizes the CEG
#' with hierarchical layout, customizable labels, and node distance adjustments.
#'
#' The function also provides detailed printing of updated edges and contracted nodes for debugging purposes.
#'
#' @import purrr
#' @import stringr
#' @import DT
#'
#' @examples
#' # Example usage of the create_ceg function:
#' event_tree <- create_event_tree(data, c(2,4,5), "both", level_separation = 1500, node_distance = 250)  # Generate the graph data
#' coloured_tree <- calculate_ahc_colouring(event_tree)
#' tree_priors <- specify_priors(coloured_tree, prior_type = "Uniform")
#' staged_tree <- staged_tree_prior(coloured_tree, tree_priors)
#' ceg <- create_ceg(staged_tree, tree_priors, view_table = TRUE)
#'
#' @export
#'
create_ceg <- function(staged_tree_obj, level_separation = 1200, node_distance = 400, label = "posterior", view_table = FALSE) {
  nodes <- staged_tree_obj$stagedtreewithpriors$x$nodes
  edges <- staged_tree_obj$stagedtreewithpriors$x$edges
  nodes$size = 400


  # Initialize contract IDs
  nodes$contract_id <- paste0(nodes$level2, "-", nodes$color)

  # Function to update contract IDs by appending connected nodes' contract IDs
  update_contract_ids <- function(nodes, edges) {
    # Contract nodes at levels 1 and 5 separately
    nodes$contract_id[nodes$level2 == 1] <- "1-#FFFFFF"
    nodes$contract_id[nodes$level2 == max(nodes$level2)] <- paste0(max(nodes$level2), "-#FFFFFF")

    for (level2 in sort(unique(nodes$level2), decreasing = TRUE)) {
      if (level2 == 1 || level2 == max(nodes$level2)) next

      current_level_nodes <- nodes[nodes$level2 == level2, ]

      for (i in 1:nrow(current_level_nodes)) {
        node <- current_level_nodes[i, ]
        connected_edges <- edges[edges$from == node$id | edges$to == node$id, ]

        connected_nodes <- unique(c(connected_edges$from, connected_edges$to))
        connected_nodes <- connected_nodes[connected_nodes != node$id]

        if (length(connected_nodes) > 0) {
          connected_contract_ids <- nodes$contract_id[nodes$id %in% connected_nodes]
          connected_levels <- nodes$level[nodes$id %in% connected_nodes]
          higher_or_same_level_ids <- connected_contract_ids[connected_levels >= node$level2]
          nodes$contract_id[nodes$id == node$id] <- paste0(nodes$contract_id[nodes$id == node$id], "-", paste(higher_or_same_level_ids, collapse = "-"))
        }
      }
    }
    return(nodes)
  }

  # Apply contract IDs update
  nodes <- update_contract_ids(nodes, edges)


  # Contracted nodes aggregation
  contracted_nodes <- nodes %>%
    group_by(contract_id) %>%
    summarise(
      ids = paste(id, collapse = ", "),
      label = first(label),
      level = first(level2),
      color = first(color),
      #prior_variance = first(priorvariance),
      .groups = 'drop'
    )


  # Sort contracted nodes by labels numerically
  contracted_nodes <- contracted_nodes[order(as.numeric(gsub("[^0-9]", "", contracted_nodes$label))), ]

  # Reassign labels sequentially from w0 to w(n-1) and set the last label as w∞
  num_nodes <- nrow(contracted_nodes)
  contracted_nodes$label <- paste0("w", 0:(num_nodes - 1))
  contracted_nodes$label[num_nodes] <- paste0("w", "\u221E")  # Unicode for ∞
  contracted_nodes$id <- contracted_nodes$label
  contracted_nodes$font <- "80px"
  contracted_nodes$size <- 100


  # Mapping individual node IDs to contracted node IDs
  id_mapping <- lapply(1:nrow(contracted_nodes), function(i) {
    # Split the ids string into individual ids
    ids <- unlist(strsplit(contracted_nodes$ids[i], ",\\s*"))
    ids <- trimws(ids)  # Remove any extra spaces
    # Create a named vector where each id is mapped to the corresponding label
    setNames(rep(contracted_nodes$label[i], length(ids)), ids)
  })

  # Flatten the list into a single vector
  id_mapping <- unlist(id_mapping, use.names = TRUE)

  #print("contracted_nodes")
  #print(contracted_nodes)

  # Copy edges and replace IDs with contracted IDs
  updated_edges <- edges
  updated_edges$from <- id_mapping[as.character(updated_edges$from)]
  updated_edges$to <- id_mapping[as.character(updated_edges$to)]

  # Print the updated edges
  #print("Updated Edges with Contracted Node Labels:")
  #print(updated_edges)

  updated_edges <- updated_edges %>%
    select(-color) %>%
    left_join(contracted_nodes %>% select(id, color), by = c("from" = "id")) %>%  # Drop the existing 'colour' column from edges
    rename(colour_from = color)
  # Check the column names of updated_edges to ensure 'colour' is the correct name
  #print(colnames(updated_edges))



  # Merge and summarize edges
  merged_edges <- updated_edges %>%
    group_by(from, to, label1, colour_from) %>%
    summarise(
      sumlabel2 = sum(label2),
      sumlabel3 = sum(as.numeric(label3)),
      total = (sumlabel2 + sumlabel3),
      label_individuals = paste(first(label1), "\n", (sumlabel2 + sumlabel3)),
      font.size = first(font.size),
      colour_from = first(colour_from),
      .groups = 'drop'
    )

  merged_edges <- merged_edges %>%
    group_by(colour_from) %>%
    mutate(stage_total_posterior = sum(total, na.rm = TRUE)) %>%
    ungroup()

  merged_edges <- merged_edges %>%
    group_by(colour_from, label1) %>%
    mutate(posterior_total = sum(total, na.rm = TRUE)) %>%
    ungroup()

  merged_edges <- merged_edges %>%
    group_by(colour_from) %>%
    mutate(stage_total_prior = sum(sumlabel3, na.rm = TRUE)) %>%
    ungroup()

  merged_edges <- merged_edges %>%
    group_by(colour_from, label1) %>%
    mutate(prior_total = sum(sumlabel3, na.rm = TRUE)) %>%
    ungroup()

  merged_edges$prior_mean <- round(merged_edges$prior_total/merged_edges$stage_total_prior,3)

  merged_edges$posterior_mean <- round(merged_edges$posterior_total/merged_edges$stage_total_posterior,3)
  merged_edges$label_posterior = paste(merged_edges$label1, "\n", merged_edges$posterior_mean)
  merged_edges$color <- "#000000"

  curvature_values <- merged_edges %>%
    group_by(from, to) %>%
    mutate(
      curvature = seq(-0.3, 0.3, length.out = n()) # Ensure curvature is evenly spaced
    ) %>%
    ungroup()

  merged_edges$smooth <- pmap(curvature_values, function(from, to, curvature, ...) {
    list(enabled = TRUE, type = "curvedCW", roundness = curvature)
  })


  merged_edges <- merged_edges %>%
    left_join(contracted_nodes %>% select(label, level), by = c("from" = "label"))

  #assign("ceg_data", list(nodes = contracted_nodes, edges = merged_edges), envir = .GlobalEnv)
  # Return the contracted nodes and edges

  #print("testing")
  # print(contracted_nodes)
  #print(merged_edges)

  if (label == "posterior") {
    merged_edges$label <- merged_edges$label_individuals  # Assign "names" (label1)
  } else if (label == "posterior_mean") {
    merged_edges$label <-  merged_edges$label_posterior  # Assign "priors" (label_prior_frac)
  } else if (label == "none") {
    merged_edges$label <-  merged_edges$label1  # Assign "priors" (label_prior_frac)
  }

  aggregated_df <- merged_edges %>%
    group_by(colour_from, level, label1) %>%
    summarise(
      data = sum(sumlabel2, na.rm = TRUE),
      prior = sum(sumlabel3, na.rm = TRUE),
      posterior = sum(total, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(level, label1)

  aggregated_df <- aggregated_df %>%
    group_by(colour_from, level) %>%
    summarise(
      data = paste(data, collapse = ","),
      prior = paste(prior, collapse = ","),
      posterior = paste(posterior, collapse = ","),
      prior_mean = paste(round(as.numeric(unlist(strsplit(prior, ","))) /
                                 sum(as.numeric(unlist(strsplit(prior, ",")))), 3), collapse = ","),
      posterior_mean = paste(round(as.numeric(unlist(strsplit(posterior, ","))) /
                                     sum(as.numeric(unlist(strsplit(posterior, ",")))), 3), collapse = ","),
      .groups = "drop"
    ) %>%
    arrange(level)

  #print(colnames(aggregated_df))

  contracted_nodes <- contracted_nodes %>%
    mutate(fixed = list(list(x = TRUE, y = FALSE)))

  # Rename the columns in prior_table to match aggregated_df
  prior_table <- staged_tree_obj$priortable %>%
    rename(colour_from = Colour)

  prior_table <- prior_table %>%
    rename(level = Level)

  # Perform the left join
  merged_table <- prior_table %>%
    left_join(aggregated_df, by = c("colour_from", "level")) %>%
    select(Stage, colour_from, level, data, prior, prior_mean, posterior, posterior_mean) %>%
    mutate(
      across(c(prior, posterior), ~ map_chr(.x, ~ {
        values <- as.numeric(unlist(strsplit(.x, ",")))  # Convert string to numeric vector
        if (any(abs(values %% 1 - 0.999) < 1e-6 | abs(values %% 1 - 0.001) < 1e-6)) {
          values <- round(values)  # Round if any number ends in .999 or .001
        }
        paste(values, collapse = ",")  # Rejoin into a string
      }))
    ) %>% rename(
      Stage = Stage,
      Colour = colour_from,
      Level = level,
      Data = data,
      Prior = prior,
      `Prior Mean` = prior_mean,
      Posterior = posterior,
      `Posterior Mean` = posterior_mean
    )

  #print("mergedtable")
  #print(merged_table)

  #  for (col in merged_table$Colour) {
  #    styled_text <- make_style(col, bg = TRUE)
  #    cat(styled_text(paste(col)), "\n")
  #  }

  # Ensure 'Colour' is a character vector
  merged_table$Colour <- as.character(merged_table$Colour)

  ChainEventGraph <- visNetwork(nodes = contracted_nodes, edges = merged_edges) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = level_separation) %>%
    visNodes(scaling = list(min = 10, max = 10), font = list(vadjust = -170), fixed = TRUE) %>%
    visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5)), smooth = TRUE) %>%
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

  UpdateTable <- datatable(merged_table, escape = FALSE, options = list(
    pageLength = 10,
    columnDefs = list(list(targets = which(names(merged_table) == "Colour"), visible = FALSE))  # Hide the Colour column
  )) %>%
    formatStyle('Stage',
                backgroundColor = styleEqual(merged_table$Stage, merged_table$Colour))  # Apply background colours based on Colour values


  # Create the result list (with invisible filtereddf)
  result <- merged_table


  # Return both the network plot and the result
  #



  if (view_table) {
    print(ChainEventGraph)  # Display the first object
    readline(prompt = "Press Enter to see the update table:")  # Wait for user to press Enter
    print(UpdateTable)  # Display the second object
    invisible(NULL)  # Ensure no output is returned in the console
    #return(list(ceg = ChainEventGraph, update_table = result))

    output <- list(ceg = ChainEventGraph, update_table = result)
    class(output) <- "chain_event_graph"
    return(output)

  } else {
    print(ChainEventGraph)  # Display only the first object if view_table is FALSE
    invisible(NULL)  # Ensure no output is returned in the console
    output <- list(ceg = ChainEventGraph, update_table = result)
    class(output) <- "chain_event_graph"
    return(output)
  }

}


summary.chain_event_graph <- function(object, ...) {
  if (is.null(object$update_table)) {
    stop("The chain_event_graph object does not contain an update_table.")
  }

  update_table <- object$update_table
  total_score <- 0
  stage_scores <- numeric(nrow(update_table))

  for (i in 1:nrow(update_table)) {
    prior <- as.numeric(unlist(strsplit(update_table$Prior[i], ",")))
    data <- as.numeric(unlist(strsplit(update_table$Data[i], ",")))

    alpha_sum <- sum(prior)
    x_sum <- sum(data)
    posterior_sum <- alpha_sum + x_sum

    term1 <- lgamma(alpha_sum) - lgamma(posterior_sum)
    term2 <- sum(lgamma(prior + data) - lgamma(prior))

    stage_score <- term1 + term2
    stage_scores[i] <- stage_score
    total_score <- total_score + stage_score
  }

  result <- list(
    total_log_marginal_likelihood = total_score,
    per_stage_log_scores = data.frame(
      Stage = update_table$Stage,
      LogScore = round(stage_scores, 3)
    )
  )

  cat("Chain Event Graph Summary\n")
  cat("--------------------------\n")
  cat("Total Log Marginal Likelihood: ", round(total_score, 3), "\n\n")
  cat("Per-Stage Log Scores:\n")
  print(data.frame(Stage = update_table$Stage, LogScore = round(stage_scores, 3)))

  class(result) <- "summary.chain_event_graph"
  return(invisible(result))
}


compare_ceg_models <- function(summary1, summary2) {
  if (!inherits(summary1, "summary.chain_event_graph") ||
      !inherits(summary2, "summary.chain_event_graph")) {
    stop("Both inputs must be of class 'summary.chain_event_graph'.")
  }

  log_marginal_1 <- summary1$total_log_marginal_likelihood
  log_marginal_2 <- summary2$total_log_marginal_likelihood

  log_BF <- log_marginal_1 - log_marginal_2
  BF <- exp(log_BF)
  preferred_model = ifelse(log_BF > 0, "Model 1", "Model 2")

  result <- list(
    log_marginal_1 = log_marginal_1,
    log_marginal_2 = log_marginal_2,
    log_Bayes_factor = log_BF,
    Bayes_factor = BF,
    preferred_model = ifelse(log_BF > 0, "Model 1", "Model 2")
  )

  # Print the requested values
  cat("Log marginal of model 1: ", round(log_marginal_1, 3), "\n")
  cat("Log marginal of model 2: ", round(log_marginal_2, 3), "\n")
  cat("Log Bayes factor of Model 1 vs Model 2: ", round(log_BF, 3), "\n")
  cat("Bayes factor of Model 1 vs Model 2: ", round(BF, 3), "\n")

  cat("Preferred Model:", preferred_model, "\n")

  jeffreys_scale <- function(BF) {
    # Handle positive Bayes factors
    if (BF < 1) {
      return("Evidence against the alternative model")
    } else if (BF >= 1 & BF < 3) {
      return("Weak evidence for the alternative model")
    } else if (BF >= 3 & BF < 10) {
      return("Moderate evidence for the alternative model")
    } else if (BF >= 10 & BF < 30) {
      return("Strong evidence for the alternative model")
    } else if (BF >= 30 & BF < 100) {
      return("Very strong evidence for the alternative model")
    } else {
      return("Decisive evidence for the alternative model")
    }
  }

  # Extend to handle the negative reciprocal
  #jeffreys_scale_negative <- function(BF) {
  #  # Reciprocal represents evidence for Model 2, interpreting negative Bayes factor
  #  if (BF < 1) {
  #    return("Weak evidence for Model 2 (moderate evidence against Model 1)")
#    } else if (BF >= 1 & BF < 3) {
#      return("Moderate evidence for Model 2 (weak evidence against Model 1)")
#    } else if (BF >= 3 & BF < 10) {
#      return("Strong evidence for Model 2")
 #   } else if (BF >= 10 & BF < 30) {
#      return("Very strong evidence for Model 2")
#    } else if (BF >= 30 & BF < 100) {
 #     return("Decisive evidence for Model 2")
#    } else {
#      return("Extreme evidence for Model 2")
#    }
#  }
#
#  # Jeffreys scale interpretation for Model 1 (positive Bayes factor)
#  jeffreys_model1 <- jeffreys_scale(BF)
 #
#  # Jeffreys scale interpretation for Model 2 (negative reciprocal of Bayes factor)

##  jeffreys_model2 <- jeffreys_scale_negative(1 / BF)
 #
  # Prepare the table comparing Bayes factors with Jeffreys scale
#  comparison_table <- data.frame(
 #   Model = c("Model 1", "Model 2"),
  #  LogBayesFactor = c(log_BF, -log_BF),
  #  BayesFactor = c(BF, 1 / BF),  # For Model 2, it's the inverse of Model 1
  #  Interpretation = c(jeffreys_model1, jeffreys_model2)
  #)
#
#  # Print the comparison table
 # cat("Log Bayes Factors Comparison and Jeffreys Scale of Evidence:\n")
 # print(comparison_table)

  # Add statement about the negative reciprocal favoring Model 2
  #cat("\nNote: The negative reciprocal of the Bayes factor favors Model 2, as it would represent evidence against Model 1 in this case.\n")


  class(result) <- "ceg_model_comparison"
  invisible(result)
}

