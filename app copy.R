library(shiny)
library(visNetwork)
library(colourpicker)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(spData)
library(shinyjqui)
library(DT)
library(sortable)
library(ceg)
library(stagedtrees)
library(colorspace)
library(igraph)
library(shinycssloaders)
setwd("/Users/holliecalley/Library/CloudStorage/OneDrive-UniversityofExeter/Documents/R/CEG")
#homicides <- read_csv("Data/Homicides_London_03_23.csv")
homicides <- read_csv("Data/Homicides_London_03_23.csv", col_types = cols(Age_Group = col_factor(levels = c("Adult","Child", "Adolescent/Young Adult","Elderly")), Sex = col_factor(levels = c("Male","Female")), Domestic_Abuse = col_factor(levels = c("Not Domestic Abuse", "Domestic Abuse")), Solved_Status = col_factor(levels = c("Solved", "Unsolved")), Local_MP = col_factor(levels = c("Conservative", "Labour", "Liberal Democrat", "Other")), Same_as_UK_Party = col_factor(levels = c("No","Yes")), Method_of_Killing = col_factor(levels = c("Blunt Implement", "Knife or Sharp Implement", "Not Known/Not Recorded", "Other Methods of Killing", "Physical Assault, no weapon", "Shooting")), Ethnicity = col_factor(levels = c("Asian", "Black", "Not Reported/Not Known", "Other", "White"))))

#plot(st_geometry(lnd))

Area_Level <- list(
  Borough = list("Barking & Dagenham","Barnet","Bexley",
                 "Brent","Bromley","Camden","Croydon", 
                 "Ealing","Enfield","Greenwich","Hackney",
                 "Hammersmith & Fulham","Haringey","Harrow",
                 "Havering", "Hillingdon","Hounslow","Islington",
                 "Kensington & Chelsea","Kingston Upon Thames",
                 "Lambeth","Lewisham","Merton","Newham","Redbridge",
                 "Richmond Upon Thames","Southwark","Sutton", 
                 "Tower Hamlets","Waltham Forest","Wandsworth","Westminster"),
  BCU = list("Central West","South West","South","South East","East", 
             "West","Central South","North","Central East", 
             "Central North","North West","North East"))

homicides$Year <- format(as.Date(homicides$Recorded_Date, format="%Y/%m/%d"),"%Y")
#transform(homicides, Year = as.numeric(Year))
homicides[, c(11)] <- sapply(homicides[, c(11)], as.numeric)

homicides <- homicides %>% mutate(BCU = ifelse(Borough %in% c("Hammersmith & Fulham", "Kensington & Chelsea", "Westminster"), "Central West",
                                               ifelse(Borough %in% c("Kingston Upon Thames", "Merton", "Richmond Upon Thames", "Wandsworth"), "South West",
                                                      ifelse(Borough %in% c("Bromley", "Croydon", "Sutton"), "South",
                                                             ifelse(Borough %in% c("Bexley", "Greenwich", "Lewisham"), "South East",
                                                                    ifelse(Borough %in% c("Barking & Dagenham", "Havering", "Redbridge"), "East",
                                                                          ifelse(Borough %in% c("Ealing", "Hillingdon", "Hounslow"), "West",
                                                                                 ifelse(Borough %in% c("Lambeth", "Southwark"), "Central South",
                                                                                        ifelse(Borough %in% c("Enfield", "Haringey"), "North",
                                                                                               ifelse(Borough %in% c("Hackney", "Tower Hamlets"), "Central East",
                                                                                                      ifelse(Borough %in% c("Camden", "Islington"), "Central North",
                                                                                                             ifelse(Borough %in% c("Barnet", "Brent", "Harrow"), "North West",
                                                                                                                    ifelse(Borough %in% c("Newham", "Waltham Forest"), "North East","Hollie messed up")))))))))))))
homicides <- homicides[(c(1:4,6:8,11:12))]



ui <- fluidPage(
  titlePanel("Chain Event Graphs"),
  tabsetPanel(
    tabPanel("Data", fluid = TRUE,
      sidebarLayout(
        
        sidebarPanel(
          
          selectInput("Division", "Choose an area division:", names(Area_Level)),
          selectInput("Area", "Select an area:", Area_Level[[1]], multiple = TRUE),
          uiOutput("picker1"),
          uiOutput("picker2"),
          uiOutput("picker3"),
          sliderInput("Timeframe", "Choose a timeframe:", min = min(homicides$Year), max = max(homicides$Year), value = c(2003, 2023), sep=""),
          actionButton(inputId = "defaultButton", label = "Set Default Selections"),
          actionButton("view", "View Selection")),
  
            mainPanel(
              
              h2('Data Frame'),
              DTOutput("table")
  ))),
  tabPanel("Plots", fluid = TRUE,
           sidebarLayout(
             
             sidebarPanel(
               actionButton("vieweventtree", "View Event Tree"),
               actionButton("updateColor", "Update Color"),
               colourpicker::colourInput("nodeColor", "Choose color", value = "#FFFFFF"),
               actionButton("viewstagedtree", "View Staged Tree"),
               actionButton("viewceg", "View Chain Event Graph"),
             ),
             
             mainPanel(
               
               h2('Event
                  Tree'),
               checkboxInput("toggleLabels", "Hide data values", value = TRUE),
               actionButton("finishedColoring", "Finished Colouring"),
               visNetworkOutput("eventtree_network",height = "800px"),
               h2('Staged
                  Tree'),
               actionButton("finishedPrior", "Finished Prior Specification"),
               DTOutput("colorLevelTable", width = "500px"),
               visNetworkOutput("stagedtree",height = "800px"),
               h2('Chain Event Graph'),
               visNetworkOutput("ceg_network", height = "700px")
               
             ))),
  
))

server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "Area", choices = Area_Level[[input$Division]])
  })

  initial_choices <- colnames(homicides)[!colnames(homicides) %in% c("Solved_Status", "Area", "Division")]
  
  output$picker1 <- renderUI({
    pickerInput(
      inputId = 'pick1', 
      label = 'Choose first variable:', 
      choices = initial_choices,
      options = list(`actions-box` = TRUE),
      multiple = FALSE
    )
  })
  
  output$picker2 <- renderUI({
    pickerInput(
      inputId = 'pick2', 
      label = 'Choose second variable:', 
      choices = initial_choices, 
      options = list(`actions-box` = TRUE),
      multiple = FALSE
    )
  })
  
  output$picker3 <- renderUI({
    pickerInput(
      inputId = 'pick3', 
      label = 'Choose third variable:', 
      choices = initial_choices, 
      options = list(`actions-box` = TRUE),
      multiple = FALSE
    )
  })
  
  # Function to update choices based on previous selections
  updatePickers <- function() {
    selected1 <- input$pick1
    selected2 <- input$pick2
    
    # Update pick2 choices based on pick1 selection
    remaining_choices_pick2 <- initial_choices[initial_choices != selected1]
    updatePickerInput(session, "pick2", choices = remaining_choices_pick2, selected = input$pick2)
    
    # Update pick3 choices based on pick1 and pick2 selections
    remaining_choices_pick3 <- remaining_choices_pick2[remaining_choices_pick2 != selected2]
    updatePickerInput(session, "pick3", choices = remaining_choices_pick3, selected = input$pick3)
  }
  
  observeEvent(input$pick1, {
    updatePickers()
  })
  
  observeEvent(input$pick2, {
    updatePickers()
  })
  
  # Handle the default button click
  observeEvent(input$defaultButton, {
    # Sample two random boroughs from the available choices
    available_boroughs <- unique(homicides$Borough)
    random_boroughs <- sample(available_boroughs, 2)
    
    # Update the 'Area' selectInput to the random boroughs
    updateSelectInput(session, "Division", selected = "Borough")
    updateSelectInput(session, "Area", selected = random_boroughs)
    
    # Set the default selections for pick1, pick2, and pick3
    updatePickerInput(session, "pick1", choices = initial_choices, selected = "Borough")
    updatePickerInput(session, "pick2", choices = initial_choices, selected = "Domestic_Abuse")
    updatePickerInput(session, "pick3", choices = initial_choices, selected = "Sex")
  })
  
  
  
  #homicide_data2 <<- homicides 
  #makeReactiveBinding("homicide_data2")
  
  homicide_data <<- eventReactive(input$view,{
    homicide_data2 <<- homicides
    reactive(input$Division,{
          if(input$Division=="Borough")
          {homicide_data2 <<- subset(homicides, select = -c(BCU))
            homicide_data2 <<- select(filter(homicides, between(Year, input$Timeframe[1], input$Timeframe[2]) & Borough %in% input$Area),input$pick1, input$pick2, input$pick3, Solved_Status)}
          else if(input$Division=="BCU") 
          {homicide_data2 <<- subset(homicides, select = -c(Borough))
             homicide_data2 <<- select(filter(homicides, between(Year, input$Timeframe[1], input$Timeframe[2]) & BCU %in% input$Area),input$pick1, input$pick2, input$pick3, Solved_Status)}
      homicide_data2 <<- as.data.frame(homicide_data2)
      return(homicide_data2)
      })})

  
  
  #homicides2 <- read_csv("C:/Users/hc629/OneDrive - University of Exeter/Documents/R/CEG/Data/datasetInput.csv")
  
  output$table <- renderDT({
    homicide_data()
  })
    #-------------------------------------------------------------------------------------------------- 

  #graph_data <- reactiveValues(
 #   data = homicide_set()
#    nodes = data$nodes,
#    edges = data$edges
#  )
  

  
  #-------------------------------------------------------------------------------------------------- 
  #homicide.set <<- NA
  
  homicide_set <<- eventReactive(input$vieweventtree,{
    g <- make_empty_graph()
    parent <- "s0"
    col1 <- sort(as.vector(unique(homicide_data2[,c(1)])))
    col1s <- paste0("s", 1:length(col1))
    col2 <- sort(as.vector(unique(homicide_data2[,c(2)])))
    col2s <- paste0("s", (length(col1)+1):(length(col1) + length(col1)*length(col2)))
    col3 <- sort(as.vector(unique(homicide_data2[,c(3)])))
    col3s <- paste0("s", (length(col1) + length(col1)*length(col2)+1):(length(col1) + length(col1)*length(col2) + length(col1)*length(col2)*length(col3)))
    col4 <- sort(as.vector(unique(homicide_data2[,c(4)])))
    col4s <- paste0("s", (length(col1) + length(col1)*length(col2) + length(col1)*length(col2)*length(col3)+1):(length(col1) + length(col1)*length(col2) + length(col1)*length(col2)*length(col3) + length(col1)*length(col2)*length(col3)*length(col4)))
    g <- add_vertices(g, 1, name = paste0("s", 0))
    g <- add_vertices(g, length(col1), name = paste0("s", 1:length(col1)))
    g <- add_vertices(g, (length(col1)*length(col2)), name = paste0("s", (length(col1)+1):(length(col1) + length(col1)*length(col2))))
    g <- add_vertices(g, (length(col1)*length(col2)*length(col3)), name = paste0("s", (length(col1) + length(col1)*length(col2)+1):(length(col1) + length(col1)*length(col2) + length(col1)*length(col2)*length(col3))))
    g <- add_vertices(g, (length(col1)*length(col2)*length(col3)*length(col4)), name = paste0("s", (length(col1) + length(col1)*length(col2) + length(col1)*length(col2)*length(col3)+1):(length(col1) + length(col1)*length(col2) + length(col1)*length(col2)*length(col3) + length(col1)*length(col2)*length(col3)*length(col4))))
  
    
    count1 <- homicide_data2 %>% group_by(homicide_data2[,c(1)], .drop = FALSE) %>% summarise(count=n())
    for (col in colnames(count1)) {
      if (col != "count") {
        count1[[col]] <- as.character(count1[[col]])
      }
    }
    count1 <- arrange(count1, count1[(1)]) #,count2[c(2)])
    

    
    count2 <- homicide_data2 %>% group_by(homicide_data2[,c(1,2)], .drop = FALSE) %>% summarise(count=n())
    for (col in colnames(count2)) {
      if (col != "count") {
        count2[[col]] <- as.character(count2[[col]])
      }
    }
    count2 <- arrange(count2, count2[(1)], count2[(2)]) #,count2[c(2)])
  
    
    
    count3 <- homicide_data2 %>% group_by(homicide_data2[,c(1,2,3)], .drop = FALSE) %>% summarise(count=n())
    for (col in colnames(count3)) {
      if (col != "count") {
        count3[[col]] <- as.character(count3[[col]])
      }
    }
    count3 <- arrange(count3, count3[(1)], count3[(2)], count3[(3)]) #,count2[c(2)])
    
    count4 <- homicide_data2 %>% group_by(homicide_data2[,c(1,2,3,4)], .drop = FALSE) %>% summarise(count=n())
    for (col in colnames(count4)) {
      if (col != "count") {
        count4[[col]] <- as.character(count4[[col]])
      }
    }
    count4 <- arrange(count4, count4[(1)], count4[(2)], count4[(3)], count4[(4)]) #,count2[c(2)])
    
    # Add edges between parent and child nodes alternately
    edges <- c()
    for (i in seq_along(col1s)) {
      edges <- c(edges, "s0", col1s[i])
    }
    
    for (i in seq_along(col1s)) {
      # Calculate the starting and ending index of grandchild nodes for the current child node
      start_index <- (i - 1) * length(col2) + 1
      end_index <- i * length(col2)
      # Connect the child node to the corresponding grandchild nodes
      for (j in seq_along(col2)) {  # Loop only through the first 3 grandchild nodes
        edges <- c(edges, col1s[i], col2s[start_index:end_index][j])
      }
    }
    
    for (j in seq_along(col2s)) {
      # Calculate the starting and ending index of great-great-grandchild nodes for the current great-grandchild node
      start_index_great <- (j - 1) * length(col3) + 1
      end_index_great <- j * length(col3)
      # Connect the great-grandchild node to the corresponding great-great-grandchild nodes
      for (k in seq_along(col3)) {
        edges <- c(edges, col2s[j], col3s[start_index_great:end_index_great][k])
      }
    }
    
    for (j in seq_along(col3s)) {
      # Calculate the starting and ending index of great-great-grandchild nodes for the current great-grandchild node
      start_index_final <- (j - 1) * length(col4) + 1
      end_index_final <- j * length(col4)
      # Connect the great-grandchild node to the corresponding great-great-grandchild nodes
      for (k in seq_along(col4)) {
        edges <- c(edges, col3s[j], col4s[start_index_final:end_index_final][k])
      }
    }
    
    
    
    
    g <- add_edges(g, edges)
    
    # Plot the graph
    layout <- layout.reingold.tilford(g)
    layout <- -layout[, 2:1]
    
    # Plot the graph
    #plot(g, layout = layout, vertex.size=18, vertex.color="white", edge.arrow.size=0.05, vertex.label.cex=0.3, asp = 5.5)
    
    data <- toVisNetworkData(g)
    data$nodes$level <- c(1,rep(2,length(col1)), rep(3, (length(col1)*length(col2))), rep(4,length(col1)*length(col2)*length(col3)), rep(5,length(col1)*length(col2)*length(col3)*length(col4)))
    #data$nodes$label = data$nodes$id
    data$nodes$shape <- 'dot'
    data$nodes$size = 100
    data$nodes$color.background <- "#ffffff"
    data$nodes$font <- "80px"
    data$nodes$title = data$nodes$id
    #data$nodes$color <- "white"
    data$edges$label1 <- c(col1, rep(col2, length(col1)), rep(col3, length(col1)*length(col2)), rep(col4, length(col1)*length(col2)*length(col3)))
    data$edges$label2 <- c(count1$count,count2$count,count3$count,count4$count)
    data$edges$label3 <- paste(data$edges$label1, "\n", data$edges$label2)
    #data$edges$label <- paste(data$edges$label1, "\n", data$edges$label2)
    data$edges$font.size <- 70
    data$edges$color <- "#000000"
    data$edges$arrows <- "to"
    #
    return(data)
    })
  
  #graph_data <- reactiveValues(data = homicide_set())
  updated_graph_data <- reactiveVal(list(
    nodes = data.frame(id = integer(), label = character(), color = character()), 
    edges = data.frame(from = integer(), to = integer())
  ))
  
  # Reactive value to store selected nodes
  selected_nodes <- reactiveVal(character())
  # Sample data for visNetwork (you should replace this with your actual data)
  observe({
    data <- homicide_set()  # Assuming homicide_set() is a function that returns your data
    updated_graph_data(data)
  })

  observe({
    data <- updated_graph_data()
    if (input$toggleLabels) {
      data$edges$label <- data$edges$label1
    } else {
      data$edges$label <- data$edges$label3
    }
    updated_graph_data(data)
  })
  # Render the network
  # Render the network
  
  output$eventtree_network <- renderVisNetwork({
    data <- updated_graph_data()
    visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = 1000) %>%
      visNodes(scaling = list(min = 300, max = 300)) %>%
      visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
      visOptions(manipulation = list(enabled = TRUE,
                                     addEdgeCols = FALSE,
                                     addNodeCols = FALSE,
                                     editEdgeCols = FALSE,
                                     editNodeCols = FALSE,
                                     multiselect = TRUE),
                 nodesIdSelection = FALSE) %>%
      visInteraction(dragNodes = FALSE, multiselect = TRUE, navigationButtons = TRUE) %>%
      visEvents(selectNode = "function(nodes) {
        Shiny.onInputChange('eventtree_network_selected_add', nodes.nodes);
      }",
                deselectNode = "function(nodes) {
        Shiny.onInputChange('eventtree_network_selected_remove', nodes.nodes);
      }") %>%
      visPhysics(solver = "forceAtlas2Based", 
                 forceAtlas2Based = list(gravitationalConstant = -50), 
                 hierarchicalRepulsion = list(nodeDistance = 300))
  })
  
  # Observe node selection and deselection
  observeEvent(input$eventtree_network_selected_add, {
    new_selection <- input$eventtree_network_selected_add
    current_selection <- selected_nodes()
    updated_selection <- unique(c(current_selection, new_selection))
    selected_nodes(updated_selection)
    print("Nodes selected:")
    print(updated_selection)  # Debugging statement
  })
  
  observeEvent(input$eventtree_network_selected_remove, {
    deselected_nodes <- input$eventtree_network_selected_remove
    current_selection <- selected_nodes()
    updated_selection <- current_selection[current_selection %in% deselected_nodes]
    selected_nodes(updated_selection)
    print("Nodes selected:")
    print(updated_selection)  # Debugging statement
  })
  
  
  # Observe the updateColor button
  observeEvent(input$updateColor, {
    print("Update Color button clicked")  # Debugging statement
    
    selected_nodes_list <- selected_nodes()
    print(selected_nodes_list)  # Debugging statement to print selected nodes
    
    if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
      data <- updated_graph_data()
      data$nodes$color[data$nodes$id %in% selected_nodes_list] <- input$nodeColor
      updated_graph_data(data)
      
      visNetworkProxy("eventtree_network") %>%
        visUpdateNodes(nodes = data$nodes)
    } else {
      print("No nodes selected")  # Additional debugging statement
    }
    
    # Reset selected nodes
    selected_nodes(NULL)
  })
  
  # observeEvent(input$updateColor, {
  #   data <- updated_graph_data()
  #   selected_nodes <- visNetworkProxy("eventtree_network")$getSelectedNodes()
  #   new_color <- input$nodeColor
  #   
  #   if (length(selected_nodes) > 0) {
  #     data$nodes$color[data$nodes$id %in% selected_nodes] <- new_color
  #     updated_graph_data(data)
  #     visNetworkProxy("eventtree_network") %>%
  #       visUpdateNodes(nodes = data$nodes)
  #   }
  # })
  
  node_colors_levels <- reactiveVal(NULL)
  
  
  # Observe the finishedColoring button click
  observeEvent(input$finishedColoring, {
    # Get the updated_graph_data
    data <- updated_graph_data()
    
    add_outgoing_edges_count <- function(nodes_df, edges_df) {
      # Count how many times each node appears in the 'from' column of the edges dataframe
      outgoing_counts <- edges_df %>%
        group_by(from) %>%
        summarise(outgoing_edges = n()) %>%
        ungroup()
      
      # Merge the counts with the nodes dataframe
      nodes_df <- nodes_df %>%
        left_join(outgoing_counts, by = c("id" = "from"))
      
      # Replace NA with 0 for nodes that have no outgoing edges
      nodes_df$outgoing_edges[is.na(nodes_df$outgoing_edges)] <- 0
      
      return(nodes_df)
    }
    
    # Example usage with your dataframes
    data$nodes <- add_outgoing_edges_count(data$nodes, data$edges)
    #print(data$nodes)
    # Extract node colors and levels
    node_colors_levels_data <- data$nodes[, c("id", "label", "color", "level", "outgoing_edges")]
    print(node_colors_levels_data)
    # Add a column with HTML code for colored cells
    #node_colors_levels_data$color_display <- sprintf('<div style="background-color:%s;width:100%%;height:100%%;"></div>', node_colors_levels_data$color)
    
    # Store the node colors and levels in the reactive value
    node_colors_levels(node_colors_levels_data)
    
  })
  
  # Render the color and level table
  output$colorLevelTable <- renderDT({
    node_colors_levels_data <- node_colors_levels()
    if (!is.null(node_colors_levels_data)) {
      unique_colors_levels_data <- unique(node_colors_levels_data[node_colors_levels_data$level != 5, c("color", "level", "outgoing_edges")])
      unique_colors_levels_data$stage <- paste0("u", seq_len(nrow(unique_colors_levels_data)))
      unique_colors_levels_data$prior <- ""
      unique_colors_levels_data <- unique_colors_levels_data[, c("color", "stage", "level", "outgoing_edges", "prior")]
      node_colors_levels(unique_colors_levels_data)
      datatable(unique_colors_levels_data, escape = FALSE, editable = TRUE, options = list(dom = 't', pageLength = 50), rownames = FALSE) %>%
        formatStyle(columns = "color", valueColumns = "color", backgroundColor = styleEqual(unique_colors_levels_data$color, unique_colors_levels_data$color), color = styleEqual(unique_colors_levels_data$color, unique_colors_levels_data$color)) %>%
        formatStyle(columns = "level", textAlign = "left")
    }
  })
  
  observeEvent(input$colorLevelTable_cell_edit, {
    info <- input$colorLevelTable_cell_edit
    node_colors_levels_data <- node_colors_levels()
    
    # Map the `col` index to the correct column name
    col_name <- names(node_colors_levels_data)[info$col + 1]  # Adjust for 0-based index from DataTable
    
    # Debugging: Print the identified column name and value to be updated
    print(paste("Editing column:", col_name, "at row:", info$row, "with value:", info$value))
    
    # Update the corresponding cell in the node_colors_levels_data
    node_colors_levels_data[info$row, col_name] <- info$value
    
    # Store the updated node_colors_levels_data in the reactive value
    node_colors_levels(node_colors_levels_data)
    
    # Debugging: Print the updated node_colors_levels_data
    print("After edit:")
    print(node_colors_levels())
    
    # Refresh the table to show the updated data
    output$colorLevelTable <- renderDT({
      datatable(node_colors_levels(), escape = FALSE, editable = TRUE, options = list(dom = 't'), rownames = FALSE) %>%
        formatStyle(columns = "color", valueColumns = "color", backgroundColor = styleEqual(node_colors_levels()$color, node_colors_levels()$color), color = styleEqual(node_colors_levels()$color, node_colors_levels()$color)) %>%
        formatStyle(columns = "level", textAlign = "left")
    })
  })
  
  
  staged_tree_data <- reactiveVal(NULL)
  
  observeEvent(input$finishedPrior, {
    # Get the updated node_colors_levels data
    edited_node_colors_levels_data <- node_colors_levels()
    
    # Debugging: Print the edited data to verify prior updates
    print("Edited node_colors_levels_data:")
    print(edited_node_colors_levels_data)
    
    # Get the updated_graph_data
    data <- updated_graph_data()
    
    # Match and apply priors to nodes in updated_graph_data
    for (i in 1:nrow(edited_node_colors_levels_data)) {
      color <- edited_node_colors_levels_data$color[i]
      level <- edited_node_colors_levels_data$level[i]
      prior <- edited_node_colors_levels_data$prior[i]
      
      # Update nodes in updated_graph_data that match the color and level
      data$nodes$prior[data$nodes$color == color & data$nodes$level == level] <- prior
    }
    
    # Update the reactive value with adjusted data
    updated_graph_data(data)
    print(updated_graph_data())
  })

  observeEvent(input$viewstagedtree, {
    data <- updated_graph_data()
    convertPrior <- function(prior) {
      as.numeric(strsplit(prior, ",")[[1]])
    }
    
    # Create a function to calculate adjusted priors
    adjustPriors <- function(prior, count) {
      prior_values <- convertPrior(prior)
      adjusted <- prior_values / count
      rounded_adjusted <- round(adjusted, 2)  # Round to 2 decimal places
      return(paste(rounded_adjusted, collapse = ","))
    }
    
    data$nodes$adjusted_prior <- ""
    
    for (color in unique(data$nodes$color)) {
      for (level in unique(data$nodes$level)) {
        # Filter nodes with the same color and level
        same_group_nodes <- data$nodes[data$nodes$color == color & data$nodes$level == level, ]
        
        if (nrow(same_group_nodes) > 0) {
          # Count the number of nodes in this group
          count <- nrow(same_group_nodes)
          
          # Loop through each node in the group and update the prior
          for (i in 1:nrow(same_group_nodes)) {
            node_index <- which(data$nodes$id == same_group_nodes$id[i])
            prior <- data$nodes$prior[node_index]
            if (!is.na(prior) && prior != "") {
              data$nodes$adjusted_prior[node_index] <- adjustPriors(prior, count)
            }
          }
        }
      }
    }
    
    print(data$nodes)
    
    assignPriorsToEdges <- function(node_data, edge_data) {
      for (i in 1:nrow(node_data)) {
        from_node <- node_data$id[i]
        adj_prior <- node_data$adjusted_prior[i]
        
        if (adj_prior != "") {
          adj_prior_values <- convertPrior(adj_prior)
          edges_from_node <- which(edge_data$from == from_node)
          
          # Ensure there are enough prior values for the edges
          if (length(adj_prior_values) >= length(edges_from_node)) {
            for (j in 1:length(edges_from_node)) {
              edge_data$label[edges_from_node[j]] <- paste(edge_data$label1[edges_from_node[j]], "\n", adj_prior_values[j])
              edge_data$label3[edges_from_node[j]] <- adj_prior_values[j]
              }
          } else {
            warning(paste("Not enough prior values for edges from node", from_node))
          }
        }
      }
      return(edge_data)
    }
    
    data$edges <- assignPriorsToEdges(data$nodes, data$edges)
    data$edges$label3 <- as.numeric(data$edges$label3)
    
    # Print edges to verify
    print(data$edges)
    
    
    # Update the reactive value
    staged_tree_data(data)
    
    output$stagedtree <- renderVisNetwork({
      data = staged_tree_data()
      visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
        visHierarchicalLayout(direction = "LR", levelSeparation = 1000) %>%
        visNodes(scaling = list(min = 300, max = 300)) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
        visOptions(manipulation = list(enabled = TRUE, addEdgeCols = FALSE, addNodeCols = FALSE, editNodeCols = FALSE, editEdgeCols = c("label3"))) %>%
        visInteraction(dragNodes = FALSE, multiselect = TRUE, navigationButtons = TRUE) %>%
        visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -50), hierarchicalRepulsion = list(nodeDistance = 300))
    })
  })
  
  contracted_data <- reactiveVal(NULL)
  
  observeEvent(input$viewceg, {
    data <- staged_tree_data()
    nodes <- data$nodes
    edges <- data$edges
    print(nodes)
    print(edges)
    
    # Initialize contract IDs
    nodes$contract_id <- paste0(nodes$level, "-", nodes$color)
    
    # Function to update contract IDs by appending connected nodes' contract IDs
    update_contract_ids <- function(nodes, edges) {
      # Contract nodes at levels 1 and 5 separately
      nodes$contract_id[nodes$level == 1] <- "1-#ffffff"
      nodes$contract_id[nodes$level == 5] <- "5-#ffffff"
      
      for (level in sort(unique(nodes$level), decreasing = TRUE)) {
        if (level == 1 || level == 5) next
        
        current_level_nodes <- nodes[nodes$level == level, ]
        
        for (i in 1:nrow(current_level_nodes)) {
          node <- current_level_nodes[i, ]
          connected_edges <- edges[edges$from == node$id | edges$to == node$id, ]
          
          connected_nodes <- unique(c(connected_edges$from, connected_edges$to))
          connected_nodes <- connected_nodes[connected_nodes != node$id]
          
          if (length(connected_nodes) > 0) {
            connected_contract_ids <- nodes$contract_id[nodes$id %in% connected_nodes]
            connected_levels <- nodes$level[nodes$id %in% connected_nodes]
            higher_or_same_level_ids <- connected_contract_ids[connected_levels >= node$level]
            nodes$contract_id[nodes$id == node$id] <- paste0(nodes$contract_id[nodes$id == node$id], "-", paste(higher_or_same_level_ids, collapse = "-"))
          }
        }
      }
      return(nodes)
    }
    
    nodes <- update_contract_ids(nodes, edges)
    
    #print("Nodes to be contracted:")
    #print(nodes %>% group_by(contract_id) %>% summarise(ids = paste(id, collapse = ", ")))
    
    contracted_nodes <- nodes %>%
      group_by(contract_id) %>%
      summarise(ids = paste(id, collapse = ", "), label = first(label), level = first(level), color = first(color), .groups = 'drop')
    
    #print("Contracted Nodes before:")
    #print(contracted_nodes)
    
    # Sort contracted_nodes by current labels numerically
contracted_nodes <- contracted_nodes[order(as.numeric(gsub("[^0-9]", "", contracted_nodes$label))), ]

# Reassign labels sequentially from v0 to v(n-1) and set the last label as v∞
num_nodes <- nrow(contracted_nodes)
contracted_nodes$label <- paste0("v", 0:(num_nodes - 1))
contracted_nodes$label[num_nodes] <- paste0("v", "\u221E")  # Unicode for ∞
    
    #print("Contracted Nodes after:")
    #print(contracted_nodes)
    #print(contracted_nodes$ids)
    
    # Create a mapping from each individual ID to the contracted ID
    id_mapping <- unlist(lapply(1:nrow(contracted_nodes), function(i) {
      ids <- unlist(strsplit(contracted_nodes$ids[i], ",\\s*"))
      ids <- trimws(ids)
      setNames(rep(contracted_nodes$label[i], length(ids)), ids)
    }))
    
    #print("ID Mapping:")
    #print(id_mapping)
    
    # Copy edges data
    updated_edges <- edges
    
    # Replace from and to IDs in edges data with contracted IDs
    updated_edges$from <- id_mapping[as.character(updated_edges$from)]
    updated_edges$to <- id_mapping[as.character(updated_edges$to)]
    
    # Check for NAs
    print("Checking for NAs in updated_edges:")
    if (any(is.na(updated_edges$from)) || any(is.na(updated_edges$to))) {
      print("NAs found in updated_edges$from or updated_edges$to:")
      print(updated_edges[is.na(updated_edges$from) | is.na(updated_edges$to), ])
    }
    
    merged_edges <- updated_edges %>%
      group_by(from, to, label1) %>%
      summarise(sumlabel2 = sum(label2),
                sumlabel3 = sum(label3),
                label = paste(first(label1), "\n", (sumlabel2+sumlabel3)),
                font.size = first(font.size),
                color = first(color),
                .groups = 'drop')
    
    # Calculate unique curvature for edges to prevent overlap
    curvature_values <- merged_edges %>%
      group_by(from, to) %>%
      mutate(curvature = seq(-0.2, 0.4, length.out = n())) %>%
      ungroup()
    
    merged_edges$smooth <- pmap(curvature_values, function(from, to, curvature, ...) {
      list(enabled = TRUE, type = "curvedCW", roundness = curvature)
    })
    
    # Print the updated edges
    #print("Updated Edges:")
    #print(merged_edges)
    #print(merged_edges$smooth)
    
    # Store the contracted data
    contracted_nodes$id <- contracted_nodes$label
    contracted_nodes$size = 90
    contracted_nodes$font <- "80px"
    contracted_data(list(nodes = contracted_nodes, edges = merged_edges))
    data <- contracted_data()
    edges <- data$edges
    nodes <- data$nodes
    edges <- merge(edges, nodes, by.x = "from", by.y = "id", all.x = FALSE, suffixes = c("_from", "_to"))
    #edges <- edges[]
    edges2 <- edges %>%
      group_by(color_to, level, label1) %>%
      summarise(
        sumlabel4 = sum(sumlabel2),
        sumlabel5 = sum(sumlabel3),
        sumlabel6 = (sumlabel4 + sumlabel5),
        .groups = 'drop' # Ungroup after summarise
      )
    
    edges3 <- edges2 %>%
      group_by(color_to, level) %>%
      summarise(
        sumlabel7 = sum(sumlabel6),
        .groups = 'drop' # Ungroup after summarise
      )
    
    edges2 <- edges2 %>%
      left_join(edges3, by = c("color_to", "level"))
    
    edges2$posteriormean <- round((edges2$sumlabel6/edges2$sumlabel7),2)
    
    
    #print("Summarised Edges with Grouped Sums:")
    #print(edges2)
    #print(edges3)
    # Display the summarised dataframe
    
    edges <- edges %>%
      left_join(edges2, by = c("color_to", "level", "label1"))
    #print(edges)
    edges <- edges %>% select(from, to, label1, font.size, color_from, smooth, level, posteriormean)
    edges$label <- paste(edges$label1, "\n", edges$posteriormean)
    edges$color <- "#000000"
    #print(edges)
    contracted_data(list(nodes = contracted_nodes, edges = edges))
  })
  
  # Render the contracted graph
  output$ceg_network <- renderVisNetwork({
    data <- contracted_data()
    edges <- data$edges
    nodes <- data$nodes
    #edges <- merge(edges, nodes, by.x = "from", by.y = "id", all.x = TRUE, suffixes = c("_from", "_to"))
    
    if (is.null(contracted_data)) {
      return(NULL)
    }
    visNetwork(nodes = data$nodes, edges = data$edges,  height = "900px") %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = 1200) %>%
      visNodes(scaling = list(min = 900, max = 900)) %>%
      visEdges(smooth = TRUE, arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
      visOptions(manipulation = list(enabled = FALSE,
                                     addEdgeCols = FALSE,
                                     addNodeCols = FALSE,
                                     editEdgeCols = FALSE,
                                     editNodeCols = c("color"),
                                     multiselect = TRUE), nodesIdSelection = TRUE) %>%
      visInteraction(dragNodes = TRUE, multiselect = FALSE, navigationButtons = TRUE) %>%
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 990), stabilization = TRUE) %>%
      visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")
  })
  # Render the contracted graph
  # output$ceg_network <- renderVisNetwork({
  # Retrieve the contracted data from the reactive value
  #   data <- contracted_data()
  
  #     visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
  #       visHierarchicalLayout(direction = "LR", levelSeparation = 1000) %>%
  #       visNodes(scaling = list(min = 300, max = 300)) %>%
  #       visOptions(manipulation = list(enabled = TRUE,
  #                                      addEdgeCols = FALSE,
  #                                      addNodeCols = FALSE,
  #                                      editEdgeCols = FALSE,
  #                                      editNodeCols = c("color"),
  #                                      multiselect = TRUE), nodesIdSelection = TRUE) %>%
  #       visInteraction(dragNodes = FALSE, multiselect = TRUE, navigationButtons = TRUE) %>%
  #       visPhysics(solver = "forceAtlas2Based", 
  #                  forceAtlas2Based = list(gravitationalConstant = -50), hierarchicalRepulsion = list(nodeDistance = 300))
  #   })
  
  
  
}

shinyApp(ui, server)



