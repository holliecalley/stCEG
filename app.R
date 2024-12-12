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
library(stagedtrees)
library(colorspace)
library(igraph)
library(shinycssloaders)
library(dirmult)
library(hwep)
library(RColorBrewer)
library(randomcoloR)
library(gtools)
library(zoo)
library(leaflet)
library(shinyjs)


ui <- fluidPage(
  titlePanel("stCEG - Modelling Over Spatial Areas Using Chain Event Graphs"),
  tabsetPanel(
    tabPanel("Upload Data", 
             sidebarLayout(
               sidebarPanel(fileInput(
                 "file1",
                 "Choose CSV File",
                 multiple = TRUE,
                 accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv"
                 )
               ),
               
               uiOutput("area_division_checkboxes"),
               uiOutput("time_division_checkboxes"),
               
               
               # Horizontal line ----
               tags$hr(),
               
               #textInput("na_values", "Specify NA values", value = ""),
               checkboxInput("exclude_row_numbers", "Exclude First Column as Row Numbers", FALSE),
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Header", TRUE),
               
               # Input: Select separator ----
               radioButtons(
                 "sep",
                 "Separator",
                 choices = c(
                   Comma = ",",
                   Semicolon = ";",
                   Tab = "\t"
                 ),
                 selected = ","
               ),
               
               # Input: Select quotes ----
               radioButtons(
                 "quote",
                 "Quote",
                 choices = c(
                   None = "",
                   "Double Quote" = '"',
                   "Single Quote" = "'"
                 ),
                 selected = '"'
               ),
               
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select number of rows to display ----
               radioButtons(
                 "disp",
                 "Display",
                 choices = c(
                   Head = "head",
                   All = "all"
                 ),
                 selected = "all"
               ), 
               uiOutput("prediction_var"),
               #actionButton("finish", "Finished"),
               ,width = 3),
               mainPanel(DTOutput("rawdata")))),
    
    
    
    tabPanel("Select Data", fluid = TRUE,
             sidebarLayout(
               
               sidebarPanel(
                 
                 uiOutput("area_dropdown"),
                 uiOutput("time_type_input"),
                 uiOutput("date_format_input"),
                 uiOutput("time_dropdown"), 
                 
                 
                 uiOutput("num_vars_ui"),
                 # Dynamically generate pickers based on the number of variables
                 uiOutput("pickers_ui"),
                 uiOutput("timeframe_slider"),
                 actionButton(inputId = "defaultButton", label = "Set Default Selections"),
                 actionButton("view", "View Selection"),
                 width = 3),
               
               mainPanel(
                 
                 h2('Data Frame'),
                 DTOutput("table")
               ))),
    tabPanel("Plots", fluid = TRUE,
             tags$head(
               tags$style(HTML("
      /* Fix sidebar panel width and alignment */
      #sidebar {
        width: 23%;
        position: fixed;
        top: 104;
        left: 1%;
        z-index: 1000;
      }
      /* Adjust main panel to have a margin-left equal to the sidebar width */
      #main {
        margin-left: 25%; /* Adjust this value to match sidebar's width plus spacing */
        
      }
    "))
             ),
             
             sidebarLayout(
               # Sidebar panel
               sidebarPanel(
                 useShinyjs(),
                 id = "sidebar",  # Assign ID for custom styling
                 style = "padding-top: 20px; padding-bottom: 20px;",
                 actionButton("vieweventtree", "View Event Tree"),
                 tags$hr(),
                 conditionalPanel(
                   condition = "input.viewOption == 'Map'",
                   style = "margin-top: 20px; margin-bottom: 20px;",
                   fileInput(
                     "shapefile",
                     "Upload Shapefile (ZIP)",
                     accept = ".zip"
                   ),
                   textInput("crs", "Specify CRS (if missing):", value = NA),
                   sliderInput("mapOpacity", "Layer Opacity", min = 0, max = 1, value = 0.7, sep = ""),
                   actionButton("process_shapefile", "Process Shapefile")
                 ),
                 colourpicker::colourInput("nodeColor", "Choose colour", value = "#FFFFFF"),
                 actionButton("updateColor", "Update Colour"),
                 actionButton("AHCColoring", "Colour using AHC"),
                 actionButton("viewstagedtree", "View Staged Tree"),
                 actionButton("viewceg", "View Chain Event Graph"),
                 width = 3
               ),
               
               # Main panel
               mainPanel(
                 id = "main",  # Assign ID for custom styling
                 selectInput(
                   "viewOption",
                   "Choose Colouring Method:",
                   choices = c("Event Tree", "Map"),
                   selected = "Map"
                 ),
                 
                 h2(textOutput("mainPanelTitle")),
                 checkboxInput("toggleLabels", "Hide data values", value = TRUE),
                 fluidRow(
                   column(6, leafletOutput("map", height = "600px")),
                   column(6, visNetworkOutput("eventtree_network", height = "1000px"))
                 ),
                 fluidRow(column(6, uiOutput("ExchangeabilityHideView")),
                          column(2, actionButton("deleteNode", "Delete Selected Node")),
                          column(2, actionButton("finishedColoring", "Finished Colouring"))
                 ),
                
                 #leafletOutput("map", height = "600px"),
                 #visNetworkOutput("eventtree_network", height = "1000px"),  # Shared ID
                 
                 #h2('Event Tree'),
                 #checkboxInput("toggleLabels", "Hide data values", value = TRUE),
                 #actionButton("deleteNode", "Delete Selected Node"),
                 #actionButton("finishedColoring", "Finished Colouring"),
                 #visNetworkOutput("eventtree_network", height = "1000px"),
                 
                 h2('Staged Tree'),
                 selectInput(
                   inputId = "priorChoice",
                   label = "Choose Prior Type:",
                   choices = c("Specify Prior", "Uniform 1,1 Prior", "Phantom Individuals Prior"),
                 ),
                 DTOutput("colorLevelTable", width = "95%"),
                 actionButton("finishedPrior", "Finished Prior Specification"),
                 checkboxInput("usePriorLabels", "Show Prior Mean", value = FALSE),
                 visNetworkOutput("stagedtree", height = "1000px"),
                 
                 h2('Chain Event Graph'),
                 checkboxInput("viewUpdateTable", "View Prior-Posterior Update Table", value = TRUE),
                 checkboxInput("showposteriormean", "Show Posterior Mean", value = TRUE),
                 sliderInput("levelSeparation", "Level Separation", min = 500, max = 10000, value = 1000, sep = ""),
                 visNetworkOutput("ceg_network", height = "1000px"),
                 
                 conditionalPanel(
                   condition = "input.viewUpdateTable == true",
                   DTOutput("UpdateTable", width = "95%")
                 )
               )
             )
    ),
#    tabPanel("Plot Map", fluid = TRUE,
#             sidebarLayout(
#               
#               sidebarPanel(
#                 fileInput(
#                   "shapefile",
#                   "Upload Shapefile (ZIP)",
#                   accept = ".zip"
#                 ),
#                 textInput("crs", "Specify CRS (if missing):", value = NA),
#                 sliderInput("mapOpacity", "Layer Opacity", min = 0, max = 1, value = 0.5, sep = ""),
#                 actionButton("process_shapefile", "Process Shapefile")
#               ),
#               mainPanel(
#                 leafletOutput("map", height = "600px")
#               ))),
  ))


server <- function(input, output, session) {
  options(shiny.maxRequestSize=30*1024^2)
  original_data <- reactiveVal()  # To store the original, unfiltered dataframe
  homicides <- reactiveVal()      # To store the filtered dataframe
  
  output$rawdata <- renderDT({
    req(input$file1)
    
    df <- read.csv(
      input$file1$datapath,
      header = input$header,
      sep = input$sep,
      quote = input$quote
    )
    
    # Ensure the Date columns are correctly formatted
    if ("DateColumn" %in% names(df)) {
      df$DateColumn <- as.Date(df$DateColumn, format = "%Y-%m-%d")  # Adjust format as needed
    }
    
    if (input$exclude_row_numbers) {
      df <- df[, -1]
    }
    
    original_data(df)  # Save the original data
    homicides(df)      # Set initial filtered data to the original data
    
    datatable(df) %>%
      formatStyle(
        columns = input$predict_var,
        backgroundColor = "rgba(0, 255, 0, 0.1)"
      )
  })
  
  output$prediction_var <- renderUI({
    req(homicides())
    df <- homicides()
    selectInput(
      "predict_var",
      "Select Prediction Variable",
      choices = names(df),
      selected = NULL
    )
  })
  
  output$area_division_checkboxes <- renderUI({
    req(homicides())
    df <- homicides()
    area_columns <- colnames(df)
    selectInput(
      "selected_area_columns",
      "Select Area Division",
      choices = c("",area_columns),
      selected = NULL,
    )
  })
  
  output$time_division_checkboxes <- renderUI({
    req(homicides())
    df <- homicides()
    time_columns <- colnames(df)
    selectInput(
      "selected_time_columns",
      "Select Time Division",
      choices = c("",time_columns),
      selected = NULL,
    )
  })
  
  output$area_dropdown <- renderUI({
    df_homicides <- homicides()
    
    # Check if selected_area_columns is not NULL and has at least one column selected
    if (input$selected_area_columns != "" && length(input$selected_area_columns) > 0) {
      
      # Dynamically select the columns from the dataframe (df_homicides assumed here)
      selected_columns2 <- df_homicides[, input$selected_area_columns, drop = FALSE]
      
      # Get the unique values from the selected columns
      unique_values_area <- unique(do.call(c, selected_columns2))
      
      # Create the selectInput UI with the unique values from the selected columns
      selectInput(
        "Area",
        "Choose Area",
        choices = unique_values_area,
        multiple = TRUE  # Enable multiple selections
      )
    } else {
      NULL
    }
  })
  
  
  observe({
    req(original_data())
    # Reset homicides to the original data
    homicides(original_data())
  })
  
  output$time_type_input <- renderUI({
    req(homicides(), input$selected_time_columns != "")
    selectInput(
      "time_type",
      "Select Time Type",
      choices = c("Date", "Month-Year", "Year", ""),
      selected = ""
    )
  })
  
  output$date_format_input <- renderUI({
    req(input$selected_time_columns != "", input$time_type)  # Ensure both are available
    
    if (input$time_type == "Date") {
      textInput("date_format", "Specify Date Format", value = "%Y-%m-%d")
    } 
    else if (input$time_type == "Month-Year") {
      textInput("month_format", "Specify Date Format", value = "%Y-%m")
    } else {
      NULL  # You can return NULL or handle other cases as needed
    }
  })
  
  
  
  # Rendering the time dropdown slider based on the selected time division
  # Check the selected time column and convert appropriately
  output$time_dropdown <- renderUI({
    req(input$selected_time_columns != "", input$time_type)
    
    df <- homicides()
    time_col <- input$selected_time_columns
    
    print(paste("Selected time column:", time_col))
    print("Column names in df:")
    print(colnames(df))
    
    if (!time_col %in% colnames(df)) {
      print("Error: Selected column does not exist in the data frame.")
      return(NULL)
    }
    
    print(paste("Column type:", class(df[[time_col]])))
    
    if (input$time_type == "Month-Year") {
      req(input$month_format)
      
      # Convert the MonthYear column to yearmon format
      print("Attempting to convert MonthYear to yearmon format")
      tryCatch({
        df[[time_col]] <- zoo::as.yearmon(df[[time_col]], format = input$month_format)
        print("Converted MonthYear to yearmon format successfully.")
        
        start_date <- min(df[[time_col]], na.rm = TRUE)
        end_date <- max(df[[time_col]], na.rm = TRUE)
        
        print(paste("Start date:", start_date))
        print(paste("End date:", end_date))
        
        # Check if start_date or end_date is NA
        if (is.na(start_date) || is.na(end_date)) {
          print("Invalid start or end date for month-year slider.")
          return(NULL)
        }
        
        # Create sequence of months between start_date and end_date
        month_year_seq <- seq(start_date, end_date, by = 1/12)  # 1/12 means monthly increments for yearmon
        month_year_labels <- format(month_year_seq, "%b %Y")
        
        print("Rendering Month-Year slider")
        
        sliderTextInput(
          inputId = "timeframe_slider",
          label = "Select Month-Year Range",
          choices = month_year_labels,
          selected = c(month_year_labels[1], month_year_labels[length(month_year_labels)])
        )
        
      }, error = function(e) {
        print("Error generating Month-Year slider:")
        print(e)
        return(NULL)
      })
      
    } else if (input$time_type == "Date") {
      req(input$date_format)
      df[[time_col]] <- as.Date(df[[time_col]], format = input$date_format)
      
      print("Rendering Date slider")
      
      sliderInput(
        "timeframe_slider",
        "Select Date Range",
        min = min(df[[time_col]], na.rm = TRUE),
        max = max(df[[time_col]], na.rm = TRUE),
        value = c(min(df[[time_col]], na.rm = TRUE), max(df[[time_col]], na.rm = TRUE)),
        timeFormat = "%Y-%m-%d"
      )
    }
    else if (input$time_type == "Year") {
      df[[time_col]] <- as.numeric(df[[time_col]])
      
      print("Rendering Year slider")
      
      sliderInput(
        "timeframe_slider",
        "Select Year Range",
        min = min(df[[time_col]], na.rm = TRUE),
        max = max(df[[time_col]], na.rm = TRUE),
        value = c(min(df[[time_col]], na.rm = TRUE), max(df[[time_col]], na.rm = TRUE)),
        step = 1,
        sep = "",
        ticks = TRUE
      )
    }
  })
  
  
  
  
  
  # Update the picker inputs based on available choices
  # Reactive function for initial column choices
  initial_choices <- reactive({
    df_homicides <- homicides()
    if (is.null(df_homicides)) return(NULL)
    
    choices <- colnames(df_homicides)[!colnames(df_homicides) %in% c(input$predict_var)]
    setNames(choices, choices)
  })
  
  # Render the numericInput for number of variables
  output$num_vars_ui <- renderUI({
    req(homicides())  # Ensure the data is uploaded
    
    df <- homicides()
    
    numericInput(
      inputId = "num_vars", 
      label = "Choose number of variables:", 
      value = 2,  # Default starting value
      min = 1, 
      max = length(initial_choices())  # Set the max value dynamically
    )
  })
  
  # Render the dynamic pickers based on the value of num_vars
  output$pickers_ui <- renderUI({
    req(homicides(), input$num_vars)  # Ensure dataset and num_vars are available
    
    # Number of variables selected by the user
    num_vars <- input$num_vars
    
    # Create a list of picker inputs dynamically
    picker_inputs <- lapply(1:num_vars, function(i) {
      pickerInput(
        inputId = paste0('pick', i), 
        label = paste0('Choose variable ', i, ':'), 
        choices = initial_choices(),
        options = list(`actions-box` = TRUE),
        multiple = FALSE
      )
    })
    
    # Return the picker inputs as a tagList to render them in the UI
    do.call(tagList, picker_inputs)
  })
  
  
  observeEvent(input$Division, {
    req(homicides())
    df <- homicides()
    updateSelectInput(session, "Area", choices = unique(df[[input$Division]]))
  })
  
  observeEvent(input$defaultButton, {
    req(homicides())
    df <- homicides()
    
    selected_area_columns <- input$selected_area_columns
    if (length(selected_area_columns) > 0) {
      selected_division <- selected_area_columns[1]
      available_areas <- unique(df[[selected_division]])
      random_areas <- if (length(available_areas) >= 2) sample(available_areas, 2) else available_areas
      
      updateSelectInput(session, "Area", choices = available_areas, selected = random_areas)
      
      # Update the pickers dynamically based on the number of variables chosen
      num_vars <- input$num_vars
      choices <- initial_choices()
      
      # Loop through and update each picker
      lapply(1:num_vars, function(i) {
        updatePickerInput(session, paste0("pick", i), choices = choices, selected = names(choices)[i])
      })
    }
  })
  
  
  homicide_data <- eventReactive(input$view, {
    req(homicides())
    df_homicides <- homicides()
    
    # Time Filtering
    if (input$selected_time_columns != "" && input$time_type != "") {
      time_col <- input$selected_time_columns
      req(time_col)
      
      print(paste("Time column selected:", time_col))
      print(paste("Time column type:", class(df_homicides[[time_col]])))
      print(paste("Timeframe input values:", input$timeframe_slider))
      
      if (input$time_type == "Date") {
        df_homicides[[time_col]] <- as.Date(df_homicides[[time_col]], format = input$date_format)
        start_time <- as.Date(input$timeframe_slider[1])
        end_time <- as.Date(input$timeframe_slider[2])
        df_homicides <- df_homicides %>%
          filter(df_homicides[[time_col]] >= start_time & df_homicides[[time_col]] <= end_time)
        
      } else if (input$time_type == "Month-Year") {
        df_homicides[[time_col]] <- zoo::as.yearmon(df_homicides[[time_col]], "%Y-%m")
        start_index <- which(input$timeframe_slider[1] == format(df_homicides[[time_col]], "%b %Y"))
        end_index <- which(input$timeframe_slider[2] == format(df_homicides[[time_col]], "%b %Y"))
        
        if (length(start_index) > 0 && length(end_index) > 0) {
          df_homicides <- df_homicides %>%
            filter(df_homicides[[time_col]] >= df_homicides[[time_col]][start_index] & 
                     df_homicides[[time_col]] <= df_homicides[[time_col]][end_index])
        }
        
      } else if (input$time_type == "Year") {
        req(input$timeframe_slider)
        
        if (inherits(df_homicides[[time_col]], "character")) {
          df_homicides[[time_col]] <- as.numeric(df_homicides[[time_col]])
        }
        
        start_year <- input$timeframe_slider[1]
        end_year <- input$timeframe_slider[2]
        
        if (!is.null(start_year) && !is.null(end_year)) {
          df_homicides <- df_homicides %>%
            filter(df_homicides[[time_col]] >= start_year & df_homicides[[time_col]] <= end_year)
        } else {
          print("Invalid year range in the slider.")
        }
      }
    }
    
    # Area Filtering
    selected_area_columns <- input$selected_area_columns
    if (length(selected_area_columns) > 0 && !is.null(input$Area) && length(input$Area) > 0) {
      df_homicides <- df_homicides %>%
        filter(df_homicides[[selected_area_columns[1]]] %in% input$Area)
    }
    
    # Dynamically gather the selected columns from the pickers
    num_vars <- input$num_vars
    selected_columns <- lapply(1:num_vars, function(i) input[[paste0("pick", i)]])
    
    # Include the prediction variable if it's selected
    if (!is.null(input$predict_var) && input$predict_var != "") {
      selected_columns <- c(selected_columns, input$predict_var)
    }
    
    df_homicides <- df_homicides %>%
      select(all_of(unlist(selected_columns)))
    
    return(df_homicides)
  })
  
  
  
  
  
  output$table <- renderDT({
    req(homicide_data())
    datatable(homicide_data())
  })
  
  
  
  
  
  
  #-------------------------------------------------------------------------------------------------- 
  
  #graph_data <- reactiveValues(
  #   data = homicide_set()
  #    nodes = data$nodes,
  #    edges = data$edges
  #  )
  
  
  
  #-------------------------------------------------------------------------------------------------- 
  #homicide.set <<- NA
  eventtree_pressed <- reactiveVal(FALSE)
  
  homicide_set <<- eventReactive(input$vieweventtree, {
    eventtree_pressed(TRUE)
    g <- make_empty_graph()
    parent <- "s0"
    homicide_data2 <- homicide_data()
    
    num_vars <- input$num_vars + 1  # Number of variables chosen by the user
    
    # Initialize lists to store unique values and state names
    unique_values_list <- vector("list", num_vars)
    state_names_list <- vector("list", num_vars)
    
    start_index <- 1
    total_states <- 1
    
    # Generate state names and keep track of the indices
    for (i in 1:num_vars) {
      # Get unique values from column without sorting or excluding NAs
      col_values <- unique(homicide_data2[[i]])
      
      # Sort values and keep NAs if they exist
      col_values <- sort(col_values, na.last = TRUE)
      
      # Store the sorted values with NAs in unique_values_list
      unique_values_list[[i]] <- col_values
      
      # Generate state names based on unique values
      state_names_list[[i]] <- paste0("s", start_index:(start_index + length(col_values) * (total_states) - 1))
      
      total_states <- total_states * length(col_values)
      start_index <- start_index + total_states
    }
    
    print("unique values list")
    print(unique_values_list)
    print("state names list")
    print(state_names_list)
    
    # Add vertices to the graph: starting with the root node "s0"
    g <- add_vertices(g, 1, name = "s0")
    
    # Add vertices dynamically for each column based on the calculated states
    for (i in 1:num_vars) {
      g <- add_vertices(g, length(state_names_list[[i]]), name = state_names_list[[i]])
    }
    vertex_names <- V(g)$name
    print("Vertex names of the graph:")
    print(vertex_names)
    
    # Function to generate combinations and counts dynamically
    generate_combinations <- function(df, cols) {
      col_names <- colnames(df)[cols]
      
      # Generate all possible combinations
      all_combinations <- expand.grid(lapply(df[col_names], unique))
      
      # Remove any combinations containing NA
      #all_combinations <- all_combinations %>% 
      #filter(!if_any(everything(), is.na))
      
      # Calculate counts for the actual data
      counts <- df %>%
        group_by(across(all_of(col_names))) %>%
        summarise(count = n(), .groups = 'drop')
      
      # Merge combinations with counts
      full_data <- full_join(all_combinations, counts, by = col_names)
      
      # Replace any NA counts with 0
      full_data$count[is.na(full_data$count)] <- 0
      
      # Arrange the final dataset
      full_data <- full_data %>%
        arrange(across(all_of(col_names)))
      
      return(full_data)
    }
    
    
    # Calculate counts dynamically
    counts_list <- lapply(1:num_vars, function(x) generate_combinations(homicide_data2, 1:x))
    print(counts_list)
    # Add edges between parent and child nodes dynamically
    edges <- c()
    state_indices <- rep(1, num_vars)
    
    edges <- c()
    
    # Loop through each column/variable to generate edges
    for (i in 1:num_vars) {
      num_states <- length(state_names_list[[i]])
      
      # Compute the total number of states in the previous columns
      prev_total_states <- if (i > 1) length(state_names_list[[i - 1]]) else 1
      
      # Adjust start and end indices for the current variable's states
      start_index <- 1
      end_index <- num_states / prev_total_states
      
      # Add edges
      if (i == 1) {
        # First column: connect to "s0" (root node)
        for (j in 1:num_states) {
          edges <- c(edges, "s0", state_names_list[[i]][j])
        }
      } else {
        # Subsequent columns: connect to the previous column's states
        for (j in 1:prev_total_states) {
          parent_state <- state_names_list[[i - 1]][j]
          child_states <- state_names_list[[i]][start_index:end_index]
          
          # Create edges from the parent state to its corresponding child states
          for (k in 1:length(child_states)) {
            edges <- c(edges, parent_state, child_states[k])
          }
          
          # Update start and end indices for the next parent node
          start_index <- end_index + 1
          end_index <- start_index + (num_states / prev_total_states) - 1
        }
      }
    }
    
    # Print the final edge list in the desired format
    print("edges")
    print(edges)
    
    
    
    
    g <- add_edges(g, edges)
    print(g)
    # Plot the graph
    layout <- layout.reingold.tilford(g)
    layout <- -layout[, 2:1]
    
    data <- toVisNetworkData(g)
    #print(data)
    # Check that state_names_list has the correct structure
    #print(state_names_list)
    
    # Adjust the times argument in the rep function
    # `1` is for the root node, and the rest corresponds to the number of states at each level
    num_levels <- num_vars + 1  # Including the root node
    print("state_names_list")
    data$nodes$level <- rep(1:num_levels, times = c(1, sapply(1:num_vars, function(x) length(state_names_list[[x]]))))
    print(data$edges)
    data$nodes$shape <- 'dot'
    data$nodes$size <- 100
    data$nodes$color.background <- "#FFFFFF"
    data$nodes$font <- "80px"
    data$nodes$title <- data$nodes$id
    # Check number of edges
    #print(nrow(data$edges))  # Number of edges in the graph
    
    # Flatten the data and extract second-to-last column (label1) and count (label2)
    # Ensure all data frames have the same number of columns by padding with NA
    # Step 1: Find the union of all column names across the data frames in counts_list
    # Step 1: Find the union of all column names across the data frames in counts_list
    all_column_names <- unique(unlist(lapply(counts_list, colnames)))
    print("all_column_names")
    print(all_column_names)
    # Step 2: Function to add missing columns to each data frame and ensure "count" is last
    align_columns <- function(df, all_column_names) {
      missing_cols <- setdiff(all_column_names, colnames(df))  # Find columns that are missing
      df[missing_cols] <- "IGNORE"  # Add missing columns filled with NA
      
      # Reorder columns to match the union and ensure "count" is the last column
      cols_ordered <- c(setdiff(all_column_names, "count"), "count")
      return(df[cols_ordered])
    }
    
    # Step 3: Apply the function to each data frame in counts_list
    counts_list_aligned <- lapply(counts_list, align_columns, all_column_names)
    
    # Step 4: Flatten the list of aligned data frames into one using rbind
    df_flat <- do.call(rbind, counts_list_aligned)
    
    print("df_flat")
    print(df_flat)
    
    get_last_non_zero_na_rowwise <- function(df) {
      # Exclude the 'count' column
      non_count_cols <- colnames(df)[-which(colnames(df) == "count")]
      
      # Function to get the last non-zero (and non-NA) entry in a row
      get_last_non_zero <- function(row) {
        # Identify non-zero entries, keeping NAs but ignoring "IGNORE" values
        non_zero_entries <- row[non_count_cols][(row[non_count_cols] != 0 | is.na(row[non_count_cols])) & row[non_count_cols] != "IGNORE"]
        
        # Check if there are any entries after filtering
        if (length(non_zero_entries) > 0) {
          last_non_zero <- tail(non_zero_entries, 1)
        } else {
          last_non_zero <- NA
        }
        
        # Replace NA with "NA" as a text string
        last_non_zero <- ifelse(is.na(last_non_zero), "NA", last_non_zero)
        
        return(last_non_zero)
      }
      
      
      # Apply the function to each row
      last_non_zero_entries <- apply(df, 1, get_last_non_zero)
      
      return(last_non_zero_entries)
    }
    
    last_entries <- get_last_non_zero_na_rowwise(df_flat)
    print("last_entries")
    print(last_entries)
    # Add this vector as a new column in the data$edges dataframe
    data$edges$label1 <- last_entries
    
    # Print the updated data$edges dataframe to check the new column
    print("Updated data$edges with new label1 column:")
    print(data$edges)
    
    
    # Check the result
    
    # The last column is the count (label2)
    #data$edges$label1 <- last_entries
    data$edges$label2 <- df_flat$count  # Last column (count)
    #print(data$edges)
    data$edges$label3 <- paste(data$edges$label1, "\n", data$edges$label2)
    
    # Assign labels to edges in the graph
    #data$edges$label1 <- label1
    #data$edges$label2 <- label2
    #data$edges$label3 <- label3
    
    
    
    #data$edges$label1 <- unlist(lapply(1:num_vars, function(x) rep(unique_values_list[[x]], each = prod(sapply((x+1):num_vars, function(y) length(unique_values_list[[y]]))))))
    #data$edges$label2 <- unlist(lapply(counts_list, function(x) x$count))
    #data$edges$label3 <- paste(data$edges$label1, "\n", data$edges$label2)
    data$edges$font.size <- 70
    data$edges$color <- "#000000"
    data$edges$arrows <- "to"
    
    return(data)
  })
  
  
  
  #graph_data <- reactiveValues(data = homicide_set())
  updated_graph_data <- reactiveVal(list(
    nodes = data.frame(id = integer(), label = character(), color = character()), 
    edges = data.frame(from = integer(), to = integer())
  ))
  
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
  
  add_counts <- function(nodes_df, edges_df) {
    # Summarize the counts from edges_df by 'from' node
    counts2 <- edges_df %>%
      group_by(from) %>%
      summarise(counts = sum(label2))
    
    # Merge the counts into nodes_df based on 'id'
    nodes_df <- nodes_df %>%
      left_join(counts2, by = c("id" = "from"))
    
    # Replace NA with 0 for nodes that have no outgoing edges
    nodes_df$counts[is.na(nodes_df$counts)] <- 0
    
    return(nodes_df)
  }
  
  # Example usage
  
  # Reactive value to store selected nodes
  selected_nodes <- reactiveVal(character())
  # Sample data for visNetwork (you should replace this with your actual data)
  observe({
    data <- homicide_set()  # Assuming homicide_set() is a function that returns your data
    
    # Add outgoing edges count to nodes
    data$nodes <- add_outgoing_edges_count(data$nodes, data$edges)
    data$nodes <- add_counts(data$nodes, data$edges)
    data$nodes$level2 <- data$nodes$level
    # Update the reactive value with the modified data
    updated_graph_data(data)
  })
  
  observe({
    data <- updated_graph_data()
    if (input$toggleLabels) {
      data$edges$label <- data$edges$label1
    } else {
      data$edges$label <- data$edges$label3
    }
  })
  
  # Render the network
  
  
  node_colors <- reactiveVal(NULL)
  
  
  
  # Reactive expression for updated graph data with node colors
  # updated_graph_data_with_colors <- reactive({
  #    data <- updated_graph_data()
  #    if (!is.null(node_colors())) {
  #      data$nodes$color <- node_colors()
  #    }
  #    data
  #  })
  
  observe({
    if (input$viewOption == "Map") {
      # Show the map and adjust event tree height
      shinyjs::show("map")
      shinyjs::show("eventtree_network")
      shinyjs::runjs('$("#eventtree_network").css("height", "600px")')  # Set event tree height to 600px
    } else {
      # Hide the map and adjust event tree height
      shinyjs::hide("map")
      shinyjs::show("eventtree_network")
      shinyjs::runjs('$("#eventtree_network").css("height", "1000px")')  # Set event tree height to 1000px
    }
  })
  
  output$ExchangeabilityHideView <- renderUI({
    if (input$viewOption == "Map") {
      actionButton("showFloretModal", "Show Floret")
    } else {
      NULL  # No button if the view is not "Map"
    }
  })
  
  output$mainPanelTitle <- renderText({
    if (input$viewOption == "Map") {
      "Colouring on Map"
    } else {
      "Colouring on Event Tree"
    }
  })
  
  
  
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
      data$nodes$number <- 1
      updated_graph_data(data)
      
      visNetworkProxy("eventtree_network") %>%
        visUpdateNodes(nodes = data$nodes)
    } else {
      print("No nodes selected")  # Additional debugging statement
    }
    
    # Reset selected nodes
    selected_nodes(NULL)
  })
  
  observeEvent(input$deleteNode, {
    # Get the list of currently selected nodes
    selected_nodes_list <- selected_nodes()
    print("Selected nodes to delete:")
    print(selected_nodes_list)  # Debugging statement to print selected nodes
    
    if (!is.null(selected_nodes_list) && length(selected_nodes_list) > 0) {
      # Access current graph data
      data <- updated_graph_data()
      data_before <- list(nodes = data$nodes, edges = data$edges)
      
      # Loop through selected nodes to delete them
      for (node in selected_nodes_list) {
        # Find the outgoing edges of the node to be deleted
        outgoing_edges <- data$edges[data$edges$from == node, ]
        
        # Find the incoming edges to the node to be deleted
        incoming_edges <- data$edges[data$edges$to == node, ]
        
        if (nrow(outgoing_edges) > 0 && nrow(incoming_edges) > 0) {
          # Redirect the outgoing edges to connect to the source of the incoming edges
          for (i in 1:nrow(outgoing_edges)) {
            for (j in 1:nrow(incoming_edges)) {
              # Ensure the new edge has the same structure as the existing edges
              new_edge <- data.frame(
                from = incoming_edges$from[j],
                to = outgoing_edges$to[i],
                label = outgoing_edges$label[i],
                label1 = outgoing_edges$label1[i],
                label2 = outgoing_edges$label2[i],
                label3 = outgoing_edges$label3[i],
                arrows = outgoing_edges$arrows[i],
                font.size = outgoing_edges$font.size[i],
                color = "#000000",  # Set color to black
                stringsAsFactors = FALSE
              )
              
              # Add any other required columns to the new edge to match the structure of data$edges
              missing_cols <- setdiff(names(data$edges), names(new_edge))
              new_edge[missing_cols] <- NA  # Assign NA to missing columns if necessary
              
              # Append the new edge to the edges data frame
              data$edges <- rbind(data$edges, new_edge)
            }
          }
        }
        
        # Remove the node and its edges (including end nodes)
        data$nodes <- data$nodes[data$nodes$id != node, ]
        data$edges <- data$edges[data$edges$from != node & data$edges$to != node, ]
      }
      
      # Update the reactive graph data
      deleted_edges <- setdiff(paste(data_before$edges$from, data_before$edges$to),
                               paste(data$edges$from, data$edges$to))
      
      
      #added_nodes <- setdiff(data$nodes$id, data_before$nodes$id)
      added_edges <- setdiff(paste(data$edges$from, data$edges$to),
                             paste(data_before$edges$from, data_before$edges$to))
      
      
      cat("Edges removed:\n")
      print(data_before$edges[with(data_before$edges, paste(from, to)) %in% deleted_edges, ])
      
      cat("Edges added:\n")
      print(data$edges[with(data$edges, paste(from, to)) %in% added_edges, ])
      
      
      # Find the unique 'from' nodes in the added edges
      unique_from_nodes <- unique(data$edges$from[with(data$edges, paste(from, to)) %in% added_edges])
      
      # Increment the 'level' for each of these unique nodes in data$nodes
      data$nodes$level2[data$nodes$id %in% unique_from_nodes] <- 
        data$nodes$level2[data$nodes$id %in% unique_from_nodes] + 1
      
      deleted_from_counts <- table(data_before$edges$from[with(data_before$edges, paste(from, to)) %in% deleted_edges])
      added_from_counts <- table(data$edges$from[with(data$edges, paste(from, to)) %in% added_edges])
      
      # Update outgoing edges for deleted edges
      for (node in names(deleted_from_counts)) {
        data$nodes$outgoing_edges[data$nodes$id == node] <- 
          data$nodes$outgoing_edges[data$nodes$id == node] - deleted_from_counts[node]
      }
      
      # Update outgoing edges for added edges
      for (node in names(added_from_counts)) {
        data$nodes$outgoing_edges[data$nodes$id == node] <- 
          data$nodes$outgoing_edges[data$nodes$id == node] + added_from_counts[node]
      }
      
      # Reassign node IDs sequentially
      old_ids <- data$nodes$label
      new_ids <- paste0("s", seq(0, nrow(data$nodes) - 1))
      id_mapping <- setNames(new_ids, old_ids)
      
      # Update node IDs in the nodes dataframe
      data$nodes$id <- new_ids
      data$nodes$label <- new_ids
      
      data$edges$from <- id_mapping[as.character(data$edges$from)]
      data$edges$to <- id_mapping[as.character(data$edges$to)]
      # Update reactive graph data
      updated_graph_data(data)
      
      # Reflect changes in the visNetwork proxy
      visNetworkProxy("eventtree_network") %>%
        visUpdateNodes(nodes = data$nodes) %>%
        visUpdateEdges(edges = data$edges)
      
      # Print deletion success message
      print("Edges and nodes updated:")
      print(data$nodes)  # Debugging statement to print updated edges
      
    } else {
      # Show a modal dialog if no nodes are selected
      showModal(modalDialog(
        title = "No Nodes Selected",
        "Please select at least one node to delete.",
        easyClose = TRUE
      ))
    }
    
    # Reset selected nodes after the operation
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
  
  
  observeEvent(input$AHCColoring, {
    data2 <- updated_graph_data()
    print("data2")
    #print(data2)
    
    exampledata <- homicide_data()
    exampledata3 <- exampledata
    
    nodes <- data2$nodes
    
    # Get unique levels from nodes
    unique_levels <- unique(nodes$level)
    #print(unique_levels)
    
    # Define levels to filter out (maximum level)
    levels_to_exclude <- max(unique_levels)
    
    # Filter out nodes at level 1 or max level
    nodes_to_consider <- nodes[!(nodes$level %in% levels_to_exclude), ]
    #print(nodes_to_consider)
    
    nodes_to_consider$id2 <- 1:nrow(nodes_to_consider)
    nodes_to_consider
    nodes_to_consider2 <- nodes_to_consider$id
    
    edges <- data2$edges
    edges_to_consider <- edges %>%
      group_by(from) %>%
      summarize(
        label2_list = paste(label2, collapse = ", ")
      )
    label_matching <- edges %>%
      group_by(from) %>%
      summarize(
        label_list = paste(label1, collapse = ", ")
      )
    #edges_to_consider
    edges_to_consider <- inner_join(edges_to_consider, label_matching, by = join_by(from == from))
    nodes_to_consider <- inner_join(nodes_to_consider, edges_to_consider, by = join_by(id == from), keep = FALSE)
    print("nodes_to_consider")
    print(nodes_to_consider)
    print("edges")
    print(edges)
    
    convert_to_matrix <- function(label2_list_str) {
      # Split the string into a numeric vector
      num_vec <- as.numeric(unlist(strsplit(label2_list_str, ", ")))
      
      # Convert the numeric vector to a matrix with 1 row
      mat <- matrix(num_vec, nrow = 1, byrow = TRUE)
      
      # If we have more than one element, return the matrix as-is.
      # If there's only one element, wrap it into a matrix format
      if (length(num_vec) == 2) {
        return(mat)
      } else {
        return(matrix(num_vec, nrow = 1))
      }
    }
    
    # Apply the conversion function to the 'label2_list' column
    
    # Ensure all columns are factors
    exampledata[] <- lapply(exampledata, function(x) {
      if (!is.factor(x)) as.factor(x) else x
    })
    print("exampledata")
    #print(exampledata)
    
    
    
    # Calculate number of variables
    numbvariables <- ncol(exampledata)
    print("numvars:")
    print(numbvariables)
    
    # Calculate number of categories for each column
    numbcat <- sapply(exampledata, nlevels)
    print("numcat:")
    print(as.vector(numbcat))
    
    # Determine the size of the largest category
    equivsize <- max(nodes_to_consider$outgoing_edges)
    print("equivsize:")
    print(equivsize)
    
    # Calculate the number of combinations
    numb <- numeric(numbvariables)
    numb[1] <- 1  # The number of combinations for 1 variable is 1
    
    for (i in 2:numbvariables) {
      numb[i] <- prod(numbcat[1:(i-1)])
    }
    
    print("numb")
    print(numb)
    
    #print(prior)
    
    
    nodes_to_consider$prior <- NA
    
    # Set the prior for the first row to equivsize
    nodes_to_consider$prior[1] <- equivsize
    prior<-c()
    for (i in 1:nrow(nodes_to_consider)) {
      
      # Get the current row's 'id' from nodes_to_consider
      current_id <- nodes_to_consider$id[i]
      
      # Get the number of outgoing edges for this node (this could be a count of edges with 'from' = current_id)
      outgoing_edges <- nodes_to_consider$outgoing_edges[i]
      
      # Step 2: Calculate the new prior (divide current prior by the number of outgoing edges)
      current_prior <- nodes_to_consider$prior[i]  # Assuming prior column exists
      new_prior <- current_prior / outgoing_edges
      
      # Step 3: Update the 'prior' for rows in edges where 'from' equals the current 'id'
      # and update the corresponding 'prior' in nodes_to_consider based on 'to'
      to_nodes <- edges$to[edges$from == current_id]  # Get all 'to' nodes where 'from' equals current_id
      
      # Update the 'prior' for corresponding nodes in nodes_to_consider
      for (j in to_nodes) {
        nodes_to_consider$prior[nodes_to_consider$id == j] <- new_prior
      }
      prior<-c(prior,list(rbind(rep(nodes_to_consider$prior[i]/outgoing_edges,outgoing_edges))))
    }
    
    #print(nodes_to_consider)
    
    print("prior")
    print(prior)
    
    #Datalist1: list of the number of individuals going from the stage along a particular edge in C_{0}
    data <- lapply(nodes_to_consider$label2_list, convert_to_matrix)
    # Print the resulting list of matrices
    #print("data")
    # print(data)
    
    #List of the stages that can be merged in the first step 
    comparisonset <- nodes_to_consider %>%
      group_by(level2, label_list) %>%
      summarise(node_ids = list(id2)) %>%
      pull(node_ids)  # Extract the list of node IDs
    
    # Print the resulting list of vectors
    print(comparisonset)
    print("end of comparisonset")
    # Initialize labelling as an empty matrix with 0 rows and columns
    
    
    # Print the levels of the factor
    #print(levels(column_vector))
    # Extract and sort levels
    #sorted_levels <- sort(levels(factor_levels))
    
    # Print sorted levels
    #print(sorted_levels)
    
    #print(sorted_levels)
    # Initialize labelling matrix
    # Initialize labelling matrix
    labelling <-c()
    # Initialize labelling matrix
    labelling <- NULL
    
    for (k in 1:(numbvariables - 1)) { 
      # Alphabetically sort the levels of the current variable
      sorted_levels <- sort(levels(factor(exampledata3[[k]])))
      print("sorted levels")
      print(sorted_levels)
      
      # Create the initial label with "NA" and appropriate repetitions
      label <- c("NA", rep("NA", sum(numb[1:k]) - 1)) 
      label <- c(label, rep(sorted_levels, numb[k]))
      print(label)
      
      # If not the last variable, continue adding labels for subsequent variables
      if (k < (numbvariables - 1)) {
        for (i in (k + 1):(numbvariables - 1)) {
          label <- c(label, rep(sorted_levels, each = numb[i + 1] / numb[k + 1], numb[k + 1] / numbcat[k]))
        }
      }
      
      labelling <- cbind(labelling, label)
      
    }
    
    labelling <- nodes_to_consider$label_list
    
    # Print the resulting labelling matrix
    # print(labelling)
    
    
    
    row_numbers <- nodes_to_consider$id
    
    # Combine the sequence with the `labelling` matrix
    # Use `matrix` to ensure the row numbers are a column vector with correct dimensions
    labelling <- cbind(labelling, row_numbers)
    print("labelling")
    print(labelling)
    
    mergedlist <-c()
    for (i in 1:nrow(nodes_to_consider)){ 
      mergedlist<-c(mergedlist,list(labelling[i,]))
    } 
    print("mergedlist")
    print(mergedlist)
    merged1<-c() 
    lik <-0
    for( i in 1: nrow(nodes_to_consider)){ 
      alpha<-unlist(prior[i])
      print("alpha")
      print(alpha)
      N<-unlist(data[i]) 
      print(N)
      lik<-lik+sum(lgamma(alpha+N)-lgamma(alpha))+sum(lgamma(sum(alpha))-lgamma(sum(alpha+N)))
    } 
    score<-c(lik)
    #At each step we calculate the difference between the current CEG and the CEG in which two stages in the current comparison set have been merged.
    #We go through every possible combination of stages that can be merged. k is an index for the comparisonset we are in,
    #and i and j the position of the stages within the comparison set.
    diff.end<-1 #to start the algorithm
    while(diff.end>0){ #We stop when no positive difference is obtained by merging two stages 
      #while(length(unlist(comparisonset))>3){ 
      difference <-0
      for (k in 1:length(comparisonset)){
        if(length(comparisonset[[k]])>1){ #can only merge if more than one stage in the comparisonset
          for (i in 1:(length(comparisonset[[k]])-1)){ 
            for (j in (i+1):length(comparisonset[[k]])){
              #to compare 
              compare1<-comparisonset[[k]][i]
              compare2<-comparisonset[[k]][j]
              #we calculate the difference between 
              #the CEG where two stages are merged
              result<-lgamma(sum(prior[[compare1]]+prior[[compare2]]))-lgamma(sum(prior[[ compare1]]+data[[compare1]]+prior[[compare2]]+data[[compare2]]))+
                sum(lgamma(prior[[compare1]]+data[[compare1]]+prior[[compare2]]+data[[ compare2]]))-sum(lgamma(prior[[compare1]]+prior[[compare2]]))-
                #and the CEG where the two stages are not merged
                (lgamma(sum(prior[[compare1]]))-lgamma(sum(prior[[compare1]]+data[[compare1 ]]))+sum(lgamma(prior[[compare1]]+data[[compare1]]))-
                   sum(lgamma(prior[[compare1]]))+lgamma(sum(prior[[compare2]]))-lgamma(sum( prior[[compare2]]+data[[compare2]]))+
                   sum(lgamma(prior[[compare2]]+data[[compare2]]))-sum(lgamma(prior[[compare2]])))
              #if the resulting difference is greater than the current difference then we replace it
              if (result > difference){ 
                difference<-result
                merged<-c(compare1,compare2,k) 
              }
            } 
          }
        }
      } 
      diff.end<-difference
      #We update our priorlist, datalist and comparisonset to obtain the priorlist , datalist and comparisonlist for C_{1}
      if(diff.end >0){
        prior[[merged[1]]]<-prior[[merged[1]]]+prior[[merged[2]]] 
        prior[[merged[2]]]<-cbind(NA,NA)
        data[[merged[1]]]<-data[[merged[1]]]+data[[merged[2]]] 
        data[[merged[2]]]<-cbind(NA,NA) 
        comparisonset[[merged[3]]]<-comparisonset[[merged[3]]][-(which(comparisonset[[merged[3]]]==merged[2]))] 
        mergedlist[[merged[1]]]<-cbind(mergedlist[[merged[1]]],mergedlist[[merged[2]]]) 
        mergedlist[[merged[2]]]<-cbind(NA,NA)
        lik<-lik+diff.end 
        score<-c(score,lik) 
        merged1<-cbind(merged1,merged)
      } 
    }
    #Output: stages of the finest partition to be combined to obtain the most probable CEG structure
    stages<-c(1)
    for (i in 2:numbvariables){ 
      stages<-c(stages,comparisonset[[i-1]])
    }
    result<-mergedlist[stages] 
    newlist<-list(prior=prior,data=data,stages=stages,result=result,score=score,merged=merged1 ,comparisonset=comparisonset ,mergedlist=mergedlist ,lik=lik) 
    mergedlist
    row_numbers_list <- list()
    
    # Loop through each sublist in mergedlist
    for (i in 1:length(mergedlist)) {
      sublist <- mergedlist[[i]]
      
      # Initialize an empty vector to hold the row_numbers from this sublist
      sublist_row_numbers <- c()
      
      # Check if the sublist is not empty and not NULL
      if (!is.null(sublist) && length(sublist) > 0) {
        # Check if sublist is a matrix and contains "row_numbers"
        if (is.matrix(sublist)) {
          # Extract "row_numbers" from the matrix
          if (any(grepl("^row_numbers$", rownames(sublist)))) {
            row_numbers <- sublist[grepl("^s\\d+$", sublist)]
            if (length(row_numbers) > 0) {
              sublist_row_numbers <- c(sublist_row_numbers, row_numbers)
            }
          }
        } else if (is.list(sublist)) {
          # If sublist is a list, check each element for "row_numbers"
          for (j in 1:length(sublist)) {
            # Ensure the sublist element is not NULL
            if (!is.null(sublist[[j]])) {
              # Check for named "row_numbers" entry or row_numbers in matrix rownames
              if (names(sublist)[j] == "row_numbers" || (is.matrix(sublist[[j]]) && any(grepl("^row_numbers$", rownames(sublist[[j]]))))) {
                row_numbers <- sublist[[j]][grepl("^s\\d+$", sublist[[j]])]
                if (length(row_numbers) > 0) {
                  sublist_row_numbers <- c(sublist_row_numbers, row_numbers)
                }
              }
            }
          }
        }
      }
      
      # Add the extracted row_numbers to the main list if any were found
      if (length(sublist_row_numbers) > 0) {
        row_numbers_list[[length(row_numbers_list) + 1]] <- sublist_row_numbers
      }
    }
    
    # Flatten the nested lists into simple vectors and print them
    flattened_list <- lapply(row_numbers_list, function(x) unlist(x))
    included_ids <- unlist(flattened_list)
    print(flattened_list)
    print(included_ids)
    #print("rownumbers")
    #print(row_numbers)
    # Step 2: Identify missing IDs
    missing_ids <- setdiff(nodes_to_consider2, included_ids)
    #print("missing:")
    #print(missing_ids)
    # Step 3: Add each missing ID as an individual sublist to flattened_list
    for (row_number in missing_ids) {
      flattened_list <- append(flattened_list, list(row_number))
    }
    
    # Print the updated flattened_list
    print(flattened_list)
    
    num_colors <- length(flattened_list) # Number of groups
    colors <- distinctColorPalette(num_colors)
    
    # Step 2: Update the nodes dataframe with these colors
    for (i in 1:num_colors) {
      group <- flattened_list[[i]]
      color <- colors[i]
      
      # Update the color for each node in the group
      #nodes$color <- "#ffffff"
      nodes[nodes$id %in% group, "color"] <- color
    }
    nodes$color[nodes$level == 1] <- "#FFFFFF"
    nodes$color[nodes$level == levels_to_exclude] <- "#FFFFFF"
    nodes$number <- 1
    
    print(nodes)
    print(edges)
    
    updated_graph_data(list(nodes = nodes, edges = edges))
    
    visNetworkProxy("eventtree_network") %>%
      visUpdateNodes(nodes = nodes)
    
  })
  
  node_colors_levels <- reactiveVal(NULL)
  
  #hi <- reactiveVal(NULL)
  
  # Observe the finishedColoring button click
  observeEvent(input$finishedColoring, {
    data <- updated_graph_data()
    print(data$nodes)
    
    # Get unique levels from the nodes
    levels <- unique(data$nodes$level)
    min_level <- min(levels)
    max_level <- max(levels)
    
    # Initialize vectors to store incorrect node IDs
    incorrect_color_nodes <- c()
    duplicate_color_nodes <- c()
    
    
    # Iterate over nodes to check the color and level conditions
    for (i in 1:nrow(data$nodes)) {
      node <- data$nodes[i, ]
      
      # Check if the node has color #ffffff and its level isn't min or max
      if (node$color == "#FFFFFF" && !(node$level2 %in% c(min_level, max_level))) {
        incorrect_color_nodes <- c(incorrect_color_nodes, node$id)
      }
    }
    
    # Check for duplicate colors in different levels (ignoring min and max levels)
    non_min_max_nodes <- data$nodes %>% filter(!(level2 %in% c(min_level, max_level)))
    duplicate_colors <- non_min_max_nodes %>% 
      group_by(color) %>% 
      filter(n_distinct(level2) > 1) %>% 
      pull(id)
    
    if (length(duplicate_colors) > 0) {
      duplicate_color_nodes <- unique(duplicate_colors)
    }
    
    # If there are any incorrect color nodes, display a modal with all node IDs
    if (length(incorrect_color_nodes) > 0) {
      showModal(modalDialog(
        title = "Incorrect Node Colours",
        paste("The following nodes have not been coloured correctly:", paste(incorrect_color_nodes, collapse = ", ")),
        easyClose = TRUE
      ))
      return() # Exit the observer to stop further processing
    }
    
    # If there are duplicate color nodes in different levels, show an error
    if (length(duplicate_color_nodes) > 0) {
      showModal(modalDialog(
        title = "Duplicate Node Colours",
        paste("The following nodes have the same colour but different levels:", 
              paste(duplicate_color_nodes, collapse = ", ")),
        easyClose = TRUE
      ))
      return() # Exit the observer to stop further processing
    }
    
    # If no issues, proceed with processing the data
    node_colors_levels_data <- data$nodes
    
    # Group by color, level, and outgoing edges, and calculate the number of nodes
    number_edges <- node_colors_levels_data %>%
      group_by(color, level2, outgoing_edges) %>%
      summarize(number_nodes = sum(number))
    
    # Join the node colors and levels data with the number of nodes per color/level/outgoing_edges
    node_colors_levels_data <- full_join(node_colors_levels_data, number_edges, by = c("color", "level2", "outgoing_edges"))
    
    # Store the node colors and levels in the reactive value
    node_colors_levels(node_colors_levels_data)
  })
  
  
  
  
  
  
  # Render the color and level table
  node_colors_levels2 <- reactiveVal(NULL)  # Keep track of edits
  asymmetric_tree_prior <- reactiveVal(NULL)
  observe({
    prior_type <- input$priorChoice
    node_colors_levels_data <- node_colors_levels()
    
    if (!is.null(node_colors_levels_data)) {
      unique_colors_levels_data <- unique(node_colors_levels_data[node_colors_levels_data$level2 != max(node_colors_levels_data$level2), c("color", "level2", "outgoing_edges", "number_nodes")])
      unique_colors_levels_data$stage <- paste0("u", seq_len(nrow(unique_colors_levels_data)))
      print("unique_colors_levels_data")
      print(unique_colors_levels_data)
      
      max_edges <- max(unique_colors_levels_data$outgoing_edges)
      
      if (!"prior" %in% colnames(unique_colors_levels_data)) {
        unique_colors_levels_data$prior <- ""
      }
      
      # Set the `prior` based on the selected `prior_type`
      if (prior_type == "Specify Prior") {
        unique_colors_levels_data$prior[unique_colors_levels_data$prior == ""] <- "Enter Prior"
      } else if (prior_type == "Uniform 1,1 Prior") {
        for (i in seq_len(nrow(unique_colors_levels_data))) {
          edges <- unique_colors_levels_data$outgoing_edges[i]
          num_repeats <- unique_colors_levels_data$number_nodes[i]
          unique_colors_levels_data$prior[i] <- ifelse(unique_colors_levels_data$prior[i] == "", paste(rep(num_repeats, edges), collapse = ","), unique_colors_levels_data$prior[i])
        }
      } else if (prior_type == "Uniform 1,1 Prior (Type 2)") {
        for (i in seq_len(nrow(unique_colors_levels_data))) {
          edges <- unique_colors_levels_data$outgoing_edges[i]
          unique_colors_levels_data$prior[i] <- ifelse(unique_colors_levels_data$prior[i] == "", paste(rep(1, edges), collapse = ","), unique_colors_levels_data$prior[i])
        }
      } else if (prior_type == "Phantom Individuals Prior") {
        #unique_levels <- unique(unique_colors_levels_data$level2)
        data <- updated_graph_data()
        edges2 <- data$edges
        
        # Find the maximum level
        max_level <- max(node_colors_levels_data$level, na.rm = TRUE)
        
        # Filter out rows where level is the maximum level
        node_colors_levels_data2 <- filter(node_colors_levels_data, level != max_level)
        
        print(node_colors_levels_data2)
        
        equivsize <- max(node_colors_levels_data2$outgoing_edges)
        node_colors_levels_data2$prior <- NA
        
        # Set the prior for the first row to equivsize
        node_colors_levels_data2$prior[1] <- equivsize
        for (i in 1:nrow(node_colors_levels_data2)) {
          
          # Get the current row's 'id' from nodes_to_consider
          current_id <- node_colors_levels_data2$id[i]
          
          # Get the number of outgoing edges for this node (this could be a count of edges with 'from' = current_id)
          outgoing_edges <- node_colors_levels_data2$outgoing_edges[i]
          
          # Step 2: Calculate the new prior (divide current prior by the number of outgoing edges)
          current_prior <- node_colors_levels_data2$prior[i]  # Assuming prior column exists
          new_prior <- current_prior / outgoing_edges
          
          # Step 3: Update the 'prior' for rows in edges where 'from' equals the current 'id'
          # and update the corresponding 'prior' in nodes_to_consider based on 'to'
          to_nodes <- edges2$to[edges2$from == current_id]  # Get all 'to' nodes where 'from' equals current_id
          
          # Update the 'prior' for corresponding nodes in nodes_to_consider
          for (j in to_nodes) {
            node_colors_levels_data2$prior[node_colors_levels_data2$id == j] <- new_prior}
          
        }
        #     node_colors_levels_data2 <- node_colors_levels_data2 %>%
        #        rowwise() %>%
        #        mutate(
        #          prior = paste(rep(prior / outgoing_edges, outgoing_edges), collapse = ", ")
        #        ) %>%
        #        ungroup()  # Ungroup after rowwise operation
        
        # Display the updated dataframe
        print("node_colors_levels_data2")
        print(node_colors_levels_data2)
        asymmetric_tree_prior(node_colors_levels_data2)
        
        df_grouped_by_color <- node_colors_levels_data2 %>%
          group_by(color, outgoing_edges) %>%
          summarise(total_prior = sum(prior, na.rm = TRUE), .groups = "drop")
        df_grouped_by_color$prior <- NA
        
        for (i in 1:nrow(df_grouped_by_color)){
          df_grouped_by_color$prior[i] <- paste(rep(round(as.numeric(df_grouped_by_color$total_prior[i])/as.numeric(df_grouped_by_color$outgoing_edges[i]),3), as.numeric(df_grouped_by_color$outgoing_edges[i])), collapse = ", ")
        }
        # View the result
        print(df_grouped_by_color)
        
        
        # View the merged dataframe
        #print("df_grouped")
        #print(df_grouped)
        
        unique_colors_levels_data <- unique_colors_levels_data %>%
          left_join(df_grouped_by_color %>% select(color, prior), by = "color") %>%
          mutate(prior = coalesce(prior.y, prior.x)) %>%
          select(-prior.x, -prior.y)
        
        # View the result
        print(unique_colors_levels_data)
      }
      
      
      # Store the modified data in node_colors_levels2 instead of node_colors_levels
      node_colors_levels2(unique_colors_levels_data)
      
      output$colorLevelTable <- renderDT({
        
        table_df <- node_colors_levels2() %>%
          arrange(mixedorder(stage)) %>%  # Order by Stage
          select(
            `Stage Colour` = color,
            `Stage` = stage,  
            `Level` = level2,
            `Outgoing Edges` = outgoing_edges,
            `Number of Nodes` = number_nodes,
            `Prior Distribution` = prior
          )
        datatable(
          table_df,  # Render the table using node_colors_levels2
          escape = FALSE,
          editable = TRUE,
          options = list(dom = 't', pageLength = 50),
          rownames = FALSE
        ) %>%
          formatStyle(
            columns = "Stage Colour",
            valueColumns = "Stage Colour",
            backgroundColor = styleEqual(table_df$`Stage Colour`, table_df$`Stage Colour`),
            color = styleEqual(table_df$`Stage Colour`, table_df$`Stage Colour`)
          ) %>%
          formatStyle(columns = "Level", textAlign = "left")
      })
    }
  })
  
  # Observe and handle cell edit events
  observeEvent(input$colorLevelTable_cell_edit, {
    info <- input$colorLevelTable_cell_edit
    node_colors_levels_data <- node_colors_levels2()  # Get the latest edited data
    
    col_name <- names(node_colors_levels_data)[info$col + 1]  # Adjust for 0-based index from DataTable
    
    # Update the corresponding cell in node_colors_levels_data
    node_colors_levels_data[info$row, col_name] <- info$value
    
    # Store the updated data back into node_colors_levels2
    node_colors_levels2(node_colors_levels_data)
    
    # Render the updated table using node_colors_levels2
    output$colorLevelTable <- renderDT({
      datatable(
        node_colors_levels2(),
        escape = FALSE,
        editable = TRUE,
        options = list(dom = 't', pageLength = 50),
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = "color",
          valueColumns = "color",
          backgroundColor = styleEqual(node_colors_levels2()$color, node_colors_levels2()$color),
          color = styleEqual(node_colors_levels2()$color, node_colors_levels2()$color)
        ) %>%
        formatStyle(columns = "level2", textAlign = "left")
    })
  })
  
  
  # Render the DataTable based on the reactive value
  
  
  staged_tree_data <- reactiveVal(NULL)
  
  observeEvent(input$finishedPrior, {
    # Get the updated node_colors_levels data
    if (!is.null(node_colors_levels2())) {
      edited_node_colors_levels_data <- node_colors_levels2()
    } else {
      edited_node_colors_levels_data <- node_colors_levels()
    }
    #edited_node_colors_levels_data <- node_colors_levels2()
    
    # Debugging: Print the edited data to verify prior updates
    print("Edited node_colors_levels_data:")
    print(edited_node_colors_levels_data)
    
    # Get the updated_graph_data
    data <- updated_graph_data()
    prior_type <- input$priorChoice
    
    # Match and apply priors to nodes in updated_graph_data
    for (i in 1:nrow(edited_node_colors_levels_data)) {
      color <- edited_node_colors_levels_data$color[i]
      level2 <- edited_node_colors_levels_data$level2[i]
      prior <- edited_node_colors_levels_data$prior[i]
      
      # Update nodes in updated_graph_data that match the color and level
      data$nodes$prior[data$nodes$color == color & data$nodes$level2 == level2] <- prior
    }
    
    # Update the reactive value with adjusted data
    updated_graph_data(data)
    print(updated_graph_data())
  })
  
  observeEvent(input$viewstagedtree, {
    data <- updated_graph_data()
    print("data")
    print(data)
    prior_type <- input$priorChoice
    
    convertPrior <- function(prior) {
      as.numeric(strsplit(prior, ",")[[1]])
    }
    
    # Create a function to calculate adjusted priors
    adjustPriors <- function(prior, count) {
      prior_values <- convertPrior(prior)
      adjusted <- prior_values / count
      rounded_adjusted <- round(adjusted, 3)  # Round to 2 decimal places
      return(paste(rounded_adjusted, collapse = ","))
    }
    
    data$nodes$adjusted_prior <- ""
    
    if (prior_type != "Phantom Individuals Prior") {
    for (color in unique(data$nodes$color)) {
      for (level2 in unique(data$nodes$level2)) {
        # Filter nodes with the same color and level
        same_group_nodes <- data$nodes[data$nodes$color == color & data$nodes$level2 == level2, ]
        
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
    
    # Debugging: Verify data after processing
    print("Processed Data (nodes):")
    print(data$nodes)
    
    
    } 
    else if (prior_type == "Phantom Individuals Prior") {
      asymmetric_data <- asymmetric_tree_prior()
      print("asymmetric_data")
      print(asymmetric_data)
      
      for (i in 1:nrow(asymmetric_data)){
        data$nodes$adjusted_prior[i] <- paste(rep(round(as.numeric(asymmetric_data$prior[i])/as.numeric(asymmetric_data$outgoing_edges[i]),3), as.numeric(asymmetric_data$outgoing_edges[i])), collapse = ", ")
        
      }
      
    }
    
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
              edge_data$label_prior_frac[edges_from_node[j]] <- paste(edge_data$label1[edges_from_node[j]], "\n", adj_prior_values[j])
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
      
    splitPriors <- function(prior) {
      as.numeric(strsplit(prior, ",")[[1]])
    }
    
    # Function to calculate ratios for each prior
    calculateRatios <- function(priors) {
      total <- sum(priors)
      if (total == 0) return(rep(0, length(priors)))
      return(priors / total)
    }
    
    assignRatiosToEdges <- function(nodes_data, edges_data) {
      # Ensure there's a column to store ratios in edges_data
      if (!"ratio" %in% colnames(edges_data)) {
        edges_data$ratio <- ""
      }
      
      # Ensure there's a column to store ratios in nodes_data
      if (!"ratio" %in% colnames(nodes_data)) {
        nodes_data$ratio <- ""
      }
      
      # Loop through each unique node
      for (i in 1:nrow(nodes_data)) {
        node_id <- nodes_data$id[i]
        prior <- nodes_data$prior[i]
        
        if (!is.na(prior) && prior != "") {
          # Split and calculate the ratios
          prior_values <- splitPriors(prior)
          ratios <- round(calculateRatios(prior_values), 3)
          
          # Assign the ratios to the node
          nodes_data$ratio[i] <- paste(ratios, collapse = ", ")
          
          # Find edges that originate from this node
          edges_from_node <- which(edges_data$from == node_id)
          
          # Ensure there are enough ratios for the edges
          if (length(ratios) >= length(edges_from_node)) {
            for (j in 1:length(edges_from_node)) {
              # Assign the ratio to the edge
              edges_data$ratio[edges_from_node[j]] <- ratios[j]
              edges_data$label_prior_mean[edges_from_node[j]] <- paste(edges_data$label1[edges_from_node[j]], "\n", ratios[j])
            }
          } else {
            warning(paste("Not enough prior values for edges from node", node_id))
          }
        }
      }
      return(list(edges = edges_data, nodes = nodes_data))
    }
    
    updated_data <- assignRatiosToEdges(data$nodes, data$edges)
    data$edges <- updated_data$edges
    data$nodes <- updated_data$nodes

    
    # Function to calculate the variance for each parameter in the Dirichlet distribution
    calculateVariance <- function(priors) {
      total <- sum(priors)
      if (total == 0) return(rep(0, length(priors)))
      return((priors * (total - priors)) / (total^2 * (total + 1)))
    }
    
    # Function to assign variances to nodes
    assignVarianceToNodes <- function(nodes_data) {
      # Ensure there's a column to store variance in nodes_data
      if (!"variance" %in% colnames(nodes_data)) {
        nodes_data$priorvariance <- NA
      }
      
      # Loop through each unique node
      for (i in 1:nrow(nodes_data)) {
        prior <- nodes_data$prior[i]  # Get the prior for the current node
        
        if (!is.na(prior) && prior != "") {  # Ensure prior is valid
          prior_values <- splitPriors(prior)  # Split and convert to numeric
          
          # Calculate variance for each component
          variances <- round(calculateVariance(prior_values), 3)  # Increased precision
          
          # Combine variances into a single string
          variance_str <- paste(variances, collapse = ", ")
          
          # Assign the variance to the node
          nodes_data$priorvariance[i] <- variance_str
        } else {
          nodes_data$priorvariance[i] <- NA
        }
      }
      
      return(nodes_data)
    }
    
    # Calculate variance for nodes
    data$nodes <- assignVarianceToNodes(data$nodes)
    
    # Debugging: Verify nodes data with variance
    print("Processed Data with Variance (nodes):")
    print(data$nodes)
    
    createTooltipWithPrior <- function(prior, ratio, priorvariance) {
      if (!is.na(prior) && prior != "") {
        tooltip_text <- paste(
          "Prior Distribution: Dirichlet(", prior, ")<br>",
          "Prior Mean: (", ratio, ")<br>",
          "Prior Variance: (", priorvariance, ")"
        )
        return(tooltip_text)
      } else {
        return("Leaf nodes have no prior")
      }
    }
    
    # Update the reactive value
    staged_tree_data(data)
    
    observe({
      data <- staged_tree_data()
      if (input$usePriorLabels) {
        data$edges$label <- data$edges$label_prior_mean
      } else {
        data$edges$label <- data$edges$label_prior_frac
      }
      
      # Assign tooltips
      data$nodes$title <- apply(data$nodes, 1, function(row) {
        createTooltipWithPrior(row['prior'], row['ratio'], row['priorvariance'])
      })
      
      # Debugging: Verify tooltips
      print("Tooltips:")
      print(data$nodes$title)
      
      staged_tree_data(data)
    })
    
    output$stagedtree <- renderVisNetwork({
      data <- staged_tree_data()  # Use the reactive data with tooltips
      
      visNetwork(nodes = data$nodes, edges = data$edges, height = "500px") %>%
        visHierarchicalLayout(direction = "LR", levelSeparation = 1000) %>%
        visNodes(
          scaling = list(min = 300, max = 300),
          title = data$nodes$title  # Ensure tooltips are set
        ) %>%
        visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
        visOptions(
          manipulation = list(enabled = FALSE, addEdgeCols = FALSE, addNodeCols = FALSE, editNodeCols = FALSE, editEdgeCols = c("label3"))
          
        ) %>%
        visInteraction(
          dragNodes = FALSE,
          multiselect = TRUE,
          navigationButtons = TRUE
        ) %>%
        visPhysics(
          solver = "forceAtlas2Based",
          forceAtlas2Based = list(gravitationalConstant = -50),
          hierarchicalRepulsion = list(nodeDistance = 300)
        )
    })
  })
  
  
  contracted_data <- reactiveVal(NULL)
  grouped_df2 <- reactiveVal(NULL)
  observeEvent(input$viewceg, {
    data <- staged_tree_data()
    nodes <- data$nodes
    edges <- data$edges
    print(nodes)
    print(edges)
    
    # Initialize contract IDs
    nodes$contract_id <- paste0(nodes$level2, "-", nodes$color)
    
    # Function to update contract IDs by appending connected nodes' contract IDs
    update_contract_ids <- function(nodes, edges) {
      # Contract nodes at levels 1 and 5 separately
      nodes$contract_id[nodes$level2 == 1] <- "1-#FFFFFF"
      nodes$contract_id[nodes$level2 == max(nodes$level2)] <- paste0(max(nodes$level2),"-#FFFFFF")
      
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
    
    nodes <- update_contract_ids(nodes, edges)
    
    #print("Nodes to be contracted:")
    #print(nodes %>% group_by(contract_id) %>% summarise(ids = paste(id, collapse = ", ")))
    
    contracted_nodes <- nodes %>%
      group_by(contract_id) %>%
      summarise(ids = paste(id, collapse = ", "), label = first(label), level = first(level2), color = first(color), prior_variance = first(priorvariance), .groups = 'drop') #prior_mean = first(ratio),
    
    print("Contracted Nodes before:")
    print(contracted_nodes)
    
    # Sort contracted_nodes by current labels numerically
    contracted_nodes <- contracted_nodes[order(as.numeric(gsub("[^0-9]", "", contracted_nodes$label))), ]
    
    # Reassign labels sequentially from v0 to v(n-1) and set the last label as v∞
    num_nodes <- nrow(contracted_nodes)
    contracted_nodes$label <- paste0("w", 0:(num_nodes - 1))
    contracted_nodes$label[num_nodes] <- paste0("w", "\u221E")  # Unicode for ∞
    
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
    print("data:")
    print(data)
    edges <- data$edges
    nodes <- data$nodes
    edges <- merge(edges, nodes, by.x = "from", by.y = "id", all.x = FALSE, suffixes = c("_from", "_to"))
    #edges <- edges[]
    edges2 <- edges %>%
      group_by(color_to, level, label1) %>%
      summarise(
        data_table = sum(sumlabel2),
        prior_table = round(sum(sumlabel3), 3),
        posterior_table = round((prior_table + data_table), 3),
        .groups = 'drop' # Ungroup after summarise
      )
    
    edges3 <- edges2 %>%
      group_by(color_to, level) %>%
      summarise(
        total_stage_posterior = sum(posterior_table),
        .groups = 'drop' # Ungroup after summarise
      )
    
    edges4 <- edges2 %>%
      group_by(color_to, level) %>%
      summarise(
        total_stage_prior = sum(prior_table),
        .groups = 'drop' # Ungroup after summarise
      )
    
    edges2 <- edges2 %>%
      left_join(edges3, by = c("color_to", "level"))
    edges2 <- edges2 %>%
      left_join(edges4, by = c("color_to", "level"))
    
    #format(round(x, 2), nsmall = 2)
    edges2$posteriormean <- format(round((edges2$posterior_table/edges2$total_stage_posterior),3), nsmall = 3)
    edges2$priormean <- round((edges2$prior_table/edges2$total_stage_prior),3)
    
    #print("Summarised Edges with Grouped Sums:")
    print("edges2")
    print(edges2)
    # Display the summarised dataframe
    
    edges <- edges %>%
      left_join(edges2, by = c("color_to", "level", "label1"))
    #print(edges)
    edges <- edges %>% select(from, to, label1, font.size, color_to, smooth, level, prior_table, priormean, data_table, posterior_table, posteriormean)
    
    edges$color <- "#000000"
    print(edges)
    contracted_data(list(nodes = contracted_nodes, edges = edges))
    
    data_number_table <- node_colors_levels2()
    data_number_table$color_to <- data_number_table$color
    print(data_number_table)
    
    grouped_df <- edges2 %>%
      group_by(color_to) %>% 
      summarize(
        priormean = paste0("(", paste(priormean, collapse = ", "), ")"),
        posteriormean = paste0("(", paste(posteriormean, collapse = ", "), ")"),
        data_table = paste0("(", paste(data_table, collapse = ", "), ")"),
        prior_table = paste0("Dirichlet(", paste(prior_table, collapse = ", "), ")"),
        posterior_table = paste0("Dirichlet(", paste(posterior_table, collapse = ", "), ")")
      )
    grouped_df <- rename(grouped_df, color = color_to)
    #grouped_df <- grouped_df %>%
    #  left_join(data_number_table, by = "color_to")
    print("grouped_df")
    #print(grouped_df)
    
    stage_column <- node_colors_levels2()
    stage_column <- stage_column[c(1,5)]
    grouped_df <- left_join(grouped_df, stage_column, by = "color" )
    print(grouped_df)
    grouped_df2(grouped_df)
    #print(grouped_df2())
  })
  
  observe({
    data <- contracted_data()
    if (input$showposteriormean) {
      data$edges$label <- paste(data$edges$label1, "\n", data$edges$posteriormean)
    } else {
      data$edges$label <- paste(data$edges$label1)
    }
    
    contracted_data(data)
   
  })
  
  extract_alpha <- function(dirichlet_str) {
    # Remove "Dirichlet(" and ")"
    params <- str_remove_all(dirichlet_str, "Dirichlet\\(|\\)")
    
    # Split by comma and convert to numeric
    alpha_params <- as.numeric(str_split(params, ",")[[1]])
    return(alpha_params)
  }
  
  # Function to calculate variance of each component in a Dirichlet distribution
  calculate_variance <- function(alpha) {
    # Sum of alpha
    alpha_sum <- sum(alpha)
    
    # Calculate variances for each component
    variances <- sapply(alpha, function(a_i) {
      (a_i * (alpha_sum - a_i)) / (alpha_sum^2 * (alpha_sum + 1))
    })
    
    variances <- round(variances, 3)
    
    return(variances)
  }
  
  # Add tooltip columns to the dataframe
  grouped_df3 <- reactive({
    df <- grouped_df2()
    
    df %>%
      rowwise() %>%
      mutate(
        alpha_params = list(extract_alpha(posterior_table)),
        variances = list(calculate_variance(alpha_params)),
        title = paste0("Posterior Distribution: ", posterior_table, "<br>",
                       "Posterior Mean: ", posteriormean, "<br>",
                       "Posterior Variance:(", paste(variances, collapse = ", "), ")")
      )
    print(df)
  })
  
  output$ceg_network <- renderVisNetwork({
    data <- contracted_data()
    edges <- data$edges
    nodes <- data$nodes
    print("ceg data")
    print(data)
    
    if (is.null(contracted_data())) {
      return(NULL)
    }
    
    
    
    # Prepare tooltips for nodes
    nodes <- nodes %>%
      full_join(grouped_df3(), by = c("color" = "color")) # Assuming `posterior_variance` is in grouped_df2
    print(nodes)
    # Prepare edges (without tooltips)
    # Ensure no tooltip is included for edges
    
    
    visNetwork(nodes, edges, height = "900px") %>%
      visHierarchicalLayout(direction = "LR", levelSeparation = input$levelSeparation) %>%
      visNodes(scaling = list(min = 900, max = 900)) %>%
      visEdges(smooth = TRUE, arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
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
      visPhysics(hierarchicalRepulsion = list(nodeDistance = 990), stabilization = TRUE) %>%
      
      visEvents(
        selectNode = "function(params) {
        var selectedNodeIds = params.nodes; // Array of selected node IDs

        // Store the original colors of the edges
        var edges = this.body.data.edges.get();
        edges.forEach(function(edge) {
          if (edge.originalColor === undefined) {
            edge.originalColor = edge.color; // Store the original edge color
          }
          if (edge.originalFontColor === undefined) {
            edge.originalFontColor = (edge.font && edge.font.color) || '#000000'; // Store the original label color
          }
        });

        // Reset all edges to their original colors
        this.body.data.edges.update(edges.map(function(edge) {
          edge.color = edge.originalColor || '#000000'; // Reset to original or default black
          edge.font = { color: edge.originalFontColor || '#000000' }; // Reset to original or default black
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
            edge.color = '#0000FF'; // Set color to blue
            edge.font = { color: '#0000FF' }; // Set label color to blue
          });
          this.body.data.edges.update(incomingEdges);

          // Highlight edges going out from the selected node (red)
          var outgoingEdges = this.body.data.edges.get({
            filter: function(edge) {
              return edge.from === selectedNodeId;
            }
          });
          outgoingEdges.forEach(function(edge) {
            edge.color = '#FF0000'; // Set color to red
            edge.font = { color: '#FF0000' }; // Set label color to red
          });
          this.body.data.edges.update(outgoingEdges);
        }, this); // Bind `this` to the function to access visNetwork context

        // Redraw network to apply changes
        this.redraw();
      }",
        deselectNode = "function(params) {
        // When deselecting, reset all edges to their original colors
        var edges = this.body.data.edges.get();
        this.body.data.edges.update(edges.map(function(edge) {
          edge.color = edge.originalColor || '#000000'; // Reset to original or default black
          edge.font = { color: edge.originalFontColor || '#000000' }; // Reset to original or default black
          return edge;
        }));
        this.redraw();
      }"
      )%>%
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
  
  extract_floret <- function(nodes, edges, start_label1) {
    # Find the 'from' node associated with the start_label1
    start_edge <- edges[edges$label1 == start_label1, ]
    if (nrow(start_edge) == 0) {
      showModal(
        modalDialog(
          title = "Error",
          paste("'", start_label1, "' not found in data.", sep = ""),
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
      return(NULL)
    }
    
    
    start_node <- start_edge$to[1]
    
    # Initialize sets for floret nodes and edges
    floret_nodes <- setNames(list(start_node), start_node)
    floret_edges <- data.frame()
    
    # Track visited nodes to prevent revisiting them
    visited_nodes <- c(start_node)
    
    # Recursive function to traverse and collect connected nodes and edges
    collect_floret <- function(current_node) {
      # Find edges starting from the current node
      outgoing_edges <- edges[edges$from == current_node, ]
      
      # Eliminate edges leading into the start node and before the start node
      outgoing_edges <- outgoing_edges[outgoing_edges$to != start_node, ]
      
      # Filter outgoing edges to remove any that have already been added (if needed)
      new_edges <- outgoing_edges#[!outgoing_edges$to %in% visited_nodes, ]
      
      # Add the new edges to the floret set
      if (nrow(new_edges) > 0) {
        floret_edges <<- rbind(floret_edges, new_edges)
      }
      
      # Get the 'to' nodes from these edges
      to_nodes <- new_edges$to
      
      # Add the nodes to the floret set if not already added
      new_nodes <- to_nodes[!to_nodes %in% visited_nodes]
      
      # Update visited nodes
      visited_nodes <<- c(visited_nodes, new_nodes)
      
      # Add new nodes to the floret set
      floret_nodes <<- c(floret_nodes, setNames(as.list(new_nodes), new_nodes))
      
      # Recursively process each new node
      for (node in new_nodes) {
        collect_floret(node)
      }
    }
    
    # Start traversal from the start_node
    collect_floret(start_node)
    
    # Filter the nodes dataframe to include only nodes in the floret
    floret_nodes_df <- nodes[nodes$id %in% names(floret_nodes), ]
    
    list(nodes = floret_nodes_df, edges = floret_edges)
  }
  
  output$UpdateTable <- renderDT({
    req(grouped_df2())  # Ensure data is available
    
    # Reorder the columns in the dataframe and sort by `Stage`
    reordered_df <- grouped_df2() %>%
      mutate(stage_num = as.numeric(gsub("\\D", "", stage))) %>%  # Extract the numeric part
      arrange(stage_num) %>%  # Order by the numeric part of Stage
      select(
        `Stage Colour` = color,
        `Stage` = stage,  
        `Prior Distribution` = prior_table,
        `Prior Mean` = priormean,
        `Data` = data_table,
        `Posterior Distribution` = posterior_table,
        `Posterior Mean` = posteriormean
      )
    
    # Render the DataTable with equal column widths
    datatable(
      reordered_df,
      escape = FALSE,  # Ensure we don't escape HTML if not necessary
      editable = TRUE,
      options = list(
        dom = 't', pageLength = 50,# Enable automatic column width calculation
        columnDefs = list(
          list(width = '14%', targets = '_all')  # Set width for all columns
        )
      ),
      rownames = FALSE
    ) %>%
      # Format the Stage Colour column for background color
      formatStyle(
        'Stage Colour',  # The renamed column
        backgroundColor = styleEqual(reordered_df$`Stage Colour`, reordered_df$`Stage Colour`),
        color = styleEqual(reordered_df$`Stage Colour`, reordered_df$`Stage Colour`)
      )
  })
  
  
  process_shapefile <- function(shape_data, input_crs) {
    if (is.na(st_crs(shape_data))) {
      # If the shapefile has no CRS, check if the user has provided one
      if (is.na(input_crs) || input_crs == "") {
        showModal(modalDialog(
          title = "Shapefile has no CRS",
          "Please specify a CRS to proceed.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
        showNotification("Shapefile has no CRS. Please assign a CRS to continue.", type = "warning")
        return(NULL)  # Exit the function if no CRS is provided
      } else {
        # Manually assign the CRS (assuming the user provides the correct CRS code)
        # Example for UK Ordnance Survey National Grid, EPSG:27700
        st_crs(shape_data) <- as.numeric(input_crs)
        
        # You could also assign EPSG:27700 for UK National Grid if no CRS is provided
        # st_crs(shape_data) <- 27700
      }
      
      # Transform the shapefile to WGS84 (EPSG:4326) which is standard for Leaflet maps
      shape_data <- st_transform(shape_data, crs = 4326)
    } else {
      # If the shapefile already has a CRS, transform it to WGS84 (EPSG:4326)
      shape_data <- st_transform(shape_data, crs = 4326)
    }
    
    return(shape_data)
  }
  
  shapefileData <- reactive({
    req(input$shapefile)
    
    # Create a temporary directory to extract the ZIP
    temp_dir <- file.path(tempdir(), as.character(Sys.time()))
    dir.create(temp_dir)
    zip_path <- input$shapefile$datapath
    
    # Unzip the uploaded file
    unzip(zip_path, exdir = temp_dir)
    
    # Search for .shp files recursively in the temporary directory
    shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
    
    if (length(shp_files) == 0) {
      showNotification("No .shp file found in ZIP archive.", type = "error")
      return(NULL)
    }
    
    # Use the first shapefile found
    shape_data <- st_read(shp_files[1])
    
    # Example: Assign CRS and transform to WGS84 if needed
    shape_data <- process_shapefile(shape_data, input$crs)
    
    return(shape_data)
  })
  
  dynamic_fill <- reactiveVal(NULL)
  observeEvent(input$process_shapefile, {
    
    if (!eventtree_pressed()) {
      # Show popup if button1 was not pressed
      showModal(modalDialog(
        title = "Action Required",
        "Please create Event Tree first.",
        easyClose = TRUE,
        footer = NULL
      ))
    } else {
      
    shape_data <- shapefileData()
    req(shape_data)  
    
    # Render the Leaflet map
    library(RColorBrewer)
    
    output$map <- renderLeaflet({
      req(shape_data)
      print("shape_data[[1]]")
      visoutputdata <- updated_graph_data()
      print("visoutputdata")
      print(visoutputdata)
      selected_ids <- selected_polygon()
      print(paste("Selected polygons:", toString(selected_ids)))  # Debugging
      
      
      
     
      for (i in seq_along(shape_data[[1]])) {  # Loop over indices of shape_data[[1]]
        
        # Skip if the current value is NA
        if (is.na(shape_data[[1]][i])) {
          fillColor <- "#FFFFFF"
          next  # Move to the next iteration
        }
        
        # Extract floret data and handle any errors
        floret3 <- tryCatch({
          extract_floret(visoutputdata$nodes, visoutputdata$edges, shape_data[[1]][i])
        }, error = function(e) NULL)
        print(shape_data[[1]][i])
        print(floret3)
        
        # Initialize the fillColor for the current shape
        fillColor <- "white"  # Default color
        
        # Check if floret3 is not NULL
        if (!is.null(floret3)) {
          # Extract nodes from floret3
          nodes <- floret3$nodes
          
          # Identify the maximum level
          max_level <- max(nodes$level, na.rm = TRUE)
          
          # Filter for nodes that are not at the max level
          non_max_level_nodes <- nodes[nodes$level != max_level, ]
          
          # Check conditions for color assignment
          if (nrow(non_max_level_nodes) > 0) {
            if (all(non_max_level_nodes$color == "#FFFFFF")) {
              fillColor <- "orangered"  # All non-max-level nodes are white
            } 
            else if (all(non_max_level_nodes$color != "#FFFFFF")) {
              fillColor <- "darkgreen"  # All non-max-level nodes are white
            }
            else {
              fillColor <- "orange"  # At least one non-max-level node is colored
            }
          }
        }
        
        # Update the color for the specific polygon in the shape data
        shape_data$fillColor[shape_data[[1]] == shape_data[[1]][i]] <- fillColor
      }
      
      
      
      #there needs to be a search over florets here, which changes based on colouring 
      
      #shape_data$fillColor <- "darkgreen"
      
      shape_data$previous_fillColor <- shape_data$fillColor
      
      # Assign colors: Red for selected polygons, or a default color
      shape_data$fillColor <- ifelse(
        shape_data[[1]] %in% selected_ids,  # Check if the polygon is selected
        "blue",                              # Color for selected polygons
        shape_data$previous_fillColor                         # Default color for unselected polygons
      )
      
      leaflet(data = shape_data) %>%
        addTiles() %>%
        addPolygons(
          layerId = ~shape_data[[1]], # Use the BCU column as unique polygon IDs
          fillColor = ~fillColor, # Assign colorblind-friendly colors
          color = "black",
          weight = 1,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          opacity = 1,
          fillOpacity = input$mapOpacity,
          popup = NULL,
          label = ~as.character(shape_data[[1]])
        ) 
    })
  }})
  
  

  

  floret2 <- reactiveVal(NULL)
  
  #selected_polygons <- reactiveVal(character())

  # Reactive value to store the currently selected polygon ID
  selected_polygon <- reactiveVal(NULL)

  # Observe click event on the map
  observeEvent(input$map_shape_click, {
    clicked_id <- input$map_shape_click$id
    
    if (!is.null(clicked_id)) {
      # Get the current selection state (list of selected polygons)
      current_selection <- selected_polygon()
      
      # Print the current selection to debug
      print(paste("Current selected polygon(s):", toString(current_selection)))
      
      # Check if the clicked polygon is already selected
      if (clicked_id %in% current_selection) {
        # Deselect the polygon if it is already selected (remove from selection)
        updated_selection <- setdiff(current_selection, clicked_id)
        selected_polygon(updated_selection)
        
        # Remove the highlight by clearing the "highlighted" group
        leafletProxy("map") %>%
          clearGroup("highlighted")
        
        print(paste("Deselected polygon:", clicked_id))  # Debugging print
      } else {
        # Select the polygon if it is not already selected (add to selection)
        updated_selection <- c(current_selection, clicked_id)
        selected_polygon(updated_selection)
        

        
        print(paste("Selected polygon:", clicked_id))  # Debugging print
      }
      
      # Print the updated list of selected polygons
      print(paste("Updated selected polygon(s):", toString(selected_polygon())))
    }
  })
  


  
  
  
  
  
  first_floret <- reactiveVal(NULL)
  all_florets <- reactiveVal(NULL)
  


  
  # Show the modal when the button is clicked
  observeEvent(input$showFloretModal, {
    clicked_id <- selected_polygon()
    
    if (!is.null(clicked_id)) {
      shape_data <- shapefileData()
      visoutputdata <- updated_graph_data()
      
      
      primary_id <- clicked_id[1]
      
      leafletProxy("map") %>%
        clearGroup("highlighted") 
      
    
      # Initialize an empty list to store all florets
      florets_list <- list()
      floret_colors_list <- list()
      
      # Iterate through clicked IDs to extract florets
      for (id in clicked_id) {
        clicked_data <- shape_data[shape_data[[1]] == id, ]  # Use the current ID
        start_label1 <- clicked_data[[1, 1]]
        
        floret3 <- tryCatch({
          extract_floret(visoutputdata$nodes, visoutputdata$edges, start_label1)
        }, error = function(e) NULL)
        
        # Store the first valid floret in 'first_floret'
        if (is.null(first_floret()) && !is.null(floret3)) {
          first_floret(floret3)
        }
        
        # Store the floret3 in the list if it is not NULL
        if (!is.null(floret3)) {
          florets_list[[id]] <- floret3
          floret_colors_list[[id]] <- floret3$nodes$color  # Store all node colors
        }
      }
      
      # Print each floret and its corresponding nodes and edges
      
      # Check for coloring consistency
      if (length(florets_list) > 1) {
        # Compare all colors for each floret
        all_colors_match <- all(
          sapply(floret_colors_list, function(x) identical(x, floret_colors_list[[1]]))
        )
        
        if (!all_colors_match) {
          showModal(modalDialog(
            title = "Inconsistent Floret Colouring",
            "The selected polygons have inconsistent floret colouring. Please ensure all selected polygons have the same floret colouring.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          first_floret(NULL)
          all_florets(NULL)
          selected_polygon(NULL)
          return() }
      }
      # Combine all florets into a single structure
      if (length(florets_list) > 0) {
        combined_nodes <- do.call(rbind, lapply(florets_list, function(f) f$nodes))
        combined_edges <- do.call(rbind, lapply(florets_list, function(f) f$edges))
        
        # Remove duplicate rows (if any) to ensure clean combined florets
        combined_nodes <- combined_nodes[!duplicated(combined_nodes), ]
        combined_edges <- combined_edges[!duplicated(combined_edges), ]
        
        # Store combined florets in 'all_florets'
        all_florets(list(nodes = combined_nodes, edges = combined_edges))
      } else {
        all_florets(NULL)  # Set all_florets to NULL if no valid florets were found
      }
      print("all_florets")
      print(all_florets)
      
      
      if (is.null(floret3) || (nrow(floret3$nodes) == 0 && nrow(floret3$edges) == 0)) {
        # Show error modal if no floret exists
        showModal(modalDialog(
          title = "Error",
          paste("No floret exists for the selected node(s):", toString(clicked_id)),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        floret <- first_floret()
        print("floret")
        print(floret)
        output$dynamic_vis <- renderVisNetwork({
          floret$nodes$title <- NULL
          visNetwork(floret$nodes, floret$edges) %>%
            visHierarchicalLayout(direction = "LR", levelSeparation = 1000) %>%
            visNodes(scaling = list(min = 300, max = 300)) %>%
            visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 5))) %>%
            visOptions(
              manipulation = list(enabled = FALSE, addEdgeCols = FALSE, addNodeCols = FALSE, editNodeCols = FALSE)
            ) %>%
            visInteraction(dragNodes = FALSE, multiselect = TRUE, navigationButtons = TRUE) %>%
            visPhysics(
              solver = "forceAtlas2Based",
              forceAtlas2Based = list(gravitationalConstant = -50),
              hierarchicalRepulsion = list(nodeDistance = 300)
            ) %>%
            visEvents(
              selectNode = "function(params) {
              Shiny.onInputChange('dynamic_vis_selectedNodes', params.nodes);
            }"
            ) %>%
            visEvents(stabilizationIterationsDone = "function() { this.physics.options.enabled = false; }")
        }
        )
        
        # Show modal dialog with floret visualization
        showModal(modalDialog(
          title = paste("Floret(s) starting from:", toString(clicked_id)),
          pickerInput("existing_colors", "Choose Existing Color:",
                      choices = c("", stored_colors$all_colors),
                      choicesOpt = list(
                        style = paste0("background:", c("#FFFFFF", stored_colors$all_colors), ";")
                      )
          ),
          colourpicker::colourInput("modal_nodeColor", "Choose Node Color", value = "#FFFFFF"),
          actionButton("colorSelectedModalNodes", "Colour Selected Nodes"),
          visNetworkOutput("dynamic_vis"),
          easyClose = FALSE,
          footer = tagList(
            actionButton("close_floret_modal", "Close")  # Custom close button
          ),
        ))
        
        #first_floret(NULL)
      }
    }
  })
  
  observeEvent(input$close_floret_modal, {

    
    # Reset variables and close the modal
    first_floret(NULL)
    all_florets(NULL)
    selected_polygon(NULL)
    removeModal()
  })
  
  
  stored_colors <- reactiveValues(all_colors = character(0))
  
  observe({
    # Ensure graph_data is available before accessing its color
    graph_data <- updated_graph_data()
    
    # Check if graph_data$nodes$color exists and has data
    if (!is.null(graph_data$nodes) && "color" %in% colnames(graph_data$nodes)) {
      # Extract unique colors from graph_data$nodes$color and update stored_colors
      stored_colors$all_colors <- unique(graph_data$nodes$color)
    }
  })

  
  observeEvent(input$colorSelectedModalNodes, {
    # Get selected nodes from the visNetwork
    selected_nodes <- input$dynamic_vis_selectedNodes
    print(paste("Selected nodes:", toString(selected_nodes)))  # Debugging statement

    if (!is.null(all_florets())) {
      # Get the combined edges and nodes from all_florets()
      combined_edges <- all_florets()$edges
      combined_nodes <- all_florets()$nodes
      
      # Function to get the full path from origin to a node (recursive search)
      get_path_to_origin <- function(node, edges) {
        path <- c()
        current_node <- node
        
        while (!is.null(current_node)) {
          # Find the incoming edge to the current node
          incoming_edge <- subset(edges, to == current_node)
          if (nrow(incoming_edge) == 0) break  # No more incoming edges
          
          # Prepend the label to the path
          path <- c(incoming_edge$label1, path)
          
          # Move to the 'from' node of the current edge
          current_node <- incoming_edge$from
        }
        
        # Return the full path as a concatenated string
        return(paste(path, collapse = " -> "))
      }
      
      # Process selected nodes based on their levels
      selected_node_levels <- combined_nodes$level[combined_nodes$id %in% selected_nodes]
      
      if (all(selected_node_levels == 2)) {
        # Use `from` for level 2 nodes
        floret_edges <- subset(combined_edges, from %in% selected_nodes)
        matching_labels <- unique(floret_edges$label1)  # Get unique labels of these edges
        
        # Find all nodes that share the same edge labels
        matching_nodes <- subset(combined_edges, label1 %in% matching_labels)
        matching_node_ids <- unique(matching_nodes$from)
        
        # Combine with the initially selected nodes
        all_selected_nodes <- unique(c(selected_nodes, matching_node_ids))
        
        print("Nodes selected using level 2 logic:")
      } else {
        # Perform recursive search for nodes not at level 2
        selected_paths <- sapply(selected_nodes, function(node) {
          get_path_to_origin(node, combined_edges)
        })
        print("Selected node paths:")
        print(selected_paths)
        
        # Get paths for all nodes
        all_paths <- sapply(combined_nodes$id, function(node) {
          get_path_to_origin(node, combined_edges)
        })
        
        # Match nodes with the same path
        matching_nodes <- combined_nodes$id[all_paths %in% selected_paths]
        
        # Combine with the initially selected nodes
        all_selected_nodes <- unique(c(selected_nodes, matching_nodes))
        
        print("Nodes selected using full path logic:")
      }
      
      # Output or use `all_selected_nodes` as needed
      print(all_selected_nodes)  # Debugging output
    } else {
      showNotification("No florets are available to match edge labels.", type = "error")
    }

    
    
    
    selected_color <- if (input$existing_colors != "") {
      input$existing_colors  # Use the selected colour from the dropdown
    } else {
      input$modal_nodeColor  # Use the new colour from the colour picker
    }
    
    # Check if nodes are selected
    if (!is.null(selected_nodes) && length(selected_nodes) > 0) {
      # Get the floret graph data
      data <- first_floret()
      data_all <- all_florets()
      
      if (!is.null(data)) {
        # Update the node colours for selected nodes
        data$nodes$color[data$nodes$id %in% selected_nodes] <- selected_color
        data_all$nodes$color[data_all$nodes$id %in% all_selected_nodes] <- selected_color
        # Update the reactive value for the floret
       first_floret(data)
       all_florets(data_all)
        # Reflect the change in the visNetwork
        visNetworkProxy("dynamic_vis") %>%
          visUpdateNodes(nodes = data$nodes)
        
        graph_data <- updated_graph_data()
        graph_data$nodes$color[graph_data$nodes$id %in% all_selected_nodes] <- selected_color
        updated_graph_data(graph_data) 
        
        # Extract all unique color values from both the floret and graph_data
        all_colors <- unique(c(stored_colors$all_colors, selected_color, graph_data$nodes$color))
        print(all_colors)
        print("all_colors")
        # Update stored_colors to include all unique colors
        stored_colors$all_colors <- all_colors
        
        # Update the dropdown to include the new and existing colors
        updateSelectInput(session, "existing_colors", choices = c("",stored_colors$all_colors), selected = "")
        
    } else {
      showNotification("No nodes selected to color.", type = "error")
    }
      
      visNetworkProxy("dynamic_vis") %>%
        visUnselectAll()
      
  }})
  
  
  
  
}

shinyApp(ui, server)


