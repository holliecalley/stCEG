.onLoad <- function(libname, pkgname) {
    # Display customized conflict message
    packageStartupMessage(
    #  "── Conflicts ────────────────────────────────────── ", pkgname, "_conflicts() ──\n",
    #  "✖ rdirichlet() from gtools masks dirmult::rdirichlet()\n",
    #  "✖ as_data_frame() from igraph masks dplyr::as_data_frame()\n",
    #  "✖ groups() from igraph masks dplyr::groups()\n",
    #  "✖ permute() from igraph masks gtools::permute()\n",
    #  "✖ union() from igraph masks dplyr::union()\n",
    #  "✖ dataTableOutput() from shiny masks DT::dataTableOutput()\n",
    #  "✖ renderDataTable() from shiny masks DT::renderDataTable()\n",
    #  "✖ alert() from shinyjs masks shinyWidgets::alert()\n",
    #  "✖ runExample() from shinyjs masks shiny::runExample()\n",
    #  "✖ show() from shinyjs masks colorspace::show()\n",
    #  "✖ crossing() from tidyr masks igraph::crossing()\n",
    #  "ℹ Use the conflicted package to force all conflicts to become errors"
    )

  # R/zzz.R

  # Load the conflicted package
  if (requireNamespace("conflicted", quietly = TRUE)) {
    library(conflicted)

    # Specify which version of the function to prefer
    conflict_prefer("rdirichlet", "gtools")
    conflict_prefer("as_data_frame", "dplyr")
    conflict_prefer("groups", "igraph")
    conflict_prefer("tree", "cli")
    conflict_prefer("permute", "gtools")
    conflict_prefer("union", "dplyr")
    conflict_prefer("simplify", "igraph")
    conflict_prefer("compose", "igraph")
    conflict_prefer("discard", "purrr")
    conflict_prefer("runExample", "colourpicker")
    conflict_prefer("dataTableOutput", "DT")
    conflict_prefer("renderDataTable", "DT")
    conflict_prefer("colourPicker", "colourpicker")
    conflict_prefer("alert", "shinyWidgets")
    conflict_prefer("updateColourInput", "colourpicker")
    conflict_prefer("runExample", "shiny")
    conflict_prefer("show", "colorspace")
    conflict_prefer("colourInput", "colourpicker")
    conflict_prefer("crossing", "igraph")
    conflict_prefer("viridis_pal", "scales")
  }


  }


