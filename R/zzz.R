.onLoad <- function(libname, pkgname) {
    # Display customized conflict message
    packageStartupMessage(
      "── Conflicts ────────────────────────────────────── ", pkgname, "_conflicts() ──\n",
      "✖ rdirichlet() from gtools masks dirmult::rdirichlet()\n",
      "✖ as_data_frame() from igraph masks dplyr::as_data_frame()\n",
      "✖ groups() from igraph masks dplyr::groups()\n",
      "✖ permute() from igraph masks gtools::permute()\n",
      "✖ union() from igraph masks dplyr::union()\n",
      "✖ dataTableOutput() from shiny masks DT::dataTableOutput()\n",
      "✖ renderDataTable() from shiny masks DT::renderDataTable()\n",
      "✖ alert() from shinyjs masks shinyWidgets::alert()\n",
      "✖ runExample() from shinyjs masks shiny::runExample()\n",
      "✖ show() from shinyjs masks colorspace::show()\n",
      "✖ crossing() from tidyr masks igraph::crossing()\n",
      "ℹ Use the conflicted package to force all conflicts to become errors"
    )
  }


