% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_plotting.R
\name{calculate_area_probabilities}
\alias{calculate_area_probabilities}
\title{Calculate Area Probabilities in a Chain Event Graph (CEG)}
\usage{
calculate_area_probabilities(
  path_df,
  unique_values,
  selected_indices,
  last_group,
  shapefile_vals
)
}
\arguments{
\item{path_df}{A data frame containing paths and their corresponding products,
as returned by the \code{calculate_path_products} function.}

\item{unique_values}{A character vector containing the unique values (e.g., labels or categories)
to condition on.}

\item{selected_indices}{A numeric vector of indices corresponding to the groupings of the unique values.}

\item{last_group}{A string representing the last group for which the conditional probability is to be calculated.}

\item{shapefile_vals}{A vector of area names (or other geographical identifiers) from the shapefile data.}
}
\value{
A list with area names as keys and their respective conditional probabilities as values.
If no paths are found for a specific area, the probability is returned as \code{NA}.
}
\description{
This function calculates the conditional probability of a given last group for each area, based on the
path products and area-specific paths. It leverages the \code{calculate_conditional_prob} function for each area.
}
