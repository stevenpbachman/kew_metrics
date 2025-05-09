% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_leaflet_choropleth.R
\name{leaflet_choropleth_ui}
\alias{leaflet_choropleth_ui}
\alias{leaflet_choropleth_server}
\title{Display a choropleth using leaflet}
\usage{
leaflet_choropleth_ui(id, label, choices)

leaflet_choropleth_server(
  id,
  data,
  edge_countries,
  shapefile = system.file("01_data", "wgsrpd_simple.rds", package = "kew.metrics",
    mustWork = TRUE)
)
}
\arguments{
\item{id}{Module ID, should match across each UI and Server pairing.}

\item{label}{Display label for the control, or \code{NULL} for no label.}

\item{choices}{List of values to select from. If elements of the list are
named, then that name --- rather than the value --- is displayed to the
user. It's also possible to group related inputs by providing a named list
whose elements are (either named or unnamed) lists, vectors, or factors. In
this case, the outermost names will be used as the group labels (leveraging
the \verb{<optgroup>} HTML tag) for the elements in the respective sublist. See
the example section for a small demo of this feature.}

\item{data}{Summary data to map onto the shapefile. The summary data should include a
\code{area_code_l3} column containing the area ID labels.}

\item{edge_countries}{The EDGE countries dataset}

\item{shapefile}{Path to shapefile.}
}
\value{
Shiny module part of a UI containing leaflet map and floating \code{\link[shiny:selectInput]{shiny::selectInput()}}, or
corresponding server code.
}
\description{
The choropleth contains one floating \link[shiny:selectInput]{shiny::selectInput} that allows for switching between
layers displayed on the plot. On the server side, this switch determines which column of data
that is joined to the shapefile is used to determine the fill colour.
}
\note{
Currently the colours used in the palette are hard-coded in the server code for this
module, with a default palette of \href{https://colorbrewer2.org/#type=sequential&scheme=YlGn}{YlGn}
used if a match is not found.
}
