library(sunburstR)
library(htmlwidgets)

# use a sample of the sequences csv data
sequences <- read.csv(
  system.file("examples/visit-sequences.csv",package="sunburstR")
  ,header = FALSE
  ,stringsAsFactors = FALSE
)[1:328,]

# disable the breadcrumb
sund2b(
  sequences,
  breadcrumbs = sund2bBreadcrumb(
    enabled = FALSE
  )
)

# change the breadcrumb content
sund2b(
  sequences,
  breadcrumbs = sund2bBreadcrumb(
    html = htmlwidgets::JS("
function(nodedata, size, percent) {
  return '<span style=\"font-weight: bold;\">' + nodedata.name + '</span>' + ' ' + size
}
    ")
  ))


# use a sample of the sequences csv data
sequences <- read.csv(
  system.file("examples/visit-sequences.csv",package="sunburstR")
  ,header = FALSE
  ,stringsAsFactors = FALSE
)[1:328,]

# create a d2b sunburst
sund2b(sequences)

# show labels
sund2b(sequences, showLabels = TRUE)

# change the colors
#   using d3.js categorical color scheme
sund2b(
  sequences,
  colors = htmlwidgets::JS("d3.scaleOrdinal(d3.schemeCategory20b)")
)
