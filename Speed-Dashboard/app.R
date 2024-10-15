library(htmltools)
library(leaflet)

squake <- SharedData$new(quakes)

container <- layout_sidebar(
  class = "p-0",
  sidebar = sidebar(
    title = "Earthquakes off Fiji",
    bg = "#1E1E1E",
    width = "35%",
    class = "fw-bold font-monospace",
    filter_slider("mag", "Magnitude", squake, ~mag)
  ),
  leaflet(squake) |> addTiles() |> addCircleMarkers()
)

tagAppendAttributes(container, style = css("--bs-card-border-color" = "#1E1E1E"))
