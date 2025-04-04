# -------------------------------------------
# Road Accidents Analysis in the United States
# Author: Salmon Rajneol B
# -------------------------------------------

# 📦 Step 1: Install and Load Required Packages
# (Run install.packages() only once if not already installed)
# install.packages(c("ggplot2", "sf", "leaflet"))

library(ggplot2)
library(sf)
library(leaflet)

# -------------------------------------------
# 📊 Step 2: Load and Prepare Your Dataset
# -------------------------------------------

# 🔹 Simulate Data (Replace with read.csv() for real data)
set.seed(100)
n <- 300
accidents <- data.frame(
  id = 1:n,
  lon = runif(n, -124, -67),  # Longitude range of US
  lat = runif(n, 25, 49),     # Latitude range of US
  severity = sample(c("Fatal", "Non-fatal"), n, replace = TRUE),
  date = sample(seq(as.Date("2019-01-01"), as.Date("2022-12-31"), by="day"), n, replace = TRUE)
)

# Convert to sf (spatial) object
accidents_sf <- st_as_sf(accidents, coords = c("lon", "lat"), crs = 4326)

# Add time period marker
accidents_sf$period <- ifelse(accidents_sf$date < as.Date("2020-03-01"), "Before", "After")

# -------------------------------------------
# 📍 Step 3: Create Static Map with ggplot2
# -------------------------------------------

ggplot(accidents_sf) +
  geom_sf(aes(color = severity, shape = severity), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("Fatal" = "red", "Non-fatal" = "gray")) +
  scale_shape_manual(values = c("Fatal" = 17, "Non-fatal" = 15)) +
  labs(
    title = "Road Accidents in the United States",
    subtitle = "Spatial Distribution by Severity (2019–2022)",
    color = "Severity",
    shape = "Severity",
    caption = "Simulated Data — Replace with real accident dataset"
  ) +
  theme_minimal(base_size = 14)

# Save the plot (optional)
ggsave("visuals/us_accidents_static_map.png", width = 10, height = 6)

# -------------------------------------------
# 🌍 Step 4: Create Interactive Map with leaflet
# -------------------------------------------

leaflet(accidents_sf) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ifelse(severity == "Fatal", "red", "gray"),
    popup = ~paste("Severity:", severity, "<br>", "Date:", date),
    radius = 5,
    fillOpacity = 0.6,
    stroke = FALSE
  ) %>%
  addLegend(
    "bottomright",
    colors = c("red", "gray"),
    labels = c("Fatal", "Non-fatal"),
    title = "Accident Severity"
  )

# -------------------------------------------
# ✅ Optional Summary Table
# -------------------------------------------
summary_table <- accidents_sf |>
  st_drop_geometry() |>
  dplyr::group_by(severity, period) |>
  dplyr::summarise(
    total = n(),
    .groups = 'drop'
  )

print(summary_table)
