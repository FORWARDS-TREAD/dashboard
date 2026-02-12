library(sf)
library(terra)
library(geodata)
library(dplyr)

# 1. Create directory structure
if (!dir.exists("data")) {
  dir.create("data")
}

# --- A. RASTER: Elevation ---
message("Downloading elevation data...")
elev_spain <- elevation_30s(country = "ESP", path = tempdir())

# Crop for Sierra Nevada area
# Extent: xmin, xmax, ymin, ymax
extent_sn <- ext(-3.6, -2.6, 36.8, 37.2)
sierra_dem <- crop(elev_spain, extent_sn)

# Save the GeoTIFF
writeRaster(sierra_dem, "data/elevation_sierra_nevada.tif", overwrite = TRUE)
message("Raster saved: data/elevation_sierra_nevada.tif")

# --- B. VECTOR 1: Park Boundary ---

# 1. Check valid data range
data_range <- minmax(sierra_dem)
max_elev <- data_range[2]

message(paste("Max elevation found in raster:", max_elev, "meters"))

# 2. Define a safe threshold (Top 20% of height)
# If max_elev is NA (bad download).
if (is.na(max_elev)) {
  stop("The raster contains no data (all NAs). Check the download.")
}

# Define top 20% of the mountain as the 'Park'
safe_threshold <- max_elev * 0.8
message(paste(
  "Creating polygon for area above:",
  round(safe_threshold),
  "meters"
))

# 3. Create the Boolean Raster
high_values <- sierra_dem > safe_threshold

# 4. Convert to Polygons
# na.rm = TRUE ensures we don't get polygons for NA areas
park_polygon <- as.polygons(high_values, aggregate = TRUE, na.rm = TRUE) |>
  st_as_sf()

# 5. Filter for the "TRUE" area (Value = 1)
# We use column index [[1]] to be safe against changing column names
park_polygon <- park_polygon[park_polygon[[1]] == 1, ]

# 6. Assign Name and Save
if (nrow(park_polygon) > 0) {
  park_polygon$name <- "Sierra Nevada N.P."
  st_write(
    park_polygon,
    "data/park_boundary.geojson",
    delete_dsn = TRUE,
    quiet = TRUE
  )
  message("Vector 1 saved: data/park_boundary.geojson")
} else {
  stop("Polygon creation failed even with the safe threshold.")
}

# --- C. VECTOR 2: Ibex Sightings ---
# Ensure points fall inside the polygon
set.seed(123)
n_points <- 50
random_points <- st_sample(park_polygon, size = n_points)
ibex_data <- st_sf(
  id = 1:n_points,
  type = sample(c("Male", "Female", "Kid"), n_points, replace = TRUE),
  geometry = random_points
)

st_write(ibex_data, "data/sightings.geojson", delete_dsn = TRUE, quiet = TRUE)
message("Vector 2 saved: data/sightings.geojson")

message("Done! You have 3 files in the 'data/' folder for your dashboard.")
