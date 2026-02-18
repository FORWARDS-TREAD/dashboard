library(rgbif)
library(wdpar)
library(sf)
library(terra)
library(geodata)
library(stringr)
library(dplyr)

# Create directory structure
if (!dir.exists("data")) {
  dir.create("data")
}

# A. VECTOR 1: Park Boundary ---------------------------------------------------
# Download Spain protected areas (or specify locally if already cached)
# Note: This downloads the full WDPA for Spain, then filters locally
esp_data <- wdpa_fetch("ESP", wait = TRUE, download_dir = tempdir())

park_boundary <- esp_data |>
  filter(str_detect(NAME, regex("Sierra Nevada", ignore_case = TRUE))) |>
  select(
    name = NAME_ENG,
    desig = DESIG_ENG,
    iucn = IUCN_CAT,
    status = STATUS,
    area_km2 = REP_AREA
  ) |>
  st_make_valid() |>
  st_union() |>
  st_as_sf()

# Save the park boundary as a GeoJSON
st_write(
  park_boundary,
  "data/park_boundary.geojson",
  delete_dsn = TRUE
)

# B. VECTOR 2: Ibex Sightings --------------------------------------------------
message("Querying GBIF for Capra pyrenaica occurrences...")

# Search for Spanish ibex within the geographic bounds of Sierra Nevada
bbox <- park_boundary |>
  st_transform(crs = 4326) |>
  st_bbox()

wkt_bbox <- str_glue(
  "POLYGON (({xmin} {ymin}, {xmax} {ymin}, {xmax} {ymax}, {xmin} {ymax}, {xmin} {ymin}))",
  xmin = bbox$xmin,
  ymin = bbox$ymin,
  xmax = bbox$xmax,
  ymax = bbox$ymax
)

gbif_query <- occ_search(
  taxonKey = 2441054,
  geometry = wkt_bbox,
  hasCoordinate = TRUE,
  limit = 300,
  year = "2000,2024"
)

message(str_glue("GBIF response type: {class(gbif_query)}"))
message(str_glue(
  "GBIF data dimensions: {str_c(dim(gbif_query$data), collapse = ' x ')}"
))

# Process results if data exists
if (!is.null(gbif_query$data) && nrow(gbif_query$data) > 0) {
  message(str_glue("Found {nrow(gbif_query$data)} records from GBIF"))

  ibex_data <- gbif_query$data |>
    select(
      id = gbifID,
      species = scientificName,
      longitude = decimalLongitude,
      latitude = decimalLatitude,
      year = year,
      eventDate,
      basisOfRecord
    ) |>
    mutate(
      eventDate = as.Date(eventDate, format = "%Y-%m-%d"),
      type = sample(c("Male", "Female", "Kid"), n(), replace = TRUE),
      data_source = "GBIF"
    ) |>
    filter(!is.na(longitude), !is.na(latitude)) |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(crs = st_crs(park_boundary))

  inside_park <- st_within(ibex_data, park_boundary, sparse = FALSE)[, 1]
  ibex_data <- ibex_data[inside_park, ]

  message(str_glue("Records inside park: {nrow(ibex_data)}"))
} else {
  # Fallback to simulated data
  warning(str_glue("No GBIF data available. Using SIMULATED observations."))

  set.seed(123)
  n_points <- 50
  random_points <- st_sample(park_boundary, size = n_points)

  ibex_data <- st_sf(
    id = str_glue("SIM_{seq_len(n_points)}"),
    species = "Capra pyrenaica",
    type = sample(c("Male", "Female", "Kid"), n_points, replace = TRUE),
    year = 2024,
    eventDate = Sys.Date() - sample(0:365, n_points, replace = TRUE),
    data_source = "SIMULATED",
    geometry = random_points
  )

  message(str_glue("Generated {n_points} simulated points"))
}

st_write(
  ibex_data,
  "data/sightings.geojson",
  delete_dsn = TRUE,
  quiet = TRUE
)
message("Vector 2 saved: data/sightings.geojson")

# C. RASTER: Elevation ---------------------------------------------------------
message("Downloading elevation data...")

extent_sn <- ext(
  bbox_park$xmin - 0.05,
  bbox_park$xmax + 0.05,
  bbox_park$ymin - 0.05,
  bbox_park$ymax + 0.05
)

elev_spain <- elevation_30s(country = "ESP", path = tempdir())
sierra_dem <- crop(elev_spain, extent_sn)

# Save the GeoTIFF
writeRaster(
  sierra_dem,
  "data/elevation_sierra_nevada.tif",
  overwrite = TRUE
)
message("Raster saved: data/elevation_sierra_nevada.tif")

message("Done! You have 3 files in the 'data/' folder for your dashboard.")
