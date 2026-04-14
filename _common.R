library(arrow)
library(purrr)

source("station_registry.R")

parquet_dir <- "_site/data/parquet"
dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

stations <- discover_stations()
save_station_registry(stations)
write_station_pages(stations)

sensor_stations <- stations |>
  filter(station_type == "sensor", has_sensor_data)

walk(seq_len(nrow(sensor_stations)), function(index) {
  station <- sensor_stations[index, ]
  station_data <- read_station_sensor_data(station)
  parquet_output_path <- str_glue(
    "{parquet_dir}/station_{station$parquet_name[[1]]}.parquet"
  )

  write_parquet(station_data, parquet_output_path)
})
