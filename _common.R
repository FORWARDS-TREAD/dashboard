library(jsonlite)
library(dplyr)
library(arrow)
library(stringr)
library(stringi)

parquet_dir <- "_site/data/parquet"
dir.create(parquet_dir, recursive = TRUE, showWarnings = FALSE)

station_dirs <- list.dirs("data/stations", recursive = FALSE)

for (station_dir in station_dirs) {
  json_files <- list.files(
    path = station_dir,
    pattern = "\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(json_files) == 0) {
    next
  }

  all_sensor_data <- list()
  station_info <- NULL

  for (json_path in json_files) {
    raw_json <- fromJSON(json_path, flatten = TRUE)

    if (is.null(station_info)) {
      station_info <- list(
        name = raw_json$data$nombre[[1]],
        id = raw_json$data$id_elemento[[1]]
      )
    }

    sensors <- raw_json$data$sensores[[1]]

    for (i in seq_len(nrow(sensors))) {
      datos <- sensors$datos[[i]]
      if (nrow(datos) > 0) {
        all_sensor_data[[length(all_sensor_data) + 1]] <- datos |>
          mutate(
            sensor_id = sensors$id_sensor_raw[i],
            parameter = sensors$parametro[i]
          )
      }
    }
  }

  df <- bind_rows(all_sensor_data) |>
    rename(value = valor_raw) |>
    mutate(
      date = as.Date(fecha_raw, format = "%Y-%m-%d"),
      hour = substr(fecha_raw, 12, 16),
      value = as.numeric(value)
    ) |>
    select(sensor_id, parameter, date, hour, value)

  parquet_name <- station_info$name |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_-]", "")

  write_parquet(df, str_glue("{parquet_dir}/station_{parquet_name}.parquet"))
}
