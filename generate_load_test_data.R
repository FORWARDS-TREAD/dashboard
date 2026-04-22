library(tidyverse)
library(jsonlite)
library(lubridate)

generate_data <- function() {
  base_path <- "data/stations/sensors_data/cardena"
  source_files <- c(
    file.path(base_path, "2026-02", "2026-02-04.json"),
    file.path(base_path, "2026-02", "2026-02-05.json")
  )

  # Load source data
  data_samples <- map(source_files, \(f) read_json(f))

  start_date <- as_date("2026-01-01")
  end_date <- as_date("2027-12-31")

  all_dates <- seq(start_date, end_date, by = "day")

  walk(all_dates, \(current_date) {
    # Pick a sample (alternate based on day)
    # R is 1-indexed, so we use day_of_month mod len + 1
    sample_idx <- (day(current_date) %% length(data_samples)) + 1
    new_data <- data_samples[[sample_idx]]

    date_str <- format(current_date, "%Y-%m-%d")
    month_str <- format(current_date, "%Y-%m")

    # Update fecha_raw in the nested list structure
    new_data$data <- map(new_data$data, \(element) {
      element$sensores <- map(element$sensores, \(sensor) {
        sensor$datos <- map(sensor$datos, \(entry) {
          time_part <- str_split_i(entry$fecha_raw, " ", 2)
          entry$fecha_raw <- str_glue("{date_str} {time_part}")
          return(entry)
        })
        return(sensor)
      })
      return(element)
    })

    # Save to file
    output_dir <- file.path(base_path, month_str)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    output_file <- file.path(output_dir, str_glue("{date_str}.json"))
    write_json(
      new_data,
      output_file,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  })
}

generate_data()
