library(dplyr)
library(jsonlite)
library(leaflet)
library(purrr)
library(readr)
library(sf)
library(stringr)
library(stringi)
library(terra)
library(tibble)

MAX_RASTER_PIXELS <- 5000000

station_registry_path <- "_station_registry.rds"
station_page_manifest_path <- "_generated_station_pages.txt"
stations_config_path <- "data/stations_config.csv"

slugify_value <- function(value) {
  value |>
    stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("^-|-$", "")
}

format_station_parquet_name <- function(value) {
  value |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("\\s+", "_") |>
    str_replace_all("[^A-Za-z0-9_-]", "")
}

discover_stations <- function(
  stations_path = "data/stations",
  config_path = stations_config_path
) {
  config <- read_csv(config_path, show_col_types = FALSE)

  stations <- config |>
    pmap_dfr(function(
      id,
      flight_display_name,
      flight_dates,
      sensor_display_name
    ) {
      flight_dir <- file.path(stations_path, "flights_data", id)
      sensor_dir <- file.path(stations_path, "sensors_data", id)

      has_flights <- dir.exists(flight_dir)
      has_sensors <- dir.exists(sensor_dir)

      res <- tibble()

      if (has_flights) {
        geotiff_files <- list.files(
          path = flight_dir,
          pattern = "\\.tif$",
          recursive = TRUE,
          full.names = TRUE,
          ignore.case = TRUE
        ) |>
          sort()

        if (length(geotiff_files) > 0) {
          res <- res |>
            bind_rows(tibble(
              station_dir = flight_dir,
              station_dir_name = id,
              station_type = "imagery",
              station_slug = str_c(id, "-flights"),
              station_id = id,
              station_name = flight_display_name,
              station_label = str_glue(
                "{flight_display_name} ({flight_dates})"
              ),
              station_title = str_glue(
                "{flight_display_name} ({flight_dates})"
              ),
              navigation_label = flight_display_name,
              page_slug = str_c(id, "-flights"),
              page_file = str_glue("{id}-flights.qmd"),
              parquet_name = NA_character_,
              parquet_relative_path = NA_character_,
              has_sensor_data = FALSE,
              has_geotiff = TRUE,
              json_files = list(character()),
              geotiff_files = list(geotiff_files)
            ))
        }
      }

      if (has_sensors) {
        json_files <- list.files(
          path = sensor_dir,
          pattern = "\\.json$",
          recursive = TRUE,
          full.names = TRUE
        ) |>
          sort()

        if (length(json_files) > 0) {
          parquet_name <- format_station_parquet_name(sensor_display_name)
          parquet_relative_path <- str_glue(
            "data/parquet/station_{parquet_name}.parquet"
          )

          res <- res |>
            bind_rows(tibble(
              station_dir = sensor_dir,
              station_dir_name = id,
              station_type = "sensor",
              station_slug = str_c(id, "-sensors"),
              station_id = id,
              station_name = sensor_display_name,
              station_label = sensor_display_name,
              station_title = sensor_display_name,
              navigation_label = sensor_display_name,
              page_slug = str_c(id, "-sensors"),
              page_file = str_glue("{id}-sensors.qmd"),
              parquet_name = parquet_name,
              parquet_relative_path = parquet_relative_path,
              has_sensor_data = TRUE,
              has_geotiff = FALSE,
              json_files = list(json_files),
              geotiff_files = list(character())
            ))
        }
      }

      res
    })

  stations
}

save_station_registry <- function(stations, filepath = station_registry_path) {
  saveRDS(stations, filepath)
}

load_station_registry <- function(filepath = station_registry_path) {
  readRDS(filepath)
}

get_station_registry_entry <- function(station_slug_value, stations = NULL) {
  if (is.null(stations)) {
    stations <- load_station_registry()
  }

  station <- stations |>
    filter(station_slug == !!station_slug_value)

  if (nrow(station) != 1) {
    stop(str_glue("Unknown station slug: {station_slug_value}"))
  }

  station
}

read_station_sensor_data <- function(station) {
  json_files <- station$json_files[[1]]
  all_sensor_data <- list()

  for (json_path in json_files) {
    raw_json <- fromJSON(json_path, flatten = TRUE)
    sensors <- raw_json$data$sensores[[1]]

    for (index in seq_len(nrow(sensors))) {
      sensor_data <- sensors$datos[[index]]

      if (nrow(sensor_data) > 0) {
        all_sensor_data[[length(all_sensor_data) + 1]] <- sensor_data |>
          mutate(
            sensor_id = sensors$id_sensor_raw[index],
            parameter = sensors$parametro[index]
          )
      }
    }
  }

  if (length(all_sensor_data) == 0) {
    return(
      tibble(
        sensor_id = integer(),
        parameter = character(),
        date = as.Date(character()),
        hour = character(),
        value = numeric()
      )
    )
  }

  bind_rows(all_sensor_data) |>
    rename(value = valor_raw) |>
    mutate(
      date = as.Date(str_sub(fecha_raw, 1, 10)),
      hour = str_sub(fecha_raw, 12, 16),
      value = as.numeric(value)
    ) |>
    select(sensor_id, parameter, date, hour, value)
}

sensor_station_page_lines <- function(station) {
  parquet_path <- str_glue(
    "_site/{station$parquet_relative_path[[1]]}"
  )

  c(
    "---",
    str_glue('pagetitle: "{station$station_title[[1]]}"'),
    "format:",
    "  dashboard:",
    "    orientation: rows",
    "    theme: minty",
    "    css: styles.css",
    "---",
    "",
    "```{r setup}",
    "#| include: false",
    str_glue('station_slug <- "{station$station_slug[[1]]}"'),
    "library(arrow)",
    "library(dplyr)",
    "library(ggplot2)",
    "library(leaflet)",
    "library(stringr)",
    "",
    "source(\"station_registry.R\")",
    "",
    "station <- get_station_registry_entry(station_slug)",
    str_glue('df <- read_parquet("{parquet_path}")'),
    "```",
    "",
    "```{ojs}",
    "//| echo: false",
    "//| output: false",
    "",
    str_glue(
      'db = DuckDBClient.of({{\n  readings: FileAttachment("{parquet_path}")\n}})'
    ),
    "",
    "parameter_options = await db.query(",
    "  \"SELECT DISTINCT parameter FROM readings ORDER BY parameter\"",
    ").then(r => r.map(d => d.parameter))",
    "",
    "date_bounds = await db.query(",
    "  \"SELECT MIN(date)::VARCHAR AS min_date, MAX(date)::VARCHAR AS max_date FROM readings\"",
    ").then(r => r[0])",
    "",
    "mutable plot_parameter = null",
    "mutable show_modal = false",
    "",
    "from_date = date_from.toISOString().slice(0, 10)",
    "to_date = date_to.toISOString().slice(0, 10)",
    "",
    "param_clause = selected_parameter.length === 0",
    "  ? \"\"",
    "  : `AND parameter IN (${selected_parameter.map(p => `'${p}'`).join(\",\")})`",
    "",
    "mutable current_page = 0",
    "reset_page = { from_date; to_date; selected_parameter; mutable current_page = 0; }",
    "",
    "page_size = 20",
    "",
    "count_result = await db.query(`",
    "  SELECT COUNT(*) AS n",
    "  FROM readings",
    "  WHERE date BETWEEN '${from_date}' AND '${to_date}'",
    "  ${param_clause}",
    "`)",
    "total_rows = Number(count_result[0].n)",
    "total_pages = Math.ceil(total_rows / page_size)",
    "",
    "page_rows = await db.query(`",
    "  SELECT sensor_id, parameter, date::VARCHAR AS date, hour, ROUND(value, 3) AS value",
    "  FROM readings",
    "  WHERE date BETWEEN '${from_date}' AND '${to_date}'",
    "  ${param_clause}",
    "  ORDER BY date, hour",
    "  LIMIT ${page_size} OFFSET ${current_page * page_size}",
    "`)",
    "page_rows_array = Array.from(page_rows)",
    "",
    "downloadCSV = async () => {",
    "  const csv_rows = await db.query(`",
    "    SELECT sensor_id, parameter, date::VARCHAR AS date, hour, ROUND(value, 3) AS value",
    "    FROM readings",
    "    WHERE date BETWEEN '${from_date}' AND '${to_date}'",
    "    ${param_clause}",
    "    ORDER BY date, hour",
    "  `)",
    "  const header = \"sensor_id,parameter,date,hour,value\"",
    "  const body = Array.from(csv_rows)",
    "    .map(r => [r.sensor_id, r.parameter, r.date, r.hour, r.value].join(\",\"))",
    "    .join(\"\\n\")",
    "  const blob = new Blob([header + \"\\n\" + body], { type: \"text/csv\" })",
    "  const url = URL.createObjectURL(blob)",
    "  const a = document.createElement(\"a\")",
    "  a.href = url",
    "  a.download = \"station-data.csv\"",
    "  a.click()",
    "  URL.revokeObjectURL(url)",
    "}",
    "",
    "plot_data = plot_parameter ",
    "  ? await db.query(`",
    "      SELECT (date || ' ' || hour || ':00')::TIMESTAMP AS datetime, value",
    "      FROM readings",
    "      WHERE parameter = '${plot_parameter}'",
    "      AND date BETWEEN '${from_date}' AND '${to_date}'",
    "      ORDER BY datetime",
    "    `)",
    "  : []",
    "",
    "render_plot = () => Plot.plot({",
    "  width: width * 0.85,",
    "  height: 400,",
    "  grid: true,",
    "  title: plot_parameter ? \"Parameter: \" + plot_parameter : \"Select a parameter to plot (click 📈 icon in sidebar)\",",
    "  x: { label: \"Date & Time\" },",
    "  y: { label: plot_parameter || \"Value\" },",
    "  marks: [",
    "    Plot.lineY(plot_data, {x: \"datetime\", y: \"value\", stroke: \"steelblue\", marker: \"circle\"}),",
    "    Plot.tip(plot_data, Plot.pointerX({x: \"datetime\", y: \"value\"}))",
    "  ]",
    "})",
    "```",
    "",
    "# Sensor Data",
    "",
    "## {.sidebar width=25% position=\"right\"}",
    "",
    "```{ojs}",
    "//| echo: false",
    "",
    "viewof selected_parameter = Inputs.checkbox(",
    "  parameter_options,",
    "  {",
    "    label: \"Parameters\",",
    "    format: (x) => htl.html`<div style=\"display: flex; justify-content: space-between; align-items: center; width: 100%;\">",
    "      <span class=\"parameter-item\">${x}</span>",
    "      <button class=\"btn btn-outline-primary btn-sm\" style=\"margin-left: 5px; padding: 0px 5px;\" onclick=${(e) => { ",
    "        e.stopPropagation(); ",
    "        e.preventDefault();",
    "        mutable plot_parameter = x;",
    "        mutable show_modal = true;",
    "        const label = e.target.closest('label');",
    "        if (label) {",
    "          const cb = label.querySelector('input[type=\"checkbox\"]');",
    "          if (cb && !cb.checked) {",
    "            cb.checked = true;",
    "            cb.dispatchEvent(new Event('input', { bubbles: true }));",
    "          }",
    "        }",
    "      }}>📈</button>",
    "    </div>`",
    "  }",
    ")",
    "",
    "htl.html`<div style=\"margin-top: 10px; margin-bottom: 25px;\">",
    "  <button class=\"btn btn-outline-secondary btn-sm\" onclick=${() => {",
    "    viewof selected_parameter.value = [];",
    "    viewof selected_parameter.dispatchEvent(new CustomEvent(\"input\"));",
    "  }}>Reset Parameters</button>",
    "</div>`",
    "",
    "viewof date_from = Inputs.date(",
    "  { label: \"From\", value: new Date(date_bounds.min_date) }",
    ")",
    "",
    "viewof date_to = Inputs.date(",
    "  { label: \"To\", value: new Date(date_bounds.max_date) }",
    ")",
    "```",
    "",
    "## Column {width=100%}",
    "",
    "### Row {height=100%}",
    "",
    "```{ojs}",
    "//| echo: false",
    "//| title: \"Sensor Readings\"",
    "",
    "table_view = htl.html`<div class=\"data-table-container\">",
    "  <div class=\"data-table-toolbar\">",
    "    <button class=\"btn-small\" onclick=${downloadCSV}>CSV</button>",
    "    <div class=\"data-table-pagination\">",
    "      <button class=\"btn-small\"",
    "        onclick=${() => { if (current_page > 0) mutable current_page -= 1 }}",
    "        ?disabled=${current_page === 0}",
    "      >Prev</button>",
    "      <span>Page ${current_page + 1} of ${total_pages} (${total_rows} rows)</span>",
    "      <button class=\"btn-small\"",
    "        onclick=${() => { if (current_page < total_pages - 1) mutable current_page += 1 }}",
    "        ?disabled=${current_page === total_pages - 1}",
    "      >Next</button>",
    "    </div>",
    "  </div>",
    "  <div class=\"data-table-wrapper\">",
    "    <table class=\"data-table\">",
    "      <colgroup>",
    "        <col style=\"width:15%\">",
    "        <col style=\"width:35%\">",
    "        <col style=\"width:20%\">",
    "        <col style=\"width:15%\">",
    "        <col style=\"width:15%\">",
    "      </colgroup>",
    "      <thead>",
    "        <tr>",
    "          ${[\"Sensor ID\", \"Parameter\", \"Date\", \"Hour\", \"Value\"].map(h =>",
    "            htl.html`<th>${h}</th>`",
    "          )}",
    "        </tr>",
    "      </thead>",
    "      <tbody>",
    "        ${page_rows_array.map(r =>",
    "          htl.html`<tr>",
    "            <td>${r.sensor_id}</td>",
    "            <td>${r.parameter}</td>",
    "            <td>${r.date}</td>",
    "            <td>${r.hour}</td>",
    "            <td>${r.value}</td>",
    "          </tr>`",
    "        )}",
    "      </tbody>",
    "    </table>",
    "  </div>",
    "</div>`",
    "",
    "modal_view = htl.html`<div class=\"chart-modal-overlay\" style=\"display: ${show_modal ? 'flex' : 'none'}\">",
    "  <div class=\"chart-modal-content\">",
    "    <span class=\"chart-modal-close\" onclick=${() => mutable show_modal = false}>&times;</span>",
    "    <div class=\"chart-modal-body\">",
    "      ${plot_parameter ? render_plot() : 'No parameter selected'}",
    "    </div>",
    "  </div>",
    "</div>`",
    "",
    "htl.html`${table_view}${modal_view}`",
    "```"
  )
}

imagery_station_page_lines <- function(station) {
  station_id <- station$station_id[[1]]

  tree_dist_rows <- list(
    pontedesor = "| Avis | 0 | 340 | 0 | 0 | 0 |",
    nazare = "| Nazaré | 0 | 0 | 0 | 193 | 0 |",
    cordoba = "| Raso | 327 | 75 | 35 | 0 | 0 |",
    cardena = "| Vegueta | 0 | 0 | 27 | 352 | 0 |",
    segovia = "| Valsain | 0 | 0 | 228 | 221 | 0 |",
    madrid = "| Valdemaqueda | 0 | 0 | 0 | 0 | 251 |"
  )

  current_tree_row <- if (station_id %in% names(tree_dist_rows)) {
    tree_dist_rows[[station_id]]
  } else {
    NULL
  }

  lines <- c(
    "---",
    str_glue('pagetitle: "{station$station_title[[1]]}"'),
    "format:",
    "  dashboard:",
    "    orientation: rows",
    "    theme: minty",
    "    css: styles.css",
    "---",
    "",
    "```{r setup}",
    "#| include: false",
    str_glue('station_slug <- "{station$station_slug[[1]]}"'),
    "library(leaflet)",
    "",
    "source(\"station_registry.R\")",
    "",
    "station <- get_station_registry_entry(station_slug)",
    "```",
    "",
    "# Map {orientation=\"columns\"}",
    "",
    "## Column",
    "```{r}",
    "#| title: \"GeoTIFF Map\"",
    "#| padding: 0",
    "",
    "build_station_map(station)",
    "```"
  )

  if (!is.null(current_tree_row)) {
    lines <- c(
      lines,
      "",
      "# Info",
      "",
      "The table below shows the tree species distribution (number of specimens) for this location:",
      "",
      "| Location | Q. ilex | Q. suber | P. Pinea | P. Pinaster | P. silvestris |",
      "|:---|:---:|:---:|:---:|:---:|:---:|",
      current_tree_row
    )
  }

  lines
}

station_page_lines <- function(station) {
  if (station$station_type[[1]] == "sensor") {
    return(sensor_station_page_lines(station))
  }

  imagery_station_page_lines(station)
}

write_station_pages <- function(stations) {
  previous_pages <- if (file.exists(station_page_manifest_path)) {
    readLines(station_page_manifest_path, warn = FALSE)
  } else {
    character()
  }

  current_pages <- stations$page_file
  stale_pages <- setdiff(previous_pages, current_pages)

  if (length(stale_pages) > 0) {
    file.remove(stale_pages[file.exists(stale_pages)])
  }

  walk(seq_len(nrow(stations)), function(index) {
    station <- stations[index, ]
    writeLines(
      station_page_lines(station),
      station$page_file[[1]],
      useBytes = TRUE
    )
  })

  writeLines(current_pages, station_page_manifest_path)
}

format_geotiff_label <- function(path, station) {
  basename(path) |>
    str_remove(regex("\\.tif$", ignore_case = TRUE)) |>
    str_remove(str_c("^", station$station_dir_name[[1]], "_")) |>
    str_replace_all("_", " ") |>
    str_squish() |>
    str_to_title(locale = "en")
}

build_raster_bounds <- function(raster) {
  extent_polygon <- terra::as.polygons(
    terra::ext(raster),
    crs = terra::crs(raster)
  )
  extent_wgs84 <- terra::project(extent_polygon, "EPSG:4326")
  extent_values <- as.vector(terra::ext(extent_wgs84))

  tibble(
    xmin = extent_values[[1]],
    xmax = extent_values[[2]],
    ymin = extent_values[[3]],
    ymax = extent_values[[4]]
  )
}

build_raster_overlay <- function(raster_path, station) {
  r <- terra::rast(raster_path)

  # Downsample if too large for web display
  current_pixels <- terra::ncell(r)

  if (current_pixels > MAX_RASTER_PIXELS) {
    fact <- ceiling(sqrt(current_pixels / MAX_RASTER_PIXELS))
    if (fact > 1) {
      r <- suppressWarnings(suppressMessages(
        terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
      ))
    }
  }

  # Pre-project to Web Mercator (EPSG:3857) for better performance
  # leaflet::addRasterImage is much faster when project = FALSE
  r_projected <- suppressWarnings(suppressMessages(
    terra::project(r, "EPSG:3857")
  ))

  list(
    raster = r_projected,
    overlay_label = format_geotiff_label(raster_path, station)
  )
}

add_raster_overlay_to_map <- function(map, overlay) {
  max_bytes <- 1024 * 1024 * 512
  r <- overlay$raster
  n_lyr <- terra::nlyr(r)

  # Check if it has 3 or 4 bands, which typically implies RGB or RGBA
  # for "already colorized" imagery. Setting the RGB flag allows leaflet
  # to use the internal colorization instead of applying a palette.
  if (n_lyr >= 3 && n_lyr <= 4) {
    terra::RGB(r) <- 1:n_lyr

    map |>
      addRasterImage(
        r,
        opacity = 0.85,
        project = FALSE,
        group = overlay$overlay_label,
        maxBytes = max_bytes
      )
  } else if (terra::has.colors(r)) {
    # Single band with its own color table. Leaflet handles this
    # automatically if we don't provide a custom palette function.
    map |>
      addRasterImage(
        r,
        opacity = 0.8,
        project = FALSE,
        group = overlay$overlay_label,
        maxBytes = max_bytes
      )
  } else {
    # Single band (or multispectral with >4 bands) without color table/RGB.
    # Use the first band with a default palette.
    r_single <- r[[1]]
    palette_values <- as.numeric(terra::minmax(r_single))

    palette <- colorNumeric(
      palette = hcl.colors(256, "Inferno"),
      domain = palette_values,
      na.color = "transparent"
    )

    map |>
      addRasterImage(
        r_single,
        colors = palette,
        opacity = 0.8,
        project = FALSE,
        group = overlay$overlay_label,
        maxBytes = max_bytes
      )
  }
}

build_imagery_station_map <- function(station) {
  # Disable terra progress bars that can leak into Quarto output
  terra::terraOptions(progress = 0)

  raster_paths <- station$geotiff_files[[1]]

  map_widget <- leaflet(
    options = leafletOptions(preferCanvas = TRUE)
  ) |>
    addProviderTiles(providers$Esri.WorldImagery)

  if (length(raster_paths) == 0) {
    return(
      map_widget |>
        addControl(
          "No GeoTIFF layers were found for this station.",
          position = "topright"
        )
    )
  }

  overlays <- raster_paths |>
    map(
      build_raster_overlay,
      station = station
    )

  for (overlay in overlays) {
    map_widget <- add_raster_overlay_to_map(map_widget, overlay)
  }

  bounds <- overlays |>
    map(~ build_raster_bounds(.x$raster)) |>
    list_rbind()
  overlay_labels <- overlays |>
    map_chr("overlay_label")

  map_widget |>
    fitBounds(
      lng1 = min(bounds$xmin),
      lat1 = min(bounds$ymin),
      lng2 = max(bounds$xmax),
      lat2 = max(bounds$ymax)
    ) |>
    addLayersControl(
      overlayGroups = rev(overlay_labels),
      options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
    )
}

build_station_map <- function(station) {
  build_imagery_station_map(station)
}
