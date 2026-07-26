library(shiny)
library(bslib)
library(ggplot2)
library(patchwork)
library(dplyr)
library(gRs)

# Large gauging exports are the norm; posit.connect.cloud allows this to be set.
options(shiny.maxRequestSize = 250 * 1024^2)

# base R only gained `%||%` in 4.4.0, and the R version on Connect Cloud is not
# ours to pin.
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}


# ---------------------------------------------------------------------------
# Palette and chrome
# ---------------------------------------------------------------------------

# Categorical slots, assigned in this fixed order and never cycled. Taken
# verbatim from a validated light-mode palette: worst adjacent-pair separation
# is CVD dE 9.1 / normal-vision dE 19.6, both clear of the gates for line
# charts. Do not reorder or extend - past eight series the app facets instead.
SERIES_COLOURS <- c(
  "#2a78d6", # blue
  "#eb6834", # orange
  "#1baf7a", # aqua
  "#eda100", # yellow
  "#e87ba4", # magenta
  "#008300", # green
  "#4a3aa7", # violet
  "#e34948" # red
)
MAX_OVERLAY_SERIES <- length(SERIES_COLOURS)

WATER_COLOUR <- SERIES_COLOURS[[1]]
LNAPL_COLOUR <- SERIES_COLOURS[[2]]

# Figures are exported for print, so only the light surface is defined.
INK <- list(
  surface = "#fcfcfb",
  primary = "#0b0b0b",
  secondary = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  axis = "#c3c2b7",
  strip = "#f0efec"
)

SEAGREEN <- "#2e8b57"


# ---------------------------------------------------------------------------
# Measures
# ---------------------------------------------------------------------------

# `type` drives the default unit label and which way the axis runs when the
# user asks for depths to be reversed.
WATER_MEASURES <- list(
  water_depth = list(label = "Depth to water", type = "depth"),
  water_level_depth = list(label = "Water level depth", type = "depth"),
  water_level = list(label = "Groundwater elevation", type = "elevation"),
  exact_elev = list(label = "Exact elevation", type = "elevation"),
  product_corrected_water_level = list(
    label = "Product-corrected groundwater elevation",
    type = "elevation"
  )
)

LNAPL_MEASURES <- list(
  lnapl_thickness = list(label = "LNAPL thickness", type = "thickness"),
  lnapl_depth = list(label = "Depth to LNAPL", type = "depth"),
  lnapl_elevation = list(label = "LNAPL elevation", type = "elevation")
)

measure_label <- function(col) {
  meta <- c(WATER_MEASURES, LNAPL_MEASURES)[[col]]
  if (is.null(meta)) col else meta$label
}

measure_type <- function(col) {
  meta <- c(WATER_MEASURES, LNAPL_MEASURES)[[col]]
  if (is.null(meta)) "depth" else meta$type
}

#' Suggest a y-axis unit label from the measure type and the export's own unit
#'
#' Only a suggestion - the sidebar exposes it as an editable text box, because
#' the datum in use is a site convention that no export column records.
default_unit_label <- function(type, depth_unit) {
  du <- tolower(trimws(depth_unit %||% "m"))
  metric <- du %in% c("m", "meters", "metres", "metre", "meter")
  imperial <- du %in% c("ft", "feet", "foot")

  if (type == "elevation") {
    if (metric) "mAHD" else if (imperial) "ft AMSL" else du
  } else if (type == "thickness") {
    if (metric) "m" else if (imperial) "ft" else du
  } else {
    if (metric) "mbtoc" else if (imperial) "ft btoc" else du
  }
}


# ---------------------------------------------------------------------------
# Import
# ---------------------------------------------------------------------------

#' Read an uploaded gauging export through gRs::data_processor()
#'
#' Shiny stores uploads under a generated name, so the file is copied to a temp
#' path carrying its original extension before readxl sees it. `report_type` is
#' pinned to "water_level" so a chemistry sheet sharing the workbook cannot win
#' the auto-detection.
#'
#' @returns list(data, error, warnings)
import_gauging <- function(datapath, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (!ext %in% c("xlsx", "xls")) {
    return(list(
      data = NULL,
      error = paste0(
        "'", filename, "' is not an Excel workbook. ",
        "Upload the .xlsx or .xls exported from EQuIS or esdat."
      ),
      warnings = character(0)
    ))
  }

  staged <- file.path(tempdir(), paste0("upload_", basename(tempfile()), ".", ext))
  file.copy(datapath, staged, overwrite = TRUE)
  on.exit(unlink(staged), add = TRUE)

  warns <- character(0)
  out <- withCallingHandlers(
    tryCatch(
      gRs::data_processor(staged, report_type = "water_level"),
      error = function(e) structure(conditionMessage(e), class = "import_error")
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  if (inherits(out, "import_error")) {
    return(list(data = NULL, error = as.character(out), warnings = warns))
  }
  if (is.null(out) || nrow(out) == 0) {
    return(list(
      data = NULL,
      error = paste0(
        "No gauging data could be read from '", filename, "'. ",
        "data_processor() needs a sheet carrying a location column, a date ",
        "column and at least one of water_level, water_depth, ",
        "water_level_depth or reference_elev."
      ),
      warnings = warns
    ))
  }

  list(data = out, error = NULL, warnings = warns)
}

#' Describe anything in the import that a user should look at before plotting
#'
#' CLAUDE.md is explicit that no corrections are applied - these are flags only,
#' and the data is passed through untouched.
#'
#' @returns tibble of level / check / detail
import_flags <- function(df) {
  add <- function(acc, level, check, detail) {
    dplyr::bind_rows(acc, tibble::tibble(
      level = level, check = check, detail = detail
    ))
  }
  flags <- tibble::tibble(
    level = character(0), check = character(0), detail = character(0)
  )

  units <- unique(stats::na.omit(df$depth_unit))
  flags <- add(
    flags,
    if (length(units) > 1) "Check" else "OK",
    "Depth unit",
    if (length(units) > 1) {
      paste0(
        "Mixed units in one file: ", paste(units, collapse = ", "),
        ". Values are plotted as supplied - no conversion is applied."
      )
    } else {
      paste0("All records reported in ", units, ".")
    }
  )

  n_na_date <- sum(is.na(df$date))
  if (n_na_date > 0) {
    flags <- add(flags, "Check", "Dates", paste0(
      n_na_date, " record(s) have no readable gauging date and are dropped ",
      "from plots."
    ))
  }

  dupes <- df %>%
    dplyr::filter(!is.na(date)) %>%
    dplyr::count(location_code, sampled_date_time) %>%
    dplyr::filter(n > 1)
  if (nrow(dupes) > 0) {
    flags <- add(flags, "Check", "Duplicate gaugings", paste0(
      nrow(dupes), " location/date-time combination(s) appear more than once. ",
      "Every reading is plotted, so these show as vertical steps."
    ))
  }

  measured <- intersect(names(WATER_MEASURES), names(df))
  empty <- measured[vapply(measured, \(m) all(is.na(df[[m]])), logical(1))]
  if (length(empty) > 0) {
    flags <- add(flags, "Info", "Empty measures", paste0(
      "No values supplied for: ", paste(empty, collapse = ", "),
      ". These are hidden from the y-axis dropdown."
    ))
  }

  n_dry <- sum(df$dry_indicator_yn == "Y", na.rm = TRUE)
  flags <- add(
    flags, if (n_dry > 0) "Info" else "OK", "Dry wells",
    if (n_dry > 0) {
      paste0(n_dry, " record(s) flagged dry. Shown as ticks on the x-axis.")
    } else {
      "No records flagged dry."
    }
  )

  lnapl_present <- intersect(names(LNAPL_MEASURES), names(df))
  n_lnapl <- sum(vapply(
    lnapl_present, \(m) sum(!is.na(df[[m]])), integer(1)
  ))
  flags <- add(
    flags, if (n_lnapl > 0) "Info" else "OK", "LNAPL",
    if (n_lnapl > 0) {
      paste0(n_lnapl, " LNAPL value(s) present. The LNAPL panel can be added.")
    } else {
      "No LNAPL values in this file. The LNAPL panel is unavailable."
    }
  )

  n_no_ref <- sum(is.na(df$reference_elev))
  if (n_no_ref > 0) {
    flags <- add(flags, "Check", "Reference elevation", paste0(
      n_no_ref, " record(s) have no reference elevation, so an elevation ",
      "could not be derived from the dip for those readings."
    ))
  }

  flags
}


# ---------------------------------------------------------------------------
# Plot theme
# ---------------------------------------------------------------------------

#' Report-figure theme
#'
#' Sized in points against a known physical figure size, so the on-screen
#' preview and the exported PNG carry identical text.
theme_hydro <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = INK$surface, colour = NA),
      plot.background = element_rect(fill = INK$surface, colour = NA),
      panel.border = element_rect(colour = INK$axis, fill = NA, linewidth = 0.4),
      panel.grid.major = element_line(colour = INK$grid, linewidth = 0.25),
      panel.grid.minor = element_line(colour = INK$grid, linewidth = 0.15),
      axis.text = element_text(colour = INK$muted, size = rel(0.9)),
      axis.title = element_text(colour = INK$secondary),
      axis.ticks = element_line(colour = INK$axis, linewidth = 0.3),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      strip.background = element_rect(
        fill = INK$strip, colour = INK$axis, linewidth = 0.4
      ),
      strip.text = element_text(
        colour = INK$primary, size = rel(0.9), margin = margin(3, 3, 3, 3)
      ),
      plot.title = element_text(
        colour = INK$primary, face = "bold", size = rel(1.15)
      ),
      plot.subtitle = element_text(colour = INK$secondary, size = rel(0.95)),
      plot.caption = element_text(
        colour = INK$muted, size = rel(0.8), hjust = 0
      ),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )
}


# ---------------------------------------------------------------------------
# Plot construction
# ---------------------------------------------------------------------------

#' Build the x scale once so both panels share it exactly
build_x_scale <- function(date_break, date_label, limits) {
  args <- list(date_labels = date_label, limits = limits, expand = expansion(0.02))
  if (!identical(date_break, "auto")) args$date_breaks <- date_break
  do.call(scale_x_datetime, args)
}

#' One measure, one panel
#'
#' @param colour_by NULL for a single-colour panel, or "location_code" to map
#'   locations onto the fixed categorical slots.
#' @param y_limits optional c(min, max) the panel must span. Used to hold
#'   separate per-location plots on a common y-axis, where a single faceted
#'   plot would have done it for us.
build_panel <- function(
  df, measure, unit_label, colour_by, palette, reverse_y, single_colour,
  facet, ncol, free_y, x_scale, y_from_zero = FALSE, y_limits = NULL
) {
  df <- df[!is.na(df[[measure]]) & !is.na(df$date), , drop = FALSE]

  p <- ggplot(df, aes(x = .data$date, y = .data[[measure]]))

  if (is.null(colour_by)) {
    p <- p +
      geom_line(
        aes(group = .data$location_code),
        colour = single_colour, linewidth = 0.45
      ) +
      geom_point(colour = single_colour, size = 1.1)
  } else {
    p <- p +
      geom_line(
        aes(colour = .data[[colour_by]], group = .data[[colour_by]]),
        linewidth = 0.45
      ) +
      geom_point(aes(colour = .data[[colour_by]]), size = 1.1) +
      scale_colour_manual(values = palette, drop = FALSE)
  }

  y_scale <- if (reverse_y) {
    scale_y_reverse(expand = expansion(0.06))
  } else if (y_from_zero) {
    scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.08)))
  } else {
    scale_y_continuous(expand = expansion(0.06))
  }

  p <- p +
    x_scale +
    y_scale +
    labs(
      x = NULL,
      y = paste0(measure_label(measure), " (", unit_label, ")")
    ) +
    theme_hydro()

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    p <- p + expand_limits(y = y_limits)
  }

  if (facet) {
    p <- p + facet_wrap(
      vars(.data$location_code),
      ncol = ncol,
      scales = if (free_y) "free_y" else "fixed"
    )
  }
  p
}

#' Assemble the hydrograph
#'
#' Returns a single ggplot when the LNAPL panel is off, otherwise a patchwork
#' of water level over LNAPL with the x-axis shared and the legend collected.
#'
#' @param opts list of plot options - see the server for the full set.
build_hydrograph <- function(df, opts) {
  df <- df[!is.na(df$date), , drop = FALSE]
  if (nrow(df) == 0) stop("No records to plot for the current selections.")

  locations <- sort(unique(df$location_code))
  df$location_code <- factor(df$location_code, levels = locations)

  overlay_colours <- !opts$facet && length(locations) > 1

  # Enforced here as well as in the UI: past the eight validated slots there is
  # no colour left to assign, and a silently dropped series is worse than a
  # refusal.
  if (overlay_colours && length(locations) > MAX_OVERLAY_SERIES) {
    stop(
      "Cannot overlay ", length(locations), " locations - the palette holds ",
      MAX_OVERLAY_SERIES, " distinguishable colours. Facet by location instead."
    )
  }

  palette <- stats::setNames(SERIES_COLOURS[seq_along(locations)], locations)
  colour_by <- if (overlay_colours) "location_code" else NULL

  x_limits <- range(df$date, na.rm = TRUE)
  x_scale <- build_x_scale(opts$date_break, opts$date_label, x_limits)

  reverse_water <- opts$reverse_y && measure_type(opts$measure) == "depth"

  # Stacking two faceted grids would not interleave - well 1's LNAPL panel
  # would land a whole grid below its water panel. Instead each location gets
  # its own water-over-LNAPL pair, and the pairs are laid out as the grid.
  if (opts$facet && !identical(opts$lnapl_measure, "none")) {
    return(annotate_plot(
      build_location_grid(df, opts, x_scale, palette, locations),
      opts, single_panel = FALSE
    ))
  }

  p_water <- build_panel(
    df, opts$measure, opts$unit_label, colour_by, palette,
    reverse_y = reverse_water,
    single_colour = WATER_COLOUR,
    facet = opts$facet, ncol = opts$ncol, free_y = opts$free_y,
    x_scale = x_scale
  )

  p_water <- add_dry_ticks(p_water, df, opts)
  p_water <- add_screen_band(p_water, df, opts, locations)

  if (identical(opts$lnapl_measure, "none")) {
    return(annotate_plot(p_water, opts, single_panel = TRUE))
  }

  lnapl_df <- df[!is.na(df[[opts$lnapl_measure]]), , drop = FALSE]
  if (nrow(lnapl_df) == 0) {
    return(annotate_plot(p_water, opts, single_panel = TRUE))
  }

  reverse_lnapl <- opts$reverse_y &&
    measure_type(opts$lnapl_measure) == "depth"

  p_lnapl <- build_panel(
    lnapl_df, opts$lnapl_measure, opts$lnapl_unit_label, colour_by, palette,
    reverse_y = reverse_lnapl,
    single_colour = LNAPL_COLOUR,
    facet = FALSE, ncol = opts$ncol, free_y = opts$free_y,
    x_scale = x_scale,
    # Thickness has a meaningful zero, so anchor the axis to it.
    y_from_zero = measure_type(opts$lnapl_measure) == "thickness"
  )

  # The panels share an x-axis, so only the lower one carries its labels.
  p_water <- p_water +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )

  combined <- p_water / p_lnapl +
    plot_layout(heights = c(2, 1), guides = "collect") &
    theme(legend.position = "bottom")

  annotate_plot(combined, opts, single_panel = FALSE)
}

#' Lay out one water-over-LNAPL pair per location
#'
#' patchwork nests, so each location is assembled as its own two-panel stack
#' and the stacks are then wrapped into the requested number of columns. Every
#' cell therefore carries its own LNAPL panel directly under its water panel.
#' The water panel keeps a one-location facet purely to draw the strip that
#' labels the pair.
build_location_grid <- function(df, opts, x_scale, palette, locations) {
  finite_range <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) NULL else range(x)
  }
  water_range <- finite_range(df[[opts$measure]])
  lnapl_range <- finite_range(df[[opts$lnapl_measure]])

  # A screen band drawn past the data would stretch that one cell's axis, so
  # the shared range has to allow for the bands too or the cells stop matching.
  screens <- screen_bands(df, opts)
  if (!is.null(screens)) {
    water_range <- finite_range(c(water_range, screens$top, screens$bottom))
  }

  reverse_water <- opts$reverse_y && measure_type(opts$measure) == "depth"
  reverse_lnapl <- opts$reverse_y && measure_type(opts$lnapl_measure) == "depth"

  stacks <- lapply(locations, function(loc) {
    df_loc <- df[df$location_code == loc, , drop = FALSE]
    has_water <- any(!is.na(df_loc[[opts$measure]]))
    has_lnapl <- any(!is.na(df_loc[[opts$lnapl_measure]]))

    # Nothing to draw either way - dropped, exactly as a plain facet would.
    if (!has_water && !has_lnapl) return(NULL)

    p_water <- build_panel(
      df_loc, opts$measure, opts$unit_label, NULL, palette,
      reverse_y = reverse_water,
      single_colour = WATER_COLOUR,
      # A one-location facet is only what draws the strip carrying the well
      # name, and a facet needs a row to work from - so on the rare well with
      # no water values the strip moves to the LNAPL panel below.
      facet = has_water, ncol = 1, free_y = FALSE, x_scale = x_scale,
      # Separate plots have no shared scale of their own, so a common y-range
      # has to be handed to each one.
      y_limits = if (opts$free_y && has_water) NULL else water_range
    )
    p_water <- add_dry_ticks(p_water, df_loc, opts)
    p_water <- add_screen_band(p_water, df_loc, opts, loc)
    p_water <- p_water +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

    lnapl_df <- df_loc[!is.na(df_loc[[opts$lnapl_measure]]), , drop = FALSE]

    # A well with no LNAPL still gets an (empty) panel, so every cell is the
    # same shape and the wells stay comparable.
    p_lnapl <- build_panel(
      lnapl_df, opts$lnapl_measure, opts$lnapl_unit_label, NULL, palette,
      reverse_y = reverse_lnapl,
      single_colour = LNAPL_COLOUR,
      facet = !has_water, ncol = 1, free_y = FALSE, x_scale = x_scale,
      y_from_zero = measure_type(opts$lnapl_measure) == "thickness",
      y_limits = if (opts$free_y && has_lnapl) NULL else lnapl_range
    )

    p_water / p_lnapl + plot_layout(heights = c(2, 1))
  })

  stacks <- Filter(Negate(is.null), stacks)
  wrap_plots(stacks, ncol = min(opts$ncol, length(stacks)))
}

#' Mark dry gaugings as ticks along the x-axis
add_dry_ticks <- function(p, df, opts) {
  if (!opts$show_dry) return(p)
  dry <- df[!is.na(df$dry_indicator_yn) & df$dry_indicator_yn == "Y", ,
            drop = FALSE]
  if (nrow(dry) == 0) return(p)
  p +
    geom_rug(
      data = dry,
      mapping = aes(x = .data$date),
      inherit.aes = FALSE,
      sides = "b", colour = INK$muted, linewidth = 0.3, length = unit(0.02, "npc")
    )
}

#' One screened interval per location, or NULL if none can be drawn
#'
#' Only meaningful for depth measures, because a screen depth and an elevation
#' are not on the same scale. A well gauged with more than one recorded screen
#' is reduced to its median, as a single band is all a hydrograph can carry.
screen_bands <- function(df, opts) {
  if (!opts$show_screen) return(NULL)
  if (measure_type(opts$measure) != "depth") return(NULL)
  if (!all(c("top_screen_depth", "bottom_screen_depth") %in% names(df))) {
    return(NULL)
  }

  screens <- df %>%
    dplyr::group_by(location_code) %>%
    dplyr::summarise(
      top = stats::median(.data$top_screen_depth, na.rm = TRUE),
      bottom = stats::median(.data$bottom_screen_depth, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(.data$top), !is.na(.data$bottom))

  if (nrow(screens) == 0) NULL else screens
}

#' Shade the screened interval behind a depth-axis hydrograph
#'
#' In overlay mode it needs a single location, since one band cannot describe
#' several wells.
add_screen_band <- function(p, df, opts, locations) {
  if (!opts$facet && length(locations) > 1) return(p)
  screens <- screen_bands(df, opts)
  if (is.null(screens)) return(p)

  # Drawn first so the data sits on top of the band.
  p$layers <- c(
    list(geom_rect(
      data = screens,
      mapping = aes(ymin = .data$top, ymax = .data$bottom),
      xmin = -Inf, xmax = Inf,
      inherit.aes = FALSE,
      fill = SEAGREEN, alpha = 0.10
    )),
    p$layers
  )
  p
}

#' Attach title, subtitle and caption to whichever object we ended up with
annotate_plot <- function(p, opts, single_panel) {
  title <- if (nzchar(opts$title)) opts$title else NULL
  subtitle <- if (nzchar(opts$subtitle)) opts$subtitle else NULL
  caption <- if (nzchar(opts$caption)) opts$caption else NULL

  if (single_panel) {
    p + labs(title = title, subtitle = subtitle, caption = caption)
  } else {
    p +
      plot_annotation(
        title = title, subtitle = subtitle, caption = caption,
        theme = theme_hydro()
      )
  }
}


# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

help_text <- function(...) {
  div(class = "form-text", style = "margin-top:-0.4rem;margin-bottom:0.6rem;", ...)
}

ui <- page_sidebar(
  title = "Water Level & LNAPL Hydrographs",
  theme = bs_theme(
    version = 5,
    primary = SEAGREEN,
    "border-radius" = "0.4rem"
  ),
  sidebar = sidebar(
    width = 340,
    open = "always",

    fileInput(
      "data_file", "Gauging export",
      accept = c(".xlsx", ".xls"),
      buttonLabel = "Browse...",
      placeholder = "No file selected"
    ),
    help_text(
      "An EQuIS 'Water Levels II' or esdat gauging report. Read with ",
      tags$code("gRs::data_processor()"), " - column names are matched ",
      "automatically, and nothing is corrected or converted."
    ),

    accordion(
      open = c("Series", "Locations"),

      accordion_panel(
        "Series", icon = bsicons::bs_icon("graph-up"),
        selectInput("measure", "Water level measure", choices = NULL),
        textInput("unit_label", "Y-axis unit"),
        help_text(
          "Edit if the site uses a different datum - the export records the ",
          "unit but not the datum."
        ),
        checkboxInput("reverse_y", "Reverse y-axis for depth measures", TRUE),
        help_text(
          "On by default whenever a depth measure is chosen: the axis ",
          "inverts, so shallower water sits higher and the trace reads like ",
          "an elevation plot. Off: depth increases upward, so a falling ",
          "water level plots as a rising line."
        ),
        selectInput(
          "lnapl_measure", "LNAPL panel",
          choices = c("None" = "none")
        ),
        textInput("lnapl_unit_label", "LNAPL unit"),
        help_text(
          "Added as a shorter panel below, sharing the x-axis. Defaults to ",
          "thickness when the file carries it."
        )
      ),

      accordion_panel(
        "Locations", icon = bsicons::bs_icon("geo-alt"),
        selectizeInput(
          "locations", "Locations", choices = NULL, multiple = TRUE,
          options = list(placeholder = "Select one or more locations")
        ),
        div(
          class = "d-flex gap-2 mb-2",
          actionButton("select_all", "All", class = "btn-sm btn-outline-secondary"),
          actionButton("select_none", "Clear", class = "btn-sm btn-outline-secondary")
        ),
        checkboxInput("facet", "One panel per location", FALSE),
        numericInput("ncol", "Panel columns", value = 2, min = 1, max = 4, step = 1),
        checkboxInput("free_y", "Independent y-axis per panel", FALSE),
        help_text(
          "Overlaid locations are capped at ", MAX_OVERLAY_SERIES,
          " so every series keeps a distinguishable colour. Above that, ",
          "switch to one panel per location. A shared y-axis keeps wells ",
          "comparable; an independent one suits wells at different elevations."
        ),
        help_text(
          "With the LNAPL panel on, each location becomes a water-over-LNAPL ",
          "pair. Allow roughly twice the figure height in that case."
        )
      ),

      accordion_panel(
        "Time", icon = bsicons::bs_icon("calendar3"),
        dateRangeInput("date_range", "Date range", start = NULL, end = NULL),
        selectInput(
          "avg_time", "Time average",
          choices = c(
            "None (as gauged)" = "none", "Day" = "day", "Week" = "week",
            "Month" = "month", "Quarter" = "quarter", "Year" = "year"
          )
        ),
        help_text(
          "Averaged with ", tags$code("openair::timeAverage()"),
          ". Dry flags are hidden when averaging."
        ),
        selectInput(
          "date_break", "X-axis breaks",
          choices = c(
            "Automatic" = "auto", "1 month" = "1 month",
            "3 months" = "3 months", "6 months" = "6 months",
            "1 year" = "1 year", "2 years" = "2 years", "5 years" = "5 years"
          )
        ),
        selectInput(
          "date_label", "X-axis label format",
          choices = c(
            "Mmm-yy" = "%b-%y", "Mmm yyyy" = "%b %Y", "yyyy" = "%Y",
            "yyyy-mm" = "%Y-%m", "dd/mm/yyyy" = "%d/%m/%Y"
          )
        )
      ),

      accordion_panel(
        "Annotation", icon = bsicons::bs_icon("type"),
        textInput("title", "Title", ""),
        textInput("subtitle", "Subtitle", ""),
        textAreaInput("caption", "Caption", "", rows = 2),
        checkboxInput("show_dry", "Mark dry gaugings", TRUE),
        checkboxInput("show_screen", "Shade screened interval", FALSE),
        help_text(
          "The screen band is drawn only against a depth axis, and needs a ",
          "single location unless panels are split."
        )
      ),

      accordion_panel(
        "Export", icon = bsicons::bs_icon("download"),
        layout_columns(
          col_widths = c(6, 6),
          numericInput("fig_width", "Width (mm)", 180, min = 40, max = 420),
          numericInput("fig_height", "Height (mm)", 110, min = 40, max = 420)
        ),
        numericInput("fig_dpi", "Resolution (dpi)", 300, min = 96, max = 600, step = 12),
        help_text(
          "180 x 110 mm at 300 dpi fits the full text width of an A4 portrait ",
          "report. The preview below is drawn at the same physical size, so ",
          "what you see is what exports."
        ),
        div(
          class = "d-flex gap-2",
          downloadButton("download_png", "PNG", class = "btn-primary btn-sm"),
          downloadButton("download_pdf", "PDF", class = "btn-outline-primary btn-sm")
        )
      )
    )
  ),

  navset_card_tab(
    nav_panel(
      "Hydrograph", icon = bsicons::bs_icon("graph-up"),
      uiOutput("plot_container")
    ),
    nav_panel(
      "Import checks", icon = bsicons::bs_icon("clipboard-check"),
      uiOutput("import_summary"),
      DT::DTOutput("flag_table")
    ),
    nav_panel(
      "Data", icon = bsicons::bs_icon("table"),
      help_text(
        "The records behind the current plot, after location and date ",
        "filtering and any time averaging. The export carries these records ",
        "on one sheet and the import checks on another."
      ),
      div(
        class = "mb-2",
        downloadButton(
          "download_xlsx", "Download Excel",
          icon = icon("file-excel"), class = "btn-primary btn-sm"
        )
      ),
      DT::DTOutput("data_table")
    )
  )
)


# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  imported <- reactiveVal(NULL)

  observeEvent(input$data_file, {
    res <- import_gauging(input$data_file$datapath, input$data_file$name)

    if (!is.null(res$error)) {
      imported(NULL)
      showNotification(res$error, type = "error", duration = NULL)
      return()
    }

    imported(res$data)
    showNotification(
      sprintf(
        "Loaded %s records from %s locations.",
        format(nrow(res$data), big.mark = ","),
        length(unique(res$data$location_code))
      ),
      type = "message"
    )
  })

  # --- populate controls from the file -------------------------------------

  observeEvent(imported(), {
    df <- imported()

    available <- names(WATER_MEASURES)[vapply(
      names(WATER_MEASURES),
      \(m) m %in% names(df) && any(!is.na(df[[m]])),
      logical(1)
    )]
    updateSelectInput(
      session, "measure",
      choices = stats::setNames(available, vapply(available, measure_label, "")),
      selected = available[[1]]
    )

    lnapl_available <- names(LNAPL_MEASURES)[vapply(
      names(LNAPL_MEASURES),
      \(m) m %in% names(df) && any(!is.na(df[[m]])),
      logical(1)
    )]
    updateSelectInput(
      session, "lnapl_measure",
      choices = c(
        c("None" = "none"),
        stats::setNames(lnapl_available, vapply(lnapl_available, measure_label, ""))
      ),
      # Thickness is the measure asked for on almost every LNAPL site, so it
      # is chosen whenever the file supplies it.
      selected = if ("lnapl_thickness" %in% lnapl_available) {
        "lnapl_thickness"
      } else if (length(lnapl_available) > 0) {
        lnapl_available[[1]]
      } else {
        "none"
      }
    )

    locs <- sort(unique(df$location_code))
    updateSelectizeInput(
      session, "locations",
      choices = locs, selected = locs[[1]], server = TRUE
    )

    dates <- range(as.Date(df$date), na.rm = TRUE)
    updateDateRangeInput(
      session, "date_range",
      start = dates[[1]], end = dates[[2]],
      min = dates[[1]], max = dates[[2]]
    )
  })

  # Unit labels follow the measure unless the user has typed over them.
  observeEvent(input$measure, {
    df <- imported()
    req(df, input$measure)
    updateTextInput(session, "unit_label", value = default_unit_label(
      measure_type(input$measure), df$depth_unit[[1]]
    ))
    # A depth axis reads the wrong way up untouched, so reversing it is the
    # default whenever a depth measure is picked.
    if (measure_type(input$measure) == "depth") {
      updateCheckboxInput(session, "reverse_y", value = TRUE)
    }
  })

  observeEvent(input$lnapl_measure, {
    df <- imported()
    req(df, input$lnapl_measure)
    if (identical(input$lnapl_measure, "none")) return()
    updateTextInput(session, "lnapl_unit_label", value = default_unit_label(
      measure_type(input$lnapl_measure), df$depth_unit[[1]]
    ))
  })

  observeEvent(input$select_all, {
    df <- imported()
    req(df)
    updateSelectizeInput(
      session, "locations",
      selected = sort(unique(df$location_code))
    )
  })

  observeEvent(input$select_none, {
    updateSelectizeInput(session, "locations", selected = character(0))
  })

  # --- data pipeline -------------------------------------------------------

  filtered <- reactive({
    df <- imported()
    req(df, input$locations, input$measure)

    df <- df %>%
      dplyr::filter(.data$location_code %in% input$locations, !is.na(.data$date))

    if (length(input$date_range) == 2 && !anyNA(input$date_range)) {
      df <- df %>%
        dplyr::filter(
          as.Date(.data$date) >= input$date_range[[1]],
          as.Date(.data$date) <= input$date_range[[2]]
        )
    }
    df
  })

  averaged <- reactive({
    df <- filtered()
    if (identical(input$avg_time, "none") || nrow(df) == 0) return(df)

    value_cols <- intersect(
      c(names(WATER_MEASURES), names(LNAPL_MEASURES)), names(df)
    )
    slim <- as.data.frame(df[, c("date", "location_code", value_cols)])

    out <- tryCatch(
      openair::timeAverage(
        slim, avg.time = input$avg_time, type = "location_code",
        statistic = "mean"
      ),
      error = function(e) NULL
    )
    if (is.null(out)) {
      showNotification(
        "Time averaging failed for this selection - showing gauged values.",
        type = "warning"
      )
      return(df)
    }

    out <- tibble::as_tibble(out)
    out$location_code <- as.character(out$location_code)
    # Averaging collapses the flag columns, so they are re-added as absent.
    out$dry_indicator_yn <- NA_character_
    out$top_screen_depth <- NA_real_
    out$bottom_screen_depth <- NA_real_
    out
  })

  plot_opts <- reactive({
    list(
      measure = input$measure,
      unit_label = input$unit_label %||% "",
      lnapl_measure = input$lnapl_measure %||% "none",
      lnapl_unit_label = input$lnapl_unit_label %||% "",
      reverse_y = isTRUE(input$reverse_y),
      facet = isTRUE(input$facet),
      ncol = max(1, as.integer(input$ncol %||% 2)),
      free_y = isTRUE(input$free_y),
      date_break = input$date_break %||% "auto",
      date_label = input$date_label %||% "%b-%y",
      title = input$title %||% "",
      subtitle = input$subtitle %||% "",
      caption = input$caption %||% "",
      show_dry = isTRUE(input$show_dry) && identical(input$avg_time, "none"),
      show_screen = isTRUE(input$show_screen) && identical(input$avg_time, "none")
    )
  })

  hydrograph <- reactive({
    df <- averaged()
    opts <- plot_opts()

    validate(
      need(nrow(df) > 0, "No records match the current location and date selections."),
      need(
        any(!is.na(df[[opts$measure]])),
        paste0(
          "No ", tolower(measure_label(opts$measure)),
          " values for the current selections. Try another measure."
        )
      ),
      need(
        opts$facet || length(unique(df$location_code)) <= MAX_OVERLAY_SERIES,
        paste0(
          length(unique(df$location_code)), " locations selected. Overlaying ",
          "is capped at ", MAX_OVERLAY_SERIES,
          " so each series keeps a distinguishable colour - tick ",
          "'One panel per location', or select fewer."
        )
      )
    )

    build_hydrograph(df, opts)
  })

  # --- preview -------------------------------------------------------------

  # Preview at the export's physical size: mm -> px at the same 96 dpi the
  # renderer uses, so point sizes land identically in both.
  px <- function(mm) round(mm / 25.4 * 96)

  output$plot_container <- renderUI({
    w <- px(input$fig_width %||% 180)
    h <- px(input$fig_height %||% 110)
    div(
      style = "overflow:auto;",
      plotOutput("hydrograph", width = paste0(w, "px"), height = paste0(h, "px"))
    )
  })

  output$hydrograph <- renderPlot(
    hydrograph(),
    res = 96,
    bg = INK$surface
  )

  # --- exports -------------------------------------------------------------

  export_stem <- reactive({
    locs <- input$locations %||% character(0)
    tag <- if (length(locs) == 1) locs else paste0(length(locs), "-locations")
    paste0(
      "hydrograph_", tag, "_", format(Sys.Date(), "%Y%m%d")
    )
  })

  save_figure <- function(file, device) {
    ggsave(
      filename = file,
      plot = hydrograph(),
      device = device,
      width = input$fig_width,
      height = input$fig_height,
      units = "mm",
      dpi = input$fig_dpi,
      bg = INK$surface
    )
  }

  output$download_png <- downloadHandler(
    filename = function() paste0(export_stem(), ".png"),
    content = function(file) save_figure(file, ragg::agg_png)
  )

  output$download_pdf <- downloadHandler(
    filename = function() paste0(export_stem(), ".pdf"),
    content = function(file) save_figure(file, grDevices::cairo_pdf)
  )

  # --- supporting tabs -----------------------------------------------------

  output$import_summary <- renderUI({
    df <- imported()
    if (is.null(df)) {
      return(div(
        class = "text-muted p-3",
        "Upload a gauging export to see import checks."
      ))
    }
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(
        "Records", format(nrow(df), big.mark = ","),
        showcase = bsicons::bs_icon("list-ol"), theme = "primary"
      ),
      value_box(
        "Locations", length(unique(df$location_code)),
        showcase = bsicons::bs_icon("geo-alt"), theme = "primary"
      ),
      value_box(
        "Date range",
        paste(format(range(as.Date(df$date), na.rm = TRUE), "%b %Y"), collapse = " to "),
        showcase = bsicons::bs_icon("calendar3"), theme = "primary"
      ),
      value_box(
        "Unit", paste(unique(stats::na.omit(df$depth_unit)), collapse = ", "),
        showcase = bsicons::bs_icon("rulers"), theme = "primary"
      )
    )
  })

  output$flag_table <- DT::renderDT({
    df <- imported()
    req(df)
    DT::datatable(
      import_flags(df),
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE),
      colnames = c("", "Check", "Detail")
    )
  })

  # Shared by the table and the Excel export, so what is downloaded is exactly
  # what is on screen.
  table_data <- reactive({
    df <- averaged()
    keep <- intersect(
      c("location_code", "date", "depth_unit", "dry_indicator_yn",
        names(WATER_MEASURES), names(LNAPL_MEASURES), "reference_elev",
        "task_code", "remark"),
      names(df)
    )
    df[, keep, drop = FALSE]
  })

  output$data_table <- DT::renderDT({
    df <- table_data()
    req(nrow(df) > 0)
    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(pageLength = 25, scrollX = TRUE)
    ) %>%
      DT::formatRound(
        columns = intersect(
          names(df),
          c(names(WATER_MEASURES), names(LNAPL_MEASURES), "reference_elev")
        ),
        digits = 3
      )
  })

  output$download_xlsx <- downloadHandler(
    filename = function() paste0(export_stem(), "_data.xlsx"),
    content = function(file) {
      df <- table_data()
      req(nrow(df) > 0)
      writexl::write_xlsx(
        list(
          "Plot data" = as.data.frame(df),
          "Import checks" = as.data.frame(import_flags(imported()))
        ),
        path = file
      )
    }
  )
}

shinyApp(ui, server)
