# CLAUDE.md

Guidance for Claude Code when working in this repository.

## Project

A Shiny (R) application for building water level and LNAPL hydrographs from
groundwater monitoring data.

- Entry point: [app.R](app.R) — single-file app for now.
- Run with: `shiny::runApp()` from the project root.

---

<!--
Fill in the sections below with what you actually want. Anything left as a
placeholder is an open question — Claude should ask rather than assume.
-->

## Data

**Source:** <!-- Excel upload of esdat or EQuIS water level reports. Imported using the gRs::data_processor() function.

**Expected columns / schema:**
<!--   Columns all expected to be detected as part of import function
  "date"
  "depth_unit",
  "dry_indicator_yn",
  "exact_elev",
  "reference_elev",
  "water_depth",
  "water_level",
  "water_level_depth" -->

**Units and conventions:**
<!-- assumed to be m by default. Could be mbtoc (meters below top of casing), mAHD (m Australian Height Datum), ft (feet) or similar.  -->

**Corrections required:**
<!-- No corrections should be required.  All data should be correct from uploaded file.
Some flags may be appropriate if source data doesnt match what is expected -->

## Plots

**Y axes:** <!-- water level (mbtoc) as default. Ability to change via drop down to water elevation etc.  A second series of LNAPL thickness. Likely as a second plot and patchwork the two one on top of the other -->

**Series per location:** <!-- Same as above.  One showing water level and one showing LNAPL thickness.  Ability to change with dropdowns -->

**Multiple locations:** <!-- Default to one location per plot. Option to facet by location. -->

**Reference features:** <!-- precipitation overlay, tide data, remediation event
markers, screen intervals, dry/NM flags -->

**Static or interactive:** <!-- Ultimately want to save out to ggplot2 static image.  PNG or collated PDF document. -->

## Outputs

<!-- PNG/PDF export. Multi-page PDF of all wells. 
     If possible an excel export with raw data included.
     Option for a PDF export with A4 landscape, 4 plots per page in a grid 2 x 2. Ordered by sys_loc_code / location_code -->

## UI

<!-- No set layout. Aim to keep as simple and user friendly as possible.
     Seagreen colour theme for components
     Helper text always welcome. -->

## Constraints

<!-- deployment target (posit.connect.cloud), large excel file upload required. -->

---

## Conventions

<!-- Update these as the project takes shape. -->

- Keep the app in a single `app.R` until it outgrows it; then split into
  `R/` modules with Shiny modules per plot type.
- Prefer base R + tidyverse packages already common in EQuIS workflows.
- Use my personal gRs and AEQuIS packages wherever possible.  These are installed locally and can be accessed here: 
          - C:\Users\Enrightj\OneDrive - AECOM\Documents\My EQuIS Work\gRs
          - C:\Users\Enrightj\OneDrive - AECOM\Documents\My EQuIS Work\AEQuIS
- I like to use openair for any time averaging required.