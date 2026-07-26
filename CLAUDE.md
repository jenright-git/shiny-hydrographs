# CLAUDE.md

Guidance for Claude Code when working in this repository.

## Project

A Shiny (R) application for building water level and LNAPL hydrographs from
groundwater monitoring data.

- Entry point: [app.R](app.R) — single-file app for now.
- Run with: `shiny::runApp()` from the project root.

---

## Data

**Source:** Excel upload of esdat or EQuIS water level reports. Imported using
the `gRs::data_processor()` function.

**Background:** Use the web or this resource for background on water levels and LNAPL https://lnapl-3.itrcweb.org/3-key-lnapl-concepts/?print=pdf 

**Expected columns / schema:** all of the following are expected to be detected
as part of the import function:

- `date`
- `depth_unit`
- `dry_indicator_yn`
- `exact_elev`
- `reference_elev`
- `water_depth`
- `water_level`
- `water_level_depth`

**Units and conventions:** assumed to be m by default. Could be mbtoc (meters
below top of casing), mAHD (m Australian Height Datum), ft (feet) or similar.

**Corrections required:** none. All data should be correct from the uploaded
file. Some flags may be appropriate if source data doesn't match what is
expected.

## Plots

**Y axes:** water level (mbtoc) as default, with a dropdown to change to water
elevation etc. A second series for LNAPL thickness — likely a second plot,
patchworked with the first, one on top of the other.

**Series per location:** same as above. One showing water level and one showing
LNAPL thickness, changeable with dropdowns.

**Multiple locations:** default to one location per plot. Option to facet by
location.

**Reference features:** precipitation overlay, tide data, remediation event
markers, screen intervals, dry/NM flags.

**Static or interactive:** ultimately want to save out to a ggplot2 static
image — PNG or a collated PDF document.

**Colours:** Use scientific colour schemes as default.  The ability to change colours via an interactive colour wheel is ideal.

**Date Ranges:** Default to ggplot2 defaults.  Add UI to allow user to change date ranges, breaks, labels etc.

## Outputs

- PNG/PDF export.
- Multi-page PDF of all wells.
- If possible, an Excel export with raw data included.
- Option for a PDF export at A4 landscape, 4 plots per page in a 2 x 2 grid,
  ordered by `sys_loc_code` / `location_code`.

## UI

No set layout. Aim to keep it as simple and user friendly as possible.

- Seagreen colour theme for components.
- Helper text always welcome.

## Constraints

- Deployment target: posit.connect.cloud.
- Large Excel file upload required.

---

## Conventions

- Keep the app in a single `app.R` until it outgrows it; then split into
  `R/` modules with Shiny modules per plot type.
- Prefer base R + tidyverse packages already common in EQuIS workflows.
- Use my personal gRs and AEQuIS packages wherever possible. These are
  installed locally and can be accessed here:
  - `C:\Users\Enrightj\OneDrive - AECOM\Documents\My EQuIS Work\gRs`
  - `C:\Users\Enrightj\OneDrive - AECOM\Documents\My EQuIS Work\AEQuIS`
- I like to use openair for any time averaging required.

- When testing, mainly test on the esdat example report due to it being a longer timeseries and more complete dataset.