# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A Shiny web application (R) for tracking January Advisors remote meeting dynamics — attendance, speaking order, streaks, and interpersonal patterns. Deployed at https://januaryadvisors.shinyapps.io/JA-remote-meetings/.

## Running Locally

Open `JA-remote-meetings.Rproj` in RStudio, then run:

```r
shiny::runApp()
```

Or use RStudio's "Run App" button. The app requires `config.yml` (gitignored) with AWS credentials and app password.

## Deploying

```r
rsconnect::deployApp()
```

Config in `rsconnect/shinyapps.io/januaryadvisors/JA-remote-meetings.dcf`. Files excluded from deploy: `blog_figures.R`, `fun_facts.csv`, `JA-remote-meetings.Rmd`, `standup_data.csv`, `wrapped-stats.R`.

## Architecture

The app follows Shiny's standard split-file pattern:

- **`read-data.R`** — sourced first; loads data from AWS S3 (`standupapp` bucket) or falls back to local `standup_data.csv`; defines all package imports
- **`ui.R`** — all UI components: date/error inputs, drag-and-drop meeting order, fun fact submission, charts, individual stat panels, data table
- **`server.R`** — reactive logic: processes form submissions, writes to S3, computes streaks, renders all charts and stats (~500 lines)
- **`helper-functions.R`** — reusable chart-building functions called from `server.R`

## Data

- `standup_data.csv` — primary dataset; one row per meeting with date, who attended, and speaking order
- `fun_facts.csv` — submitted fun facts with ratings
- `config.yml` (gitignored) — AWS credentials and app password; uses the `config` R package

## Key Libraries

`shiny`, `highcharter` (charts), `shinyjqui` (drag-and-drop order input), `aws.s3` (data persistence), `jastyle` (internal JA theme), `DT` (data table), `tidyverse`

## Team Members

The app tracks 13 active members: Adelle, Carly, David, Divia, Gerard, Jeff, Jonathan, Juweek, Kaitlin, Katie, Kelsey, Marian, Shannon. Adding/removing a member requires updating the member list in `read-data.R`, the UI panels in `ui.R`, and the individual stat rendering blocks in `server.R`.

## Notes

- `server.R` has repetitive per-person stat rendering blocks — one block per team member. This is intentional (not a bug), though could be refactored with loops.
- `figures/` (gitignored) holds generated chart exports; do not commit these.
- `JA-remote-meetings.Rmd` and `wrapped-stats.R` are standalone analysis scripts, not part of the running app.
