# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A Shiny web application (R) for tracking January Advisors remote meeting dynamics — attendance, speaking order, streaks, and interpersonal patterns. Branded "Stand-app!" with a baseball theme. Deployed at https://januaryadvisors.shinyapps.io/JA-remote-meetings/.

## Running Locally

Open `JA-remote-meetings.Rproj` in RStudio, then run:

```r
shiny::runApp()
```

Or use RStudio's "Run App" button. The app requires `config.yml` (gitignored) with AWS credentials and app password.

When run interactively (RStudio), startup also syncs all fun facts to a Google Sheet via `googlesheets4` (prompts for Google login the first time). The deployed app skips this (`if (interactive())` in `read-data.R`) because it has no Google credentials.

## Deploying

```r
rsconnect::deployApp()
```

Config in `rsconnect/shinyapps.io/januaryadvisors/JA-remote-meetings.dcf`. Files excluded from deploy: `blog_figures.R`, `fun_facts.csv`, `JA-remote-meetings.Rmd`, `standup_data.csv`, `wrapped-stats.R`.

## Architecture

The app follows Shiny's standard split-file pattern:

- **`roster.csv`** — every person who has ever been in the data, with `active` TRUE/FALSE. Drives all member lists in the app
- **`read-data.R`** — sourced first; loads data from AWS S3 (`standupapp` bucket), reads the roster, builds `attended` (long format: one row per person per meeting, keyed by `meeting_id` = row order) and computes all stats (`stats`, `freq_first_last`, `personal_records`, streaks, `batting_average`); defines all package imports
- **`ui.R`** — all UI components: date/error inputs, drag-and-drop lineup, fun fact submission, scoreboard, charts, baseball cards (generated from `active_members`), data table
- **`server.R`** — reactive logic: processes form submissions, writes to S3, renders all charts and stats; per-person outputs are created in a loop over `active_members`
- **`helper-functions.R`** — palettes (`position_colors`, `member_colors`), chart builders (`make_order_hist`, `make_colors_chart`) the baseball card UI (`person_panel`, `personal_records_section`), and `on_this_day_fact` (fun fact from ~a year ago, shown under the scoreboard next to the random "Show me a fun fact!" button)

All stats are computed once at app startup, so they (including "Led off in <month>") reflect the data as of the last restart.

## Data

- S3 `standapp-data.rds` — primary dataset; one row per meeting with date, error, and one column per person holding their speaking order (NA = absent). Use row order, not `date`, for chronology. On 2026-10-09, 11 one-person placeholder rows (left over from manually adding new people) were removed; the pre-cleanup data is saved in the bucket as `standapp-data-backup-2026-10-09.rds`
- `fun_facts.csv` — old local export of fun facts; the live copy is S3 `funfact-data.rds`
- `standup_data.csv` — old local copy (2021), not used by the app
- `config.yml` (gitignored) — AWS credentials and app password; uses the `config` R package

## Key Libraries

`shiny`, `highcharter` (charts), `shinyjqui` (drag-and-drop order input), `aws.s3` (data persistence), `jastyle` (internal JA theme), `DT` (data table), `tidyverse`, `googlesheets4` (local-only fun fact sync)

## Team Members

Edit `roster.csv` only — nothing else names people:
- **New person:** add a row with `active = TRUE`. Their empty data column is created on load and saved to S3 with the next submission. Do not add a placeholder row to the data.
- **Person leaves:** set `active = FALSE`. Their history stays in the data and still counts toward others' pairwise stats.
- Never delete a roster row; the app stops with an error if the data has a column not in the roster (protects history on save).

Attendance rate ("Who suits up?") and "Most consecutive games" count meetings since each person's first appearance in the data. "Most likely to call on / be called on by" only name active members.

## Design

- **Brand:** January Advisors — navy `#004E7B`, teal `#00B9C3`, orange `#FAA634`, logo blue `#2495B7`, text `#262626`; font Onest. Reference stylesheet: `brand/ja_report.css`. App styles live in `www/custom.css`.
- **Baseball theme:** user-facing copy uses baseball terms — meetings are "games", speaking order is the "lineup" (the name list is the "Roster"), going first is "leadoff", going last is "cleanup", the error-free-meeting streak is a "hitting streak", and the share of error-free meetings is the "Batting Average" (shown as e.g. 0.841). Per-person panels are "Baseball Cards"; their attendance run is "Most consecutive games" (not "hitting streak", which is team-level). Keep new copy consistent with this.
- **Colors in charts:** bars/histograms use navy. "Last 10 games" tiles use `position_colors()` — orange (led off) → gray → navy (batted late). The "Who calls on whom?" wheel uses `member_colors`, a brand-derived palette ordered so wheel neighbors stay distinguishable (incl. colorblind); keep person colors from the brand families rather than adding unrelated hues.
- **Assets in `www/`:** `ja-logo.svg` (current JA logo), `standapp.svg` (pixel wordmark), `num_holder_1/2.svg` (baseball circles behind the scoreboard numbers), `baseball-pattern.svg` (faint step-and-repeat behind the input area and baseball cards), `favicon.svg`.

## Notes

- `figures/` (gitignored) holds generated chart exports; do not commit these.
- `JA-remote-meetings.Rmd`, `wrapped-stats.R` and `blog_figures.R` are standalone analysis scripts, not part of the running app.
