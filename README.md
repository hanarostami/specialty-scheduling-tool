# Specialty Scheduling / Capacity Planning Tool

An R Shiny app for planning exam-room and staffing needs for a multi-specialty medical practice. Given a set of specialties with their physician FTE counts, clinic-day cadence, room requirements, and Advanced Practice Provider (APP) staffing ratios, the app expands those inputs into individual providers, builds a week-long room schedule, and visualizes room utilization.

## What it does

1. You enter (or load a preset for) each specialty: physician FTE, clinic days per FTE, exam rooms needed per clinic day, and the equivalent APP staffing ratio/cadence.
2. The app expands each specialty's aggregate FTE into individual physicians and their supporting APPs.
3. It assigns each provider to specific rooms and half-day slots across a Mon–Fri week.
4. It renders an interactive room-by-slot grid (color-coded by specialty and role) and a plain-language utilization summary ("4 rooms used for 5 days; 2 rooms used for 2.5 days...").

## Methodology

### 1. Capacity expansion (`expand_capacity`)

Each specialty row is broken into discrete providers:

- **Physicians**: `physician_fte` is split into `floor(fte)` full-time physicians plus one partial-FTE physician for the remainder (e.g., 2.5 FTE → two 1.0-FTE physicians + one 0.5-FTE physician). Each physician needs `clinic_days_per_fte * fte * 2` half-day slots and `rooms_per_clinic_day` contiguous rooms per clinic day.
- **APPs**: for every physician, the number of supporting APP half-days is `app_ratio * physician_fte * app_days_per_fte`, rounded to the nearest half-day. Each half-day becomes its own APP "unit" to schedule.
- **Mirror vs. Independent** (`type`): a *Mirror* specialty's APPs are meant to work alongside "their" physician (e.g., an APP seeing overflow patients in the room next to the surgeon they support). An *Independent* specialty's APPs run their own clinic and aren't tied to a specific physician's schedule. This tag (`mirrors`) drives the placement logic below.

### 2. Room/slot assignment (`schedule_all`)

The week is modeled as a `rooms x 10` grid (5 days x AM/PM). Rooms are a shared, capped resource (`MAX_ROOMS`, currently 100).

- **Physicians** are placed first, greedily: for each physician, the algorithm scans room blocks (sized to `rooms_per_clinic_day`) and days in order, claiming the first available half-day slots until the physician's required half-days are filled.
- **Independent-type APPs** are placed next into any single free room block that has enough open half-day slots.
- **Mirror-type APPs** are placed via `assign_mirrored_apps_optimally`, which looks up the room row(s) used by their physician and tries to co-locate the APP in a nearby room (within 5 rows) during one of the physician's already-assigned slots, adding new rows if needed.

This is a **greedy, deterministic heuristic**, not a constraint solver — it optimizes for "does everyone fit," processed in input order, not for a globally optimal room layout. Given the same inputs it always produces the same schedule.

### 3. Handling infeasible demand

If total demand exceeds what fits in `MAX_ROOMS` rooms (or a mirrored APP can't be placed near its physician), that provider is **not silently dropped**. `schedule_all` returns both the schedule and an `unmet` list of exactly who couldn't be fully placed and why; the app surfaces this as a warning notification and appends a flag to the written summary. Treat any `unmet` result as a signal that the specialty mix needs more room capacity than modeled, not as a valid final plan.

### 4. Utilization summary (`summarize_utilization`)

Counts, per room, how many clinic-days (half-days / 2) it's occupied across the week, then groups rooms by that utilization level into a plain-English summary.

### Known limitations

- Single week, no seasonality, PTO, holidays, or multi-week rotation patterns.
- Half-day is the smallest unit of granularity.
- Room capacity is homogeneous — the model doesn't distinguish room types/equipment (e.g., procedure rooms vs. exam rooms).
- The scheduler is a first-fit heuristic; it will not re-shuffle earlier assignments to make room for a later, harder-to-place provider.

## Running locally

Open `app.r` in RStudio and click **Run App**, or from an R console:

```r
shiny::runApp("app.r")
```

Required packages: `shiny`, `shinyjs`, `jsonlite`, `dplyr`, `colorspace`, `tidyr`, `plotly`, `ggplot2`, `tibble`, `RColorBrewer`.

## Tests

Core scheduling logic (`expand_capacity`, `schedule_all`, `summarize_schedule`) has `testthat` coverage in `tests/testthat/`, run from the project root:

```r
testthat::test_dir("tests/testthat")
```

## Deployment

Currently deployed to shinyapps.io (see `rsconnect/`, gitignored — it's environment-specific and regenerates automatically). To redeploy after changes:

```r
rsconnect::deployApp()
```

## Before publishing to GitHub

- [ ] **Confidentiality/IP check.** This tool was built for a client engagement. Before making the repo public, confirm with your firm/engagement lead that the code and methodology are clear to open-source, and that no client-identifying details, real facility data, or proprietary assumptions are embedded anywhere (the current specialty presets in `app.r` are generic specialty names/ratios, not client-specific figures — but double check before pushing).
- [ ] **Unrelated files.** `data/` (`languages.csv`, `languages_raw.csv`, `languages.R`) and `misc/` (`almostVF.R`, `edits.R`, `jul18.R`) are leftovers from other work, not used by `app.r`. Decide whether to delete them or leave them out of the initial commit — no changes were made to these per your request.
- [ ] **License.** No `LICENSE` file exists yet. Add one (e.g., MIT) once you've confirmed you're allowed to open-source this — GitHub's repo-creation flow can generate one for you.
- [ ] **`.gitignore`.** Added — excludes `.Rhistory`, `.Rproj.user/`, `rsconnect/`, and `.DS_Store`.
- [ ] **Secrets.** No API keys or credentials are referenced in the code; `rsconnect/` (gitignored) is the only place account-identifying info lives.
