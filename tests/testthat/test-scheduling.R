# Sources app.r to test the core scheduling functions (expand_capacity,
# schedule_all, summarize_schedule) without launching the Shiny server.
# shinyApp() just builds an app object here; it doesn't block or serve.
source(file.path("..", "..", "app.r"), local = TRUE)

test_that("expand_capacity expands FTE into per-physician rows", {
  df_inputs <- data.frame(
    specialty = "Test Spec",
    type = 2,
    physician_fte = 2.5,
    clinic_days_per_fte = 2,
    rooms_per_clinic_day = 2,
    app_ratio = 0.5,
    app_days_per_fte = 2,
    app_rooms_per_clinic_day = 1,
    stringsAsFactors = FALSE
  )

  expanded <- expand_capacity(df_inputs)

  # 2.5 FTE -> 2 full-time physicians + 1 half-time physician
  expect_equal(nrow(expanded$md_df), 3)
  expect_equal(expanded$md_df$fte, c(1, 1, 0.5))
})

test_that("expand_capacity tags mirrored APPs to their physician, independent APPs get no mirror", {
  df_inputs <- data.frame(
    specialty = c("Mirrored Spec", "Independent Spec"),
    type = c(1, 2),
    physician_fte = c(1, 1),
    clinic_days_per_fte = c(2, 2),
    rooms_per_clinic_day = c(2, 2),
    app_ratio = c(1, 1),
    app_days_per_fte = c(2, 2),
    app_rooms_per_clinic_day = c(1, 1),
    stringsAsFactors = FALSE
  )

  expanded <- expand_capacity(df_inputs)
  mirrored_apps <- expanded$app_df[expanded$app_df$specialty == "Mirrored Spec", ]
  independent_apps <- expanded$app_df[expanded$app_df$specialty == "Independent Spec", ]

  expect_true(all(!is.na(mirrored_apps$mirrors)))
  expect_true(all(is.na(independent_apps$mirrors)))
})

test_that("schedule_all handles mirrored (Type = 1) specialties without error", {
  # Regression test: this path previously hit a `colnamesa` typo and crashed
  # every time a Mirror-type specialty's APPs were scheduled.
  df_inputs <- data.frame(
    specialty = "ARJR",
    type = 1,
    physician_fte = 1.5,
    clinic_days_per_fte = 2.5,
    rooms_per_clinic_day = 4,
    app_ratio = 1.5,
    app_days_per_fte = 2.5,
    app_rooms_per_clinic_day = 1,
    stringsAsFactors = FALSE
  )

  expanded <- expand_capacity(df_inputs)
  result <- schedule_all(expanded$md_df, expanded$app_df)

  expect_true(is.data.frame(result$schedule))
  expect_true(nrow(result$schedule) > 0)
  expect_equal(length(result$unmet), 0)
})

test_that("schedule_all reports unmet demand instead of silently dropping providers", {
  # Demand that cannot possibly fit within MAX_ROOMS rooms.
  df_inputs <- data.frame(
    specialty = "Overload",
    type = 2,
    physician_fte = 40,
    clinic_days_per_fte = 5,
    rooms_per_clinic_day = 5,
    app_ratio = 0.5,
    app_days_per_fte = 5,
    app_rooms_per_clinic_day = 1,
    stringsAsFactors = FALSE
  )

  expanded <- expand_capacity(df_inputs)
  result <- schedule_all(expanded$md_df, expanded$app_df)

  expect_gt(length(result$unmet), 0)
})

test_that("summarize_schedule returns a readable non-empty summary", {
  df_inputs <- data.frame(
    specialty = "Test Spec",
    type = 2,
    physician_fte = 1,
    clinic_days_per_fte = 2,
    rooms_per_clinic_day = 2,
    app_ratio = 0.5,
    app_days_per_fte = 2,
    app_rooms_per_clinic_day = 1,
    stringsAsFactors = FALSE
  )

  expanded <- expand_capacity(df_inputs)
  result <- schedule_all(expanded$md_df, expanded$app_df)
  summary_text <- summarize_schedule(result$schedule)

  expect_type(summary_text, "character")
  expect_true(nchar(summary_text) > 0)
})
