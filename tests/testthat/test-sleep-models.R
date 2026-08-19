test_that("acti_sleep_asleep validates input and harmonizes predictions", {
  data <- data.frame(
    time = as.POSIXct("2020-01-01", tz = "UTC") + 0:2,
    X = c(0, 0.1, 0.2), Y = c(0, 0.1, 0.2), Z = c(1, 1, 1)
  )
  testthat::local_mocked_bindings(
    asleep = function(file, ...) {
      expect_named(file, c("time", "X", "Y", "Z"))
      list(predictions = data.frame(
        time = c("2020-01-01 00:00:00", "2020-01-01 00:00:30"),
        sleep_wake = c("wake", "sleep"), sleep_stage = c("wake", "N2")
      ))
    },
    .package = "asleep"
  )

  out <- acti_sleep_asleep(data, verbose = FALSE)
  expect_s3_class(out, "acti_sleep_estimate")
  expect_named(out, c("time", "sleep", "sleep_probability", "sleep_stage", "nonwear", "method"))
  expect_equal(out$sleep, c(FALSE, TRUE))
  expect_true(all(is.na(out$sleep_probability)))
  expect_equal(out$method, rep("asleep", 2))
})

test_that("acti_sleep_sleeper supplies required input and harmonizes predictions", {
  data <- data.frame(
    time = as.POSIXct("2020-01-01", tz = "UTC") + 0:2,
    X = c(0, 0.1, 0.2), Y = c(0, 0.1, 0.2), Z = c(1, 1, 1)
  )
  model_dir <- tempfile()
  dir.create(model_dir)
  testthat::local_mocked_bindings(
    estimate_sleep = function(data, epoch, model_dir) {
      expect_named(data, c("timestamp", "x", "y", "z"))
      expect_type(data$timestamp, "double")
      expect_equal(epoch, 30L)
      data.frame(
        time = c(1577836800, 1577836830, 1577836860),
        classification = c("Wake", "Sleep", "Nonwear")
      )
    },
    .package = "sleeper"
  )

  out <- acti_sleep_sleeper(data, model_dir = model_dir)
  expect_s3_class(out, "acti_sleep_estimate")
  expect_equal(out$sleep, c(FALSE, TRUE, FALSE))
  expect_equal(out$nonwear, c(FALSE, FALSE, TRUE))
  expect_true(all(is.na(out$sleep_stage)))
  expect_equal(out$method, rep("sleeper", 3))
})

test_that("model wrappers reject incomplete or unordered acceleration data", {
  data <- data.frame(
    time = as.POSIXct("2020-01-01", tz = "UTC") + c(1, 0),
    X = c(0, NA_real_), Y = c(0, 0), Z = c(1, 1)
  )
  expect_error(acti_sleep_asleep(data, verbose = FALSE), "complete")
  expect_error(acti_sleep_sleeper(data, model_dir = tempdir()), "complete")
})
