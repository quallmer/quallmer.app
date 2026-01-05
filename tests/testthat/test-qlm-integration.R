test_that("prepare_comparison_data transforms wide format correctly", {
  # Create sample wide-format data
  df <- data.frame(
    id = 1:5,
    coder1 = c("A", "B", "A", "B", "A"),
    coder2 = c("A", "A", "B", "B", "A"),
    stringsAsFactors = FALSE
  )

  result <- prepare_comparison_data(df, "id", c("coder1", "coder2"))

  # Check structure
  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("coder1", "coder2"))

  # Check coder1 data
  expect_s3_class(result$coder1, "data.frame")
  expect_equal(nrow(result$coder1), 5)
  expect_equal(names(result$coder1), c(".id", "code"))
  expect_equal(result$coder1$.id, 1:5)
  expect_equal(result$coder1$code, c("A", "B", "A", "B", "A"))

  # Check coder2 data
  expect_s3_class(result$coder2, "data.frame")
  expect_equal(nrow(result$coder2), 5)
  expect_equal(result$coder2$code, c("A", "A", "B", "B", "A"))
})

test_that("prepare_comparison_data handles multiple coders", {
  df <- data.frame(
    id = 1:3,
    c1 = c("X", "Y", "Z"),
    c2 = c("X", "Y", "Y"),
    c3 = c("X", "X", "Z"),
    stringsAsFactors = FALSE
  )

  result <- prepare_comparison_data(df, "id", c("c1", "c2", "c3"))

  expect_length(result, 3)
  expect_named(result, c("c1", "c2", "c3"))

  # Verify each coder has correct structure
  for (coder in c("c1", "c2", "c3")) {
    expect_equal(nrow(result[[coder]]), 3)
    expect_equal(names(result[[coder]]), c(".id", "code"))
  }
})

test_that("prepare_comparison_data preserves data types", {
  df <- data.frame(
    id = letters[1:4],
    coder1 = 1:4,
    coder2 = 2:5,
    stringsAsFactors = FALSE
  )

  result <- prepare_comparison_data(df, "id", c("coder1", "coder2"))

  # Check that character IDs are preserved
  expect_type(result$coder1$.id, "character")
  expect_equal(result$coder1$.id, letters[1:4])

  # Check that numeric codes are preserved
  expect_type(result$coder1$code, "integer")
  expect_equal(result$coder1$code, 1:4)
  expect_equal(result$coder2$code, 2:5)
})

test_that("prepare_comparison_data handles missing values", {
  df <- data.frame(
    id = 1:4,
    coder1 = c("A", NA, "B", "A"),
    coder2 = c("A", "B", NA, "A"),
    stringsAsFactors = FALSE
  )

  result <- prepare_comparison_data(df, "id", c("coder1", "coder2"))

  # NA values should be preserved
  expect_true(is.na(result$coder1$code[2]))
  expect_true(is.na(result$coder2$code[3]))
  expect_equal(result$coder1$code[c(1, 3, 4)], c("A", "B", "A"))
})
