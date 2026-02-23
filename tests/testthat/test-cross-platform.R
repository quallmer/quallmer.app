test_that("safe_character handles NULL", {

  expect_null(safe_character(NULL))
})

test_that("safe_character handles character vector", {
  expect_equal(safe_character(c("a", "b")), c("a", "b"))
})

test_that("safe_character handles list input", {
  # This is the bug scenario: list instead of vector from state restoration
  input_list <- list("col1", "col2")
  result <- safe_character(input_list)

  expect_type(result, "character")
  expect_equal(result, c("col1", "col2"))
})

test_that("safe_character handles nested list", {
  input_list <- list(list("a"), "b")
  result <- safe_character(input_list)

  expect_type(result, "character")
  expect_equal(result, c("a", "b"))
})

test_that("intersect with safe_character returns character vector", {
  # Simulate corrupted state where coder_cols is a list
  saved_cols <- list("coder1", "coder2")
  available_cols <- c("coder1", "coder2", "coder3")

  # Without fix: intersect(list, vector) returns list
  bad_result <- intersect(saved_cols, available_cols)
  expect_type(bad_result, "list")  # This is the bug


  # With fix: safe_character ensures character vector
  good_result <- safe_character(intersect(safe_character(saved_cols), available_cols))
  expect_type(good_result, "character")
  expect_equal(good_result, c("coder1", "coder2"))
})

test_that("column subscripting works after safe_character coercion", {
  df <- data.frame(
    unit_id = 1:3,
    coder1 = c("A", "B", "A"),
    coder2 = c("A", "B", "B")
  )

  # Simulate list input (the bug scenario)
  unit_id_list <- list("unit_id")

  # This would fail: df[[unit_id_list]]
  expect_error(df[[unit_id_list]], "invalid subscript type")

  # After safe_character coercion, it works
  unit_id_safe <- safe_character(unit_id_list)
  if (length(unit_id_safe) > 0) unit_id_safe <- unit_id_safe[1]

  expect_no_error(df[[unit_id_safe]])
  expect_equal(df[[unit_id_safe]], 1:3)
})

test_that("prepare_comparison_data works with safe_character coerced inputs", {
  df <- data.frame(
    id = 1:5,
    coder1 = c("A", "B", "A", "B", "A"),
    coder2 = c("A", "A", "B", "B", "A"),
    stringsAsFactors = FALSE
  )

  # Simulate list inputs from corrupted state
  unit_id_list <- list("id")
  coder_cols_list <- list("coder1", "coder2")

  # Coerce to character vectors
  unit_id <- safe_character(unit_id_list)
  if (length(unit_id) > 0) unit_id <- unit_id[1]
  coder_cols <- safe_character(coder_cols_list)

  # Should work without error
  result <- prepare_comparison_data(df, unit_id, coder_cols)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("coder1", "coder2"))
})
