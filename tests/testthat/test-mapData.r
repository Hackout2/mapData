
context("Test to make sure lat checks and long checks are ok")

test_that("lat and long checks work correctly", {
    expect_false(test_lat(-1900))
    expect_true(test_lat(-89))
    expect_false(test_long(-1800))
    expect_true(test_long(-89))
})
