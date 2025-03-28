test_that("filter_if_truthy() returns the original dataset if non-truthy condition given", {
  skip_if_not_installed("datasets")
  expect_identical(filter_if_truthy(datasets::iris, .data$Species, ""), iris)
  expect_identical(filter_if_truthy(datasets::iris, .data$Species, NULL), iris)
  expect_identical(filter_if_truthy(datasets::iris, .data$Species, FALSE), iris)
})

test_that("filter_if_truthy() filters if it gets a truthy value.", {
  skip_if_not_installed("datasets")
  expect_identical(filter_if_truthy(datasets::iris, .data$Species, "setosa"),
                   iris[iris$Species == "setosa", ])
  expect_equal(filter_if_truthy(datasets::iris, .data$Species, c("setosa", "virginica")),
               iris[iris$Species %in% c("setosa", "virginica"), ],
               ignore_attr = TRUE)
})

test_that("filter_if_truthy() allows use of data masking methods", {
  skip_if_not_installed("datasets")
  expect_identical(filter_if_truthy(datasets::iris, Species, "setosa"),
                   iris[iris$Species == "setosa", ])
})
