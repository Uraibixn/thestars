# Checks on data/out/stars.json, the file the website reads.
# Run from the project folder: Rscript -e 'testthat::test_dir("tests")'
library(testthat)
library(jsonlite)

data  <- fromJSON(file.path("..", "data", "out", "stars.json"))  # test_dir runs from tests/
stars <- data$stars

test_that("every column the website needs is there", {
  expect_setequal(names(stars), c("id", "name", "designation", "constellation", "ra", "dec",
                                  "dist_ly", "mag", "ci", "spectral", "family"))
})

test_that("the filters removed the Sun, unknown distances and faint stars", {
  expect_false(0 %in% stars$id)
  expect_true(all(stars$dist_ly < 100000 * 3.26156))
  expect_true(all(stars$mag <= 8))
})

test_that("coordinates are in degrees", {
  expect_true(all(stars$ra >= 0 & stars$ra < 360))
  expect_true(all(stars$dec >= -90 & stars$dec <= 90))
})

test_that("reference stars match the report within 1%", {
  expected <- c(Sirius = 8.6, Vega = 25.0, Mizar = 85.8, Polaris = 432.6)
  for (star in names(expected)) {
    dist <- stars$dist_ly[stars$name == star]
    expect_equal(dist, expected[[star]], tolerance = 0.01, label = star)
  }
  # RA in hours instead of degrees would put Sirius at 6.75
  expect_equal(stars$ra[stars$name == "Sirius"], 101.29, tolerance = 0.001)
})

test_that("the Ursa Major family has exactly its 5 members", {
  members <- stars$name[!is.na(stars$family) & stars$family == "uma"]
  expect_setequal(members, c("Merak", "Phecda", "Megrez", "Alioth", "Mizar"))
  # Dubhe and Alkaid are in the Big Dipper but not in the family
  expect_false(any(c("Dubhe", "Alkaid") %in% members))
  expect_equal(data$families$uma$name, "Ursa Major Moving Group")
})
