context("Utilities")

test_that("STRING ids are trimmed properly", {
  test_string <- c("9606.ENSP00000360761", "9606.ENSP00000380184", "9606.ENSP00000264409",
                   "9606.ENSP00000317300", "9606.ENSP00000291572")
  expected_string <- c("ENSP00000360761", "ENSP00000380184", "ENSP00000264409", "ENSP00000317300",
                       "ENSP00000291572")

  expect_equal(expected_string, strip_species(test_string))
})
