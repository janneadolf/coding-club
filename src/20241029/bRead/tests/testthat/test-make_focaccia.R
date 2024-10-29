test_that("Correct amount of focaccia is returned", {
  bread_1 <- make_focaccia(grains = 1, yeast = 1, water = 1.5, salt = 3)
  bread_2 <- 1 + 1.5*1 + 0.7*1.5 + 2*3
  expect_equal(bread_1, bread_2)
})
test_that("Error is returned in case of character input", {
  expect_error(
    make_focaccia(grains = 1, yeast = 1, water = 1.5, salt = "hello"),
    "non-numeric argument to binary operator"
    )
})
