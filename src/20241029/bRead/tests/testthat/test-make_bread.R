test_that("Correct amount of bread is returned", {
  bread_1 <- make_bread(grains = 1, yeast = 1, water = 1.5, salt = 3)
  bread_2 <- 1 + 1 + 1.5 + 3
  expect_equal(bread_1, bread_2)
})
