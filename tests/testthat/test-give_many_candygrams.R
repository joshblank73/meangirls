test_that("give_many_candygrams works", {
  correct_result <- c("One for Brendad. Ok", "None for Gretchen Weiners.",
                      "Six for Josh. You go, Josh!")

  my_result <- give_many_candygrams(c('Brendad', 'Gretchen', 'Josh'), c(1, 2, 6), c('Ok', NA, NA))

  expect_equal(my_result, correct_result)
})
