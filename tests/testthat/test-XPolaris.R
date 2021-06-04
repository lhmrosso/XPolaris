data('exkansas')
plt <- xplot(exkansas)
test_that('no error in generating the plot', {
  expect_false(inherits(plt, "try-error"))
})
