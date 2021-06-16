data('exkansas')

plt <- xplot(exkansas)

img <- ximages(locations = exkansas,
               variables = c('clay'),
               statistics = c('mean'),
               layersdepths = c('0_5'))

soi <- xsoil(img)

test_that('no error plotting', {
  expect_false(inherits(plt, "try-error"))
})

test_that('no error downloading images', {
  expect_false(inherits(img, "try-error"))
})

test_that('no error extracting soil', {
  expect_false(inherits(soi, "try-error"))
})