
test_that("The size parameter error works",{
  expect_error(cloud_setup(size = 3))
  expect_error(cloud_setup(size = NA))
  expect_error(cloud_setup(size = "big"))
})

test_that("The path parameter errors work",{
  expect_error(deploy_shiny())
  expect_error(deploy_shiny(droplet = 1))
})


test_that("The droplet parameter errors work",{
  expect_error(deploy_shiny(path = "./xai2shiny"))
  expect_error(deploy_shiny(droplet = "name", path = "./xai2shiny"))
})
