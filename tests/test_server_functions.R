context("Server Functions")

library(testthat)
source("server.R")

test_that("Making a link to the recipe", {
  name1 <- "Thit Kho (Braised and Carmelized Pork Belly with Eggs)"
  address1 <- "http://jennguyen1.github.io/nhuyhoa/recipes/Thit-Kho-(Braised-and-Carmelized-Pork-Belly-with-Eggs).html"
  link1 <- "window.open('http://jennguyen1.github.io/nhuyhoa/recipes/Thit-Kho-(Braised-and-Carmelized-Pork-Belly-with-Eggs).html', '_blank')"
  name2 <- "Rainbow Cupcakes"
  address2 <- "http://jennguyen1.github.io/nhuyhoa/recipes/Rainbow-Cupcakes.html"
  link2 <- "window.open('http://jennguyen1.github.io/nhuyhoa/recipes/Rainbow-Cupcakes.html', '_blank')"
  
  expect_equal(make_address(name1), address1)
  expect_equal(make_address(name2), address2)
  expect_equal(make_link(name1), link1)
  expect_equal(make_link(name2), link2)
})

test_that("Making a link to the picture", {
  name1 <- "Thit Kho (Braised and Carmelized Pork Belly with Eggs)"
  address1 <- "http://jennguyen1.github.io/nhuyhoa/figure/food/thumbnail/Thit_Kho.JPG"
  name2 <- "Rainbow Cupcakes"
  address2 <- "http://jennguyen1.github.io/nhuyhoa/figure/food/thumbnail/nophoto.JPG"

  expect_equal(make_pic_address(name1), address1)
  expect_equal(make_pic_address(name2), address2)
})

test_that("Finding matches", {
  chosen_options <- list(
    meat = c("duck"),
    veggie = c("bamboo shoots"),
    fruit = NULL
  )
  or_result <- c("Bun Mang Vit (Duck Noodle Soup with Bamboo Shoot)", "Com Chien (Fried Rice)", "Mi Vit Quay (Roast Duck Noodle Soup)", "Pad Thai")
  and_result <- c("Bun Mang Vit (Duck Noodle Soup with Bamboo Shoot)", "Com Chien (Fried Rice)", "Pad Thai")
  
  expect_equal(or_match(chosen_options, meal_recipes), or_result)
  expect_equal(and_match(chosen_options, meal_recipes), and_result)
})
