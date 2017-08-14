# Recipe Finder UI Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(shinydashboard)
library(magrittr)
load("recipes.Rdata")

# options
clean <- function(x) x %>% sort %>% unique
meat_options <- c("pork", "chicken", "beef", "crab", "shrimp", "fish", "eggs", "tofu") %>% clean
veggie_options <- c("green beans", "carrot", "asparagus", "spinach", "mushrooms", "garlic",
                    "dill", "shallots", "green onions", "cucumber", "lettuce", "mint", "thai basil",
                    "yellow onion", "cilantro", "cabbage", "mustard greens", "taro", "yam",
                    "rice paddy herbs", "elephant ear stem", "bean sprouts", "sweet potato", 
                    "water chestnuts", "bok choy") %>%
  clean
fruit_options <- c("tomato", "pineapple", "avocado") %>% clean
food_box <- function(type, option){
  column(width = 4,
         box(title = paste("Choose", type), width = NULL,
             solidHeader = TRUE, status = "primary", collapsible = TRUE,
             option)
         )
}

# UI functions
dashboardPage(

  # app title
  dashboardHeader(title = "Recipe Finder"),

  # disable sidebar
  dashboardSidebar(disable = TRUE),

  dashboardBody(verticalLayout(

    # inputs: ingredient options
    fixedRow(

      food_box("Meat", checkboxGroupInput("choose_meat", "", meat_options)),
      food_box("Veggies", checkboxGroupInput("choose_veggie", "", veggie_options)),
      food_box("Fruit", checkboxGroupInput("choose_fruit", "", fruit_options))

    ),

    # outputs: links to matched recipes
    box(title = "Recipes", width = NULL, solidHeader = TRUE, status = "primary",
      lapply(1:length(recipes), function(i) {
        uiOutput(paste0("match",i))
      })
    )

  ))
)

