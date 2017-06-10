
# open libraries
library(shiny)
library(shinydashboard)
library(magrittr)
load("recipes.Rdata")

# options
meat_options <- c("pork", "chicken", "beef", "crab", "shrimp", "fish", "eggs", "tofu") %>% sort %>% unique
veggie_options <- c("green beans", "carrot", "asparagus", "spinach", "mushrooms", "garlic", "dill", "shallots", "green onions", "cucumber", "lettuce", "mint", "thai basil", "yellow onion", "cilantro", "cabbage", "mustard greens", "taro", "yam", "rice paddy herbs", "elephant ear stem", "bean sprouts", "sweet potato") %>% sort %>% unique
fruit_options <- c("tomato", "pineapple", "avocado", "mango", "strawberries") %>% sort %>% unique
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

    fixedRow(

      food_box("Meat", checkboxGroupInput("choose_meat", "", meat_options)),
      food_box("Veggies", checkboxGroupInput("choose_veggie", "", veggie_options)),
      food_box("Fruit", checkboxGroupInput("choose_fruit", "", fruit_options))

    ),

    # outputs: matched recipes
    box(title = "Recipes", width = NULL, solidHeader = TRUE, status = "primary",
      lapply(1:length(recipes), function(i) {
        uiOutput(paste0("match",i))
      })
    )

  ))
)

