# Recipe Finder UI Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(shinydashboard)
library(magrittr)
load("recipes.Rdata")

# UI functions
dashboardPage(

  # app title
  dashboardHeader(title = "Recipe Finder"),

  # disable sidebar
  dashboardSidebar(disable = TRUE),

  # app main body
  dashboardBody(verticalLayout(

    # inputs: choose ingredients to use
    tabBox(width = NULL, selected = "Meals",

      tabPanel("Meals",
        uiOutput("input_selection_options")
      )

    ),

    # outputs: links to matched recipes
    box(title = "Recipes", width = NULL, solidHeader = TRUE, status = "primary",
        uiOutput("randomize"),
        uiOutput("recipe_options")
    )

  ))
)

