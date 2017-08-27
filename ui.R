# Recipe Finder UI Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(shinydashboard)
library(magrittr)
load("recipes.Rdata")

# UI functions
dashboardPage(skin = "black",

  # app title
  dashboardHeader(title = "Recipe Finder"),

  # disable sidebar
  dashboardSidebar(disable = TRUE),

  # app main body
  dashboardBody(
    # CSS
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    # app
    verticalLayout(
      tabBox(width = NULL, selected = "Meals",

        # meal options
        tabPanel("Meals",
                 # inputs: choose ingredients to use
                 uiOutput("input_selection_options"),

                 # outputs: links to matched recipes
                 box(title = "Recipes", width = NULL,
                     solidHeader = TRUE, status = "primary", class = "light-blue",
                     uiOutput("randomize"),
                     br(),
                     uiOutput("recipe_options")
                 )
        ),

        # dessert options
        tabPanel("Desserts",
                 box(title = "Choose Dessert", width = NULL,
                   solidHeader = TRUE, status = "info", class = "teal",
                   uiOutput("randomize_dessert"),
                   br(),
                   uiOutput("desserts")
                 )
        )

      )
    )
  )
)

