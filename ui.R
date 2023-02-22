# Recipe Finder UI Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(shinydashboard)
library(shinyWidgets)

# UI functions
function(request){
dashboardPage(skin = "black",

  # app title
  dashboardHeader(
    title = "Recipe Finder",
    dropdownMenu(
      type = "notifications", 
      icon = paste("Updated ", as.Date(file.info("data/recipes.db")$mtime)),
      headerText = "",
      badgeStatus = NULL
    )
  ),

  # sidebar
  dashboardSidebar(
    collapsed = TRUE, 
    sidebarMenu(
      radioButtons("match_algorithm", "Choosen matching algorithm", c('or', 'and')),
      menuItem("Recipes", href = "https://nhuyhoa-recipes.netlify.app/", icon = icon("book")), 
      menuItem("Github Source Code", href = "https://github.com/jennguyen1/recipe_finder", icon = icon("github"))
    )
  ),

  # app main body
  dashboardBody(
    # CSS
    tags$head(includeScript("www/google-analytics.js")),
    # includeCSS("www/bootstrap.css"), 
    includeCSS("www/custom.css"),

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
                 ), 
                 bookmarkButton()
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
    ),
    span(p("Copyright (c) 2018 Jennifer N Nguyen under the MIT License"), style = "font-size:12px; color:grey")
  )
)}

