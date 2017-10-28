# Recipe Finder Server Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(stringr)
library(dplyr)
library(purrr)
library(RSQLite)

#-------------------------------------------------

# load recipes
connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "data/recipes.db")
recipes <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM recipes")
ingredients <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM ingredients")
RSQLite::dbDisconnect(conn = connect)

recipe_info <- merge(recipes, ingredients, by = "recipe")

dessert_recipes <- subset(recipe_info, meal_type == "dessert")
meal_recipes <- subset(recipe_info, meal_type != "other")

recipe_names <- unique(recipe_info$recipe)
dessert_names <- unique(dessert_recipes$recipe)

#-------------------------------------------------
# FUNCTIONS FOR UI INPUTS ------------------------

# function to clean up food ingredients
clean <- function(food_type) ingredients %>% subset(type == food_type) %>% pull(ingredients) %>% sort() %>% unique() %>% discard(~ .x %in% c("any"))

# obtain food options
meat_options <- c("pork", "chicken", "beef", "crab", "shrimp", "fish", "eggs", "tofu", "lobster", "cha") %>% unique() %>% sort()
veggie_options <- clean("veggie")
fruit_options <- clean("fruit")


#-------------------------------------------------
# FUNCTIONS FOR UI OUTPUTS -----------------------

# recipe address
make_address <- function(name){
  address <- str_replace_all(name, " ", "-") %>%
    paste0("http://jennguyen1.github.io/nhuyhoa/2017/05/Recipe-", ., ".html")
  return(address)
}

# recipe picture address
make_pic_address <- function(name){
  fixed_name <- str_replace(name, "\\s+\\(.*", "") %>% str_replace_all(" ", "_")
  address <- paste0("http://jennguyen1.github.io/nhuyhoa/figure/food/thumbnail/", fixed_name, ".JPG")
  return(address)
}

# output match link for list
output_match_link <- function(name, w){
  # if the match exists - export a picture & link of recipe
  if(!is.na(name)){
    column(width = w,
           a(href = make_address(name), img(src = make_pic_address(name), width = 200, height = 150), target = "_blank"),
           h5(a(href = make_address(name), name, target = "_blank")),
           br()
    )
  }
}


#-------------------------------------------------


shinyServer(function(input, output) {


  #################
  # RECIPE FINDER #
  #################

  # function to format the box per each food group with an 'all' function
  food_box <- function(type, option){

    # whether to check all: no if all button not available or not previously checked
    t <- paste0("all_", type)
    if( is.null(input[[t]]) ){
      check_all <- FALSE
    } else if( !input[[t]] ){
      check_all <- FALSE
    } else{
      check_all <- TRUE
    }

    # whether to select all group input buttons based on check all
    if(check_all){
      selected <- option
    } else{
      selected <- NULL
    }

    status <- switch(type,
                     meat = "danger",
                     veggies = "success",
                     fruit = "warning")

    bg <- switch(type,
                 meat = "red",
                 veggies = "green",
                 fruit = "yellow")

    # make box
    column(width = 4,
           box(title = paste("Choose", str_to_title(type)), width = NULL, collapsible = TRUE,
               solidHeader = TRUE, status = status, class = bg,
               checkboxInput(t, "all", check_all),
               checkboxGroupInput(paste0("choose_", type), "", option, selected))
    )
  }

  # UI Inputs rendering
  output$input_selection_options <- renderUI({
    fixedRow(
      food_box("meat", meat_options),
      food_box("veggies", veggie_options),
      food_box("fruit", fruit_options)
    )
  })

  # process chosen options from user - generate a list of matches
  match_list <- reactive({

    match_algorithm <- 'or' # TODO make an option for this

    chosen_options <- list(
      meat = input$choose_meat,
      veggie = input$choose_veggies,
      fruit = input$choose_fruit
    )

    # finds matching dish for chosen options types and all 'any' dishes
    if(match_algorithm == 'or'){

      #' matching ingredients based on or
      matches <- map2_df(names(chosen_options), chosen_options, function(name, options){
        if( length(options) == 0 ) return(data.frame(recipe = character(0)))

        type_match <- subset(ingredients, type == name & str_detect(ingredients, options))
        any <- subset(ingredients, type == name & ingredients == "any")
        bind_rows(any, type_match) %>% dplyr::select(recipe)
      }) %>% distinct() %>% arrange(recipe) %>% pull(recipe)

    } else{

      # matching ingredients based on and
      matches <- map2(names(chosen_options), chosen_options, function(name, options){
        if( length(options) == 0 ) return(data.frame(recipe = character(0)))

        type_match <- map(options, ~ subset(ingredients, type == name & ingredients == .x)) %>%
          reduce(~ merge(.x, .y, 'recipe'))
        any <- subset(ingredients, type == name & ingredients == "any")
        merge(any, type_match, 'recipe') %>% dplyr::select(recipe)
      }) %>% discard(~ nrow(.x) == 0) %>% reduce(~ merge(.x, .y, 'recipe')) %>% pull(recipe)

    }

    return(matches)
  })


  # creating output objects - create links to recipe site
  # loop over recipes and assign output objects within there
  observeEvent(match_list(), {
    if( length(match_list()) > 0 ){
      lapply(1:length(match_list()), function(i) {
        output[[paste0("match", i)]] <- renderUI({
          output_match_link(match_list()[i], 3)
        })
      })
    }
  })

  # generates ui output for matching options
  output$recipe_options <- renderUI({
    if( length(match_list()) > 0 ){
      lapply(1:length(match_list()), function(i) {
        uiOutput(paste0("match",i), class = "match_recipe")
      })
    }
  })


  #################
  # RANDOM PICKER #
  #################

  # link to random dish
  get_random <- function() sample(recipe_names, 1)
  random_dish <- reactiveValues(dish = get_random())

  # detect changes in match list
  observeEvent(match_list(), {
    random_dish$dish <- ifelse( length(match_list()) > 0, sample(match_list(), 1), get_random() )
  })

  # detect if the button was clicked before
  observeEvent(input$random, {
    random_dish$dish <- ifelse( length(match_list()) > 0, sample(match_list(), 1), get_random() )
  })

  # create the action when click on the randomizer
  output$randomize <- renderUI({

    # makes address
    address <- make_address(random_dish$dish)

    # generates link cmd
    link_cmd <- paste0("window.open('", address, "', '_blank')")

    # makes the action button to open in new page
    actionButton("random", "I'm Feeling Lucky", onclick = link_cmd)

  })


  ##################
  # RANDOM DESSERT #
  ##################

  # link to random dish
  get_random_dessert <- function() sample(dessert_names, 1)
  random_dessert <- reactiveValues(dish = get_random_dessert())

  # detect if the button was clicked before
  observeEvent(input$random_dessert, {
    random_dessert$dish <- get_random_dessert()
  })

  # create the action when click on the randomizer
  output$randomize_dessert <- renderUI({

    # makes address
    address <- make_address(random_dessert$dish)

    # generates link cmd
    link_cmd <- paste0("window.open('", address, "', '_blank')")

    # makes the action button to open in new page
    actionButton("random_dessert", "I'm Feeling Lucky", onclick = link_cmd)

  })

  # creating output objects - dessert pic which links to recipe site
  lapply(1:length(dessert_names), function(i){
    output[[paste0("dessert", i)]] <- renderUI({
      output_match_link(dessert_names[i], 3)
    })
  })

  # generates ui output for all desserts
  output$desserts <- renderUI({
    lapply(1:length(dessert_names), function(i){
      uiOutput(paste0("dessert", i), class = "match_recipe")
    })
  })

})
