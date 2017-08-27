# Recipe Finder Server Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(stringr)
library(purrr)

#-------------------------------------------------

# load list 'recipes'
load("recipes.Rdata")

# dessert
dessert_recipes <- keep(recipes, transpose(recipes)$meal == "dessert")
dessert_names <- names(dessert_recipes)

# meals
meal_recipes <- discard(recipes, transpose(recipes)$meal == "other")
recipe_names <- names(meal_recipes)
r <- transpose(transpose(meal_recipes)$ingredients)


#-------------------------------------------------
# FUNCTIONS FOR UI INPUTS ------------------------

# function to clean up food ingredients
clean <- function(x) x %>% unlist %>% sort %>% unique %>% discard(~ .x %in% c("", "any"))

# obtain food options
meat_options <- c("pork", "chicken", "beef", "crab", "shrimp", "fish", "eggs", "tofu") %>% clean
veggie_options <- r$Veggies %>% clean
fruit_options <- r$Fruit %>% clean


#-------------------------------------------------
# FUNCTIONS FOR UI OUTPUTS -----------------------

# recipe address
make_address <- function(name){
  address <- str_replace_all(name, " ", "-") %>%
    paste0("http://jnguyen92.github.io/nhuyhoa//2017/05/", ., ".html")
  return(address)
}

# recipe picture address
make_pic_address <- function(name){
  fixed_name <- str_replace(name, "\\s+\\(.*", "")
  address <- paste0("http://jnguyen92.github.io/nhuyhoa/figure/food/", fixed_name, ".JPG")
  return(address)
}

# output match link for list
output_match_link <- function(name){
  # if the match exists - export a picture & link of recipe
  if(!is.na(name)){
    column(width = 12,
           a(href = make_address(name), img(src = make_pic_address(name), width = 200, height = 150), target = "_blank"),
           h5(a(href = make_address(name), name, target = "_blank", class = "match_recipe")),
           h1()
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
    chosen_options <- list(
      meat = input$choose_meat,
      veggies = input$choose_veggies,
      fruit = input$choose_fruit
    )

    # finds matching dish for chosen options types and all 'any' dishes
    # prints out unique options in alphabetical order
    matches <- map(names(chosen_options), function(type){

      # finds any matches, returns nothing if none requested
      options <- chosen_options[[type]]
      if(length(options) == 0) return(character(0))
      map(options, function(x){
        matches <- recipe_names[str_detect(r[[str_to_title(type)]], x)]
        any <- recipe_names[str_detect(r[[str_to_title(type)]], "any")]
        return( c(matches, any) )
      })

    }) %>% unlist %>% unique %>% sort

    return(matches)

  })

  # creating output objects - create links to recipe site
  # loop over recipes and assign output objects within there
  lapply(1:length(recipes), function(i) {
    output[[paste0("match", i)]] <- renderUI({
      output_match_link(match_list()[i])
    })
  })

  # generates ui output for matching options
  output$recipe_options <- renderUI({
    lapply(1:length(recipes), function(i) {
      uiOutput(paste0("match",i))
    })
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

    pic <- make_pic_address(dessert_names[i])
    link <- make_address(dessert_names[i])

    output[[paste0("dessert", i)]] <- renderUI({
      a(href = link, img(src = pic, width = 200, height = 150), target = "_blank")
    })

  })

  # generates ui output for all desserts
  output$desserts <- renderUI({
    lapply(1:length(dessert_names), function(i){
      uiOutput(paste0("dessert", i))
    })
  })


})
