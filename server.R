# Recipe Finder Server Code
# Date: July 2017
# Author: Jenny Nguyen
# Email: jennifernguyen1992@gmail.com

library(shiny)
library(stringr)
library(dplyr)
library(purrr)
library(RSQLite)
library(RCurl)

#-------------------------------------------------
# LOAD RECIPES -----------------------------------

connect <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = "data/recipes.db")
recipes <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM recipes")
ingredients <- RSQLite::dbGetQuery(conn = connect, statement = "SELECT * FROM ingredients")
RSQLite::dbDisconnect(conn = connect)

recipe_info <- merge(recipes, ingredients, by = "recipe")

dessert_recipes <- subset(recipe_info, meal_type == "dessert")
meal_recipes <- subset(recipe_info, meal_type != "other")

recipe_names <- unique(meal_recipes$recipe)
dessert_names <- unique(dessert_recipes$recipe)


#-------------------------------------------------
# FUNCTIONS FOR UI INPUTS ------------------------

# function to clean up food ingredients
clean <- function(food_type) meal_recipes %>% subset(type == food_type) %>% pull(ingredients) %>% sort() %>% unique() %>% discard(~ .x %in% c("any"))

# obtain food options
meat_options <- c("pork", "chicken", "beef", "crab", "shrimp", "fish", "eggs", "tofu", "lobster", "cha", "duck") %>% unique() %>% sort()
veggie_options <- clean("veggie")
fruit_options <- clean("fruit")

# function to format the box per each food group (color) 
food_box <- function(type, option){
  
  status <- switch(type,
                   meat = "danger",
                   veggie = "success",
                   fruit = "warning")
  
  bg <- switch(type,
               meat = "red",
               veggie = "green",
               fruit = "yellow")
  
  # make box
  column(width = 4,
         box(title = paste("Choose", str_to_title(type)), width = NULL, collapsible = TRUE,
             solidHeader = TRUE, status = status, class = bg,
             actionButton(stringr::str_interp("all_${type}"), "Select All", icon = icon("check")),
             checkboxGroupInput(stringr::str_interp("choose_${type}"), "", option))
  )
}


#-------------------------------------------------
# FUNCTIONS FOR UI OUTPUTS -----------------------

# recipe address
make_address <- function(name){
  address <- str_replace_all(name, " ", "-") %>%
    paste0("http://jennguyen1.github.io/nhuyhoa/recipes/", ., ".html")
  return(address)
}

# recipe picture address
make_pic_address <- function(name){
  fixed_name <- str_replace(name, "\\s+\\(.*", "") %>% str_replace_all(" ", "_")
  address <- paste0("http://jennguyen1.github.io/nhuyhoa/figure/food/thumbnail/", fixed_name, ".JPG")
  url <- ifelse(RCurl::url.exists(address), address, "http://jennguyen1.github.io/nhuyhoa/figure/food/thumbnail/nophoto.JPG")
  return(url)
}

# output match link for list
output_match_link <- function(name, w){
  # if the match exists - export a picture & link of recipe
  if(!is.na(name)){
    display_name <- str_replace(name, " \\(.*", "")
    column(width = w,
           a(href = make_address(name), img(src = make_pic_address(name), width = 200, height = 150), target = "_blank"),
           h5(a(href = make_address(name), display_name, target = "_blank")),
           br()
    )
  }
}

# open link in new page
make_link <- function(name){
  address <- make_address(name)
  link <- stringr::str_interp("window.open('${address}', '_blank')")
  return(link)
}

# or match algorithm
or_match <- function(chosen_options, meal_recipes){
  matches <- map2_df(names(chosen_options), chosen_options, function(name, options){
    
    if( length(options) == 0 ) return(data.frame(recipe = character(0)))
    
    type_match <- map(options, ~ subset(meal_recipes, type == name & str_detect(ingredients, .x))) %>% 
      bind_rows()
    any <- subset(meal_recipes, type == name & ingredients == "any")
    sub_matches <- bind_rows(any, type_match) %>% dplyr::select(recipe)
    
    return(sub_matches)
  }) %>% distinct() %>% arrange(recipe) %>% pull(recipe)
  
  return(matches)
}

# or match algorithm
and_match <- function(chosen_options, meal_recipes){
  matches <- map2(names(chosen_options), chosen_options, function(name, options){
    
    if( length(options) == 0 ) return(data.frame(recipe = character(0)))
    
    type_match <- map(options, ~ subset(meal_recipes, type == name & str_detect(ingredients, .x))) %>%
      reduce(~ merge(.x, .y, 'recipe'))
    any <- subset(meal_recipes, type == name & ingredients == "any")
    sub_matches <- dplyr::bind_rows(type_match, any) %>% dplyr::select(recipe) %>% distinct()
    
    return(sub_matches)
  }) %>% discard(~ nrow(.x) == 0) 
  
  if( length(matches) == 0 ){
    matches <- character(0)
  } else{
    matches <- reduce(matches, ~ merge(.x, .y, 'recipe'))$recipe
  }
  
  return(matches)
}


#-------------------------------------------------


shinyServer(function(input, output, session) {


  #################
  # RECIPE FINDER #
  #################

  setBookmarkExclude(c("all_meat", "all_veggie", "all_fruit", "random", "sidebarCollapsed", "sidebarItemExpanded"))
  
  # ui for ingredients, all button
  output$input_selection_options <- renderUI({
    fixedRow(
      food_box("meat", meat_options),
      food_box("veggie", veggie_options),
      food_box("fruit", fruit_options)
    )
  })
  observeEvent(input$all_meat, {
    if(input$all_meat == 0) return(NULL)
    else if(input$all_meat %% 2 == 1){
      updateCheckboxGroupInput(session, "choose_meat", choices = meat_options, selected = meat_options)
    } else{
      updateCheckboxGroupInput(session, "choose_meat", choices = meat_options)
    }
  })
  observeEvent(input$all_veggie, {
    if(input$all_veggie == 0) return(NULL)
    else if(input$all_veggie %% 2 == 1){
      updateCheckboxGroupInput(session, "choose_veggie", choices = veggie_options, selected = veggie_options)
    } else{
      updateCheckboxGroupInput(session, "choose_veggie", choices = veggie_options)
    }
  })
  observeEvent(input$all_fruit, {
    if(input$all_fruit == 0) return(NULL)
    else if(input$all_fruit %% 2 == 1){
      updateCheckboxGroupInput(session, "choose_fruit", choices = fruit_options, selected = fruit_options)
    } else{
      updateCheckboxGroupInput(session, "choose_fruit", choices = fruit_options)
    }
  })

  
  #################
  # MATCH CHOICES #
  #################

  # process chosen options from user - generate a list of matches
  match_list <- reactive({

    match_algorithm <- input$match_algorithm
    chosen_options <- list(
      meat = input$choose_meat,
      veggie = input$choose_veggie,
      fruit = input$choose_fruit
    )

    # finds matching dish for chosen options types and all 'any' dishes
    if(match_algorithm == 'or'){
      matches <- or_match(chosen_options, meal_recipes)
    } else if (match_algorithm == 'and'){
      matches <- and_match(chosen_options, meal_recipes)
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

    # generates link cmd
    link_cmd <- make_link(random_dish$dish)

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

    # generates link cmd
    link_cmd <- make_link(random_dessert$dish)

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
