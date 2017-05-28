library(shiny)
library(stringr)
library(purrr)

# load recipes
load("recipes.Rdata")
r <- transpose(transpose(recipes)$ingredients)

shinyServer(function(input, output) {

  # chosen options from user
  match_list <- reactive({
    chosen_options <- list(
      meat = input$choose_meat,
      veggies = input$choose_veggie,
      fruit = input$choose_fruit
    )

    # finds matching dish for chosen options and all 'any' dishes
    # prints out in alphabetical order
    matches <- map(names(chosen_options), function(type){
      # finds all matches, returns if none requested
      options <- chosen_options[[type]]
      if(length(options) == 0) return(character(0))
      map(options, function(x){
        matches <- names(recipes)[str_detect(r[[str_to_title(type)]], x)]
        any <- names(recipes)[str_detect(r[[str_to_title(type)]], "any")]
        return( c(matches, any) )
      })
    }) %>% unlist %>% unique %>% sort

  })

  # output matches
  lapply(1:length(recipes), function(i) {
    output[[paste0("match", i)]] <- renderUI({

      name <- match_list()[i]
      address <- name %>%
        str_replace_all(" ", "-") %>%
        paste0("http://jnguyen92.github.io/nhuyhoa//2017/05/", ., ".html")
      if(!is.na(name)) h4(a(href = address, name))

    })
  })

})
