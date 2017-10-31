# Recipe Finder

## Check it Out!

[Run the app](https://jennynguyen.shinyapps.io/recipe_finder/)

## Quick Summary

Don't let your food waste away in the refrigerator! Use the [Recipe Finder](https://jennynguyen.shinyapps.io/recipe_finder/) to search for delicious recipes!

The [Recipe Finder](https://jennynguyen.shinyapps.io/recipe_finder/) is an R Shiny application that searches through recipes available on [NhuyHoa's recipes](http://jennguyen1.github.io/nhuyhoa/recipes/) for selected ingredients. All recipe information is stored on SQLite database `data/recipes.db`, which both the website and the application pulls from. 

![screenshot of display 1](images/app_screenshot1.jpg)

Click on any match to get the recipe! If you can't decide, click on the *I'm Feeling Lucky* button and we will find one for you!

![screenshot of display 2](images/app_screenshot2.jpg)

The default matching algorithm searches for recipes that contain any of selected ingredients. This option can be set to display only the recipes that contain all of the selected ingredients. 

![screenshot of display 3](images/app_screenshot3.jpg)
![screenshot of display 4](images/app_screenshot4.jpg)

You can also view all dessert recipes and find a random dessert recipe to make.

![screenshot of display 5](images/app_screenshot5.jpg)

New recipes are added daily, so check back soon!
