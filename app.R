library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(dplyr)
library(shinythemes)

category_drinks <- c("Calories", "Fat (g)", "Carb. (g)", "Fiber", "Protein", "Sodium")
category_food <- c("Calories", "Fat (g)", "Carb. (g)", "Fiber (g)", "Protein (g)")

drinks <- read_csv("data/starbucks-menu-nutrition-drinks.csv")
foods <- read_csv("data/starbucks-menu-nutrition-food.csv")

#This block is for converting the chars of the data into numbers

i1 <- c(2,3,4,5,6,7)
i2 <- c(2,3,4,5,6)

drinks[ , i1] <- apply(drinks[ , i1], 2, function(x) as.numeric(as.character(x)))
foods[ , i2] <- apply(foods[ , i2], 2, function(x) as.numeric(as.character(x)))


ui <- fluidPage(theme = shinytheme("flatly"),
  
  # Application title
  titlePanel(
    h1("Starbucks Nutition Information", align = "center")),
  
  sidebarLayout(
    sidebarPanel(
      
      h5("Use the dropdown to choose the desired nutritional information."),
      
      selectInput(inputId =  "drink_criteria", label = "Drink Criteria", choices = category_drinks, selected = "Calories"),
      
      selectInput(inputId =  "food_criteria", label = "Food Criteria", choices = category_food, selected = "Calories"),
      
      h5("Use the sliders to select the desired range for the selected nutritional information."),
      
      sliderInput(inputId = "max", label = "Maximum", min = 0, max = 700, value = 700),
      
      sliderInput(inputId = "min", label = "Minimum", min = 0, max = 500, value = 0),
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel(title = "Drink Chart",
                           plotOutput(outputId = "drink_plot")
                           
                  ),
                  
                  tabPanel(title= "Drink Table",
                           DTOutput(outputId = "drink_table")
                  ),
                  
                  tabPanel(title = "Food Chart",
                           plotOutput(outputId = "food_plot")
                  ),
                  
                  tabPanel(title = "Food Table",
                           DTOutput(outputId = "food_table")
                  ), 
                  
                  tabPanel(title = "About",
                           
                           h2("About the App"),
                           h3("This app was built to show the nutritional values for a variety of Starbucks drinks and foods."),
                           h2("How to Use"),
                           h3("All the controls for this app are located in the sidebar on the left. To choose the nutritional information you would like to see in the graphs
                           select it from the dropdown. Once the desired nutitional information is picked use the sliders below it to select the range you would like to
                           view. If you desire an exact measurement (ex: 100 calories) move the slider to close to the desired number. Once it is close use the 
                              arrow keys for fine adjustment."),
                           h2("What You See"),
                           h3("Each of Food and Drink has two tabs. The first is a graph showing a list of items within the selected range. 
                        The second is a table of all the resusts within the selected range. This allows you to view the other nutritional 
                           infromation about the items." ),
                           h2("Replacement is Length Zero Error"),
                           h3("This Error occurs when the sliders are set so that there is no item that fits the selections criteria. 
                              the way to fix this error is to move the sliders to the minimum and maximum positions respectivly to see
                              the range in which items in the selected nutritional information category fall."),
                           h5("This app was created by Adam Maier for DSBA 5122"),
                           h5("The data was sourced from: https://www.kaggle.com/datasets/starbucks/starbucks-menu")
                  )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  drinksinput <- reactive({
    drinks$mycal <- as.numeric(drinks[[input$drink_criteria]])
    drinks %>% dplyr::filter(input$drink_criteria != "NA" ) %>% 
      dplyr::filter(mycal >= input$min, mycal <= input$max) %>%  select(-c(mycal))
  })
  foodsinput <- reactive({
    foods$mycal <- as.numeric(foods[[input$food_criteria]])
    foods %>% dplyr::filter(input$food_criteria != "NA" ) %>% 
      dplyr::filter(mycal >= input$min, mycal <= input$max) %>%  select(-c(mycal))
  })
  
  #render both of the plots
  
  output$drink_plot <- renderPlot({
    
    drinksdf <- drinksinput()
    ggplot(drinksdf) +
      geom_col(aes(x = drink_name, y = .data[[input$drink_criteria]], fill = drink_name), show.legend = FALSE) +
      labs(x = "Drink", y = input$drink_criteria) +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(angle = 90))
  }, height = 800)
  
  output$food_plot <- renderPlot({
    
    foodsdf <- foodsinput()
    ggplot(foodsdf) +
      geom_col(aes(x = food_name, y = .data[[input$food_criteria]], fill = food_name), show.legend = FALSE) +
      labs(x = "Food", y = input$food_criteria) +
      theme_bw() +
      scale_x_discrete(guide = guide_axis(angle = 90))
  }, height = 800)
  
  #render both of the tables
  output$drink_table <- renderDT({DT::datatable(drinksinput())})
  output$food_table <- renderDT({DT::datatable(foodsinput())})
  
}
# Run the application 
shinyApp(ui = ui, server = server)