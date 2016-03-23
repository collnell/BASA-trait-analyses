library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("BASA G*E tests"),
  
  # Sidebar with controls to select the variable to plot against
  # mpg and to specify whether outliers should be included
  sidebarLayout(
    sidebarPanel(
      selectInput("trait", "Plant trait:",
                  list("Growth" = "N.shoot.lsm",
                    "Terpenes" = "N.terps.lsm"))
    ),
    
    # Show the caption and plot of the requested variable against
    # mpg
    mainPanel(
      h3(textOutput("caption")),
      tabsetPanel(
        tabPanel("Genetic Variation and Plasticity",
                 plotOutput("famplots",height="300",width="800"),
                 plotOutput("plasticplots",height="300",width="800"))
        
      )
    )
  )
))
