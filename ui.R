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
                  list("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","Resistance"="resist.lsm","%N"="perN.lsm"))
    ),
    
    # Show the caption and plot of the requested variable against
    # mpg
    mainPanel(
      tabsetPanel(
        tabPanel("Genetic Variation",
                 
                 h3(textOutput("text1")),
                 
                 plotOutput("famplots",height="300",width="800")
        ),
        tabPanel("Plasticity",
                 plotOutput("plasticplots",height="300",width="800"))
        
      )
    )
  )
))
