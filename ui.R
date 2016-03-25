library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Trait variation and plasticity in BASA"),
  sidebarLayout(
    sidebarPanel(
      selectInput("trait", "Select plant trait to plot:",
                  list("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","Resistance"="resist.lsm","%N"="perN.lsm")),
      conditionalPanel(
        condition="input.trait=='N.shoot.lsm'",
        radioButtons("growwater","Water Treatment",choices=c("Yes","No","Both"),selected="No")
      ),
      conditionalPanel(
        condition="input.trait=='N.terps.lsm'",
        radioButtons("terpswater","Water Treatment",choices=c("Yes","No","Both"),selected="Both")
      )
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Genetic Variation",
                 br(),
                 h4(textOutput("text1")),
                 br(),
                 plotOutput("famplots",height="300",width="800"),
                 br(),
                 br(),
                 tableOutput("mytable1")
        ),
        tabPanel("Plasticity",
                 plotOutput("plasticplots",height="300",width="800"))
        
      )
    )
  )
))
