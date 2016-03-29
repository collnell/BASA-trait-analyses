library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("BASA trait variation analyses"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition="input.tabs=='Trait variation' || input.tabs=='Plasticity'",
          selectInput("trait", "Select plant trait to plot:",
                      list("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","Resistance"="resist.lsm","%N"="perN.lsm"))
      ),
        conditionalPanel(
          condition="input.trait=='N.shoot.lsm' && input.tabs=='Trait variation'",
          radioButtons("growwater","Pick treatment to plot:",choices=c("Water","No water","Both treatments"),selected="No water")
        ),
        conditionalPanel(
          condition="input.trait=='N.terps.lsm' && input.tabs=='Trait variation'",
          radioButtons("terpswater","Pick treatment to plot:",choices=c("Water","No water","Both treatments"),selected="No water")
        ),
      conditionalPanel(
        condition="input.tabs=='Trade-offs'",
        checkboxGroupInput("trade","Select traits for correlations:",choices=c("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","Resistance"="resist.lsm","%N"="perN.lsm"))
      )
    ),

    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Trait variation",
                 br(),
                 h4("Do plant traits vary by genotype and watering treatment?"),
                 br(),
                 plotOutput("famplots",height="300",width="1000"),
                 br(),
                 br(),
                 dataTableOutput("GEtable"),
                 br(),
                 br(),
                 dataTableOutput("SEXtable"),
                 br(),
                 br(),
                 dataTableOutput("SEXh2otable"),
                 br(),
                 br(),
                 p("All mixed models were run using 'lmer' in 'lme4' package. P-values are calculated from 10,000 parametric bootstrap simulations of the deviance statistic for each fixed effect term in the model (package 'afex').")
        ),
        tabPanel("Plasticity",
                 br(),
                 h4("Is there genetic variation in trait plasticity?"),
                 br(),
                 plotOutput("plasticplots",height="300",width="1000"),
                 br(),
                 br(),
                 h4("Trait plasticity = ln(water treatment/no water)"),
                 br(),
                 dataTableOutput("estests",width="400"),
                 br(),
                 br()
        
        ),
        tabPanel("Trade-offs",
                 br(),
                 h4("How are BASA traits correlated?"),
                 br(),
                 plotOutput("corrplot"),
                 br()
        ),
        tabPanel("Resistance",
                 br(),
                 h4("Does genetic variation in traits influence plant resistance?"),
                 br(),
                 plotOutput("resistplot"),
                 br()
        )
      )
    )
  )
))
