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
        condition="input.tabs=='Correlations'",
        checkboxGroupInput("trade","Select traits for correlations:",choices=c("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","Resistance"="resist.lsm","%N"="perN.lsm"))
      ),
      conditionalPanel(
        condition="input.tabs=='Resistance'",
        selectInput("vresist", "Select trait to test against 'resistance':",
                    list("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","%N"="perN.lsm")),
        checkboxGroupInput("herbyear","Select sampling year for herbivore data:",choices=c("2010","2011")),
        checkboxGroupInput("resist","Define arthropod taxa used in resistance:",choices=c("Hemiptera","Diptera","Hymenoptera","Pscoptera","Coleoptera","Lepidoptera"))
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
                 h4("Do sexes differ in trait plasticity?"),
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
        tabPanel("Correlations",
                 br(),
                 h4("Are traits correlated?"),
                 p("(Figure takes 30 seconds to load)"),
                 br(),
                 plotOutput("corrplot",width="950",height="800"),
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
