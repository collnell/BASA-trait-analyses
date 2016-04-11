library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("BASA common garden traits"),
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
        condition="input.tabs=='lsms'",
        checkboxGroupInput("trade","Select traits for correlations:",choices=c("Growth" = "N.shoot.lsm","Terpenes" = "N.terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","Resistance"="resist.lsm","%N"="perN.lsm"))
      ),
      conditionalPanel(
        condition="input.tabs=='Resistance'",
        selectInput("vresist", "Select trait to plot against 'resistance':",
                    list("Growth" = "shoot.lsm","Terpenes" = "terps.lsm","SLA"="SLA.lsm","%H20"="H20.lsm","leaf toughness"="tough.lsm.x","CN"="CN.lsm","WUE"="WUE.lsm","flowers"="flower.lsm","%N"="perN.lsm"))
      ),
      conditionalPanel(
        condition="input.tabs=='Arthropods'",
        radioButtons("testtype","Include plant sex in model?",choices=c("Yes","No"),selected="Yes"))
      ),
    
     
    mainPanel(
      tabsetPanel(id="tabs",
        tabPanel("Trait variation",
                 br(),
                 h4("Genotype*Water effects"),
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
                 h4("Genotype*Water interaction"),
                 br(),
                 plotOutput("plasticplots",height="300",width="1000"),
                 br(),
                 br(),
                 h4("Trait plasticity = ln(water treatment/no water)"),
                 br(),
                 verbatimTextOutput(outputId = "estests"),
                 br(),
                 br()
        
        ),
        tabPanel("lsms",
                 br(),
                 h4("Quick look at lsmeans"),
                 p("(Figure takes 30 seconds to load)"),
                 br(),
                 plotOutput("corrplot",width="950",height="800"),
                 br()
        ),
        tabPanel("Resistance",
                 br(),
                 h4("Resistance = log(1/herbivore abundance)"),
                 br(),
                 plotOutput("resistplot",width="500",height="500"),
                 br(),
                 dataTableOutput("resistGE"),
                 br(),
                 br(),
                 dataTableOutput("resistaov"),
                 br(),
                 br()
        ),
        tabPanel("Arthropods",
                 br(),
                 h4("Arthropod trophic groups"),
                 br(),
                 plotOutput("herbpred",width="500"),
                 p("Fig. Relationship between arthropod herbivores and predators sample from common garden plants."),
                 br(),
                 p("Model Summary"),
                 verbatimTextOutput(outputId = "raw_summary_phsex"),
                 br(),
                 br(),
                 p("Using residuals from the line pred~herb"),
                 br(),
                 br()
      )
    )
  )
))
)