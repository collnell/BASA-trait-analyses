library(shiny)
library(datasets)
library(ggplot2)
library(cowplot)

setwd("/Users/colleennell/Documents/R")
traitlsms.water <-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_fxh20.csv")
traitlsms.fam<- read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_es.csv")
sexmeans<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_sexmeansge.csv")
theme_set(theme_minimal())
traitlsms.fam$FAM<-as.factor(traitlsms.fam$FAM)
# Define server logic required to plot various variables against
# mpg
shinyServer(function(input, output) {
  gtReactive <- reactive({input$trait
  })

  # Generate a plot of the requested variable against mpg and
  # only include outliers if requested
 
    output$famplots <- renderPlot({
      # Create long form subset of the data
      gtsub = traitlsms.fam[,c("FAM",gtReactive())]
      gtmelt = melt(gtsub,id = "FAM")
      ptrait = input$trait
      growrank = interaction(reorder(traitlsms.fam$FAM,traitlsms.fam[,input$trait]),traitlsms.fam$SEX)
      growvar<-ggplot(traitlsms.fam,aes_string(x=growrank,y=ptrait,color="SEX"))+
        geom_point(size=2)+
        theme_bw()+
        labs(x=" ",y="Growth (no water)",color=" ")+
        theme(axis.text.x=element_blank(),legend.position="none",text=element_text(size=15),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        scale_y_continuous(limits = c(-300, 7000))
      
      growsex<-ggplot(sexmeans,aes(x=sexmeans$SEX,y=sexmeans$shootmean,color=sexmeans$SEX))+
        geom_point(size=6)+
        geom_errorbar(aes(ymin=(sexmeans$shootmean-sexmeans$shootse),ymax=(sexmeans$shootmean+sexmeans$shootse)),width=.1)+
        theme_bw()+
        labs(y="",x="Plant Sex",color="Plant Sex")+
        theme(axis.text.y=element_blank(),text=element_text(size=15), panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
        scale_y_continuous(limits = c(-300, 7000))
      
      fig2 <- ggdraw() + 
        draw_plot(growvar, x = 0, y = 0, width = .75, height = 1) +
        draw_plot(growsex, x = .75, y = 0, width = .25, height = 1)
      fig2
    })
  output$plasticplots <- renderPlot({
    growrankes<-interaction(reorder(traitlsms.fam$FAM,traitlsms.fam$shootes),traitlsms.fam$SEX)
    growvares<-ggplot(traitlsms.fam,aes(x=growrankes,y=traitlsms.fam$shootes,color=traitlsms.fam$SEX))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=(traitlsms.fam$shootes-.1),ymax=(traitlsms.fam$shootes+.1)),width=.1)+
      theme_bw()+
      labs(x="Genotype (rank)",y="Growth Plasticity",color="Plant Sex")+
      theme(legend.position="none",text=element_text(size=15),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      scale_y_continuous(limits = c(-.6, 1.5))
    growsexes<-ggplot(sexmeans,aes(x=sexmeans$SEX,y=sexmeans$shootESmean,color=sexmeans$SEX))+
      geom_point(size=5)+
      geom_errorbar(aes(ymin=(sexmeans$shootESmean-sexmeans$shootESse),ymax=(sexmeans$shootESmean+sexmeans$shootESse)),width=.1)+
      theme_bw()+
      labs(x="Plant Sex",y=" ",color="Plant Sex")+
      theme(axis.text.y=element_blank(),text=element_text(size=15),  panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
      scale_y_continuous(limits = c(-.6, 1.5))
    fig4 <- ggdraw() + 
      draw_plot(growvares, x = 0, y = 0, width = .75, height = 1) +
      draw_plot(growsexes, x = .75, y = 0, width = .25, height = 1)
    fig4
  })
})