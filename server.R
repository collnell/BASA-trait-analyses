library(shiny)##interactive
library(reshape)
library(datasets)##melting
library(ggplot2)#plotting
library(cowplot)#plot arrangment
library(plotrix)#standard error calculations
library(DT)
library(dplyr)
library(ggthemes)
library(GGally)
library(car)

set.seed(300)
theme_set(theme_bw())##removing this creates error with the theme and doesnt put out the both water treats


setwd("/Users/colleennell/Documents/R")
traitlsms.water <-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_fxh20.csv")
traitlsms.fam<- read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_fx_shiny.csv")
sexmeans<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_sexmeansge.csv")
GE.grow<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_shiny_GE_grow.csv")
sextable<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_sextable_shiny.csv")
SEXh2o<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_sexh2o_shiny.csv")
esttest<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_estests_shiny.csv")
resistgetab<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_shiny_resistGE.csv")
resistaovtab<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_shiny_resistaov.csv")
resistwaterlsms<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_famwater.csv")
resistlsms<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_famnowater.csv")
byfeed<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/basa_traits_herb_feed.csv")
traits<-read.csv("/Users/colleennell/Documents/R/basa_traits/data/BASA_traits_raw.csv")

traitlsms.fam$FAM<-as.factor(traitlsms.fam$FAM)

traits$BLOCK<-ifelse(traits$ROW>=22 & traits$COL>=12,"12",
                     ifelse(traits$ROW>=22 & traits$COL>=7 & traits$COL<=11,"11",
                     ifelse(traits$ROW>=22 & traits$COL<=6,"10",
                     ifelse(traits$ROW>=15 & traits$ROW<=21 & traits$COL>=12,"9",
                     ifelse(traits$ROW>=15 & traits$ROW<=21 & traits$COL<=11 & traits$COL>=7,"8",
                     ifelse(traits$ROW>=15 & traits$ROW<=21 & traits$COL<=6,"7",
                     ifelse(traits$ROW>=8 & traits$ROW<=14 & traits$COL>=12,"6",
                     ifelse(traits$ROW>=8 & traits$ROW<=14 & traits$COL<=11 & traits$COL>=7,"5",
                     ifelse(traits$ROW>=8 & traits$ROW<=14 & traits$COL<=6,"4",
                     ifelse(traits$ROW<=7 & traits$COL>=12,"3",
                     ifelse(traits$ROW<=7 & traits$COL<=11 & traits$COL>=7,"2",
                     ifelse(traits$ROW<=7 & traits$COL<=6,"1",NA
                     ))))))))))))

byfeed$ID<-as.factor(byfeed$ID)
byfeed[is.na(byfeed)]<-0
traits$ID<-as.factor(traits$ID)
arth_data<-left_join(byfeed,traits,by="ID")
arth_data$pred_total<-arth_data$Pred.x+arth_data$Pred.y
arth_data$herb_total<-arth_data$Herb.x+arth_data$Herb.y
arth_data$detrit_total<-arth_data$Detrit.x+arth_data$Detrit.y
arth_data$omni_total<-arth_data$Omni.x+arth_data$Omni.y
arth_data$non_total<-arth_data$Non.x+arth_data$Non.y
arth_data$SEX<-arth_data$SEX.x
predherbsex<-lm(pred_total~herb_total+SEX,data=arth_data)

traitlsms.fam$Growth_LRR<-traitlsms.fam$shootes
traitlsms.fam$Terpenes_LRR<-traitlsms.fam$terpses

# Define server logic required to plot various variables against
# mpg
shinyServer(function(input, output) {
  traitReactive <- reactive({input$trait
  })
  resistReactive <- reactive({input$vresist
  })
    output$resistplot <- renderPlot({
      # Create long form subset of the data
      ressub = resistlsms[,c("FAM",resistReactive())]
      resmelt = melt(ressub,id = "FAM")
      vsres = input$vresist
      
      resplot<-ggplot(resistlsms,aes_string(x=vsres,y="rez.log"))+
        geom_point(size=3)+geom_smooth(method="lm",se=F)+
        labs(y="Low <- Resistance -> High")+theme_bw()+
        scale_color_brewer(palette=c("Set1"))
      
        if ((input$vresist) == "shoot.lsm") {
          resplot<-ggplot(resistwaterlsms,aes(x=log(shoot.lsm),y=rez.log,group=WATER,color=WATER))+
            geom_point(size=3)+geom_smooth(method="lm",se=F)+
            labs(y="Low <- Resistance -> High",x="Growth")+theme_bw()+
            scale_color_brewer(palette=c("Set2"))
        }
        if ((input$vresist) == "terps.lsm") {
          resplot<-ggplot(resistwaterlsms,aes(x=log(terps.lsm),y=rez.log,group=WATER,color=WATER))+
            geom_point(size=3)+geom_smooth(method="lm",se=F)+
            labs(y="Low <- Resistance -> High",x="Terpenes")+theme_bw()+
            scale_color_brewer(palette=c("Set2"))
        }
      
      resplot
    })
    output$GEtable <- renderDataTable({
      datatable(GE.grow,rownames=F,escape=T,filter="none",class="compact cell-border",options=list(dom='tr',scrollX=T,scrollY="250px",scrollCollapse = F,columns.orderable="F"),
                caption="Table 1. Results from linear mixed models testing for the effects of water, 
                genotype, and water*genotype interaction. Block' is included in the model as a random effect)
                to account for spatial correlation in the common garden. An interaction between 'Water' and 'Genotype' indicates genetic variation in plasticity. 
                Model: lmer(Trait~Water*Genotype+(1|Block))")
    })
    output$SEXtable <- renderDataTable({
      datatable(sextable, rownames=F,filter="none",class="compact cell-border",options=list(dom='tr',columns.orderable="F"),
                caption="Table 2. ANOVA results testing for sexual dimorphism in plant traits. Analysis executed using genotype ls-means derived from raw data, and 'no water' treatment ls-means for the 'Growth' and 'Terpenes'.
                Model: Anova(lm(Trait~Sex))")
    })
    output$SEXh2otable <- renderDataTable({
      datatable(SEXh2o, rownames=F,filter="none",class="compact cell-border",options=list(dom='tr',columns.orderable="F"),
                caption="Table 3. LMM results testing for sex differences in traits that showed genotype*water interaction. Analysis executed using genotype ls-means for each water treatment (2 values per genotype), and genotype included as a random effect to account for repeated measures.
                Model: lmer(Trait~Sex*Water+(1|FAM)))")
    })
    output$estests <- renderPrint({
        essext <- Anova(lm(Growth_LRR~SEX,data=traitlsms.fam),type="III")
        print(essext)
        essex <- Anova(lm(Terpenes_LRR~SEX,data=traitlsms.fam),type="III")
        print(essex)
    })
    output$resistGE <- renderDataTable({
      datatable(resistgetab, rownames=F,filter="none",class="compact cell-border",options=list(dom='tr',columns.orderable="F"),
                caption="Table . LMM results for plant resistance.  Model: lmer(Resistance~Water*genotype+year+(1|ID)+(1|BLOCK))") 
    })
    output$resistaov<-renderDataTable({
      datatable(resistaovtab,rownames=F,filter="none",class="compact cell-border",options=list(dom='tr',pageLength=18,columns.orderable="F"),
                caption="Table. Regression & ANCOVA results on resistance using trait lsms.  Model1: Anova(lm(Resistance~Trait)), Model2: Anova(lm(Resistance~Trait*Water+Genotype))")
    })
    output$raw_summary_phsex <- renderPrint({
      if ((input$testtype)=="Yes"){
      fm <- lm(pred_total~herb_total+SEX,data=arth_data)
      print(summary(fm))
      }
      if ((input$testtype)=="No"){
        fm <- lm(pred_total~herb_total,data=arth_data)
        pval<-summary(fm)$coefficents[,4]
        print(summary(fm))
      }
    })
    output$corrplot<-renderPlot({
      corrtrait = input$trade
      ggpairs(traitlsms.fam[,c("N.shoot.lsm","N.terps.lsm","SLA.lsm","resist.lsm","tough.lsm.x","SEX")],
              title="BASA Traits",
              upper=list(continuous="cor",combo="box",discrete="ratio"),
              lower=list(continuous="smooth",combo="facetdensity",discrete="facetbar"),
              aes(colour=SEX,alpha=.5))
    })
    output$herbpred<-renderPlot({
      if ((input$testtype)=="Yes"){
        herbpred<-ggplot(arth_data,aes(x=herb_total,y=pred_total,group=SEX,color=SEX, lty=SEX))+geom_point(size=3)+geom_smooth(method="lm",se=F)+
          scale_color_brewer(palette=c("Set1"))+labs(x="Total Herbivores",y="Total Predators",color="Plant Sex")
        print(herbpred)
      }
      if ((input$testtype)=="No"){
        herbpred<-ggplot(arth_data,aes(x=herb_total,y=pred_total))+geom_point(size=3)+geom_smooth(method="lm",se=F)+
          labs(x="Total Herbivores",y="Total Predators")+
          scale_linetype_manual(values = ifelse(pval<= 0.05,"solid",ifelse(pval<=0.10 & pval>0.05,"dashed", "blank")))
        herbpred
      }
    })
    output$famplots <- renderPlot({
      # Create long form subset of the data
      traitsub = traitlsms.fam[,c("FAM",traitReactive())]
      traitmelt = melt(traitsub,id = "FAM")
      ptrait = input$trait
    
      growrank = interaction(reorder(traitlsms.fam$FAM,traitlsms.fam[,input$trait]),traitlsms.fam$SEX)
      growvar<-ggplot(traitlsms.fam,aes_string(x=growrank,y=ptrait,color="SEX"))+
        geom_point(size=3)+
        scale_color_brewer(palette=c("Set1"))+
        theme_bw()+
        labs(x=" Genotype (rank)",color=" ")+
        theme(panel.border = element_blank(),axis.text.x=element_blank(),legend.position="none",text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))

      growsex<-ggplot(traitlsms.fam,aes_string(x="SEX",y=ptrait,color="SEX"))+
        stat_summary(fun.y="mean",geom="point",size=6)+theme_bw()+
        stat_summary(fun.data=mean_se,geom="errorbar",width=.1)+
        scale_color_brewer(palette=c("Set1"))+
        labs(y="",x="Sex",color="Plant Sex")+
        theme(panel.border = element_blank(),axis.text=element_blank(),text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
      
      
        if ((input$trait) == "N.shoot.lsm") {
          growsex<-growsex+scale_y_continuous(limits = c(-500,7000))
          growvar<-ggplot(traitlsms.fam,aes_string(x=growrank,color="SEX"))+
            theme_bw()+
            scale_color_brewer(palette=c("Set1"))+
            theme(panel.border = element_blank(),axis.text.x=element_blank(),legend.position="none",text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
          growvar+
            scale_y_continuous(limits = c(-500,7000))+labs(y="Growth")
          if ((input$growwater) == "Water"){
            growranks=interaction(reorder(traitlsms.fam$FAM,traitlsms.fam$Y.shoot.lsm),traitlsms.fam$SEX)
            growvar<-growvar+
              geom_point(aes(x=growranks,y=Y.shoot.lsm),size=3)+
              labs(x=" Genotype (rank)",y= "Growth",color=" ")+
              geom_errorbar(aes(ymin=Y.shoot.lsm-Y.shoot.se,ymax=Y.shoot.lsm+Y.shoot.se),width=.3)
          } 
          if ((input$growwater) == "No water"){
            growvar<-growvar+
              geom_point(aes(y=N.shoot.lsm),size=3)+
              labs(x=" Genotype (rank)",y= "Growth",color=" ")+
              geom_errorbar(aes(ymin=N.shoot.lsm-N.shoot.se,ymax=N.shoot.lsm+N.shoot.se),width=.3)
          }
          if ((input$growwater) == "Both treatments"){
            growvar<-growvar+
              geom_point(aes(y=N.shoot.lsm),shape=16,size=3)+
              geom_point(aes(y=Y.shoot.lsm),size=3,shape=1)+
              geom_errorbar(aes(ymin=Y.shoot.lsm-Y.shoot.se,ymax=Y.shoot.lsm+Y.shoot.se),width=.3)+
              geom_errorbar(aes(ymin=N.shoot.lsm-N.shoot.se,ymax=N.shoot.lsm+N.shoot.se),width=.3)+
              labs(x=" Genotype (ranked by 'no water' trait values)",y= "Growth",color=" ")
            growsex<-ggplot(traitlsms.water,aes(x=SEX,y=shoot.lsm,shape=WATER,color=SEX))+
              stat_summary(fun.y="mean",geom="point",size=6)+
              labs(y="",x="Sex",color="Plant Sex")+
              stat_summary(fun.data=mean_se,geom="errorbar",width=.3)+
              scale_y_continuous(limits = c(-500,7000))+
              scale_color_brewer(palette=c("Set1"))+
              scale_shape_manual(values=c(16,1))+
              labs(shape="Water \nTreatment")+
              theme(panel.border = element_blank(),axis.text=element_blank(),text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
            
          } 
      } else if ((input$trait) == "N.terps.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(-120,550))
        growvar<-ggplot(traitlsms.fam,aes_string(x=growrank,color="SEX"))+
          theme_bw()+
          scale_color_brewer(palette=c("Set1"))+
          scale_y_continuous(limits = c(-120,550))+labs(y="Total terpenes")+
          theme(panel.border = element_blank(),axis.text.x=element_blank(),legend.position="none",text=element_text(size=18,face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
      
        if ((input$terpswater) == "Water"){
          terpsranks=interaction(reorder(traitlsms.fam$FAM,traitlsms.fam$Y.terps.lsm),traitlsms.fam$SEX)
          growvar<-growvar+
            geom_point(aes(x=terpsranks,y=Y.terps.lsm),size=3)+
            labs(x=" Genotype (rank)",y= "Total terpenes",color=" ")+
            geom_errorbar(aes(ymin=Y.terps.lsm-Y.terps.se,ymax=Y.terps.lsm+Y.terps.se),width=.3)
        } 
        if ((input$terpswater) == "No water"){
          growvar<-growvar+
            geom_point(aes(y=N.terps.lsm),size=3)+
            labs(x=" Genotype (rank)",y= "Total terpenes",color=" ")+
            geom_errorbar(aes(ymin=N.terps.lsm-N.terps.se,ymax=N.terps.lsm+N.terps.se),width=.3)
        }
        if ((input$terpswater) == "Both treatments"){
          growvar<-growvar+
            geom_point(aes(y=N.terps.lsm),shape=16,size=3)+
            geom_point(aes(y=Y.terps.lsm),size=3,shape=1)+
            geom_errorbar(aes(ymin=Y.terps.lsm-Y.terps.se,ymax=Y.terps.lsm+Y.terps.se),width=.3)+
            geom_errorbar(aes(ymin=N.terps.lsm-N.terps.se,ymax=N.terps.lsm+N.terps.se),width=.3)+
            labs(x=" Genotype (rank order 'no water' values)",y= "Total terpenes",color=" ")
          growsex<-ggplot(traitlsms.water,aes(x=SEX,y=terps.lsm,shape=WATER,color=SEX))+
            stat_summary(fun.y="mean",geom="point",size=6)+
            labs(y="",x="Sex",color="Plant Sex")+
            stat_summary(fun.data=mean_se,geom="errorbar",width=.3)+
            scale_y_continuous(limits = c(-120,550))+
            scale_color_brewer(palette=c("Set1"))+
            scale_shape_manual(values=c(16,1))+
            labs(shape="Water \nTreatment")+
            theme(panel.border = element_blank(),axis.text=element_blank(),text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
          
        } 
        
        
      } else if ((input$trait) == "SLA.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(100,180))
        growvar<-growvar+scale_y_continuous(limits = c(100,180))+labs(y="SLA")+geom_errorbar(aes(ymin=SLA.lsm-SLA.se,ymax=SLA.lsm+SLA.se),width=.3)
      } else if ((input$trait) == "H20.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(0.64,.78))
        growvar<-growvar+scale_y_continuous(limits = c(0.64,.78))+labs(y="% H2O")+geom_errorbar(aes(ymin=H20.lsm-H20.se,ymax=H20.lsm+H20.se),width=.3)
      } else if ((input$trait) == "tough.lsm.x") {
        growsex<-growsex+scale_y_continuous(limits = c(65,110))
        growvar<-growvar+scale_y_continuous(limits = c(65,110))+labs(y="Leaf toughness")+geom_errorbar(aes(ymin=tough.lsm.x-tough.se.x,ymax=tough.lsm.x+tough.se.x),width=.3)
      } else if ((input$trait) == "CN.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(12,20))
        growvar<-growvar+scale_y_continuous(limits = c(12,20))+labs(y="C:N")+geom_errorbar(aes(ymin=CN.lsm-CN.se,ymax=CN.lsm+CN.se),width=.3)
      } else if ((input$trait) == "WUE.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(21,24))
        growvar<-growvar+scale_y_continuous(limits = c(21,24))+labs(y="WUE")+geom_errorbar(aes(ymin=WUE.lsm-WUE.se,ymax=WUE.lsm+WUE.se),width=.3)
      } else if ((input$trait) == "flower.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(-50,500))
        growvar<-growvar+scale_y_continuous(limits = c(-50,500))+labs(y="Total flowers")+geom_errorbar(aes(ymin=flower.lsm-flower.se,ymax=flower.lsm+flower.se),width=.3)
      } else if ((input$trait) == "resist.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(0,.05))
        growvar<-growvar+scale_y_continuous(limits = c(0,.05))+labs(y="Resistance (1/herbivore density)")+geom_errorbar(aes(ymin=resist.lsm-resist.se,ymax=resist.lsm+resist.se),width=.3)
      } else if ((input$trait) == "perN.lsm") {
        growsex<-growsex+scale_y_continuous(limits = c(2.2,3.6))
        growvar<-growvar+scale_y_continuous(limits = c(2.2,3.6))+labs(y="% Nitrogen")+geom_errorbar(aes(ymin=perN.lsm-perN.se,ymax=perN.lsm+perN.se),width=.3)
      } else {
        growsex<-growsex+scale_y_continuous(limits=c(-300,6500))
        growvar<-growvar+scale_y_continuous(limits = c(-300,6500))+labs(y=input$trait)
      }
      
      fig2 <- ggdraw() + 
        draw_plot(growvar, x = 0, y = 0, width = .75, height = 1) +
        draw_plot(growsex, x = .75, y = 0, width = .25, height = 1)
      fig2
      
    })
    
    ###now plasticity plots, just for growth and terpenes (GE interact)
  output$plasticplots <- renderPlot({
    traitsub = traitlsms.fam[,c("FAM",traitReactive())]
    traitmelt = melt(traitsub,id = "FAM")
    pltrait = input$trait
    
    growranky<-interaction(reorder(traitlsms.fam$FAM,traitlsms.fam$shootes),traitlsms.fam$SEX)
    growvary<-ggplot(traitlsms.fam,aes_string(x=growranky,y=traitlsms.fam$shootes,color="SEX"))+
      geom_point(size=3)+
      geom_errorbar(ymin=traitlsms.fam$shootes-traitlsms.fam$shootes.se,ymax=traitlsms.fam$shootes+traitlsms.fam$shootes.se,width=.3)+
      theme_bw()+
      labs(x="Genotype (rank)",y="Growth Plasticity",color="Plant Sex")+
      scale_color_brewer(palette=c("Set1"))+
      theme(panel.border = element_blank(),axis.text.x=element_blank(),legend.position="none",text=element_text(size=18,face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))+
      scale_y_continuous(limits = c(-4, 4))
    growsexy<-ggplot(traitlsms.fam,aes(x=traitlsms.fam$SEX,y=traitlsms.fam$shootes,color=SEX))+
      stat_summary(fun.y="mean",geom="point",size=6)+
      stat_summary(fun.data=mean_se,geom="errorbar",width=.3)+
      theme_bw()+
      scale_color_brewer(palette=c("Set1"))+
      labs(x="Plant Sex",y=" ",color="Plant Sex")+
      theme(panel.border = element_blank(),axis.text.x=element_blank(),text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))+
      scale_y_continuous(limits = c(-4, 4))
    
    if ((input$trait) == "N.terps.lsm") {
    growranky<-interaction(reorder(traitlsms.fam$FAM,traitlsms.fam$terpses),traitlsms.fam$SEX)
    growvary<-ggplot(traitlsms.fam,aes_string(x=growranky,y=traitlsms.fam$terpses,color="SEX"))+
      geom_point(size=3)+
      geom_errorbar(ymin=traitlsms.fam$terpses-traitlsms.fam$terpses.se,ymax=traitlsms.fam$terpses+traitlsms.fam$terpses.se,width=.3)+
      theme_bw()+
      labs(x="Genotype (rank)",y="Terpene Plasticity",color="Plant Sex")+
      scale_color_brewer(palette=c("Set1"))+
      scale_y_continuous(limits = c(-4, 4))+
      theme(panel.border = element_blank(),axis.text.x=element_blank(),legend.position="none",text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
    growsexy<-ggplot(traitlsms.fam,aes(x=traitlsms.fam$SEX,y=traitlsms.fam$terpses,color=traitlsms.fam$SEX))+
      stat_summary(fun.y="mean",geom="point",size=6)+
      stat_summary(fun.data=mean_se,geom="errorbar",width=.3)+
      theme_bw()+
      scale_color_brewer(palette=c("Set1"))+
      labs(x="Plant Sex",y=" ",color="Plant Sex")+
      scale_y_continuous(limits = c(-4, 4))+
      theme(panel.border = element_blank(),axis.text.x=element_blank(),text=element_text(size=18, face = "plain",margin = margin(), debug = FALSE),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=2,color = "black"))
    
    }
  
    fig4 <- ggdraw() + 
      draw_plot(growvary, x = 0, y = 0, width = .75, height = 1) +
      draw_plot(growsexy, x = .75, y = 0, width = .25, height = 1)
    fig4
  })
})