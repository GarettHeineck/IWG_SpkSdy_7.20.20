## Seed development and timing of harvest for maximum per spike yield in intermediate wheatgrass
## Script for Experiment 1
## Exploring seed yield development among cool-season grass species 
## Defining methodology used in Experiments 2 and 3
## Data can be found online: 
## Scirpt runs off of "metadata_IWG_SpkSdy_03.xlsx"
## Author: Garett Heineck

#################################
######################
###########
########### Loading packages
###########
######################
#################################
##
require(nlme)
require(rnoaa)
require(tidyr)
require(dplyr)
require(lubridate)
require(tidyverse)
require(ggplot2)
require(ggformula)
require(readxl)
require(segmented)
require(agricolae)
require(devtools)
require(pkgload)
require(car)
require(gridExtra)
require(cowplot)
require(easynls)
require(easynls)

## Loading a growth curve toolkit from GitHub
if(!require("devtools")) install.packages("devtools")
devtools::install_github("briandconnelly/growthcurve", build_vignettes = TRUE)
require(growthcurve)

## summary function
data_summary.se <- function(x) {
  m <- mean(x, na.rm=T)
  ymin <- m-(sd(x, na.rm=T)/sqrt(length(x)))
  ymax <- m+(sd(x, na.rm=T)/sqrt(length(x)))
  return(c(y=m,ymin=ymin,ymax=ymax))
} 

## geom_smooth function
lm_left <- function(formula,data,...){
  mod <- lm(formula,data)
  class(mod) <- c('lm_left',class(mod))
  mod
}

## Custom theme 
IWG_theme<- function(base_size = 16) {
  theme_minimal(base_size = base_size) %+replace%
    theme(strip.background = element_rect(fill = "grey85", color = "black", linetype = 1),
          legend.background =  element_rect(fill = "white", linetype = 0),
          legend.position = "bottom",
          panel.grid.major.y = element_line(linetype = "dotted", color = "grey40", size = .3),
          panel.grid.major.x = element_line(linetype = "dotted", color = "grey40", size = .3),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(fill = alpha("white", 0), color = "grey85"),
          axis.line = element_line(size = 2.5, colour = "grey50"),
          complete = TRUE)
}
#################################
######################
###########
###########
###########
######################
#################################
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Loading data sets and viewing summaries
###########
######################
#################################
## Sources of data can be found in Table one of this publication
## Figures of interest were cropped from the original text and loaded into FIJI
## Copies of each figure used were also pasted into the metadata for this script
## From there set scale function was used to apply pixel number to the trait of interest on each figure (example, number of pixels per one unit increase in seed weight)
## See excel spreadsheet for metadata
##
#Data from the discussion of Anslow (1963) p. 93
#Anslow discusses changes in FSU in relation to mid ansthesis (~June 6th), however anthesis was complete on the 10th 
#variables: FSU
anslow.disc.63<- data.frame(FSU=c(59.2, 62.1, 64.4, 67.8, 67.9, 69.1),
                            day.post.mid.anths=c(11,15,19,23,26,31),
                            day.post.end.anths=c(6,10,14,18,21,26))
summary(anslow.disc.63)
##
#Figure 4 from Anslow (1964)
#variables: seed wt.
anslow.fig4.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                             sheet="metadata.anslow.fig4",
                             skip=5,
                             na="NA")
summary(anslow.fig4.dat)
##
#Figure 7 from Anslow (1964)
#variables: floret shatter
anslow.fig7.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                             sheet="metadata.anslow.fig7",
                             skip=6,
                             na="NA")
summary(anslow.fig7.dat)
##
#Figure 8 from Anslow (1964)
#variables: seed yield 
anslow.fig8.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                             sheet="metadata.anslow.fig8",
                             skip=6,
                             na="NA")
summary(anslow.fig8.dat)
##
#Figure 1 from Anslow (1964)
#variables: moitsure content
anslow.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                             sheet="metadata.anslow.fig1",
                             skip=3,
                             na="NA")
summary(anslow.fig1.dat)
##
#Figure 1 and Table 1 Hill and Watkin 1975 (b) (second in series)
#variables: moisture content, seed weight, seed yield
hill.moist.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                             sheet="metadata.hill75.moist.fig1",
                             skip=4,
                             na="NA")
hill.seed.wt.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                               sheet="metadata.hill75.seed.wt.fig1",
                               skip=4,
                               na="NA")
hill.table1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                               sheet="metadata.hill75.yield.table1",
                               skip=4,
                               na="NA") 
summary(hill.moist.fig1.dat)
summary(hill.seed.wt.fig1.dat)
summary(hill.table1.dat)
##
#Figure 3 from Williams (1972)
#variables: floret shatter, FSU
williams.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                          sheet="metadata.williams.fig3",
                          skip=12,
                          na="NA")
summary(williams.dat)
williams.table1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                          sheet="metadata.williams.table1",
                          skip=4,
                          na="NA")
summary(williams.table1.dat)
williams.fig2.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                           sheet="metadata.williams.fig2",
                           skip=2,
                           na="NA")
summary(williams.fig2.dat)
##
#Figure 3 from Grabe (1956)
#variables: seed wt.
grabe.fig3.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                            sheet="metadata.grabe.fig3",
                            skip=5,
                            na="NA")
summary(grabe.fig3.dat)
##
#Figure 4 from Grabe (1956)
#variables: moisture
grabe.fig4.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                            sheet="metadata.grabe.fig4",
                            skip=5,
                            na="NA")
summary(grabe.fig4.dat)
##
#Figure 1 from Hyde (1959)
#variables: moisuture
hyde.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                           sheet="metadata.hyde.figure1",
                           skip=6,
                           na="NA")
summary(hyde.fig1.dat)
##
## Figure 1 and 2 from Berdahl (1998)
#variables: moisture
berdahl.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                              sheet="metadata.berdahl.fig1.2",
                              skip=6,
                              na="NA")
summary(berdahl.fig1.dat)
##
## Figure 1 from Burbidge (1978)
#variables: FSU
burbidge.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                               sheet="metadata.burbidge.fig1",
                               skip=5,
                               na="NA")
summary(burbidge.fig1.dat)
##
## Figure 1 panel c and e from Pegler (1976)
#variables: seed wt and yield
pegler.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                             sheet="metadata.pegler.fig1ce",
                             skip=5,
                             na="NA")
summary(pegler.fig1.dat)
##
## Figure 1 from Nellist and Rees (1968)
#variables: moisture
nellist.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                                    sheet="metadata.nellist.fig1",
                                    skip=4,
                                    na=".")
summary(nellist.fig1.dat)
##
## Figure 2 Hebblethwaite and Ahmed (1978)
#variables: moisture and seed wt.
hebblethwaite.fig1.2.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                              sheet="metadata.hebblethwaite.fig1.2",
                              skip=4,
                              na=".")
summary(hebblethwaite.fig1.2.dat)
##
## Figure 1 Roberts (1971)
#variables: moisture and seed wt.
roberts.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                                      sheet="metadata.roberts.fig1",
                                      skip=4,
                                      na=".")
summary(roberts.fig1.dat)
##
## Figure 1 Arnold and Lake (1966)
#variables: moisture
arnold.fig1.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                              sheet="metadata.arnold66.fig1",
                              skip=5,
                              na=".")
summary(arnold.fig1.dat)
##
#variables: FSU
elgersma.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                          sheet="metadata.elgersma88.table1.2",
                          skip=5,
                          na=".")
summary(elgersma.dat)
##
## Andrade (1994)
## Plotting the relationship between GDD and max yield, seed weight, Phase II moisture
andrade.dat<- read_excel("/Users/heine237/Desktop/Ongoing Projects/IWG_SpkSdy.01/Metadata/metadata_IWG_SpkSdy_03.xlsx",
                         sheet="metadata.andrade.fig5",
                         skip=5,
                         na=".")
summary(andrade.dat)
##
##
## Summary of historical data
# 15 studies total with 32 data sets total
# Moisture dynamics: 11
# Seed dry weight accumulation: 10
# Floret site utilization: 4
# Spikelet and floret shatter: 3
# Seed yield development: 4
#################################
######################
###########
###########
###########
######################
#################################
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Analysis of seed dry weight accumulation
###########
######################
#################################
## Formal statistics were applied by Warringa et al. (1998) who used the Gompertz model to study perennial ryegrass https://www.rdocumentation.org/packages/easynls/versions/5.0/topics/nlsfit
#They used the weight of the lemma and palea as the lower asymptote (see Figure 3)
#They set the upper asymptote dictated the maximum seed size once 95% of the fitted maximum value was obtained
## Loss et al., (1989) used old and modern wheat varieties to come to the conclusion that logistic regression was best suited and may be easier to apply than a Gompertz function
##
##
##
##
#####
## Analysis of  Hyde et al. (1959) on perennial ryegrass - Figure 1
#####
## self-starting
hyde.fig1.SS<-getInitial(seed.wt~SSlogis(days.post.pollination,alpha,xmid,scale),data=hyde.fig1.dat)
hyde.fig1.K_start<-hyde.fig1.SS["alpha"]
hyde.fig1.R_start<-1/hyde.fig1.SS["scale"]
hyde.fig1.N0_start<-hyde.fig1.SS["alpha"]/(exp(hyde.fig1.SS["xmid"]/hyde.fig1.SS["scale"])+1)
#the formula for the model
log_formula<-formula(seed.wt~K*N0*exp(R*days.post.pollination)/(K+N0*(exp(R*days.post.pollination)-1)))
#fit the model
hyde.fig1.nls<-nls(log_formula,start=list(K=hyde.fig1.K_start,R=hyde.fig1.R_start,N0=hyde.fig1.N0_start), data = hyde.fig1.dat)
#estimated parameters
summary(hyde.fig1.nls)$coefficients[1]*0.95 #95% maximum sed weight***
hyde.fig1.nls.dat<- data.frame(days.post.pollination=c(seq(0,45)))
hyde.fig1.nls.dat$caryopsis.wt<- predict(hyde.fig1.nls, list(days.post.pollination=c(seq(0:45))))
##
hyde.seed.wt<- ggplot()+
  geom_point(data = hyde.fig1.dat, 
             mapping = aes(x=days.post.pollination, 
                           y=seed.wt),
             size=2,
             shape=1)+
  geom_line(data = hyde.fig1.nls.dat, 
            mapping = aes(x=days.post.pollination, 
                          y=caryopsis.wt),
            size=1.5,
            color="grey20")+
  geom_hline(yintercept = (unlist(summary(hyde.fig1.nls))$coefficients1)*0.95,
             linetype="dashed",
             size=.75)+
  annotate("text", x = 7, y = 2.7,
           label = "95% maximum at 29d", 
           size=5)+
  scale_y_continuous(limits = c(0,3), breaks = c(seq(0,3, by=.5)))+
  scale_x_continuous(limits = c(0,50), breaks = c(seq(0,50, by=10)))+
  labs(x="Days post pollination", y="Caryopsis weight (mg)", 
       subtitle="Figure 1 [L. perenne]", 
       title="A) Hyde et al., (1956)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); hyde.seed.wt
##
##
##
##
#####
## Analysis of  Grabe (1956) on smooth bromegrass - Figure 3
#####
## self-starting
grabe.fig3.SS<-getInitial(seed.wt~SSlogis(days.post.anthesis,alpha,xmid,scale),data=group_by(grabe.fig3.dat, days.post.anthesis) %>% summarise(seed.wt=mean(seed.wt)))
grabe.fig3.K_start<-grabe.fig3.SS["alpha"]
grabe.fig3.R_start<-1/grabe.fig3.SS["scale"]
grabe.fig3.N0_start<-grabe.fig3.SS["alpha"]/(exp(grabe.fig3.SS["xmid"]/grabe.fig3.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*days.post.anthesis)/(K+N0*(exp(R*days.post.anthesis)-1)))
gomp_formula<- formula(seed.wt ~ grabe.fig3.N0_start*exp())
#fit the model
grabe.fig3.nls<-nls(log_formula,start=list(K=grabe.fig3.K_start,R=grabe.fig3.R_start,N0=grabe.fig3.N0_start), data = group_by(grabe.fig3.dat, days.post.anthesis) %>% summarise(seed.wt=mean(seed.wt)))
#estimated parameters
summary(grabe.fig3.nls)$coefficients[1]*0.95
grabe.fig3.nls.dat<- data.frame(days.post.anthesis=c(seq(0,30)))
grabe.fig3.nls.dat$seed.wt<- predict(grabe.fig3.nls, list(days.post.anthesis=c(seq(0:30))))
##
grabe.seed.wt<- ggplot()+
  geom_point(data = grabe.fig3.dat, 
             mapping = aes(x=days.post.anthesis, 
                           y=seed.wt),
             shape=1,
             size=2)+
  geom_hline(yintercept = (unlist(summary(grabe.fig3.nls))$coefficients1)*0.95,
             linetype="dashed",
             size=.75)+
  geom_line(data = grabe.fig3.nls.dat, 
            mapping = aes(x=days.post.anthesis, 
                          y=seed.wt))+
  scale_color_manual(values=c("grey20", "grey60"))+
  scale_y_continuous(limits = c(0,650), breaks = c(seq(0,650, by=100)))+
  scale_x_continuous(limits = c(0,32), breaks = c(seq(0,32, by=5)))+
  labs(title="B) Grabe (1956)", subtitle = "Figure 3 [B. inermis]", 
       x="Days post anthesis", y="200-seed weight (mg)")+
  IWG_theme(base_size = 15)+
  annotate("text", x = 5.4, y = 570,
           label = "95% maximum at 15", 
           size=5)+
  theme(panel.background = element_rect(fill = "#f2eecb")); grabe.seed.wt
##
##
##
##
#####
## Analysis of  Anslow (1964) on perennial ryegrass Figure 4
#####
## self-starting
anslow.fig4.SS<-getInitial(seed.wt~SSlogis(day,alpha,xmid,scale),data=filter(anslow.fig4.dat, emergence %in% c("intermediate","late")))
anslow.fig4.K_start<-anslow.fig4.SS["alpha"]
anslow.fig4.R_start<-1/anslow.fig4.SS["scale"]
anslow.fig4.N0_start<-anslow.fig4.SS["alpha"]/(exp(anslow.fig4.SS["xmid"]/anslow.fig4.SS["scale"])+1)
#the formula for the model
log_formula<-formula(seed.wt~K*N0*exp(R*day)/(K+N0*(exp(R*day)-1)))
#fit the model
anslow.fig4.nls<-nls(log_formula,start=list(K=anslow.fig4.K_start,R=anslow.fig4.R_start,N0=anslow.fig4.N0_start), data = filter(anslow.fig4.dat, emergence %in% c("intermediate","late")))
#estimated parameters
summary(anslow.fig4.nls)
anslow.fig4.nls.dat<- data.frame(day=c(seq(0,33)))
anslow.fig4.nls.dat$seed.wt<- predict(anslow.fig4.nls, list(day=c(seq(0:33))))
##
anslow.seed.wt<- ggplot(filter(anslow.fig4.dat, emergence %in% c("intermediate","late")), aes(x=day, y=seed.wt))+
  geom_point(size = 2,
             shape=1)+
  geom_line(data = anslow.fig4.nls.dat, 
            mapping = aes(x=day, 
                          y=seed.wt),
            size=1,
            color="grey40")+
  geom_hline(yintercept = (unlist(summary(anslow.fig4.nls))$coefficients1)*0.95,
             linetype="dashed",
             size=.75)+
  annotate("text", x = 7, y = 230,
           label = "95% maximum at 29d", 
           size=5)+
  scale_x_continuous(limits = c(0,35), breaks = c(seq(0,35, by=10)))+
  scale_y_continuous(limits = c(0,270), breaks = c(seq(0,270, by=50)))+
  labs(title="G) Anslow (1964)", subtitle = "Figure 4 [L. perenne]", 
       x="Days post anthesis", y="100-seed weight (mg)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); anslow.seed.wt
##
##
##
##
#####
## Analysis of Pegler (1976) on perennial ryegrass Figure 4
#####
## Analysis of (1976) Figure 1 panel c and e on perennial ryegrass
## self-starting for cultivar S.23
pegler.fig1.S.23.SS<-getInitial(seed.wt~SSlogis(Days.post.anthesis,alpha,xmid,scale),data=filter(pegler.fig1.dat, cultivar=="S.23"))
pegler.fig1.S.23.K_start<-pegler.fig1.S.23.SS["alpha"]
pegler.fig1.S.23.R_start<-1/pegler.fig1.S.23.SS["scale"]
pegler.fig1.S.23.N0_start<-pegler.fig1.S.23.SS["alpha"]/(exp(pegler.fig1.S.23.SS["xmid"]/pegler.fig1.S.23.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*Days.post.anthesis)/(K+N0*(exp(R*Days.post.anthesis)-1)))
#fit the model
pegler.fig1.S.23.nls<-nls(log_formula,start=list(K=pegler.fig1.S.23.K_start,R=pegler.fig1.S.23.R_start,N0=pegler.fig1.S.23.N0_start), data = filter(pegler.fig1.dat, cultivar=="S.23"))
# self-starting for cultivar S.24
pegler.fig1.S.24.SS<-getInitial(seed.wt~SSlogis(Days.post.anthesis,alpha,xmid,scale),data=filter(pegler.fig1.dat, cultivar=="S.24"))
pegler.fig1.S.24.K_start<-pegler.fig1.S.24.SS["alpha"]
pegler.fig1.S.24.R_start<-1/pegler.fig1.S.24.SS["scale"]
pegler.fig1.S.24.N0_start<-pegler.fig1.S.24.SS["alpha"]/(exp(pegler.fig1.S.24.SS["xmid"]/pegler.fig1.S.24.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*Days.post.anthesis)/(K+N0*(exp(R*Days.post.anthesis)-1)))
#fit the model
pegler.fig1.S.24.nls<-nls(log_formula,start=list(K=pegler.fig1.S.24.K_start,R=pegler.fig1.S.24.R_start,N0=pegler.fig1.S.24.N0_start), data = filter(pegler.fig1.dat, cultivar=="S.24"))
#estimated parameters
pegler.fig1.nls.dat<- data.frame(Days.post.anthesis=rep(seq(10,45),2),
                                 cultivar = rep(c("S.23","S.24"), each=36))
pegler.fig1.nls.dat$seed.wt<- c(predict(pegler.fig1.S.23.nls, newdata = data.frame(Days.post.anthesis=rep(seq(10,45)))),
                                predict(pegler.fig1.S.24.nls, newdata = data.frame(Days.post.anthesis=rep(seq(10,45)))))
## Combinging output from both cultivars
pegler.hline<- data.frame(cultivar= c("S.23", 
                                      "S.24"),
                          seed.wt=  c(summary(pegler.fig1.S.23.nls)$coefficients[1]*0.95,
                                      summary(pegler.fig1.S.24.nls)$coefficients[1]*0.95),
                          max= c("95% max at 36","95% max at 33"),
                          Days.post.anthesis=c(14,16))

## 
pegler.seed.wt<- ggplot(pegler.fig1.dat, aes(x=Days.post.anthesis, y=seed.wt, color=cultivar))+ 
  geom_point(shape=1,
             size=2)+
  geom_line(pegler.fig1.nls.dat, mapping = aes(x=Days.post.anthesis, y=seed.wt))+
  facet_wrap(~cultivar)+
  geom_hline(pegler.hline,
             mapping= aes(yintercept = seed.wt),
             linetype="dashed",
             size=.75)+
  geom_text(pegler.hline,
            mapping= aes(x= Days.post.anthesis+5, y = seed.wt+.2, label=max),
            color="black",
            size=4)+
  scale_color_manual(values=c("grey5", "grey50"))+
  guides(color=F)+
  labs(x="Days post anthesis", y="Weight per 1000 seeds (g)",
       title="A) Pegler (1976) - L. perenne")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); pegler.seed.wt
##
##
##
##
#####
## Analysis of Hebblethwaite and Ahmed (1978)
#####
# self-starting for cultivar Gaol
hebblethwaite.LP.gaol.SS<-getInitial(seed.wt~SSlogis(day.first.anth,alpha,xmid,scale),data=filter(hebblethwaite.fig1.2.dat, species == "L.perenne" & cultivar == "Gaol"))
hebblethwaite.LP.gaol.K_start<-hebblethwaite.LP.gaol.SS["alpha"]
hebblethwaite.LP.gaol.R_start<-1/hebblethwaite.LP.gaol.SS["scale"]
hebblethwaite.LP.gaol.N0_start<-hebblethwaite.LP.gaol.SS["alpha"]/(exp(hebblethwaite.LP.gaol.SS["xmid"]/hebblethwaite.LP.gaol.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day.first.anth)/(K+N0*(exp(R*day.first.anth)-1)))
#fit the model
hebblethwaite.LP.gaol.nls<-nls(log_formula,start=list(K=hebblethwaite.LP.gaol.K_start,
                                                      R=hebblethwaite.LP.gaol.R_start,
                                                      N0=hebblethwaite.LP.gaol.N0_start), 
                               data = filter(hebblethwaite.fig1.2.dat, species == "L.perenne" & cultivar == "Gaol"))
##
# self-starting for cultivar Sprinter
hebblethwaite.LP.sprinter.SS<-getInitial(seed.wt~SSlogis(day.first.anth,alpha,xmid,scale),data=filter(hebblethwaite.fig1.2.dat, species == "L.perenne" & cultivar == "Sprinter"))
hebblethwaite.LP.sprinter.K_start<-hebblethwaite.LP.sprinter.SS["alpha"]
hebblethwaite.LP.sprinter.R_start<-1/hebblethwaite.LP.sprinter.SS["scale"]
hebblethwaite.LP.sprinter.N0_start<-hebblethwaite.LP.sprinter.SS["alpha"]/(exp(hebblethwaite.LP.sprinter.SS["xmid"]/hebblethwaite.LP.sprinter.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day.first.anth)/(K+N0*(exp(R*day.first.anth)-1)))
#fit the model
hebblethwaite.LP.sprinter.nls<-nls(log_formula,start=list(K=hebblethwaite.LP.sprinter.K_start,
                                                          R=hebblethwaite.LP.sprinter.R_start,
                                                          N0=hebblethwaite.LP.sprinter.N0_start), 
                                   data = filter(hebblethwaite.fig1.2.dat, species == "L.perenne" & cultivar == "Sprinter"))
##
# self-starting for cultivar Formosa
hebblethwaite.FR.Formosa.SS<-getInitial(seed.wt~SSlogis(day.first.anth,aFRha,xmid,scale),data=filter(hebblethwaite.fig1.2.dat, species == "F.rubra" & cultivar == "Formosa"))
hebblethwaite.FR.Formosa.K_start<-hebblethwaite.FR.Formosa.SS["aFRha"]
hebblethwaite.FR.Formosa.R_start<-1/hebblethwaite.FR.Formosa.SS["scale"]
hebblethwaite.FR.Formosa.N0_start<-hebblethwaite.FR.Formosa.SS["aFRha"]/(exp(hebblethwaite.FR.Formosa.SS["xmid"]/hebblethwaite.FR.Formosa.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day.first.anth)/(K+N0*(exp(R*day.first.anth)-1)))
#fit the model
hebblethwaite.FR.Formosa.nls<-nls(log_formula,start=list(K=hebblethwaite.FR.Formosa.K_start,
                                                         R=hebblethwaite.FR.Formosa.R_start,
                                                         N0=hebblethwaite.FR.Formosa.N0_start), 
                                  data = filter(hebblethwaite.fig1.2.dat, species == "F.rubra" & cultivar == "Formosa"))
##
# self-starting for cultivar Rapid
hebblethwaite.FR.Rapid.SS<-getInitial(seed.wt~SSlogis(day.first.anth,aFRha,xmid,scale),data=filter(hebblethwaite.fig1.2.dat, species == "F.rubra" & cultivar == "Rapid"))
hebblethwaite.FR.Rapid.K_start<-hebblethwaite.FR.Rapid.SS["aFRha"]
hebblethwaite.FR.Rapid.R_start<-1/hebblethwaite.FR.Rapid.SS["scale"]
hebblethwaite.FR.Rapid.N0_start<-hebblethwaite.FR.Rapid.SS["aFRha"]/(exp(hebblethwaite.FR.Rapid.SS["xmid"]/hebblethwaite.FR.Rapid.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day.first.anth)/(K+N0*(exp(R*day.first.anth)-1)))
#fit the model
hebblethwaite.FR.Rapid.nls<-nls(log_formula,start=list(K=hebblethwaite.FR.Rapid.K_start,
                                                       R=hebblethwaite.FR.Rapid.R_start,
                                                       N0=hebblethwaite.FR.Rapid.N0_start), 
                                data = filter(hebblethwaite.fig1.2.dat, species == "F.rubra" & cultivar == "Rapid"))
##
# self-starting for cultivar S59
hebblethwaite.FR.S59.SS<-getInitial(seed.wt~SSlogis(day.first.anth,aFRha,xmid,scale),data=filter(hebblethwaite.fig1.2.dat, species == "F.rubra" & cultivar == "S59"))
hebblethwaite.FR.S59.K_start<-hebblethwaite.FR.S59.SS["aFRha"]
hebblethwaite.FR.S59.R_start<-1/hebblethwaite.FR.S59.SS["scale"]
hebblethwaite.FR.S59.N0_start<-hebblethwaite.FR.S59.SS["aFRha"]/(exp(hebblethwaite.FR.S59.SS["xmid"]/hebblethwaite.FR.S59.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day.first.anth)/(K+N0*(exp(R*day.first.anth)-1)))
#fit the model
hebblethwaite.FR.S59.nls<-nls(log_formula,start=list(K=hebblethwaite.FR.S59.K_start,
                                                     R=hebblethwaite.FR.S59.R_start,
                                                     N0=hebblethwaite.FR.S59.N0_start), 
                              data = filter(hebblethwaite.fig1.2.dat, species == "F.rubra" & cultivar == "S59"))
##
# combining output from models
hebblethwaite.LP.nls.dat<- data.frame(day.first.anth=rep(seq(0,50),5),
                                      cultivar = rep(c("Gaol","Sprinter","Formosa","Rapid","S59"), each=51),
                                      species  = rep(c("L.perenne","L.perenne","F.rubra","F.rubra","F.rubra"), each=51))
hebblethwaite.LP.nls.dat$seed.wt<- c(predict(hebblethwaite.LP.gaol.nls,     newdata = data.frame(day.first.anth=rep(seq(0,50)))),
                                     predict(hebblethwaite.LP.sprinter.nls, newdata = data.frame(day.first.anth=rep(seq(0,50)))),
                                     predict(hebblethwaite.FR.Formosa.nls, newdata = data.frame(day.first.anth=rep(seq(0,50)))),
                                     predict(hebblethwaite.FR.Rapid.nls, newdata = data.frame(day.first.anth=rep(seq(0,50)))),
                                     predict(hebblethwaite.FR.S59.nls, newdata = data.frame(day.first.anth=rep(seq(0,50)))))
##
hebblethwaite.hline<- data.frame(cultivar= c("Gaol","Sprinter","Formosa","Rapid","S59"),
                                 species=c("L.perenne","L.perenne","F.rubra","F.rubra","F.rubra"),
                                 seed.wt=  c(summary(hebblethwaite.LP.gaol.nls)$coefficients[1]*0.95,
                                             summary(hebblethwaite.LP.sprinter.nls)$coefficients[1]*0.95,
                                             summary(hebblethwaite.FR.Formosa.nls)$coefficients[1]*0.95,
                                             summary(hebblethwaite.FR.Rapid.nls)$coefficients[1]*0.95,
                                             summary(hebblethwaite.FR.S59.nls)$coefficients[1]*0.95),
                                 max= c("95% maximum at 41",
                                        "95% maximum at 49",
                                        "95% maximum at 35",
                                        "95% maximum at 35",
                                        "95% maximum at 42"),
                                 day.first.anth=c(21,25,24,33,28))

hebblethwaite.LP.nls.dat %>% filter(cultivar == "S59")
##
hebblethwaite.seed.wt<- ggplot(hebblethwaite.fig1.2.dat, aes(x=day.first.anth, 
                                                             y=seed.wt))+
  facet_wrap(~species)+
  geom_point(shape=1,
             size=2.5)+
  geom_line(hebblethwaite.LP.nls.dat, mapping = aes(x=day.first.anth, 
                                                    y=seed.wt,
                                                    linetype=cultivar),
            size=1.2)+
  labs(x="Days from first anthesis", y="Weight per 1000 seeds (g)",
       title="H) Hebblethwaite and Ahmed (1978)")+
  geom_text(hebblethwaite.hline,
            mapping= aes(x= day.first.anth, y = seed.wt+c(-.1,-.1,-.65,-.5,-.55), label=max),
            color="black",
            size=4)+
  geom_hline(hebblethwaite.hline,
             mapping= aes(yintercept = seed.wt),
             linetype="dashed",
             size=.75)+
  IWG_theme(base_size = 15)+
  scale_x_continuous(limits = c(10,45))+
  scale_y_continuous(limits = c(0,1.8))+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white"),
        legend.position = "right"); hebblethwaite.seed.wt
##
##
##
##
#####
## Analysis of Hill and Watkin (1975)
#####
##
#subtracting the weight of the lemma and palea***
#day 0 is assumed to contain only lemma and palea***
hill.seed.wt.fig1.dat$caryopsis.wt<- hill.seed.wt.fig1.dat$seed.wt-hill.seed.wt.fig1.dat$lemma.palea
##
# self-starting for species B.unioloides
hill.PG.SS<-getInitial(caryopsis.wt~SSlogis(day.mid.anthesis,alpha,xmid,scale),data=filter(hill.seed.wt.fig1.dat, species=="B.unioloides"))
hill.PG.K_start<-hill.PG.SS["alpha"]
hill.PG.R_start<-1/hill.PG.SS["scale"]
hill.PG.N0_start<-hill.PG.SS["alpha"]/(exp(hill.PG.SS["xmid"]/hill.PG.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(caryopsis.wt~K*N0*exp(R*day.mid.anthesis)/(K+N0*(exp(R*day.mid.anthesis)-1)))
#fit the model
hill.PG.nls<-nls(log_formula,start=list(K=hill.PG.K_start,R=hill.PG.R_start,N0=hill.PG.N0_start), data = filter(hill.seed.wt.fig1.dat, species=="B.unioloides"))
##
# self-starting for species L.perenne
hill.PR.SS<-getInitial(caryopsis.wt~SSlogis(day.mid.anthesis,alpha,xmid,scale),data=filter(hill.seed.wt.fig1.dat, species=="L.perenne"))
hill.PR.K_start<-hill.PR.SS["alpha"]
hill.PR.R_start<-1/hill.PR.SS["scale"]
hill.PR.N0_start<-hill.PR.SS["alpha"]/(exp(hill.PR.SS["xmid"]/hill.PR.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(caryopsis.wt~K*N0*exp(R*day.mid.anthesis)/(K+N0*(exp(R*day.mid.anthesis)-1)))
#fit the model
hill.PR.nls<-nls(log_formula,start=list(K=hill.PR.K_start,R=hill.PR.R_start,N0=hill.PR.N0_start), data = filter(hill.seed.wt.fig1.dat, species=="L.perenne"))
##
hill.nls.dat<- data.frame(day.mid.anthesis=rep(seq(0,55),2),
                          species = rep(c("B.unioloides","L.perenne"), each=56))
hill.nls.dat$caryopsis.wt<- c(predict(hill.PG.nls, newdata = data.frame(day.mid.anthesis=rep(seq(0,55)))),
                              predict(hill.PR.nls, newdata = data.frame(day.mid.anthesis=rep(seq(0,55))))) #weigth minus lemma and palea
hill.nls.dat$seed.wt<- if_else(hill.nls.dat$species == "B.unioloides", 
                               hill.nls.dat$caryopsis.wt+5.079,  #weigth WITH lemma and palea for B. unioloides***
                               hill.nls.dat$caryopsis.wt+0.401)  #weigth WITH lemma and palea for L. perenne***

##
hill.hline<- data.frame(species= c("B.unioloides", 
                                   "L.perenne"),
                        seed.wt=  c((summary(hill.PG.nls)$coefficients[1]*0.95)+5.079,
                                    (summary(hill.PR.nls)$coefficients[1]*0.95)+0.401),
                        max= c("95% maximum at 46d","95% maximum at 35d"),
                        lemma.palea=c(5.079,0.401),
                        day.mid.anthesis=c(19,20))
##
hill.seed.wt<- ggplot(hill.seed.wt.fig1.dat, aes(x=day.mid.anthesis, y=seed.wt))+ 
  geom_point(shape=1,
             size=2)+
  geom_line(hill.nls.dat, mapping = aes(x=day.mid.anthesis, y=seed.wt))+
  facet_wrap(~species)+
  geom_hline(hill.hline,
             mapping= aes(yintercept = lemma.palea),
             linetype="dotted",
             size=.75)+
  geom_hline(hill.hline,
             mapping= aes(yintercept = seed.wt),
             linetype="dashed",
             size=.75)+
  scale_x_continuous(limits = c(0,45), breaks = c(seq(0,45, by=10)))+
  geom_text(hill.hline,
            mapping= aes(x= day.mid.anthesis, y = seed.wt+1.5, label=max),
            color="black",
            size=5)+
  scale_color_manual(values=c("grey5", "grey50"))+
  guides(color=F)+
  labs(x="Days post peal anthesis", y="Individual seed weight (mg)", 
       title="E) Hill and Watkin (1975)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); hill.seed.wt
##
##
##
##
#####
## Analysis of Nellis and Reese (1968)
#####
# self-starting
nellist.SS<-getInitial(seed.wt~SSlogis(day,alpha,xmid,scale),data=nellist.fig1.dat)
nellist.K_start<-nellist.SS["alpha"]
nellist.R_start<-1/nellist.SS["scale"]
nellist.N0_start<-nellist.SS["alpha"]/(exp(nellist.SS["xmid"]/nellist.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day)/(K+N0*(exp(R*day)-1)))
#fit the model
nellist.PG.nls<-nls(log_formula,start=list(K=nellist.K_start,R=nellist.R_start,N0=nellist.N0_start), data = nellist.fig1.dat)
##
nellist.nls.dat<- data.frame(day=rep(seq(0,15),1))
nellist.nls.dat$seed.wt<- c(predict(nellist.PG.nls, newdata = data.frame(day=rep(seq(0,15)))))
nellist.nls.dat$date<- seq(7.08,7.23, by=0.01)
##
nellist.seed.wt<- ggplot(nellist.fig1.dat, aes(x=date, y=seed.wt))+ 
  geom_point(shape=1,
             size=2)+
  geom_line(nellist.nls.dat, mapping = aes(x=date, y=seed.wt))+
  scale_color_manual(values=c("grey5", "grey50"))+
  guides(color=F)+
  labs(x="Date", y="Weight per 1000 seeds (g)", 
       subtitle="Table 6 [L. perenne]", 
       title="I) Nellis and Reese (1968)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); nellist.seed.wt
##
##
##
##
#####
## Analysis of Williams (1972)
#####
# self starting
williams.SS<-getInitial(seed.wt~SSlogis(day.post.anthesis,alpha,xmid,scale),data=williams.dat)
williams.K_start<-williams.SS["alpha"]
williams.R_start<-1/williams.SS["scale"]
williams.N0_start<-williams.SS["alpha"]/(exp(williams.SS["xmid"]/williams.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*day.post.anthesis)/(K+N0*(exp(R*day.post.anthesis)-1)))
#fit the model
williams.PG.nls<-nls(log_formula,start=list(K=williams.K_start,R=williams.R_start,N0=williams.N0_start), data = williams.dat)
##
williams.nls.dat<- data.frame(day.post.anthesis=rep(seq(10,40),1))
williams.nls.dat$seed.wt<- c(predict(williams.PG.nls, newdata = data.frame(day.post.anthesis=seq(10,40))))
##
##
williams.hline<- data.frame(seed.wt=  c(summary(williams.PG.nls)$coefficients[1]*0.95),
                            max= c("95% maximum at 35d"),
                            day.post.anthesis=c(21))
##
williams.seed.wt<- ggplot(williams.dat, aes(x=day.post.anthesis, y=seed.wt))+ 
  geom_point(shape=1,
             size=2)+
  geom_line(williams.nls.dat, mapping = aes(x=day.post.anthesis, y=seed.wt))+
  geom_hline(williams.hline,
             mapping= aes(yintercept = seed.wt),
             linetype="dotted",
             size=.75)+
  geom_text(williams.hline,
            mapping= aes(x= day.post.anthesis, y = seed.wt-.3, label=max),
            color="black",
            size=5)+
  scale_color_manual(values=c("grey5", "grey50"))+
  guides(color=F)+
  labs(x="Days post max anthesis", y="Weight per 1000 seeds (g)", 
       subtitle="Figure 2 [L. perenne]", 
       title="F) Williams (1972)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); williams.seed.wt
##
##
##
##
#####
## Analysis of Roberts (1971)
#####
# self-starting
roberts.SS<-getInitial(seed.wt~SSlogis(days.post.ear,alpha,xmid,scale),data=filter(roberts.fig1.dat, cultivar !="Sabalin"))
roberts.K_start<-roberts.SS["alpha"]
roberts.R_start<-1/roberts.SS["scale"]
roberts.N0_start<-roberts.SS["alpha"]/(exp(roberts.SS["xmid"]/roberts.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*days.post.ear)/(K+N0*(exp(R*days.post.ear)-1)))
#fit the model
roberts.nls<-nls(log_formula,start=list(K=roberts.K_start,R=roberts.R_start,N0=roberts.N0_start), data = filter(roberts.fig1.dat, cultivar !="Sabalin"))
##
roberts.nls.dat<- data.frame(days.post.ear=rep(seq(25,80),1))
roberts.nls.dat$seed.wt<- c(predict(roberts.nls, newdata = data.frame(days.post.ear=seq(25,80))))
##
##
roberts.hline<- data.frame(seed.wt=  c(summary(roberts.nls)$coefficients[1]*0.95),
                           max= c("95% maximum at 65d"),
                           days.post.ear=c(33))
##
roberts.seed.wt<- ggplot(filter(roberts.fig1.dat, cultivar !="Sabalin"), aes(x=days.post.ear, y=seed.wt))+
  geom_point(shape=1,
             size=2)+
  geom_line(roberts.nls.dat, mapping = aes(x=days.post.ear, y=seed.wt))+
  geom_hline(roberts.hline,
             mapping= aes(yintercept = seed.wt),
             linetype="dotted",
             size=.75)+
  geom_text(roberts.hline,
            mapping= aes(x= days.post.ear, y = seed.wt-.3, label=max),
            color="black",
            size=5)+
  scale_color_manual(values=c("grey5", "grey50"))+
  scale_x_continuous(limits = c(0,80), breaks = c(seq(0,80, by=20)))+
  labs(x="Days post inflorescence emergence", y="Weight per 1000 seeds (g)", 
       subtitle="Figure 1 [L. perenne]", 
       title="C) Roberts (1971)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); roberts.seed.wt
##
##
##
##
#####
#####
## Analysis of Berdahl (1998)
#####
# self-starting
berdahl.SS<-getInitial(seed.wt~SSlogis(GDD,alpha,xmid,scale),data=berdahl.fig1.dat)
berdahl.K_start<-berdahl.SS["alpha"]
berdahl.R_start<-1/berdahl.SS["scale"]
berdahl.N0_start<-berdahl.SS["alpha"]/(exp(berdahl.SS["xmid"]/berdahl.SS["scale"])+1)
#the formula for the logistic growth model
log_formula<-formula(seed.wt~K*N0*exp(R*GDD)/(K+N0*(exp(R*GDD)-1)))
#fit the model
berdahl.nls<-nls(log_formula,start=list(K=berdahl.K_start,R=berdahl.R_start,N0=berdahl.N0_start), data = berdahl.fig1.dat)
##
berdahl.nls.dat<- data.frame(GDD=rep(seq(0,1050),1))
berdahl.nls.dat$seed.wt<- c(predict(berdahl.nls, newdata = data.frame(GDD=seq(0,1050))))
##
##
berdahl.hline<- data.frame(seed.wt=  c(summary(berdahl.nls)$coefficients[1]*0.95),
                           max= c("95% maximum at 873 GDD"),
                           GDD=c(330))
##
berdahl.seed.wt<- ggplot(berdahl.fig1.dat, aes(x=GDD, y=seed.wt))+
  geom_point(shape=1,
             size=2)+
  geom_line(berdahl.nls.dat, mapping = aes(x=GDD, y=seed.wt))+
  geom_hline(berdahl.hline,
             mapping= aes(yintercept = seed.wt),
             linetype="dotted",
             size=.75)+
  geom_text(berdahl.hline,
            mapping= aes(x= GDD, y = seed.wt-.3, label=max),
            color="black",
            size=5)+
  labs(x="GDD", y="Seed weight (mg)", 
       subtitle="Figure 2 [T. intermedium]", 
       title="D) Berdahl (1998)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); berdahl.seed.wt
##
##
##
##
#####
## Analysis of Andrade (1994)
## NOTE: this is not included in the analysis of Experiment 1 
## This is referenced many times and is included as Supplemental Fig. 1
#####
log_formula<-formula(seed.wt~K*N0*exp(R*GDD)/(K+N0*(exp(R*GDD)-1))| year_cultivar)
##
andrade.seed.wt.SS<-getInitial(seed.wt~SSlogis(GDD,alpha,xmid,scale),data=andrade.dat)
andrade.seed.wt.K_start<-andrade.seed.wt.SS["alpha"]
andrade.seed.wt.R_start<-1/andrade.seed.wt.SS["scale"]
andrade.seed.wt.N0_start<-andrade.seed.wt.SS["alpha"]/(exp(andrade.seed.wt.SS["xmid"]/andrade.seed.wt.SS["scale"])+1)
#the formula for the model
#fit the model
MN.seed.wt.nls<-nlsList(log_formula, 
                        start=list(K=andrade.seed.wt.K_start,R=andrade.seed.wt.R_start,N0=andrade.seed.wt.N0_start), 
                        data = andrade.dat, na.action=na.omit)
##
andrade.line<- data.frame(GDD= rep(seq(100,500, length.out = 100),5),
                          year_cultivar=rep(c(unique(andrade.dat$year_cultivar)),each=1000),
                          seed.wt=predict(MN.seed.wt.nls, newdata = data.frame(GDD= rep(seq(100,500, length.out = 100),5),
                                                                               year_cultivar=rep(c(unique(andrade.dat$year_cultivar)),each=1000))))
##
andrade.max<- data.frame(GDD=rep(150, 5),
                         phase.II= c(313,297,308,299,221),
                         max.yld=c(308,360,345,354,306),
                         phase.des=c("","Blue = Phase II","","",""),
                         yld.des=  c("","Red = Max Yield","","",""),
                         year_cultivar=rep(c("1990_Bonanza","1991_Chesapeake","1992_Bonanza","1992_Chesapeake","1992_Emperor"),each=1),
                         seed.wt=summary(MN.seed.wt.nls)[["coefficients"]][,,"K"][c(1,2,3,4,5)]*.95,
                         label=paste(rep("95% max=",5),round(summary(MN.seed.wt.nls)[["coefficients"]][,,"K"][c(1,2,3,4,5)]*.95,2), sep=" "))
##
supplemental.fig.2<- ggplot(andrade.dat, aes(x=GDD, y=seed.wt))+
  geom_point(size=2,
             shape=1)+
  facet_wrap(~factor(year_cultivar, levels=c("1991_Chesapeake","1992_Chesapeake","1990_Bonanza","1992_Bonanza","1992_Emperor")), ncol=2)+
  geom_line(andrade.line, mapping = aes(x=GDD, y=seed.wt))+
  geom_text(andrade.max, mapping = aes(x=GDD+50, y=seed.wt+-1.4,
                                       label=phase.des),
            size=5)+
  geom_text(andrade.max, mapping = aes(x=GDD+50, y=seed.wt-1.7,
                                       label=yld.des),
            size=5)+
  geom_text(andrade.max, mapping = aes(x=GDD+50, y=seed.wt+.3,
                                       label=label),
            size=5)+
  geom_vline(andrade.max, mapping = aes(xintercept=phase.II),
             color="dodgerblue4")+
  geom_vline(andrade.max, mapping = aes(xintercept=max.yld),
             color="red4")+
  geom_hline(andrade.max, mapping = aes(yintercept=seed.wt))+
  labs(x="GDD post mid anthesis", y="Seed weight (mg)", 
       subtitle="[F. arundinacea]", 
       title="D) Andrade et al. (1994)")+
  IWG_theme(base_size = 22)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white"));supplemental.fig.2
#####
##
##
##
##
## Plotting all seed weight figures
plot_grid(hyde.seed.wt, grabe.seed.wt, roberts.seed.wt, berdahl.seed.wt, hill.seed.wt, williams.seed.wt, anslow.seed.wt,   
          hebblethwaite.seed.wt, nellist.seed.wt, pegler.seed.wt,
          ncol = 2) #1250 x 1600
#################################
######################
###########
###########
###########
######################
#################################
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Modeling seed moisture phases
###########
######################
#################################
## Modeling moisture content phases in the spike, with the assumption that discrete phases may exist
##
##
##
#####
## Analysis of Hyde et al. (1959) data on perennial ryegrass
#####
## Fitting piecewise regression
hyde.wtr.mass.fm<- glm(moist.wt ~ days.post.pollination, data = hyde.fig1.dat)
hyde.wtr.perc.fm<- glm(perc.moisture2 ~ days.post.pollination, data = hyde.fig1.dat)
##
davies.test(hyde.wtr.mass.fm)
davies.test(hyde.wtr.perc.fm)
#break points detected***
## Fitting the segmented models
hyde.wtr.mass.seg <- segmented(hyde.wtr.mass.fm, 
                               seg.Z = ~ days.post.pollination, 
                               psi = list(days.post.pollination = c(12,
                                                                    28,
                                                                    35))) 
hyde.wtr.perc.seg <- segmented(hyde.wtr.perc.fm, 
                              seg.Z = ~ days.post.pollination, 
                              psi = list(days.post.pollination = c(9,
                                                                   32,
                                                                   36)))
hyde.wtr.perc.seg$psi
slope(hyde.wtr.mass.seg)
intercept(hyde.wtr.mass.seg)
summary(hyde.wtr.mass.seg)
## Predicting across data frames
hyde.wtr.mass.pred<- data.frame(days.post.pollination= seq(0,45, length.out = 200))
hyde.wtr.mass.pred$moist.wt<- predict.segmented(hyde.wtr.mass.seg, newdata = data.frame(days.post.pollination= seq(0,45, length.out = 200)))
hyde.wtr.perc.pred<- data.frame(days.post.pollination= seq(0,45, length.out = 200))
hyde.wtr.perc.pred$perc.moisture2<- predict.segmented(hyde.wtr.perc.seg, newdata = data.frame(days.post.pollination= seq(0,45, length.out = 200)))
## Creating a data frame with breakpoints
hyde.wtr.mass.phase<- data.frame(days.post.pollination = c(hyde.wtr.mass.seg$psi[1,2],
                                                           hyde.wtr.mass.seg$psi[2,2],
                                                           hyde.wtr.mass.seg$psi[3,2])) #isolating each breakpoint***
hyde.wtr.perc.phase<- data.frame(days.post.pollination = c(hyde.wtr.perc.seg$psi[1,2],
                                                           hyde.wtr.perc.seg$psi[2,2],
                                                           hyde.wtr.perc.seg$psi[3,2])) 
## Creating a data frame with slopes and significance
hyde.wtr.mass.slope<- data.frame(days.post.pollination =c(2,20,31,40),
                                 moist.wt = c(1.55, 2.55, 2.1, 1.55),
                                 slope= c(slope(hyde.wtr.mass.seg)[[1]][1:4,1]),
                                 sig= c("***", "**", "***", "NS")) #NOTE: hard coding this in***
hyde.wtr.perc.slope<- data.frame(days.post.pollination =c(2,20,35,43),
                                 perc.moisture2 = c(82, 63, 40, 25),
                                 slope= c(slope(hyde.wtr.perc.seg)[[1]][1:4,1]),
                                 sig= c("NS", "***", "**", "NS"))
## Plotting water mass in Hyde et al., 1959
supplemental.fig.1<- ggplot(hyde.fig1.dat, aes(x=days.post.pollination, y=moist.wt))+
  geom_point(size=2,
             shape=1)+
  
  geom_line(data = hyde.wtr.mass.pred,
            mapping = aes(x=days.post.pollination, y=moist.wt), #adding predicted line***
            linetype="dashed",
            color="grey5",
            size=1)+
  geom_segment(data = hyde.wtr.mass.phase, mapping = aes(x = days.post.pollination, 
                                                         xend = days.post.pollination,  
                                                         y = c(3,3,3), yend = predict(hyde.wtr.mass.seg, data.frame(days.post.pollination=c(hyde.wtr.mass.seg$psi[1:3,2])))),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_segment(aes(x = 29, 
                   xend = 29,  
                   y = 0, 
                   yend = 1.85),
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_text(data = hyde.wtr.mass.slope, mapping = aes(x = days.post.pollination, y = moist.wt, 
                                                 label = round(slope, 2)),
            size=5)+
  geom_text(data = hyde.wtr.mass.slope, mapping = aes(x = days.post.pollination + 3, y = moist.wt, 
                                                 label = sig),
            size=5)+
  
  scale_y_continuous(limits = c(0,3), breaks = c(seq(0,3, by=.5)))+
  labs(x="Days post pollination", y="Mass of seed water (mg seed)", title = "A) Hyde et al. (1959) - L. perenne")+
  IWG_theme(base_size = 18)+
  theme(panel.background = element_rect(fill = "#f2eecb")); supplemental.fig.1
##
##
##
## Plotting water content (%) in Hyde et al., 1959
hyde.moist<- ggplot(hyde.fig1.dat, aes(x=days.post.pollination, y=perc.moisture2*100))+
  geom_point(size=1.5,
             shape=1)+
  
  geom_line(data = hyde.wtr.perc.pred,
            mapping = aes(x=days.post.pollination, y=perc.moisture2*100), #adding predicted line***
            linetype="dashed",
            color="grey5",
            size=1)+
  
  geom_segment(data = hyde.wtr.perc.phase, mapping = aes(x = days.post.pollination, 
                                                         xend = days.post.pollination,  
                                                         y = c(85,85,85), yend = predict(hyde.wtr.perc.seg, 
                                                                                         data.frame(days.post.pollination=c(hyde.wtr.perc.seg$psi[1:3,2])))*100),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_segment(aes(x = 29, 
                   xend = 29,  
                   y = 0, 
                   yend = predict(hyde.wtr.perc.seg, 
                                  data.frame(days.post.pollination=c(29)))*100),
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_text(data = hyde.wtr.perc.slope, mapping = aes(x = days.post.pollination, y = perc.moisture2, 
                                                      label = round(slope, 2)),
            size=4)+
  
  geom_text(data = hyde.wtr.perc.slope, mapping = aes(x = days.post.pollination + 4, y = perc.moisture2, 
                                                      label = sig),
            size=4)+
  
  scale_y_continuous(limits = c(0,85), breaks = c(seq(0,85, by=20)))+
  labs(x="Days post pollination", y="Seed moisture (%)", 
       title = "A) Hyde et al. (1959) - L. perenne")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); hyde.moist
##
##
##
##
#####
## Analysis of Grabe (1956) data on smooth bromegrass
#####
##
## Making a model
Grabe.perc.fm<- glm(perc.moisture ~ days.post.anthesis, data = grabe.fig4.dat)
##
davies.test(Grabe.perc.fm)
#breakpoint detected
## Applying breakpoint analysis
Grabe.perc.fm.seg <- segmented(Grabe.perc.fm, 
                               seg.Z = ~ days.post.anthesis, 
                               psi = list(days.post.anthesis = c(5,16,23)),
                               control=seg.control(it.max=100)) #guessing what the break points are***
Grabe.perc.fm.seg$psi
slope(Grabe.perc.fm.seg)
## Predicting across a data frame
Grabe.perc.line<- data.frame(days.post.anthesis = seq(1,28, length.out = 100))
Grabe.perc.line$perc.moisture<- predict(Grabe.perc.fm.seg, newdata = data.frame(days.post.anthesis= seq(1,28, length.out = 100)))                            

Grabe.seg.slope<- data.frame(days.post.anthesis =c(2,11,21,26),
                             perc.moisture = c(73, 68, 47,22),
                             slope= c(slope(Grabe.perc.fm.seg)[[1]][1:4,1]),
                             sig= c("NS", "**", "***", "NS")) #NOTE: I am hard coding this in*******

Grabe.seg.phase<- data.frame(days.post.anthesis = c(Grabe.perc.fm.seg$psi[1,2],
                                                    Grabe.perc.fm.seg$psi[2,2],
                                                    Grabe.perc.fm.seg$psi[3,2])) #isolating each breakpoint***


grabe.moist<-ggplot(grabe.fig4.dat, aes(x=days.post.anthesis, y=perc.moisture))+
  geom_point(size=1.5,
             shape=1)+
  geom_line(data = Grabe.perc.line, mapping = aes(x=days.post.anthesis, y=perc.moisture), color="grey40", size=1)+
  scale_y_continuous(limits = c(0,85), breaks = c(seq(0,85, by=20)))+
  scale_x_continuous(limits = c(0,30), breaks = c(seq(0,30, by=5)))+
  geom_text(data = Grabe.seg.slope, mapping = aes(x = days.post.anthesis, y = perc.moisture, 
                                                  label = round(slope,2)),
            size=4)+
  geom_text(data = Grabe.seg.slope, mapping = aes(x = days.post.anthesis + 2, y = perc.moisture, 
                                                  label = sig),
            size=4)+
  geom_segment(data = Grabe.seg.phase, mapping = aes(x = days.post.anthesis, 
                                                         xend = days.post.anthesis,  
                                                         y = c(85,85,85), yend = predict(Grabe.perc.fm.seg, 
                                                                                         data.frame(days.post.anthesis=c(Grabe.perc.fm.seg$psi[1:3,2])))),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_segment(aes(x = 15, 
                   xend = 15,  
                   y = 0, 
                   yend = predict(Grabe.perc.fm.seg, 
                                  data.frame(days.post.anthesis=c(15)))),
               arrow = arrow(length = unit(0.02, "npc")))+
  labs(x="Days post anthesis", y="Seed moisture (%)", 
       title = "B) Grabe (1959) - B. inermis")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); grabe.moist
##
##
##
##
#####
## Analysis of Berdahl (1998) data on intermediate wheatgrass
#####
berdahl.perc.fm<- glm(perc.moisture ~ GDD, data = berdahl.fig1.dat)
##
davies.test(berdahl.perc.fm)
#breakpoint detected
##
berdahl.perc.fm.seg <- segmented(berdahl.perc.fm, 
                                 seg.Z = ~ GDD, 
                                 psi = list(GDD = c(550)),
                                 control=seg.control(it.max=100)) #guessing what the break points are***
##
berdahl.perc.line<- data.frame(GDD = seq(150,950, length.out = 500))
berdahl.perc.line$perc.moisture<- predict(berdahl.perc.fm.seg, newdata = data.frame(GDD= seq(150,950, length.out = 500)))  

berdahl.seg.slope<- data.frame(GDD =c(300,850),
                               perc.moisture = c(400,450),
                               slope= c(slope(berdahl.perc.fm.seg)[[1]][1:2,1]/10),
                               sig= c("***", "**")) #NOTE: I am hard coding this in*******
##
berdahl.seg.phase<- data.frame(GDD = c(berdahl.perc.fm.seg$psi[1,2])) #isolating each breakpoint***
##
berdahl.moist<- ggplot(berdahl.fig1.dat, aes(x=GDD, y=perc.moisture/10))+
  geom_point(shape=1,
             size=2)+
  geom_line(data = berdahl.perc.line, mapping = aes(x=GDD, y=perc.moisture/10), color="grey40", size=1)+
  geom_text(data = berdahl.seg.slope, mapping = aes(x = GDD, y = perc.moisture/10, 
                                                    label = round(slope,2)),
            size=4)+
  geom_text(data = berdahl.seg.slope, mapping = aes(x = GDD + 78, y = perc.moisture/10, 
                                                    label = sig),
            size=4)+
  geom_segment(data = berdahl.seg.phase, mapping = aes(x = GDD, 
                                                     xend = GDD,  
                                                     y = c(65), yend = predict(berdahl.perc.fm.seg, 
                                                                                     data.frame(GDD=c(berdahl.perc.fm.seg$psi[1,2])))/10),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_segment(aes(x = 870, 
                   xend = 870,  
                   y = 0, 
                   yend = predict(berdahl.perc.fm.seg, 
                                  data.frame(GDD=c(870)))/10),
               arrow = arrow(length = unit(0.02, "npc")))+
  scale_x_continuous(limits = c(0,1000), breaks = c(seq(0,1000, by=250)))+
  scale_y_continuous(limits = c(0,65), breaks = c(seq(0,65, by=10)))+
  labs(x="GDD post anthesis", y="Seed moisture (%)", title = "D) Berdahl (1998) - T. intermedium")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); berdahl.moist
##
##
##
##
#####
## Analysis of Roberts (1971) on perennial ryegrass
#####
## Making a model
roberts.perc.fm<- glm(moisture ~ days.post.ear, data = filter(roberts.fig1.dat, cultivar !="Sabalin"))
##
davies.test(roberts.perc.fm)
#breakpoint detected***
##
## Applying breakpoint analysis
roberts.perc.fm.seg <- segmented(roberts.perc.fm, 
                               seg.Z = ~ days.post.ear, 
                               psi = list(days.post.ear = c(60)),
                               control=seg.control(it.max=100)) #guessing what the break points are***
roberts.perc.fm.seg$psi
## Predicting across a data frame
roberts.perc.line<- data.frame(days.post.ear = seq(35,78, length.out = 150))
roberts.perc.line$moisture<- predict(roberts.perc.fm.seg, newdata = data.frame(days.post.ear= seq(35,78, length.out = 150)))                            

roberts.seg.slope<- data.frame(days.post.ear =c(45,72),
                               moisture = c(65, 50),
                               slope= c(slope(roberts.perc.fm.seg)[[1]][1:2,1]),
                               sig= c("**", "***")) #NOTE: I am hard coding this in*******

roberts.seg.phase<- data.frame(days.post.ear = c(roberts.perc.fm.seg$psi[1,2])) #isolating each breakpoint***

roberts.moist<- ggplot(filter(roberts.fig1.dat, cultivar !="Sabalin"), aes(x=days.post.ear, y=moisture))+
  geom_point(size=1.5,
             shape=1)+
  geom_line(data = roberts.perc.line, mapping = aes(x=days.post.ear, y=moisture), color="grey40", size=1)+
  scale_y_continuous(limits = c(0,80), breaks = c(seq(0,80, by=20)))+
  geom_text(data = roberts.seg.slope, mapping = aes(x = days.post.ear, y = moisture, 
                                                  label = round(slope, 2)),
            size=4)+
  geom_text(data = roberts.seg.slope, mapping = aes(x = days.post.ear + 3, y = moisture, 
                                                  label = sig),
            size=4)+
  geom_segment(data = roberts.seg.phase, mapping = aes(x = days.post.ear, 
                                                       xend = days.post.ear,  
                                                       y = c(80), yend = predict(roberts.perc.fm.seg, 
                                                                                 data.frame(days.post.ear=c(roberts.perc.fm.seg$psi[1,2])))),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.02, "npc")))+
  geom_segment(aes(x = 66, 
                   xend = 66,  
                   y = 0, 
                   yend = predict(roberts.perc.fm.seg, 
                                  data.frame(days.post.ear=c(66)))),
               arrow = arrow(length = unit(0.02, "npc")))+
  labs(x="Days post inflorescence emergence", y="Seed moisture (%)", 
       title = "C) Roberts (1971) - L. perenne")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); roberts.moist
#NOTE that seed sampling began for germ 30-44 days post emergence
##
##
##
##
## Analysis of Hebblethwaite and Ahmed (1978) on perennial ryegrass and fine fescue
#####
## Making a model
hebblethwaite.PR.perc.fm<- glm(moisture ~ day.first.anth, data = filter(hebblethwaite.fig1.2.dat, species=="L.perenne"))
hebblethwaite.FF.perc.fm<- glm(moisture ~ day.first.anth, data = filter(hebblethwaite.fig1.2.dat, species=="F.rubra"))
##
davies.test(hebblethwaite.PR.perc.fm)
summary(hebblethwaite.PR.perc.fm)
#no breakpoint***
davies.test(hebblethwaite.FF.perc.fm)
summary(hebblethwaite.FF.perc.fm)
#no breakpoint***
##
hebblethwaite.slope<- data.frame(species=c("F.rubra","L.perenne"),
                                 slope=c("-1.56  ***"," -2.47  ***"),
                                 day.first.anth=c(22,28),
                                 cultivar=c("S59","S59"),
                                 moisture=c(32,32))
##
hebblethwaite.moist<- ggplot()+
  facet_wrap(~species)+
  geom_point(hebblethwaite.fig1.2.dat, mapping= aes(x=day.first.anth,
                                           y=moisture,
                                           linetype=cultivar),
             shape=1,
             size=2)+
  geom_smooth(hebblethwaite.fig1.2.dat, mapping=aes(x=day.first.anth,
                                            y=moisture,
                                            linetype=cultivar),
              method = 'lm',
              se=F,
              color="black")+
  geom_text(hebblethwaite.slope, mapping = aes(x=day.first.anth,
                                               y=moisture,
                                               label=slope,
                                               size=5))+
  scale_x_continuous(limits = c(15,45), breaks = c(seq(15,45, by=5)))+
  scale_y_continuous(limits = c(0,65), breaks = c(seq(0,65, by=20)))+
  IWG_theme(base_size = 15)+
  labs(x="Days post first anthesis", y="Seed moisture (%)", 
       title = "H) Hebblethwaite and Ahmed (1978)")+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        legend.position = "right",
        strip.background = element_rect(fill="white")); hebblethwaite.moist
##
##
##
##
#####
## Analysis of Nellist and Rees (1968) on perennial ryegrass 
#####
## Making a model
nellist.perc.fm<- glm(moisture.adj ~ date, data = nellist.fig1.dat)
##
davies.test(nellist.perc.fm)
summary(nellist.perc.fm)
summary(nellist.perc.fm)[['coefficients']][2,1]/100
#no breakpoint***
##
neelist.slope= data.frame(date=c(7.10),
                          moisture.adj=c(37),
                          slope=c("-1.72  ***"))

nellist.moist<- ggplot(nellist.fig1.dat, aes(x=date, y=moisture.adj))+
  geom_point(shape=1,
             size=2)+
  IWG_theme(base_size = 15)+
  geom_smooth(method = 'lm',
              se=F,
              color="black")+
  geom_text(data = neelist.slope, mapping = aes(x = date, y = moisture.adj, 
                                                    label = slope),
            size=5)+
  scale_y_continuous(limits = c(20,60), breaks = c(seq(20,60, by=20)))+
  labs(x="Calender date", y="Seed moisture (%)", 
       subtitle = "Figure 1 [L. perenne]",title = "I) Nellist and Rees (1968)")+
  theme(panel.background = element_rect(fill = "#f2eecb")); nellist.moist
##
##
##
##
#####
## Anslow 1964
#####
## Making a model
anslow.perc.fm<- glm(adj.moisture ~ day.max.anthesis, data = filter(anslow.fig1.dat, emergence %in% (c("intermediate","late"))))
summary(anslow.perc.fm)
summary(anslow.perc.fm)[['coefficients']][2,1]
##
davies.test(anslow.perc.fm)
#insignificant breakpoint***
##
predict(anslow.perc.fm, newdata = data.frame(day.max.anthesis=23))
##
anslow.seg.slope<- data.frame(day.max.anthesis =c(18),
                              adj.moisture = c(40),
                               slope= c("-0.87  ***")) #NOTE: I am hard coding this in*******
##
anslow.moist<- ggplot(filter(anslow.fig1.dat, emergence %in% c("intermediate","late")), aes(x=day.max.anthesis, y=adj.moisture))+
  geom_point(size=2,
             shape=1)+
  geom_smooth(method = 'lm',
              se=F,
              color="black")+
  geom_text(anslow.seg.slope, mapping = aes(x=day.max.anthesis, y=adj.moisture, label=slope),
            size=5)+
  scale_y_continuous(limits = c(0,70), breaks = c(seq(0,70, by=20)))+
  labs(x="Days post maximum anthesis", y="Seed moisture (%)", 
       subtitle = "Figure 1 [L. perenne]",title = "G) Anslow (1964)")+
  IWG_theme(base_size = 16)+
  theme(panel.background = element_rect(fill = "#f2eecb")); anslow.moist
##
##
##
##
#####
## Hill and Watkin 1975 b
#####
## Making models
hill.PR.fm<- glm(seed.moisture ~ day.mid.anthesis, data = filter(hill.moist.fig1.dat, species == "L.perenne"))
hill.PG.fm<- glm(seed.moisture ~ day.mid.anthesis, data = filter(hill.moist.fig1.dat, species == "B.unioloides")) 
##
davies.test(hill.PR.fm)
davies.test(hill.PG.fm)
#breakpoints detected***
##
hill.PR.fm.seg <- segmented(hill.PR.fm, 
                                seg.Z = ~ day.mid.anthesis, 
                                psi = list(day.mid.anthesis = c(23))) #guessing what the break points are***
hill.PG.fm.seg <- segmented(hill.PG.fm, 
                            seg.Z = ~ day.mid.anthesis, 
                            psi = list(day.mid.anthesis = c(31))) 
hill.PG.fm.seg$psi
##
hill.PR.line<- data.frame(day.mid.anthesis = seq(0,50, length.out = 100))
hill.PR.line$seed.moisture<- predict(hill.PR.fm.seg, newdata = data.frame(day.mid.anthesis= seq(0,50, length.out = 100))) 
hill.PR.line$species<- rep("L.perenne")
hill.PG.line<- data.frame(day.mid.anthesis = seq(0,50, length.out = 100))
hill.PG.line$seed.moisture<- predict(hill.PG.fm.seg, newdata = data.frame(day.mid.anthesis= seq(0,50, length.out = 100)))
hill.PG.line$species<- rep("B.unioloides")
hill.line<- rbind(hill.PG.line, hill.PR.line) #combining data frames***
##
hill.seg.slope<- data.frame(day.mid.anthesis =c(14,45,8,38),
                            seed.moisture = c(65, 55, 55, 53),
                              slope= c(slope(hill.PG.fm.seg)[[1]][1:2,1],
                                       slope(hill.PR.fm.seg)[[1]][1:2,1]),
                              sig= c("*", "**",
                                     "NS","***"),  #NOTE: I am hard coding this in*******
                              species=c("B.unioloides","B.unioloides",
                                        "L.perenne","L.perenne"))
##
hill.seg.phase<- data.frame(day.mid.anthesis = c(hill.PG.fm.seg$psi[1,2],
                                                 hill.PR.fm.seg$psi[1,2]),
                            species=c("B.unioloides",
                                      "L.perenne")) #isolating each breakpoint***
##
hill.moist<- ggplot(hill.moist.fig1.dat, aes(x=day.mid.anthesis, y=seed.moisture)) +
  facet_wrap(~species)+
  geom_point(shape=1,
             size=2)+
  geom_line(data = hill.line, mapping = aes(x=day.mid.anthesis, y=seed.moisture), color="grey40", size=1)+
  scale_y_continuous(limits = c(0,70), breaks = c(seq(0,70, by=20)))+
  geom_text(data = hill.seg.slope, mapping = aes(x = day.mid.anthesis, y = seed.moisture, 
                                                   label = round(slope, 2)),
            size=4)+
  geom_text(data = hill.seg.slope, mapping = aes(x = day.mid.anthesis + 7, y = seed.moisture, 
                                                   label = sig),
            size=4)+
  
  geom_segment(data = hill.seg.phase, mapping = aes(x = day.mid.anthesis, 
                                                       xend = day.mid.anthesis,  
                                                       y = c(70,70), yend = c(predict(hill.PG.fm.seg, 
                                                                                 data.frame(day.mid.anthesis=c(hill.PG.fm.seg$psi[1,2]))),
                                                                              predict(hill.PR.fm.seg, 
                                                                                      data.frame(day.mid.anthesis=c(hill.PR.fm.seg$psi[1,2]))))+1),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.03, "npc")))+
  geom_segment(hill.seg.phase, mapping = aes(x = day.mid.anthesis+c(4,14), 
                                             xend = day.mid.anthesis+c(4,14),   
                   y = c(0,0), 
                   yend = c(predict(hill.PG.fm.seg, 
                                  data.frame(day.mid.anthesis=c(46))),
                            predict(hill.PR.fm.seg, 
                                    data.frame(day.mid.anthesis=c(35))))),
               arrow = arrow(length = unit(0.02, "npc")))+
  
  labs(x="Days post peak anthesis", y="Seed moisture (%)", 
       title = "E) Hill and Watkin (1975)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); hill.moist
##
##
##
##
#####
#####
## Analysis of Williams (1972)
#####
williams.fm<- glm(seed.moist ~ day.post.anthesis, data=williams.dat)
##
davies.test(williams.fm)
#breakpoint detected***
##
williams.perc.seg <- segmented(williams.fm, 
                               seg.Z = ~ day.post.anthesis, 
                               psi = list(day.post.anthesis = c(30)),
                               control=seg.control(it.max=100)) 
##
williams.perc.line<- data.frame(day.post.anthesis = seq(18,38, length.out = 100))
williams.perc.line$seed.moist<- predict(williams.perc.seg, newdata = data.frame(day.post.anthesis= seq(18,38, length.out = 100)))                            

williams.seg.slope<- data.frame(day.post.anthesis =c(28,37),
                                seed.moist = c(55, 48),
                                slope= c(slope(williams.perc.seg)[[1]][1:2,1]),
                                sig= c("***", "**")) #NOTE: I am hard coding this in*******

williams.seg.phase<- data.frame(day.post.anthesis = c(williams.perc.seg$psi[1,2])) #isolating each breakpoint***
##
williams.moist<-ggplot(williams.dat, aes(x=day.post.anthesis, y=seed.moist))+
  geom_point(size=2,
             shape=1)+
  geom_line(data = williams.perc.line, mapping = aes(x=day.post.anthesis, y=seed.moist), color="grey40", size=1)+
  scale_y_continuous(limits = c(0,80), breaks = c(seq(0,80, by=20)))+
  scale_x_continuous(limits = c(0,40), breaks = c(seq(0,40, by=10)))+
  geom_text(data = williams.seg.slope, mapping = aes(x = day.post.anthesis, y = seed.moist, 
                                                  label = round(slope,2)),
            size=4)+
  geom_text(data = williams.seg.slope, mapping = aes(x = day.post.anthesis + 2.5, y = seed.moist, 
                                                  label = sig),
            size=4)+
  geom_segment(data = williams.seg.phase, mapping = aes(x = day.post.anthesis, 
                                                       xend = day.post.anthesis,  
                                                       y = c(80), yend = predict(williams.perc.seg, 
                                                                                 data.frame(day.post.anthesis=c(williams.perc.seg$psi[1,2])))+2),
               linetype="dashed",
               color="grey20",
               arrow = arrow(length = unit(0.03, "npc")))+
  geom_segment(aes(x = 35, 
                   xend = 35,  
                   y = 0, 
                   yend = predict(williams.perc.seg, 
                                  data.frame(day.post.anthesis=c(35)))-2),
               arrow = arrow(length = unit(0.02, "npc")))+
  labs(x="Days post max anthesis", y="Seed moisture (%)", 
      title = "F) Williams (1972) - L. perenne")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); williams.moist
##
##
##
##
#####
## Analysis of Arnold and Lake (1966)
#####
arnold.fm1<- glm(seed.moisture~day, data = filter(arnold.fig1.dat, year==1963))
davies.test(arnold.fm1)
arnold.fm2<- glm(seed.moisture~day, data = filter(arnold.fig1.dat, year==1964))
davies.test(arnold.fm2)
#no breakpoint detectd
##
arnold.slope<- data.frame(seed.moisture=c(32,32),
                          day=c(7,3),
                          year=c(1963,1964),
                          slope=c(summary(lm(seed.moisture~day, data = filter(arnold.fig1.dat, year==1963)))[[4]][2,1],
                                  summary(lm(seed.moisture~day, data = filter(arnold.fig1.dat, year==1964)))[[4]][2,1]))

##
arnold.moist<-ggplot(arnold.fig1.dat, aes(x=day, y=seed.moisture))+
  geom_point(shape=1,
             size=2)+
  geom_smooth(method = 'lm',
              se=F,
              color="black")+
  geom_text(arnold.slope, mapping = aes(x=day, y=seed.moisture, label=round(slope,2)),
            size=5)+
  scale_x_continuous(limits = c(0,17), breaks = c(seq(0,18, by=5)))+
  facet_wrap(~year)+
  labs(x="Days after 17 July", y="Seed moisture (%)", 
       title = "J) Arnold and Lake (1966)")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); arnold.moist
##
##
##
##
#####
## Plotting those authors that found a significant breakpoints
## Figure 1
plot_grid(hyde.moist, grabe.moist, roberts.moist, berdahl.moist,
          hill.moist, williams.moist,
          ncol = 3) #1250 x 1600
#################################
######################
###########
###########
###########
######################
#################################
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Analysis of biological floret site utilization data
###########
######################
#################################
##
##
##
##
#####
## Analysis of Burbidge (1978) on perennial ryegrass
#####
burbidge.dat<- burbidge.fig1.dat %>%
  group_by(year, cultivar, Days.post.anthesis) %>%
  summarise(FSU = mean(FSU)) %>%
  ungroup()

burbidge.fm1<- lm(FSU~ Days.post.anthesis + I(Days.post.anthesis^2), data=filter(burbidge.fig1.dat, year==1974 & lodge.trt=="control"))
burbidge.fm2<- lm(FSU~ Days.post.anthesis + I(Days.post.anthesis^2), data=filter(burbidge.fig1.dat, year==1975 & lodge.trt=="control"))
burbidge.fm3<- lm(FSU~ Days.post.anthesis + I(Days.post.anthesis^2), data=filter(burbidge.fig1.dat, year==1974 & lodge.trt=="erect"))
burbidge.fm4<- lm(FSU~ Days.post.anthesis + I(Days.post.anthesis^2), data=filter(burbidge.fig1.dat, year==1975 & lodge.trt=="erect"))
#no interaction between days and lodge treatment

burbidge.line<- data.frame(Days.post.anthesis= c(rep(seq(8,30, length.out = 100),1),
                                                 rep(seq(3,28, length.out = 100),1),
                                                 rep(seq(8,30, length.out = 100),1),
                                                 rep(seq(3,28, length.out = 100),1)),
                           lodge.trt= rep(c("control","control","erect","erect"),each=100),
                           year=rep(c(1974,1975,1974,1975),each=100),
                           FSU= c(predict(burbidge.fm1, newdata = data.frame(Days.post.anthesis=seq(8,30, length.out = 100))),
                                  predict(burbidge.fm2, newdata = data.frame(Days.post.anthesis=seq(3,28, length.out = 100))),
                                  predict(burbidge.fm3, newdata = data.frame(Days.post.anthesis=seq(8,30, length.out = 100))),
                                  predict(burbidge.fm4, newdata = data.frame(Days.post.anthesis=seq(3,28, length.out = 100)))))

burbidge.peak<- data.frame(Days.post.anthesis=c(nlsfit(filter(burbidge.fig1.dat, year==1974 & lodge.trt=="control") %>% select(Days.post.anthesis,FSU), model=2)[[2]][12,1],
                                                nlsfit(filter(burbidge.fig1.dat, year==1975 & lodge.trt=="control") %>% select(Days.post.anthesis,FSU), model=2)[[2]][12,1],
                                                nlsfit(filter(burbidge.fig1.dat, year==1974 & lodge.trt=="erect") %>% select(Days.post.anthesis,FSU), model=2)[[2]][12,1],
                                                nlsfit(filter(burbidge.fig1.dat, year==1975 & lodge.trt=="erect") %>% select(Days.post.anthesis,FSU), model=2)[[2]][12,1]),
                           FSU=c(nlsfit(filter(burbidge.fig1.dat, year==1974 & lodge.trt=="control") %>% select(Days.post.anthesis,FSU), model=2)[[2]][11,1],
                                 nlsfit(filter(burbidge.fig1.dat, year==1975 & lodge.trt=="control") %>% select(Days.post.anthesis,FSU), model=2)[[2]][11,1],
                                 nlsfit(filter(burbidge.fig1.dat, year==1974 & lodge.trt=="erect") %>% select(Days.post.anthesis,FSU), model=2)[[2]][11,1],
                                 nlsfit(filter(burbidge.fig1.dat, year==1975 & lodge.trt=="erect") %>% select(Days.post.anthesis,FSU), model=2)[[2]][11,1]),
                           year=c(1974,1975,1974,1975),
                           lodge.trt= rep(c("control","control","erect","erect"),each=1))
## Plotting Figure 2
burbidge.FSU<- ggplot(burbidge.fig1.dat)+
  geom_point(burbidge.fig1.dat, mapping = aes(x=Days.post.anthesis, y=FSU, shape=lodge.trt),
             size=3)+
  geom_line(burbidge.line, mapping = aes(x=Days.post.anthesis, y=FSU, linetype=lodge.trt))+
  facet_wrap(~year)+
  geom_point(burbidge.peak, mapping = aes(x=Days.post.anthesis, y=FSU, shape=lodge.trt),
             size=6,
             color="black")+
  scale_x_continuous(limits = c(3,30), breaks = c(seq(3,30, by=5)))+
  scale_linetype_discrete(labels=c("lodged", "standing"))+
  labs(x="Days post anthesis", y="FSU",
       title="A) Burbidge (1978) - L. perenne",
       linetype="Lodging",
       shape="lodging")+
  IWG_theme(base_size = 12)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); burbidge.FSU
##
##
##
##
#####
## Analysis of Williams (1972) on perennial ryegrass
#####
summary(lm(FSU.max~day.post.anthesis, data=williams.dat))
#data are linear 
williams.FSU<- ggplot(williams.dat, aes(x=day.post.anthesis, y=FSU.max))+
  geom_point(size=3,
             shape=1)+
  geom_smooth(method = 'lm',
              se=F,
              color="black")+
  labs(x="Days post anthesis", y="FSU", 
       subtitle="Figure 3 [L. perenne]", 
       title="C) Williams (1972)")+
  IWG_theme(base_size = 12)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); williams.FSU
##
##
##
##
#####
## Analysis of Anslow (1963) on perennial ryegrass
#####
summary(lm(FSU ~ day.post.end.anths + I(day.post.end.anths^2), data = anslow.disc.63))
#data are quadratic 
anlsow.FSU.lm<- lm(FSU ~ day.post.end.anths + I(day.post.end.anths^2), data = anslow.disc.63)
anlsow.FSU.pred<- data.frame(day.post.end.anths=c(seq(5,30,length.out= 100)),
                            FSU=predict(anlsow.FSU.lm, newdata = data.frame(day.post.end.anths=c(seq(5,30,length.out= 100)))))

anlsow.FSU.max<- data.frame(day.post.end.anths=nlsfit(select(anslow.disc.63, day.post.end.anths,FSU), model=2)[[2]][12,1],
                            FSU=nlsfit(select(anslow.disc.63, day.post.end.anths,FSU), model=2)[[2]][11,1])

anslow.FSU<- ggplot(anslow.disc.63, aes(x=day.post.end.anths, y=FSU))+
  geom_line(anlsow.FSU.pred, mapping = aes(x=day.post.end.anths, y=FSU))+
  geom_point(shape=1,
             size=3)+
  geom_point(anlsow.FSU.max, mapping = aes(x=day.post.end.anths, y=FSU),
             size=6,
             shape=18,
             color="black")+
  labs(x="Days post anthesis", y="FSU", 
       subtitle="Discussion [L. perenne]", 
       title="B) Anlsow (1963)")+
  IWG_theme(base_size = 12)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); anslow.FSU
##
##
##
##
#####
## Analysis of Elgersma on perennial ryegrass
#####
summary(lm(FSU~cultivar*(days.post.anthesis+I(days.post.anthesis^2)), data = elgersma.dat))
## Data are quadratic for each cultivar
elgersma.FSU.fm<- lm(FSU~cultivar*(days.post.anthesis+I(days.post.anthesis^2)), data = elgersma.dat)
##
elgersma.FSU.dat<-data.frame(days.post.anthesis=c(seq(7,35,length.out = 100)),
                             cultivar=rep(c("175_5","371_9","Lamora","Perma"), each=100))
elgersma.FSU.dat$FSU<- predict(elgersma.FSU.fm, newdata = elgersma.FSU.dat)
##
elgersma.FSU.max<- data.frame(days.post.anthesis=c(NA, 20,19,25),
                              FSU=c(NA,74,69,70),
                              cultivar=c("175_5","371_9","Lamora","Perma"))
##
elgersma.FSU<- ggplot(elgersma.dat, aes(x=days.post.anthesis, y=FSU))+
  geom_point(size=2,
             shape=1)+
  geom_point(elgersma.FSU.max, mapping = aes(x=days.post.anthesis, y=FSU),
             size=6,
             shape=18,
             color="black")+
  geom_line(elgersma.FSU.dat, mapping=aes(x=days.post.anthesis, y=FSU))+
  facet_wrap(~cultivar)+
  labs(x="Days post anthesis", y="FSU", 
       subtitle="Table 5 [L. perenne]", 
       title="C) Elgersma et al. (1988)")+
  IWG_theme(base_size = 12)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); elgersma.FSU
#####
## Plotting all FSU data
plot_grid(burbidge.FSU,anslow.FSU,williams.FSU,elgersma.FSU, ncol = 2)
#################################
######################
###########
###########
###########
######################
#################################
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Analysis of spikelet and floret shatter
###########
######################
#################################
##
##
##
##
#####
## Analysis of Williams (1972) on perennial ryegrass
#####
williams.fm1<- lm(tot.floret~moisture.perc, data = williams.dat); summary(williams.fm1) #significant positive slope (negative) slope***
williams.fm2<- lm(tot.floret~moisture.perc+I(moisture.perc^2), data = williams.dat); summary(williams.fm2)  #insignificant quadratic slope***
williams.fm3<- lm(tot.floret~moisture.perc+I(moisture.perc^.5), data = williams.dat); summary(williams.fm3)  #insignificant plateau slope***
williams.fm4<- lm(tot.floret~moisture.perc+I(moisture.perc^2)+I(moisture.perc^3), data = williams.dat); summary(williams.fm4) #insignificant cubic slope***
anova(williams.fm1, williams.fm4)#insignificance between models***
## Based on the intercepts, a linear relationship is most probable for these data
##
## Fitting a linear model past the data to y-intercept
williams.shatter<-ggplot(williams.dat, aes(x=day.post.anthesis, y=tot.floret))+
  geom_point(size=2,
             shape=1)+
  stat_smooth(method="lm", 
              fullrange=F,
              color="grey30",
              se=F)+
  labs(title = "C) Williams (1972) - L. perenne", x = "Days post anthesis", y = "Florets per spike")+
  scale_y_continuous(limits = c(0,350), breaks = c(seq(0,350, by=100)))+
  annotate("text", x = 25, y = 250,
           label = "slope= -5.5 *", 
           size=5)+
  scale_x_continuous(limits = c(5,40), breaks = c(seq(5,40, by=5)))+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); williams.shatter
##
##
##
##
#####
## Analysis of Anslow (1964) on perennial ryegrass
#####
anova(lm(floret.ct_skplet ~ day * fraction, data = anslow.fig7.dat)) #no interaction between day and fraction***
anova(lm(floret.ct_skplet ~ day * I(day^2) * fraction, data = anslow.fig7.dat)) #no interaction with a polynomial***
anova(lm(floret.ct_skplet ~  (day + I(day^2)) * fraction, data = anslow.fig7.dat)) #significant polynomial
anslow.fm.log<- lm(floret.ct_skplet ~ day + I(day^3) + fraction, data = anslow.fig7.dat)
anova(lm(floret.ct_skplet ~  day + I(day^3) + fraction, data = anslow.fig7.dat), 
      lm(floret.ct_skplet ~  day + fraction, data = anslow.fig7.dat)) #significance between models, polynomial is best fit***
#replicate cannot be incorperated due to means only available from the manuscript***
#however, there is no ineraction between day and fraction P=0.58***
#both day and fraction are significant***
##
anslow.mod.dat<- data.frame(fraction = rep(c("basal", "middle", "upper"), each=100),
                            day = seq(5,35, length.out = 100))
anslow.mod.dat$floret.ct_skplet<- (predict(anslow.fm.log, newdata =anslow.mod.dat))
##
anslow.intcpt.dat<- data.frame(day=c(0,0,0),
                               fraction=c("basal","middle","upper"),
                               floret.ct_skplet=predict(anslow.fm.log, newdata = data.frame(day=c(0),fraction=c("basal","middle","upper"))))
##
anslow.shatter.lm<- ggplot(anslow.fig7.dat, aes(x=day, y=floret.ct_skplet, color=fraction, linetype=fraction))+
  stat_summary(fun = "mean", 
               size = 3,
               geom="point",
               position=position_jitter(width = .2),
               shape=1)+
  geom_text(anslow.intcpt.dat, mapping = aes(x=day+c(7,5,9),y=floret.ct_skplet+.4, label=round(floret.ct_skplet,2)),
            size=5,
            color="black")+
  annotate("text", x = 30, y = 7,
           label = "fraction:day (P = 0.58)", 
           size=5)+
  scale_color_manual(values=c("grey5", "grey30", "grey60"))+
  geom_line(anslow.mod.dat, mapping = aes(x=day, y=floret.ct_skplet, color=fraction, linetype=fraction))+
  scale_x_continuous(limits = c(5,35), breaks = c(seq(5,35, by=5)))+
  labs(title= "B) Anslow (1964)", subtitle = "Figure 7 [L. perenne]", x="Days post anthesis", y="Florets per spikelet", color="Spike fraction", linetype="Spike fraction")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); anslow.shatter.lm
##
##
#using breakpoint regression to obtain more meaningful parameters***
anslow.glm.basal<- glm(floret.ct_skplet ~ day, data = filter(anslow.fig7.dat, fraction == "basal"))
anslow.glm.middle<- glm(floret.ct_skplet ~ day, data = filter(anslow.fig7.dat, fraction == "middle"))
anslow.glm.upper<- glm(floret.ct_skplet ~ day, data = filter(anslow.fig7.dat, fraction == "upper"))
##
davies.test(anslow.glm.basal)
davies.test(anslow.glm.middle)
davies.test(anslow.glm.upper)
#no break points detected***
#NOTE I will proceed with segmented anyways***
##
anslow.basal.seg <- segmented(anslow.glm.basal, 
                              seg.Z = ~ day, 
                              psi = list(day = c(23)),
                              control=seg.control(it.max=100))
anslow.middle.seg <- segmented(anslow.glm.middle, 
                              seg.Z = ~ day, 
                              psi = list(day = c(22)),
                              control=seg.control(it.max=100))
anslow.upper.seg <- segmented(anslow.glm.upper, 
                              seg.Z = ~ day, 
                              psi = list(day = c(20)),
                              control=seg.control(it.max=100))
##
anslow.seg.line<- data.frame(fraction = rep(c("basal", "middle", "upper"), each=100),
                             day = seq(5,35, length.out = 100),
                             floret.ct_skplet=c(predict(anslow.basal.seg, data.frame(day = seq(5,35, length.out = 100))),
                                                predict(anslow.middle.seg, data.frame(day = seq(5,35, length.out = 100))),
                                                predict(anslow.upper.seg, data.frame(day = seq(5,35, length.out = 100)))))
##
anslow.seg.phase<- data.frame(fraction = rep(c("basal", "middle", "upper"), each=1),
                              day      = c(anslow.basal.seg$psi[1,2],
                                           anslow.middle.seg$psi[1,2],
                                           anslow.upper.seg$psi[1,2]))
##
anslow.seg.slope<- data.frame(fraction = rep(c("basal", "middle", "upper"), each=2),
                              day      = c(10,30,10,30,10,30),
                              floret.ct_skplet=c(4,8,4,8,4,8),
                              slope    = c(slope(anslow.basal.seg)[[1]][1:2,1],
                                           slope(anslow.middle.seg)[[1]][1:2,1],
                                           slope(anslow.upper.seg)[[1]][1:2,1]),
                              sig      = c("NS","*",
                                           "NS","**",
                                           "NS","**"))
##
## Figure 3 (left panel)
anslow.shatter.seg<- ggplot(anslow.fig7.dat, aes(x=day, y=floret.ct_skplet, color=fraction, linetype=fraction))+
  stat_summary(fun = "mean", 
               size = 3,
               geom="point",
               position=position_jitter(width = .2),
               shape=1)+
  geom_vline(data = anslow.seg.phase, mapping = aes(xintercept=day), #adding breakpoints with vertical lines***
             linetype="dashed",
             color="grey20")+
  geom_text(anslow.seg.slope, mapping= aes(x=day, y=floret.ct_skplet-c(0,1.5,1,2.5,2,3.5), 
                                           color=fraction, label=round(slope,2)),
            size=5)+
  geom_text(anslow.seg.slope, mapping= aes(x=day+3, y=floret.ct_skplet-c(0,1.5,1,2.5,2,3.5), 
                                           color=fraction, label=sig),
            size=5)+
  guides(linetype=F)+
  scale_color_manual(values=c("grey5", "grey30", "grey60"), labels = c("Basal", "Middle", "Distal"))+
  geom_line(anslow.seg.line, mapping = aes(x=day, y=floret.ct_skplet, color=fraction, linetype=fraction))+
  scale_x_continuous(limits = c(5,35), breaks = c(seq(5,35, by=5)))+
  labs(title= "Anslow (1964) - L. perenne", x="Days post anthesis", y="Florets per spikelet", color="Spike fraction", linetype="Spike fraction")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); anslow.shatter.seg
##
##
##
##
#####
## Elgersma Table 5
#####
elgersma.shatter.dat<- elgersma.dat %>%
  group_by(days.post.anthesis) %>%
  summarise(florets.spklt = mean(florets.spklt))
##
summary(lm(florets.spklt~days.post.anthesis+I(days.post.anthesis^5), data = elgersma.shatter.dat))
##
elgersma.shatter.fm<- lm(florets.spklt~days.post.anthesis+I(days.post.anthesis^5), data = elgersma.shatter.dat)
##
elgersma.shatter.fm.dat<- data.frame(days.post.anthesis=seq(5,35,length.out = 100),
                                     florets.spklt=predict(elgersma.shatter.fm, newdata = data.frame(days.post.anthesis=seq(5,35,length.out = 100))))
##
elgersma.shatter<- ggplot(elgersma.shatter.dat, aes(y=florets.spklt, x=days.post.anthesis))+
  geom_point(size=2,
             shape=1)+
  geom_line(elgersma.shatter.fm.dat, mapping = aes(y=florets.spklt, x=days.post.anthesis))+
  annotate("text", x = 9, y = 7,
           label = "y-intercept = 9.0", 
           size=6)+
  scale_x_continuous(limits = c(5,35), breaks = c(seq(5,35, by=5)))+
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10, by=2)))+
  labs(title= "C) Elgersma and Sniezko (1988)", subtitle = "Table 4 [L. perenne]", x="Days post anthesis", y="Florets per spikelet")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); elgersma.shatter
##
#breakpoint regression***
elgersma.shatter.glm<- glm(florets.spklt~days.post.anthesis, data = elgersma.shatter.dat)
##
davies.test(elgersma.shatter.glm)
##
elgersma.shatter.seg<- segmented(elgersma.shatter.glm, 
                                 seg.Z = ~ days.post.anthesis, 
                                 psi = list(days.post.anthesis = c(24)),
                                 control=seg.control(it.max=100))
##
elgersma.seg.line<- data.frame(days.post.anthesis=seq(5,35,length.out = 100),
                               florets.spklt=predict(elgersma.shatter.seg, newdata = data.frame(days.post.anthesis=seq(5,35,length.out = 100))))
##
elgersma.seg.phase<- data.frame(days.post.anthesis=elgersma.shatter.seg$psi[1,2])
##
elgersma.seg.slope<- data.frame(days.post.anthesis=c(12,27),
                                florets.spklt=c(5,5),
                                slope=c(slope(elgersma.shatter.seg)[[1]][1:2,1]),
                                sig=c("NS","***"))
##
elgersma.seg.incpt<- data.frame(days.post.anthesis=c(7),
                                florets.spklt=c(7),
                                intrcpt=c(paste("y intrcpt= ", round(intercept(elgersma.shatter.seg)[[1]][1],1))))
##
elgersma.shatter.segm<- ggplot(elgersma.shatter.dat, aes(y=florets.spklt, x=days.post.anthesis))+
  geom_point(size=2,
             shape=1)+
  geom_line(elgersma.seg.line, mapping = aes(y=florets.spklt, x=days.post.anthesis))+
  geom_vline(data = elgersma.seg.phase, mapping = aes(xintercept=days.post.anthesis), #adding breakpoints with vertical lines***
             linetype="dashed",
             color="grey20")+
  geom_text(elgersma.seg.slope, mapping= aes(x=days.post.anthesis, y=florets.spklt, 
                                           label=round(slope,2)),
            size=5)+
  geom_text(elgersma.seg.slope, mapping= aes(x=days.post.anthesis+2, y=florets.spklt, 
                                           label=sig),
            size=5)+
  geom_text(elgersma.seg.incpt, mapping= aes(x=days.post.anthesis, y=florets.spklt, 
                                             label=intrcpt),
            size=5)+
  scale_x_continuous(limits = c(5,35), breaks = c(seq(5,35, by=5)))+
  scale_y_continuous(limits = c(0,10), breaks = c(seq(0,10, by=2)))+
  labs(title= "C) Elgersma and Sniezko (1988)", subtitle = "Table 4 [L. perenne]", x="Days post anthesis", y="Florets per spikelet")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb")); elgersma.shatter.segm
##
##
##
##
#####
## Plotting all shattering data
plot_grid(williams.shatter, anslow.shatter.seg, elgersma.shatter.segm, ncol = 1)
##
#################################
######################
###########
###########
###########
######################
#################################
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Analysis of seed yield over time
###########
######################
#################################
##
##
##
##
#####
## Anslow 1964 - Figure 8
#####
## Seed yield of full florets of 100% germinating capacity in 100 spikelets of perennial ryegrass
anslow.fig8.fm1<- lm(seed.yld_100.spklet ~ fraction * day * I(day^2) , data = anslow.fig8.dat); anova(anslow.fig8.fm1) #significant main effects
anslow.fig8.fm2<- lm(seed.yld_100.spklet ~ fraction * (day + I(day^2)) , data = anslow.fig8.dat); anova(anslow.fig8.fm2)
anslow.fig8.fm3<- lm(seed.yld_100.spklet ~ fraction + day + I(day^2) , data = anslow.fig8.dat); anova(anslow.fig8.fm3); summary(anslow.fig8.fm3) #optimal model
anslow.fig8.fm4<- lm(seed.yld_100.spklet ~ fraction + day, data = anslow.fig8.dat); anova(anslow.fig8.fm4)
anova(anslow.fig8.fm2, anslow.fig8.fm3)
#there was no interaction between fraction and day indicating fractions generally behave the same***
#Fraction was highly significant***
##
plateau_formula<-formula(seed.yld_100.spklet ~ alpha - (((.5*gamma)/sqrt(peak))*day) + (gamma*day^.5) | fraction)

anslow.fig4.nls<-nlsList(plateau_formula, 
                         start=list(alpha=-1000,
                                    peak=  20,
                                    gamma=1000), 
                         data = anslow.fig8.dat, 
                         na.action=na.omit)

anslow.fig4.lm.pred<-data.frame(day=rep(seq(5,32),3), fraction=rep(c("basal","middle","upper"), each=28), model=rep("LM"))
anslow.fig4.lm.pred$seed.yld_100.spklet<-  predict(anslow.fig8.fm3, newdata = data.frame(day=rep(seq(5,32),3), fraction=rep(c("basal","middle","upper"),each=28)))

anslow.fig4.nls.pred<-data.frame(day=rep(seq(5,32),3), fraction=rep(c("basal","middle","upper"), each=28), model=rep("NLS"))
anslow.fig4.nls.pred$seed.yld_100.spklet<-  predict(anslow.fig4.nls, newdata = data.frame(day=rep(seq(5,32),3), fraction=rep(c("basal","middle","upper"),each=28)))
anslow.pred.dat<- rbind(anslow.fig4.lm.pred,anslow.fig4.nls.pred)
##
##
anslow.fig4.nls.sum<- summary(anslow.fig4.nls)
anslow.fig4.max<-data.frame(peak      = anslow.fig4.nls.sum$coefficient[,,'peak'][1:3,1],
                            peak.se   = anslow.fig4.nls.sum$coefficient[,,'peak'][1:3,2],
                            yield     = predict(anslow.fig4.nls, data.frame(day      =anslow.fig4.nls.sum$coefficient[,,'peak'][1:3,1], 
                                                                            fraction =c("basal","middle","upper")), 
                                                level=0),
                            fraction  = c("basal","middle","upper"))
##
## Figure 3 (right panel)
anslow.yield<-ggplot(anslow.fig8.dat, aes(x=day, y=seed.yld_100.spklet, color=fraction))+
  stat_summary(fun = "mean",
               geom="point",
               size = 3,
               position=position_jitter(width = .3),
               shape=1)+
  scale_color_manual(values=c("grey5", "grey30", "grey60"))+
  geom_line(data = filter(anslow.pred.dat, model=="NLS"), #NOTE****
            mapping = aes(x=day, 
                          y=seed.yld_100.spklet 
                          #linetype=model
            ))+
  geom_point(data=anslow.fig4.max, 
             mapping= aes(x=peak, 
                          y=yield,
                          color=fraction), shape=18, size=6)+
  geom_segment(data=anslow.fig4.max, 
               mapping=aes(x=peak-peak.se, 
                           y=yield,
                           color=fraction,
                           xend=peak+peak.se,
                           yend=yield), alpha=.5, linetype=1, size=1.5, color="black")+
  labs(title="Anslow (1964 - L. perenne)",
       x="Days post maximum anthesis",
       y="Seed yield (g/100 spikelets)",
       color="Fraction")+
  scale_x_continuous(limits = c(5,40), breaks = c(seq(5,40, by=10)))+
  scale_y_continuous(limits = c(0,900), breaks = c(seq(0,900, by=200)))+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10)); anslow.yield
##
##
##
##
#####
## Re creation from Pegler (1976) Figure 1 panel c and e
#####
## Two perennial ryegrass cultivars were tested S.23 and S.24
plateau_formula<-formula(yield ~ alpha - (((.5*gamma)/sqrt(peak))*Days.post.anthesis) + (gamma*Days.post.anthesis^.5) | cultivar)

pegler.fig1.nls<-nlsList(plateau_formula, 
                         start=list(alpha=-10,
                                    peak=  20,
                                    gamma=10), 
                         data = pegler.fig1.dat, 
                         na.action=na.omit)

pegler.fig1.nls.pred<-data.frame(Days.post.anthesis=rep(seq(12,45),2), cultivar=rep(c("S.23","S.24"), each=34))
pegler.fig1.nls.pred$yield<-  predict(pegler.fig1.nls, newdata = pegler.fig1.nls.pred)

pegler.fig1.nls.sum<- summary(pegler.fig1.nls)
pegler.fig1.max<-data.frame(peak      = pegler.fig1.nls.sum$coefficient[,,'peak'][1:2,1],
                            peak.se   = pegler.fig1.nls.sum$coefficient[,,'peak'][1:2,2],
                            yield     = predict(pegler.fig1.nls, data.frame(Days.post.anthesis      =pegler.fig1.nls.sum$coefficient[,,'peak'][1:2,1], 
                                                                            cultivar =c("S.23","S.24")), 
                                                level=0),
                            cultivar  = c("S.23","S.24"))
##
pegler.seed.max<- data.frame(Days.post.anthesis= c(36,33),
                             yield     = predict(pegler.fig1.nls, data.frame(Days.post.anthesis      =c(36,33), 
                                                                             cultivar =c("S.23","S.24")), 
                                                 level=0),
                             cultivar  = c("S.23","S.24"))
##
## Figure 4
pegler.yield<- ggplot(pegler.fig1.dat, aes(x=Days.post.anthesis, y=yield, color=cultivar))+ 
  geom_point(shape=1,
             size=2)+
  geom_line(pegler.fig1.nls.pred, mapping = aes(x=Days.post.anthesis, y=yield))+
  facet_wrap(~cultivar)+
  scale_color_manual(values=c("grey5", "grey50"))+
  geom_point(data=pegler.fig1.max, 
             mapping= aes(x=peak, 
                          y=yield,
                          color=cultivar), shape=18, size=6, alpha=.5)+
  geom_segment(data=pegler.fig1.max, 
               mapping=aes(x=peak-peak.se, 
                           y=yield,
                           color=cultivar,
                           xend=peak+peak.se,
                           yend=yield), alpha=.5, linetype=1, size=2, color="black")+
  geom_segment(data=pegler.seed.max, 
               mapping=aes(x=Days.post.anthesis, 
                           y=0,
                           color=cultivar,
                           xend=Days.post.anthesis,
                           yend=yield-.1),
               arrow = arrow(length = unit(0.02, "npc")))+
  guides(color=F)+
  labs(x="Days post anthesis", y="Seed yield (g per 100 spikes)",
       title="Pegler (1976) - L. perenne")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); pegler.yield
##
##
##
##
#####
## Re creation from Hill and Watkin (1975) Table 1 and Figure 1 panel c and e
#####
hill.table1.dat<- hill.table1.dat %>%
  rowwise() %>%
  mutate(yield=mean(c(yield.swath,yield.drct.comb), na.rm=T))
##
summary(lm(yield~day.mid.anthesis+I(day.mid.anthesis^6), data = filter(hill.table1.dat, species=="B.unioloides")))
summary(lm(yield~day.mid.anthesis+I(day.mid.anthesis^2), data = filter(hill.table1.dat, species=="L.perenne")))
##
hill.PG.lm<- lm(yield~day.mid.anthesis+I(day.mid.anthesis^6), data = filter(hill.table1.dat, species=="B.unioloides"))
hill.PR.lm<- lm(yield~day.mid.anthesis+I(day.mid.anthesis^2), data = filter(hill.table1.dat, species=="L.perenne"))
##
hill.max.dat<- data.frame(day.mid.anthesis=c(35,
                                              nlsfit(filter(hill.table1.dat, species=="L.perenne") %>% select(day.mid.anthesis,yield), model = 2)[[2]][12,1]),
                          yield=c(3920,
                                  nlsfit(filter(hill.table1.dat, species=="L.perenne") %>% select(day.mid.anthesis,yield), model = 2)[[2]][11,1]),
                          species=c("B.unioloides","L.perenne"))
##
hill.lm.dat<- data.frame(day.mid.anthesis= rep(seq(0,52, length.out = 100),2),
                         yield=c(predict(hill.PG.lm, newdata = data.frame(day.mid.anthesis= seq(0,52, length.out = 100))),
                                 predict(hill.PR.lm, newdata = data.frame(day.mid.anthesis= seq(0,52, length.out = 100)))),
                         species=rep(c("B.unioloides","L.perenne"),each=100))
##
hill.yield<- ggplot(hill.table1.dat, aes(y=yield,x=day.mid.anthesis))+
  geom_point(shape=1,
             size=2)+
  geom_point(hill.max.dat, mapping = aes(y=yield,x=day.mid.anthesis),
             shape=18,
             size=6)+
  geom_line(hill.lm.dat, mapping = aes(y=yield,x=day.mid.anthesis),
            color="black")+
  facet_wrap(~species)+
  scale_y_continuous(limits = c(0,4500), breaks = c(seq(0,4500, by=1000)))+
  labs(x="Days post mid anthesis", y="Seed yield (100 spikes)",
       title="B) Hill and Watkin (1975) ")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); hill.yield
##
##
##
##
#####
## Williams 1972 
#####
summary(lm(spk.yld~day.post.anthesis, data = williams.fig2.dat)) #linear is best fit***
##
williams.lm2<- lm(spk.yld~day.post.anthesis, data = williams.fig2.dat)
##
williams.lm.day2<- data.frame(day.post.anthesis=c(seq(20,38, length.out = 100)),
                              spk.yld= predict(williams.lm2, newdata = data.frame(day.post.anthesis=c(seq(20,38, length.out = 100)))))
##
williams.yield.g.spk<- ggplot(williams.fig2.dat,aes(x=day.post.anthesis, y=spk.yld))+
  geom_point(size=2,
             shape=1)+
  geom_line(williams.lm.day2, mapping = aes(y=spk.yld,x=day.post.anthesis),
            color="black")+
  scale_y_continuous(limits = c(.1,.45), breaks = c(seq(.1,.45, by=.1)))+
  scale_x_continuous(limits = c(5,40), breaks = c(seq(5,40, by=5)))+
  labs(x="Days post anthesis", y="Seed yield (g/spike)",
       title="D) Williams (1972) - L. perenne")+
  IWG_theme(base_size = 15)+
  theme(panel.background = element_rect(fill = "#f2eecb"),
        strip.background = element_rect(fill="white")); williams.yield.g.spk
#####
## Plotting all seed yield data
plot_grid(anslow.yield,hill.yield,williams.yield.kg.ha,williams.yield.g.spk,pegler.yield, ncol = 2)
#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Figure 3
###########
######################
#################################
## Figure 3 from Pegler (1976)
plot_grid(pegler.seed.wt, pegler.yield, ncol = 2)
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#################################
######################
###########
########### Figure 4
###########
######################
#################################
## Combining floret shatter and yield
plot_grid(anslow.shatter.seg, anslow.yield, ncol = 2)
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#
#****#