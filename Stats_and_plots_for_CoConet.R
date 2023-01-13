##--------------------------------------------------------------
##--Script to summarise and Plot CoConet outputs on CoTS control
##--------------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(broom)
library(RColorBrewer)
library(bit64)
library(magrittr)
library(ggpubr)
library(rgdal)
library(rgeos)
library(raster)
library(visreg)
library(tidymv)
library(stringr)

#filepath="CoCoNet_outputs"

##--------------------------------
##Edit Counterfactual labels
##--------------------------------
edit_counterfactual_names<-function(x){
  names(x)<-c("simul","Year","ReefID","cover","coralarea","cotsdens","n","Active")## no-control
  return(as.data.frame(x))
}
edit_counterfactual_names2<-function(x){
  names(x)<-c("simul","Year","ReefID","cover")## no-coTS
  return(as.data.frame(x))
}

##--------------------
##Load Counterfactual
##--------------------
outputs=list.files(path = filepath, pattern=c("Counterfactual_n"))## to select both counterfactuals 
load(paste(filepath,outputs[1],sep="/"))
dat01=model0##each row is a reef
rm(model0)

dat01=edit_counterfactual_names(dat01)

##average cover across reefs
c01=dat01%>%group_by(Year,simul)%>%
  summarise_at(vars(cover),mean)%>%as.data.frame()


##Count outbreaks per year
cc01=dat01%>%
  group_by(Year,simul)%>%
  summarise_at(vars(n),sum)%>%as.data.frame()


##NoCoTS
load(paste(filepath,outputs[2],sep="/"))
dat00=model0##each row is a reef
rm(model0)

dat00=edit_counterfactual_names2(dat00)

##average cover across reefs
c00=dat00%>%group_by(Year,simul)%>%
  summarise_at(vars(cover),mean)%>%as.data.frame()

##Add legend
ann_text<-data.frame(Year=c(2047, 2047),
                     cover=c(33,31),
                     label=c("No CoTS","No CoTS control"))

##----------
##Figure 1
##----------

p1= ggplot(c01,aes(x=as.numeric(Year), y=cover))+
  stat_summary(fun.data=mean_sd, geom="ribbon", fill="grey75")+
  stat_summary(fun=mean, geom="line", color="black")+
  ggtitle("CoCoNet\nCounterfactual of coral cover")+
  theme(legend.position = "bottom")+ylim(0,36)+
  ylab("Total coral cover (%)")+xlab("Years")+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover), fun.data=mean_sd, geom="ribbon", fill=c("#FFE0E0"),alpha=0.2)+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover),fun=mean, geom="line", color="red", linetype="dashed")+theme_bw()


p1=p1 + geom_text(data = ann_text, label=ann_text$label, color= c("red","black"),size=2)


p2=ggplot(cc01,aes(x=as.numeric(Year), y=n))+
  stat_summary(fun.data=mean_sd, geom="ribbon", fill="grey75",color="grey75")+
  stat_summary(fun=mean, geom="line", color="black")+
  ggtitle("CoCoNet\nCounterfactual of outbreaking reefs")+
  theme(legend.position = "bottom")+ylim(0,1900)+
  ylab("Number of outbreaking reefs \n(Cots density per tow > 0.22)")+xlab("Years")+
  theme_bw()


ggarrange(p1,p2, labels=c("C","D"), ncol = 1,align = "v") 


filename="Counterfactuals_CoCoNet.jpeg"
ggsave(filename=filename, width = 4, height = 6, dpi=300)


#############################################
##Figure 2. GBR-wide Benefits in Corals
############################################

##---------------------------------------------------------------------
#Read the reef-level outputs of control
outputs=list.files(path = filepath, pattern="ALLoutputs_newGBR")

#Convert Benefits as a proportion of cover on a healthy reef
coeff<-2219 ##the area of healthy (34%) GBR in 1990 

##--------------------------------
##Edit Counterfactual labels
##--------------------------------
edit_variable_names<-function(x){
  
  names(x)<-c("simul","Year","ReefID","areaKm2","cover","coralarea","cotsdens","n","Active")
  return(as.data.frame(x))
}

##Compare area estimates using reef-specific coral habitat instead (Use new updated table)
GBRpolygons <- read.csv("GBR_REEFS_for_ReefMod.csv")

reefarea=data.frame("ReefID"=GBRpolygons$LABEL_ID,"CHarea"=GBRpolygons$GeomorphicAreaKm2_2dCH)


#extract benefits and concatenate all strategies
B=data.frame()
Covdat=data.frame()


for (d in outputs) {
  
  load(paste(filepath,d,sep="/"))
 #Extract the name of scenario
  d%<>%str_replace("ALLoutputs_new", "") 
  d%<>%str_sub(1,nchar(d)-2) ## removes file extension
  
  ##Extract cover trajectories
  model1%<>%as.data.frame()%>%edit_variable_names()
  areaKm2=model1$areaKm2
  model1%<>%group_by(simul,Year)%>%summarise(MeanCover=mean(cover))
  model1$strat=d
  Covdat=rbind(Covdat,as.data.frame(model1))
  
  ###Calculate Area Benefits
  model_delta%<>%as.data.frame()%>%edit_variable_names()%>%mutate(areaKm2=areaKm2)##Reef area is in Km2
  model_delta%<>%left_join(reefarea)
  model_delta$AreaGained_CH=(model_delta$cover/100)*model_delta$CHarea##USE THIS
  model_delta%<>%group_by(simul,Year)%>%summarise(AreaGained=sum(coralarea),AreaGained_CH=sum(AreaGained_CH))
  model_delta$PropBenefit= model_delta$AreaGained/coeff
  model_delta$PropBenefit_v2= model_delta$AreaGained_CH/coeff
  model_delta$strat=d

  B=rbind(B,as.data.frame(model_delta))

}


##---------------------------------------------------------------------  
##--------------------------------
##Edit table labels for scenarios
##--------------------------------
edit_labels<-function(x){
  
  ##Edit Strat Names
  x$Boats=x$strat
  x$Boats<-sub("GBR_", "", x$Boats)
  ##Arrange by Boats
 # x$Boats <- factor(x$Boats, levels = c("2", "5", "8","14","30"))
  x$Boats<-as.numeric(as.character(x$Boats))
  
  ##Create New strat label
  x$stratnames=x$strat
  x$strat%<>%str_sub(1,nchar(x$strat)-2)#remove boats
  x$strat<- str_replace(x$strat, "GBR_", "GBR")
  x=x%>%filter(strat=="GBR")
  x$stratnames <- factor(x$stratnames, levels = c("GBR_02","GBR_05","GBR_08","GBR_14","GBR_30"))
  return(as.data.frame(x))
}
##---------------------------------------------------------------------  

B=edit_labels(B)

##----------
##Figure 2
##----------

##Only GBR-wide

ggplot(B,aes(x=as.factor(Year), y=AreaGained_CH,fill=stratnames))+
  geom_boxplot(outlier.size=0.5, outlier.alpha = 0.5, lwd=0.3, fatten=2, alpha=.6)+ylim(-100,1000)+
  scale_y_continuous(sec.axis=sec_axis(~./coeff*100, name="Percentage of healthy GBR Area"))+
  stat_mean(size=0.7, color="red")+
  facet_grid(cols=vars(Boats))+
  scale_fill_manual(values=c(brewer.pal(6,'Blues')[-1]))+
  theme(legend.position = "none",axis.text.x=element_text(size=10))+
  ylab(expression(Area~of~live~coral~gained~(Km^2)))+xlab("Years")+
  ggtitle("CoCoNet\nControl effort (# vessels)")+
  scale_x_discrete(breaks = c("2025", "2035", "2045"))+
  theme(panel.background = element_rect(fill="grey96"))


filename="AreaBenefits_CoCoNet_GBR.jpeg"
ggsave(filename=filename, width = 9, height = 3, dpi=300)


##Some stats for 2032 (Max. yearly benefits before cover declines)
B%>%filter(Year==2032)%>%group_by(stratnames)%>%summarise(mean_area=mean(AreaGained_CH), mean_pbenef=mean(PropBenefit_v2))
B%>%filter(Year==2037)%>%group_by(stratnames)%>%summarise(mean_area=mean(AreaGained_CH), mean_pbenef=mean(PropBenefit_v2))



##----------
##Figure S1: Compare control-cover trajectories vs no-CoTS counterfactual
##----------
Covdat=edit_labels(Covdat)

f5= ggplot(subset(Covdat, Boats==5),aes(x=as.numeric(Year), y=MeanCover))+
  stat_summary(fun.data=mean_sd, geom="ribbon", fill="#9ECAE1")+
  stat_summary(fun=mean, geom="line", color="#08519C")+
  ylim(0,36)+
  ylab("")+xlab("Years")+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover), fun.data=mean_sd, geom="ribbon", fill=c("#FFE0E0"),alpha=0.2)+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover),fun=mean, geom="line", color="red", linetype="dashed")+theme_bw()

f14= ggplot(subset(Covdat, Boats==14),aes(x=as.numeric(Year), y=MeanCover))+
  stat_summary(fun.data=mean_sd, geom="ribbon", fill="#9ECAE1")+
  stat_summary(fun=mean, geom="line", color="#08519C")+
  ylim(0,36)+
  ylab("")+xlab("Years")+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover), fun.data=mean_sd, geom="ribbon", fill=c("#FFE0E0"),alpha=0.2)+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover),fun=mean, geom="line", color="red", linetype="dashed")+theme_bw()
f30= ggplot(subset(Covdat, Boats==30),aes(x=as.numeric(Year), y=MeanCover))+
  stat_summary(fun.data=mean_sd, geom="ribbon", fill="#9ECAE1")+
  stat_summary(fun=mean, geom="line", color="#08519C")+
  ylim(0,36)+
  ylab("")+xlab("Years")+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover), fun.data=mean_sd, geom="ribbon", fill=c("#FFE0E0"),alpha=0.2)+
  stat_summary(data=c00,aes(x=as.numeric(Year), y=cover),fun=mean, geom="line", color="red", linetype="dashed")+theme_bw()


##Add legends
ann_text5<-data.frame(Year=c(2047, 2047),
                      MeanCover=c(35,29),
                     label=c("No CoTS","CoTS control\n 5 vessels"))
ann_text14<-data.frame(Year=c(2047, 2047),
                       MeanCover=c(35,29),
                      label=c("No CoTS","CoTS control\n 14 vessels"))
ann_text30<-data.frame(Year=c(2047, 2047),
                       MeanCover=c(35,29),
                      label=c("No CoTS","CoTS control\n 30 vessels"))


f5=f5 + geom_text(data = ann_text5, label=ann_text5$label, color= c("red","#3182BD"),size=3)
f14=f14 + geom_text(data = ann_text14, label=ann_text14$label, color= c("red","#3182BD"),size=3)
f30=f30 + geom_text(data = ann_text30, label=ann_text30$label, color= c("red","#3182BD"),size=3)

ggarrange(f5,f14,f30, labels=c("D","E","F"), ncol = 1,align = "v") 

filename="Control_vs_NoCoTS_CoCoNET_SFigure.jpeg"
ggsave(filename=filename, width = 5, height = 8, dpi=300)

##----------
##Figure 3
##----------

Outbreaks=data.frame()

for (d in outputs) {
  
  load(paste(filepath,d,sep="/"))
  #Extract the name of scenario
  d%<>%str_replace("ALLoutputs_new", "") 
  d%<>%str_sub(1,nchar(d)-2) ## removes file extension
  
  model1%<>%as.data.frame()%>%edit_variable_names()
  ##Count outbreaks per year
  model1%<>%
    group_by(Year,simul)%>%
    summarise_at(vars(n),sum)%>%as.data.frame()
  
  ##Proportion relative to counterfactual 
  model1$PBenefit=(model1$n-cc01$n)/cc01$n 
  ##If no Outbreaks on counterfactual, PB=Inf so those are absolute gains (of 100%)
  model1%<>%mutate(PBenefit=ifelse(PBenefit==Inf, model1$n*1, PBenefit))  
  model1$strat=d
  Outbreaks=rbind(Outbreaks,as.data.frame(model1))
  rm(model1)
  
}

Outbreaks=edit_labels(Outbreaks)



##ONly GBR-wide

Outbreaks%>%filter(strat=="GBR")%>%as.data.frame()%>%
  ggplot(aes(x=as.factor(Year), y=PBenefit,fill=stratnames))+
  geom_hline(yintercept=0, linetype=2, color="grey")+
  geom_boxplot(outlier.size=0.5, outlier.alpha = 0.5, lwd=0.3, fatten=2, alpha=.6)+
  stat_mean(size=0.7, color="red")+
  facet_grid(cols=vars(Boats))+
  scale_fill_manual(values=c(brewer.pal(6,'Blues')[-1]))+
  theme(legend.position = "none",axis.text.x=element_text(size=10))+
  ylab("Relative Change in number of outbreaking reefs")+xlab("Years")+ggtitle("CoCoNet")+ 
  scale_x_discrete(breaks = c("2025", "2035", "2045"))+theme(panel.background = element_rect(fill="grey96"))

filename="OutbreakBenefits_CoCoNet_GBR.jpeg"
ggsave(filename=filename, width = 9, height = 3, dpi=300)


rm(model_delta)


# ##----------------------------------------------------
# Number of culled reefs
# ##----------------------------------------------------
outputs=list.files(path = filepath, pattern=c("Total_Reefculled_newGBR"))##ALL culled Reefs


TotalCulled=data.frame()##Total Area culled
yearculled=data.frame()##Total number of reefs culled

for (d in outputs) {
  
  load(paste(filepath,d,sep="/"))
  #Extract the name of scenario
  d%<>%str_replace("Total_Reefculled_new", "") 
  d%<>%str_sub(1,nchar(d)-2) ## removes file extension
  
  names(Totculled)=c("simul","Year","ReefID","cvisits")##cumulative visits
  
  ##Count culled per year
  Totculled%<>%arrange(simul,ReefID)
  Totculled%<>%group_by(ReefID,simul)%>%mutate(lagvisit=lag(cvisits))
  Totculled%<>%group_by(ReefID,simul)%>%mutate(visited=cvisits-lagvisit)##visit per year
  Totculled%<>%mutate(visited= ifelse(is.na(visited), 1, visited))

   totyear=Totculled%>%group_by(simul,Year)%>%summarise(tot=sum(visited))%>%data.frame()
   totyear$strat=d
  
   Totculled%<>%filter(visited==1)%>%left_join(reefarea)
   Totculled%<>%group_by(simul,Year)%>%
   summarise(visitedArea=sum(CHarea))%>%as.data.frame()
  
  Totculled$strat=d
  
  yearculled=rbind(yearculled, as.data.frame(totyear))
  TotalCulled=rbind(TotalCulled,as.data.frame(Totculled))
  
  rm(Totculled,totyear)
  
}

TotalCulled=edit_labels(TotalCulled)##Total reef area culled
yearculled=edit_labels(yearculled)##Total culled reefs per year

  
###############################################
##Figure 4. Model effect of effort on benefits
##############################################

#Use GBR-wide benefits of coral area as response variable
#Use the TOTAL reef area controlled (i.e. sum over controlled reefs) as covariate


##ADD the Total area of controlled reefs
B%<>%dplyr::left_join(TotalCulled)%>%as.data.frame()
B$Boats=as.numeric(as.character(B$Boats))

##------------------------------
##Only additive- GBR strategy
##------------------------------

GBRstrat=B%>%dplyr::filter(strat=="GBR")%>%droplevels()

hist(GBRstrat$AreaGained)#very skewed distribution

##Use Gaussian?
Mod1gam=mgcv::gam(AreaGained_CH~s(Boats,k=3)+visitedArea+s(Year,k=3), data=GBRstrat, method = "REML")#Min k=3
mgcv::gam.check(Mod1gam, type="response")##residuals "roughly normally distributed"
summary(Mod1gam)
plot(Mod1gam)

Preds<-predict_gam(Mod1gam, values = list(Boats=seq(1,30),Year = c(2025, 2035, 2045)))

m1<-ggplot(Preds,aes(Boats, fit,group=Year)) +
  scale_fill_manual(values=c(brewer.pal(5,'YlOrRd')[-1]), aesthetics = c("color","fill"))+
  geom_smooth(aes(color=as.factor(Year)))+ylim(-100,200)+
  geom_abline(intercept = 0, slope=0,linetype="dashed",color="red")+ ggtitle("CoCoNet")+
  xlab("Number of control vessels")+ ylab(expression(Area~of~live~coral~gained~(Km^2)))+
  theme_bw()+theme(legend.position = "none",panel.grid=element_blank())+
  annotate( "text", x = 25, y = mean(Preds$fit[Preds$Year==2025 & Preds$Boats==5]+10), label ="2025",size=3,color=brewer.pal(5,'YlOrRd')[2])+
  annotate("text", x = 25, y = mean(Preds$fit[Preds$Year==2035 & Preds$Boats==5]+10), label ="2035",size=3,color=brewer.pal(5,'YlOrRd')[3])+
  annotate("text", x = 25, y = mean(Preds$fit[Preds$Year==2045 & Preds$Boats==2]+10), label ="2045",size=3,color=brewer.pal(5,'YlOrRd')[4])


####-----------------
#Model2 for Outbreaks
####-----------------

Outbreaks%<>%dplyr::left_join(TotalCulled)%>%as.data.frame()
Outbreaks$Boats=as.numeric(as.character(Outbreaks$Boats))

##------------------------------
##Only additive- GBR strategy
##------------------------------
GBRstrat2=Outbreaks%>%dplyr::filter(strat=="GBR")%>%dplyr::select(Year,Boats,visitedArea,n)%>%as.data.frame()
hist(GBRstrat2$n)####Count data, very skewed distribution
summary(GBRstrat2)


##TRY Quasipoisson
Mod2qp_gam=mgcv::gamm(n~s(Boats,k=3)+visitedArea+s(Year,k=3), data=GBRstrat2,family="quasipoisson")
summary(Mod2qp_gam$gam)#29%R2
Mod2qp_gam<-Mod2qp_gam$gam
mgcv::gam.check(Mod2qp_gam)##Better distribution of residuals  but not great fitted values

par(mfrow=c(2,2))
plot(Mod2qp_gam)##Keep


M2preds_2=mgcv::predict.gam(Mod2qp_gam,type="response",se.fit = TRUE)

fitted<-M2preds_2$fit
fit_se<-M2preds_2$se.fit
##Add fitted to data
GBRstrat2$fit=fitted
GBRstrat2$se.fit=fit_se

GBRdata=GBRstrat2%>%filter(GBRstrat2$Year%in%c(2025,2035,2045))

m2<-GBRdata%>%ggplot(aes(Boats, fit,group=Year)) +
  scale_fill_manual(values=c(brewer.pal(4,'YlOrRd')[-1]), aesthetics = c("color","fill"))+
  geom_smooth(aes(color=as.factor(Year)))+ylim(0,900)+
  xlab("Number of control vessels")+ ylab("Number of outbreaking reefs")+
  theme_bw()+theme(legend.position = "none",panel.grid=element_blank())+
  annotate( "text", x = 4, y = mean(GBRdata$fit[GBRdata$Year==2025 & GBRdata$Boats==2]-50), label ="2025",size=3,color=brewer.pal(5,'YlOrRd')[2])+
  annotate("text", x = 4, y = mean(GBRdata$fit[GBRdata$Year==2035 & GBRdata$Boats==2]), label ="2035",size=3,color=brewer.pal(5,'YlOrRd')[3])+
  annotate("text", x = 4, y = mean(GBRdata$fit[GBRdata$Year==2045 & GBRdata$Boats==5]-100), label ="2045",size=3,color=brewer.pal(5,'YlOrRd')[4])

ggarrange(m1,m2,ncol = 1,align = "v", labels=c("C","D"))


filename=paste0("BoatsEffect_GBRmod2_CoCoNet.jpeg")
ggsave(filename=filename, width = 3, height = 6, dpi=300)


##Plot cots distribution with RM
save(dat01, file="CoCoNet_counterfactual.R")


##----------------------------------------------------
## MAP spatial distribution of Cots at start of Control
##----------------------------------------------------

GBRpolygons=GBRpolygons[c(1,3,8,13,6,7,15:17)]


## GBRMPA management Areas
Mngarea=readOGR(dsn=path.expand("C:/Users/uqccastr/OneDrive - The University of Queensland/2021/SEES/Data/GBR_offshore_Analyses/GBRMPA_shapefiles"), layer="Management_Areas")
Mngarea <- tidy(Mngarea, region = "AREA_DESCR")
Mngarea$group <- gsub(" Management Area.1", "", Mngarea$group)


## Reef Polygons
gbr<-readOGR(dsn = "C:/Users/uqccastr/Dropbox/GBRMPA_Data", layer="Great_Barrier_Reef_Features")
gbr=gbr[gbr@data$FEAT_NAME=="Reef",]
gbr<-gSimplify(gbr, tol=0.00001)
crs(gbr)
projection(gbr)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

# ##Crop Map for plotting
# GBR=GBRpolygons
# coordinates(GBR)= ~LON+LAT
# extm=extent(GBR@bbox[1],GBR@bbox[3],GBR@bbox[2],GBR@bbox[4])
# gbr<-crop(gbr,extm)

#Add location to reefs
GBRpolygons%<>%dplyr::select(LABEL_ID,LAT,LON)
GBRpolygons$LABEL_ID=as.character(GBRpolygons$LABEL_ID)

##CoCoNet's counterfactual
CoCoMod=dat01%>%dplyr::left_join(GBRpolygons,by=c("ReefID"="LABEL_ID"))%>%
  filter(Year > 2018)%>% dplyr::select(Year,simul,ReefID,LAT,LON,cotsdens)%>%
  group_by(Year,ReefID,LAT,LON)%>%
  summarise_at(vars(cotsdens),mean)%>%##average across simulations
  filter(cotsdens>=0.22)%>%#show only outbreaking reefs
  arrange(cotsdens)%>%data.frame()


##RM counterfactual
#Load counterfactual trends of no control
load("ReefMod_01_Data.R")

names(dat)=c("reftaxa_df","refcov_df","reftow_df")


## Year GBR-wide Average 

ReefMod=dat$reftow_df%>%filter(years>2018)%>%dplyr::group_by(years,reefID,LAT,LON,simul)%>%summarise(cotsdens=mean(cotsdens))%>%ungroup()
rm(dat)
ReefMod%<>%dplyr::group_by(years,reefID,LAT,LON)%>%summarise(cotsdens=mean(cotsdens))%>%ungroup()
ReefMod%<>%filter(cotsdens>=0.22)%>%#show only outbreaking reefs
arrange(cotsdens)

CoCoMod$Mod="CoCoNet"
ReefMod$Mod="ReefMod_GBR"  
ReefMod%<>%as.data.frame()
CoCoMod=CoCoMod[-1]

names(ReefMod)[c(1,2)]=c("Year","ReefID")


COTS<-rbind(CoCoMod,ReefMod)
rm(CoCoMod,ReefMod)

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

##Plot only few years
COTS2=COTS%>%filter(Year %in% c(2020,2025,2030,2035,2040,2045))%>%droplevels()%>%data.frame()
##Compare Model Initial Conditions

levels(as.factor(COTS2$Mod))
##Arrange Order to plot
COTS2$Mod <- factor(COTS2$Mod, levels = c("ReefMod-GBR", "CoCoNet"))


gmap=ggplot() +
  geom_polygon(data = Mngarea, aes(x = long, y = lat, group = group),fill=c("#D2EAFA"), alpha=0.4)+  
  geom_polygon(data = gbr, aes(x = long, y = lat,group = group),colour="grey89", fill=NA,alpha=0.3)+
  geom_point(COTS2, mapping=aes(colour=cotsdens, x=LON, y=LAT), size=1)+labs(fill = "CoTS per tow")+
  scale_colour_gradientn(name="CoTS per tow", colours = c("#FCD65B", "#F27435E6", "#FC0F0F"))+#trans="log1p",
  coord_fixed()+
  coord_map(orientation = c(55,0,0))+theme_classic()+ditch_the_axes+
  facet_grid(vars(Mod),vars(Year))+
  theme(legend.position = "bottom")#,panel.spacing.x = unit(-0.5, "lines"))


##Adjust panel size
require(grid)
gt=ggplot_gtable(ggplot_build(gmap))
##Check panel positions
gtable::gtable_show_layout(gt)
gt$widths[c(5,7,9,11,13,15)]=0.8*gt$widths[5]
grid.draw(gt)

filename=paste0("CoTS_MAP_Counterfactuals.jpeg")
ggsave(filename=filename, width = 8, height =4, dpi=300)


save.image("CoCoNet_summarised_Outputs_for_Paper.R")

