rm(list=ls())
memory.limit(size=30000)

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
library(sf)

##--------------------------------
##Edit table labels for scenarios
##--------------------------------
edit_labels<-function(x){

  ##Edit Strat Names
  x$Boats=x$strat
  x$Boats<-sub("...", "", x$Boats)
  x$Boats<-str_replace(x$Boats, "b", "")
  ##Arrange by Boats
  x$Boats <- factor(x$Boats, levels = c("2", "5", "8","14","30"))
  ##Create New strat label
  x$stratnames=x$strat
  x$strat<-substr(x$strat, 1,3)
  x=x%>%filter(strat=="14_")
  x$strat<- str_replace(x$strat, "14_", "GBR")
  
  ##Arrange stratnames
  x$stratnames <- factor(x$stratnames, levels = c("14_2b", "14_5b", "14_8b","14_14b","14_30b"))
  return(as.data.frame(x))
}



#######################################
##Figure 1a. Counterfactual of cover
#######################################


#Load counterfactual trends of no control
load("ReefMod/ReefMod_01_Data.R")

dat01=dat
names(dat01)=c("reftaxa_df","refcov_df","reftow_df")
rm(dat)
#Load counterfactual trends of no CoTS
load("ReefMod/ReefMod_00_Data.R")

dat00=dat
names(dat00)=c("reftaxa_df","refcov_df")
rm(dat)


## Year GBR-wide Average 

c01=dat01$refcov_df%>%filter(years>2017)%>%group_by(years,reefID,simul)%>%
         summarise_at(vars(cover),mean)%>%group_by(years,simul)%>%
  summarise_at(vars(cover),mean)

c00=dat00$refcov_df%>%filter(years>2017)%>%group_by(years,reefID,simul)%>%
  summarise_at(vars(cover),mean)%>%group_by(years,simul)%>%
  summarise_at(vars(cover),mean)
rm(dat00)

##Add legend
ann_text<-data.frame(years=c(2047, 2047),
                     cover=c(33,31),
                     label=c("No CoTS","No CoTS control"))



p1= ggplot(c01,aes(x=as.numeric(years), y=cover))+
  stat_summary(fun.data=mean_sd, geom="ribbon", fill="grey75")+
  stat_summary(fun=mean, geom="line", color="black")+
  ggtitle("ReefMod-GBR\nCounterfactual of coral cover")+
  theme(legend.position = "bottom")+ylim(0,36)+
  ylab("Total coral cover (%)")+xlab("Years")+
  stat_summary(data=c00,aes(x=as.numeric(years), y=cover), fun.data=mean_sd, geom="ribbon", fill=c("#FFE0E0"),alpha=0.2)+
  stat_summary(data=c00,aes(x=as.numeric(years), y=cover),fun=mean, geom="line", color="red", linetype="dashed")+theme_bw()


p1=p1 + geom_text(data = ann_text, label=ann_text$label, color= c("red","black"),size=2)
 

#############################################
 ##Figure 1b. Counterfactual of CoTS Outbreaks
#############################################

 
 cots01=dat01$reftow_df%>%filter(years>2017)%>%filter(cotsdens>=0.22)%>%group_by(simul,years_ts,years)%>%count()%>%
 group_by(years,simul)%>%
 summarise_at(vars(n),sum)##sum per year
 rm(dat01)
 
 p2=ggplot(cots01,aes(x=as.numeric(years), y=n))+
   stat_summary(fun.data=mean_sd, geom="ribbon", fill="grey75",color="grey75")+
   stat_summary(fun=mean, geom="line", color="black")+
   ggtitle("ReefMod-GBR\nCounterfactual of outbreaking reefs")+
   theme(legend.position = "bottom")+
   ylim(0,1900)+##To match CoCoNet plots
   ylab("Number of outbreaking reefs \n(CoTS density per tow > 0.22)")+xlab("Years")+
  theme_bw()


ggarrange(p1,p2, labels=c("A","B"), ncol = 1,align = "v") 


filename="Counterfactuals_RM.jpeg"
ggsave(filename=filename, width = 4, height = 6, dpi=300)


#############################################
##Figure 3. GBR-wide Benefits in Corals
############################################

#Load Benefits of coral
load("CoralAreaBenefits.Rdata")

##Create single Table

B=do.call(rbind, GBRbenef)##GBR-wide

rm(GBRbenef)

## ---------------------------------------------
##BoxPlot of Total GBR-wide Coral Area Benefits
## ---------------------------------------------

B=edit_labels(B)

#Convert Benefits as a proportion of cover on a healthy reef
coeff<-2219 # (i.e. 6,528 km2 of CH * 34 % cover).  ##the area of healthy (34%) GBR in 1990 -- 
B$PropBenefit= B$AreaGained/coeff

ggplot(subset(B, strat=="GBR"),aes(x=as.factor(Year), y=AreaGained,fill=stratnames))+
  geom_boxplot(outlier.size=0.5, outlier.alpha = 0.5, lwd=0.3, fatten=2, alpha=.6)+
  scale_y_continuous(sec.axis=sec_axis(~./coeff*100, name="Percentage of healthy GBR Area"))+
  stat_mean(size=0.7, color="red")+
  facet_grid(cols=vars(Boats))+
  scale_fill_manual(values=c(brewer.pal(6,'Blues')[-1]))+
  theme(legend.position = "none",axis.text.x=element_text(size=10))+
  ylab(expression(Area~of~live~coral~gained~(Km^2)))+xlab("Years")+
  xlab("years")+ggtitle("Control effort (# vessels)")+
  scale_x_discrete(breaks = c("2025", "2035", "2045"))+
  theme(panel.background = element_rect(fill="grey96"))#,strip.text.y = element_blank())#remove facet label


filename="AreaBenefits_ReefMod.jpeg"
ggsave(filename=filename, width = 9, height = 3, dpi=300)


##Some stats for 2041 (Mean yearly benefits)
B%>%filter(Year==2041)%>%group_by(stratnames)%>%summarise(mean_area=mean(AreaGained), mean_pbenef=mean(PropBenefit))
B%>%filter(Year==2037)%>%group_by(stratnames)%>%summarise(mean_area=mean(AreaGained), mean_pbenef=mean(PropBenefit))


################################################
##Figure 4. GBR-wide Benefits in CoTS outbreaks
###############################################

#Load Benefits 
load("Outbreaks_Data.R")#summarised
#load("CoTS_density.R")#raw

Outbreaks=edit_labels(Outbreaks)

#Create years for plotting
Outbreaks$Year=Outbreaks$years_ts
Outbreaks$Year<- as.numeric(substr(Outbreaks$Year,1,4))


##SUM Active+Incipient outbreaks per year (remove 2050 as control stop the year before end of simulations)
Outbreaks%<>%filter(Year>2018 & Year<2050)%>%group_by(stratnames,strat,Boats,simul,years_ts,Year)%>%
  summarise(n=sum(n),cnt_n=sum(cnt_n),benefit=sum(benefit))%>%data.frame()
#Sum time-steps for Total per year
Outbreaks%<>%group_by(stratnames,strat,Boats,simul,Year)%>%summarise(n=sum(n),cnt_n=sum(cnt_n),benefit=sum(benefit))%>%data.frame()

##Proportion of outbreaks lost
Outbreaks$PBenefit=(Outbreaks$n-Outbreaks$cnt_n)/Outbreaks$cnt_n
summary(Outbreaks$PBenefit)


##Visualise as % of outbreaks lost

Outbreaks%>%
  ggplot(aes(x=as.factor(Year), y=PBenefit,fill=stratnames))+
  geom_hline(yintercept=0, linetype=2, color="grey")+
  geom_boxplot(outlier.size=0.5, outlier.alpha = 0.5, lwd=0.3, fatten=2, alpha=.6)+
  stat_mean(size=0.7, color="red")+
  facet_grid(cols=vars(Boats))+
  scale_fill_manual(values=c(brewer.pal(6,'Blues')[-1]))+
  theme(legend.position = "none",axis.text.x=element_text(size=10))+
  ylab("Relative Change in number of outbreaking reefs")+xlab("Years")+ggtitle("ReefMod-GBR Control effort (# vessels)")+ 
  scale_x_discrete(breaks = c("2025", "2035", "2045"))+theme(panel.background = element_rect(fill="grey96"))

filename="OutbreakBenefits_RM_GBR.jpeg"
ggsave(filename=filename, width = 9, height = 3, dpi=300)

meanbenefit=Outbreaks%>%group_by(Boats,Year)%>%summarise_all(funs(mean))
meanbenefit%<>%group_by(Boats)%>%mutate(cum_n=cumsum(benefit), cum_prop=cumsum(PBenefit))

Outbreaks%>%filter(Boats==5 & Year==2049)%>%summary()
Outbreaks%>%filter(Boats==8 & Year==2049)%>%summary()
Outbreaks%>%filter(Boats==14 & Year==2049)%>%summary()
Outbreaks%>%filter(Boats==30 & Year==2049)%>%summary()
# 
##--------------------------------------------------------------------
## Supplementary Figure S4
##--------------------------------------------------------------------
#load index of visited reefs
load("ControlledReefs_idx.Rdata")

##Extract reef count per year (i.e. only if visited at that given year)
Totcull=controlled%>%group_by(strat,simul,Year)%>%summarise(total=sum(culled))
Totcull%<>%edit_labels()

##Need to extract the reefID of controlled reefs only, first ID how many times each reef was visited
##Out of 60 time-steps, select reefs visited at least 8 times (75% quantile)-- 
Totcull%>%ggplot(aes(x=as.factor(Year), y=total, color=stratnames)) + 
  stat_summary(fun=mean, geom="line")+
  stat_summary(fun.data=mean_sd, geom="pointrange",size=0.1)+
  facet_grid(cols=vars(Boats))+
  scale_color_manual(values=c(brewer.pal(6,'Blues')[-1]))+
  ylab("Culled reefs")+xlab("Years")+
  theme(legend.position = "none",axis.text.x=element_text(size=10))+ggtitle("ReefMod-GBR")+
  scale_x_discrete(breaks = c("2025", "2035", "2045"))+
  theme(panel.background = element_rect(fill="grey98"))

filename="Number_CulledReefs_RM.jpeg"
ggsave(filename=filename, width = 9, height = 3, dpi=300)



###############################################
##Figure 4. Model effect of effort on benefits
##############################################

#Model1
#Use GBR-wide benefits of coral area as response variable
#Use the TOTAL reef area controlled (i.e. sum over controlled reefs) as covariate


##ADD the Total area of controlled reefs
controlled%<>%edit_labels()
head(GBRpolygons)

reefarea=GBRpolygons%>%dplyr::select(OrderInMatrix,GeomorphicAreaKm2)%>%mutate(reefID=as.character(OrderInMatrix))%>%dplyr::select(-OrderInMatrix)
totarea=controlled%>%filter(strat=="GBR")%>%dplyr::select(reefID,simul,Year,Boats)

totarea%<>%left_join(reefarea)##
totarea%<>%group_by(simul,Year,Boats)%>%summarise(TotalArea=sum(GeomorphicAreaKm2))
totarea$Boats=as.numeric(as.character(totarea$Boats))


##------------------------------
##Only additive- GBR strategy
##------------------------------

GBRstrat=B%>%mutate(Boats=as.numeric(as.character(Boats)))
GBRstrat%<>%dplyr::left_join(as.data.frame(totarea))%>%na.omit()##will drop 2050 data


hist(GBRstrat$AreaGained)#close to normal
summary(GBRstrat$AreaGained)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-64.70   21.87   56.78   73.44  105.06  386.67 

Mod1gam=mgcv::gam(AreaGained~s(Boats,k=3)+TotalArea+s(Year,k=3), data=GBRstrat, method = "REML")#Min k=3

mgcv::gam.check(Mod1gam)
summary(Mod1gam)##Good Fit
plot(Mod1gam)


##Plot conditional to Year
visreg(Mod1gam,"Boats", by="Year", overlay=TRUE,partial=TRUE,line=list(col=c("yellow","orange","red")), fill=list(col=c("yellow","orange","red")),##ONe per year (2025,2035,2045)
    jitter=TRUE, points=list(col="gray80",cex=0.3,pch=1),
    ylab=(expression(Total~coral~cover~area~gained~(Km^2))))+ theme_classic()

filename=paste0("BoatsEffect_GBRmod.jpeg")
#ggsave(filename=filename, width = 3, height = 3, dpi=300)


Preds<-predict_gam(Mod1gam, values = list(Boats=seq(1,30),Year = c(2025, 2035, 2045)))# %>%
  #filter(Year==2030)%>%

m1<-ggplot(Preds,aes(Boats, fit,group=Year)) +
  scale_fill_manual(values=c(brewer.pal(5,'YlOrRd')[-1]), aesthetics = c("color","fill"))+
  #geom_jitter(data=GBRstrat, aes(x=Boats,y=AreaGained),alpha=0.1, color="grey88",width = 0.35)+##Add obs
  geom_smooth(aes(color=as.factor(Year)))+
  geom_abline(intercept = 0, slope=0,linetype="dashed",color="red")+ ylim(-100,200)+ggtitle("ReefMod-GBR")+
  xlab("Number of control vessels")+ ylab(expression(Area~of~live~coral~gained~(Km^2)))+
   theme_bw()+theme(legend.position = "none",panel.grid=element_blank())+
   annotate( "text", x = 25, y = mean(Preds$fit[Preds$Year==2025 & Preds$Boats==25])+8, label ="2025",size=3,color=brewer.pal(5,'YlOrRd')[2])+
   annotate("text", x = 25, y = mean(Preds$fit[Preds$Year==2035 & Preds$Boats==25])+8, label ="2035",size=3,color=brewer.pal(5,'YlOrRd')[3])+
   annotate("text", x = 25, y = mean(Preds$fit[Preds$Year==2045 & Preds$Boats==25])+8, label ="2045",size=3,color=brewer.pal(5,'YlOrRd')[4])



filename="BoatsEffect_GBRmod_RM.jpeg"
ggsave(filename=filename, width = 3, height = 3, dpi=300)


####-----------------
# Model for Outbreaks
####-----------------

#Use GBR-wide Number of outbreaks as response variable
##ADD the area of controlled reefs

GBRstrat2=Outbreaks%>%dplyr::select(Year,simul,Boats,n)%>%as.data.frame()
GBRstrat2$Boats=as.numeric(as.character(GBRstrat2$Boats))
GBRstrat2%<>%left_join(totarea)%>%na.omit()

##------------------------------
##Only additive- GBR strategy
##------------------------------

Mod2qp_gam=mgcv::gamm(n~s(Boats,k=3)+TotalArea+s(Year,k=3), data=GBRstrat2,family="quasipoisson")
summary(Mod2qp_gam$gam)##Area non-significant
Mod2qp_gam<-Mod2qp_gam$gam
mgcv::gam.check(Mod2qp_gam)##Better fitted values

Mod2qp_gam<-Mod2qp_gam$gam
M2preds_2=mgcv::predict.gam(Mod2qp_gam,type="response",se.fit = TRUE)

fitted<-M2preds_2$fit
fit_se<-M2preds_2$se.fit
##Add fitted to data
GBRstrat2$fit=fitted
GBRstrat2$se.fit=fit_se

GBRdata=GBRstrat2%>%filter(GBRstrat2$Year%in%c(2025,2035,2045))

m2<-GBRdata%>%ggplot(aes(Boats, fit,group=Year)) +
  scale_fill_manual(values=c(brewer.pal(4,'YlOrRd')[-1]), aesthetics = c("color","fill"))+
  geom_smooth(aes(color=as.factor(Year)))+
  xlab("Number of control vessels")+ ylab("Number of outbreaking reefs")+
  theme_bw()+theme(legend.position = "none",panel.grid=element_blank())+ylim(0,900)+
  annotate( "text", x = 4, y = mean(GBRdata$fit[GBRdata$Year==2025 & GBRdata$Boats==5]-40), label ="2025",size=3,color=brewer.pal(5,'YlOrRd')[2])+
  annotate("text", x = 4, y = mean(GBRdata$fit[GBRdata$Year==2035 & GBRdata$Boats==5]-35), label ="2035",size=3,color=brewer.pal(5,'YlOrRd')[3])+
  annotate("text", x = 4, y = mean(GBRdata$fit[GBRdata$Year==2045 & GBRdata$Boats==5]-30), label ="2045",size=3,color=brewer.pal(5,'YlOrRd')[4])

ggarrange(m1,m2,ncol = 1,align = "v", labels=c("A","B"))

filename=paste0("BoatsEffect_GBRmod2_RM.jpeg")
ggsave(filename=filename, width = 3, height = 6, dpi=300)


##------------------------------------
### Spatial Distribution of Outbreaks
##------------------------------------

## GBRMPA management Areas--NOT same as areas of prioritization, but use as template?
Mngarea=readOGR(dsn=path.expand("GBRMPA_shapefiles"), layer="Management_Areas")
Mngarea <- tidy(Mngarea, region = "AREA_DESCR")
Mngarea$group <- gsub(" Management Area.1", "", Mngarea$group)


gbr <- readOGR(dsn=path.expand("GBRMPA_shapefiles"), layer="Great_Barrier_Reef_Features")##as spatialpolygonDataFrame
gbr=gbr[gbr@data$FEAT_NAME=="Reef",]
gbr <- gSimplify(gbr, tol = 0.00001)##if error
crs(gbr)
projection(gbr)<-CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")

##Crop Map for plotting
GBR=GBRpolygons
coordinates(GBR)= ~LON+LAT
extm=extent(GBR@bbox[1],GBR@bbox[3],GBR@bbox[2],GBR@bbox[4])
gbr1<-crop(gbr,extm)

dat01$reftow_df%>%
  filter(years > 2020)%>% group_by(years,simul,reefID,ReefName,LAT,LON)%>%
  summarise_at(vars(cotsdens),mean)%>%##average across seasons
  group_by(years,reefID,ReefName,LAT,LON)%>%
  summarise_at(vars(cotsdens),mean)%>%##average across simulations
  filter(cotsdens>0.219)%>%#show only outbreaking reefs
  arrange(cotsdens)%>%## to plot larger values last
  ggplot(aes(x=LON, y=LAT))+
  geom_polygon(data=gbr1, aes(group=group,x=long,y=lat),fill="White", alpha=0.8)+
  geom_point(aes(colour=cotsdens), size=1.2)+labs(fill = "CoTS per tow")+
  ggtitle(title)+
  facet_wrap(~years)+#, scales="free")+
  #scale_colour_gradientn(name="CoTS per tow", colours = c("grey86","orange","red"),breaks=c(0.22, 1, 3))+
  scale_colour_gradientn(name="CoTS per tow", trans="log1p",colours = c("grey86","orange","red"))+#,
  theme(panel.background = element_rect(fill="slategray1"), panel.grid = element_line(color="slategray1"))



###--------------------------------------------------------------------------------
## Figure 2 (Maps): Spatial extent of Control within Management Areas-- highlight PR reefs
###--------------------------------------------------------------------------------


##Load PRreefs
Preefs=read.csv("COTS/PriorityReefs_GBRMPA.csv")
Preefs%<>%dplyr::select(-c(X, Reef.index, CSIRO.region))%>%
  filter(Priority=="Y")%>%droplevels()


ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


##Plot GBR-wide scenario
ggplot() +
  geom_polygon(data = Mngarea, aes(x = long, y = lat, group = group, fill=group, alpha=0.5))+  
  scale_fill_manual(values=c(rep("#C5E7FA", 4)), aesthetics = c("color","fill"))+ ##strat=GBR-wide
  geom_polygon(data = gbr, aes(x = long, y = lat,group = group),colour="grey80", fill=NA,alpha=0.6)+
  geom_point(data = Preefs, aes(x = Centroid.LON, y = Centroid.LAT),size=0.2,color=c("#2C32DE"))+
  coord_fixed()+
  coord_map(orientation = c(55,0,0))+theme_classic()+theme(legend.position = "none")+
  ditch_the_axes

filename=paste0("GBRstrat.jpeg")
ggsave(filename=filename, width = 3, height = 3, dpi=300)


