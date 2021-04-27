#----------------------------Data Setup------------------------------------
#Set working directory to the source location
library(dplyr)
library(lmtest) #for likelihood ratio test
library(lme4)   #for linear mixed effects model
library(lmerTest)  #for pvalue for mixed effects models(lme4)
library(broom) 	# for augment
library(MuMIn)
library(ggplot2)
library(DHARMa) #for GLMM diagnostics

#Read in data set
GBD.full<-read.csv("GolfBat_ActivitySpreadsheet_Year.csv",header = TRUE,sep=",")
GBD<-filter(GBD.full, Bmean<90)    #Removed Cream Ridge 2020 (only has 2 nights)

#Alternate tall format, for night-by-night numbers
#scaling my data
GBD.night<-read.csv("GolfBat_ActBuzzNights.csv",header = TRUE,sep=",")
GBD.night$Year = factor(GBD.night$Year)
GBD.night$UrbanPct2km = scale(GBD.night$UrbanPct2km,scale = TRUE, center = FALSE)
GBD.night$FieldPct2km = scale(GBD.night$FieldPct2km,scale = TRUE, center = FALSE)
GBD.night$ForestLPI_2km = scale(GBD.night$ForestLPI_2km,scale = TRUE, center = FALSE)
GBD.night$EdgeDen_mperhect_2km = scale(GBD.night$EdgeDen_mperhect_2km,scale = TRUE, center = FALSE)
GBD.night$AgriculturePct2km = scale(GBD.night$AgriculturePct2km,scale = TRUE, center = FALSE)
GBD.night$SuburbanPct2km = scale(GBD.night$SuburbanPct2km,scale = TRUE, center = FALSE)
GBD.night$ForestedStreams_km_2km = scale(GBD.night$ForestedStreams_km_2km,scale = TRUE, center = FALSE)
GBD.night$Hectares = scale(GBD.night$Hectares,scale = TRUE, center = FALSE)

#Remove HiF,LoF,Myse, and Myle
GBD.night.spec = subset(GBD.night, GBD.night$Species != "LoF" & GBD.night$Species != "HiF" & GBD.night$Species != "Myle" & GBD.night$Species != "Myse")

#Breakdown by species
epfu <- subset(GBD.night, Species == "Epfu", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))
labo <- subset(GBD.night, Species == "Labo", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))
laci <- subset(GBD.night, Species == "Laci", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))
lano <- subset(GBD.night, Species == "Lano", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))
luso <- subset(GBD.night, Species == "Luso", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))
nyhu <- subset(GBD.night, Species == "Nyhu", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))
pesu <- subset(GBD.night, Species == "Pesu", select = c("Site","Year","Night","Act","Buzz","Hectares","Perim_meters","DateBuilt","Altitude_ft","ImpSurf_acres2km","SmallRd_km2km","BigRD_km2km","Roads","ForestedStreams_km_2km","UrbanPct2km","BarrenPct2km","SuburbanPct2km","AgriculturePct2km","ForestPct2km","FieldPct2km","ShrubPct2km","WaterPct2km","WetlandPct2km","ForestLPI_2km","TotEdge_m_2km","EdgeDen_mperhect_2km","ContrastWeightedED_2km","ECON_MN_2km","CLUMPY_2km","InterspersJuxt_2km"))

#Same thing, but combined for all species. Alternate tall format, for night-by-night numbers
GBD.night.tot<-read.csv("GolfBat_ActBuzzTotalNights.csv",header = TRUE,sep=",")
GBD.night.tot$Year = factor(GBD.night.tot$Year)
#Scale the covariates
GBD.night.tot$UrbanPct2km = scale(GBD.night.tot$UrbanPct2km,scale = TRUE, center = FALSE)
GBD.night.tot$FieldPct2km = scale(GBD.night.tot$FieldPct2km,scale = TRUE, center = FALSE)
GBD.night.tot$ForestLPI_2km = scale(GBD.night.tot$ForestLPI_2km,scale = TRUE, center = FALSE)
GBD.night.tot$EdgeDen_mperhect_2km = scale(GBD.night.tot$EdgeDen_mperhect_2km,scale = TRUE, center = FALSE)
GBD.night.tot$AgriculturePct2km = scale(GBD.night.tot$AgriculturePct2km,scale = TRUE, center = FALSE)
GBD.night.tot$SuburbanPct2km = scale(GBD.night.tot$SuburbanPct2km,scale = TRUE, center = FALSE)
GBD.night.tot$ForestedStreams_km_2km = scale(GBD.night.tot$ForestedStreams_km_2km,scale = TRUE, center = FALSE)
GBD.night.tot$Hectares = scale(GBD.night.tot$Hectares,scale = TRUE, center = FALSE)

#-------------------------------Feeding buzz nested models------------------------------
#Quick correlation test on my chosen variables:
sitecov <- subset(epfu, select = c("ForestedStreams_km_2km","UrbanPct2km","SuburbanPct2km","ForestLPI_2km","FieldPct2km","AgriculturePct2km"))
CorrMatrix <- cor(sitecov)
#Eliminated forest edge and forest percent because they are so correlated with Forest LPI
#All between 0.65 - 0.70
#Field vs Ag
ggplot(epfu) + aes(x = FieldPct2km,y=AgriculturePct2km) + 
  geom_point()+geom_smooth(method = "loess", se= FALSE, color = "blue")

#Field vs Urban
ggplot(epfu) + aes(x = FieldPct2km,y=UrbanPct2km) + 
  geom_point()+geom_smooth(method = "loess", se= FALSE, color = "blue")

#Urban vs Suburban
ggplot(epfu) + aes(x = UrbanPct2km,y=SuburbanPct2km) + 
  geom_point()+geom_smooth(method = "loess", se= FALSE, color = "blue")


#Start with checking the family for a global model
#Check error structures
buzz.nb<- glmer.nb(Buzz~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
buzz.pois<- glmer(Buzz~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = GBD.night.tot)
AIC(buzz.nb,buzz.pois) #Negative binomial wins by a lot.

#Now play with the fixed effects
summary(buzz.nb)

#Nested models
#forest LPI is not significant. drop it
buzz1A<- glmer.nb(Buzz~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(buzz1A)
lrtest(buzz.nb,buzz1A)    #This was an okay choice

#urban is not significant. drop it
buzz1B<- glmer.nb(Buzz~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(buzz1B)
lrtest(buzz1A,buzz1B)    #This was an okay choice

#All remaining are significant
#To make sure this is robust, let's try dropping each of these 4 remaining variables
buzz1B.i<- glmer.nb(Buzz~ AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
buzz1B.ii<- glmer.nb(Buzz~ FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
buzz1B.iii<- glmer.nb(Buzz~ FieldPct2km + AgriculturePct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
buzz1B.iv<- glmer.nb(Buzz~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
AIC(buzz1B)
AIC(buzz1B.i) 
AIC(buzz1B.ii)
AIC(buzz1B.iii)
AIC(buzz1B.iv)

summary(buzz1B)      #WINNER WINNER
res_buzz <- simulateResiduals(fittedModel = buzz1B, plot = F)
plot(res_buzz)


#Plotting the GLMM
#Something I found online. 
#Create a new data frame. Simulate data. I have 4 fixed effects. So hold 3 constant, and then vary the 4th.
#I chose to vary suburban %. Site and Year are my random factors. Vary those too, but as factors
buzz.pred.data.sub = expand.grid(ForestedStreams_km_2km=mean(GBD.night.tot$ForestedStreams_km_2km),
                      FieldPct2km=mean(GBD.night.tot$FieldPct2km), 
                      AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km),
                       SuburbanPct2km=seq(0,2,0.05), Year=unique(GBD.night.tot$Year),Site=unique(GBD.night.tot$Site))

#Add the model predictions to the new data frame.
buzz.pred.data.sub$buzz1B_pred = predict(buzz1B, type = "response", newdata=buzz.pred.data.sub)

#Plot the real data vs. your model predictions. Can specify categories for your random effects
#Plot for year as the random effect. With standard error plotted around it
ggplot() +
  geom_point(data=GBD.night.tot, aes(SuburbanPct2km, Buzz, shape=Year, col=Year)) +
  geom_smooth(data=buzz.pred.data.sub, aes(SuburbanPct2km, buzz1B_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover %")+ylab("Feeding Buzzes")

#Let's do it again, but with a different x-axis. This time, Forested streams
buzz.pred.data.stream = expand.grid(SuburbanPct2km=mean(GBD.night.tot$SuburbanPct2km),
                                    FieldPct2km=mean(GBD.night.tot$FieldPct2km), 
                                    AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km),
                                    ForestedStreams_km_2km=seq(0.25,2,0.05), Year=unique(GBD.night.tot$Year),Site=unique(GBD.night.tot$Site))

#Add the model predictions to the new data frame.
buzz.pred.data.stream$buzz1B_pred = predict(buzz1B, type = "response", newdata=buzz.pred.data.stream)

#Plot the real data vs. your model predictions. Can specify categories for your random effects
#Plot for year as the random effect. With standard error plotted around it
ggplot() +
  geom_point(data=GBD.night.tot, aes(ForestedStreams_km_2km, Buzz, shape=Year, col=Year)) +
  geom_smooth(data=buzz.pred.data.stream, aes(ForestedStreams_km_2km, buzz1B_pred, col=Year),se=TRUE) + 
  xlab("Forested Stream Length (km)")+ylab("Feeding Buzzes")

#----------------------------------Total activity nested models----------------------
#Check error structure
act.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
act.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = GBD.night.tot)
AIC(act.nb,act.pois)
summary(act.nb)

#urban is not significant. drop it
act1<- glmer.nb(Act~ FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(act1)
lrtest(act.nb,act1)    #This was an okay choice

#forest LPI is not significant. drop it
act2<- glmer.nb(Act~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(act2)
lrtest(act1,act2)    #This was an okay choice

#streams are not significant. drop it
act3<- glmer.nb(Act~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(act3)
lrtest(act2,act3)   #This was an okay choice

#All remaining are significant
#To make sure this is robust, let's try dropping each of these 3 remaining variables
act3.i<- glmer.nb(Act~ SuburbanPct2km + AgriculturePct2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
act3.ii<- glmer.nb(Act~ FieldPct2km + AgriculturePct2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
act3.iii<- glmer.nb(Act~ FieldPct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = GBD.night.tot)

AIC(act3)
AIC(act3.i)
AIC(act3.ii)
AIC(act3.iii)


summary(act3)      #WINNER WINNER
res_act <- simulateResiduals(fittedModel = act3, plot = F)
plot(res_act)

#Plotting the GLMM
act.pred.data.field = expand.grid(AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km), 
                                  SuburbanPct2km=mean(GBD.night.tot$SuburbanPct2km), 
                                 FieldPct2km=seq(0,2,0.05), Year=unique(GBD.night.tot$Year), Site=unique(GBD.night.tot$Site))
act.pred.data.field$act3_pred = predict(act3, type = "response", newdata=act.pred.data.field)
ggplot() + geom_point(data=GBD.night.tot, aes(FieldPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=act.pred.data.field, aes(FieldPct2km, act3_pred, col=Year),se=TRUE) + 
  xlab("Field Land Cover %")+ylab("Total Bat Recordings")

#Next, Agriculture
act.pred.data.ag = expand.grid(FieldPct2km=mean(GBD.night.tot$FieldPct2km), 
                               SuburbanPct2km=mean(GBD.night.tot$SuburbanPct2km), 
                               AgriculturePct2km=seq(0,2.5,0.05), Year=unique(GBD.night.tot$Year), Site=unique(GBD.night.tot$Site))
act.pred.data.ag$act3_pred = predict(act3, type = "response", newdata=act.pred.data.ag)
ggplot() + geom_point(data=GBD.night.tot, aes(AgriculturePct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=act.pred.data.ag, aes(AgriculturePct2km, act3_pred, col=Year),se=TRUE) + 
  xlab("Agricultural Land Cover %")+ylab("Total Bat Recordings")

#Lastly, suburbs
act.pred.data.sub = expand.grid(FieldPct2km=mean(GBD.night.tot$FieldPct2km), 
                                AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km),
                                SuburbanPct2km=seq(0,2,0.05), Year=unique(GBD.night.tot$Year), 
                                Site=unique(GBD.night.tot$Site))
act.pred.data.sub$act3_pred = predict(act3, type = "response", newdata=act.pred.data.sub)
ggplot() + geom_point(data=GBD.night.tot, aes(SuburbanPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=act.pred.data.sub, aes(SuburbanPct2km, act3_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover")+ylab("Total Bat Recordings")

#---------------------------------Species-specific activity models:--------------------
#-------------------------------------Epfu-------------------------------------
#Start with global model
epfu.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = epfu)
epfu.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = epfu)
AIC(epfu.pois,epfu.nb)
summary(epfu.nb)

#urban is not significant. drop it
epfu1<- glmer.nb(Act~ FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = epfu)
lrtest(epfu.nb,epfu1)    #This was an okay choice
summary(epfu1)

#streams are not significant. drop it
epfu2<- glmer.nb(Act~ FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = epfu)
lrtest(epfu1,epfu2)    #This was an okay choice
summary(epfu2)

#forest lpi is not significant. drop it
epfu3<- glmer.nb(Act~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = epfu)
lrtest(epfu2,epfu3)    #This was an okay choice
summary(epfu3)

#ag is not significant. drop it
epfu4<- glmer.nb(Act~ FieldPct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = epfu)
lrtest(epfu3,epfu4)    #This was an okay choice
summary(epfu4)

#field is not significant. drop it
epfu5<- glmer.nb(Act~ SuburbanPct2km + (1 | Site) + (1 | Year), data = epfu)
lrtest(epfu4,epfu5)    #This was an okay choice
summary(epfu5)

summary(epfu5)      #WINNER WINNER, Note, this is almost the same as the total activity model
res_epfu <- simulateResiduals(fittedModel = epfu5, plot = F)
plot(res_epfu)

#Plotting the GLMM
#Plot Suburban
epfu.pred.data.sub = expand.grid(SuburbanPct2km=seq(0,2,0.05), Year=unique(epfu$Year),Site=unique(epfu$Site))
epfu.pred.data.sub$epfu5_pred = predict(epfu5, type = "response", newdata=epfu.pred.data.sub)
ggplot() + geom_point(data=epfu, aes(SuburbanPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=epfu.pred.data.sub, aes(SuburbanPct2km, epfu5_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover %")+ylab("Total Epfu Recordings")

#-----------------------------------------Labo-------------------------------------------
#Error structure
labo.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
labo.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = labo)
AIC(labo.pois,labo.nb)
summary(labo.nb)

#forest LPI not significant, drop it
labo1<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
lrtest(labo.nb,labo1)    #This was an okay choice
summary(labo1)

#agriculture not significant, drop it
labo2<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
lrtest(labo1,labo2)    #This was an okay choice
summary(labo2)

#All remaining are significant
#To make sure this is robust, let's try dropping each of these 5 remaining variables
labo3A<- glmer.nb(Act~ FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
labo3B<- glmer.nb(Act~ UrbanPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
labo3D<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
labo3E<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = labo)
AIC(labo2)
AIC(labo3A)
AIC(labo3B)
AIC(labo3D)
AIC(labo3E)

summary(labo2)      #WINNER WINNER
res_labo <- simulateResiduals(fittedModel = labo2, plot = F)
plot(res_labo)

#The plots, start with Suburban
labo.pred.data.sub = expand.grid(FieldPct2km=mean(labo$FieldPct2km), UrbanPct2km=mean(labo$UrbanPct2km), 
                                 ForestedStreams_km_2km=mean(labo$ForestedStreams_km_2km),
                                 SuburbanPct2km=seq(0,2,0.05), Year=unique(labo$Year), Site=unique(labo$Site))
labo.pred.data.sub$labo2_pred = predict(labo2, type = "response", newdata=labo.pred.data.sub)
ggplot() +
  geom_point(data=labo, aes(SuburbanPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=labo.pred.data.sub, aes(SuburbanPct2km, labo2_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover %")+ylab("Total Labo Recordings")

#Now field
labo.pred.data.field = expand.grid(UrbanPct2km=mean(labo$UrbanPct2km), 
                                ForestedStreams_km_2km=mean(labo$ForestedStreams_km_2km),
                                SuburbanPct2km=mean(labo$SuburbanPct2km),
                                FieldPct2km=seq(0,2,0.05), Year=unique(labo$Year), Site=unique(labo$Site))
labo.pred.data.field$labo2_pred = predict(labo2, type = "response", newdata=labo.pred.data.field)
ggplot() +
  geom_point(data=labo, aes(FieldPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=labo.pred.data.field, aes(FieldPct2km, labo2_pred, col=Year),se=TRUE) + 
  xlab("Field Land Cover %")+ylab("Total Labo Recordings")

#Now forest streams
labo.pred.data.stream = expand.grid(UrbanPct2km=mean(labo$UrbanPct2km), 
                                    FieldPct2km=mean(labo$FieldPct2km),
                                    SuburbanPct2km=mean(labo$SuburbanPct2km),
                                    ForestedStreams_km_2km=seq(0.25,2,0.05), Year=unique(labo$Year), Site=unique(labo$Site))
labo.pred.data.stream$labo2_pred = predict(labo2, type = "response", newdata=labo.pred.data.stream)
ggplot() +
  geom_point(data=labo, aes(ForestedStreams_km_2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=labo.pred.data.stream, aes(ForestedStreams_km_2km, labo2_pred, col=Year),se=TRUE) + 
  xlab("Length of Forested Streams (km)")+ylab("Total Labo Recordings")

#And urban
labo.pred.data.urb = expand.grid(FieldPct2km=mean(labo$FieldPct2km), 
                                 ForestedStreams_km_2km=mean(labo$ForestedStreams_km_2km),
                                 SuburbanPct2km=mean(labo$SuburbanPct2km),
                                 UrbanPct2km=seq(0,2.5,0.05), Year=unique(labo$Year), Site=unique(labo$Site))
labo.pred.data.urb$labo2_pred = predict(labo2, type = "response", newdata=labo.pred.data.urb)
ggplot() +
  geom_point(data=labo, aes(UrbanPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=labo.pred.data.urb, aes(UrbanPct2km, labo2_pred, col=Year),se=TRUE) + 
  xlab("Urban Landcover %")+ylab("Total Labo Recordings")

#---------------------------------------------Laci----------------------------------
#Error structure
laci.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
laci.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = laci)
AIC(laci.pois,laci.nb)
summary(laci.nb)

#Agriculture is not significant, drop it
laci1<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
lrtest(laci.nb,laci1)    #This was an okay choice
summary(laci1)

#urban is not significant, drop it
laci2<- glmer.nb(Act~ ForestLPI_2km + FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
lrtest(laci1,laci2)    #This was an okay choice
summary(laci2)

#forest LPI is not significant, drop it
laci3<- glmer.nb(Act~ FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
lrtest(laci2,laci3)    #This was an okay choice
summary(laci3)

#suburban is not significant, drop it
laci4<- glmer.nb(Act~ FieldPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
lrtest(laci3,laci4)    #This was an okay choice
summary(laci4)

#All remaining are significant
#To make sure this is robust, let's try dropping each of these 2 remaining variables
laci5A<- glmer.nb(Act~ ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
laci5B<- glmer.nb(Act~ FieldPct2km + (1 | Site) + (1 | Year), data = laci)
AIC(laci4)
AIC(laci5A)        #Suburban may or may not be significant (delta <2)
AIC(laci5B)

summary(laci4)      #WINNER WINNER
res_laci <- simulateResiduals(fittedModel = laci4, plot = F)
plot(res_laci)

#Start with Field
laci.pred.data.field = expand.grid(ForestedStreams_km_2km=mean(laci$ForestedStreams_km_2km), 
                                    FieldPct2km=seq(0,2.5,0.05), 
                                    Site=unique(laci$Site))
laci.pred.data.field$laci4_pred = predict(laci4, type = "response", newdata=laci.pred.data.field)
ggplot() +
  geom_point(data=laci, aes(FieldPct2km, Act)) +
  geom_smooth(data=laci.pred.data.field, aes(FieldPct2km, laci4_pred),se=TRUE) + 
  xlab("Field Land Cover %")+ylab("Total Laci Recordings")

#Streams
laci.pred.data.stream = expand.grid(FieldPct2km=mean(laci$FieldPct2km), 
                                    ForestedStreams_km_2km=seq(0.25,1.75,0.05), 
                                   Site=unique(laci$Site))
laci.pred.data.stream$laci4_pred = predict(laci4, type = "response", newdata=laci.pred.data.stream)
ggplot() +
  geom_point(data=laci, aes(ForestedStreams_km_2km, Act)) +
  geom_smooth(data=laci.pred.data.stream, aes(ForestedStreams_km_2km, laci4_pred),se=TRUE) + 
  xlab("Length of Forested Stream (km^2)")+ylab("Total Laci Recordings")

#-------------------------------------------------Lano--------------------------------
#Error structure
lano.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = lano)
lano.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = lano)
AIC(lano.pois,lano.nb)
summary(lano.nb)

#Forest LPI is not significant, drop it
lano1<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = lano)
lrtest(lano.nb,lano1)    #This was an okay choice
summary(lano1)

#Streams are not significant, drop it
lano2<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = lano)
lrtest(lano1,lano2)    #This was an okay choice
summary(lano2)

#suburban is not significant, drop it
lano3<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + (1 | Site) + (1 | Year), data = lano)
lrtest(lano2,lano3)    #This was an okay choice
summary(lano3)

#urban is not significant, but it makes the residuals fit better, and doesn't lower AIC
res_lano <- simulateResiduals(fittedModel = lano3, plot = F)
plot(res_lano)
summary(lano3)      #WINNER WINNER

#I chose to vary ag %. Site and Year are my random factors. Vary those too, but as factors
lano.pred.data.ag = expand.grid(FieldPct2km=mean(lano$FieldPct2km), UrbanPct2km=mean(lano$UrbanPct2km),
                                   AgriculturePct2km=seq(0,2.5,0.05), Year=unique(lano$Year), 
                                   Site=unique(lano$Site))
lano.pred.data.ag$lano3_pred = predict(lano3, type = "response", newdata=lano.pred.data.ag)
ggplot() +
  geom_point(data=lano, aes(AgriculturePct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=lano.pred.data.ag, aes(AgriculturePct2km, lano3_pred, col=Year),se=TRUE) + 
  xlab("Agricultural Land Cover %")+ylab("Total Lano Recordings")

#Now field
lano.pred.data.field = expand.grid(AgriculturePct2km=mean(lano$AgriculturePct2km), UrbanPct2km=mean(lano$UrbanPct2km),
                                   FieldPct2km=seq(0,2,0.05), Year=unique(lano$Year), 
                                Site=unique(lano$Site))
lano.pred.data.field$lano3_pred = predict(lano3, type = "response", newdata=lano.pred.data.field)
ggplot() +
  geom_point(data=lano, aes(FieldPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=lano.pred.data.field, aes(FieldPct2km, lano3_pred, col=Year),se=TRUE) + 
  xlab("Field Land Cover %")+ylab("Total Lano Recordings")

#-----------------------------------------------------Luso---------------------------------------------
#Error structure
luso.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = luso)
luso.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = luso)
AIC(luso.pois,luso.nb)
summary(luso.nb)
summary(luso.pois)    
#The negative binomial is acting weird. 
#But I'll remove urban because I don't think that will be important
#Run it again
luso.nb2<- glmer.nb(Act~ FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = luso)
luso.pois2<- glmer(Act~ FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = luso)
AIC(luso.pois2,luso.nb2)
summary(luso.nb2)
#It worked

#Agriculture has smallest effect size, drop it
luso1<- glmer.nb(Act~ FieldPct2km + ForestLPI_2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = luso)
lrtest(luso.nb2,luso1)
summary(luso1)

#Field has smallest effect size, drop it
luso2<- glmer.nb(Act~ ForestLPI_2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = luso)
lrtest(luso1,luso2)
summary(luso2)

#Suburban has smallest effect size, drop it
luso3<- glmer.nb(Act~ ForestLPI_2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = luso)
lrtest(luso2,luso3)
summary(luso3)

#Streams are not significant
luso4<- glmer.nb(Act~ ForestLPI_2km + (1 | Site) + (1 | Year), data = luso)
lrtest(luso3,luso4)
summary(luso4)

res_luso <- simulateResiduals(fittedModel = luso4, plot = F)
plot(res_luso)

#Forest LPI graph
luso.pred.data.for = expand.grid(ForestLPI_2km=seq(0,2,0.05), 
                  Year=unique(luso$Year),Site=unique(luso$Site))
luso.pred.data.for$luso4_pred = predict(luso4, type = "response", newdata=luso.pred.data.for)
ggplot() + geom_point(data=luso, aes(ForestLPI_2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=luso.pred.data.for, aes(ForestLPI_2km, luso4_pred, col=Year),se=TRUE) + 
  xlab("Forest LPI")+ylab("Total 'Luso' Recordings")

#---------------------------------------------------------Nyhu---------------
#Error structure
nyhu.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
nyhu.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = nyhu)
AIC(nyhu.pois,nyhu.nb)
summary(nyhu.nb)

#forest LPI is not significant, drop it
nyhu1<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
lrtest(nyhu.nb,nyhu1)
summary(nyhu1)

#All remaining are significant, so I'll try to remove them one by one
nyhu1a<- glmer.nb(Act~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
nyhu1b<- glmer.nb(Act~ UrbanPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
nyhu1c<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
nyhu1d<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
nyhu1e<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = nyhu)
AIC(nyhu1,nyhu1a,nyhu1b,nyhu1c,nyhu1d,nyhu1e)

#nyhu1 wins. check diagnostics
res_nyhu <- simulateResiduals(fittedModel = nyhu1, plot = F)
plot(res_nyhu)

#which is winner?
summary(nyhu1)      #Winner?


#-------------------------------------------------------------Pesu------------------------------------------------
#Error structure
pesu.nb<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = pesu)
pesu.pois<- glmer(Act~ UrbanPct2km + FieldPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), family = poisson, data = pesu)
AIC(pesu.pois,pesu.nb)
summary(pesu.nb)

#Field is not important, drop it
pesu1<- glmer.nb(Act~ UrbanPct2km + ForestLPI_2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = pesu)
lrtest(pesu.nb,pesu1)
summary(pesu1)

#Suburban is not important, drop it
pesu2<- glmer.nb(Act~ UrbanPct2km + ForestLPI_2km + AgriculturePct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = pesu)
lrtest(pesu1,pesu2)
summary(pesu2)

#Run diagnostics
res_pesu <- simulateResiduals(fittedModel = pesu2, plot = F)
plot(res_pesu)

#------------------------------WINNERS----------------------------------
#Buzz
buzz1B<- glmer.nb(Buzz~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(buzz1B)
res_buzz <- simulateResiduals(fittedModel = buzz1B, plot = F)
plot(res_buzz)
#Suburban graph
buzz.pred.data.sub = expand.grid(ForestedStreams_km_2km=mean(GBD.night.tot$ForestedStreams_km_2km),FieldPct2km=mean(GBD.night.tot$FieldPct2km),AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km),SuburbanPct2km=seq(0,2,0.05), Year=unique(GBD.night.tot$Year),Site=unique(GBD.night.tot$Site))
buzz.pred.data.sub$buzz1B_pred = predict(buzz1B, type = "response", newdata=buzz.pred.data.sub)
ggplot() +geom_point(data=GBD.night.tot, aes(SuburbanPct2km, Buzz, shape=Year, col=Year)) +
  geom_smooth(data=buzz.pred.data.sub, aes(SuburbanPct2km, buzz1B_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover")+ylab("Foraging Observations per Night")
#Forested streams
buzz.pred.data.stream = expand.grid(SuburbanPct2km=mean(GBD.night.tot$SuburbanPct2km),FieldPct2km=mean(GBD.night.tot$FieldPct2km), AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km),ForestedStreams_km_2km=seq(0.25,2,0.05), Year=unique(GBD.night.tot$Year),Site=unique(GBD.night.tot$Site))
buzz.pred.data.stream$buzz1B_pred = predict(buzz1B, type = "response", newdata=buzz.pred.data.stream)
ggplot() +
  geom_point(data=GBD.night.tot, aes(ForestedStreams_km_2km, Buzz, shape=Year, col=Year)) +geom_smooth(data=buzz.pred.data.stream, aes(ForestedStreams_km_2km, buzz1B_pred, col=Year),se=TRUE) + 
  xlab("Forested Stream Length")+ylab("Foraging Observations per Night")


#Activity
act3<- glmer.nb(Act~ FieldPct2km + AgriculturePct2km + SuburbanPct2km + (1 | Site) + (1 | Year), data = GBD.night.tot)
summary(act3)
res_act <- simulateResiduals(fittedModel = act3, plot = F)
plot(res_act)
#Agriculture
act.pred.data.ag = expand.grid(FieldPct2km=mean(GBD.night.tot$FieldPct2km), SuburbanPct2km=mean(GBD.night.tot$SuburbanPct2km), AgriculturePct2km=seq(0,2.5,0.05), Year=unique(GBD.night.tot$Year), Site=unique(GBD.night.tot$Site))
act.pred.data.ag$act3_pred = predict(act3, type = "response", newdata=act.pred.data.ag)
ggplot() + geom_point(data=GBD.night.tot, aes(AgriculturePct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=act.pred.data.ag, aes(AgriculturePct2km, act3_pred, col=Year),se=TRUE) + 
  xlab("Agricultural Land Cover")+ylab("Bat Passes per Night")
#suburbs
act.pred.data.sub = expand.grid(FieldPct2km=mean(GBD.night.tot$FieldPct2km), AgriculturePct2km=mean(GBD.night.tot$AgriculturePct2km),SuburbanPct2km=seq(0,2,0.05), Year=unique(GBD.night.tot$Year), Site=unique(GBD.night.tot$Site))
act.pred.data.sub$act3_pred = predict(act3, type = "response", newdata=act.pred.data.sub)
ggplot() + geom_point(data=GBD.night.tot, aes(SuburbanPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=act.pred.data.sub, aes(SuburbanPct2km, act3_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover")+ylab("Bat Passes per Night")


#Epfu
epfu5<- glmer.nb(Act~ SuburbanPct2km + (1 | Site) + (1 | Year), data = epfu)
summary(epfu5)
res_epfu <- simulateResiduals(fittedModel = epfu5, plot = F)
plot(res_epfu)
#Suburban
epfu.pred.data.sub = expand.grid(SuburbanPct2km=seq(0,2,0.05), Year=unique(epfu$Year),Site=unique(epfu$Site))
epfu.pred.data.sub$epfu5_pred = predict(epfu5, type = "response", newdata=epfu.pred.data.sub)
ggplot() + geom_point(data=epfu, aes(SuburbanPct2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=epfu.pred.data.sub, aes(SuburbanPct2km, epfu5_pred, col=Year),se=TRUE) + 
  xlab("Suburban Land Cover")+ylab("E. fuscus Recordings per Night")


#Labo
labo2<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = labo)
summary(labo2)
res_labo <- simulateResiduals(fittedModel = labo2, plot = F)
plot(res_labo)
#forest streams
labo.pred.data.stream = expand.grid(UrbanPct2km=mean(labo$UrbanPct2km),FieldPct2km=mean(labo$FieldPct2km),SuburbanPct2km=mean(labo$SuburbanPct2km),ForestedStreams_km_2km=seq(0.25,2,0.05), Year=unique(labo$Year), Site=unique(labo$Site))
labo.pred.data.stream$labo2_pred = predict(labo2, type = "response", newdata=labo.pred.data.stream)
ggplot() + geom_point(data=labo, aes(ForestedStreams_km_2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=labo.pred.data.stream, aes(ForestedStreams_km_2km, labo2_pred, col=Year),se=TRUE) + 
  xlab("Length of Forested Streams")+ylab("L. borealis Recordings per Night")


#Laci
laci4<- glmer.nb(Act~ FieldPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = laci)
summary(laci4)
res_laci <- simulateResiduals(fittedModel = laci4, plot = F)
plot(res_laci)


#Lano
lano3<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + (1 | Site) + (1 | Year), data = lano)
summary(lano3)
res_lano <- simulateResiduals(fittedModel = lano3, plot = F)
plot(res_lano)


#Luso
luso4<- glmer.nb(Act~ ForestLPI_2km + (1 | Site) + (1 | Year), data = luso)
summary(luso4)
res_luso <- simulateResiduals(fittedModel = luso4, plot = F)
plot(res_luso)
#Forest LPI graph
luso.pred.data.for = expand.grid(ForestLPI_2km=seq(0,2,0.05), 
                                 Year=unique(luso$Year),Site=unique(luso$Site))
luso.pred.data.for$luso4_pred = predict(luso4, type = "response", newdata=luso.pred.data.for)
ggplot() + geom_point(data=luso, aes(ForestLPI_2km, Act, shape=Year, col=Year)) +
  geom_smooth(data=luso.pred.data.for, aes(ForestLPI_2km, luso4_pred, col=Year),se=TRUE) + 
  xlab("Forest LPI")+ylab("Total 'Luso' Recordings")


#Nyhu
nyhu1<- glmer.nb(Act~ UrbanPct2km + FieldPct2km + AgriculturePct2km + SuburbanPct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = nyhu)
summary(nyhu1)
res_nyhu <- simulateResiduals(fittedModel = nyhu1, plot = F)
plot(res_nyhu)


#Pesu
pesu2<- glmer.nb(Act~ UrbanPct2km + ForestLPI_2km + AgriculturePct2km + ForestedStreams_km_2km + (1 | Site) + (1 | Year), data = pesu)
summary(pesu2)
res_pesu <- simulateResiduals(fittedModel = pesu2, plot = F)
plot(res_pesu)


#Make a table of all the results so I can see all of the parameter estimates together
bat.mods=model.sel(buzz1B,act3,epfu5,labo2,laci4,lano4,luso4,nyhu1,pesu2)
