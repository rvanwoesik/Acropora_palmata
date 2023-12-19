#Environmental predictors for the restoration of a critically 
  #endangered coral, Acropora palmata, along the Florida reef 
  #tract
#Authors: Raymond B. Banister, T. Shay Viehman, 
         #Stephanie Schopmeyer, Robert van Woesik*
#Script last edited: 12/12/2023

#------------------------------------------------------------

#Clear Environment
rm(list=ls())

# install.packages('maptools') #install necessary packages - only need if first 
#time using packages
library(maptools) #load the library
library(sp)
library(raster)
library(dismo)
library(corrplot)
library(gam)
library(randomForest)

main_directory=setwd("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R")
wcdir<-getwd() #save working directory for later

data <- read.csv("1_Acropora_palmata_pres_abs.csv") #read in presence points

#Compare to after forloop below. 
table(data$Acropora_palmata_Presence)
#    0     1 
#19367  6130 

#If presence at a site, ignore the remaining 0's (absences) afterwards
ID=unique(data$Site_ID)

for(i in ID){
  x<- subset(data, Site_ID==i)
  if((sum(x$Acropora_palmata_Presence)>=1)==TRUE){
    data$Acropora_palmata_Presence[which(data$Site_ID==i)]<-1
  }
}

#Compare to before forloop above 
table(data$Acropora_palmata_Presence)
#0     1 
#18227  7270 


#Remove
data <- data[!(data$Site_ID %in% c("3313", "3311", "3278", "3276")), ]

#SUBSET
presence <- data[ -c(1,4:5,7:8) ] #subset based on what variables you want
names(presence)[names(presence)=='Acropora_palmata_Presence']<-'Presence'
Longitude=presence$Longitude_Degrees
Latitude=presence$Latitude_Degrees
latlonUnique=unique(presence[ , c("Longitude_Degrees","Latitude_Degrees")])
#write.csv(latlonUnique, file=file.path(main_directory,'UniqueSiteLocations_Banister.csv'), row.names=TRUE) 

View(presence) #If you want to take a look at the dataset

GENUS<-'A.pal'

#Quick look at spatial orientation
presPts <- cbind(presence$Longitude_Degrees, presence$Latitude_Degrees)
bbox(presPts)
plot(presPts)
data(wrld_simpl)
plot(wrld_simpl, xlim=c(-83.5,-80.5), ylim=c(24,27), axes=TRUE, col='light yellow')
box()
points(latlonUnique, col='blue')

#set extent
ext<-extent(-84,-79,24,31)

#reset wd
setwd("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R")

#IN USE:
Chl.a_Mean<-raster("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R/NOAA 1km/NOAA_1km_Chla_Data/daily/meanChla_4km_1997_2021_all.tiff")
PAR_Min<-raster("D:/Lab Data/Florida/PAR/2005_2015/Annual_Averages/4km/MODIS_Aqua/AnnualAveragePARMin2005_2015_4km.tif")
PAR_Max<-raster("D:/Lab Data/Florida/PAR/2005_2015/Annual_Averages/4km/MODIS_Aqua/AnnualAveragePARMax2005_2015_4km.tif")
SST_Min=raster("D:/Lab Data/NOAA (Michael Coyne)/Florida/MUR SST L4 Global 1km daily (2002 – present)/Min SST/minSST_1km_2002_2021_all.tiff")
SST_Max=raster("D:/Lab Data/NOAA (Michael Coyne)/Florida/MUR SST L4 Global 1km daily (2002 – present)/Max SST/maxSST_1km_2002_2021_all.tiff")
Fetch<-raster("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/Wave Exposure/Figures/Average_Wave_Energy_1km_1987_2015.tif") #course bathymetry
TNB_Mean=raster("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/SERC Data/Mean/TNB_Mean.tif")
TPB_Mean=raster("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/SERC Data/Mean/TPB_Mean.tif")
SAL_Min=raster("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/SERC Data/MaxMin/IDW_MinSAL.tif")
SAL_Max=raster("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/SERC Data/MaxMin/IDW_MaxSAL.tif")

#Crop all rasters to norther extent of SERC data
#This is all within the FKNMS and Acropora critical habitat. 
ext1=extent(-83.08572, -80.09697, 24.37173, 25.59242)
Chl.a_Mean <- crop(x = Chl.a_Mean, y = ext1)
SST_Min <- crop(x = SST_Min, y = ext1)
SST_Max <- crop(x = SST_Max, y = ext1)
Fetch <- crop(x = Fetch, y = ext1)
TNB_Mean <- crop(x = TNB_Mean, y = ext1)
TPB_Mean <- crop(x = TPB_Mean, y = ext1)
SAL_Min <- crop(x = SAL_Min, y = ext1)
SAL_Max <- crop(x = SAL_Max, y = ext1)
PAR_Min <- crop(x = PAR_Min, y = ext1)
PAR_Max <- crop(x = PAR_Max, y = ext1)
###~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~###
###~~~~~~~~~~~~~~~~~~~~###

Chl.a_Mean #-83.08334, -80.08334, 24.37499, 25.58333
SST_Min #-83.08231, -80.09222, 24.36771, 25.58784
SST_Max #-83.08231, -80.09222, 24.36771, 25.58784
PAR_Min #-83.08334, -80.08334, 24.37499, 25.58332
PAR_Max #-83.08334, -80.08334, 24.37499, 25.58332
Fetch #-83.06251, -80.09807, 24.37267, 25.59438
TNB_Mean #-83.08593, -80.09754, 24.37637, 25.59484
TPB_Mean #-83.08593, -80.09754, 24.37637, 25.59484
SAL_Min #-83.08572, -80.09697, 24.37173, 25.59242
SAL_Max #-83.08572, -80.09697, 24.37173, 25.59242



#whatever has the mass extent, resample from that. 
#TEMPLATE:     #newraster <- resample(rasterToEdit,template_raster,method='bilinear')

#IN USE:
Chl.a_Mean <- resample(Chl.a_Mean,SST_Min,method='bilinear')
SST_Min <- resample(SST_Min,SST_Min,method='bilinear')
SST_Max <- resample(SST_Max,SST_Min,method='bilinear')
PAR_Min <- resample(PAR_Min,SST_Min,method='bilinear')
PAR_Max <- resample(PAR_Max,SST_Min,method='bilinear')
Fetch <- resample(Fetch,SST_Min,method='bilinear')
TNB_Mean <- resample(TNB_Mean,SST_Min,method='bilinear')
TPB_Mean <- resample(TPB_Mean,SST_Min,method='bilinear')
SAL_Min <- resample(SAL_Min,SST_Min,method='bilinear')
SAL_Max <- resample(SAL_Max,SST_Min,method='bilinear')

#RASTER STACK
stack<-stack(Chl.a_Mean,SST_Min,SST_Max,PAR_Min,PAR_Max, #Ranges,Chl.a_Min,TNB_Min,TPB_Min removed
             Fetch,TNB_Mean,TPB_Mean,SAL_Min,SAL_Max) #Max WE, NO3,NO2,NH4, removed and Total nitrate surface added (TNS) (May 2021)
stack
names(stack)<-c("Chl.a_Mean","SST_Min","SST_Max","PAR_Min","PAR_Max", #Ranges,Chl.a_Min,TNB_Min,TPB_Min removed
                "Fetch","TNB_Mean","TPB_Mean","SAL_Min","SAL_Max")
stack


#CROP RASTERS WITH A.palmata CRITICAL HABITAT
library(rgdal)
library(raster)
setwd("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R/Critical Habitat")
critical<-(readOGR(".","Acropora_Critical_Habitat"))
critical
critical <- spTransform(critical, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
extentmask<-mask(stack, critical)


#MAKE A MAP (Not for official figures)
longlat<- '+proj=longlat +ellps=WGS84 +no_defs'
eqc<- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
utm <- "+proj=utm +zone=17 +units=km +datum=WGS84"
library(rlang)
library(rworldmap)
library(maptools)
library(raster)
library(rworldxtra)
wholeworld<-getMap(resolution="high")
FLOutplantMap<-crop(wholeworld, c(-83.25548, -79.8523,24.23897, 27.4085)) #ignore warning #message that they have different projections
FLOutplantMap<-spTransform(FLOutplantMap, longlat)
plot(FLOutplantMap)

#Color packages
library(viridis)
library(scales)
library(ggplot2)
library(RColorBrewer)

####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####
#Practice code for figure 1 --> Figure of all sites along 
  #the Florida reef tract: presence black, absence pink

library(RColorBrewer)
col_critical <- brewer.pal(9, "BuGn")
col_critical
col_critical <- colorRampPalette("#C6DBEF")

library(dplyr)
pres_POINTS = filter(presence, Presence==1)
pres_POINTS = cbind(pres_POINTS$Longitude_Degrees,pres_POINTS$Latitude_Degrees)
abs_POINTS = filter(presence, Presence==0)
abs_POINTS = cbind(abs_POINTS$Longitude_Degrees, abs_POINTS$Latitude_Degrees)

#Higher Resolution Map
#install.packages(c("maps", "mapdata", "ncdf4", "raster", "rgdal", "RColorBrewer", "sp"))
# To load and process GIS data
require(sp)
require(rgdal)
require(raster)
require(ncdf4)
#To make nicer looking maps
require(maps) 
require(mapdata)
require(RColorBrewer)
par(mfrow=c(1,1))
plot(FLOutplantMap, add=FALSE, border="light blue",col="light blue", xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26),bg="light blue")
map("worldHires", col="darkseagreen3", border="grey50",ylim=c(24.23897, 27.4085),
    xlim=c(-83.25548, -79.8523), 
    fill=TRUE, add=TRUE,plot=TRUE)
points(abs_POINTS, col="#FFCCCC", cex=0.5, pch=20) 
points(pres_POINTS, col='black', cex=1, pch=20)
maps::map.scale(x=-80.6, y=24.19, ratio=FALSE, relwidth = 0.15)
box(which = "plot", lty = "solid",bgc="light blue")
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25,26), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.05, c("83W","81W"))
text(-83.19, c(24.5,25.5), c("24.5N", "25.5N"))
text(-80.77, 25.6, "Florida",font=2)
text(-82.8, 26.1, "Gulf of Mexico", font=2)


####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

#Create color ramps for rasters
cividis<-cividis(200)
pal <- colorRampPalette(c("blue","yellow","red"))
Temp<-pal(200)
colChla <- brewer.pal(7, "YlGn")
colChla
pal2 <- colorRampPalette(colChla)
Chla_pal<-pal2(200)
plasma<-plasma(200)
pal3 <- colorRampPalette(c("lightblue", "blue", "darkblue"))
TNB_pal<-pal3(200)
colTPB <- brewer.pal(7, "Greens")
colTPB
pal4 <- colorRampPalette(colTPB)
TPB_pal<-pal4(200)
library(RColorBrewer)
colSAL <- brewer.pal(7, "OrRd")
colSAL
pal5 <- colorRampPalette(colSAL)
SAL_pal<-pal5(200)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###### SUPPLEMENTARY ######
#Figure A
######
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
#plot(extentmask$TNB_Mean, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=TNB_pal,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
SERC_Points = read.csv("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/CSS/GIS Water Quality/wq_Keys.csv")
SERC_Points = cbind(SERC_Points$LONDEC,SERC_Points$LATDEC)
SERC_Points <- na.omit(SERC_Points)
SERC_Points<-SpatialPoints(SERC_Points)  # run this if you have points outside of raster data
proj4string(SERC_Points) <- CRS("+proj=longlat +ellps=WGS84 +no_defs")  #setting the coordinate reference system is important to run further tests
SERC_Points <- as.data.frame(SERC_Points)
colnames(SERC_Points)<- c("Longitude","Latitude")
points(SERC_Points, col='red', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Sample Sites"),col=c("Red"), pch=c(16), cex=1.3, bg='white',border = "black")
###########

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Figure B
######
#Figure B(a) - TNB Mean
#plot(extentmask$TNB_Range, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE, col=TNB_pal)
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$TNB_Mean, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=TNB_pal,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.41, labels = expression(paste(ppm)), pos = 3, cex = 0.8)

#Figure B(b) - TPB Mean
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$TPB_Mean, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=TPB_pal,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.41, labels = expression(paste(ppm)), pos = 3, cex = 0.8)

#Figure B(c) - Salinity min
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$SAL_Min, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=SAL_pal,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.41, labels = expression(paste(ppt)), pos = 3, cex = 0.8)

#Figure B(d) - Salinity max
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$SAL_Max, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=SAL_pal,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.41, labels = expression(paste(ppt)), pos = 3, cex = 0.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Figure C
##########
#FIGURE C(a)- CHL-A MEAN
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$Chl.a_Mean, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=Chla_pal,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.42, labels = expression("mg m"^-3), pos = 3, cex = 0.8)

#FIGURE C(b)- Fetch
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$Fetch, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=cividis,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.42, labels = expression("kJ m"^-2), pos = 3, cex = 0.8)

#FIGURE C(c)- Min SST
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$SST_Min, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=Temp,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.42, labels = expression(paste("", degree*C)), pos = 3, cex = 0.8)

#FIGURE C(d)- Max SST
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$SST_Max, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=Temp,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.42, labels = expression(paste("", degree*C)), pos = 3, cex = 0.8)


#FIGURE C(e)- PAR min
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$PAR_Min, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=plasma,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.41, labels = expression(paste(~mol~m^-2~s^-1)), pos = 3, cex = 0.8)


#FIGURE C(f)- PAR max
par(mar = c(2, 2, 2, 0.5))  # Adjust the margin values as needed
plot(FLOutplantMap, add=FALSE, border='dark grey',col="grey",xlim=c(-83.25548,-79.7523),ylim=c(24.23897,25.9))
plot(extentmask$PAR_Max, xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE,col=plasma,add=TRUE)
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.24, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.80, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.41, labels = expression(paste(~mol~m^-2~s^-1)), pos = 3, cex = 0.8)
###############
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Change ALNUS extent - repeat - double-check
presPts<-SpatialPoints(presPts)  # run this if you have points outside of raster data
presPts<-crop(presPts,Chl.a_Mean)
bbox(presPts)
proj4string(presPts) <- CRS("+proj=longlat +ellps=WGS84 +no_defs")  #setting the coordinate reference system is important to run further tests
presPts <- as.data.frame(presPts)
colnames(presPts)<- c("Longitude","Latitude")
plot(presPts, cex = 0.3, col = "red", pch = 16)#This shows all of the points. 
table(data$Acropora_palmata_Presence)



##########################################
#Extract the environmental conditions where genus was present
coordinates<-cbind(presence$Longitude_Degrees, presence$Latitude_Degrees)
vals_Chla_Mean<-raster::extract(Chl.a_Mean, coordinates)
vals_SST_Min<-raster::extract(SST_Min, coordinates)
vals_SST_Max<-raster::extract(SST_Max, coordinates)
vals_PAR_Min<-raster::extract(PAR_Min, coordinates)
vals_PAR_Max<-raster::extract(PAR_Max, coordinates)
vals_Fetch<-raster::extract(Fetch, coordinates)
vals_TNB_Mean<-raster::extract(TNB_Mean, coordinates)
vals_TPB_Mean<-raster::extract(TPB_Mean, coordinates)
vals_SAL_Min<-raster::extract(SAL_Min, coordinates)
vals_SAL_Max<-raster::extract(SAL_Max, coordinates)


#CREATING DATAFRAME WITH EXTRACTED VALUES @ PRESENCE/ABSENCE POINTS

#IN USE:
sdmdata<-data.frame("Latitude_Degrees"=presence$Latitude_Degrees)
sdmdata$Longitude_Degrees<-presence$Longitude_Degrees
sdmdata$Presence<-presence$Presence
sdmdata$Chl.a_Mean<-vals_Chla_Mean
sdmdata$SST_Min<-vals_SST_Min
sdmdata$SST_Max<-vals_SST_Max
sdmdata$PAR_Min<-vals_PAR_Min
sdmdata$PAR_Max<-vals_PAR_Max
sdmdata$Fetch<-vals_Fetch
sdmdata$TNB_Mean<-vals_TNB_Mean
sdmdata$TPB_Mean<-vals_TPB_Mean
sdmdata$SAL_Min<-vals_SAL_Min
sdmdata$SAL_Max<-vals_SAL_Max

#####################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#####################################

#CHECKING CORRELATIONS
library(corrplot)
correlations<- cor(as.matrix(sdmdata[c("Chl.a_Mean","SST_Min","SST_Max","PAR_Min","PAR_Max", #Ranges,Chl.a_Min,TNB_Min,TPB_Min removed
                                       "Fetch","TNB_Mean","TPB_Mean","SAL_Min","SAL_Max")]),use="pairwise.complete.obs")#Pairwise.complete.obs removes NAs
#png(file=file.path(output_directory,'corrplot.png'), res=300,width=3000,height=3000)
corrplot(correlations)
corrplot.mixed(correlations, lower.col = "black", number.cex = .7)
corrplot(correlations,method='number',number.cex = .7)
#When highly correlated, pick one to remove from the models. 


#K-FOLD CROSS VALIDATION - evaluating models that have limited sample
print('k-folding')
library(dismo)
group_pres <- kfold(sdmdata[sdmdata$Presence==1,], 5) 
pres_train <- sdmdata[sdmdata$Presence==1,][group_pres != 1, ] 
pres_test <- sdmdata[sdmdata$Presence==1,][group_pres == 1, ] 
group_abs <- kfold(sdmdata[sdmdata$Presence==0,], 5) 
backg_train <- sdmdata[sdmdata$Presence==0,][group_abs != 1, ]
backg_test <- sdmdata[sdmdata$Presence==0,][group_abs == 1, ]
train <- rbind(pres_train, backg_train)
test <- rbind(pres_test, backg_test)



#CSV files written for all tested variables:
#Template:
#write.csv(sdmdata,"C:/WorkingDirectory/sdmdata.csv", row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~BRT~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Booster Regression Tree
#install.packages("gbm")
library(dismo)
library(tidyverse)
setwd("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R/BRT/September2023")
#BRT=read.csv("BRT_All_Vars_Final.csv")
BRT=sdmdata
table(BRT$Presence)
#0     1 
#18227  7270 

brt1 <- gbm.step(data=BRT, gbm.x = 4:13, gbm.y = 3,family="bernoulli", tree.complexity=5, learning.rate=0.005, bag.fraction=0.5)
#Performing cross-validation optimization of a boosted regression tree model 
#for Presence and using a family of bernoulli 
#Using 25497 observations and 7 predictors 


#Description for assessing the model performance scores for "gbm.step" - 
#Model performance scores. Training data Area Under Curve (AUC), Cross-Validated (CV)
#AUC (higher is better for both), CV AUC standard error, and training data AUC minus CV AUC
#(overfitting (O); lower is better for both). For the original full dataset, mature females, juveniles, and
#the averages of each. AUC values over 0.8 are very good, and over 0.9 excellent
#The above model "brt1" has great scores:
#Training AUC = 0.972 (Higher is better)
#CV AUC = 0.971 (Higher is better)
#CV AUC se = 0.001 (Lower is better)
#T AUC - CV AUC = 0.001 (Lower is better)
#Source: Dedman et al. 2017 --> Advanced Spatial Modeling to Inform Management of Data-Poor Juvenile and Adult Female Rays

length(brt1$fitted) #25436
names(brt1)
dev.off()
summary(brt1)
#                 var   rel.inf
# Chl.a_Mean Chl.a_Mean 33.695539
# Fetch           Fetch 10.816537
# TPB_Mean     TPB_Mean 10.106063
# SST_Min       SST_Min  9.907761
# PAR_Max       PAR_Max  8.211242
# TNB_Mean     TNB_Mean  6.901076
# SAL_Max       SAL_Max  6.880109
# SST_Max       SST_Max  6.464164
# SAL_Min       SAL_Min  5.016171
# PAR_Min       PAR_Min  2.001338


#WHAT VARIABLE WOULD BE BEST TO DROP?
#Run the code below to simplify the model by dropping predictor variables
# brt1simp <- gbm.simplify(brt1, n.drops = 3) #This identifies the 3 variables best to drop.


#PARTIAL DEPENDENCY PLOT
#plotting the functions and fitted values from the model
#plotting all of the variables on a sheet
par(mfrow=c(1,1)) #x.label = c("Chlorophyll-a Range", "SST Range", "Total Phosphorus Range", "Total Nitrogen Range", "Fetch", "Salinity Range", "Light Intensity Range")
library(dismo)
dev.off()
gbm.plot(brt1, lwd=2, n.plots=10, write.title = FALSE, plot.layout = c(2,5),y.label = "Fitted Function", show.contrib = TRUE, cex.axis = 1.2,cex.lab=1.5) #smooth = TRUE, will add a smoothed red line for the fitted functions. 

#If you can't fix the text along the axis, use whiteout:
#install.packages('spatstat')
#library(spatstat)
#par(xpd=NA) 
#whiteout<-clickpoly(add=TRUE,np=1,col='white') #draw a white polygon around 'Time'
#plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
#title(xlab='Insert text',cex.lab=1.5) #label the x-axis
 

#plotting the fitted values in relation to the predictors used in the model. 
gbm.plot.fits(brt1)


#Interrogate and plot the interactions
#The first 2 components summarize the results, first as 
#a ranked list of the 5 most important pairwise interactions, 
#and the second tabulating all pairwise interactions.

find.int <- gbm.interactions(brt1)
find.int$interactions
#             Chl.a_Mean SST_Min SST_Max PAR_Min PAR_Max Fetch TNB_Mean TPB_Mean SAL_Min SAL_Max
# Chl.a_Mean          0    10.1    7.79   15.62    0.61  1.14     8.94    11.99   18.61   18.41
# SST_Min             0     0.0   26.04    3.88    4.84  4.42     2.49    12.66   10.55    2.94
# SST_Max             0     0.0    0.00    1.76    1.87  9.32    26.70    14.30    4.29    4.79
# PAR_Min             0     0.0    0.00    0.00   12.77 28.75     4.06    26.16    4.04    8.53
# PAR_Max             0     0.0    0.00    0.00    0.00  8.75    37.16    17.35    1.29    1.90
# Fetch               0     0.0    0.00    0.00    0.00  0.00    41.96     0.91    3.06    9.16
# TNB_Mean            0     0.0    0.00    0.00    0.00  0.00     0.00    14.06   17.29   21.07
# TPB_Mean            0     0.0    0.00    0.00    0.00  0.00     0.00     0.00   13.31    6.51
# SAL_Min             0     0.0    0.00    0.00    0.00  0.00     0.00     0.00    0.00    4.16
# SAL_Max             0     0.0    0.00    0.00    0.00  0.00     0.00     0.00    0.00    0.00


find.int$rank.list
# var1.index var1.names var2.index var2.names int.size
# 1          7   TNB_Mean          6      Fetch    41.96
# 2          7   TNB_Mean          5    PAR_Max    37.16
# 3          6      Fetch          4    PAR_Min    28.75
# 4          7   TNB_Mean          3    SST_Max    26.70
# 5          8   TPB_Mean          4    PAR_Min    26.16



##############

#The command below gives a 3D look at interactions (two lines of code below are just to check things out)
par(mfrow=c(1,1)) 
gbm.perspec(brt1, 7, 1, y.range=c(24,36), z.range=c(0,1))
gbm.perspec(brt1, 9, 1, y.range=c(0,0.02), z.range=c(0,1))

#See plot pairwise interaction plot. 
plot(extentmask)

#Creating prediction
library(raster)
p=predict(extentmask, brt1, n.trees=brt1$gbm.call$best.trees, type="response")
p=mask(p,raster(extentmask,1))
plot(p,main='A. palmata - BRT No Threshold', xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE)
plot(FLOutplantMap, add=TRUE, border='dark grey',col="tan")


brt1e=evaluate(p=test[test$Presence==1,], a=test[test$Presence==0,], model=brt1)
brt1e
# class          : ModelEvaluation 
# n presences    : 1443 
# n absences     : 3644 
# AUC            : 0.973018 
# cor            : 0.465302 
# max TPR+TNR at : -1.215347


#Evaluate the model
plot(brt1e,  'ROC') #AUC = 0.973

pg <- predict(extentmask, brt1, ext=ext)
pg
summary(brt1)

#                 var   rel.inf
# Chl.a_Mean Chl.a_Mean 33.695539
# Fetch           Fetch 10.816537
# TPB_Mean     TPB_Mean 10.106063
# SST_Min       SST_Min  9.907761
# PAR_Max       PAR_Max  8.211242
# TNB_Mean     TNB_Mean  6.901076
# SAL_Max       SAL_Max  6.880109
# SST_Max       SST_Max  6.464164
# SAL_Min       SAL_Min  5.016171
# PAR_Min       PAR_Min  2.001338



par(mfrow=c(1,1))

#Establish threshold
tr <- threshold(brt1e, 'spec_sens')
#tr1 = threshold(brt1e, 'prevalence')
#tr2 = threshold(brt1e, 'kappa')
#tr3 = threshold(brt1e, 'no_omission')
#tr4 = threshold(brt1e, 'equal_sens_spec')


#SELECTING COLORS FOR FURTHER USE
library(RColorBrewer)
cols <- brewer.pal(9, "BuGn")
cols
breaks <- seq(0, 1, by = 0.01)
cols <- colorRampPalette(c("#C6DBEF", "#238B45"))(length(breaks) - 1)

cols2 <- brewer.pal(9, "Set1")
cols2
breaks2 <- seq(0, 1, by = 0.01)
cols2 <- colorRampPalette(c("#C6DBEF", "#E41A1C"))(length(breaks2) - 1)

cols3 <- brewer.pal(9, "Set1")
cols3
breaks3 <- seq(0, 1, by = 0.01)
cols3 <- colorRampPalette(c("#C6DBEF", "#FFFF33"))(length(breaks3) - 1)

cols4 <- brewer.pal(8, "Accent")
cols4
breaks4 <- seq(0, 1, by = 0.01)
cols4 <- colorRampPalette(c("#C6DBEF", "#F0027F"))(length(breaks4) - 1)

cols5 <- brewer.pal(12, "Paired")
cols5
breaks5 <- seq(0, 1, by = 0.01)
cols5 <- colorRampPalette(c("#C6DBEF", "#6A3D9A"))(length(breaks5) - 1)

cols6 <- brewer.pal(12, "Paired")
cols6
breaks6 <- seq(0, 1, by = 0.01)
cols6 <- colorRampPalette(c("#C6DBEF", "#B15928"))(length(breaks6) - 1)

cols8 <- brewer.pal(9, "Set1")
cols8
breaks8 <- seq(0, 1, by = 0.01)
cols8 <- colorRampPalette(c("#C6DBEF00", "#FFFF33"))(length(breaks6) - 1)


#TRANSPARENT COLORS - for kmz files (SUPPLEMENTARY)
library(RColorBrewer)
# Define the color with transparency
transparent_color <- rgb(198, 219, 239, alpha = 0, maxColorValue = 255)
# Choose a color palette from ColorBrewer
cols10 <- brewer.pal(9, "BuGn")
# Replace the first color in the palette with the transparent color
cols10[1] <- transparent_color
# Display the modified color palette
cols10

# Define the color with transparency
transparent_color <- rgb(198, 219, 239, alpha = 0, maxColorValue = 255)
# Define the replacement color as #F0027F
replacement_color <- "#F0027F"
# Create a custom color palette
cols11 <- c(transparent_color, replacement_color)
# Display the modified color palette
cols11

# Define the color with transparency
transparent_color <- rgb(198, 219, 239, alpha = 0, maxColorValue = 255)
# Define the replacement color as a bright yellow
replacement_color <- "#FFFF33"
# Create a custom color palette
cols12 <- c(transparent_color, replacement_color)
# Display the modified color palette
cols12

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#CREATING HARDBOTTOM MAP
#Coral_and_Hard_Bottom_Habitats_in_Florida.shp
library(sf)
library(sp)
library(rgdal)
setwd("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R/Critical Habitat")
critical<-(readOGR(".","Acropora_Critical_Habitat"))
critical <- spTransform(critical, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
setwd("D:/Lab Data/Florida/Coral_and_Hard_Bottom_Habitats_in_Florida")
hbshape=readOGR(".","Coral_and_Hard_Bottom_Habitats_in_Florida")
hbshape <- spTransform(hbshape, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
#plot(hbshape)
hbshape

################CORAL REEF#######################
cr_hbshape=subset(hbshape, DESCRIPT == "Coral Reef")
#plot(cr_hbshape)
library(terra)
cr_shape<-crop(cr_hbshape, critical,mask=TRUE)
#plot(cr_shape)
# crop raster using the vector extent
cr_shape_crop <- mask(pg > tr,cr_shape)
#plot(cr_shape_crop, main = "Cropped Coral Reef")

################HARDBOTTOM#######################
hb_hbshape=subset(hbshape, DESCRIPT == "Hardbottom")
#plot(hb_hbshape)
library(terra)
hb_shape<-crop(hb_hbshape, critical,mask=TRUE)
#plot(cr_shape)
# crop raster using the vector extent
hb_shape_crop <- mask(pg > tr,hb_shape)
#plot(cr_shape_crop, main = "Cropped Hardbottom)

################HARDBOTTOM AND SEAGRASS#######################
hbs_hbshape=subset(hbshape, DESCRIPT == "Hardbottom with Seagrass")
#plot(hbs_hbshape)
library(terra)
hbs_shape<-crop(hbs_hbshape, critical,mask=TRUE)
plot(hbs_shape)
# crop raster using the vector extent
hbs_shape_crop <- mask(pg > tr,hbs_shape)
#plot(hbs_shape_crop, main = "Cropped Hardbottom and Seagrass")

################PROBABLE HARDBOTTOM#######################
ph_hbshape=subset(hbshape, DESCRIPT == "Probable Hardbottom")
#plot(ph_hbshape)
library(terra)
ph_shape<-crop(ph_hbshape, critical,mask=TRUE)
#plot(cr_shape)
# crop raster using the vector extent
ph_shape_crop <- mask(pg > tr,ph_shape)

################CORAL REEF & HARDBOTTOM#######################
crhb_hbshape=subset(hbshape, DESCRIPT == "Coral Reef" | DESCRIPT == "Hardbottom")
unique(crhb_hbshape$DESCRIPT)
#plot(crhb_hbshape)
library(terra)
crhb_shape<-crop(crhb_hbshape, critical,mask=TRUE)
#plot(crhb_shape)
# crop raster using the vector extent
crhb_shape_crop <- mask(pg > tr,crhb_shape)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#############
######prep Florida shapefile
Florida <- sf::read_sf("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Shapefile/Florida_Shapefile/1_Florida_Official.shp")
target_crs <- st_crs("+proj=longlat +ellps=WGS84 +no_defs")
Florida <- st_transform(Florida, target_crs)
############
#############

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

###  FIGURE 1
par(mfrow=c(1,1),mar=c(1,1,1,1))
image(abs_POINTS,xlim=c(-83.25548,-79.8523),ylim=c(24.03897,26), axes=FALSE, col = "#FFCCCC") #main= expression('BRT - Habitat Suitability for '*italic(A.~palmata)) #, useRaster=TRUE, interpolate=TRUE) #, interpolate=TRUE) #-83.25548, -79.8523,24.23897, 27.4085
plot(Florida["Shape_Leng"],xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26),border="NA",bg="white",col="navajowhite3", main="",add=TRUE)
points(abs_POINTS, col="#FFCCCC", cex=0.5, pch=20) 
points(pres_POINTS, col='black', cex=1, pch=20)
box(which = "plot", lty = "solid")
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.57, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.25, ratio=FALSE, cex=0.9,relwidth = 0.10)

axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.12, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.75, "Florida",font=2,cex=1.0)
text(-82.7, 25.15, "Gulf of Mexico", cex=0.8)

text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.10, 24.95, "Upper Florida Keys", cex=0.8)
text(-80.55, 25.6, "Biscayne Bay", cex=0.8)
text(-80.55, 25.89, "Broward-Miami", cex=0.8)

library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presence","Absence"),col=c("black","#FFCCCC"), pch=c(16,16), cex=1.3, bg='white',border = "black")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Figure 2

#PARTIAL DEPENDENCY PLOT
#plotting the functions and fitted values from the model
#plotting all of the variables on a sheet
par(mfrow=c(1,1)) #x.label = c("Chlorophyll-a Range", "SST Range", "Total Phosphorus Range", "Total Nitrogen Range", "Fetch", "Salinity Range", "Light Intensity Range")
library(dismo)
dev.off()
gbm.plot(brt1, lwd=2, n.plots=10, write.title = FALSE, plot.layout = c(2,5),y.label = "Fitted Function", show.contrib = TRUE, cex.axis = 1.2,cex.lab=1.5) #smooth = TRUE, will add a smoothed red line for the fitted functions. 

#If you can't fix the text along the axis, use whiteout:
#install.packages('spatstat')
#library(spatstat)
#par(xpd=NA) 
#whiteout<-clickpoly(add=TRUE,np=1,col='white') #draw a white polygon around 'Time'
#plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
#title(xlab='Insert text',cex.lab=1.5) #label the x-axis

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Figure 3
par(mar = c(2, 2, 2, 1))  # Adjust the margin values as needed
image(abs_POINTS,xlim=c(-83.25548,-79.6523),ylim=c(24.03897,26), axes=FALSE, col = "#FFCCCC") #main= expression('BRT - Habitat Suitability for '*italic(A.~palmata)) #, useRaster=TRUE, interpolate=TRUE) #, interpolate=TRUE) #-83.25548, -79.8523,24.23897, 27.4085
plot(extentmask$Chl.a_Mean,axes=FALSE,col=Chla_pal,add=TRUE)
plot(Florida["Shape_Leng"], add=TRUE, border='NA',col="navajowhite3",main="")
box(which = "plot", lty = "solid")
arrows(-82.7, 25.2, -82.7, 25.5)
text(-82.7, 25.6, "N",cex=0.9)
maps::map.scale(x=-80.70, y=24.3, ratio=FALSE, cex=0.9,relwidth = 0.10)

axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.11, c("83W","81W"),cex=0.9)
text(-83.12, c(24.5,25.5), c("24.5N", "25.5N"),cex=.9)
text(-80.77, 25.75, "Florida",font=2,cex=1.0)
text(-82.7, 25.1, "Gulf of Mexico", cex=0.8)

points(pres_POINTS, col='black', cex=1, pch=20)

library(rgdal)
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)

text(-82.85, 24.26, "Dry Tortugas", cex=0.8)
text(-82.25, 24.34, "Marquesas", cex=0.8)
text(-81.38, 24.39, "Lower Florida Keys", cex=0.8)
text(-80.58, 24.59, "Middle Florida Keys", cex=0.8)
text(-80.21, 24.85, "Upper Florida Keys", cex=0.8)
text(-80.62, 25.6, "Biscayne Bay", cex=0.8)
text(-80.57, 25.95, "Broward-Miami", cex=0.8)
plot(regionslonglat, add=TRUE)
legend("topleft", legend=c("Presences"),col=c("black"), pch=c(16), cex=1.3, bg='white',border = "black")
text(x = -79.84, y = 24.42, labels = expression("mg m"^-3), pos = 3, cex = 0.8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# FIGURE 4
#HABITAT SUITABILITY MAP WITH THRESHOLD

par(mfrow=c(1,1),mar=c(1,1,1,1))
image(pg > tr,xlim=c(-83.25548,-79.8523),ylim=c(24.23897,26), axes=FALSE, col = cols) #main= expression('BRT - Habitat Suitability for '*italic(A.~palmata)) #, useRaster=TRUE, interpolate=TRUE) #, interpolate=TRUE) #-83.25548, -79.8523,24.23897, 27.4085
plot(Florida["Shape_Leng"],border="NA",bg="white",col="navajowhite3",add=TRUE)
arrows(-82.7, 25.15, -82.7, 25.45)
text(-82.7, 25.5, "N")
maps::map.scale(x=-80.70, y=24.4, ratio=FALSE, relwidth = 0.10)

box(which = "plot", lty = "solid")
axis(1, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(2, at=c(24.5,25.5,25,26), NA, cex.axis=.7, font=1, tck=.02)
axis(3, at=seq(-84,-79,1), NA, cex.axis=.7, font=1, tck=.02)
axis(4, at=c(24.5,25.5, 25,26), NA, cex.axis=.7, font=1, tck=.02)
text(c(-83, -81), 24.30, c("83W","81W"))
text(-83.11, c(24.5,25.5), c("24.5N", "25.5N"))
text(-80.77, 25.77, "Florida",font=2,cex=1.4)
text(-82.7, 25.1, "Gulf of Mexico", font=2)

library(rgdal)
#read in subregions shapefile and unified benthic habitat shapefile
#project both to longlat
subregions <- readOGR("D:/Lab Data/Florida/Unified Florida Reef Tract v 2.0 2017/Regions Shapes","Unified_Florida_Reef_Map_v2.0_Regions")
regionslonglat<- spTransform(subregions, longlat)

text(-82.85, 24.41, "Dry Tortugas")
text(-82.25, 24.47, "Marquesas")
text(-81.38, 24.4, "Lower Florida Keys")
text(-80.58, 24.6, "Middle Florida Keys")
text(-80.17, 24.85, "Upper Florida Keys")
text(-80.6, 25.6, "Biscayne Bay")
text(-80.55, 25.9, "Broward-Miami")

#Adding in hardbottom and coral reef
################CORAL REEF#######################
# add shapefile on top of the existing raster
image(hb_shape_crop, col=cols12, add = TRUE, legend=FALSE)
image(cr_shape_crop, col=cols11,add = TRUE,  legend=FALSE)
legend("topleft", legend=c("Optimal Habitat","Coral Reef","Hardbottom", "Mission: Iconic Reefs"),col=c("#238B45","#F0027F","#FFFF33","#525252"), pch=c(15,15,15,3), cex=1.3, bg='white',border = "black")

#ADD IN REGION OUTLINE AFTER HARDBOTTOM LAYER
plot(regionslonglat,add=TRUE)

#MISSION: ICONIC REEFS
setwd("C:/Users/rbani/OneDrive/Desktop/Florida Tech/PhD Chapters/Chapter 2/Florida Group/R")
MIR=read.csv('MIR_Locations.csv')
MIR <- cbind(MIR$Longitude, MIR$Latitude)
cols7 <- brewer.pal(9, "Greys")
cols7
points(MIR, pch = 3, cex = 1.9, col='#525252',lwd=1.2)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#SUPPLEMENTARY FILES S2, S3, & S4
### GOOGLE EARTH KMZ FILES ###
library(raster)
library(rgdal)
library(plotKML)
library(sp)

#Set where S2,S3, & S4 files will end up
setwd("C:/Users/rbani/OneDrive/Desktop/Manuscripts/Florida SDM/Submission October 2023/Submission November 2023")

pgtr_Kml <- projectRaster(pg > tr, crs = "+proj=longlat +init=epsg:4326 +no_defs", method = 'ngb',col=cols)

#CREATE KML/KMZ FILES
KML(pgtr_Kml, 'S2', col = cols10)
KML(cr_shape_crop, 'S4', col = cols11)
KML(hb_shape_crop, 'S3', col = cols12)
