#################################################################
######## Extract GIS covariates for camera locations ############
############ Mitchell Parsons ###################################
############# February 23/2024 ##################################

#### The second half of this script uses the covariates and output for REST 
#### models to predict pig and deer density on the landscape
#### I kept it as one script to avoid having to read in data again
#### So the first half needs to be run before running rest models
#### And the second half can't be run until after

#### Load packages ####
library(raster)
library(amt)
library(sf)
library(tidyverse)
library(nngeo)
library(terra)
library(ggpubr)
library(ggplot2)
library(tidyterra)
library(gridExtra)

#### Read in data ####
# read in camera locations and format columns
cameralocs <- read.csv("../RawData/GridProblems_correctedseasons.csv")
cameralocs$xcord <- cameralocs$UTM_X
cameralocs$ycord <- cameralocs$UTM_Y

# Add start and end dates for seasons
cameralocs <- cameralocs %>% 
  mutate(seasonstart = case_when(
    Season == "summer" & Year == 2021 ~ mdy("05/01/2021"),
    Season == "fall" & Year == 2021 ~ mdy("08/01/2021"),
    Season == "winter" & Year == 2021 ~ mdy("11/01/2021"),
    Season == "spring" & Year == 2022 ~ mdy("02/01/2022"),
    Season == "summer" & Year == 2022 ~ mdy("05/01/2022"),
    Season == "fall" & Year == 2022 ~ mdy("08/01/2022"),
    Season == "winter" & Year == 2022 ~ mdy("11/01/2022"),
    Season == "spring" & Year == 2023 ~ mdy("02/01/2023"),
  )) %>% 
  mutate(seasonend = case_when(
    Season == "summer" & Year == 2021 ~ mdy("07/31/2021"),
    Season == "fall" & Year == 2021 ~ mdy("10/31/2021"),
    Season == "winter" & Year == 2021 ~ mdy("01/31/2022"),
    Season == "spring" & Year == 2022 ~ mdy("04/30/2022"),
    Season == "summer" & Year == 2022 ~ mdy("07/31/2022"),
    Season == "fall" & Year == 2022 ~ mdy("10/31/2022"),
    Season == "winter" & Year == 2022 ~ mdy("1/31/2023"),
    Season == "spring" & Year == 2023 ~ mdy("04/30/2023"),
  ))

# Create sf object of camera locations 
cameralocs <- st_as_sf(cameralocs, coords = c("UTM_X", "UTM_Y"), crs = 32610)

# Read in raster layers of all covariates
# Elevation, slope, terrain ruggedness, and topographic position
# Proportion of different landcovers
# Distance to different landcovers
# Hunting and NDVI
DEM <- rast("../ProcessedData/resamp_DEM.tiff")
slope <- rast("../ProcessedData/resamp_Slope.tiff")
TPI <- rast("../ProcessedData/resamp_TPI.tiff")
TRI <- rast("../ProcessedData/resamp_TRI.tiff")
TPI_500 <- rast("../ProcessedData/TPI_500m.tif")
TRI_500 <- rast("../ProcessedData/TRI_500m.tif")
riparian_dist <- rast("../ProcessedData/log_riparian_dist.tiff")
propshrub <- rast("../ProcessedData/propshrub_289cell.tif")
propgrass <- rast("../ProcessedData/propgrass_289cell.tif")
propforest <- rast("../ProcessedData/propforested_289cell.tif")
propriparian <- rast("../ProcessedData/propriparian_289cell.tif")
hunting_rast <- rast("../ProcessedData/HuntingRaster.tif")
NDVIS21 <- rast("../ProcessedData/NDVIS21_289cell.tif")
NDVIF21 <- rast("../ProcessedData/NDVIF21_289cell.tif")
NDVIW21 <- rast("../ProcessedData/NDVIW21_289cell.tif")
NDVISp22 <- rast("../ProcessedData/NDVISp22_289cell.tif")
NDVIS22 <- rast("../ProcessedData/NDVIS22_289cell.tif")
NDVIF22 <- rast("../ProcessedData/NDVIF22_289cell.tif")
NDVIW22 <- rast("../ProcessedData/NDVIW22_289cell.tif")
NDVISp23 <- rast("../ProcessedData/NDVISp23_289cell.tif")
shrub_dist <- rast("../ProcessedData/log_shrub_dist.tiff")
grass_dist <- rast("../ProcessedData/log_grass_dist.tiff")
forest_dist <- rast("../ProcessedData/log_forest_dist.tiff")
agri_dist <- rast("../ProcessedData/agri_dist.tiff")

# hunting_rast2 <- classify(hunting_rast, matrix(c(1,NA,0,1),nrow = 2, byrow = T))
# hunting_dist <- log(distance(hunting_rast2)+1)

#### Extract covariates to camera locations ####
# Create a raster stack of all covariates for easy extraction
camcovsrast <- c(DEM,TPI,TRI,TPI_500,TRI_500,slope,
                 riparian_dist, propshrub, propforest, propgrass,propriparian,
                 hunting_rast, NDVIS21, NDVIF21, NDVIW21, NDVISp22, NDVIS22, 
                 NDVIF22,NDVIW22, NDVISp23,shrub_dist,grass_dist,
                 forest_dist,agri_dist)
nlyr(camcovsrast)

# extrat variables to camera locations
camcovs <- terra::extract(x = camcovsrast, y = cameralocs)

# add column of csy ID and rename columns
camerastations <- as.data.frame(cbind(cameralocs$CSY2,camcovs))
colnames(camerastations) <- c("CSY2","ID","Elevation","TPI","TRI","TPI_500","TRI_500","slope","RiparianDist","PropShrub","PropForest","PropGrass","PropRiparian","hunting",
                              "NDVI_S21", "NDVI_F21", "NDVI_W21", "NDVI_Sp22",
                              "NDVI_S22", "NDVI_F22", "NDVI_W22", "NDVI_Sp23","shrub_dist",
                              "grass_dist","forest_dist","agri_dist")

# Make sure all variables are numeric
camerastations$Elevation <- as.numeric(camerastations$Elevation)
camerastations$slope <- as.numeric(camerastations$slope)
camerastations$TPI <- as.numeric(camerastations$TPI)
camerastations$TRI <- as.numeric(camerastations$TRI)
camerastations$TPI_500 <- as.numeric(camerastations$TPI_500)
camerastations$TRI_500 <- as.numeric(camerastations$TRI_500)
camerastations$RiparianDist <- as.numeric(camerastations$RiparianDist)
camerastations$PropShrub <- as.numeric(camerastations$PropShrub)
camerastations$PropForest <- as.numeric(camerastations$PropForest)
camerastations$PropGrass <- as.numeric(camerastations$PropGrass)
camerastations$hunting <- as.numeric(camerastations$hunting)
camerastations$NDVI_S21 <- as.numeric(camerastations$NDVI_S21)
camerastations$NDVI_F21 <- as.numeric(camerastations$NDVI_F21)
camerastations$NDVI_W21 <- as.numeric(camerastations$NDVI_W21)
camerastations$NDVI_Sp22 <- as.numeric(camerastations$NDVI_Sp22)
camerastations$NDVI_S22 <- as.numeric(camerastations$NDVI_S22)
camerastations$NDVI_F22 <- as.numeric(camerastations$NDVI_F22)
camerastations$NDVI_W22 <- as.numeric(camerastations$NDVI_W22)
camerastations$NDVI_Sp23 <- as.numeric(camerastations$NDVI_Sp23)
camerastations$shrub_dist <- as.numeric(camerastations$shrub_dist)
camerastations$forest_dist <- as.numeric(camerastations$forest_dist)
camerastations$grass_dist <- as.numeric(camerastations$grass_dist)
camerastations$agri_dist <- as.numeric(camerastations$agri_dist)

# join covariates with camera station data frame so all information is together
camerastations2 <- cameralocs %>% 
  left_join(camerastations, by = "CSY2")
camerastations2 <- as.data.frame(camerastations2)
# remove unnecessary column
camerastations2 <- camerastations2[,-54]

# Createa column for NDVI that matches the season of each camera station deployment
camerastations2 <- camerastations2 %>% 
  mutate(NDVI_Proper = case_when(
    grepl("summer_2021",CSY) ~ NDVI_S21,
    grepl("fall_2021",CSY) ~ NDVI_F21,
    grepl("winter_2021",CSY) ~ NDVI_W21,
    grepl("spring_2022",CSY) ~ NDVI_Sp22,
    grepl("summer_2022",CSY) ~ NDVI_S22,
    grepl("fall_2022",CSY) ~ NDVI_F22,
    grepl("winter_2022",CSY) ~ NDVI_W22,
    grepl("spring_2023",CSY) ~ NDVI_Sp23
  ))

# Write csv of camera covariates for future use. 
write.csv(camerastations2,"../ProcessedData/CameraCovariates_NDVI_289_log.csv", row.names = F)


#### This chunk of code calculates variables while ignoring season ####
# not necessary, but a variation that coulbe be used in the future

# head(camerastations2)
# 
# camstationssinseason <- camerastations2[!duplicated(camerastations2$Station.ID),]
# 
# DD2 <- camerastations2 %>% 
#   filter(Year == 2021) %>% 
#   group_by(Station.ID) %>% 
#   summarise(ndays = sum(Deployment_days))
# 
# camstationssinseason <- camstationssinseason %>% 
#   st_join(DD2, by = "Station.ID")
#
# write.csv(camstationssinseason,file = "../ProcessedData/CameraCovariates_NDVI_noseasons.csv", row.names = F)

################################################################################
################################################################################
###################### Deer and Pig Prediction Rasters #########################
################################################################################
################################################################################

## This chunk of code is for after REST models have been run
## It reads in model outputs and creates rasters of predicted deer and pig density
## I left it here since all variables are already read into the environment

#### Prep data for density predictions ####

# Read in additional layers for visualization
# boundary of Fort hunter liggett, major rivers, and cougar SM6's home range
FHLBound <- read_sf("../ProcessedData/FHLBoundary.shp")
Rivers <- read_sf("../ProcessedData/FHLRivers.shp")

# convert boundary and river shape files to UTM
# remove holes from the boundary
FHLBoundUTM <- sf_transform_xy(FHLBound,32610,4326)
RiversUTM <- sf_transform_xy(Rivers,32610,4326)
RiversUTM <- st_zm(RiversUTM)
RiversUTM <- st_intersection(RiversUTM,FHLBoundUTM)
FHLBoundUTM <- st_remove_holes(FHLBoundUTM)

# Rasterize boundary so you can multiply it by varaibles to avoid
# predicting outside the study area
FHLBoundUTM_rast <- terra::rasterize(FHLBoundUTM,DEM,fun = min)

cols.fill = c("Even" = "grey50", "Odd" = "grey1")

Cameraplot <- ggplot()+
  geom_spatraster(data = DEM,maxcell = 1000000) +
  xlim(640000,680000) +
  ylim(3960000,4000000) +
  geom_sf(data = cameralocs, size = 2)+
  # scale_color_manual(name = "Set",values = cols.fill)+
  scale_fill_viridis_c(option = "magma",direction = -1)+
  # labs(fill = expression("Deer density (#/km" ^2 *")"))+
  labs(fill = expression("Elevation (m)"))+
  geom_sf(data = FHLBound,color = "black",fill = NA,lwd = 1)+
  theme_bw()+
  theme(axis.title = element_text(size = 18),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 14))+
  # ggplot2::annotate("text", x = 641500, y = 3998500, label = "B", size = 6)+
  coord_sf(datum = st_crs(DEM))+
  xlab("Easting")+
  ylab("Northing")
Cameraplot



# write the holeless boundary file for later use
# st_write(FHLBoundUTM, "../GIS Data/FHLBoundNoHoles.shp")

# hunting_rast[is.na(hunting_rast)] <- 1

# Scale all raster data to match the values that were used in the REST model
# pulling the scaling parameters from the camera covariates data
# otherwise will scale across the whole raster range and cause issues
# riparian_dist_scale <- (riparian_dist - mean(camerastations2$RiparianDist))/sd(camerastations2$RiparianDist)
# grass_dist_scale <- (grass_dist - mean(camerastations2$grass_dist))/sd(camerastations2$grass_dist)
# DEM_scale <- (DEM - mean(camerastations2$Elevation))/sd(camerastations2$Elevation)
# DEM_scale2 <- DEM_scale * DEM_scale
# propforest_scale <- (propforest - mean(camerastations2$PropForest))/sd(camerastations2$PropForest)
# propgrass_scale <- (propgrass - mean(camerastations2$PropGrass))/sd(camerastations2$PropGrass)
# propriparian_scale <- (propriparian - mean(camerastations2$PropRiparian))/sd(camerastations2$PropRiparian)
# NDVIS21_scale <- (NDVIS21 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVIF21_scale <- (NDVIF21 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVIW21_scale <- (NDVIW21 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVISp22_scale <- (NDVISp22 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVIS22_scale <- (NDVIS22 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVIF22_scale <- (NDVIF22 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVIW22_scale <- (NDVIW22 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# NDVISp23_scale <- (NDVISp23 - mean(camerastations2$NDVI_Proper))/sd(camerastations2$NDVI_Proper)
# TPI_scale <- (TPI - mean(camerastations2$TPI))/sd(camerastations2$TPI)
# TPI_500_scale <- (TPI_500 - mean(camerastations2$TPI_500))/sd(camerastations2$TPI_500)
# TRI_500_scale <- (TRI_500 - mean(camerastations2$TRI_500))/sd(camerastations2$TRI_500)
# grass.elev <- propgrass_scale*DEM_scale
# grass.elev_scale <- (grass.elev - mean(as.numeric(scale(camerastations2$PropGrass)) * 
#                                          as.numeric(scale(camerastations2$Elevation))))/
#   sd(as.numeric(scale(camerastations2$PropGrass)) * as.numeric(scale(camerastations2$Elevation))) 

#### create raster of predicted pig density####

# Read in output from pig REST model
pigmod <- readRDS("pigcovsmodel_log.rds")

#1 pigpred <- exp(pigmod$mean$effects[1] +
#2                  pigmod$mean$effects[2]*riparian_dist +
#3                  pigmod$mean$effects[3]*grass_dist +
#4                  pigmod$mean$effects[4]*grass_dist*fall +
#5                  pigmod$mean$effects[5]*grass_dist*winter +
#6                  pigmod$mean$effects[6]*hunting_dist+
#7                  pigmod$mean$effects[7]*hunting_dist*fall+
#8                  pigmod$mean$effects[8]*riparian_dist*summer+
#9                  pigmod$mean$effects[9]*riparian_dist*fall+
#10                 pigmod$mean$effects[10]*TRI_500)

# predict average pig density (no season) and write raster
pigpred_average <- exp(pigmod$mean$effects[1] + 
                         pigmod$mean$effects[2]*riparian_dist + 
                         pigmod$mean$effects[3]*grass_dist +
                         pigmod$mean$effects[6]*hunting_rast+
                         pigmod$mean$effects[10]*TRI_500)
par(mar = c(5,5,2,2))
plot(pigpred_average*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3960000,4000000),
     xlab = "Easting", ylab = "Northing", cex.lab = 1.75, cex.axis = 1.25)
lines(FHLBound)
writeRaster(pigpred_average,filename = "../ProcessedData/PigDensityPred_average.tif")

is.land <- rast("../ProcessedData/is_land.tiff")
pig.filtered <- pigpred_average*is.land

pigplot <- ggplot()+
  geom_spatraster(data = pig.filtered,maxcell = 1000000) +
  xlim(640000,680000) +
  ylim(3960000,4000000) +
  scale_fill_viridis_c(option = "mako",direction = -1)+
  labs(fill = expression("Pred. pig\ndensity (#/km"^2*")"))+
  geom_sf(data = FHLBound,color = "black",fill = NA,lwd = 1)+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  ggplot2::annotate("text", x = 641500, y = 3998500, label = "(a)", size = 5)+
  coord_sf(datum = st_crs(pig.filtered))+
  xlab("Easting")+
  ylab("Northing")

pigplot
# Then predict pig density for each unique season and write rasters
pigpred_summer <- exp(pigmod$mean$effects[1] + 
                          pigmod$mean$effects[2]*riparian_dist + 
                          pigmod$mean$effects[3]*grass_dist +
                          pigmod$mean$effects[6]*hunting_rast+
                          pigmod$mean$effects[8]*riparian_dist+
                          pigmod$mean$effects[10]*TRI_500)


plot(pigpred_summer*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3960000,4000000))
writeRaster(pigpred_summer, filename = "../ProcessedData/PigDensityPred_Summer.tif")


pigpred_winter <- exp(pigmod$mean$effects[1] + 
                          pigmod$mean$effects[2]*riparian_dist + 
                          pigmod$mean$effects[3]*grass_dist + 
                          pigmod$mean$effects[5]*grass_dist + 
                          pigmod$mean$effects[6]*hunting_rast+
                          pigmod$mean$effects[10]*TRI_500)
plot(pigpred_winter*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3960000,4000000))
writeRaster(pigpred_winter, filename = "../ProcessedData/PigDensityPred_Winter.tif")
#### Visualize the average pig density ####

plot(pigpred_average,xlim = c(643000,678000),ylim = c(3962000,3998400),
     legend.args = list(text = "Pig density \n (#/km2) \n  ", cex = 1.75))

points(pigcamerastay$x,
       pigcamerastay$y,
       cex = sqrt(pigcamerastay$Density+0.5)/2,
       pch = 19)

#### Create rasters of predicted deer density ####
# same process as above, repeated for deer

#read in deer model
deermod <- readRDS("deercovsmodel_log.rds")

# deerpred_summer21 <- exp(deermod$mean$effects[1] +
#                             deermod$mean$effects[2]*riparian_dist +
#                             deermod$mean$effects[3]*propforest +
#                             deermod$mean$effects[4]*propforest*fall +
#                             deermod$mean$effects[5]*propforest*winter
#                             deermod$mean$effects[6]*hunting_rast+
#                             deermod$mean$effects[7]*hunting_rast*fall+
#                             deermod$mean$effects[8]*riparian_dist_scale+
#                             deermod$mean$effects[9]*riparian_dist_scale)

# predicted average deer density and write file for future use
deerpred_average <- exp(deermod$mean$effects[1] + 
                           deermod$mean$effects[2]*riparian_dist + 
                           deermod$mean$effects[3]*propforest +
                           deermod$mean$effects[6]*hunting_rast)
# plot(deerpred_average*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3950000,4000000))


deer.filtered <- deerpred_average*is.land

deerplot <- ggplot()+
  geom_spatraster(data = deer.filtered,maxcell = 1000000) +
  xlim(640000,680000) +
  ylim(3960000,4000000) +
  scale_fill_viridis_c(option = "mako",direction = -1)+
  # labs(fill = expression("Deer density (#/km" ^2 *")"))+
  labs(fill = expression("Pred. deer\ndensity (#/km"^2*")"))+
  geom_sf(data = FHLBound,color = "black",fill = NA,lwd = 1)+
  theme_bw()+
  theme(axis.title = element_text(size = 12),
        axis.text.x = element_text(angle=45, hjust=1),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))+
  ggplot2::annotate("text", x = 641500, y = 3998500, label = "(b)", size = 5)+
  coord_sf(datum = st_crs(pig.filtered))+
  xlab("Easting")+
  ylab("Northing")
deerplot

png("../Figures/PigDeerDensity_EA.png",width = 10, height = 4,
     res = 300, units = "in")
ggarrange(plotlist = list(pigplot,deerplot),ncol = 2)
dev.off()

plotsize <- matrix(c(1,2,
                     1,2,
                     1,2),
                   nrow = 3, byrow = T)
grid.arrange(pigplot,deerplot,layout_matrix = plotsize)
dev.off()


# predict density for each season and write files
deerpred_summer <- exp(deermod$mean$effects[1] + 
                           deermod$mean$effects[2]*riparian_dist + 
                           deermod$mean$effects[3]*propforest + 
                           deermod$mean$effects[6]*hunting_rast+
                           deermod$mean$effects[9]*riparian_dist)

deerpred_winter <- exp(deermod$mean$effects[1] + 
                           deermod$mean$effects[2]*riparian_dist + 
                           deermod$mean$effects[3]*propforest +
                           deermod$mean$effects[5]*propforest +
                           deermod$mean$effects[6]*hunting_rast)

plot(deerpred_average*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3960000,4000000))
plot(deerpred_summer*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3960000,4000000))
plot(deerpred_winter*FHLBoundUTM_rast,xlim = c(640000,680000),ylim=c(3960000,4000000))

writeRaster(deerpred_summer, filename = "../ProcessedData/DeerDensityPred_Summer.tif")
writeRaster(deerpred_winter, filename = "../ProcessedData/DeerDensityPred_Winter.tif")
writeRaster(deerpred_average,filename = "../ProcessedData/DeerDensityPred_average.tif")

#### Visualize the average deer density ####


plot(pigpred_average*FHLBoundUTM_rast,xlim = c(640000,680000),ylim = c(3960000,4000000),
     main = expression("Pig Density (#/km" ^2 * ")"))

plot(deerpred_average*FHLBoundUTM_rast,xlim = c(640000,680000),ylim = c(3960000,4000000),
     main = expression("Deer Density (#/km" ^2 * ")"))

points(pigcamerastay$x,
       pigcamerastay$y,
       cex = sqrt(pigcamerastay$Density+0.5)/2,
       pch = 19)

#### Moving window of deer and pigs ####
# This averages pig and deer density over a larger area to account for spatial error
# Change window size to change the scale of smoothing. 17 cells is approx 510 m x 510 m

windowsize <- 17
window <- matrix(rep(1,windowsize^2),ncol = windowsize, nrow = windowsize)

deerpred_summer_289 <- focal(x = deerpred_summer, w = window, fun = mean)
writeRaster(deerpred_summer_289, filename = "../ProcessedData/deerpred_summer_289cell.tif")

deerpred_winter_289 <- focal(x = deerpred_winter, w = window, fun = mean)
writeRaster(deerpred_winter_289, filename = "../ProcessedData/deerpred_winter_289cell.tif")

pigpred_summer_289 <- focal(x = pigpred_summer, w = window, fun = mean)
writeRaster(pigpred_summer_289, filename = "../ProcessedData/pigpred_summer_289cell.tif")

pigpred_winter_289 <- focal(x = pigpred_winter, w = window, fun = mean)
writeRaster(pigpred_winter_289, filename = "../ProcessedData/pigpred_winter_289cell.tif")

deerpred_average_289 <- focal(x = deerpred_average, w = window, fun = mean)
writeRaster(deerpred_average_289, filename = "../ProcessedData/deerpred_average_289cell.tif")

pigpred_average_289 <- focal(x = pigpred_average, w = window, fun = mean)
writeRaster(pigpred_average_289, filename = "../ProcessedData/pigpred_average_289cell.tif",overwrite = T)



# plot and look at smoothed pig density 
plot(deerpred_summer_289*FHLBoundUTM_rast,xlim = c(640000,680000),ylim = c(3960000,4000000))
points(cameralocs)
temp <- cbind(cameralocs,extract(pigpred_summer21_289,cameralocs))
