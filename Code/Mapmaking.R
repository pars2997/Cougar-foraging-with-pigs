library(terra)
library(sf)
library(ggplot2)
library(tidyterra)
library(ggmap)
library(tidyverse)
library(nngeo)

sites <- read.csv("../Raw Data/cameradata/GridProblems_correctedseasons.csv")
head(sites)

bounds <- st_as_sfc(st_bbox(c(xmin = min(sites$UTM_X) - 50000,
                  xmax = max(sites$UTM_X) + 50000,
                  ymin = min(sites$UTM_Y) - 10000,
                  ymax = max(sites$UTM_Y) + 10000),
                  crs = st_crs(32610)))

sites <- sites %>% 
  select(UTM_X, UTM_Y, Station.ID) %>% 
  filter(!duplicated(Station.ID))
sites
sites <- st_as_sf(sites, coords = c("UTM_X","UTM_Y"),crs = 32610)
sites <- st_transform(sites, crs = 4326)
plot(sites)

FHLBound <- read_sf("../GIS Data/FHLBoundary.shp")
FHLBoundUTM <- sf_transform_xy(FHLBound,32610,4326)
FHLBoundUTM <- st_remove_holes(FHLBoundUTM)


# install.packages("basemaps")
library(basemaps)
library(ggspatial)
# get_maptypes()

set_defaults(map_service = "esri",map_type = "world_hillshade")
topomap <- basemap_geotif(ext = bounds)
topomap2 <- rast(topomap)
# refmap <- basemap_geotif(ext = bounds, map_service = "esri",map_type = "world_reference_overlay")

topomap2 <- terra::project(topomap2,y = "epsg:4326")
# refmap2 <- rast(refmap)
# refmap2 <- terra::project(refmap2, y = "epsg:32612")

sitemap <- ggplot()+
  geom_spatraster(data = topomap2, aes(fill = red))+
  scale_fill_gradient(low = "grey60",high = "grey90")+
  guides(fill = "none")+
  xlim(-121.5,-121.0)+
  ylim(35.75,36.15)+
  # geom_spatraster(data = refmap2, aes(fill = red)) +
  geom_sf(data = sites, color = "black", size = 4) +
  geom_sf(data = FHLBound, color = "black", fill = NA, lwd = 1) +
  theme(panel.grid = element_blank())+
  theme(panel.border = element_blank())+
  # theme(panel.background = element_blank())+
  annotation_scale(
    width_hint = 0.3,
    location = "bl",
    bar_cols = c("black","white"),
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    text_cex = 1.25
  )+
  annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.6, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("black","white"),
      text_size = 16,
      line_width = 0
    )
  )+
  theme(text = element_text(size = 18),axis.text.x = element_text(angle=45, hjust=1))+
  xlab("Latitude")+
  ylab("Longitude")+
  theme(panel.background = element_blank())
  # theme(legend.position = "inside")+
  # theme(legend.position.inside = c(0.225,0.86))

sitemap  
  

library(spData)
data("us_states",package = "spData")
us_states = st_transform(us_states, crs = 4326)

insetmap <- ggplot() + 
  geom_sf(data = us_states, fill = "white") + 
  geom_point(aes(x = -121.25, y = 36.00),pch = 18, size = 5)+
  theme(legend.title = element_blank())+
  xlab("")+
  ylab("")+
  coord_sf(datum = st_crs(sites))+
  xlim(-124,-114.5)+
  ylim(32,42)+
  theme(panel.grid = element_blank())+
  theme(panel.background = element_blank())+
  theme(axis.ticks = element_blank())+
  theme(axis.text = element_blank())+
  theme(plot.background = element_rect(color = "black",fill = "white"))
insetmap
  
# install.packages("cowplot")
library(cowplot)

gg_inset_map <- ggdraw()+
  draw_plot(sitemap)+
  draw_plot(insetmap,x = 0.74, y = 0.68, width = 0.2, height = 0.2)
gg_inset_map

tiff(filename = "../Figures/insetmap.tiff",width = 6, height = 7, units = "in", 
     compression = "lzw",res = 300)
gg_inset_map
dev.off()
