## README

Metadata for the manuscript 'Introduced wild pigs affect the foraging ecology of a native predator as both prey and scavenger' published in XXXX by MA Parsons, JA Dellinger, JV Lombardi, KC VerCauteren, and JK Young.

The data and R code provided here can be used to recreate all results from this manuscript. The data files included several raster layers of topographic and landcover variables, photo data from a camera grid, cougar kill site information, photo data from kill sites, GPS data from cougar collars, and cougar scat data.

The R scripts conduct all analyses including estimating density of deer and pigs, analyzing cougar diet, analyzing the effects of pig scavenging on cougars, and analyzing cougar habitat selection.

### DATA FILES

The datafiles include:

1. Raster layers of landcover and topographic data
2. Camera station and photo data from a camera grid used to monitor deer and pigs
3. Cougar kill site locations and data
4. Isotope data

#### GIS Data
#### Vegetation
##### log_forest_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to forest cover. This layer was derived from the 2021 National Landcover Database (https://www.mrlc.gov/data; classes 41, 42, and 43).

##### log_grass_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to herbaceous cover. This layer was derived from the 2021 National Landcover Database (classes 71, 72, 73, 74, 81, 82).

##### log_riparian_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to riparian cover. This layer was derived from the 2021 National Landcover Database (classes 11, 12, 90, 95).

##### log_shrub_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to shrub cover. This layer was derived from the 2021 National Landcover Database (classes 51, 52).

##### agri_dist.tiff

This raster layer is a 30x30m resolution raster of the log transformed distance to agricultural cover. This layer was derived from the 2021 National Landcover Database.

##### propforested_289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of forest cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### propgrass_289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of herbaceous cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### propriparian_289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of riparian cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### propshrub289cell.tif

This raster layer is a 30x30m resolution raster of the proportion of shrub cover within a 17x17 cell moving window (510m) derived from teh 2021 National Landcover Database.

##### is_land.tiff

This raster layer is a 30x30m resolution raster of whether a cell is terrestial land or open water. This layer was derived from the 2021 National Landcover Database (all classes except open water) and was used to mask ocean areas from modeled habitats.

#### NDVI***_289cell.tif

These rasters are 30mx30m resolution rasters of the average seasonal NDVI value for four seasons: summer (May-July), fall (August-October), winter (November-January), and spring (February-April). These data are derived from MODIS 16-day data at the 250m scale.

#### Topographic

##### resampled_DEM.tiff

This raster layer is a 30x30m resolution raster of elevation. This layer was derived from the USGS 1/3 arc second digital elevation model (https://apps.nationalmap.gov/downloader/)

##### resampled_Slope.tiff

This raster layer is a 30x30m resolution raster of slope calculated from resampled_DEM.tiff using the terra package in R.
##### resampled_TRI.tiff

This raster layer is a 30x30m resolution raster of the terrain ruggedness index calculated from resampled_DEM.tiff using the terra package in R.

##### resampled_TPI.tiff

This raster layer is a 30x30m resolution raster of the topographic position index calculated from resampled_DEM.tiff using the terra package in R.

##### TRI_500m.tif

This raster layer is a 30x30m resolution raster of the terrain ruggedness index averaged over a 17x17 cell moving window (510 m).

##### TPI_500m.tif

This raster layer is a 30x30m resolution raster of the topographic position index averaged over a 17x17 cell moving window (510m).

#### Other

##### HuntingRaster.tif

This raster layer is a 30mx30m resolution raster that defines the subset of areas withing the study area that are closed to hunting.

##### FHLBoundary.shp
##### FHLRivers.shp
##### FHLBoundNoHoles.shp

These three shape files are the boundary and rivers of the Fort Hunter Liggett and are used for mapping and figure making.

#### Camera Data

##### GridProblems_correctedseasons.csv

This csv file contains the the camera station locations and deployment dates. Each row represents a camera deployment and columns provide information on the deployment. There are 26 columns:

1. Station ID - the name of the camera location, each location was sampled multiple times
2. CSY - the camera-season-year combination of the deployment
3. CSY2 - the camera-season-year combination plus a unique identifier when there were multiple deployments at a location within a season. This occurred when cameras were moved or replace because of technical issues. This is a unique identifier for each deployment.
4. UTM_X - the UTM easting of the camera deployment
5. UTM_Y - the UTM northing of the camera deployment
6. Habitat - a broad habitat category for the camera location
7. Distance - the distance of the 100% detection zone in meters. All detection zones used a 30 degree field of view
8. Set - was this location part of the "even" set of cameras or "odd" set of cameras during our camera rotations
9. Season - season of the camera deployment
10. Year - year of the camera deployment
11. Set Date - the date the camera was deployed
12. Pull - the date the camera was removed from the location
13. Deployment_days - the total length of the deployment 
14. Camera Type - The type of camera that was used: Browning (Brown), Bushnell (Bush), Reconyx. Old (O), New (N), UCD (from project partner)
15. Problem*_From - The start date of a problem with the camera that prevented detection (e.g., card filled, batteries died, animal damaged/displaced). Max of 4 problems for a single deployment
16. Probem*_to - The end date of a problem with the camera
17. Problem*_days - The number of days the problem persisted

##### Deer_and_pig_visits.csv

This csv file contains the information of visits to cameras by deer and pigs and is used to run the Random Encoutner Staying Time model. Each row represents a visit by a single animal to the camera. There are 10 columns:

1. CSY - the camera-season-year combination of the camera deployment where the visit occurred
2. CameraID - the station ID of the camera where the visit occurred
3. Date - the date of the visit
4. Time - the time the visit began
5. season - the season of the visit
6. year - the year of the visit
7. Species - the species for each visit. either deer or pig
8. Start - the start time of the visit in seconds (origin 1/1/1970)
9. Stop - the stop time of the visit in seconds (origin 1/1/1970)
10. visit_length - the duration of the visit in seconds


#### Cougar kill site and scat information

#### ClusterMasterSheet.csv

We are unable to share these raw data because of their sensitive nature. We have uploaded a censored version of this data that does not include GPS locations (ClusterMasterSheet_noGPS.csv). 

This csv files contains the metadata for each GPS cluster (e.g., formation date, duration, radius). It matches with the cougar_cluster_investigations file for analysis. Each row is a cluster and there are 14 columns.

1. AID - the ID of the individual cougar associated with the cluster
2. clus_ID - the name of the cluster, unique within each individual cougar
3. clus_start - the date and time of the first GPS location included in the cluster
4. clus_end - the date and time of the last GPS location included in the cluster
5. open - Unnecessary, only used during monitoring to indicate if a cluster was active or complete
6. lat - the latitutde of the GPS cluster
7. lon - the longitude of the GPS cluster
8. dur - the duration of the cluster in hours. clus_end minus clust_start
9. locs - the number of GPS locations for the cougar included in the cluster
10. radius - the radius of the cluster
11. inv date - the date the cluster was investigated
12. found - indicator of whether a prey item was found at the cluster (Y = yes, N = no)
13. species - the species of the prey item found
14. notes - any notes regarding the cluster


##### cougar_cluster_investigations

We are unable to share these raw data because of their sensitive nature. We have uploaded a censored version of this data that does not include GPS locations (cougar_clusters_noGPS.csv). 

This csv file includes information on cougar kill site investigations. Each row represents a cluster of cougar GPS points that was investigated because it met a criteria to be a possible kill site. Each column provides information on the cluster. There are 62 columns.

1. cluster_id - the name of the GPS cluster investigated
2. cougar_id - the ID of the individual cougar associated with the cluster
3. type - what type of cluster was investigated. The majority are "current" clusters indicated that they were investigated soon after formation. Other options include April (clusters investigated long after formation), day (presumed daybed locations), Den (confirmed den locations), and trapping (area where we deployed bait to trap animals). Only 'current' clusters were used in analysis.
4. collar_serial - the serial number of the cougar's GPS collar
5. n_points - the number of points in the GPS cluster
6. radius - the radius of the GPS cluster
7. duration_h - the duration between the first and last point at the GPS cluster
8. form_date - the date of the first point at the GPS cluster
9. centroid_lat - the latitude of the centroid of the GPS cluster
10. centroid lon - the longitude of the centroid of the GPS cluster
11. inv_date - the date we investigated the GPS cluster
12. personnel - initials of the investigators
13. time_200m - the time of day we reached 200m from the cluster centroid
14. habitat - broad habitat category that the cluster was in
15. habitat2 - broad secondary habitat category that the cluster was in
16. canopy - whether the site had open, moderate, or closed canopy cover
17. carcass_found - whether or not a carcass was found at the site
18. time_found - the time of day the carcass was found
19. carcass_lat - the latitude the carcass was located at
20. carcass_lon - the latitude the carcass was located at
21. species - the species of the located carcass
22. how_id - what features were used to identify the carcass
23. sex - the sex of the carcass
24. how_sex - what features were used to determine the sex of the carcass
25. age - the age of the carcass. Neonate (<6 mo), Fawn/Calf (6 - 18 mo), yearling (18 - 30 mo), adult (>30 mo)
26. how_age - what features were used to determine the age of the carcass
27. est_age - the estimated age of the carcass
28. cacehd - whether or not the caracss was cached currently, previously, or not at all
29. n_prey - the number of prey items at the cluster
30. n_piles - the number of piles of remains
31. condition - the condition of the carcass (intact, dismantled, dispersed)
32. parts - a description of what parts of the carcass were located
33. bite_wounds - whether bite wounds were located
34. hemorrhaging - whether hemorrhaging was located and if so the location of hemorrhaging
35. fed_on - whether the carcass had been fed on
36. per_rem - the rough percentage of biomass remaining on the carcass
37. feeding_use - a description of the observed feeding sign on the carcass
38. marrow - the color and texture of the bone marrow from long bones
39. drag_trail - whether a drag trail was located
40. length - the length of the drag trail
41. kill_site - whether a kill site was located
42. kill_lat - the latitude of the kill site
43. kill_lon - the longitude of the kill site
44. evidence_kill_site - the evidence used to identify the kill site
45. consistent_date - whether the condition of the carcass is consistent with the cluster formation date
46. confidence_cougar_kill - confidence level in whether a cougar killed or scavened the prey. positive, probable, unknown, or scavenge
47. other_sign - sign of other species that was observed at the carcass
48. scavenged - whether or not the carcass was scavenged by other species. Y, N, or Likely
49. scavened_by - what other species scavenged the carcass
50. kill_2 - wether there was a second kill at the cluster
51. species - the species of the second prey item
52. cached - whether the second prey item was cached
53. notes - any notes about the cluster investigation
54. cache_photo - whether photos were taken of the cache
55. carcass_photo - whetehr photos were taken of the carcass
56. bite_photo - whether photos were taken of the bite wounds
57. hem_photo - whether photos were taken of the hemorrhaging
58. drag_photo - whether photos were taken of the drag trail
59. kill_photo - whether photos were taken of the kill site
60. coug_sign_photo - whether photos were taken of cougar sign
61. scav_sign_photo - whether photos were taken of scavenger sign
62. time_leave - the time of day we left the GPS cluster


#### scats_NODUPLICATES.csv

This csv files contains the data from macroscopic analysis of cougar scats. Each row is an individual scat and there are 14 columns.

1. Date - the date the scat was processed
2. Obs - the initials of the observer who processed the scat
3. Sample_ID - the ID of each scat sample. Can be either a numerical identifier or a cluster name identifying what feeding site the scat came from
4. Species_1 - four letter code for the most abundant species in the scat
5. Sp1_% - the volumetric percentage of species 1
6. Species_2 - four letter code for the second most abundant species in the scat
7. Sp2_% - the volumetric percentage of species 2
8. Species_3 - four letter code for the third most abundant species in the scat
9. Sp3_% - the volumetric percentage of species 3
10. Species_4 - four letter code for the forth most abundant species in the scat
11. Sp4_% - the volumetric percentage of species 4
12. Notes - additional notes about the scat. Some notes contain the cluster name that associates with the kill site data.
13. Season - whether the scat was collected in summer or winter
14. Cluster - whether the scat was collected from a known feeding site or not (Y = yes, N = no)

Four letter codes for species are:
BIRD - remains of small birds unidentifiable to species
BONE - bone fragments
CACA - Castor canadensis
CALA - Canis latrans
CECA - Cervus canadensis
LECA - Lepus californicus 
MEGA - Meleagris gallopavo
ODHE - Odocoileus hemionus columbianus
OTBE - Otospermophilus beecheyi
PLANT - plant material, incidentally consumed or stuck to scat when collected
PRLO - Procyon lotor
PUCO - Puma concolor
SCGR - Sciurus griseus
Small Mammal - small mammal remains not identified to species
SUSC - Sus scrofa
SYBA - Sylvilagus bachmani
UNK - unknown species
UNK1 - unknown species if two unknowns in the same scat


#### Pig scavenging data

#### TimelapseData_KillSites_***.csv

These four files contain the photo data from cougar killsites that we monitored with cameras to document scavenging. All four files contain the same information for photos from different seasons. Each row is an individual photo and there are 15 columns.
This data is paired with the cougar kill site data to identify what kills were and were not scavenged by pigs.

1. File - the filename of the photo
2. RelativePath - the location of the photo. Each relative path represents an individual kill site and should match a kill in the cluster master sheet data. There are some minor differences that the code corrects.
3. Folder - the folder that contains all other folders/relative paths in each .csv file
4. Date - the date the photo was taken, recorded by the camera
5. Time - the time the photo was taken, recorded by the camera
6. ImageQuality - default column included by timelapse. Did not use so all will be "OK"
7. DeleteFlag - default column included by timelapse. Did not use so all will be "FALSE"
8. Species - species ID of any animal observed in the photo
9. Species2 - species ID of any second species for photos that included multiple species
10. SpeciesCount - the number of individuals of the primary species in the photo
11. Species2Count - the number of individuals of species 2 in the photo
12. IfOther - text identifier for species that were identified as "Other" in the species column
13. IfOther2 - text identifier for species that were identified as "Other" in the species2 column
14. Comment - any additional information about the photograph
15. Pig Behavior - Identifier of pig behavior that was observed in each photo. Only used for photos of pigs. Options include: moving, vigilant, investigating, and feeding. Only feeding behavior was counted as pig scavenging.


#### Cougar GPS Location Data

We are unable to share these raw data because of their sensitive nature. I will still describe their contents below.

##### Cougar GPS collar data (6 files)

1. collar44744_SF09_02-10-2021_12-21-2022.csv
2. collar44746_SF09_12-21-2022_12-31-2023.csv
3. collar44747_SM06_02-09-2021_12-18-2022.csv
4. collar44748_SF1_02-05-2021_11-26-2021.csv
5. collar45771_SM06_12-18-2022_12-31-2023.csv
6. SLO Data 211130.csv

These csv files include the raw GPS location data for the GPS collared cougars. Each file is for a different collar, but not always a different animal, as two animals each had two different collars through the project. Each row represents an individual GPS location and each column provides information on that location. There are 52 columns, I will describe the important ones here.

1. No - the ID number for the GPS location
2. Collar ID - the collar serial number
3. UTC_Date - the date the GPS location was acquired (UTC)
4. UTC_Time - the time the GPS location was acquired (UTC)
5. LMT_Date - the date the GPS location was acquired (still UTC)
6. LMT_Time - the time the GPS location was acquired (still UTC)
7. Origin - whether the location data was transmitted by the satellite (iridium) or downloaded from the collar (collar)
8. SCTS_Date - the date the location was retrieved from the satellite/collar
9. SCTS_Time - the time the location was retrieved from the satellite/collar
13. Latitude - the latitude of the GPS location
14. Longitude - the longitude of the GPS location
15. Height - the elevation of the GPS location
16. DOP - the dilution of precision of the GPS location

The SLO Data 211130.csv contains slightly different columns than the other data. These data are from cougars that were collared during the first phase of the project.

1. ID - the ID of the cougar
2. LMT_Date - the date the location was collected
3. Latitude - the latitude of the GPS location
4. Longitude - the longitude of the GPS location
5. Easting - the UTM zone 10 easting value of the location
6. Northing - the UTM zone 10 northing value of the location
7. Day - the day the location is from
8. Month - the month the location is from
9. Year - the year the location is from
10. Hour - the hour of day that the location was taken
11. Sex - sex of the collared cougar (1 = male, 0 = female)
12. StudyArea - study area where the cougar was collared. Will be "CC" for all locations (central coast).

#### cougar_collars.csv

This file contains a row for each indiviudal cougar collar that was used in the project. There are 9 columns.

1. collar_id - the numerical ID of the collar
2. drop_id - the numerical ID of the dropoff mechanism 
3. vhf_freq - the frequency of the VHF beacon in MHz
4. dropoff_date - the scheduled dropoff date of the GPS collar
5. gps_schedule_hr - the hours between GPS locations
6. vhf_beacon_start - the time each day that the VHF beacon turns on
7. vhf_beacon_stop - the time each day that the VHF beacon turns off
8. mortality_schedule - the duration of non-activity that triggers a mortality signal
9. model - model of the VHF collar. All collars are Vectronic Vertex Lite

#### cougar_collar_deployments.csv

This file contains the collar deployment data for each GPS collar deployed during the second phase of cougar captures (2021 - 2022). Each row represents a collar deployment and there are 9 columns

1. deployment_id - a numerical identifier unique to each deployment
2. collar_id - the numerical ID of the GPS collar
3. cougar_id - the ID of the cougar the collar was deployed on
4. start_date - the date the collar was deployed
5. start_day - the day of the month that the collar was deployed
6. start_month - the month the collar deployment started
7. start_year - the year the collar deployment started
8. end_date - the date the collar deployment ended
9. end_cause - the reason the collar deployment ended (e.g., collar removed, mortality) 


#### cougar_kitten_dates.csv

This file contains the dates that each collared female cougar was known to have kittens during the diet monitoring portion of the study. Each row represents a different litter of kittens and has 4 columns.

1. cougar_id - the of the collared female cougar
2. kitten_start - the best estimated date that the litter of kittens was born
3. kitten_end - the best estimated date that the adult female was no longer travelling with any kittens
4. min_num - the best estimated number of kittens present



1. Extracting camera covariates. Also used later to make layers of predicted deer and pig density
2. Run the random encounter staying time model for deer and pigs. Seperate file for each species
3. Analyze deer body condition and fawn to doe ratios to evaluate if there is evidence of competition with pigs
4. Analyze the isotope data

##### 01_CameraCovariates.R 

This file uses the camera station data and GIS data to extract covariates for each camera location
It creates and saves a new file (CameraCovariates_NDVI_289_log.csv) that is needed by the random encounter staying time model
Then, starting on Line 174, this file creates rasters of predicted deer and pig density needed for the deer body condition models
The second half of this script cannot be run until the REST model has been run and output saved.

##### 02_RESTAnalysis_NoVS_Randomeffect_deer_20240405_logunscale.R
##### 03_RESTAnalysis_NoVS_Randomeffect_pig_20240405_logunscale.R

These files use the camera station data, camera covariate data (created from previous script), and deer and pig visit data.
It processes data and runs the random encounter staying time model for the species.
It then runs a cross validation of the model to test model performance
Note that these models can take several hours to run depending on the number of iterations, chains, and computer specs
The beginning of this code is also used to estiamte deer and pig temporal overlap

##### 04_HomeRanges_NewCollars.R

This file uses the collar data from 2021-2023 capture efforts and estimates a 100% minimum convex polygon home range for each cougar

##### 05_Kill Summary_correctedages.R

This file analyzes cougar diet data for each cougar and for the population as a whole based on cluster data
It uses the cluster data and analyzes diet composition by season and age class of prey. It also creates several figures that are included in the manuscript.
Finally, it runs generalized linear models to test if predation of pigs varies by season, cougar ID or cougar sex.

##### 06_Scatanalysis.R

This file analyzes the cougar scat data. The only input is the scats_NODUPLICATES.csv file. The code processes the data to calculate the frequency of occurrence and volume of each prey item in each scat. 
The code then analyzes the data to evaluate if diet based on scat varies by season or based on where scats were collected (e.g., feeding sites, day beds, scat detection dogs).

##### 07_LionSSF.R

This file runs an integrated step selection analysis for GPS collared cougars to evaluate habitat selection within home ranges.
This codes uses the cougar GPS collar data and GIS variables. It runs iSSA's with only landscape variables and with both landscape and prey variables to determine if including prey density improves model performance.

##### 08_RSF_multiple-animals_withprey.R

This file runs a resource selection function for kill site habitat selection by collared cougars and evaluates if included prey density in the RSF improves model performance.
The code uses the cougar GPS collar data, cluster data, and GIS variables. For each cougar it uses the previously generated MCP home range to create many available points.
It then uses the 'amt' package to run a RSF to determine what habitat variables are associated with cougar kills. It then compares models with only landscape variables to models with both landscape and prey variables.
Finally, it makes the effects plot figures from the manuscript.

##### 09_Pig Scavenging and Kill Rates_correctedages.R

This file combines the cougar kill site data and pig scavenging data to evaluate if pig scavenging affects cougar foraging behavior at individual kills and over monitoring periods >21 days.
The code uses the cluster investigation data and photo data from feeding sites. It first merges the pig scavenging data from photo data with the cluster investigation data. It then uses the photos and sign surveys to identify cougar kills that were scavenged.
Once the data are processed, the code identifies continuous monitoring periods for cougars and summarizes feeding behavior during those periods. It then runs generalized linear models to test if cougar kill rates are affected by scavenging.
Next it tests if cougar feeding duration at an individual kill depends on pig scavenging.
Finally, it uses only the photo data to describe pig scavenging behavior and whether behavior (frequency of discovery, time to discovery, feeding behavior) differs between different prey species. 
