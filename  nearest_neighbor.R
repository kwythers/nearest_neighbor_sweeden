require(tidyverse)
require(data.table)
require(qqrepel)
require(dplyr)
require(ggplot2)
require(lubridate)
require(sp)
require(ggmap)
require(mapproj)
require(rspatial)
require(ggvoronoi)
require(rgeos)
require(geosphere)

# load data
Weather_Stations_Prec_20082012 <- read_csv("Desktop/Weather Stations Prec 20082012.dat")
View(Weather_Stations_Prec_20082012)

AllSwedishData_toMatchWeaSta <- read_csv("Desktop/AllSwedishData_toMatchWeaSta.dat")
View(AllSwedishData_toMatchWeaSta)

# sweden_precip_pts <- SpatialPoints(Weather_Stations_Prec_20082012[, 3:2], proj4string=CRS("+proj=longlat +datum=NAD83"))
# sweden_precip_pts <- SpatialPointsDataFrame(sweden_precip_pts, Weather_Stations_Prec_20082012)

sweden <- map_data("world") %>% filter(region == "Sweden")

sweden_map <- 
ggplot(data = Weather_Stations_Prec_20082012, aes(x = Longitude, y = Latitude)) +
#scale_fill_gradientn("Elevation", 
                       #colors=c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
                       #values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  #scale_color_gradientn("Elevation", 
                        #colors=c("seagreen","darkgreen","green1","yellow","gold4", "sienna"),
                        #values=scales::rescale(c(-60,0,1000,2000,3000,4000))) + 
  coord_quickmap() + 
  theme_minimal() + 
  theme(axis.text = element_blank(), 
        axis.title=element_blank()) 

sweden_map + geom_point(size = 0.5) + geom_path(data = sweden, aes(long, lat, group = group), color = "red") 

sweden_map +
  geom_voronoi(aes(fill = Elevation_m_asl), outline = sweden) + 
  geom_point(size = 0.5) 


p1 <- AllSwedishData_toMatchWeaSta %>% ##### p1 sites #####
  dplyr::select(Longitude, Latitude)
p2 <- Weather_Stations_Prec_20082012 %>% ##### p2 prec #####
  dplyr::select(Longitude, Latitude)

distp1p2 <- function(p1,p2) {
  dst <- sqrt((p1[1]-p2[1])^2+(p1[2]-p2[2])^2)
  return(dst)
}

dist2b <- function(y) which.min(apply(set2, 1, function(x) min(distp1p2(x,y))))
apply(set1, 1, dist2b)

sites_sp <- SpatialPoints(sites)
prec_sp <- SpatialPoints(prec)

sites$nearest_in_prec <- apply(gDistance(sites_sp, prec_sp, byid = TRUE), 1, which.min)




#test data
#First data set of stations
set1 <- structure(list(lon = c(13.671114, 12.866947, 15.94223, 11.099736,  
                               12.958342, 14.203892, 11.86389, 16.526674, 16.193064, 17.071392
), lat = c(48.39167, 48.148056, 48.721111, 47.189167, 47.054443, 
           47.129166, 47.306667, 47.84, 47.304167, 48.109444)), .Names = c("lon", 
                                                                           "lat"), row.names = c(NA, 10L), class = "data.frame")

#Second data set
set2 <- structure(list(lon = structure(c(14.4829998016357, 32.4000015258789, 
                                         -8.66600036621094, 15.4670000076294, 18.9160003662109, 19.0160007476807, 
                                         31.0990009307861, 14.3660001754761, 9.59899997711182, 11.0830001831055
), .Dim = 10L), lat = structure(c(35.8499984741211, 34.75, 70.9329986572266, 
                                  78.25, 69.6829986572266, 74.515998840332, 70.3659973144531, 67.265998840332, 
                                  63.6990013122559, 60.1990013122559), .Dim = 10L)), .Names = c("lon", 
                                                                                                "lat"), row.names = c(NA, 10L), class = "data.frame")

set1sp <- SpatialPoints(set1)
set2sp <- SpatialPoints(set2)

set1$nearest_in_set2 <- apply(gDistance(set1sp, set2sp, byid=TRUE), 1, which.min)


##### another esxample

# load libraries
# library(data.table)
# library(geosphere)
# library(UScensus2000tract)
# library(rgeos)

# get all combinations of origin and destination pairs
# Note that I'm considering here that the distance from A -> B is equal 
from B -> A.
odmatrix <- CJ(Datatwo$Code_A , Dataone$Code_B)
names(odmatrix) <- c('Code_A', 'Code_B') # update names of columns

# add coordinates of Datatwo centroids (origin)
odmatrix[Datatwo, c('lat_orig', 'long_orig') := list(i.Latitude, 
                                                     i.Longitude), on= "Code_A" ]

# add coordinates of facilities (destination)
odmatrix[Dataone, c('lat_dest', 'long_dest') := list(i.Latitude,  
                                                     i.Longitude), on= "Code_B" ]


Now you just need to:
  
  # calculate distances
  odmatrix[ , dist := distHaversine(matrix(c(long_orig, lat_orig), ncol 
                                           = 2), 
                                    matrix(c(long_dest, lat_dest), ncol  
                                           = 2))]

# and get the nearest destinations for each origin
odmatrix[, .(  Code_B = Code_B[which.min(dist)],
               dist = min(dist)), 
         by = Code_A]

### Prepare data for this reproducible example
# load data
data("oregon.tract")

# get centroids as a data.frame
centroids <- as.data.frame(gCentroid(oregon.tract,byid=TRUE))

# Convert row names into first column
setDT(centroids, keep.rownames = TRUE)[]

# get two data.frames equivalent to your census and facility data 
frames
Datatwo<- copy(centroids)
Dataone <- copy(centroids)

names(Datatwo) <- c('Code_A', 'Longitude', 'Latitude')
names(Dataone) <- c('Code_B', 'Longitude', 'Latitude')

        


