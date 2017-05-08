#' A function to calculate distance (kilometer) between two locations with lat long coordinates
#' 
#' Modfied from https://www.r-bloggers.com/great-circle-distance-calculations-in-r/
#' https://rdrr.io/cran/prettymapr/src/R/scalebar.R
#' epsg = 4326 decimal degree
#' several functions to convert lat long to meters, projected to lat long 
#' 
#' @param lat1 Latitute for location 1 
#' @param lon1 Longitude for location 2
#' @param lat2 Latitute for location 1 
#' @param lon2 Longitude for location 2
#' @examples
#' latlon2meter(lat1 = 45, lon1 = -90, lat2 = 34.5, lon2 = -125.5)  # USA map with  projection
#' .geodist(x1 = 34.1, y1 = 45, x2 = 34.5, y2 = 45.5, epsg = 4326)
#' bx <- bbox(us.states)
#' latlon2meter(lat1 = bx[2,1], lon1 = bx[1,1], lat2 = bx[2,2], lon2 = bx[1,1])
#' @export

tolatlon <- function(x, y, epsg) {  # exist projected values into decimal degree
  # for lat/lon coordaintes just return x and y
  if(epsg == 4326) return(cbind(x, y))
  
  # require gdal for actual projections
  if(!requireNamespace("sp")) stop("package 'sp' is required for non lat/lon coordinates")
  if(!requireNamespace("rgdal")) stop("package 'rgdal' is required for non lat/lon coordinates")
  
  coords <- sp::coordinates(matrix(c(x,y), byrow=TRUE, ncol=2))
  spoints <- sp::SpatialPoints(coords, sp::CRS(paste0("+init=epsg:", epsg)))
  spnew <- sp::spTransform(spoints, sp::CRS("+init=epsg:4326"))
  c(sp::coordinates(spnew)[1], sp::coordinates(spnew)[2])
}

latlon2meter <- function (lon1, lat1, lon2, lat2){  # // generally used geo measurement function
  R =  6378.137  # ; // Radius of earth in KM
  dLat = (lat2 - lat1) * pi / 180;
  dLon = (lon2 - lon1) * pi / 180;
  a = sin(dLat/2) * sin(dLat/2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon/2) * sin(dLon/2);
  c = 2 * atan2(sqrt(a), sqrt(1-a));
  d = R * c;
  return (d)  #  kilometers
}

.geodist <- function(x1, y1, x2, y2, epsg) {
  lonlat1 <- tolatlon(x1, y1, epsg)
  lonlat2 <- tolatlon(x2, y2, epsg)
  
  long1 <- lonlat1[1] * pi / 180
  lat1 <-  lonlat1[2]* pi / 180
  long2 <- lonlat2[1]* pi / 180
  lat2 <- lonlat2[2] * pi / 180
  R <- 6371009 # Earth mean radius [m]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in m
}



