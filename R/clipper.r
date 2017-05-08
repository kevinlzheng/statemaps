#' A clip function to create an outliner for national or state maps
#' 
#' This function performed basic clip function to geoobj and add a polygon outside line
#' 
#' @param geoobj A geo-object, 
#' @param state a state abbrebriation, e.g. "CA", 
#' @param margin margin added to maps, %
#' @param yName yName for making equation labels
#' @examples
#' clip1 <- clipper(geoobj = us.states, state = unique(us.states$STATE_ABBR))  # USA map with  projection
#' plot(clip1,col = "red", border ="black", xpd =F)  
#' clip2 <- clipper(geoobj =  us.states, state = "CA")   # CA map with nhd2 projection
#' plot(clip2, col ="blue")
#' clip3 <- clipper(geoobj = us.counties, state = as.vector(unique(us.counties$STATE_ABBR)))  # USA map with  projection
#' plot(clip3,col = "red", border ="black", xpd =F) 
#' @export

clipper <- function(geoobj, state, margin = 0.1) {
  mystate <- subset(geoobj, STATE_ABBR %in% state)
  ranges <- mystate@bbox
  xdif <- diff(ranges[1,])*margin;  ydif <- diff(ranges[2,])*margin
  uppers <- c(ranges[1,1]-xdif, ranges[2,1]-ydif)
  lowers <- c(ranges[1,2]+xdif, ranges[2,2]+ydif)
  p1 <- Polygons(list(Polygon(cbind(c(uppers[1], lowers[1], lowers[1], uppers[1]),c(lowers[2],
                    lowers[2],uppers[2], uppers[2])))), "p1")
  sqr = SpatialPolygons(list(p1))
  proj4string(sqr) <- proj4string(geoobj)
  sqr2 <- gDifference(sqr, mystate)
  return(sqr2)
}

  
