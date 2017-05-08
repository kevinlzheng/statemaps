#' A scale function to add beside a map 
#' 
#' Map scale was modified from the map package add scale to unprojected map
#' if map has been projected, then use scale bar function, need to specify magnification for that.
#' see Tanimura et al: auxiliary cartographic functions in R: North Arrow, Scale Bar, and label with a leader arrow
#' the default ppi if not specified is 72 ppi/inch
#' if px is specified, decimal degree is used to calculate actual distance 
#' 
#' @param x A geo-object, 
#' @param y a state abbrebriation, e.g. "CA", 
#' @param relwidth margin added to maps, %
#' @param metric if metric system is used? TRUE default 
#' @param ratio default = TRUE
#' @param cols black and white interval
#' @param projected if projection is used
#' @param mag magnification default =1
#' @param inset  is for box into the plot margin
#' @param mark mark is the text line under the scale ba
#' @param madj adjustment
#' @examples
#' map("usa") 
#' mapscale(x="bottomleft", relwidth = 0.2, mark ="", projected = F, ratio=F, inset = c(0.05, -0.05)) 
#' par(xpd =TRUE)
#'  plot(0:1,0:1)
#'  map("state", "ohio")
#'  mapscale(x="topleft", relwidth = 0.2,ratio=T) 
#'  mapscale(x="topright", relwidth = 0.2,ratio=F)     
#'  mapscale(x="bottomright", relwidth = 0.2,ratio=F) 
#'  mapscale(x="bottomleft", relwidth = 0.2, projected =T, ratio=F)      
#'  mapscale(x="center", relwidth = 0.2, projected =T, ratio=F)   
#'  box()
#'   plot(us.states)
#'  mapscale(x="bottomright", relwidth = 0.2,ratio=F, projected =T, inset = c(0.1, 0.2)) 
#'  mapscale(x="topright", relwidth = 0.2, ratio=F, projected =T, epsg = 2163, inset = c(0.1, 0.2))
#' @export

mapscale <- function (x, y, relwidth = 0.15, metric = TRUE, ratio = TRUE,
      cols = rep(c("black","white"),10),  cex = 0.8,projected = FALSE, epsg = 4326,
      inset = 0, mark = "Albers Equal Area Conic USGS version", madj = 0,...) {

     format.pretty <- function(x, digits = 2) {
        x = signif(x, 2)
        prettyNum(formatC(x, format = "fg", digits = digits),
            big.mark = ",")
     }
#    par(xpd = TRUE)
    usr0 <- usr <- par("usr")  # user dimension by inch usr0 is reserved current coordinate system

    if (missing(y))
        y <- (9 * usr[3] + usr[4])/10      # upper north of equator  usr[3]  # 
    if (abs(y) >= 90 & !projected)
        warning("location of scale out of this world!")
    if (missing(x))
        x <- (9 * usr[1] + usr[2])/10      # almost more left usr[1]  # 
    auto <- if (is.character(x)) {
       match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))
    } else auto <- NA
    cosy <- cos((2 * pi * y)/360)         # per degree arc  for latitude
##### The circumference at the given latitude is interpolated from a
######  radius of 6356.78 km at the pole and 6378.16 km at the equator.
##### 21.38 is the difference between pole and equator
    perdeg <- (2 * pi * (6356.78 + 21.38 * cosy) * cosy)/360   # per degree distance  for longitude
    # pin is current plot dimension in inches
    # diff(par("usr"))[-2] finds the width and height of current plot dimension in current unit
    # *2.54 to convert to cm
    # scale = (1km * perdeg)/current distance (cm)
    scale <- (perdeg * 1e+05)/(2.54 * (par("pin")/diff(usr)[-2])[1])
    if (metric)      {
        unit <- "km"
     }  else {
        perdeg <- perdeg * 0.6213712
        unit <- "mi"
     }
    ###### scale dimension and labels
    w = relwidth * (usr[2L] - usr[1L])    # scale width
    len <- perdeg * w      # scale length actual labeled distance
    ats <- pretty(c(0, len), n = 4)     # divide bars
    nats <- length(ats)                 # numer of bars 
    cxy <- par("cxy")     # size of charactor(width, height) in user coordinate units
#    cxy <- tolatlon(cxy[1]+usr0[1],cxy[2]+usr0[3], epsg)-tolatlon(usr0[1], usr0[3], epsg)

    dy <- abs(cxy[2] * par("tcl")* 0.25)          # height of tick mark
    dx <- ats[nats]/perdeg/(nats - 1)   # length/width of each scale bar
    dy2 <- cxy[2] * 0.25  # height of bar
    xwidth <- strwidth(" km", units = "user")   # unit text width
    xheight <- strheight(" km", units = "user") # text height
    h = cxy[2]*.33 + dy + dy2 + xheight        
    labs <- as.character(ats)
    xtra <-strwidth(labs[nats], units= "user")/2   # last label exceeding scale bar
    ####### scale location 
    if(!is.na(auto)) {
            if(length(inset)==1) inset <- rep(inset, length.out = 2)
            insetx <- inset[1L] * (usr[2L] - usr[1L])
            left <- switch(auto, bottomright = , topright = , 
                right = usr[2L] - w - insetx, bottomleft = , 
                left = , topleft = usr[1L] + insetx, bottom = , 
                top = , center = (usr[1L] + usr[2L] - w)/2)
            insety <- inset[2L] * (usr[4L] - usr[3L])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                insety, topleft = , top = , topright = usr[4L] - 
                insety, left = , right = , center = (usr[3L] + 
                usr[4L] + h)/2)
        } else {
        top <- y
        left <- x
        }
    ###### label location
    lab.y <- top + dy + dy2 + 0.4 * cxy[2]
    lab.x <- left + c(ats/perdeg, ats[nats]/perdeg + xwidth/2 + xtra )
    mark.y <- top - dy2 - 0.4 * cxy[2]
    mark.x <- mean(lab.x) + madj *(usr[2L] - usr[1L]) 

    ###### label location
    is.odd <- function(x) x %% 2 != 0
    if(is.odd(nats) & nats > 4) {
       labs[!is.odd(1:nats)] <- ""
      }  else if(nats >4) {
      labs[(nats/2 + 2):(nats-1)] <-""
      }
      labs <- append(labs, unit)
    ###### plot bar and label if not projected
    if(!projected) {
      text(lab.x, lab.y, labs,  adj = c(0.5,0.4), cex = cex, ...)
      if(is.character(mark)) text(mark.x, mark.y, mark, cex = cex, ... )
      for (i in 1:(nats-1)) {
         rect(left + dx *(i-1), top, left + dx * i, top + dy2, col = cols[i])
         segments(left + dx *(i-1), top + dy2, left + dx * (i-1),  top + dy2 + dy)
         }
      segments(left + dx * (nats-1), top + dy2, left + dx * (nats-1), top + dy2 + dy)

      if (ratio)
        text(left, top + h +0.2 *cxy[2], paste("scale approx 1:", format.pretty(scale),
            sep = ""), cex = cex,  adj = 0, ...)
      invisible(scale)
    } else  {   # projected 
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
        usr[c(1,3)] <- tolatlon(usr[1],usr[3], epsg)  # new user coordinates (decimal) for longitude
        usr[c(2,4)] <- tolatlon(usr[2],usr[4], epsg)  # new user coordinates for latitude
        xscale <- (usr0[2]-usr0[1])/(usr[2]-usr[1])   # from original scale to decimal
        yscale <- (usr0[4]-usr0[3])/(usr[4]-usr[3])   # from original scale (usr0) to decimal (usr)

        latlon2meter <- function (lon1, lat1, lon2, lat2){  # // generally used geo measurement function
          R =  6378.137  # ; // Radius of earth in KM
          dLat = (lat2 - lat1) * pi / 180;
          dLon = (lon2 - lon1) * pi / 180;
          a = sin(dLat/2) * sin(dLat/2) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(dLon/2) * sin(dLon/2);
          c = 2 * atan2(sqrt(a), sqrt(1-a));
          d = R * c;
          return (d)  #  kilometers
        }     
        
        fin <- par("fin")
        dist <- latlon2meter(usr[1], usr[3], usr[2], usr[4])  # total km in decimal user space across
        mag = dist/(sqrt(fin[1]^2+ fin[2]^2)*2.54/100000) # total corner dimension (km)/total corner dimension cm
        ### rescale
 #       lab.tmp <- tolatlon(lab.x, lab.y, epsg)
#        lab.x <- lab.tmp[1] ; lab.y <- lab.tmp[2]
#        mark.tmp <- tolatlon(mark.x, mark.y, epsg)
#        mark.x <- mark.tmp[1]; mark.y <- mark.tmp[2]
 #       leftop <- tolatlon(left, top, epsg)
#        text(lab.x, lab.y, labs,  adj = c(0.5,0.4), cex = cex, ...)
 #       left <- leftop[1]; top <- leftop[2]
#        dx <- dx * xscale; dy <- dy * yscale; dy2 <- dy2 * yscale
#        if(is.character(mark)) text(mark.x, mark.y, mark, cex = cex, ... )
 #       for (i in 1:(nats-1)) {
#          rect(left + dx *(i-1), top, left + dx * i, top + dy2, col = cols[i])
#          segments(left + dx *(i-1), top + dy2, left + dx * (i-1),  top + dy2 + dy)
 #       }
#        segments(left + dx * (nats-1), top + dy2, left + dx * (nats-1), top + dy2 + dy)

        distance <- fin[1] * relwidth * 2.54 * mag /100000  # bar distance     from cm to km

        x.bar <- pretty(c(0, distance/c(4,2,4/3,1)))
        x <- c(0,(max(x.bar)/distance)*w/c(4,2,4/3,1), (max(x.bar)/distance)*w* 1.05) + left
        y <- c(0, (max(x.bar)/distance)*w/(10*3:1)) + top
        for (i in 1:4) rect(x[i],y[1],x[i+1],y[2],col=cols[i])
        for (i in 1:5) segments(x[i],y[2],x[i],y[3])
        labels <- x.bar [1:4]        
        labels <- append(labels,paste(x.bar[5],unit))
        text(x[c(1,3,5)],y[4],labels=labels[c(1,3,5)],adj= 0.5,cex=cex)
       }
   }


