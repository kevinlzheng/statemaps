# only x is used to specify one of the 9 positions 
# inset is used to adjust the position relative to the whole plot region 

north.arrow <- function (x, relwidth = 0.15, metric = TRUE,
      cols = rep(c("black","white"),8), bearing=0,cex = .8, inset = 0, ...) {
       par(xpd = TRUE)
       auto <- if (is.character(x))
       match.arg(x, c("bottomright", "bottom", "bottomleft",
            "left", "topleft", "top", "topright", "right", "center"))

        cin <- par("cin")                     # size by inches
        usr <- par("usr")
        Cex <- cex * par("cex")
        b <- c("E","N","W","S")
        xwidth <- strwidth(b, units = "user")
        xheight <- strheight(b, units = "user")

#        xc <- Cex * xinch(cin[1L], warn.log = FALSE)
#        yc <- Cex * yinch(cin[2L], warn.log = FALSE)
        h = relwidth * (usr[4L]- usr[3L])
        w = relwidth * (usr[2L] - usr[1L])

           # checking arguments
 #       left <- usr[1]; top <- usr[4]
        
        if(!is.na(auto)) {
            if(length(inset)==1) inset <- rep(inset, length.out = 2)
            insetx <- inset[1L] * (usr[2L] - usr[1L])
            left <- switch(auto, bottomright = , topright = ,
                right = usr[2L] - w/2 - insetx + xwidth[3], bottomleft = ,
                left = , topleft = usr[1L] + insetx, bottom = ,
                top = , center = (usr[1L] + usr[2L] )/2)
            insety <- inset[2L] * (usr[4L] - usr[3L])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] +
                insety, topleft = , top = , topright = usr[4L] -
                insety, left = , right = , center = (usr[3L] +
                usr[4L])/2)
        }
        
        size <- relwidth * (usr[2L] - usr[1L])
         # calculating coordinates of polygons
        radii <- rep(size/c(1,4,2,4),4)
        xx <- radii[(0:15)+1]*cos((0:15)*pi/8+bearing) + left
        yy <- radii[(0:15)+1]*sin((0:15)*pi/8+bearing) + top
          # drawing polygons
        for (i in 1:15) {
            x1 <- c(xx[i],xx[i+1],left)
            y1 <- c(yy[i],yy[i+1],top)
            polygon(x1,y1,col=cols[i])
          }
          # drawing the last polygon
          polygon(c(xx[16],xx[1],left),c(yy[16],yy[1],top),col=cols[16])
         # drawing letters

         for (i in 0:3) text((size+ xwidth[i+1])*cos(bearing+i*pi/2)+ left,
            (size+xheight[i+1])*sin(bearing+i*pi/2)+ top,b[i+1], cex = cex)

         }
         
         
 #        map("state", "west virginia")
 #        box()
 #        north.arrow(relwidth =0.06, x="center")
 #        north.arrow(relwidth =0.06,  x="bottomright")
 #        north.arrow(relwidth =0.06,   x="topleft")
  #       north.arrow(relwidth =0.06, x="bottomleft")