#' A function to plot US map and add colors to each state based on values
#' 
#' This function used a stored US state and county GIS data to plot color maps to each state and county;
#' the US map is projected to Albers
#' 
#' @param geoobj A geo-object, 
#' @param values assigned values for each state to create color gradient, 
#' @param type seven types to pick from c("rgb","topo","heat", "rain","cm","ter"(rain), "gray")
#' @param ltitle legend title
#' @param unit unit for legend
#' @param scheme color scheme for rgb.palette, only used for rbg.palette
#' @examples
#' colbar(geoobj=us.states, values= runif(nrow(us.states)+1, 0,100), # 1:51, #
#' type = "rgb.col", ltitle ="Density",
#' unit ="/mile", main ="Population",
#' scheme = c("purple4", "blue4","blue1","green3", "yellowgreen","yellow2","sandybrown","red","darkred") )
#' @export
#' 
#' other schemes c("gray1", "gray99"), c("seagreen1", "seagreen4"),c("yellow1", "yellow4"),
#' c("steelblue1", "steelblue3") c("pink1", "pink3")

# load(file = "C:/Users/l.zheng/Desktop/GitHub/statemaps/data/states.RData")


colbar <- function(geoobj=us.states, values= runif(nrow(us.states)+1, 0,100), # 1:51, #
                   type = "rgb", ltitle ="Density",
                   unit ="/mile", main ="Population",
                   scheme = c("purple4", "blue4","blue1","green3", "yellowgreen",
                              "yellow2","sandybrown","red","darkred") ) {
    layout(matrix(1:2,ncol=2), width = c(7,1),height = c(1,1))
    # values <- runif(nrow(us.states), 0,100)
    brks <- pretty(values, n = nrow(geoobj)+1)
    ncolor <- length(brks)
    cuts <- cut(values, breaks = brks, include.lowest =T)
    rgb.palette <- colorRampPalette(scheme, space = "rgb")  

    val.col <- switch(type, 
         rgb = rgb.palette(ncolor)[as.numeric(cuts)],
         topo = topo.colors(ncolor)[as.numeric(cuts)],
         heat = heat.colors(ncolor)[as.numeric(cuts)],
         rain = rainbow(ncolor)[as.numeric(cuts)],
         cm = cm.colors(ncolor)[as.numeric(cuts)],  
         ter = terrain.colors(ncolor)[as.numeric(cuts)], 
         gray = gray(as.numeric(cuts/ncolor)) )

    labs <- pretty(values, 6)
    # ats <- findInterval(labs, brks)-0.5
    pos <- 0.75 - (0:(length(labs)-1))/(length(labs)-1)*.5
    print(val.col)
    plot(geoobj, col = val.col, border=T, add = F)  
    mtext(main, side = 3,line=1, cex = 2)
    north.arrow(relwidth = 0.05,   x="bottomright", inset = c(0.1,0.15))
    
    par(mar = c(0,1,0,1))
    plot(c(0,1),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = "", cex.main = 1)
    text(x=.7, y = pos, labels = labs)
    par(xpd = NA)
    text(x= 0.1, y = 0.87, labels = ltitle, pos =3)
    text(x= 0.1, y = 0.8, labels = unit, pos =3)
    legend_image <- as.raster(matrix(rgb.palette(ncolor), ncol=1))
    rasterImage(legend_image, 0, 0.25,.4,.75)
    }

