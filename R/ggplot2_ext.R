# Multiple plot function 
#
# Copied from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



#' @import ggplot2
#' @export
gg.correlation <- function( samples, xcol, idcol, 
                            geom_diag = "histogram", geom_co = "point",
                            colourcol = NULL )
{
  pars <- unique(samples[[idcol]])
  
  plots <- list()

  for (par2 in pars)
  {
    for (par1 in pars)
    {
      # Need to call the standard stat, and pass it the geom
      if (par1==par2)
      {
        fg <- paste0("geom_",geom_diag)
        gg <- ggplot2::ggplot(data=samples[(samples[[idcol]]==par1)]) +
          do.call(fg, list(ggplot2::aes_string(x=xcol,colour=colourcol)))
        plots <- c(plots,list(gg))
      } 
      else
      {
        fg <- paste0("geom_",geom_co)
        ss <- samples[samples[[idcol]]==par1|samples[[idcol]]==par2]
        cn <- colnames(ss)
        cn <- cn[cn != xcol & cn != idcol]
        fm <- formula( paste(paste(cn, collapse="+"), "~", idcol ))
        
        gg <- ggplot2::ggplot(data=dcast(samples, fm)) +
          do.call(fg, list(ggplot2::aes_string(x=par1,y=par2,colour=colourcol)))
        plots <- c(plots,list(gg))
      }
    }
  }
  
  multiplot( plotlist=plots, cols = length(pars) )
  return(plots)
}
