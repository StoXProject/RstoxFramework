

PlotReportBootstrap <- function(
        
    ReportBootstrapData, 
    PlotVariables = character(), 
    
    
    # Options for the colors:
    UseDefaultColorSettings = TRUE, 
    PointColor = character(), 
    
    # Options for the point sizes and shapes:
    UseDefaultSizeSettings = TRUE, 
    PointSize = numeric(), 
    
    
    # Options for the labels and other text:
    UseDefaultTextSettings = TRUE, 
    Title = character(), 
    AxisTitleSize = numeric(), 
    AxisTickSize = numeric(), 
    LegendTitleSize = numeric(), 
    LegendTextSize = numeric(), 
    
    # Options for the output file:
    #Format = c("png", "tiff", "jpeg", "pdf"), 
    UseDefaultFileSettings = TRUE, 
    Format = character(), 
    Width = numeric(), 
    Height = numeric(), 
    DotsPerInch = numeric()	
) {
    
    ## Remove rows with either missing 
    #
    #if(is.empty(grp2)){
    #    pl <- ggplot() + 
    #        geom_boxplot(data=abundanceSum, aes_string(x=thisgrp1, y="Ab.Sum"), outlier.shape=18) + 
    #        theme_bw() + 
    #        scale_x_discrete(drop=FALSE) + 
    #        geom_line(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=1), data=outtmp, show.legend=FALSE) + 
    #        geom_point(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=1), data=outtmp, show.legend=FALSE)
    #}	
    #else{
    #    pl <- ggplot() + 
    #        geom_boxplot(data=abundanceSum, aes_string(x=thisgrp1, y="Ab.Sum", fill=grp2), outlier.shape=18) + 
    #        theme_bw() + 
    #        scale_x_discrete(drop=FALSE) + 
    #        scale_fill_discrete(name=grp2) + 
    #        geom_line(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=grp2, colour=out[[grp2]]), data=outtmp, show.legend=FALSE) + 
    #        geom_point(aes_string(x=thisgrp1, y="Ab.Sum.cv", group=grp2, colour=out[[grp2]]), data=outtmp, show.legend=FALSE)
    #}
    #pl <- pl + 
    #    scale_y_continuous(trans=if("y" %in% log) "log10" else "identity", sec.axis=sec_axis(~./cvScalingFactor, name="CV")) + 
    #    coord_cartesian(ylim=ylim) + 
    #    xlab(xlab) +
    #    ylab(ylab) + 
    #    ggtitle(main)
    #
    #
    ## Adjust text size and other theme variables:
    #pl + theme(axis.text=element_text(size=1.5), axis.title=element_text(size=2,face="bold"), , legend.text=element_text(size=2))
    
    
    
    
}