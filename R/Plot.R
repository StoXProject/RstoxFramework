

#' Plot ReportBootstrapData
#' 
#' @inheritParams RstoxBase::ModelData
#' @inheritParams RstoxBase::general_plot_arguments
#' @inheritParams RstoxBase::general_map_plot_arguments
#' 
#' @return
#' A \code{\link{PlotReportBootstrapData}} object.
#' 
#' @export
#' 
PlotReportBootstrap <- function(
        
    ReportBootstrapData, 
    
    PlotType = c("ErrorBarPlot"), 
    
    PlottingVariable = character(), 
    PlottingVariableLower = character(), 
    PlottingVariableUpper = character(), 
    
    AddCVToPlot = FALSE, 
    CVVariable = character(), 
    
    GroupingVariables = character(), 
    
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
    
    # Get the formals:
    plotArguments <- allargs()
    
    
    # Remove all rows with missing 
    plotArguments$ReportBootstrapData <- data.table::copy(plotArguments$ReportBootstrapData)
    plotArguments$ReportBootstrapData <- subset(plotArguments$ReportBootstrapData, !is.na(get(PlottingVariable)))
    if(AddCVToPlot) {
        if(!length(CVVariable)) {
            stop("CVVariable must be given when AddCVToPlot = TRUE.")
        }
        plotArguments$ReportBootstrapData <- subset(plotArguments$ReportBootstrapData, !is.na(get(CVVariable)))
    }
    if(length(GroupingVariables) > 2) {
        stop("Only up to 2 variables can be specified in GroupingVariables The ReportBootstrapData may contain several grouping variables, but only 1 or 2 of these can be included in the plot, with one plot produced for each combination of the remaining grouping variables.")
    }
    for(var in plotArguments$GroupingVariables) {
        plotArguments$ReportBootstrapData[, eval(var) := RstoxBase::factorNAfirst(get(var))]
    }
    
    
    xlab <- paste(plotArguments$GroupingVariables, collapse = " by ")
    ylab <- paste0(plotArguments$PlottingVariable, " (", plotArguments$ReportBootstrapData$Unit[1], ")")
    
    if(!length(plotArguments$Title)) {
        plotArguments$Title <- paste0(PlotType, " of ", plotArguments$PlottingVariable, "(min/max: ", paste(PlottingVariableLower, PlottingVariableUpper, sep = "/"), ")", " by ", paste(plotArguments$GroupingVariables, collapse = " and "), if(AddCVToPlot) paste(" (CV in dashed line("))
    }
    
    # Strip PlottingVariable, PlottingVariable and all columns of the ReportBootstrapData of %:
    plotArguments$GroupingVariables <- gsub("%", "percent", plotArguments$GroupingVariables)
    plotArguments$PlottingVariableLower <- gsub("%", "percent", plotArguments$PlottingVariableLower)
    plotArguments$PlottingVariableUpper <- gsub("%", "percent", plotArguments$PlottingVariableUpper)
    data.table::setnames(plotArguments$ReportBootstrapData, names(plotArguments$ReportBootstrapData), gsub("%", "percent", names(plotArguments$ReportBootstrapData)))
    
    
    maxPlottingVariable <- plotArguments$ReportBootstrapData[, max(get(plotArguments$PlottingVariableUpper), na.rm = TRUE)]
    if(AddCVToPlot) {
        maxCV <- plotArguments$ReportBootstrapData[, max(get(CVVariable), na.rm = TRUE)]
        cvScalingFactor <- maxPlottingVariable / maxCV
        plotArguments$ReportBootstrapData[, eval(CVVariable) := get(CVVariable) * cvScalingFactor]
    }
    
    browser(
        
    )
    
    
    
    p <- ggplot2::ggplot(data = plotArguments$ReportBootstrapData)
    if(length(plotArguments$GroupingVariables) == 1){
        p <- p + ggplot2::geom_errorbar(
            ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable, ymin = plotArguments$PlottingVariableLower, ymax = plotArguments$PlottingVariableUpper), 
            width = .1
        ) + 
            #ggplot2::geom_line() +
            ggplot2::geom_point(
                ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable)
            ) 
        
        if(AddCVToPlot) {
            p <- p + 
                ggplot2::geom_line(
                    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, group = 1), 
                    show.legend = FALSE, 
                    linetype = "dashed"
                ) + 
                ggplot2::geom_point(
                    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, group = 1), 
                    show.legend = FALSE
                )
            
            p <- p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./cvScalingFactor, name = "CV"))
            
    
        } 
            
    }
    else if(length(plotArguments$GroupingVariables) == 2){
        p <- p + ggplot2::geom_errorbar(
            ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, ymin = plotArguments$PlottingVariableLower, ymax = plotArguments$PlottingVariableUpper, color = plotArguments$GroupingVariables[2]), 
            width = .1
        ) + 
            #ggplot2::geom_line(
            #    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable, color = plotArguments$GroupingVariables[2])
            #) +
            ggplot2::geom_point(
                ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable, color = plotArguments$GroupingVariables[2])
            )
        
        if(AddCVToPlot) {
            p <- p + 
                ggplot2::geom_line(
                    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, group = 1, color = plotArguments$GroupingVariables[2]), 
                    show.legend = FALSE, 
                    linetype = "dashed"
                ) + 
                ggplot2::geom_point(
                    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, group = 1, color = plotArguments$GroupingVariables[2]), 
                    show.legend = FALSE
                )
            
            p <- p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./cvScalingFactor, name = "CV"))
        }
    }
    else {
        stop("One or two GroupingVariables must be given.")
    }
    
    
    
    p <- p + 
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) + 
        ggplot2::ggtitle(plotArguments$Title)
    
    
    
    p <- p + ggplot2::theme(
        axis.title.x = ggplot2::element_text(size = if(length(plotArguments$AxisTitleSize)) plotArguments$AxisTitleSize else 10), 
        axis.title.y = ggplot2::element_text(size = if(length(plotArguments$AxisTitleSize)) plotArguments$AxisTitleSize else 10), 
        axis.text.x = ggplot2::element_text(size = if(length(plotArguments$AxisTickSize)) plotArguments$AxisTickSize else 10), 
        axis.text.y = ggplot2::element_text(size = if(length(plotArguments$AxisTickSize)) plotArguments$AxisTickSize else 10), 
        legend.text = ggplot2::element_text(size = if(length(plotArguments$LegendTextSize)) plotArguments$LegendTextSize else 10), 
        legend.title = ggplot2::element_text(size = if(length(plotArguments$LegendTitleSize)) plotArguments$LegendTitleSize else 10)
    )
    
    return(p)
}