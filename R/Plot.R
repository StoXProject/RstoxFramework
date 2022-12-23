

#' Plot ReportBootstrapData
#' 
#' @inheritParams ModelData
#' @inheritParams RstoxBase::general_plot_arguments
#' @inheritParams RstoxBase::general_map_plot_arguments
#' @param PlotType The type of plot to produce. Currently only "ErrorBarPlot" is implemented.
#' @param PlottingVariable The variable to plot the points for. Normally this is the variable giving the mean.
#' @param PlottingVariableLower The variable to use for the lower end of the error bars.
#' @param PlottingVariableUpper The variable to use for the upper end of the error bars. 
#' @param AddCVToPlot Logical: If TRUE add a dotted line with points for the coefficient of variation (standard deviation divided by mean) CV. Setting this to TRUE requires to specify the \code{CVVariable}.
#' @param CVVariable The name of the variable holding the CV. Must be given if \code{AddCVToPlot} is TRUE.
#' @param GroupingVariables A vector of length 1 or 2 giving the names of the variables to plot along the x axis, where the second is coded with colors in the plot.
#' @param SubPlots A vector of the names of the plots to produce. To get a list of possible plot names use getSubPlotNames_PlotReportBootstrap(ReportBootstrapData, GroupingVariables).
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
    SubPlots = character(), 
    
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
    plotArguments <- RstoxBase::allargs()
    
    # Make a copy as we do modification by reference:
    plotArguments$ReportBootstrapData <- data.table::copy(plotArguments$ReportBootstrapData)
    
    plotArguments <- RstoxBase::setDefaultsInStoxFunction(plotArguments, StoxFunctionName = "PlotReportBootstrap", stoxFunctionAttributes = stoxFunctionAttributes)
    
    if(length(plotArguments$PlottingVariable) != 1) {
        stop("StoX: PlottingVariable must have length 1.")
    }
    if(length(plotArguments$PlottingVariableLower) != 1) {
        stop("StoX: PlottingVariableLower must have length 1.")
    }
    if(length(plotArguments$PlottingVariableUpper) != 1) {
        stop("StoX: PlottingVariableUpper must have length 1.")
    }
    # Giving the same variable as PlottingVariable and PlottingVariableLower or PlottingVariableUpper is not valid:
    if(plotArguments$PlottingVariable == plotArguments$PlottingVariableLower) {
        stop("StoX: PlottingVariable and PlottingVariableLower cannot be equal.")
    }
    if(plotArguments$PlottingVariable == plotArguments$PlottingVariableUpper) {
        stop("StoX: PlottingVariable and PlottingVariableUpper cannot be equal.")
    }
    if(plotArguments$PlottingVariableLower == plotArguments$PlottingVariableUpper) {
        stop("StoX: PlottingVariableLower and PlottingVariableUpper cannot be equal.")
    }
    
    
    
    # Control the GroupingVariables specified in this function (maximum 2 allowed in the plot):
    if(length(GroupingVariables) > 2) {
        stop("Only up to 2 variables can be specified in GroupingVariables The ReportBootstrapData may contain several grouping variables, but only 1 or 2 of these can be included in the plot, with one plot produced for each combination of the remaining grouping variables.")
    }
    
    # Get the remaining variables (those that are not used in the plot):
    by <- getRemainingGroupingVariables_PlotReportBootstrap(ReportBootstrapData, GroupingVariables)
    if(length(by)) {
        # Get the sub plot names:
        subPlotNames <- getSubPlotNames_PlotReportBootstrap(plotArguments$ReportBootstrapData, plotArguments$GroupingVariables)
        # Split into subgroups for convenience:
        plotArguments$ReportBootstrapData <- split(plotArguments$ReportBootstrapData, by = by)
        # Get the requested sub plot:
        subPlotsInd <- if(is.character(SubPlots)) match(SubPlots, subPlotNames) else if(is.numeric(SubPlots)) SubPlots else stop("SubPlots must be numeric or character.")
        subPlotsInd <- stats::na.omit(subPlotsInd)
        if(!length(subPlotsInd)) {
            warning("StoX: No plots selected using SubPlot. If GroupingVariables was changed, SubPlot must be changed accordinglly. Choose between the following plots:\n", paste(subPlotNames, collapse = ", "))
        }
        # Run and name the plots:
        plots <- lapply(subPlotsInd, function(ind) PlotReportBootstrapOne(plotArguments, ind = ind))
        names(plots) <- subPlotNames[subPlotsInd]
    }
    else {
        plots <- PlotReportBootstrapOne(plotArguments)
    }
    
    
    return(plots)
}


getRemainingGroupingVariables_PlotReportBootstrap <- function(ReportBootstrapData, GroupingVariables) {
    # Get the original GroupingVariables and InformationVariables used in the report:
    originalGroupingVariables <- attr(ReportBootstrapData, "GroupingVariables")
    originalInformationVariables <- attr(ReportBootstrapData, "InformationVariables")
    # Get the variables that are not to be used as GroupingVariables in the plot:
    setdiff(c(originalGroupingVariables, originalInformationVariables), GroupingVariables)
}



#' List PlotReportBootstrap plots
#' 
#' @inheritParams ModelData
#' @inheritParams PlotReportBootstrap
#' 
#' @export
#' 
getSubPlotNames_PlotReportBootstrap <- function(ReportBootstrapData, GroupingVariables, SubPlots = NULL) {
    # Get the variables that are not to be used as GroupingVariables in the plot:
    by <- getRemainingGroupingVariables_PlotReportBootstrap(ReportBootstrapData, GroupingVariables)
    if(!length(by)) {
        return("ReportBootstrapData")
    }
    
    # Get unique groupings:
    uniqueSubGroups <- unique(subset(ReportBootstrapData, select = by))
    subPlotNames <- apply(do.call(cbind, mapply(paste, by, uniqueSubGroups, sep = "-", SIMPLIFY = F)), 1, paste, collapse = "_")
    subPlotNames <- setdiff(subPlotNames, SubPlots)
    return(subPlotNames)
}



PlotReportBootstrapOne <- function(plotArguments, ind = NULL) {
    
    if(length(ind)) {
        plotArguments$ReportBootstrapData <- plotArguments$ReportBootstrapData[[ind]]
    }
    
    # Set grouping variables to factor with NA last:
    for(var in plotArguments$GroupingVariables) {
        plotArguments$ReportBootstrapData[, eval(var) := RstoxBase::factorNAfirst(get(var))]
    }
    
    
    xlab <-plotArguments$GroupingVariables[1]
    ylab <- plotArguments$PlottingVariable
    if("Unit" %in% names(plotArguments$ReportBootstrapData)) {
        ylab <- paste0(ylab, " (", plotArguments$ReportBootstrapData$Unit[1], ")")
    }
    # Add info about the lower and upper value:
    #ylab <- paste0(ylab, " (Lower: ", plotArguments$PlottingVariableLower, ", Upper: ", plotArguments$PlottingVariableUpper, ")")
    ylab <- paste0(ylab, " (", plotArguments$PlottingVariableLower, ", ", plotArguments$PlottingVariableUpper, ")")
    
    approximateMaxWidthXlab <- 50 * plotArguments$Width / plotArguments$AxisTitleSize
    approximateMaxWidthYlab <- 50 * plotArguments$Height / plotArguments$AxisTitleSize
    xlab <- paste(stringi::stri_wrap(xlab, width = approximateMaxWidthXlab), collapse = "\n")
    ylab <- paste(stringi::stri_wrap(ylab, width = approximateMaxWidthYlab), collapse = "\n")
    
    approximateMaxWidthXtick <- approximateMaxWidthXlab / length(levels(plotArguments$GroupingVariables[1]))
    
    #if(!length(plotArguments$Title)) {
    #    plotArguments$Title <- paste0(plotArguments$PlotType, " of ", plotArguments$PlottingVariable, "(min/max: ", paste(plotArguments$PlottingVariableLower, plotArguments$PlottingVariableUpper, sep = "/"), ")", " by ", paste(plotArguments$GroupingVariables, collapse = " and "), if(plotArguments$AddCVToPlot) paste(" (CV in dashed line)"))
    #}
    
    # Strip GroupingVariables and PlottingVariable and all columns of the ReportBootstrapData[[ind]] of %:
    plotArguments$GroupingVariables <- gsub("%", "percent", plotArguments$GroupingVariables)
    plotArguments$PlottingVariable <- gsub("%", "percent", plotArguments$PlottingVariable)
    plotArguments$PlottingVariableLower <- gsub("%", "percent", plotArguments$PlottingVariableLower)
    plotArguments$PlottingVariableUpper <- gsub("%", "percent", plotArguments$PlottingVariableUpper)
    data.table::setnames(plotArguments$ReportBootstrapData, names(plotArguments$ReportBootstrapData), gsub("%", "percent", names(plotArguments$ReportBootstrapData)))
    
    
    maxPlottingVariable <- plotArguments$ReportBootstrapData[, max(get(plotArguments$PlottingVariableUpper), na.rm = TRUE)]
    if(plotArguments$AddCVToPlot) {
        maxCV <- plotArguments$ReportBootstrapData[, max(get(plotArguments$CVVariable), na.rm = TRUE)]
        cvScalingFactor <- maxPlottingVariable / maxCV
        plotArguments$ReportBootstrapData[, eval(plotArguments$CVVariable) := get(plotArguments$CVVariable) * cvScalingFactor]
    }
    
    
    p <- ggplot2::ggplot(data = plotArguments$ReportBootstrapData)
    if(length(plotArguments$GroupingVariables) == 1){
        p <- p + ggplot2::geom_errorbar(
            ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable, ymin = plotArguments$PlottingVariableLower, ymax = plotArguments$PlottingVariableUpper), 
            position = ggplot2::position_dodge(0.5),
            width = 0.5
        ) + 
        #ggplot2::geom_line() +
        ggplot2::geom_point(
            ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable), 
            position = ggplot2::position_dodge(0.5)
        ) 
        
        if(plotArguments$AddCVToPlot) {
            p <- p + 
                ggplot2::geom_line(
                    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, group = 1), 
                    show.legend = FALSE, 
                    linetype = "dashed"
                ) + 
                ggplot2::geom_point(
                    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, group = 1), 
                    show.legend = FALSE, 
                    shape = 15
                )
            
            if(!is.na(cvScalingFactor)) {
                p <- p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./cvScalingFactor, name = "CV"))
            }
        } 
        
    }
    else if(length(plotArguments$GroupingVariables) == 2){
        p <- p + ggplot2::geom_errorbar(
            ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$CVVariable, ymin = plotArguments$PlottingVariableLower, ymax = plotArguments$PlottingVariableUpper, color = plotArguments$GroupingVariables[2]), 
            position = ggplot2::position_dodge(0.5),
            width = 0.5
        ) + 
        #ggplot2::geom_line(
        #    ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable, color = plotArguments$GroupingVariables[2])
        #) +
        ggplot2::geom_point(
            ggplot2::aes_string(x = plotArguments$GroupingVariables[1], y = plotArguments$PlottingVariable, color = plotArguments$GroupingVariables[2]), 
            position = ggplot2::position_dodge(0.5)
        )
        
        if(plotArguments$AddCVToPlot) {
            p <- p + 
                ggplot2::geom_line(
                    ggplot2::aes_string(
                        x = plotArguments$GroupingVariables[1], 
                        y = plotArguments$CVVariable, 
                        group = plotArguments$GroupingVariables[2], 
                        color = plotArguments$GroupingVariables[2]
                    ), 
                    show.legend = FALSE, 
                    linetype = "dashed"
                ) + 
                ggplot2::geom_point(
                    ggplot2::aes_string(
                        x = plotArguments$GroupingVariables[1], 
                        y = plotArguments$CVVariable, 
                        group = plotArguments$GroupingVariables[2], 
                        color = plotArguments$GroupingVariables[2]
                    ), 
                    show.legend = FALSE, 
                    shape = 15
                )
            
            if(!is.na(cvScalingFactor)) {
                p <- p + ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~./cvScalingFactor, name = "CV"))
            }
        }
    }
    else {
        stop("One or two GroupingVariables must be given.")
    }
    
    p <- p + ggplot2::scale_x_discrete(
        drop = FALSE, 
        labels = scales::label_wrap(approximateMaxWidthXtick)
        )
    
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
    
    # Set the plot attributes to the output:
    p <- setPlotAttributes(
        plotObject = p, 
        plotArguments = plotArguments
    )
    
    
    return(p)
}