#' Plot time series object in ggplot
#'
#' @param xtsTimeSeriesObject an xts matrix of *levels*
#' @param plotTitle character string of plot title
#' @param yTitle character string of y-axis title
#' @param yLabelType is an object from the *scales* package, one of dollar, percent, scientific, comma; defaults to dollar
#' @param xTitle title for x-axis, defaults to "Date"
#' @param levels logical, if FALSE (default) indicates that the object contents are returns, if TRUE, indicates that the object contents are levels (e.g. Prices)
#' 
#'
#' @return a timeseries plot (ggplot)
#'
#' @examples
#' none
#'
#' @export
plotTimeSeriesObject <- function(xtsTimeSeriesObject, plotTitle, yTitle, yLabelType = scales::dollar, xTitle = "Date", levels = FALSE){

  if(levels){
    xtsTimeSeriesObject <- cumprod(xtsTimeSeriesObject + 1) #convert to levels
  }
  
  plotData <- cbind( reshape2::melt(data = toDF(xtsTimeSeriesObject), id.vars = 1), Title = plotTitle)
  colnames(plotData) <- c(xTitle, "Ticker", "value","facetTitle")
  
  ggplot2::ggplot( data = plotData, aes(x = Date, y = value)) +
  geom_line(aes(colour = Ticker), size = 1) +
  scale_colour_brewer(palette = ifelse(ncol(xtsTimeSeriesObject)<=9,"Set1","Paired"), type = "qual") + 
  labs(ylab(yTitle)) + 
  theme_bw() +
  facet_grid(~facetTitle) + 
  theme(panel.grid.major = element_line(colour = "grey")
        , panel.grid.minor = element_line(colour = "grey", linetype = "dotted")
        , panel.border = element_rect(colour = "black")
        , strip.background = element_rect(fill = "dark blue", colour = "black", size = 1)
        , strip.text.x = element_text(colour = "white", size = 17, face = "bold")
        , axis.title = element_text(size = 15)
        , axis.text = element_text(size = 13)
  ) +
  scale_y_continuous(labels = yLabelType)
}