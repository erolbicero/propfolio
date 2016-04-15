#' Plot time series object in ggplot
#'
#' @param xtsTimeSeriesObject an xts matrix of *levels*
#' @param plotTitle character string of plot title
#' @param yTitle character string of y-axis title
#' @param yLabelType is a string, one of dollar, percent, scientific, comma; defaults to dollar
#' @param xTitle title for x-axis, defaults to "Date"
#' @param levels logical, if FALSE (default) indicates that the object contents are returns, if TRUE, indicates that the object contents are levels (e.g. Prices)
#' @param interactive logical, whether or not to render in plotly for interactive use of plot; defaults to FALSE
#'
#' @return a timeseries plot (ggplot)
#'
#' @examples
#' none
#'
#' @export
plotTimeSeriesObject <- function(xtsTimeSeriesObject, plotTitle, yTitle, yLabelType = "dollar", xTitle = "Date", levels = FALSE, interactive = FALSE){

  if(levels){
    xtsTimeSeriesObject <- cumprod(xtsTimeSeriesObject + 1) #convert to levels
  }
  
  yLabelTypeValue <- switch (yLabelType
                        , "dollar" = scales::dollar
                        , "percent" = scales::percent
                        , "scientific" = scales::scientific
                        , "comma" = scales::comma
    
  )
  
  plotData <- cbind( reshape2::melt(data = toDF(xtsTimeSeriesObject), id.vars = 1), Title = plotTitle)
  colnames(plotData) <- c(xTitle, "Ticker", "value","facetTitle")
  
  plotObject <-
  ggplot2::ggplot( data = plotData, ggplot2::aes(x = Date, y = value)) +
  ggplot2::geom_line(ggplot2::aes(colour = Ticker), size = 1) +
  ggplot2::scale_colour_brewer(palette = ifelse(ncol(xtsTimeSeriesObject)<=9,"Set1","Paired"), type = "qual") + 
  ggplot2::labs(ggplot2::ylab(yTitle)) + 
  ggplot2::theme_bw() +
  ggplot2::facet_grid(~facetTitle) + 
  ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey")
        , panel.grid.minor = ggplot2::element_line(colour = "grey", linetype = "dotted")
        , panel.border = ggplot2::element_rect(colour = "black")
        , strip.background = ggplot2::element_rect(fill = "dark blue", colour = "black", size = 1)
        , strip.text.x = ggplot2::element_text(colour = "white", size = 17, face = "bold")
        , axis.title = ggplot2::element_text(size = 15)
        , axis.text = ggplot2::element_text(size = 13)
  ) +
  ggplot2::scale_y_continuous(labels = yLabelTypeValue)
  
  if(interactive){
      plotly::ggplotly(plotObject)
  }else{
    plotObject
  }
  
}