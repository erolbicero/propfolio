#' Plot time series object in ggplot
#'
#' @param xtsTimeSeriesObject an xts matrix of *returns*
#' @param plotTitle character string of plot title
#' @param yTitle character string of y-axis title; defaults to "Annualized Return"
#' @param xTitle title for x-axis, defaults to "Annualized Risk"
#' @param scale is a character string, one of "daily", "weekly", "monthly", "quarterly", "yearly"
#' @param interactive logical, whether or not to render in plotly for interactive use of plot; defaults to FALSE
#'
#' @return a timeseries plot (ggplot)
#'
#' @examples
#' none
#'
#' @export
plotRiskReturnScatter <- function(xtsTimeSeriesObject, plotTitle, yTitle = "Annualized Return", xTitle = "Annualized Risk", scale = "daily", interactive = FALSE){
  
  scalingFactor <- 
    switch(scale
           , daily = 252
           , weekly = 52
           , monthly = 12
           , quarterly = 4
           , yearly = 1
    )
  
  
  
  
  scatterData <-
    data.frame(
      Names = colnames(xtsTimeSeriesObject)
      , Risk = as.vector(PerformanceAnalytics::StdDev.annualized(xtsTimeSeriesObject, scale = scalingFactor))
      , Return = as.vector(PerformanceAnalytics::Return.annualized(xtsTimeSeriesObject, scale = scalingFactor))
      , facetTitle = plotTitle
      , stringsAsFactors = FALSE
    )
  
  xLimHigh <- max(scatterData$Risk)*1.5
  
  plotObject <- 
  ggplot2::ggplot( data = scatterData, ggplot2::aes(x = Risk, y = Return)) +
    ggplot2::geom_point(ggplot2::aes(colour = Names), size = 1) +
    ggplot2::scale_colour_brewer(palette = ifelse(ncol(xtsTimeSeriesObject)<=9,"Set1","Paired"), type = "qual") + 
    ggplot2::labs(ggplot2::ylab(yTitle)) + 
    ggplot2::labs(ggplot2::xlab(xTitle)) + 
    ggplot2::theme_bw() +
    ggplot2::facet_grid(~facetTitle) + 
    ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "grey")
          , panel.grid.minor = ggplot2::element_line(colour = "grey", linetype = "dotted")
          , panel.border = ggplot2::element_rect(colour = "black")
          , strip.background = ggplot2::element_rect(fill = "dark blue", colour = "black", size = 1)
          , strip.text.x = ggplot2::element_text(colour = "white", size = 17, face = "bold")
          , axis.title = ggplot2::element_text(size = 15)
          , axis.text = ggplot2::element_text(size = 13)
          , legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(labels = scales::percent) + 
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggrepel::geom_text_repel(label = colnames(xtsTimeSeriesObject), segment.size = 0, ggplot2::aes(colour = Names)) + 
    ggplot2::geom_hline(ggplot2::aes(yintercept=0)) + 
    ggplot2::geom_vline(ggplot2::aes(xintercept=0)) + 
    ggplot2::geom_abline(intercept = 0, slope = 3, linetype = "dashed", colour = "grey") + 
    ggplot2::geom_abline(intercept = 0, slope = 2, linetype = "dashed", colour = "grey") + 
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey")
  
  if(interactive){
    plotly::ggplotly(plotObject)
  }else{
    plotObject
  }
  
}