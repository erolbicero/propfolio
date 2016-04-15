#' Plot time series object in ggplot
#'
#' @param xtsTimeSeriesObject an xts matrix of *returns*
#' @param plotTitle character string of plot title
#' @param yTitle character string of y-axis title; defaults to "Annualized Return"
#' @param xTitle title for x-axis, defaults to "Annualized Risk"
#' @param scale is a character string, one of "daily", "weekly", "monthly", "quarterly", "yearly"
#' 
#'
#' @return a timeseries plot (ggplot)
#'
#' @examples
#' none
#'
#' @export
plotRiskReturnScatter <- function(xtsTimeSeriesObject, plotTitle, yTitle = "Annualized Return", xTitle = "Annualized Risk", scale = "daily"){
  
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
  
  ggplot2::ggplot( data = scatterData, aes(x = Risk, y = Return)) +
    geom_point(aes(colour = Names), size = 1) +
    scale_colour_brewer(palette = ifelse(ncol(xtsTimeSeriesObject)<=9,"Set1","Paired"), type = "qual") + 
    labs(ylab(yTitle)) + 
    labs(xlab(xTitle)) + 
    theme_bw() +
    facet_grid(~facetTitle) + 
    theme(panel.grid.major = element_line(colour = "grey")
          , panel.grid.minor = element_line(colour = "grey", linetype = "dotted")
          , panel.border = element_rect(colour = "black")
          , strip.background = element_rect(fill = "dark blue", colour = "black", size = 1)
          , strip.text.x = element_text(colour = "white", size = 17, face = "bold")
          , axis.title = element_text(size = 15)
          , axis.text = element_text(size = 13)
          , legend.position = "none"
    ) +
    scale_y_continuous(labels = scales::percent) + 
    scale_x_continuous(labels = scales::percent) +
    ggrepel::geom_text_repel(label = colnames(xtsTimeSeriesObject), segment.size = 0, aes(colour = Names)) + 
    geom_hline(aes(yintercept=0)) + 
    geom_vline(aes(xintercept=0)) + 
    geom_abline(intercept = 0, slope = 3, linetype = "dashed", colour = "grey") + 
    geom_abline(intercept = 0, slope = 2, linetype = "dashed", colour = "grey") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", colour = "grey")
  
  
}