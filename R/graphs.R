#' Colorado State Demography Office ggplot2 Theme
#' 
#' Custom \code{ggplot2} theme that borrows heavily from the 
#'\code{theme_fivethirtyeight()} in ggthemes.
#'
#' @param base_size Base font size.
#' @param base_family Plot text font family.


theme_codemog <- function(base_size = 12, base_family = "sans"){
  codemog_pal=c(
    dkblu=rgb(31,73,125, max=255),
    dkred=rgb(192,80,77, max=255),
    dkgray = rgb(78, 87, 88, max = 255),
    medgray = rgb(210, 210, 210, max = 255),
    ltgray = rgb(208, 210, 211, max = 255),
    green = rgb(119, 171, 67, max = 255)
  )
    theme(
     line = element_line(),
     rect = element_blank(),
     text = element_text(colour = codemog_pal['dkgray']),
     axis.title = element_text(family=base_family, colour=codemog_pal['dkgray']),
     axis.text = element_text(colour=codemog_pal['dkgray'], family=base_family),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     panel.grid = element_line(colour = NULL),
     panel.grid.major = element_line(colour = codemog_pal['medgray']),
     panel.grid.minor = element_blank(),
     plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.background=element_rect())
}

#' Colorado State Demography Office Color Palette for ggplot2
#'
#'Custom color palette using a mix of SDO colors and DOLA
#' Brand Colors from Brand Colorado. 
#'
#'
codemog_pal=c(
  dkblu=rgb(31,73,125, max=255),
  dkred=rgb(192,80,77, max=255),
  dkgray = rgb(78, 87, 88, max = 255),
  medgray = rgb(210, 210, 210, max = 255),
  ltgray = rgb(208, 210, 211, max = 255),
  green = rgb(119, 171, 67, max = 255)
  )
#' Creates a \code{ggplot2} chart of the population for a CO county
#'
#' Takes some basic input on the timeperiod and county then creates a 
#' plot of the data in \code{ggplot2}.  Similar to the county_ts_data()
#' function.  Can create timeseries from 1990 to 2040 (beyond 2013 are
#' forecasts).
#' Note: Requires dplyr, ggplot2, ggthemes, scales, and grid R packages.
#'
#' @param fips The County FIPS number (without leading Zeros)
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endyear The first year in the timeseries Defaults to 2013. 


county_ts_chart=function(fips, beginyear=1990, endyear=2013){
  require(dplyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  fips=as.numeric(fips)
  
  d=county_forecast%>%
    filter(countyfips==fips, year<=endyear, year>=beginyear)%>%
    group_by(county,countyfips, year)%>%
    summarise(totalPopulation=sum(totalPopulation))%>%
    mutate(type=ifelse(year>=2014, "Forecast", "Estimate"))
  p=d%>%
    ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    geom_line(color=codemog_pal['dkblu'], size=1.75)+
    labs(x="Year", y="Population", title=paste(d$county,"County Population,", beginyear, "to", endyear, sep=" "))+
    scale_y_continuous(label=comma)+
    theme_codemog()+
    theme(axis.text.x=element_text(angle=90))
  return(p)
}

