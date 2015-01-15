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
#' @param fips The County FIPS number
#' @param beginyear The first year in the timeseries Defaults to 1990.
#' @param endyear The first year in the timeseries Defaults to 2013. 



county_ts_chart=function(fips, beginyear=1990, endyear=2013){
  require(dplyr, quietly=TRUE)
  fips=as.numeric(fips)
  
  d=county_forecast%>%
    filter(countyfips==fips, year<=endyear, year>=beginyear)%>%
    group_by(county,countyfips, year)%>%
    summarise(totalPopulation=sum(totalPopulation))%>%
    mutate(type=ifelse(year>=2014, "Forecast", "Estimate"))
  p=d%>%
    ggplot2::ggplot(aes(x=as.factor(year), y=as.integer(totalPopulation), group=countyfips))+
    ggplot2::geom_line(color=codemog_pal['dkblu'], size=1.75)+
    ggplot2::labs(x="Year", y="Population", title=paste(d$county,"County Population,", beginyear, "to", endyear, sep=" "))+
    scales::scale_y_continuous(label=comma)+
    theme_codemog()+
    ggplot2::theme(axis.text.x=element_text(angle=90))
  return(p)
}

#' Creates a \code{ggplot2} chart of the educational attainment comparison 
#'
#' Takes two places (using fips numbers) and then  creates a plot of the data in \code{ggplot2}.  
#' Note: Requires dplyr, ggplot2, ggthemes, scales, grid,
#' reshape2,tidur, stringi R packages.
#'
#' @param fips A Place (or County) FIPS number 
#' @param state The initial State using a State FIPS (08=CO) Defaults to 08.
#' @param fips2 A Place (or County) FIPS number for comparison Defaults to ""
#' @param state2 The comparison place's State using a State FIPS (08=CO) Defaults to 08.


ms_ed=function(fips, state="08", fips2="", state2="08"){
  require(dplyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  require(reshape2, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(stringi, quietly=TRUE)
  
  
  d13p=codemog_api(data="b15003",db="acs0913",geonum=paste("1",state , fips,sep=""),meta="no")
  d13p[,7:32]=as.numeric(as.character(d13p[,7:32]))
  d13pm=d13p%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012,
           ed2=b15003013+b15003014+b15003015+b15003016,
           ed3=b15003017+b15003018,
           ed4=b15003019+b15003020,
           ed5=b15003021,
           ed6=b15003022,
           ed7=b15003023+b15003024+b15003025)%>%
    select(geoname:geonum,ed1:ed7)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(agecat=ordered(as.factor(variable), levels=c("ed1", "ed2", "ed3", "ed4", 
                                                        "ed5", "ed6", "ed7"), 
                          labels=c("Less than 9th grade", "9th to 12th grade",
                                   "High School Graduate \n(or GED)","Some College, \nno degree", "Associate's Degree", "Bachelor's Degree", 
                                   "Graduate or \nProfessional Degree")))%>%
    separate(geoname, into=c("geoname","statename"),sep=",")%>%
    select(-statename)%>%
    mutate(geoname=stri_trans_general(geoname,id="Title"))
  
  d13c=codemog_api(data="b15003",db="acs0913",geonum=paste("1",state2 , fips2,sep=""),meta="no")
  d13c[,7:32]=as.numeric(as.character(d13c[,7:32]))
  d13cm=d13c%>%
    mutate(ed1=b15003002+b15003003+b15003004+b15003005+b15003006+b15003007+b15003008+b15003009+b15003010+b15003011+
             b15003012,
           ed2=b15003013+b15003014+b15003015+b15003016,
           ed3=b15003017+b15003018,
           ed4=b15003019+b15003020,
           ed5=b15003021,
           ed6=b15003022,
           ed7=b15003023+b15003024+b15003025)%>%
    select(geoname:geonum,ed1:ed7)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(agecat=ordered(as.factor(variable), levels=c("ed1", "ed2", "ed3", "ed4", 
                                                        "ed5", "ed6", "ed7"), 
                          labels=c("Less than 9th grade", "9th to 12th grade",
                                   "High School Graduate \n(or GED)","Some College, \nno degree", "Associate's Degree", "Bachelor's Degree", 
                                   "Graduate or \nProfessional Degree")))%>%
    mutate(geoname=stri_replace_all_charclass(geoname, "\\p{WHITE_SPACE}", ""))
  d=rbind(d13cm,d13pm)%>%
    group_by(geoname)%>%
    mutate(p=value/sum(value))
  p=ggplot(d, aes(x=agecat, y=p, fill=geoname))+
    geom_bar(stat="identity", position="dodge")+#, fill=rgb(31,74,126, max=255))+ 
    scale_y_continuous(label=percent)+
    scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                      name="Geography")+
    theme_codemog()+
    theme(axis.text.x=element_text(angle=0))+
    labs(x="Age", y="Population", title="Educational Attainment \nSource: ACS 2013 5-Year File")
  return(p)
  
  
}

#' Creates a \code{ggplot2} chart of the Census Age Distribution in 2000 and 2010.
#'
#' Takes a place and then  creates a plot of the 2000 and 2010  data in \code{ggplot2}.  
#' Note: Requires dplyr, ggplot2, ggthemes, scales, grid,
#' reshape2,tidur, stringi R packages.
#'
#' @param fips A Place (or County) FIPS number 
#' @param state The initial State using a State FIPS (08=CO) Defaults to 08.
#' 

ms_census_age=function(fips, state="08"){
  require(dplyr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  require(reshape2, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(stringr, quietly=TRUE)
  
  d10=codemog_api(data="p12",db="c2010",geonum=paste("1",state , fips,sep=""),meta="no")
  d00=codemog_api(data="p12",db="c2000",geonum=paste("1",state , fips,sep=""), meta="no")
  d10[,7:56]=as.numeric(as.character(d10[,7:56]))
  d00[,7:56]=as.numeric(as.character(d00[,7:56]))
  d10c=d10%>%
    mutate(age1=p12003+p12027,
           age2=p12004+p12028,
           age3=p12005+p12029,
           age4=p12006+p12007+p12030+p12031,
           age5=p12008+p12009+p12010+p12032+p12033+p12034,
           age6=p12011+p12012+p12035+p12036,
           age7=p12013+p12014+p12037+p12038,
           age8=p12015+p12016+p12039+p12040,
           age9=p12017+p12018+p12019+p12041+p12042+p12043,
           age10=p12020+p12021+p12022+p12023+p12024+p12025+
             p12044+p12045+p12046+p12047+p12048+p12049)%>%
    select(geoname:geonum,age1:age10)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(
      agecat=ordered(as.factor(variable), levels=c("age1", "age2", "age3", "age4", 
                                                   "age5", "age6", "age7", "age8",
                                                   "age9", "age10"), 
                     labels=c("Less than 5", "5 to 9",
                              "10 to 14", "15 to 19", "20 to 24", "25 to 34","35 to 44", 
                              "45 to 54", "55 to 64", "65 and Over")),
      year="2010",
      geoname=stri_trans_general(geoname, id="Title"))
  d00c=d00%>%
    mutate(age1=p12003+p12027,
           age2=p12004+p12028,
           age3=p12005+p12029,
           age4=p12006+p12007+p12030+p12031,
           age5=p12008+p12009+p12010+p12032+p12033+p12034,
           age6=p12011+p12012+p12035+p12036,
           age7=p12013+p12014+p12037+p12038,
           age8=p12015+p12016+p12039+p12040,
           age9=p12017+p12018+p12019+p12041+p12042+p12043,
           age10=p12020+p12021+p12022+p12023+p12024+p12025+
             p12044+p12045+p12046+p12047+p12048+p12049)%>%
    select(geoname:geonum,age1:age10)%>%
    melt(id=c("geoname", "state", "county", "place", "tract", "bg", "geonum"))%>%
    mutate(
      agecat=ordered(as.factor(variable), levels=c("age1", "age2", "age3", "age4", 
                                                   "age5", "age6", "age7", "age8",
                                                   "age9", "age10"), 
                     labels=c("Less than 5", "5 to 9",
                              "10 to 14", "15 to 19", "20 to 24", "25 to 34","35 to 44", 
                              "45 to 54", "55 to 64", "65 and Over")),
      year="2000",
      geoname=stri_trans_general(geoname, id="Title"))
  d=rbind(d10c, d00c)#%>%spread(year,value)
  p=ggplot(d, aes(x=agecat, y=value, fill=year))+
    geom_bar(stat="identity", position="dodge")+#, fill=rgb(31,74,126, max=255))+ 
    scale_y_continuous(label=comma)+
    scale_fill_manual(values=c(rgb(31,74,126, max=255), rgb(192,80,77,max=255)),
                      name="Census Year",
                      breaks=c("2010", "2000"),
                      labels=c("2010","2000"))+
    theme_codemog()+
    labs(x="Age", y="Population", title=paste(d10c$geoname, "Population by Age \nSource: U.S. Census Bureau"))
  return(p)
}
