#' Links R to the URL based REST API for the CO Demogrpahy Office Databases
#'
#' Takes some basic geographic information along with the table/field and 
#' dataset to grab tables from the CO Demography Office cleaned Census
#' databases.
#' 
#' @param datacall table or field based on call, field if specific column, table for all Defaults to table
#' @param data number of either a census product table or table and field number based on the datacall
#' @param db database c1980, c1990, c2000, c2010 for Census Data, acs0812 or acs0913 for ACS data
#' @param geonum a geographic identifier created by 1 followed by a State FIPS and a PLace/County FIPS Defaults to 108
#' @param sumlev can be used to call all geographies in a summary level (See Census for Definitions) Defaults to NULL
#' @param type can siwtch between JSON and CSV for output format (pretty much use CSV only) Defaults to CSV
#' @param meta a command that indicates whether include feild and table meta data in line 2 Defaults to yes

codemog_api=function(datacall="table",data, db="c2010", geonum="108", sumlev=NULL, type="csv", meta="yes"){
  
  url_base="http://codemogapi-166520.usw1.nitrousbox.com/demog.php?"
  
  call=switch(datacall,
              field=paste("&field=", data, sep=""),
              table=paste("&table=", data, sep=""))
  db=paste("db=", db, sep="")
  geonum=paste("&geonum=", geonum, sep="")
  sumlev=paste("&sumlev=", sumlev, sep="")
  type=paste("&type=", type, sep="")
  url=paste(url_base,db,call,geonum,sumlev,type, sep="")
  x=read.csv(url, stringsAsFactors=FALSE)
  y=switch(meta,yes=x, no=x[-1,])
  return(y)
}

ms_ed=function(fips, state="08", fips2="", state2="08"){
  require(stringr, quietly=TRUE)
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  require(reshape2, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(dplyr, quietly=TRUE)
  
  
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
chart=p
data=d
  return(list(chart, data))
  
}

ms_census_age=function(fips, state="08"){
  require(ggplot2, quietly=TRUE)
  require(scales, quietly=TRUE)  
  require(grid, quietly=TRUE)
  require(reshape2, quietly=TRUE)
  require(tidyr, quietly=TRUE)
  require(stringi, quietly=TRUE)
  require(dplyr, quietly=TRUE)
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
 chart=p
 data=d
  return(list(chart, data))
}

