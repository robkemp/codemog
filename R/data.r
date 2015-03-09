#' County Estimates
#'
#' Population Estimates from 2010 to 2013 for the Counties in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips},
#'   \code{county}, \code{year}, \code{groupquartersPopulation}, \code{householdPopulation},\code{occupiedHousehold},
#'    \code{totalHousingUnits}, and \code{totalPopulation}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"county_est"
#' County Historical Population Estimates
#'
#' Population Estimates from 1980 to 2009 for the Counties in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips},
#'   \code{county}, \code{year}, \code{totalPopulation}, and \code{datatype}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"county_hist"

#' Municipal Estimates
#'
#' Population Estimates from 2010 to 2013 for the Municipalities in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{placefips},
#'   \code{municipality}, \code{year}, \code{groupquartersPopulation}, \code{householdPopulation},\code{occupiedHousehold},
#'    \code{totalHousingUnits}, and \code{totalPopulation}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"muni_est"

#' Municipal Historical Population Estimates
#'
#' Population Estimates from 1980 to 2009 for the Municipalities in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{placefips},
#'   \code{municipality}, \code{year}, \code{totalPopulation}, and \code{countyfips}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"muni_hist"


#' Municipalities within County Estimates
#'
#' Population Estimates from 2010 to 2013 for the municipalities within counties in 
#' Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips}, 
#' \code{placefips}, \code{municipality}, \code{year}, \code{groupquartersPopulation}, 
#' \code{householdPopulation},\code{occupiedHousehold}, \code{totalHousingUnits}, 
#' and \code{totalPopulation}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"muni_win_est"

#' Municipal parts within counties Historical Population Estimates
#'
#' Population Estimates from 1980 to 2009 for the Municipalities within counties in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{placefips},\code{county_name},
#'   \code{municipality}, \code{year}, \code{totalPopulation}, and \code{countyfips}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"muni_win_hist"

#' County by Age and Sex Estimates and Forecasts
#'
#' Population Estimates from 1990 to 2013 and population forecasts from 2014 to 2040 for
#' counties in Colorado.
#'
#' @format A data frame with nine variables: \code{countyfips},\code{year}, \code{age}, 
#'   \code{county},  \code{male Population}, \code{femalePopulation}, 
#'   \code{totalPopulation}, and \code{datatype}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"county_forecast"

#' County Base Industry Analysis Data
#'
#' Jobs by Base Industry for Colorado Counties and the Metro Area in 2013
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips},
#'   \code{county}, \code{year}, \code{baseIndustry}, \code{jobs},\code{geonum}, and \code{industry_group}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset: http://www.github.com/robkemp/codemog
"county_base"

#' County Base Industry Analysis Data
#'
#' Jobs by Base Industry for Colorado Counties and the Metro Area in 2013
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips},
#'   \code{county}, \code{year}, \code{baseIndustry}, \code{jobs},\code{geonum}, and \code{industry_group}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset: http://www.github.com/robkemp/codemog
"county_base"

#' County Jobs Data
#'
#' Jobs by sector from 2001 to 2013, estimates created by the State Demography Office.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips},
#'   \code{sector_id}, \code{year}, \code{sector_name}, \code{jobs},\code{geonum}, and \code{total_flag}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset: http://www.github.com/robkemp/codemog
"county_jobs"