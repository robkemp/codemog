#' County Estimates
#'
#' Population Estimates from 2010 to 2013 for the Counties in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{countyfips},
#'   \code{county}, \code{year}, \code{groupquartersPopulation}, \code{householdPopulation},\code{occupiedHousehold},
#'    \code{totalHousingUnits}, and \code{totalPopulation}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"county_est"

#' Municipal Estimates
#'
#' Population Estimates from 2010 to 2013 for the Municipalities in Colorado.
#'
#' @format A data frame with nine variables: \code{geonum}, \code{placefips},
#'   \code{municipality}, \code{year}, \code{groupquartersPopulation}, \code{householdPopulation},\code{occupiedHousehold},
#'    \code{totalHousingUnits}, and \code{totalPopulation}.
#' @details See 'Data Dictionary' vignette for specifics on the dataset
"muni_est"

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