#' Recategorize the ACS Employment status variable (ESR)
#' 
#' @param data Data Frame with the education variable to categorize.
#' @param employment var Name of the education variable.  Defaults to ESR
#' @param groups Categorization Scheme: basic, military, lf Defaults to basic
#' @param factor Return a factor or not.  Defaults to TRUE 
#' @keywords employment, recode
#' @return A factor, unless otherwise specified, with recoded \code{edvar}.
#' @examples
#' emp_cat()

emp_cat= function(data, empvar="ESR", groups="basic",factor=TRUE){
  require(car, quietly=TRUE)
  gl=list(
    basic= list(
      recode="2=10;4:5=10; 3=11; 6=12",
      labels=c("Employed", "Unemployed", "Not In Labor Force"),
      levels=12),
    military=list(
      recode="2=10;4:5=11; 3=12; 6=13",
      labels=c("Employed, Civilian","Employed, Armed Forces", "Unemployed", "Not In Labor Force"),
      levels=13),
    lf= list(
      recode="2:5=10; 6=11",
      labels=c("In Labor Force", "Not In Labor Force"),
      levels=11))
  grp=gl[[groups]]
  v=recode(data[[empvar]], grp$recode)
  f=ordered(v,levels=10:grp$levels, labels=grp$labels)
  if(factor==TRUE){
    return(f)
  } else{
    return(v)
  }
}