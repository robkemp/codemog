#' Categorize a continuous education variable into common categories.
#' 
#' @param data Data Frame with the education variable to categorize.
#' @param edvar Name of the education variable.
#' @param groups Categorization Scheme: more, basic, higher 
#' @param factor Return a factor or not.  Defaults to TRUE 
#' @keywords education, recode
#' @return A factor, unless otherwise specified, with recoded \code{edvar}.
#' @examples
#' ed_cat()

ed_cat= function(data, edvar, groups="basic",factor=TRUE){
  require(car, quietly=TRUE)
  gl=list(
    more=list(
      recode="0:15=1;16:24=2",
      labels=c("Less than High School", "High School or higher"),
      levels=2),
    basic= list(
      recode="0:15=1;16:17=2;18:19=3; 20=4; 21:24=5",
      labels=c("Less than High School", "High School or GED", "Some College", 
               "Associate's Degree", "Bachelor's or higher"),
      levels=5),
    higher=list(
      recode="0:15=1;16:17=2;18:19=3; 20=4; 21=5; 22=6; 23:24=7",
      labels=c("Less than High School", "High School or GED", "Some College", 
               "Associate's Degree","Bachelor's Degree", "Master's Degree", 
               "Graduate or Professional Degree"),
      levels=7))
  grp=gl[[groups]]
  v=recode(data[[edvar]], grp$recode)
  f=ordered(v,levels=1:grp$levels, labels=grp$labels)
  if(factor==TRUE){
    return(f)
  } else{
    return(v)
  }
}
