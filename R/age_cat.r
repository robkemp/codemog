#' Categorize a continuous age variable into common categories.
#' 
#' @param data Data Frame with the age variable to categorize.
#' @param agevar Name of the age variable.
#' @param groups Categorization Scheme: census, census65,  five, or ten. 
#' @param factor Return a factor or not.  Defaults to TRUE 
#' @keywords age, recode
#' @return A factor, unless otherwise specified, with recoded \code{agevar}.
#' @examples
#' age.cat()


age_cat= function(data, agevar, groups="census", factor=TRUE){
  require(car, quietly=TRUE)
  gl=list(
    census65=list(
      recode="0:4=1; 5:17=2; 18:24=3;  25:34=4; 35:44=5; 45:54=6; 55:64=7; 65:115=8", 
      labels=c("0 to 4" ,"5 to 17", "18 to 24", "25 to 34", "35 to 44","45 to 54","55 to 64", "65 and Over"),
      levels=8),
    census=list(
      recode="0:4=1; 5:17=2; 18:24=3;  25:34=4; 35:44=5; 45:54=6; 55:64=7; 65:74=8; 75:84=9; 85:115=10", 
      labels=c("0 to 4" ,"5 to 17", "18 to 24", "25 to 34", "35 to 44","45 to 54","55 to 64", "65 to 74", "75 to 84", "85 and Over"),
      levels=10),
    five=list(
      recode="0:4=1;5:9=2;10:14=3;15:19=4;20:24=5;25:29=6;30:34=7;35:39=8;40:44=9;45:49=10;
            50:54=11;55:59=12;60:64=13;65:69=14;70:74=15;75:79=16;80:84=17;85:115=18", 
      labels=c("0 to 4", "5 to 9", "10 to 14", "15 to 19", "20 to 24", "25 to 29", "30 to 34", 
               "35 to 39","40 to 44","45 to 49","50 to 54","55 to 59", "60 to 64", "65 to 69",
               "70 to 74", "75 to 79","80 to 84", "85 and Over"),
      levels=18),
    ten=list(
      recode="0:9=1;10:19=2;20:29=3;30:39=4;40:49=5;50:59=6;60:69=7;70:79=8;80:89=9;90:115=10",
      labels=c("0 to 9", "10 to 19", "20 to 29", "30 to 39","40 to 49","50 to 59", "60 to 69",
               "70 to 79","80 to 89", "90 and Over"),
      levels=10))
  grp=gl[[groups]]
  v=car::recode(data[[agevar]], grp$recode)
  f=ordered(v,levels=1:grp$levels, labels=grp$labels)
  if (factor==TRUE) {
    return(f)
  } else {
    return(v)
  }
}