
#' Calculates annualized growth rates.
#' 
#' @param begin Value at the beginning of the period.
#' @param end Value at the end of the period.
#' @param n Number of years from beginning to end
#' @keywords growth rate, annualized
#' @examples
#' ann.gr(4300,7950, 10)


ann_gr<- function(begin,end, n){
  gr=(((end/begin)^(1/n))-1)*100
  return(gr)
}