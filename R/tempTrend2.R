#' @param r spatraster, each layer represents one year
#' @param years vector of years from first year to n year represented by climate data (e.g., c(2000,2020))
#' @param th number of years required to calculate regression
#'
#' @export
temptrend = function(r, years, th) {
  notna = sum(!is.na(r)) # sum na values across temporal layers
  # if there aren't enough cells (by th), classify as NA
  notna = classify(notna, rcl = matrix(c(0, th-1, NA), nrow = 1), include.lowest = T)

  # mask r by notna
  r = mask(r, notna)

  # fit linear regression for each cell of the layered raster
  trend = regress(r, years)

  return(list(slpTrends = trend$x))

}
