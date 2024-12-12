#' Rescale two indices to have the same geometric mean
#' 
#' @param idx1 An object of class \code{brmsfit}.
#' @param idx2 The variable to obtain.
#' @return A \code{data.frame}.
#' 
#' @import dplyr
#' @export
#' 
rescale_index <- function(fit1, fit2, rescale_series = 1) {
  
  for (i in 1:length(fits)) {
    if (i != rescale_series) {
      fout <- df0[[i]]
      df1 <- df0[[rescale_series]] %>% filter(Year %in% fout$Year)
      df2 <- df0[[i]] %>% filter(Year %in% df1$Year)
      gm1 <- geo_mean(df1$Mean)
      gm2 <- geo_mean(df2$Mean)
      fout$Mean <- fout$Mean / gm2 * gm1
      fout$Qlower <- fout$Qlower / gm2 * gm1
      fout$Qupper <- fout$Qupper / gm2 * gm1
      fout$Median <- fout$Median / gm2 * gm1
      df0[[i]] <- fout
    }
  }
  
  return(coefs)
}
