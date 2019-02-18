bprules <- function(x) {
  predictors <- colnames(x)
  
  assign("bp.stats",
         t(
           vapply(x, function(attr) {
             stat.bp <- boxplot.stats(attr)$stats
             c(median = stat.bp[3], iqr = stat.bp[4] - stat.bp[2])
           }, FUN.VALUE = double(2L)
           )
         )
  )
  
  medians <- bp.stats[which(bp.stats[ ,'iqr'] == 0), 'median']
  
  bp.stats[which(bp.stats[ ,'iqr'] == 0), 'iqr'] <- medians
  
  bprule <- as.data.frame(
    vapply(
      seq_along(x),
      function(j) {
        abs(x[ ,j] - bp.stats[predictors[j], "median"]) / bp.stats[predictors[j], "iqr"]
      },
      double(nrow(x))
    )
  )
  colnames(bprule) <- paste("bpr", predictors, sep="_")
  
  bprule
}