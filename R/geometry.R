normalize.samples <- function( data )
{
  df <- data
  for (i in 1:ncol(df)) {
    mi <- min(df[,i])
    ma <- max(df[,i])
    df[,i] <- sapply( df[,i], function(x) (x-mi)/(ma-mi) )
  }
  return(df)
}

# Sample that when discarded reduces volume the most
# Iterative throwing away might not be optimal, could also say what if we 
# throw you away and closest n friends. That might solve some problems with
# outlying clusters. Quite dangerous though.
# Maybe add a jitter=F to deal with outliers that are exactly the same
hpdi.discard.id <- function( data, normalize = T )
{
  # Normalize first if needed
  
  hull <- convhulln(df, "FA")
  # Foreach sample in hull$hull, calculate the reduction in hull$vol when discarded
  # Throw away the one which results in biggest reduction
}