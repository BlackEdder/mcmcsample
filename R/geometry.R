library(geometry)

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
.hpdi.discard.id.one <- function( data )
{
  df <- data
  hull <- convhulln(df, "FA")
  # Foreach sample in hull$hull, calculate the reduction in hull$vol when discarded
  reductions <- sapply(unique(as.vector(hull$hull)), function(id) {
    nv <- convhulln(df[-id,], "FA")$area
    list( "id"=id, "delta" = hull$area - nv)
  })
  # Throw away the one which results in biggest reduction
  return(reductions[,which.max(reductions[2,])]$id)
}

hpdi.discard.id <- function( data, dim = 1, normalize = T)
{
  d.f <- data
  if (normalize)
    d.f <- normalize.samples(data)
  row.id <- seq(1,nrow(d.f))
  ids <- c()
  while( length(ids)<dim )
  {
    id <- .hpdi.discard.id.one( d.f[row.id,] )
    ids <- c(ids,row.id[id])
    row.id <- row.id[-id]
  }
  return(ids)
}
