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
hpdi.discard.id.one <- function( data )
{
  df <- data
  hull <- geometry::convhulln(df, "FA")
  # Foreach sample in hull$hull, calculate the reduction in hull$vol when discarded
  reductions <- sapply(unique(as.vector(hull$hull)), function(id) {
    nv <- geometry::convhulln(df[-id,], "FA")$area
    list( "id"=id, "delta" = hull$area - nv)
  })
  # Throw away the one which results in biggest reduction
  return(reductions[,which.max(reductions[2,])]$id)
}

hpdi.discard.id <- function( data, dim = 1, normalize = T)
{

}

#' @title Calculate samples outside credibility region using convex hull method
#' 
#' @description Calculate which samples will fall outside a credibility region.
#' 
#' @param samples Data frame holding the posterior samples. Each row is a sample, each column a parameter in the sample
#' @param max.outside Number of samples should lie outside
#' @param normalize Whether to normalize the data before calculating the region.
#' 
#' @return A boolean vector, with true for samples inside the credibility region
#'
#' @export
ci.chull <- function( samples, max.outside=1, normalize=T )
{
  d.f <- samples
  if (normalize)
    d.f <- normalize.samples(samples)
  row.id <- seq(1,nrow(d.f))
  ids <- c()
  while( length(ids)<max.outside )
  {
    id <- hpdi.discard.id.one( d.f[row.id,] )
    ids <- c(ids,row.id[id])
    row.id <- row.id[-id]
  }
  return(ids)
}

#' @title Calculate samples outside credibility region using minmax method
#' 
#' @description Calculate which samples will fall outside a credibility region.
#' 
#' @param samples Data frame holding the posterior samples. Each row is a sample, each column a parameter in the sample
#' @param max.outside Number of samples should lie outside
#' 
#' @return A boolean vector, with true for samples inside the credibility region
#'
#' @export
ci.minmax <- function( samples, max.outside=1 )
{
  d.f <- samples
  
  row.id <- seq(1,nrow(d.f))

  ids <- c()
  next.ids <- c()
  while( length(ids) + length(next.ids) <= max.outside )
  {
    ids <- c(ids, next.ids)
    next.ids <- c()
    for( j in 1:ncol(d.f) )
    {
      next.ids <- c(next.ids,
                    row.id[which.min(d.f[row.id,j])],
                    row.id[which.max(d.f[row.id,j])]
                    )
    }
    next.ids <- unique(next.ids)
    row.id <- row.id[-next.ids]
  }
  return(ids)
}

#' @title Calculate samples within credibility region
#' 
#' @description Calculate which samples will fall inside a credibility region and which outside.
#' 
#' @param samples Data frame holding the posterior samples. Each row is a sample, each column a parameter in the sample
#' @param ci Minimum fraction the credibility region should cover
#' @param method Method to use. Currently chull and minmax are supported
#' @param ... Parameters forwarded to the method used for calculating the regions
#' 
#' @return A boolean vector, with true for samples inside the credibility region
#'
#' @export
inside.ci <- function( samples, ci = 0.9, method = "chull", ... )
{
  # This can mostly be moved to general function that takes hpdi.discard.id as a function
  discard <- floor(nrow(samples)-nrow(samples)*ci)
  inside <- rep(T,nrow(samples))
  if (method == "chull")
    ids <- ci.chull( samples, discard, ... )
  else if (method == "minmax")
    ids <- ci.minmax( samples, discard, ... )
  
  if (length(ids)==0)
    warning("No samples could be discarded, choose a lower ci value")
  inside[ids] <- F
  return(inside)
}

