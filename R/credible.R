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
hpdi.discard.id.one <- function( samples )
{
  df <- samples
  if (ncol(df)>1)
  {
    hull <- geometry::convhulln(df, "FA")
    # Foreach sample in hull$hull, calculate the reduction in hull$vol when discarded
    reductions <- sapply(unique(as.vector(hull$hull)), function(id) {
      nv <- geometry::convhulln(df[-id,], "FA")$vol
      list( "id"=id, "delta" = hull$vol - nv)
    })
    # Throw away the one which results in biggest reduction
    return(reductions[,which.max(reductions[2,])]$id)
  } else {
    v <- df[,1]
    idmin <- which.min(v)
    idmax <- which.max(v)
    rmin <- range(v[-idmin])
    rmax <- range(v[-idmax])
    if ((rmin[2]-rmin[1])>(rmax[2]-rmax[1]))
      return( idmax )
    return( idmin )
  }
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
  row.id <- seq(1,nrow(d.f))
  ids <- c()
  while( length(ids)<max.outside )
  {
    sub.df <- d.f[row.id,,drop=F]
    if (normalize)
      sub.df <- normalize.samples(sub.df)
    id <- hpdi.discard.id.one( sub.df  )
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
  running <- T
  while( running )
  {
    next.ids <- c()
    for( j in 1:ncol(d.f) )
    {
      next.ids <- c(next.ids,
                    which.min(d.f[row.id,j]),
                    which.max(d.f[row.id,j])
                    )
    }
    next.ids <- unique(next.ids)
    if ( length(ids) + length( next.ids ) <= max.outside )
      ids <- c(ids, row.id[next.ids])
    else
      running <- F
    row.id <- row.id[-next.ids]
    if (length(ids) != length(unique(ids)))
      stop("Going wrong")
  }
  return(ids)
}

#' @title Calculate samples outside credibility region using bin method
#'
#' @description Calculate which samples will fall outside a credibility region.
#'
#' @param samples Data frame holding the posterior samples. Each row is a sample, each column a parameter in the sample
#' @param max.outside Number of samples should lie outside
#'
#' @return A boolean vector, with true for samples inside the credibility region
#'
#' @import data.table
#' @export
ci.bin <- function( samples, max.outside=1 )
{
  d.f <- samples

  row.id <- seq(1,nrow(d.f))

  ids <- c()
  running <- TRUE

  no.bins <- 2
  while( running )
  {
    dropped <- bin.min(d.f[row.id,], no.bins)

    if (length(dropped$ids)+length(ids) <= max.outside) {
      ids <- c(ids, dropped$ids)
      row.id <- row.id[-dropped$ids]
      if (length(ids)==max.outside)
        running <- FALSE
    }
    else if (dropped$size>1)
    {
      no.bins <- no.bins + 1
    } else
      running <- FALSE
  }
  return(ids)
}

#' @title Calculate samples within credibility region
#'
#' @description Calculate which samples will fall inside a credibility region and which outside.
#'
#' @param samples Data frame holding the posterior samples. Each row is a sample, each column a parameter in the sample
#' @param ci Minimum fraction the credibility region should cover
#' @param method Method to use. Currently bin,chull and minmax are supported
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
  else if (method == "bin")
    ids <- ci.bin( samples, discard, ... )

  if (length(ids)==0)
    warning("No samples could be discarded, choose a lower ci value")
  inside[ids] <- F
  return(inside)
}

bin.id <- function( v, n = 10 )
{
  r <- range(v)
  binwidth <- (r[2]-r[1])/(n)
  sapply( v, function(x)
  {
    id <- 1+floor((x-r[1])/binwidth)
    if (id == n+1) # The max value goes into the previous bin
      id <- id - 1
    return(id)
  } )
}

bin.min <- function( samples, n )
{
  smpls <- data.table::as.data.table( samples )
  bins <- data.table::as.data.table(apply(smpls,2,function(v) bin.id(v,n)))
  grouped <- bins[,.N,by=eval(colnames(bins))]
  min <- min( grouped$N )
  matched <- grouped[N==min,1:ncol(smpls),with=F]
  ids <- c()

  for (id in 1:nrow(bins) )
  {
    for (k in 1:nrow(matched) )
    {
      if( identical(bins[id,], matched[k,]) )
      {
        ids <- c(ids, id)
      }
    }
  }
  return( list( "size"=min, "ids"=ids ) )
}
