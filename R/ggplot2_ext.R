#' @export
gg.correlation <- function( samples, xcol, idcol, geom_diag = "histogram", geom_co = "point" )
{
  pars <- unique(samples[[idcol]])

  for (par1 in pars)
  {
    for (par2 in pars)
    {
      # Need to call the standard stat, and pass it the geom
      if (par1==par2)
      {
        gg <- ggplot2::ggplot(data=samples[(samples[[idcol]]==par1)]) +
          ggplot2::geom_histogram(ggplot2::aes_string(x=xcol))
        print(gg)
      } else
      {
        gg <- ggplot2::ggplot(data=
                   data.frame(list(par1=samples[(samples[[idcol]]==par1)],
                        par2=samples[(samples[[idcol]]==par2)]))) +
          ggplot2::geom_point(ggplot2::aes_string(x=par1,y=par2))
        print(gg)
      }
    }
  }
  return(gg)
}
