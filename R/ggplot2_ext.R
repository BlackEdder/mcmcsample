StatCM <- ggplot2::ggproto("StatCM", ggplot2::Stat,
                  setup_data = function(data,params) {
                    data <- data[order(data$x,data$y),]
                    data$alpha <- rep(0.1,length(data$x))
                    data
                  },
                  compute_group = function(data, scales) {
                    lw <- seq(1,length(data$x),2)
                    hg <- rev(seq(2,length(data$x),2))
                    data[c(lw,hg),]
                  },
                  default_aes = ggplot2::aes(fill = ..id.., colour=..id..),
                  required_aes = c("x", "id")
)

#'
#' @title Draw a covariance matrix
#'
#' @description stat_ function to draw a covariance matrix. Will draw 2D point plots (or hist?)
#' when column id != row id, histogram otherwise. The x column should hold the values of the
#' different measurements, while the id column holds the measurement id/name
#'
#' @param mapping The aesthetic mapping, usually constructed with aes or aes_string. Only needs to be set at the layer level if you are overriding the plot defaults.
#' @param data A layer specific dataset - only needed if you want to override the plot defaults.
#' @param geom The geometric object to use display the data
#' @param position The position adjustment to use for overlapping points on this layer
#' @param na.rm If FALSE (the default), removes missing values with a warning. If TRUE silently removes missing values.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' @param ... other arguments passed on to layer. This can include aesthetics whose values you want to set, not map. See layer for more details.
#'
#' @export
stat_ci <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatCI, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
