#' Citron palette with ramped colours.
#' Adapted from ochRe
#'
#' @param palette Choose from 'citron_palettes' list
#'
#' @param alpha transparency
#'
#' @param reverse If TRUE, the direction of the colours is reversed.
#'
#' @examples
#' scales::show_col(citron_pal()(10))
#'
#' filled.contour(volcano,color.palette = citron_pal(), asp=1)
#'
#' @export
citron_pal <- function(palette="citron_qual", alpha = 1, reverse = FALSE) {
  pal   <- citron_palettes[[palette]]
  if (reverse){
    pal <- rev(pal)
  }
  return(grDevices::colorRampPalette(pal, alpha))
}

#' Setup colour palette for ggplot2
#'
#' @rdname scale_color_citron
#'
#' @param palette Choose from 'ochre_palettes' list
#'
#' @param reverse logical, Reverse the order of the colours?
#'
#' @param alpha transparency
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @inheritParams viridis::scale_color_viridis
#'
#' @importFrom ggplot2 scale_colour_manual
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = factor(cyl))) +
#'   scale_colour_citron(palette="citron_grad")
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(aes(colour = hp)) +
#'   scale_colour_citron(palette="citron_grad_blue", discrete = FALSE)
#' ggplot(data = mpg) +
#'   geom_point(mapping = aes(x = displ, y = hwy, color = class)) +
#'   scale_colour_citron(palette="citron_div")
#' ggplot(diamonds) + geom_bar(aes(x = cut, fill = clarity)) +
#'   scale_fill_citron()
#' @export
#'
#' @importFrom ggplot2 discrete_scale scale_color_gradientn
scale_color_citron <- function(..., palette="citron_qual",
                              discrete = TRUE, alpha = 1, reverse = FALSE) {
  if (discrete) {
    ggplot2::discrete_scale("colour", "citron", palette = citron_pal(palette, alpha = alpha, reverse = reverse))
  } else {
    ggplot2::scale_color_gradientn(colours = citron_pal(palette, alpha = alpha, reverse = reverse, ...)(256))
  }
  #scale_colour_manual(values=ochre_palettes[[palette]])
}

#' @rdname scale_color_citron
#' @inheritParams viridis::scale_color_viridis
#' @inheritParams citron_pal
#' @export
scale_colour_citron <- scale_color_citron

#' Setup fill palette for ggplot2
#'
#' @param palette Choose from 'citron_palettes' list
#'
#' @inheritParams viridis::scale_fill_viridis
#' @inheritParams citron_pal
#'
#' @param discrete whether to use a discrete colour palette
#'
#' @param ... additional arguments to pass to scale_color_gradientn
#'
#' @importFrom ggplot2 scale_fill_manual discrete_scale scale_fill_gradientn
#'
#' @export
scale_fill_citron <- function(..., palette="citron_qual",
                             discrete = TRUE, alpha=1, reverse = TRUE) {
  if (discrete) {
    ggplot2::discrete_scale("fill", "citron", palette=citron_pal(palette, alpha = alpha, reverse = reverse))
  }
  else {
    ggplot2::scale_fill_gradientn(colours = citron_pal(palette, alpha = alpha, reverse = reverse, ...)(256))
  }
}
