library(tidyverse)
library(ggplot2)
library(magrittr)
library(viridis)
library(cowplot)
library(grid)

ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

geom_boxplot2 <- function(mapping = NULL, data = NULL,
                          stat = "boxplot", position = "dodge",
                          ...,
                          outlier.colour = NULL,
                          outlier.color = NULL,
                          outlier.fill = NULL,
                          outlier.shape = 19,
                          outlier.size = 1.5,
                          outlier.stroke = 0.5,
                          outlier.alpha = NULL,
                          notch = FALSE,
                          notchwidth = 0.5,
                          varwidth = FALSE,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomBoxplot2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      outlier.colour = outlier.color %||% outlier.colour,
      outlier.fill = outlier.fill,
      outlier.shape = outlier.shape,
      outlier.size = outlier.size,
      outlier.stroke = outlier.stroke,
      outlier.alpha = outlier.alpha,
      notch = notch,
      notchwidth = notchwidth,
      varwidth = varwidth,
      na.rm = na.rm,
      ...
    )
  )
}

newggtheme <- theme(
  plot.title = element_text(hjust =.5, vjust = 1, size = rel(2.3)),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  panel.border = element_rect(fill = NA, colour = "black"),
  plot.background = element_blank(),
  text = element_text(color = "gray20", size = 10),
  axis.text = element_text(size = rel(1)),
  axis.text.x = element_text(color = "gray20", size = rel(1.8)),
  axis.text.y = element_text(color = "gray20", size = rel(1.8)),
  axis.title.x = element_text(size = rel(2.5), vjust = 0),
  axis.title.y = element_text(size = rel(2.5), vjust = 1),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_blank(),
  strip.text.x = element_text(size = rel(2.3)),
  legend.key=element_rect(fill=NA),
  legend.title = element_text(size=rel(2.0)),
  legend.text=element_text(size=rel(1.7))
)


GeomBoxplot2 <- ggproto("GeomBoxplot2", Geom,
                        setup_data = function(data, params) {
                          data$width <- data$width %||%
                            params$width %||% (resolution(data$x, FALSE) * 0.9)
                          
                          if (!is.null(data$outliers)) {
                            suppressWarnings({
                              out_min <- vapply(data$outliers, min, numeric(1))
                              out_max <- vapply(data$outliers, max, numeric(1))
                            })
                            
                            data$ymin_final <- pmin(out_min, data$ymin)
                            data$ymax_final <- pmax(out_max, data$ymax)
                          }
                          
                          # if `varwidth` not requested or not available, don't use it
                          if (is.null(params) || is.null(params$varwidth) || !params$varwidth || is.null(data$relvarwidth)) {
                            data$xmin <- data$x - data$width / 2
                            data$xmin2 <- data$x - data$width / 4
                            data$xmax <- data$x + data$width / 2
                          } else {
                            # make `relvarwidth` relative to the size of the largest group
                            data$relvarwidth <- data$relvarwidth / max(data$relvarwidth)
                            data$xmin <- data$x - data$relvarwidth * data$width / 2
                            data$xmin2 <- data$x - data$relvarwidth * data$width / 4
                            data$xmax <- data$x + data$relvarwidth * data$width / 2
                          }
                          data$width <- NULL
                          if (!is.null(data$relvarwidth)) data$relvarwidth <- NULL
                          
                          data
                        },
                        
                        draw_group = function(data, panel_params, coord, fatten = 2,
                                              outlier.colour = NULL, outlier.fill = NULL,
                                              outlier.shape = 19,
                                              outlier.size = 1.5, outlier.stroke = 0.5,
                                              outlier.alpha = NULL,
                                              notch = FALSE, notchwidth = 0.5, varwidth = FALSE) {
                          
                          common <- data.frame(
                            colour = data$colour,
                            size = data$size,
                            linetype = data$linetype,
                            fill = alpha(data$fill, data$alpha),
                            group = data$group,
                            stringsAsFactors = FALSE
                          )
                          
                          whiskers <- data.frame(
                            x = c(data$x,data$x,data$xmin2,data$xmin2),
                            xend = c(data$x,data$x,data$x,data$x),
                            y = c(data$upper, data$lower,data$ymax,data$ymin),
                            yend = c(data$ymax, data$ymin,data$ymax,data$ymin),
                            alpha = NA,
                            common,
                            stringsAsFactors = FALSE
                          )
                          
                          
                          box <- data.frame(
                            xmin = data$xmin,
                            xmax = data$x,
                            ymin = data$lower,
                            y = data$middle,
                            ymax = data$upper,
                            ynotchlower = ifelse(notch, data$notchlower, NA),
                            ynotchupper = ifelse(notch, data$notchupper, NA),
                            notchwidth = notchwidth,
                            alpha = data$alpha,
                            common,
                            stringsAsFactors = FALSE
                          )
                          
                          if (!is.null(data$outliers) && length(data$outliers[[1]] >= 1)) {
                            outliers <- data.frame(
                              y = data$outliers[[1]],
                              x = data$x[1],
                              colour = outlier.colour %||% data$colour[1],
                              fill = outlier.fill %||% data$fill[1],
                              shape = outlier.shape %||% data$shape[1],
                              size = outlier.size %||% data$size[1],
                              stroke = outlier.stroke %||% data$stroke[1],
                              fill = NA,
                              alpha = outlier.alpha %||% data$alpha[1],
                              stringsAsFactors = FALSE
                            )
                            outliers_grob <- GeomPoint$draw_panel(outliers, panel_params, coord)
                          } else {
                            outliers_grob <- NULL
                          }
                          
                          ggname("geom_boxplot2", grobTree(
                            outliers_grob,
                            GeomSegment$draw_panel(whiskers, panel_params, coord),
                            GeomCrossbar$draw_panel(box, fatten = fatten, panel_params, coord)
                          ))
                        },
                        
                        draw_key = draw_key_boxplot,
                        
                        default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                                          alpha = NA, shape = 19, linetype = "solid"),
                        
                        required_aes = c("x", "lower", "upper", "middle", "ymin", "ymax")
)