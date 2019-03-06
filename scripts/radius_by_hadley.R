library("ggplot2")

me <- data.frame(Time = c(0, 1, 2, 3, 4, 5, 6, 7),
                 Forcefull = c(2, 0, 1, 1, 2, 3, 4, 5),
                 Sneaky = c(3, 0, 1, 1, 2, 3, 4, 5),
                 Carefull = c(2, 0, 1, 1, 2, 3, 4, 5),
                 Flashy = c(1, 0, 1, 1, 2, 3, 4, 5),
                 Clever = c(0, 0, 1, 1, 2, 3, 4, 5),
                 Quick = c(1, 0, 1, 1, 2, 3, 4, 5))
me <- reshape2::melt(me[1, ], id.vars = "Time")
me$value <- me$value/ 5

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}

# coord_radar <- function (theta = "x", start = 0, direction = 1) 
# {
#   theta <- match.arg(theta, c("x", "y"))
#   r <- if (theta == "x") "y" else "x"
#   ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
#           direction = sign(direction),
#           is_linear = function(coord) TRUE)
# }

ggplot(me, aes(x = variable, y = value)) +
  geom_polygon(aes(group = Time, color = Time), fill = "black", size = 2, show.legend = FALSE) +
  xlab("") + ylab("") +
  theme(axis.text.x = element_text(vjust = 1)) +
  coord_radar(theta = 'x', start = 0, direction = 1)