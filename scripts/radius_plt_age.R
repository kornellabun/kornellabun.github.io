rm(list = ls(all = TRUE))
gc(reset = TRUE)

library(plotly)
library(dplyr)
library(ggplot2)
library(gganimate)

setwd("/home/ai/Projects/nubalion/kornellabun.github.io/")

me <- data.frame(Group = 0:34,
                 Strength = c(seq(0, 10, length.out = 15),
                              seq(10, 25, length.out = 10), 
                              seq(25, 18, length.out = 10)) / 25, # max 25
                 Dexterity = c(seq(0, 10, length.out = 5),
                               rep(10, 10),
                               seq(10, 15, length.out = 11),
                               rep(15, 4), seq(15, 20, length.out = 5)) / 25,
                 Charisma = c(seq(0, 25, length.out = 10),
                              seq(25, 15, length.out = 11), 
                              seq(15, 20, length.out = 14)) / 25,
                 Inteligence = c(seq(0, 17, length.out = 20),
                                 seq(17, 20, length.out = 15)) / 25,
                 Wisdom = c(seq(0, 10, length.out = 7),
                            rep(10, 13),
                            seq(10, 15, length.out = 5),
                            seq(15, 25, length.out = 10)) / 25,
                 Constitution = c(rep(0, 5),
                                  seq(0, 7, length.out = 7),
                                  seq(7, 10, length.out = 5), 
                                  seq(10, 25, length.out = 8), 
                                  seq(25, 17, length.out = 10)) / 25)
mtcars <- me

### 0.1b Create quantile data from the example dataset to create the grid (recommended)
# tmp <- lapply(1:ncol(mtcars[, 1:6]), function(j) {
#   rank(mtcars[, 1:6][,j], na.last = "keep")/sum(!is.na(mtcars[, 1:6][,j]))
# })
# qmtcars <- do.call(data.frame, tmp)
# colnames(qmtcars) <- colnames(mtcars[, 1:6])
# rownames(qmtcars) <- rownames(mtcars[, 1:6])
# ## do the same as with the rescaled data
# qmtcars %>% tibble::rownames_to_column(var = 'group') %>%
#   tail(4) %>% select(1:6) -> mtcars_radar
mtcars_radar <- me

# # ### 0.2 Create hover data for plotly - leave this out when you chose a)
# mtcars[, 1:6] %>%
#   tail(4) %>% select(1:6) -> mtcars_hover
# mtcars_hover[, 7] <- mtcars_hover[, 1]
# colnames(mtcars_hover)[7] <- colnames(mtcars_hover)[1]
mtcars_hover <- mtcars_radar
mtcars_hover[, 7] <- mtcars_hover[, 2]
colnames(mtcars_hover)[7] <- colnames(mtcars_hover)[2]

### 0.3 Create quantile data for the hoverinfo of the gridlines - leave this out when you chose a)
qdata <- sapply(colnames(mtcars_radar)[-1], function(j) {
  quantile(0:25/25, probs = seq(0, 1, 0.25)) #mtcars[, 2:7][, j]
  #1:8/8
})
qdata <- as.data.frame(qdata)
qdata[, 7] <- qdata[, 1]
colnames(qdata)[7] <- colnames(qdata)[1]
qdata <- as.matrix(qdata)

################################################################################
### 0.4 Set the plot parameters - mostly similar to ggradar
plot.data <- mtcars_radar
### parameters
axis.labels <- colnames(plot.data)[-1]                             
grid.min <- 0  
grid.mid <- 0.5
grid.max <- 1  
centre.y <- grid.min - ((1/9)*(grid.max-grid.min))
plot.extent.x.sf <- 1.2
plot.extent.y.sf <- 1.2
axis.label.offset <- 1.15
axis.line.colour <- "grey"
background.circle.transparency <- 0.2
r <- seq(0, 1, 0.25) ## Radius of the gridlines


#####################################################################################################
############################ 1. Helper functions ####################################################
#####################################################################################################

CalculateGroupPath4 <- function(df) {
  angles = seq(from = 0, to = 2*pi, by = (2*pi)/(ncol(df)-1)) # find increment
  xx<-c(rbind(t(plot.data.offset[,-1]) * sin(angles[-ncol(df)]),
              t(plot.data.offset[,2]) * sin(angles[1])))
  yy<-c(rbind(t(plot.data.offset[,-1]) * cos(angles[-ncol(df)]), 
              t(plot.data.offset[,2]) * cos(angles[1])))
  graphData <- data.frame(group = rep(df[,1], each = ncol(df)), x = xx, y = yy)
  return(graphData)
}

CalculateAxisPath2 <- function(var.names,min,max) {
  n <- length(var.names)
  #Cacluate required number of angles (in radians)
  angles <- seq(from = 0, to = 2*pi, by = (2*pi)/n)
  #calculate vectors of min and max x+y coords
  min.x <- min*sin(angles)
  min.y <- min*cos(angles)
  max.x <- max*sin(angles)
  max.y <- max*cos(angles)
  tmp <- lapply(1:n, function(i) {
    matrix(c(i, i, min.x[i], max.x[i], min.y[i], max.y[i]), 2, 3)
  })
  res <- as.data.frame(do.call(rbind,tmp))
  colnames(res) <- c("axis.no","x","y")
  return(res)
}

funcCircleCoords <- function(center = centre.y, r = 1, npoints = ncol(plot.data)){
  #Adapted from Joran's response to http://stackoverflow.com/questions/6862742/draw-a-circle-with-ggplot2
  tt <- seq(0,2*pi,length.out = npoints)
  yy <- center + r * cos(tt)
  xx <- center + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

#####################################################################################################
############################ 2. Prepare Plotting  ####################################################
#####################################################################################################

### 2.1. Create vector with all KPI/variable names; Set up the data for plotting 
var.names <- colnames(plot.data)[-1]  # Short version of variable names 
plot.data.offset <- plot.data
plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + 
  abs(centre.y)

### 2.2. Calculate the x and y coordinates for our data
xy_lines <- CalculateGroupPath4(plot.data.offset)
xy_lines$annot<- c(t(mtcars_hover))
xy_lines$text <- paste(paste(rep(
  colnames(mtcars_hover), nrow(mtcars_radar)), xy_lines$annot, sep=": "), '<br />')

### 2.3. Create a list containing all grid-objects:                 # Note for shiny: All elements of `grid` are static
## 2.3.1 Calculate the data frame for the axis-lines
grid <- NULL
grid$axis_path  <- CalculateAxisPath2(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
n.vars <- length(var.names)

## 2.3.2 Calculate the coordinates for the axis labels
grid$axis_label <-funcCircleCoords(
  0, (grid.max + abs(centre.y)) * axis.label.offset, ncol(plot.data))[
    -ncol(plot.data), ]
grid$axis_label$text <- axis.labels

## 2.3.3a For polygon-radar (spider-chart): Calculate the grid-lines
grid$lines <- lapply(1:length(r), function(i) {
  funcCircleCoords(0, r[i] + abs(centre.y), ncol(plot.data))
})
names(grid$lines) <- paste("q", r*100, sep='')

## 2.3.3b For circular radar: Calculate the grid-lines
grid$lines_circle <- lapply(1:length(r), function(i) {
  funcCircleCoords(0, r[i] + abs(centre.y), ncol(plot.data)*(2^7))
})
names(grid$lines_circle) <- paste(r * 100, '% Quantile', sep = '')

# ## 2.3.4 Add the real values to the gridlines
rownames(qdata) = names(grid$lines)
grid$lines <- lapply(1:length(grid$lines),
                     function(j) {
                       cbind(grid$lines[[j]],
                             values = round(qdata[names(grid$lines[j]),], 2))
                     })
names(grid$lines) <- rownames(qdata)

# ## 2.3.5 Bind all the grid-lines in one data.frame
grid$all_lines <- do.call(rbind, grid$lines)
n <- nrow(grid$all_lines)/length(grid$lines)        # n 
grid$all_lines$q <- rep(names(grid$lines), each = n)  # The quantiles of each grid

#### next
### Combine with all the code in the first chunk of shiny_example.Rmd to make it work
### 3.1. Create an empty theme for ggplot
theme_clear <- theme_bw(base_size = 24) + 
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.key = element_rect(linetype = "blank"))

### 3.2. Set the extent of the plot and the colors of the grid
plot.extent.x = (grid.max+abs(centre.y)) * plot.extent.x.sf
plot.extent.y = (grid.max+abs(centre.y)) * plot.extent.y.sf
bg_colors <- c(RColorBrewer::brewer.pal(9, 'Purples')[2], '#DFDFED') # Set the grid colors

### 3.3. Set up a basic empty gg-object
basething <- ggplot() + xlab(NULL) + ylab(NULL) + coord_fixed() +
  scale_x_continuous(limits = c(-plot.extent.x, plot.extent.x)) + 
  scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y)) + 
  theme_clear

### 3.4 For the polygon-radar (spider-chart): Add the gridlines
n <- nrow(grid$all_lines)/length(grid$lines)
base_grid <- basething + 
  geom_polygon(data = grid$all_lines[nrow(grid$all_lines):1, ],
               aes(x, y, group = rev(q)),
               fill = c(rep(rep(rev(bg_colors), 2), each = n), 
                        rep('white', n))) +
  geom_polygon(data = grid$lines$q0, aes(x, y), fill = "white") +
  geom_path(data = grid$axis_path,
            aes(x = x, y = y, group = axis.no),
            colour = axis.line.colour, alpha = 0.4)

### 3.6 Add observation lines to gg-object
gg <- base_grid + 
  geom_polygon(data = xy_lines, 
               aes(x = x, y = y, group = group, color = group, 
                   fill = group), 
               alpha = 0.2, size = 1) +
  # geom_text(data = grid$all_lines,
  #           aes(x, y, label = values), size = 2) +
  geom_text(data = grid$axis_label, aes(x, y, label = text), size = 4) +
  theme(legend.position = "left",
        legend.title = element_blank())
gg

age <- 28
gg <- base_grid + 
  geom_polygon(data = xy_lines, #[xy_lines$group == age, ], 
               aes(x = x, y = y, group = group),
               fill = "#56b4e9", color = "#0072b2",
               alpha = 0.6, size = 1) +
  # geom_text(data = grid$all_lines,
  #           aes(x, y, label = values), size = 2) +
  geom_text(data = grid$axis_label, aes(x, y, label = text), size = 6) +
  theme(legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Age: {frame_time}') +
  transition_time(group) +
  ease_aes('linear')

anim_save("images/growth.gif", animation = 
            animate(gg, 
                    nframes = 100, fps = 10, 
                    detail = 10, 
                    renderer = gifski_renderer(), device = "png"))

#ggsave("~/hxh.png", gg, width = 10, height = 10, dpi = 400)
