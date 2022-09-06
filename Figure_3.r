#############################################################################
## ecochange: An R-package to derive ecosystem change indicators from freely
## available Earth Observation products  (Lara et al., 2022)
##
## Figure 3. Typologies and R-classes processed by ecochange.
#############################################################################
## 
## Required R libraries
library('lattice')
library('latticeExtra')
library('gcookbook')
library('tidyverse')
library('ggplot2')
library('viridis')
##
## Matrix for ordering the R-classes 
mat <- matrix(nrow = 13, ncol = 10)
mat[1,1]  <- mat[2,1] <- 1 
mat[1,3]  <- 1 
mat[3,2]  <- mat[4,2] <- 1 
for(i in 5:8)
mat[i,4] <- 1
mat[9,5]  <- mat[9,6] <- 1
for(i in 5:10)
    for(j in 7:8)
mat[i,j] <- 1
mat[11,9] <- mat[12,10] <- 1
for(i in 2:6)
    mat[13,i] <- 1
bin_mat <- as.data.frame(mat)
##
## Typologies
nms <- c('Print downloadable data','Administrative data map','Polygon geometry','Remote sensing product','Area of interest','Target ecosystem attributes','Layer of changes','Area of species suitability','Ecosystem-change represent.','Grid-cell indicator','Categorical data table', 'Summary data table','Inherited inputs')
colnames(bin_mat) <- c('getGADM','getrsp','rsp2ebv','echanges','sampleIndicator','gaugeIndicator','EBVstats','plot.echanges','plot.Indicator','plot.EBVstats')
bin_mat[,'Typology'] <- as.factor(nms)
dt. <- gather(bin_mat, Function, val, getGADM:plot.EBVstats, factor_key = F)
dt. <- dt. %>%
  mutate(Typology = fct_relevel(Typology, unique(rev(nms))))
tail(dt.)
fns <- colnames(bin_mat)[-1L]
dt. <- dt. %>%
  mutate(Function = fct_relevel(Function, colnames(bin_mat)[-12]))
##
## Labels for input classes
names(nms) <- c('NULL','character','SpatialPolygon','character','RasterStack','RasterStack','RasterLayer','RasterStack','RasterStack','RasterStack','tibble','tibble','... Inherits')
nmsd <- data.frame(Typology = nms, Input_class = names(nms))
require('plyr')
dt. <- join(dt., nmsd)
##
## Labels for output classes
names(fns) <- c('SpatialPolygon','character','RasterStack','RasterStack','RasterStack','tibble','tibble','levelplot\n boxplot','barplot\n boxplot','barplot')
## Data set
fnsd <- data.frame(Function = fns, Outputs = names(fns))
dt. <- join(dt., fnsd)
##
## Plot of typologies and R-classes
p <- ggplot(dt.) +
    theme_bw() +
    geom_point(aes(x = Function, y = Typology, size = val, color = Input_class)) +
    theme(aspect.ratio = 1, text = element_text(size = 12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    viridis::scale_fill_viridis(discrete = TRUE)
## p
## https://github.com/tidyverse/ggplot2/issues/3171
guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}
guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}
q <- p + guides(x.sec = guide_axis_label_trans(~names(fns))) 
q
## Warning message:
## Removed 100 rows containing missing values (geom_point).

