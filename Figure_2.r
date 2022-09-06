#############################################################################
## ecochange: An R-package to derive ecosystem change indicators from freely
## available Earth Observation products  (Lara et al., 2022)
##
## Figure 2. R packages available to compute biodiversity indicators
## ordered according to the classes defined by Pereira et
## al. (2013). To support biodiversity monitoring at regional and
## national scales, the packages should include routines to download
## spatially-explicit data and workflows to compute and visualize
## indicators.
##
## Pereira, H.M., Ferrier, S., Walters, M., Geller, G.N., Jongman,
## R.H.G., Scholes, … M., & Wegmann, M. (2013). Essential biodiversity
## variables. Science 339,
## 277–278. https://doi.org/10.1126/science.1229931
##
#############################################################################
## 
## Required R libraries
library('tidyverse')
library('ggalluvial')
library('RColorBrewer')
## 
## Table with R packages classified into biodiversity classes 
dt. <- read.csv('EBV_packages_1.csv')
## 
## Data formatting
dt <- count(dt., Package.name, Data.type,
            Biodiversity.class,
            Downloading.module, Integration.module,
            name = 'Count')
## 
## Packages belonging to  multiple biodiversity classes are disregarded
## dt <- dt[!dt$'Biodiversity.class'%in%'Multiple classes',]
## 
## Formating EBV-class names  
spl <- data.frame(
    do.call('rbind', strsplit(dt$'Biodiversity.class',' ')))
spl1 <- paste0(spl[,1L],'\n ',spl[,2L])
dt[,'Biodiversity.class']  <- spl1
## 
## Color selection
my.cols. <- brewer.pal(4, "Set1")
my.cols <- my.cols.
my.cols[1:2] <- my.cols[2:1] 
my.cols[3:4] <- my.cols[4:3] 
## 
## Alluvial plot
ggplot(dt,
       aes(y = Count,
           axis1 = Package.name,
           axis2 = Biodiversity.class,
           axis4 = Data.type,
           axis3 = Downloading.module,
           axis5 = Integration.module)) +
  geom_alluvium(aes(fill = Biodiversity.class), width = 1/8) +
  geom_stratum(width = 1/12, fill = "grey", color = "black") +
    geom_label(stat = "stratum",
               aes(label = after_stat(stratum)), size = 3.2) +
    scale_x_discrete(limits = names(dt)[!names(dt)%in%'Count'],
                     labels = c('Package\n name',
                                'Biodiversity\n class',
                                'Downloading of\n data',
                                'Data\n type',
                                'Monitoring\n workflow'),
                     expand = c(.15,.15,.05,.15)) +
    scale_fill_manual(values = my.cols) +
    theme(legend.position = 'right') +
    labs(y = 'Number of packages',
         fill = 'Biodiversity class')


