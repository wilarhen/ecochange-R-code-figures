#############################################################################
## ecochange: An R-package to derive ecosystem change indicators from freely
## available Earth Observation products  (Lara et al., 2022)
##
## Figure 4. Example workflow to calculate ecosystem-class areas and
## conditional entropy processing a predefined RasterStack area of
## interest included in the package.
#############################################################################
## 
## Required R libraries
library('ecochange')
library('viridis')
#
# (Step 1)
# Example of RasterStack Area of Interest:
AOI <- system.file('amazon.grd', package = 'ecochange')
amazon <- brick(AOI)

# Subset of ecosystem variables
# (see panel names):
nm <- names(amazon)[c(3:6,2,7)]

# Levelplot:
plot.echanges(amazon[[nm]])

# (Step 2)
# Ecosystem-change representation:
ech <- echanges(amazon,
                eco = 'TC',
                change = 'loss',
                sp_dist = 'nigri',
                eco_range = c(1,80),
                change_vals = c(1,5,10,15),
                get_unaffected = TRUE,
                binary_output = FALSE,
                mc.cores = 2)
## 'eco' has length > 1: matching alphanumerics in 'eco' with values in 'change'...

plot.echanges(ech)

# See other arguments in echanges().
?echanges

# (Step 3)
# Landscape-class metric:
areas  <- gaugeIndicator(ech,
   metric = 'area',
   smp_lsm = list(what = 'lsm_c_ca'))

plot.Indicator(areas)

# (Step 4)
# Grid metric (Conditional entropy):
H <- sampleIndicator(ech,
metric = 'condent',
min = 1,
max = 80,
classes = 5)

plot.echanges(H)

