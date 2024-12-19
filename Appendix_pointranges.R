# Clean environment
rm(list = ls())
gc()
setwd("D:\\Data\\Personal\\TeeuwenAS\\3spire")
source("./Functions.R")


# Libraries
library(gridExtra)
library(grid)
library(ggplot2)
library(ggExtra)
library(lattice)
library(egg)
library(meantables)
library(MASS)
library(RColorBrewer)
library(GGally)
library(dplyr)
library(readr)
library(tidyr)
library(ggmagnify)
library(ineq)
library(ggrepel)
library(gridExtra)

dat <- read.csv(file = "./R_Output/dat_quicker.csv") 
dat <- processdatas(dat)
 
# Structural, behavioural assumptions
asplabels <- c("I=T_F=F_L=F" = "I00", "I=F_F=T_L=F" = "0F0", "I=F_F=F_L=T" = "00L", 
               "I=F_F=T_L=T" = "0FL", "I=T_F=F_L=T" = "I0L", "I=T_F=T_L=F" = "IF0",
               "I=T_F=T_L=T" = "IFL")
declabels <- c("random draw_defined_by_similarity" = "ran_def", "sequential loop_defined_by_similarity" = "seq_def",  
               "optimised selection_random" = "opt", "random draw_random" = "ran_ran",
               "sequential loop_random" = "seq_ran")
thrlabels <- c("dynamic_external only" = "dyn_ext", "dynamic_internal and external" = "dyn_both",  
               "dynamic_internal only" = "dyn_int", "static_no adaptation" = "static")
utilabels <- c("cobb_douglasln" = "CBlog", "cobb_douglas+" = "CBs", "cobb_douglas*" = "CBp",
               "rank_sum" = "Rs", "weighted_sum" = "Ws")

# Scenarios
#clu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "scenario",
#                   xtitle = "Implementations of AAT", showlegend = FALSE, ytitle = "Scenario")

# Populations
# Repetitions

# Aspirational dimensions
## Household indicators
clu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)", labels = asplabels)
hs <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "herd", colnm = "asp_dims",
                  xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Herd size \n(number of animals)", labels = asplabels)
icv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_cv", colnm = "asp_dims",
                  xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Income volatiltity \n(coefficient of variation)", labels = asplabels)
ic <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_outcome_t1end", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Income \n(100k ETB)", labels = asplabels)
ilu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "intensive_lu", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Agricultural input use \n(fraction of farmland)", labels = asplabels)
wlt <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "savings", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Accumulated wealth \n(100k ETB)", labels = asplabels)
#fd <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_loans", colnm = "asp_dims",
#                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Food grain deficit \n(value in 100k ETB per hh member)", labels = asplabels)
fp <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maizeproduction", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Food grain production \n(tonnes)", labels = asplabels)
fpv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "asp_dims",
                  xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Food grain production volatility \n(coefficient of variation)", labels = asplabels)
fc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "value_of_consumed_products", colnm = "asp_dims",
                  xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Food consumption \n(value in 1k ETB per hh member)", labels = asplabels)

## Community indicators
fie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "food2_gini", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Food consumption inequality \n(Gini-coefficient)", labels = asplabels)
lud <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "mean_lu_div", colnm = "asp_dims",
                  xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Land use\n diversity index", labels = asplabels)
iie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_atk", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)", labels = asplabels)
nid <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Number of indebted\n households", labels = asplabels)
nnc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "asp_dims",
                   xtitle = "Aspirational dimensions", showlegend = FALSE, ytitle = "Number of households\n without cattle", labels = asplabels)

png(filename = "./SAfigures/Outcomes vs asp dims5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs asp dims5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Search for alternatives
## Household indicators
clu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)", labels = declabels)
hs <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "herd", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Herd size\n (number of animals)", labels = declabels)
icv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_cv", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Income volatiltity \n(coefficient of variation)", labels = declabels)
ic <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_outcome_t1end", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)", labels = declabels)
ilu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "intensive_lu", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)", labels = declabels)
wlt <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "savings", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)", labels = declabels)
#fd <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_loans", colnm = "decision_order",
#                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Food grain deficit\n (value in 1k ETB per hh member)", labels = declabels)
fp <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maizeproduction", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)", labels = declabels)
fpv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Food grain production volatility \n(coefficient of variation)", labels = declabels)
fc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "value_of_consumed_products", colnm = "decision_order",
                  xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)", labels = declabels)

## Community indicators
fie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "food2_gini", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)", labels = declabels)
lud <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Land use\n diversity index", labels = declabels)
iie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_atk", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)", labels = declabels)
nid <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Number of indebted\n households", labels = declabels)
nnc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "decision_order",
                   xtitle = "Search for alternatives", showlegend = FALSE, ytitle = "Number of households\n without cattle", labels = declabels)

png(filename = "./SAfigures/Outcomes vs search5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs search5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Threshold adjustment
## Household indicators
clu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "threshold_adjustment",
                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)", labels = thrlabels)
hs <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "herd", colnm = "threshold_adjustment",
                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Herd size\n (number of animals)", labels = thrlabels)
ic <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_outcome_t1end", colnm = "threshold_adjustment",
                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)", labels = thrlabels)
icv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_cv", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)", labels = thrlabels)
ilu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "intensive_lu", colnm = "threshold_adjustment",
                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)", labels = thrlabels)
wlt <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "savings", colnm = "threshold_adjustment",
                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)", labels = thrlabels)
#fd <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_loans", colnm = "threshold_adjustment",
#                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)", labels = thrlabels)
fp <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maizeproduction", colnm = "threshold_adjustment",
                      xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)", labels = thrlabels)
fpv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)", labels = thrlabels)
fc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "value_of_consumed_products", colnm = "threshold_adjustment",
                  xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)", labels = thrlabels)

## Community indicators
fie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "food2_gini", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)", labels = thrlabels)
lud <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Land use\n diversity index", labels = thrlabels)
iie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_atk", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)", labels = thrlabels)
nid <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Number of indebted\n households", labels = thrlabels)
nnc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "threshold_adjustment",
                   xtitle = "Threshold type and adjustment", showlegend = FALSE, ytitle = "Number of households\n without cattle", labels = thrlabels)

png(filename = "./SAfigures/Outcomes vs thresholds5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs thresholds5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Utility
## Household indicators
clu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "Utility_calculation",
                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)", labels = utilabels)
hs <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "herd", colnm = "Utility_calculation",
                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Herd size\n (number of animals)", labels = utilabels)
ic <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_outcome_t1end", colnm = "Utility_calculation",
                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)", labels = utilabels)
icv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_cv", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)", labels = utilabels)
ilu <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "intensive_lu", colnm = "Utility_calculation",
                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)", labels = utilabels)
wlt <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "savings", colnm = "Utility_calculation",
                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)", labels = utilabels)
#fd <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_loans", colnm = "Utility_calculation",
#                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Food grain deficit\n (value in 1k ETB per hh member)", labels = utilabels)
fp <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maizeproduction", colnm = "Utility_calculation",
                    xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)", labels = utilabels)
fpv <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)", labels = utilabels)
fc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "value_of_consumed_products", colnm = "Utility_calculation",
                  xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)", labels = utilabels)

## Community indicators
fie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "food2_gini", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)", labels = utilabels)
lud <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Land use\n diversity index", labels = utilabels)
iie <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "income_atk", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)", labels = utilabels)
nid <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Number of indebted\n households", labels = utilabels)
nnc <- cat_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "Utility_calculation",
                   xtitle = "Utility calculation", showlegend = FALSE, ytitle = "Number of households\n without cattle", labels = utilabels)

png(filename = "./SAfigures/Outcomes vs utility5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs utility5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Global variables
clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "yield_mean",
                    xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "yield_mean",
                    xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "yield_mean",
                    xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "yield_mean",
                    xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "yield_mean",
                    xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "yield_mean",
                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs mean yield5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs mean yield5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "yield_sd",
                   xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "yield_sd",
                   xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "yield_sd",
                   xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "yield_sd",
                   xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "yield_sd",
                    xtitle = "Deviation from mean yield trend", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs sd yield5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs sd yield5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "price_mean",
                   xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "price_mean",
                   xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "price_mean",
                   xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "price_mean",
                   xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "price_mean",
                    xtitle = "Mean price trend", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs mean price5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs mean price5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "price_sd",
                   xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "price_sd",
                   xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "price_sd",
                   xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "price_sd",
                   xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "price_sd",
                    xtitle = "Deviation from mean price trend", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs sd price5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs sd price5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "b_no",
                   xtitle = "b", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "b_no",
                   xtitle = "b", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "b_no",
                   xtitle = "b", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "b_no",
                   xtitle = "b", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "b_no",
                    xtitle = "b", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs b5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs b5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()


clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "optimism",
                   xtitle = "Optimism", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "optimism",
                   xtitle = "Optimism", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "optimism",
                   xtitle = "Optimism", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "optimism",
                   xtitle = "Optimism", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "optimism",
                    xtitle = "Optimism", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs optimism5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs optimism5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

clu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "cash_lu", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)")
#fd <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maize_loans", colnm = "yield_mean",
#                   xtitle = "Mean yield trend", showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
fp <- glob_scatter(data = dat, inputnm = "Population", outputnm = "maizeproduction", colnm = "maizeland_initial_run",
                   xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- glob_scatter(data = dat, inputnm = "Population", outputnm = "value_of_consumed_products", colnm = "maizeland_initial_run",
                   xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "maize_cv", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- glob_scatter(data = dat, inputnm = "Population", outputnm = "herd", colnm = "maizeland_initial_run",
                   xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_outcome_t1end", colnm = "maizeland_initial_run",
                   xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Income\n (100k ETB per year)")
icv <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_cv", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Income volatiltity\n (coefficient of variation)")
ilu <- glob_scatter(data = dat, inputnm = "Population", outputnm = "intensive_lu", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- glob_scatter(data = dat, inputnm = "Population", outputnm = "savings", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")

fie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "food2_gini", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- glob_scatter(data = dat, inputnm = "Population", outputnm = "income_atk", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "lu_div", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_indebted", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- glob_scatter(data = dat, inputnm = "scenario", outputnm = "n_nocow", colnm = "maizeland_initial_run",
                    xtitle = "Initial food grain area", showlegend = FALSE, ytitle = "Number of households\n without cattle")


png(filename = "./SAfigures/Outcomes vs maizeland5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs maizeland5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Household parameters
gc()

# Household members
dat$hhs1000 <- paste0(dat$hhid, "_", dat$pop_region_zone)

clu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "cash_lu", colnm = "hhmembers", cut = TRUE, from = 0, to = NA, 
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)") 
fp <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maizeproduction", colnm = "hhmembers", cut = TRUE, from = 0, to = NA, 
                 xtitle = "Household members", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "value_of_consumed_products", colnm = "hhmembers", cut = TRUE, from = 0, to = 20, low = 0, high = 30,
                   xtitle = "Household members", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maize_cv", colnm = "hhmembers", cut = TRUE, from = 0, to = NA, 
                    xtitle = "Household members", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "herd", colnm = "hhmembers", cut = TRUE, from = 0, to = NA, 
                 xtitle = "Household members", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_outcome_t1end", colnm = "hhmembers", cut = TRUE, from = 0, to = 20, low = 0, high = 100,
                 xtitle = "Household members", showlegend = FALSE, ytitle = "Income\n (100k ETB)")
icv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_cv", colnm = "hhmembers", cut = TRUE, from = 0, to = NA, 
                 xtitle = "Household members", showlegend = FALSE, ytitle = "Income volatility\n (coefficient of variation)")
ilu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "intensive_lu", colnm = "hhmembers", cut = TRUE, from = 0, to = NA,  
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "savings", colnm = "hhmembers", cut = TRUE, from = 0, to = 20, low = 0, high = 2000,
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")


fie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "food2_gini", colnm = "hhmembers", cut = TRUE, from = 0, to = NA,  
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_atk", colnm = "hhmembers", cut = TRUE, from = 0, to = NA,  
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "lu_div", colnm = "hhmembers", cut = TRUE, from = 0, to = NA,  
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_indebted", colnm = "hhmembers", cut = TRUE, from = 0, to = NA,  
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_nocow", colnm = "hhmembers", cut = TRUE, from = 0, to = NA, 
                  xtitle = "Household members", showlegend = FALSE, ytitle = "Number of households\n without cattle")

png(filename = "./SAfigures/Outcomes vs hhmembers5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs hhmembers5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Farmland

clu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "cash_lu", colnm = "farmland", cut = TRUE, from = 0, to = 20, 
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)") 
fp <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maizeproduction", colnm = "farmland", cut = TRUE, from = 0, to = 20, low = 0, high = 50,
                 xtitle = "Farmland", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "value_of_consumed_products", colnm = "farmland", cut = TRUE, from = 0, to = 20, low = 0, high = 30,
                 xtitle = "Farmland", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maize_cv", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "herd", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                 xtitle = "Farmland", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_outcome_t1end", colnm = "farmland", cut = TRUE, from = 0, to = 20, low = 0, high = 100,
                 xtitle = "Farmland", showlegend = FALSE, ytitle = "Income\n (100k ETB)")
icv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_cv", colnm = "farmland", cut = TRUE, from = 0, to = 20, low = 0, high = 2000,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Income volatility\n (coefficient of variation)")
ilu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "intensive_lu", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "savings", colnm = "farmland", cut = TRUE, from = 0, to = 20, low = 0, high = 2000,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")


fie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "food2_gini", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_atk", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "lu_div", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_indebted", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_nocow", colnm = "farmland", cut = TRUE, from = 0, to = 20,
                  xtitle = "Farmland", showlegend = FALSE, ytitle = "Number of households\n without cattle")

png(filename = "./SAfigures/Outcomes vs farmland5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs farmland5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

#Fields
clu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "cash_lu", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)") 
fp <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maizeproduction", colnm = "fields", cut = TRUE, from = 0, to = 15, low = 0, high = 50,
                 xtitle = "Fields", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "value_of_consumed_products", colnm = "fields", cut = TRUE, from = 0, to = 15, low = 0, high = 30,
                 xtitle = "Fields", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maize_cv", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "herd", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                 xtitle = "Fields", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_outcome_t1end", colnm = "fields", cut = TRUE, from = 0, to = 15, low = 0, high = 100,
                 xtitle = "Fields", showlegend = FALSE, ytitle = "Income\n (100k ETB)")
icv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_cv", colnm = "fields", cut = TRUE, from = 0, to = 15, low = 0, high = 2000,
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Income volatility\n (coefficient of variation)")
ilu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "intensive_lu", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "savings", colnm = "fields", cut = TRUE, from = 0, to = 15, low = 0, high = 2000,
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")


fie <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "food2_gini", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "income_atk", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "lu_div", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "n_indebted", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "n_nocow", colnm = "fields", cut = TRUE, from = 0, to = 15, 
                  xtitle = "Fields", showlegend = FALSE, ytitle = "Number of households\n without cattle")

png(filename = "./SAfigures/Outcomes vs fields5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs fields5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()


# Initial income
clu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "cash_lu", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)") 
fp <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maizeproduction", colnm = "income_initial", cut = TRUE, from = 0, to = 30, low = 0, high = 50,
                 xtitle = "Initial income", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "value_of_consumed_products", colnm = "income_initial", cut = TRUE, from = 0, to = 30, low = 0, high = 30,
                 xtitle = "Initial income", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maize_cv", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "herd", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                 xtitle = "Initial income", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_outcome_t1end", colnm = "income_initial", cut = TRUE, from = 0, to = 30, low = 0, high = 100,
                 xtitle = "Initial income", showlegend = FALSE, ytitle = "Income\n (100k ETB)")
icv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_cv", colnm = "income_initial", cut = TRUE, from = 0, to = 30, low = 0, high = 2000,
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Income volatility\n (coefficient of variation)")
ilu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "intensive_lu", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "savings", colnm = "income_initial", cut = TRUE, from = 0, to = 30, low = 0, high = 2000,
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")


fie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "food2_gini", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_atk", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "lu_div", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_indebted", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_nocow", colnm = "income_initial", cut = TRUE, from = 0, to = 30, 
                  xtitle = "Initial income", showlegend = FALSE, ytitle = "Number of households\n without cattle")

png(filename = "./SAfigures/Outcomes vs initial income5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs income_initial5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()

# Initial food self-sufficiency

# dat$food_initial <- round(dat$food_initial)
# dat$food_initial[dat$food_initial > 600] <- NA # Removing outliers
# 
# clu <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "food_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                   showlegend = FALSE, ytitle = "Cash crop cultivation (fraction of farmland)") 
# fie <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "food2_gini", colnm = "food_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                   showlegend = FALSE, ytitle = "Food consumption inequality (Gini-coefficient)")
# from <- c(xmin = -20, xmax = 40, ymin = -200, ymax = 0); to <- c(0, 45, -1200, -250)
# fd <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "maize_loans", colnm = "food_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                  showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
# fp <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "maizeproduction", colnm = "food_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                  showlegend = FALSE, ytitle = "Food grain production (tonnes)")
# from <- c(xmin = 0, xmax = 10, ymin = 0, ymax = 5); to <- c(20, 50, 10, 75)
# hs <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "herd", colnm = "food_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                  showlegend = FALSE, ytitle = "Herd size (number of animals)")
# ic <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "income_outcome_t1end", colnm = "food_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                  showlegend = FALSE, ytitle = "Income (100k ETB)")
# from <- c(xmin = 0, xmax = 10, ymin = 0, ymax = 1); to <- c(20, 50, -1, 3)
# iie <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "income_atk", colnm = "food_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                   showlegend = FALSE, ytitle = "Income inequality (Atkinson index)")
# ilu <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "intensive_lu", colnm = "food_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                   showlegend = FALSE, ytitle = "Agricultural input use (fraction of farmland)")
# wlt <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "savings", colnm = "food_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial food self-sufficiency", 
#                   showlegend = FALSE, ytitle = "Accumulated wealth (100k ETB)")
# 
# png(filename = "./SAfigures/Outcomes vs initial food.png", width = 2000, height = 2500)
# grid.arrange(clu, fie,  fd, fp, hs, ic, iie, ilu, wlt, ncol = 3)
# dev.off()

# Initial leisure
# hist(dat$leisure_initial)
# dat$leisure_initial <- round(dat$leisure_initial)
# range(dat$leisure_initial, na.rm = T)
# 
# clu <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "cash_lu", colnm = "leisure_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                   showlegend = FALSE, ytitle = "Cash crop cultivation (fraction of farmland)") 
# fie <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "food2_gini", colnm = "leisure_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                   showlegend = FALSE, ytitle = "Food consumption inequality (Gini-coefficient)")
# from <- c(xmin = -20, xmax = 40, ymin = -200, ymax = 0); to <- c(0, 45, -1200, -250)
# fd <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "maize_loans", colnm = "leisure_initial", 
#                  from = from, to = to, zoom = TRUE, sampledata = FALSE, xtitle = "Initial leisure", 
#                  showlegend = FALSE, ytitle = "Food grain deficit (value in 1k ETB per hh member)")
# fp <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "maizeproduction", colnm = "leisure_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                  showlegend = FALSE, ytitle = "Food grain production (tonnes)")
# hs <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "herd", colnm = "leisure_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                  showlegend = FALSE, ytitle = "Herd size (number of animals)")
# ic <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "income_outcome_t1end", colnm = "leisure_initial", 
#                  from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                  showlegend = FALSE, ytitle = "Income (100k ETB)")
# iie <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "income_atk", colnm = "leisure_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                   showlegend = FALSE, ytitle = "Income inequality (Atkinson index)")
# ilu <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "intensive_lu", colnm = "leisure_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                   showlegend = FALSE, ytitle = "Agricultural input use (fraction of farmland)")
# wlt <- hh_scatter(data = dat, inputnm = "scenario", outputnm = "savings", colnm = "leisure_initial", 
#                   from = from, to = to, zoom = FALSE, sampledata = FALSE, xtitle = "Initial leisure", 
#                   showlegend = FALSE, ytitle = "Accumulated wealth (100k ETB)")
# 
# png(filename = "./SAfigures/Outcomes vs initial leisure.png", width = 2000, height = 2500)
# grid.arrange(clu, fie,  fd, fp, hs, ic, iie, ilu, wlt, ncol = 3)
# dev.off()

# Relative importance income
clu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "cash_lu", colnm = "relativeimportanceincome", cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)") 
fp <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maizeproduction", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, low = 0, high = 50,
                 xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "value_of_consumed_products", colnm = "relativeimportanceincome", cut = TRUE, from = 0, to = NA, low = 0, high = 30,
                 xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maize_cv", colnm = "relativeimportanceincome", cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "herd", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                 xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_outcome_t1end", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, low = 0, high = 100,
                 xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Income\n (100k ETB)")
icv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_cv", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, low = 0, high = 2000,
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Income volatility\n (coefficient of variation)")
ilu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "intensive_lu", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "savings", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, low = 0, high = 2000,
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")


fie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "food2_gini", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_atk", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "lu_div", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_indebted", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "n_nocow", colnm = "relativeimportanceincome",  cut = TRUE, from = 0, to = NA, 
                  xtitle = "Relative importance income", showlegend = FALSE, ytitle = "Number of households\n without cattle")

png(filename = "./SAfigures/Outcomes vs RII5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs RII5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()


# Prior knowledge #n_strats
clu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "cash_lu", colnm = "n_strats", cut = TRUE, from = 0, to = NA, 
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Cash crop cultivation\n (fraction of farmland)") 
fp <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maizeproduction", colnm = "n_strats", cut = TRUE, from = 0, to = NA, low = 0, high = 50,
                 xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Food grain production\n (tonnes)")
fc <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "value_of_consumed_products", colnm = "n_strats", cut = TRUE, from = 0, to = NA, low = 0, high = 30,
                 xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Food consumption\n (value in 1k ETB per hh member)")
fpv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "maize_cv", colnm = "n_strats", cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Food grain production volatility\n (coefficient of variation)")
hs <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "herd", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                 xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Herd size\n (number of animals)")
ic <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_outcome_t1end", colnm = "n_strats",  cut = TRUE, from = 0, to = NA, low = 0, high = 100,
                 xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Income\n (100k ETB)")
icv <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "income_cv", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,  low = 0, high = 2000,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Income volatility\n (coefficient of variation)")
ilu <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "intensive_lu", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Agricultural input use\n (fraction of farmland)")
wlt <- hh_scatter(data = dat, inputnm = "hhs1000", outputnm = "savings", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,  low = 0, high = 2000,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Accumulated wealth\n (100k ETB)")


fie <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "food2_gini", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Food consumption inequality\n (Gini-coefficient)")
iie <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "income_atk", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Income inequality\n (Atkinson index)")
lud <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "lu_div", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Land use\n diversity index")
nid <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "n_indebted", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Number of indebted\n households")
nnc <- hh_scatter(data = dat, inputnm = "Repetition", outputnm = "n_nocow", colnm = "n_strats",  cut = TRUE, from = 0, to = NA,
                  xtitle = "Prior knowledge", showlegend = FALSE, ytitle = "Number of households\n without cattle")

png(filename = "./SAfigures/Outcomes vs knowledge5_hh.png", width = 2200, height = 2000)
grid.arrange(wlt,ilu, clu, fc, fp, fpv, hs, ic, icv, ncol = 3)
dev.off()

png(filename = "./SAfigures/Outcomes vs knowledge5_com.png", width = 2200, height = 1300)
grid.arrange(fie, lud, iie, nid, nnc, ncol = 3)
dev.off()
