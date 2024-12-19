# OLS (based on SUR.R) for all indicators + Jackknife

# Clean environment
rm(list = ls())
gc()
setwd("D:\\Data\\Personal\\TeeuwenAS\\3spire")
source("./Functions.R")

library(systemfit)
library(dplyr)
library(ineq)
library(Matrix)
library(parallel)
library(ggplot2) ; theme_set(theme_classic(base_size = 16))
library(tidyr)

# Data
dat <- read.csv(file = "./R_Output/dat_quicker.csv") 
table(dat$Utility_calculation)
dat <- processdatas(dat)
table(dat$Utility_calculation)

xes <- c("decision_order", "threshold_adjustment", "Utility_calculation", "asp_dims", 
         "price_mean", "price_sd", "yield_mean", "yield_sd", "b_no", "optimism", 
         "maizeland_initial_run", "hhmembers", "farmland", "fields", "log_income_initial", 
         "food_initial", "leisure_initial", "relativeimportanceincome", "n_strats")

# Jackknife
results <- data.frame(Indicator = NA, Predictor = NA, Model = NA, Data = NA,
                      Coefficient = NA, Estimate = NA, StdError = NA,
                      R2 = NA, R2adj = NA, Fstat = NA, pval = NA)

for (ind in c("intensive_lu", "cash_lu", "log_savings", "log_income_outcome_t1end", "income_cv", "value_of_consumed_products",
              "maize_loans", "maizeproduction", "maize_cv", "herd", "lu_div", "sales_div")){
  
  print(paste0("started ", ind, " at ", Sys.time()))
  
  A <- makedatas2(fulldata = dat, y = ind, xes = xes) ; A$idrow <- 1:nrow(A) # no optimising and no satisficing random, why??
  A <- A  %>% mutate_if(is.logical, as.integer) # This way lm can understand
  
  A$Utility_calculation <- as.factor(A$Utility_calculation) # set the desired reference category
  A$Utility_calculation <- relevel(A$Utility_calculation, ref = "weighted_sum")
  
  colnames(A)[which(colnames(A) == ind)] <- "yvar"
  
  B <- A %>% filter(a1_random_random == FALSE, a1_random_defined == FALSE, 
                    b1_asp_Ioo == FALSE, b1_asp_oFo == FALSE, b1_asp_ooL == FALSE)
  
  C <- A %>% filter(c1_stat == FALSE)
  D <- A %>% filter(a1_random_random == FALSE, a1_random_defined == FALSE, 
                    b1_asp_Ioo == FALSE, b1_asp_oFo == FALSE, b1_asp_ooL == FALSE,
                    c1_stat == FALSE)
  
  n1 <- nrow(A) ; n2 <- nrow(B) ; n3 <- nrow(C) ; n4 <- nrow(D)
  
  weights <- c(
    rep( (n1 - n2 - n3 + n4) / n1, n1),  # Weights for A
    rep( (n2 - n4) / n1, n2),     # Weights for B
    rep( (n3 - n4) / n1, n3),     # Weights for C
    rep( (n4) /n1, n4)            # Weights for D
  )
  
  ya <- yvar ~ 
    a1_random_random + a1_random_defined + a1_satisfice_random + a1_optimise + # all categories + a0_satisfice_defined
    b1_asp_Ioo + b1_asp_oFo + b1_asp_ooL + b1_asp_IFo + b1_asp_IoL + b1_asp_oFL +  # all b0_asp_IFL +
    c1_stat + c1_dyn_inter + c1_dyn_exter + # all + c0_dyn_both
    # No utility
    price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  # No b and optimism
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial + relativeimportanceincome + n_strats
  
  yb <- yvar ~ 
    a1_satisfice_random + a1_optimise + # not random_random and random_defined + a0_satisfice_defined
    b1_asp_IFo + b1_asp_IoL + b1_asp_oFL + # > 1 dimension + b0_asp_IFL
    c1_stat + c1_dyn_inter + c1_dyn_exter + # all + c0_dyn_both
    Utility_calculation + # all
    price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  # no b and optimism
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial +
    relativeimportanceincome + n_strats
  
  yc <- yvar ~ 
    a1_random_random + a1_random_defined + a1_satisfice_random + a1_optimise + # + a0_satisfice_defined all categories
    b1_asp_Ioo + b1_asp_oFo + b1_asp_ooL + b1_asp_IFo + b1_asp_IoL + b1_asp_oFL +  # all b0_asp_IFL +
    c1_dyn_inter + c1_dyn_exter + # c0_dyn_both + not static
    # No utility
    b_no + optimism + price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  # all
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial + relativeimportanceincome + n_strats
  
  yd <- yvar ~ 
    a1_satisfice_random + a1_optimise + #not random_random and random_defined a0_satisfice_defined +
    b1_asp_IFo + b1_asp_IoL + b1_asp_oFL + # > 1 dimension + b0_asp_IFL
    c1_dyn_inter + c1_dyn_exter + # c0_dyn_both + not static
    Utility_calculation + #all
    b_no + optimism + price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  #all
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial + relativeimportanceincome + n_strats
  
  formulas <- list(ya, yb, yc, yd) ; names(formulas) <- c("A", "B", "C", "D")
  data_list <- list(A, B, C, D) ; names(data_list) <- c("A", "B", "C", "D")
  all_vars <- unique(unlist(lapply(formulas, function(f) all.vars(f)))) # Find all unique variables across all formulas
  
  jackfull <- ols_jackknife(formulas, data_list) #relativeimportance does not come out as a predictor, debug
  jackfull$Data <- "Full"
  results <- rbind(results, jackfull)
  jacksubsets <- subset_jackknife(formulas, data_list)
  results <- rbind(results, jacksubsets)
}

results$level = "Household"


# Explore community level indicators


# Community level indicators
for (ind in c("income_atk", "food2_gini", "mean_lu_div", "mean_sales_div", "n_indebted", "n_nocow")){ # "mean_income" , "mean_food2", "mean_cash_lu", , "mean_savings"
  
  print(paste0("started ", ind, " at ", Sys.time()))
  
  A <- makedatas2(fulldata = dat, y = ind, xes = xes) ; 
  A <- A  %>% mutate_if(is.logical, as.integer) # This way lm can understand
  
  A$Utility_calculation <- as.factor(A$Utility_calculation) # set the desired reference category
  A$Utility_calculation <- relevel(A$Utility_calculation, ref = "weighted_sum")
  
  colnames(A)[which(colnames(A) == ind)] <- "yvar"
  
  # Average household characteristics across populations --> 
  hhvars <- c("hhmembers", "farmland", "fields", "log_income_initial", "food_initial", "leisure_initial", "relativeimportanceincome", "n_strats")
  popvars <- colnames(A)[!colnames(A) %in% hhvars]
  
  A <- A %>% group_by_at(popvars) %>%
    summarise_at(hhvars, mean, na.rm = TRUE) %>% distinct()
  
  A$idrow <- 1:nrow(A)
  
  B <- A %>% filter(a1_random_random == FALSE, a1_random_defined == FALSE, 
                    b1_asp_Ioo == FALSE, b1_asp_oFo == FALSE, b1_asp_ooL == FALSE)
  
  C <- A %>% filter(c1_stat == FALSE)
  D <- A %>% filter(a1_random_random == FALSE, a1_random_defined == FALSE, 
                    b1_asp_Ioo == FALSE, b1_asp_oFo == FALSE, b1_asp_ooL == FALSE,
                    c1_stat == FALSE)
  
  n1 <- nrow(A) ; n2 <- nrow(B) ; n3 <- nrow(C) ; n4 <- nrow(D)
  
  weights <- c(
    rep( (n1 - n2 - n3 + n4) / n1, n1),  # Weights for A
    rep( (n2 - n4) / n1, n2),     # Weights for B
    rep( (n3 - n4) / n1, n3),     # Weights for C
    rep( (n4) /n1, n4)      # Weights for D
  )
  
  ya <- yvar ~ 
    a1_random_random + a1_random_defined + a1_satisfice_random + a1_optimise + # all categories  + a0_satisfice_defined
    b1_asp_Ioo + b1_asp_oFo + b1_asp_ooL + b1_asp_IFo + b1_asp_IoL + b1_asp_oFL + # all  + b0_asp_IFL
    c1_stat + c1_dyn_inter + c1_dyn_exter + # all  + c0_dyn_both
    # No utility
    price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  # No b and optimism
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial + relativeimportanceincome + n_strats
  
  yb <- yvar ~ 
    a1_satisfice_random + a1_optimise + # not random_random and random_defined  + a0_satisfice_defined
    b1_asp_IFo + b1_asp_IoL + b1_asp_oFL + # > 1 dimension  + b0_asp_IFL
    c1_stat + c1_dyn_inter + c1_dyn_exter + #all  + c0_dyn_both
    Utility_calculation + # all
    price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  # no b annd optimism
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial +
    relativeimportanceincome + n_strats
  
  yc <- yvar ~ 
    a1_random_random + a1_random_defined + a1_satisfice_random + a1_optimise + # all categories  + a0_satisfice_defined
    b1_asp_Ioo + b1_asp_oFo + b1_asp_ooL + b1_asp_IFo + b1_asp_IoL + b1_asp_oFL + # all  + b0_asp_IFL
    c1_dyn_inter + c1_dyn_exter + #not static  + c0_dyn_both
    # No utility
    b_no + optimism + price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  # all
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial + relativeimportanceincome + n_strats
  
  yd <- yvar ~ 
    a1_satisfice_random + a1_optimise + #not random_random and random_defined  + a0_satisfice_defined
    b1_asp_IFo + b1_asp_IoL + b1_asp_oFL + # > 1 dimension  + b0_asp_IFL
    c1_dyn_inter + c1_dyn_exter + # not static  + c0_dyn_both
    Utility_calculation + #all
    b_no + optimism + price_mean + price_sd + yield_mean + yield_sd + maizeland_initial_run +  #all
    hhmembers + farmland + fields + log_income_initial + food_initial + leisure_initial + relativeimportanceincome + n_strats
  
  formulas <- list(ya, yb, yc, yd) ; names(formulas) <- c("A", "B", "C", "D")
  data_list <- list(A, B, C, D) ; names(data_list) <- c("A", "B", "C", "D")
  all_vars <- unique(unlist(lapply(formulas, function(f) all.vars(f)))) # Find all unique variables across all formulas
  
  jackfull <- ols_jackknife(formulas, data_list)
  jackfull$Data <- "Full"
  jackfull$level <- "Community"
  results <- rbind(results, jackfull)
  jacksubsets <- subset_jackknife(formulas, data_list)
  jacksubsets$level <- "Community"
  results <- rbind(results, jacksubsets)
}

results0 <- results

resultssum <- results %>%
  filter(!is.na(Coefficient), grepl("ntercept", Coefficient) == FALSE) %>%
  group_by(Predictor, Model, Data, level) %>%
  summarise(R2 = mean(R2)) %>%
  mutate(Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0,  mean(results$R2[results$Model == "Full"], na.rm = TRUE)),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

resultssum$Indicator <- "All"
results <- full_join(results, resultssum)

out <- results %>%
  group_by(Indicator, Data, level) %>%
  mutate(R2max = max(R2),
         R2 = ifelse(Model == "Drop one", mean(R2end - R2), R2),
         Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0, R2max),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

outfull <- results %>% filter(Data == "Full") %>% group_by(Indicator, level) %>%
  mutate(R2max = max(R2),
         Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0, R2max),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

outA <- results %>% filter(Data == "A") %>% group_by(Indicator, level) %>%
  mutate(R2max = max(R2),
         Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0, R2max),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

outB <- results %>% filter(Data == "B") %>% group_by(Indicator, level) %>%
  mutate(R2max = max(R2),
         Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0, R2max),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

outC <- results %>% filter(Data == "C") %>% group_by(Indicator, level) %>%
  mutate(R2max = max(R2),
         Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0, R2max),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

outD <- results %>% filter(Data == "D") %>% group_by(Indicator, level) %>%
  mutate(R2max = max(R2),
         Dotlab = ifelse(Model == "Full", NA, round(R2, 2)),
         R2end = ifelse(Model == "Add one", 0, R2max),
         Dotlab = ifelse(Dotlab == round(R2end, 2), NA, Dotlab)) %>%
  filter(Model != "Full")

varorder <- resultssum %>% mutate(R2 = ifelse(Model == "Drop one", -R2, R2)) %>% 
  group_by(Predictor, level) %>% summarise(R2dummy = sum(R2))

out <- full_join(out, varorder)

varorderfull <- resultssum %>% filter(Data == "Full") %>% mutate(R2 = ifelse(Model == "Drop one", -R2, R2)) %>% 
  group_by(Predictor, level) %>% summarise(R2dummy = sum(R2))

varorderA <- resultssum %>% filter(Data == "A") %>% mutate(R2 = ifelse(Model == "Drop one", -R2, R2)) %>% 
  group_by(Predictor, level) %>% summarise(R2dummy = sum(R2))
varorderB <- resultssum %>% filter(Data == "B") %>% mutate(R2 = ifelse(Model == "Drop one", -R2, R2)) %>% 
  group_by(Predictor, level) %>% summarise(R2dummy = sum(R2))
varorderC <- resultssum %>% filter(Data == "C") %>% mutate(R2 = ifelse(Model == "Drop one", -R2, R2)) %>% 
  group_by(Predictor, level) %>% summarise(R2dummy = sum(R2))
varorderD <- resultssum %>% filter(Data == "D") %>% mutate(R2 = ifelse(Model == "Drop one", -R2, R2)) %>% 
  group_by(Predictor, level) %>% summarise(R2dummy = sum(R2))

outfull <- full_join(outfull, varorderfull)
outA <- full_join(outA, varorderA)
outB <- full_join(outB, varorderB)
outC <- full_join(outC, varorderC)
outD <- full_join(outD, varorderD)

out <- out[order(out$R2dummy),] 
out <- out %>% dplyr::select(Predictor, Indicator, Model, Data, R2:R2dummy, level) %>% distinct() %>% filter(Model != "None", Predictor != "None")

outfull <- outfull[order(outfull$R2dummy),] 
outfull <- outfull %>% dplyr::select(Predictor, Indicator, Model, R2:R2dummy, level) %>% distinct() %>% filter(Model != "None", Predictor != "None")
outA <- outA[order(outA$R2dummy),] 
outA <- outA %>% dplyr::select(Predictor, Indicator, Model, R2:R2dummy, level) %>% distinct() %>% filter(Model != "None", Predictor != "None", Predictor != "Utility_calculationrank_sum", Predictor != "b0_asp_IFL")
outB <- outB[order(outB$R2dummy),] 
outB <- outB %>% dplyr::select(Predictor, Indicator, Model, R2:R2dummy, level) %>% distinct() %>% filter(Model != "None", Predictor != "None", Predictor != "Utility_calculationrank_sum", Predictor != "b0_asp_IFL")
outC <- outC[order(outC$R2dummy),] 
outC <- outC %>% dplyr::select(Predictor, Indicator, Model, R2:R2dummy, level) %>% distinct() %>% filter(Model != "None", Predictor != "None", Predictor != "Utility_calculationrank_sum", Predictor != "b0_asp_IFL")
outD <- outD[order(outD$R2dummy),] 
outD <- outD %>% dplyr::select(Predictor, Indicator, Model, R2:R2dummy, level) %>% distinct() %>% filter(Model != "None", Predictor != "None", Predictor != "Utility_calculationrank_sum", Predictor != "b0_asp_IFL")

library(ggpubr)

indicatornames <-  c("All" = "All", "cash_lu" = "1: cash crop conversion", "herd" = "2: herd size", "income_cv" = "3: income volatility", "intensive_lu" = "4: agricultural input use", 
                     "log_income_outcome_t1end" = "5: income", "log_savings" = "6: wealth", "maize_cv" = "7: food production volatility",  "maize_loans" = "8: food grain deficit",
                     "maizeproduction" =  "8: food grain production", "value_of_consumed_products" =  "9: food consumption", "lu_div" = "Y: land use diversity",  "sales_div" = "Y: income diversity", 
                     "food2_gini" = "1: food consumption inequality", 
                      "mean_food2" = "5: mean food consumption", "income_atk" = "2: income inequality", 
                      "mean_income" = "6: mean income",
                     "mean_cash_lu" = "4: mean cash crop conversion", 
                     "n_indebted" = "4: indebted households",  
                      "mean_savings" = "7: mean wealth", 
                     "mean_lu_div" = "3: land use diversity", 
                     "mean_sales_div" = "X: income diversity", "n_nocow" = "5: households without cattle")

outfull <- outfull %>% filter(Predictor != "Utility_calculationrank_sum", Predictor != "b0_asp_IFL")

p <- outfull %>% filter(Indicator == "All") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "grey70", colour = "grey50") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R squared)") + 
  facet_wrap(~level, scales = "free_x", ncol = 2) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S2: utility calculation", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H3: initial food self-sufficiency", "optimism" = "G3: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G2: food grain area",
                            "log_income_initial" = "H4: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H5: household members", "c1_threshold_adjustment" = "S3: threshold type & adjustment", 
                            "leisure_initial" = "H7: initial leisure", "price_mean" = "G4: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_all_grey9.png", width = 2000, height = 1200)
p 
dev.off() 

outfull$fit <- paste0("Full model:")
outfull$fit <- ifelse(outfull$R2end == 0, "", outfull$fit)
outfull$fit2 <- paste0("R^2 == ", round(outfull$R2end, 2))
outfull$fit2 <- ifelse(outfull$R2end == 0, "", outfull$fit2)

p <- outfull %>% filter(level == "Household", Indicator != "sales_div", Indicator != "maize_loans", Indicator != "lu_div") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 2, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 2, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "grey70", colour = "grey50") + 
  geom_text(colour = "black", size = 9) +
  geom_text(aes(label = fit, y = R2end*0.9), size = 11, x = 3.5, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit2, y = R2end*0.9), size = 11, x = 2, hjust = 1, colour = "grey50", parse = TRUE) + 
  coord_flip() +
  xlab("Model parameters") + ylab(bquote("Variance explained (" ~ R^2 ~ ")")) + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S2: utility calculation", 
                            "a1_decisionmaking_order" = "S3: search method & order", "c1_threshold_adjustment" = "S4: threshold type & adjustment", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "food_initial" = "H4: initial food self-sufficiency", 
                            "log_income_initial" = "H5: initial income (log)", "hhmembers" = "H6: household members", "leisure_initial" = "H7: initial leisure",
                            "relativeimportanceincome" = "H8: relative importance income", "n_strats" = "H3: prior knowledge",  
                            "yield_mean" = "G1: yield trend", "maizeland_initial_run" = "G2: food grain area", "yield_sd" = "G3: yield variability",
                            "price_sd" = "G4: price variability", "optimism" = "G5: optimism", "price_mean" = "G6: price trend", 
                             "b_no" = "G7: b")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p 

png("./SAfigures/Jackknife_full_hh9.png", width = 2000, height = 2500)
p + facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames))
dev.off() 

p <- outfull %>% filter(level == "Community", Indicator != "mean_sales_div") %>% mutate(Dotlab = ifelse(Dotlab  == 0.01, NA, Dotlab)) %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "grey70", colour = "grey50") + 
  geom_text(colour = "black", size = 9) +
  geom_text(aes(label = fit, y = R2end*0.9), size = 11, x = 3.5, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit2, y = R2end*0.9), size = 11, x = 2, hjust = 1, colour = "grey50", parse = TRUE) + 
  coord_flip() +
  xlab("Model parameters") + ylab(bquote("Variance explained (" ~ R^2 ~ ")")) + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("c1_threshold_adjustment" = "S1: threshold type & adjustment",  "b1_asp_dims" = "S2: aspiration dimensions", 
                            "Utility_calculation" = "S3: utility calculation", "a1_decisionmaking_order" = "S4: search method & order",
                            
                            "fields" = "H2: average fields",  "hhmembers" = "H4: average household members", "log_income_initial" = "H3: average initial income (log)",
                             "leisure_initial" = "H5: average initial leisure", "farmland" = "H6: average farmland", "relativeimportanceincome" = "H8: relative importance income",
                            "food_initial" = "H7: initial food self-sufficiency", "n_strats" = "H1: prior knowledge",
                            
                            "price_sd" = "G4: price variability", "yield_mean" = "G3: yield trend", "price_mean" = "G5: price trend", "b_no" = "G7: b",
                            "maizeland_initial_run" = "G1: food grain area", "yield_sd" = "G2: yield variability", "optimism" = "G6: optimism")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p + facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames))

png("./SAfigures/Jackknife_full_com9.png", width = 2000, height = 2500/1.8)
p 
dev.off() 

outA$fit <- paste0("Full model:")
outA$fit <- ifelse(outfull$R2end == 0, "", outA$fit)
outA$fit2 <- paste0("R^2 == ", round(outA$R2end, 2))
outA$fit2 <- ifelse(outA$R2end == 0, "", outfull$fit2)


p <- outA %>% filter(level == "Household") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "grey99", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  geom_text(aes(label = fit, y = R2end*0.9), size = 11, x = 3, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit2, y = R2end*0.9), size = 11, x = 2, hjust = 1, colour = "grey50", parse = TRUE) + 
  coord_flip() +
  xlab("Model parameters") + ylab(bquote("Variance explained (" ~ R^2 ~ ")")) + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S2: utility calculation", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H3: initial food self-sufficiency", "optimism" = "G3: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G2: food grain area",
                            "log_income_initial" = "H4: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H5: household members", "c1_threshold_adjustment" = "S3: threshold type & adjustment", 
                            "leisure_initial" = "H7: initial leisure", "price_mean" = "G4: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_A_hh7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outA %>% filter(level == "Community") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "grey99", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S3: utility calculation", 
                            "farmland" = "H2: farmland", "fields" = "H1: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H7: initial food self-sufficiency", "optimism" = "G2: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G4: food grain area",
                            "log_income_initial" = "H3: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H4: household members", "c1_threshold_adjustment" = "S2: threshold type & adjustment", 
                            "leisure_initial" = "H5: initial leisure", "price_mean" = "G3: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_A_com7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outB %>% filter(level == "Household") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "yellow", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S2: utility calculation", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H3: initial food self-sufficiency", "optimism" = "G3: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G2: food grain area",
                            "log_income_initial" = "H4: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H5: household members", "c1_threshold_adjustment" = "S3: threshold type & adjustment", 
                            "leisure_initial" = "H7: initial leisure", "price_mean" = "G4: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_B_hh7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outB %>% filter(level == "Community") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "yellow", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S3: utility calculation", 
                            "farmland" = "H2: farmland", "fields" = "H1: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H7: initial food self-sufficiency", "optimism" = "G2: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G4: food grain area",
                            "log_income_initial" = "H3: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H4: household members", "c1_threshold_adjustment" = "S2: threshold type & adjustment", 
                            "leisure_initial" = "H5: initial leisure", "price_mean" = "G3: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_B_com7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outC %>% filter(level == "Household") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "red", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S2: utility calculation", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H3: initial food self-sufficiency", "optimism" = "G3: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G2: food grain area",
                            "log_income_initial" = "H4: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H5: household members", "c1_threshold_adjustment" = "S3: threshold type & adjustment", 
                            "leisure_initial" = "H7: initial leisure", "price_mean" = "G4: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_C_hh7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outC %>% filter(level == "Community") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "red", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S3: utility calculation", 
                            "farmland" = "H2: farmland", "fields" = "H1: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H7: initial food self-sufficiency", "optimism" = "G2: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G4: food grain area",
                            "log_income_initial" = "H3: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H4: household members", "c1_threshold_adjustment" = "S2: threshold type & adjustment", 
                            "leisure_initial" = "H5: initial leisure", "price_mean" = "G3: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_C_com7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outD %>% filter(level == "Household") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "orange", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S2: utility calculation", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H3: initial food self-sufficiency", "optimism" = "G3: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G2: food grain area",
                            "log_income_initial" = "H4: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H5: household members", "c1_threshold_adjustment" = "S3: threshold type & adjustment", 
                            "leisure_initial" = "H7: initial leisure", "price_mean" = "G4: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_D_hh7.png", width = 2000, height = 2500)
p 
dev.off() 

p <- outD %>% filter(level == "Community") %>%
  ggplot(aes(x = reorder(Predictor, R2dummy), y = R2, shape = Model, label = Dotlab)) + 
  geom_hline(aes(yintercept = R2max), size = 1, colour = "grey70") + 
  geom_hline(yintercept = 0, size = 1, colour = "black", alpha = 0.2) + 
  geom_point(alpha = 0.5, size = 12, fill = "orange", colour = "grey20") + 
  geom_text(colour = "black", size = 9) +
  coord_flip() +
  xlab("Model parameters") + ylab("Variance explained (R2)") + 
  facet_wrap(~Indicator, scales = "free_x", ncol = 3, labeller = as_labeller(indicatornames)) + #
  scale_shape_manual(values = c(24,25)) + 
  scale_x_discrete(labels=c("b1_asp_dims" = "S1: aspiration dimensions", "Utility_calculation" = "S3: utility calculation", 
                            "farmland" = "H2: farmland", "fields" = "H1: fields", "yield_mean" = "G1: yield trend", 
                            "food_initial" = "H7: initial food self-sufficiency", "optimism" = "G2: optimism", 
                            "a1_decisionmaking_order" = "S4: search method & order", "maizeland_initial_run" = "G4: food grain area",
                            "log_income_initial" = "H3: initial income (log)", "price_sd" = "G5: price variability", 
                            "hhmembers" = "H4: household members", "c1_threshold_adjustment" = "S2: threshold type & adjustment", 
                            "leisure_initial" = "H5: initial leisure", "price_mean" = "G3: price trend", 
                            "yield_sd" = "G6: yield variability", "b_no" = "G7: b", "relativeimportanceincome" = "H6: relative importance income")) + 
  theme_bw(base_size = 34) + theme(legend.position = "none")

p

png("./SAfigures/Jackknife_D_com7.png", width = 2000, height = 2500)
p 
dev.off() 


# Export to csv file
outfull %>% dplyr::select(Predictor, Indicator, Model, R2, level) %>% distinct() %>% write_csv("./R_Output/Jackknife_full.csv")
outA %>% dplyr::select(Predictor, Indicator, Model, R2, level) %>% distinct() %>% write_csv("./R_Output/Jackknife_A.csv")
outB %>% dplyr::select(Predictor, Indicator, Model, R2, level) %>% distinct() %>% write_csv("./R_Output/Jackknife_B.csv")
outC %>% dplyr::select(Predictor, Indicator, Model, R2, level) %>% distinct() %>% write_csv("./R_Output/Jackknife_C.csv")
outD %>% dplyr::select(Predictor, Indicator, Model, R2, level) %>% distinct() %>% write_csv("./R_Output/Jackknife_D.csv")

# Standard coefficient figures
fits <- results %>% filter(Model == "Full", !is.na(Estimate))

intercepts <- fits$Estimate[fits$Coefficient == "`(Intercept)`" & fits$Data == "Full"]
fits$Intercept <- NA
fits$Intercept[fits$Data == "Full"] <- rep(intercepts, each = length(unique(fits$Coefficient[fits$Data == "Full"])))
intercepts <- fits$Estimate[fits$Coefficient == "(Intercept)" & fits$Data == "A"]
fits$Intercept[fits$Data == "A"] <- rep(intercepts, each = length(unique(fits$Coefficient[fits$Data == "A"])))
intercepts <- fits$Estimate[fits$Coefficient == "(Intercept)" & fits$Data == "B"]
fits$Intercept[fits$Data == "B"] <- rep(intercepts, each = length(unique(fits$Coefficient[fits$Data == "B"])))
intercepts <- fits$Estimate[fits$Coefficient == "(Intercept)" & fits$Data == "C"]
fits$Intercept[fits$Data == "C"] <- rep(intercepts, each = length(unique(fits$Coefficient[fits$Data == "C"])))
intercepts <- fits$Estimate[fits$Coefficient == "(Intercept)" & fits$Data == "D"]
fits$Intercept[fits$Data == "D"] <- rep(intercepts, each = length(unique(fits$Coefficient[fits$Data == "D"])))

avfits <- fits %>% mutate(Estimate = abs(Estimate - Intercept)) %>% group_by(Coefficient, Data) %>% summarise(SumEstimate = sum(Estimate)) #Can be used to order parameters
fits <- full_join(fits, avfits)

#fits$fit <- paste0("R2 = ", round(fits$R2, 2), "\nR2adj = ", round(fits$R2adj, 2))
fits$fit <- paste0("Full model:")
fits$fit2 <- paste0("R^2 == ", round(fits$R2, 2))


fits$type <- NA
fits$type <- ifelse(grepl("(a|b|c)1_", fits$Coefficient), "Structural", fits$type)
fits$type <- ifelse(grepl("(price|yield|b)_", fits$Coefficient), "Global", fits$type)
fits$type <- ifelse(grepl("_initial", fits$Coefficient), "Household", fits$type)
fits$type <- ifelse(grepl("optimism|maizeland", fits$Coefficient), "Global", fits$type)
fits$type <- ifelse(grepl("(hhmembers|farmland|fields|relativeimportanceincome|n_strats)", fits$Coefficient), "Household", fits$type)
fits$type <- ifelse(grepl("Utility|Intercept|cobb_douglas", fits$Coefficient), "Structural", fits$type)
fits$colour <- ifelse(fits$type == "Global", "darkgreen", "darkorange")
fits$colour <- ifelse(fits$type == "Household", "blue", fits$colour)

fits$Coefficient <- gsub("\\*", "x", fits$Coefficient)
fits$Coefficient <- gsub("\\+", "t", fits$Coefficient)
fits$Coefficient <- gsub("\\`", "", fits$Coefficient)
fits$Coefficient <- gsub("Utility_calculation", "d1_", fits$Coefficient)
fits$Estend <- 0 # ifelse(fits$type == "Structural", fits$Intercept, 0)
fits$b0lab <- paste0("Intercept\n S1: income, food & leisure\n S2: weighted sum\n S3: satisfice, defined\n S4: dyn thresh, int & ext")
fits$Dotlab <- ifelse(round(fits$Estimate, 1) != 0, round(fits$Estimate, 1), NA)
fits$Dotlab <- ifelse(fits$pval <= 0.05 & !is.na(fits$Dotlab), paste0(fits$Dotlab, "*"), fits$Dotlab)
fits$Dotlab <- ifelse(fits$pval <= 0.01 & !is.na(fits$Dotlab), paste0(fits$Dotlab, "*"), fits$Dotlab)
fits$Dotlab <- ifelse(fits$pval <= 0.001 & !is.na(fits$Dotlab), paste0(fits$Dotlab, "*"), fits$Dotlab)

p <- fits %>% 
  filter(Data == "Full", level == "Household", Indicator != "sales_div", Indicator != "maize_loans", Indicator != "lu_div") %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  #geom_text(aes(label = fit), size = 9, x = 0.8, y = 4, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit), x = 0.8, size = 11, y = 5.5, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit2), x = 0.8, size = 11, y = 4, hjust = 1, colour = "grey50", parse = TRUE) + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("grey50", "grey30", "grey70", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.1 * (abs(Estimate)/Estimate)), colour = "black", size = 8) + 
  scale_colour_manual(values = c("grey50", "grey30", "grey70", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "d1_cobb_douglasln" = "S2: Cobb-Douglas log-product", "d1_cobb_douglast" = "S2: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S2: Cobb-Douglas product", "d1_rank_sum" = "S2: rank sum", 
                            "c1_dyn_exter" = "S3: dyn thresholds, ext adjust", "c1_dyn_inter" = "S3: dyn thresholds, int adjust",
                            "c1_stat" = "S3: static thresholds", 
                            "a1_optimise" = "S4: optimising", "a1_random_random" = "S4: randomise, random order", 
                            "a1_random_defined" = "S4: randome, defined order", "a1_satisfice_random" = "S4: satisfice, random order",
                            "yield_mean" = "G2: yield trend", "maizeland_initial_run" = "G1: food grain area", "yield_sd" = "G3: yield variability", "price_sd" = "G4: price variability", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", "n_strats" = "H3: prior knowledge",
                            "food_initial" = "H3: initial food self-sufficiency", "log_income_initial" = "H4: initial income (log)"
  ),
  limits = rev(c("(Intercept)", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL",
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_optimise", "a1_random_random", "a1_random_defined", "a1_satisfice_random",
                 "maizeland_initial_run", "yield_mean",  "yield_sd",
                 "farmland", "fields", "n_strats"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 34) + theme(legend.position = "none")

p 

png("./SAfigures/Coefficient estimates_hh9.png", width = 2000, height = 2300)
p + facet_wrap(~Indicator, 
               #scales = "free_x", 
               ncol = 3, labeller = as_labeller(indicatornames))
dev.off() 

p <- fits %>% 
  filter(Data == "Full", level == "Community", Indicator != "mean_sales_div") %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  #geom_text(aes(label = fit), size = 9, x = 3, y = 3, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit), x = 3, size = 11, y = 5.5, hjust = 1, colour = "grey50") + 
  geom_text(aes(label = fit2), x = 3, size = 11, y = 4, hjust = 1, colour = "grey50", parse = TRUE) + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("grey50", "grey30", "grey70", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.3 * (abs(Estimate)/Estimate)), colour = "black", size = 8) + 
  scale_colour_manual(values = c("grey50", "grey30", "grey70", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", 
                            "b1_asp_IFo" = "S2: income & food", "b1_asp_IoL" = "S2: income & leisure", "b1_asp_oFL" = "S2: food & leisure",
                            "b1_asp_Ioo" = "S2: income", "b1_asp_oFo" = "S2: food", "b1_asp_ooL" = "S2: leisure",
                            
                            "d1_cobb_douglasln" = "S3: Cobb-Douglas log-product", "d1_cobb_douglast" = "S3: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S3: Cobb-Douglas product", "d1_rank_sum" = "S3: rank sum", 
                            
                            "c1_dyn_exter" = "S1: dyn thresholds, ext adjust", "c1_dyn_inter" = "S1: dyn thresholds, int adjust",
                            "c1_stat" = "S1: static thresholds", 
                            
                            "maizeland_initial_run" = "G1: initial food grain area", "yield_sd" = "G2: yield variability", "price_sd" = "G3: price variability", 
                            "n_strats" = "H1: (average) prior knowledge","fields" = "H2: (average) fields",  "log_income_initial" = "H3: (average) initial income (log)"
  ),
  limits = rev(c("(Intercept)", 
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL",
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "maizeland_initial_run", "yield_sd",  "price_sd", 
                 "n_strats", "fields", "log_income_initial" 
                  ))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 34) + theme(legend.position = "none")

p 

png("./SAfigures/Coefficient estimates_comm9.png", width = 2000, height = 2500/1.8)
p + facet_wrap(~Indicator, 
               #scales = "free_x", 
               ncol = 3, labeller = as_labeller(indicatornames))
dev.off() 

# Subset A
p <- fits %>% 
  filter(Data == "A", level == "Household") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 0.7, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("grey89", "grey79", "grey99", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.15 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("grey30", "grey20", "grey40", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "d1_cobb_douglasln" = "S2: Cobb-Douglas log-product", "d1_cobb_douglast" = "S2: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S2: Cobb-Douglas product", "d1_rank_sum" = "S2: rank sum", 
                            "c1_dyn_exter" = "S3: dyn thresholds, ext adjust", "c1_dyn_inter" = "S3: dyn thresholds, int adjust",
                            "c1_stat" = "S3: static thresholds", 
                            "a1_optimise" = "S4: optimising", "a1_random_random" = "S4: randomise, random order", 
                            "a1_random_defined" = "S4: randome, defined order", "a1_satisfice_random" = "S4: satisfice, random order",
                            "yield_mean" = "G1: yield trend", "maizeland_initial_run" = "G2: food grain area", "optimism" = "G3: optimism", "price_mean" = "G4: price trend", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", 
                            "food_initial" = "H3: initial food self-sufficiency", "log_income_initial" = "H4: initial income (log)"
  ),
  limits = rev(c("(Intercept)", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL",
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_optimise", "a1_random_random", "a1_random_defined", "a1_satisfice_random",
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "yield_mean", "maizeland_initial_run", "optimism",  "price_mean", 
                 "farmland", "fields", 
                 "food_initial", "log_income_initial"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

p 

png("./SAfigures/Coefficient estimates_hh_A.png", width = 2000, height = 2500)
p 
dev.off() 

p <- fits %>% 
  filter(Data == "A", level == "Community") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 2.2, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("grey89", "grey79", "grey99", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.3 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("grey30", "grey20", "grey40", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "c1_dyn_exter" = "S2: dyn thresholds, ext adjust", "c1_dyn_inter" = "S2: dyn thresholds, int adjust",
                            "c1_stat" = "S2: static thresholds", "d1_cobb_douglasln" = "S3: Cobb-Douglas log-product", "d1_cobb_douglast" = "S3: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S3: Cobb-Douglas product", "d1_rank_sum" = "S3: rank sum", 
                            "a1_random_random" = "S4: random draws, random order", "a1_random_defined" = "S4: random draws, defined order", 
                            "a1_satisfice_random" = "S4: sequential loop, random order", "a1_optimise" = "S4: optimised selection",
                            "yield_mean" = "G1: yield trend", "optimism" = "G2: optimism", 
                            "price_mean" = "G3: price trend", "maizeland_initial_run" = "G4: food grain area", 
                            "fields" = "H1: mean number of fields", "farmland" = "H2: mean farmland", 
                            "log_income_initial" = "H3: mean initial income", "hhmembers" = "H4: mean household size"
  ),
  limits = rev(c("(Intercept)", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", 
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_random_random", "a1_random_defined", "a1_satisfice_random", "a1_optimise",
                 "yield_mean", "optimism", "price_mean", "maizeland_initial_run", 
                 "fields",  "farmland", 
                 "log_income_initial", "hhmembers"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

p 

png("./SAfigures/Coefficient estimates_comm_A.png", width = 2000, height = 2500)
p 
dev.off() 

# Subset B
p <- fits %>% 
  filter(Data == "B", level == "Household") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 0.7, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("yellow", "yellow3", "lightyellow", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.15 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("yellow4", "yellow4", "yellow4", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "d1_cobb_douglasln" = "S2: Cobb-Douglas log-product", "d1_cobb_douglast" = "S2: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S2: Cobb-Douglas product", "d1_rank_sum" = "S2: rank sum", 
                            "c1_dyn_exter" = "S3: dyn thresholds, ext adjust", "c1_dyn_inter" = "S3: dyn thresholds, int adjust",
                            "c1_stat" = "S3: static thresholds", 
                            "a1_optimise" = "S4: optimising", "a1_random_random" = "S4: randomise, random order", 
                            "a1_random_defined" = "S4: randome, defined order", "a1_satisfice_random" = "S4: satisfice, random order",
                            "yield_mean" = "G1: yield trend", "maizeland_initial_run" = "G2: food grain area", "optimism" = "G3: optimism", "price_mean" = "G4: price trend", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", 
                            "food_initial" = "H3: initial food self-sufficiency", "log_income_initial" = "H4: initial income (log)"
  ),
  limits = rev(c("(Intercept)", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL",
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_optimise", "a1_random_random", "a1_random_defined", "a1_satisfice_random",
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "yield_mean", "maizeland_initial_run", "optimism",  "price_mean", 
                 "farmland", "fields", 
                 "food_initial", "log_income_initial"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

p 

png("./SAfigures/Coefficient estimates_hh_B.png", width = 2000, height = 2500)
p 
dev.off() 

p <- fits %>% 
  filter(Data == "B", level == "Community") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 2.3, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("yellow", "yellow3", "lightyellow", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.3 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("yellow4", "yellow4", "yellow4", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "c1_dyn_exter" = "S2: dyn thresholds, ext adjust", "c1_dyn_inter" = "S2: dyn thresholds, int adjust",
                            "c1_stat" = "S2: static thresholds", "d1_cobb_douglasln" = "S3: Cobb-Douglas log-product", "d1_cobb_douglast" = "S3: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S3: Cobb-Douglas product", "d1_rank_sum" = "S3: rank sum", 
                            "a1_random_random" = "S4: random draws, random order", "a1_random_defined" = "S4: random draws, defined order", 
                            "a1_satisfice_random" = "S4: sequential loop, random order", "a1_optimise" = "S4: optimised selection",
                            "yield_mean" = "G1: yield trend", "optimism" = "G2: optimism", 
                            "price_mean" = "G3: price trend", "maizeland_initial_run" = "G4: food grain area", 
                            "fields" = "H1: mean number of fields", "farmland" = "H2: mean farmland", 
                            "log_income_initial" = "H3: mean initial income", "hhmembers" = "H4: mean household size"
  ),
  limits = rev(c("(Intercept)", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", 
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_random_random", "a1_random_defined", "a1_satisfice_random", "a1_optimise",
                 "yield_mean", "optimism", "price_mean", "maizeland_initial_run", 
                 "fields",  "farmland", 
                 "log_income_initial", "hhmembers"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

p 

png("./SAfigures/Coefficient estimates_comm_B.png", width = 2000, height = 2500)
p 
dev.off() 

# Subset C
p <- fits %>% 
  filter(Data == "C", level == "Household") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 0.7, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("red", "red4", "salmon", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.15 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("grey40", "grey20", "grey70", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "d1_cobb_douglasln" = "S2: Cobb-Douglas log-product", "d1_cobb_douglast" = "S2: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S2: Cobb-Douglas product", "d1_rank_sum" = "S2: rank sum", 
                            "c1_dyn_exter" = "S3: dyn thresholds, ext adjust", "c1_dyn_inter" = "S3: dyn thresholds, int adjust",
                            "c1_stat" = "S3: static thresholds", 
                            "a1_optimise" = "S4: optimising", "a1_random_random" = "S4: randomise, random order", 
                            "a1_random_defined" = "S4: randome, defined order", "a1_satisfice_random" = "S4: satisfice, random order",
                            "yield_mean" = "G1: yield trend", "maizeland_initial_run" = "G2: food grain area", "optimism" = "G3: optimism", "price_mean" = "G4: price trend", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", 
                            "food_initial" = "H3: initial food self-sufficiency", "log_income_initial" = "H4: initial income (log)"
  ),
  limits = rev(c("(Intercept)", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL",
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_optimise", "a1_random_random", "a1_random_defined", "a1_satisfice_random",
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "yield_mean", "maizeland_initial_run", "optimism",  "price_mean", 
                 "farmland", "fields", 
                 "food_initial", "log_income_initial"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

png("./SAfigures/Coefficient estimates_hh_C.png", width = 2000, height = 2500)
p 
dev.off() 

p <- fits %>% 
  filter(Data == "C", level == "Community") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 1.6, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("red", "red4", "salmon", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.3 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("grey40", "grey20", "grey70", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "c1_dyn_exter" = "S2: dyn thresholds, ext adjust", "c1_dyn_inter" = "S2: dyn thresholds, int adjust",
                            "c1_stat" = "S2: static thresholds", "d1_cobb_douglasln" = "S3: Cobb-Douglas log-product", "d1_cobb_douglast" = "S3: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S3: Cobb-Douglas product", "d1_rank_sum" = "S3: rank sum", 
                            "a1_random_random" = "S4: random draws, random order", "a1_random_defined" = "S4: random draws, defined order", 
                            "a1_satisfice_random" = "S4: sequential loop, random order", "a1_optimise" = "S4: optimised selection",
                            "yield_mean" = "G1: yield trend", "optimism" = "G2: optimism", 
                            "price_mean" = "G3: price trend", "maizeland_initial_run" = "G4: food grain area", 
                            "fields" = "H1: mean number of fields", "farmland" = "H2: mean farmland", 
                            "log_income_initial" = "H3: mean initial income", "hhmembers" = "H4: mean household size"
  ),
  limits = rev(c("(Intercept)", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", 
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_random_random", "a1_random_defined", "a1_satisfice_random", "a1_optimise",
                 "yield_mean", "optimism", "price_mean", "maizeland_initial_run", 
                 "fields",  "farmland", 
                 "log_income_initial", "hhmembers"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

png("./SAfigures/Coefficient estimates_comm_C.png", width = 2000, height = 2500)
p 
dev.off() 

# Subset D
p <- fits %>% 
  filter(Data == "D", level == "Household") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 0.7, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.5, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("darkorange", "darkorange3", "orange", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.15 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("grey40", "grey20", "grey70", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "d1_cobb_douglasln" = "S2: Cobb-Douglas log-product", "d1_cobb_douglast" = "S2: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S2: Cobb-Douglas product", "d1_rank_sum" = "S2: rank sum", 
                            "c1_dyn_exter" = "S3: dyn thresholds, ext adjust", "c1_dyn_inter" = "S3: dyn thresholds, int adjust",
                            "c1_stat" = "S3: static thresholds", 
                            "a1_optimise" = "S4: optimising", "a1_random_random" = "S4: randomise, random order", 
                            "a1_random_defined" = "S4: randome, defined order", "a1_satisfice_random" = "S4: satisfice, random order",
                            "yield_mean" = "G1: yield trend", "maizeland_initial_run" = "G2: food grain area", "optimism" = "G3: optimism", "price_mean" = "G4: price trend", 
                            "farmland" = "H1: farmland", "fields" = "H2: fields", 
                            "food_initial" = "H3: initial food self-sufficiency", "log_income_initial" = "H4: initial income (log)"
  ),
  limits = rev(c("(Intercept)", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL",
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_optimise", "a1_random_random", "a1_random_defined", "a1_satisfice_random",
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "yield_mean", "maizeland_initial_run", "optimism",  "price_mean", 
                 "farmland", "fields", 
                 "food_initial", "log_income_initial"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

png("./SAfigures/Coefficient estimates_hh_D.png", width = 2000, height = 2500)
p 
dev.off() 

p <- fits %>% 
  filter(Data == "D", level == "Community") %>% mutate(Coefficient = gsub("xs", "", Coefficient)) %>%
  ggplot(aes(y = Coefficient, x = Estimate, colour = type, fill = type)) +
  geom_vline(xintercept = 0, size = 2, alpha = 0.5) + 
  geom_vline(xintercept = 0) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 2, alpha = 0.5) + 
  geom_vline(aes(xintercept = Intercept), colour = "grey70", size = 1) + 
  geom_text(aes(label = fit), size = 9, x = 1.6, y = 3, hjust = 1, colour = "grey50") + 
  geom_segment(aes(y = Coefficient, yend = Coefficient, x = Estimate, xend = Estend,
                   alpha = 0.1), size = 1.7, lineend = "round") + 
  geom_point(shape = 21, size = 9) + scale_fill_manual(values = c("darkorange", "darkorange3", "orange", "white")) +
  geom_text(aes(label = Dotlab, 
                x = Estimate + 0.3 * (abs(Estimate)/Estimate)), colour = "grey30", size = 8) + 
  scale_colour_manual(values = c("grey40", "grey20", "grey70", "white")) + 
  scale_y_discrete(labels=c("(Intercept)" = "intercept", "b1_asp_Ioo" = "S1: income", "b1_asp_oFo" = "S1: food", "b1_asp_ooL" = "S1: leisure",
                            "b1_asp_IFo" = "S1: income & food", "b1_asp_IoL" = "S1: income & leisure", "b1_asp_oFL" = "S1: food & leisure",
                            "c1_dyn_exter" = "S2: dyn thresholds, ext adjust", "c1_dyn_inter" = "S2: dyn thresholds, int adjust",
                            "c1_stat" = "S2: static thresholds", "d1_cobb_douglasln" = "S3: Cobb-Douglas log-product", "d1_cobb_douglast" = "S3: Cobb-Douglas sum",
                            "d1_cobb_douglasx" = "S3: Cobb-Douglas product", "d1_rank_sum" = "S3: rank sum", 
                            "a1_random_random" = "S4: random draws, random order", "a1_random_defined" = "S4: random draws, defined order", 
                            "a1_satisfice_random" = "S4: sequential loop, random order", "a1_optimise" = "S4: optimised selection",
                            "yield_mean" = "G1: yield trend", "optimism" = "G2: optimism", 
                            "price_mean" = "G3: price trend", "maizeland_initial_run" = "G4: food grain area", 
                            "fields" = "H1: mean number of fields", "farmland" = "H2: mean farmland", 
                            "log_income_initial" = "H3: mean initial income", "hhmembers" = "H4: mean household size"
  ),
  limits = rev(c("(Intercept)", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", 
                 "c1_dyn_exter", "c1_dyn_inter", "c1_stat", 
                 "d1_cobb_douglasln", "d1_cobb_douglast", "d1_cobb_douglasx", "d1_rank_sum", 
                 "a1_random_random", "a1_random_defined", "a1_satisfice_random", "a1_optimise",
                 "yield_mean", "optimism", "price_mean", "maizeland_initial_run", 
                 "fields",  "farmland", 
                 "log_income_initial", "hhmembers"))) + 
  facet_wrap(~Indicator, ncol = 3, labeller = as_labeller(indicatornames)) +
  theme_bw(base_size = 30) + theme(legend.position = "none")

png("./SAfigures/Coefficient estimates_comm_D.png", width = 2000, height = 2500/1.8)
p 
dev.off() 

fits %>% dplyr::select(Predictor, Indicator, Coefficient, Estimate, pval, Dotlab, R2, Data, level) %>% distinct() %>% write_csv("./R_Output/Stdcoefs.csv")

