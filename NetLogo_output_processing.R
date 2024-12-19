# Test loading data using furrr

rm(list = ls())
gc()
setwd("D:\\Data\\Personal\\TeeuwenAS\\3spire")

# To do:
## test
## sat --> seq
## years --> numeric

# Done:
## _quicker

library(furrr)
library(dplyr)
library(readr)
library(purrr)
library(ggplot2)
library(ggExtra)
library(stringr)
library(tidyr)
library(mclm)
library(tictoc)
library(ineq)
is_all_numeric <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}
source("./Functions.R")

future::plan(multisession, workers = 15) 
# future::plan(sequential)

runfiles <- list.files("./Netlogo_Output/SArunfiles_quicker", full.names = TRUE, pattern = ".txt")
runfiles_short <- list.files("./Netlogo_Output/SArunfiles_quicker", full.names = FALSE, pattern = ".txt")
tickfiles <- list.files("./Netlogo_Output/SAtickfiles_quicker", full.names = TRUE, pattern = ".txt")
tickfiles_short <- list.files("./Netlogo_Output/SAtickfiles_quicker", full.names = FALSE, pattern = ".txt")

tic()
str_order <- c("ran_", "def_")
dec_mak <- c("ran_", "seq_", "opt_")
thre_typ <- c("stat_", "dyn_", "inf_")
adjust <- c("in_", "ex_", "ie_")
util_calc <- c("WS_", "RS_", "CBt_", "CBx_", "CBln_")
aspsi <- c("It_", "If_")
aspsf <- c("Ft_", "Ff_")
aspsl <- c("Lt", "Lf")

ascens <- do.call(paste0,expand.grid(str_order, dec_mak))
ascens <- ascens[-which(ascens == "def_opt_")] # strategy order does not matter for optim, only ran with "ran"
ascens <- do.call(paste0,expand.grid(ascens, util_calc))
ascens <- ascens[-grep("_ran_(RS|CBt|CBx|CBln)", x = ascens)] # utility calculation does not matter for random draws, ran with "WS" only
ascens <- do.call(paste0,expand.grid(ascens, thre_typ))
ascens <- ascens[-grep("seq_(WS|RS|CBt|CBx|CBln)_inf_", x = ascens)] # infinite threshold dont make sense for satisficing, give same result as optimising
ascens <- do.call(paste0,expand.grid(ascens, adjust)) 
ascens <- ascens[-grep("_(stat|inf)_(ie|ex)_", x = ascens)] # adjustment does not matter for static or infinite, run with "in" only
ascens <- do.call(paste0,expand.grid(ascens, aspsi)) 
ascens <- do.call(paste0,expand.grid(ascens, aspsf)) 
ascens <- do.call(paste0,expand.grid(ascens, aspsl)) 
ascens <- c(ascens[-grep("If_Ff_Lf$", x = ascens)], "ran_ran_WS_dyn_in_If_Ff_Lf")
ascens <- ascens[-grep("(RS|CBt|CBx|CBln)_(dyn|stat|inf)_(in|ie|ex)_(It_Ff_Lf|If_Ft_Lf|If_Ff_Lt)", ascens)] #If there is only one aspiration dimension, util does not matter, run with "WS" only

ascensdf <- data.frame(scenario = ascens, order = NA, decision = NA, utility = NA, threshold = NA, adaptation = NA, income = NA, food = NA, leisure = NA)
print(ascensdf)

for(i in 1:nrow(ascensdf)){
  ascensdf[i,2:9] <- unlist(strsplit(ascensdf[i,1], split = "_"))
}

ascensdf$colour <- NA
ascensdf$colour[ascensdf$nasp == 0] <- "green"
ascensdf$colour[ascensdf$threshold == "dyn" & ascensdf$decision == "seq"] <- "yellow"
ascensdf$nasp <- grepl("t", ascensdf$income) +  grepl("t", ascensdf$food) +  grepl("t", ascensdf$leisure)

rm(ascensdf)

# # For testing 
# these <- sample(1:length(runfiles), size = 20)
# runfiles <- sort(runfiles)
# tickfiles <- sort(tickfiles)
# runfiles <- runfiles[these] ; tickfiles <- tickfiles[these] ; runfiles_short <- runfiles_short[these] ; tickfiles_short <- tickfiles_short[these]
# 
# # 

tic()
runl <- future_map(runfiles, ~c(read_delim(.x))) %>%
  set_names(runfiles_short)

rundat <- bind_rows(runl)
unique(rundat$match_check)
colnames(rundat)[4:38] <- c(colnames(rundat)[5:38], "remove")
rundat <- rundat %>% select(-remove)
table(rundat$Utility_calculation)

write_csv(rundat, file = "./R_Output/rundat_quicker.csv") #
rm(runl)

toc()

rundat$runID <- str_trim(rundat$runID)
rundat$hhid <- str_trim(rundat$hhid)
rundat$scenario <- gsub(pattern = "_p[0-9]+_r[0-9]+", replacement = "", x = rundat$runID)
rundat$income_on <- grepl(pattern = "_It_", rundat$runID)
rundat$food_on <- grepl(pattern = "_Ft_", rundat$runID)
rundat$leisure_on <- grepl(pattern = "_Lt_", rundat$runID)

print("Remove these scenarios (they are non-sensical):")
unique(rundat$scenario)[!(unique(rundat$scenario) %in% ascens)] #these scenarios are nonsensical

rundat <- rundat %>% filter(rundat$scenario %in% ascens) #keep only sensical scenarios
table(rundat$Utility_calculation)

write_csv(rundat, file = "./R_Output/rundat_quicker.csv")
toc()

tic() # This takes ~3 hours
# Prepare input and output variables for sensitivity analysis
savl <- future_map(tickfiles, ~c( 
  read_delim(.x) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    filter(year == 23) %>%
    select(runID, hhid, savings) %>%
    rename(savings_t1start = savings) %>%
    mutate_if(is.character, str_trim) %>%
    distinct() %>%
    mutate_if(is_all_numeric, as.numeric) %>%
    mutate(savings_t1start = as.numeric(as.character(savings_t1start))))) %>%
  set_names(tickfiles_short)

savings <- bind_rows(savl)
head(savings)
rundat <- left_join(rundat, savings)

table(rundat$Utility_calculation)

rm(savl)
write_csv(savings, file = "./R_Output/savings_quicker.csv")
rm(savings)
toc()

tic()
# Variability over time
scale_values <- function(x){(x-min(x))/(max(x)-min(x))} # scale between zero and one

varl <- future_map(tickfiles, ~c(
  read_delim(.x) %>%
    select(runID, hhid, year, income_outcome_t1end, maizeproduction) %>%
    mutate(year = as.numeric(as.character(year)),
           income_scaled = scale_values(as.numeric(as.character(income_outcome_t1end))),
           maize_scaled = scale_values(as.numeric(as.character(maizeproduction)))) %>%
    group_by(runID, hhid) %>%
    summarise(income_sd = sd(income_outcome_t1end, na.rm = TRUE),
              maize_sd = sd(maizeproduction, na.rm = TRUE),
              income_mean = mean(income_scaled, na.rm = TRUE),
              maize_mean = mean(maize_scaled, na.rm = TRUE)
              ))) %>%
  set_names(tickfiles_short)

 
varl <- bind_rows(varl) %>%
  mutate_if(is.character, str_trim)

varl <- varl %>% distinct() %>%
  mutate_if(is_all_numeric, as.numeric) %>%
  mutate(income_cv = income_sd/income_mean,
         maize_cv = maize_sd/maize_mean) %>%
  select(-income_mean, -maize_mean)

write_csv(varl, file = "./R_Output/vars_quicker.csv")
hist(varl$maize_cv, breaks = 100)
rundat <- left_join(rundat, varl, by = join_by(runID, hhid))
rundat$income_cv[rundat$income_sd == 0] <- 0
rundat$maize_cv[rundat$maize_sd == 0] <- 0
hist(rundat$maize_cv, breaks = 100, col = "yellowgreen")
table(rundat$Utility_calculation)
rm(varl)
gc()

toc()

tic() # This also takes ~3 hours
strl <- future_map(tickfiles, ~c(
  read_delim(.x) %>%
    select(runID, hhid, chosen_strategy) %>%
    mutate(chosen = ifelse(chosen_strategy %in% c("0 ", "NA ", "stick to current "), 0, 1)) %>%
    group_by(runID, hhid) %>%
    summarise(chosen = sum(chosen)) %>%
    ungroup() %>%
    mutate_if(is.character, str_trim) %>%
    distinct() %>%
    mutate_if(is_all_numeric,as.numeric)
))

strats <- bind_rows(strl)
write_csv(strats, file = "./R_Output/strats_quicker.csv")
rm(strl)
rundat <- left_join(rundat, strats)
rm(strats)
toc()

hist(rundat$maize_cv, breaks = 100, col = "yellow")
table(rundat$Utility_calculation)

# rundat$n_strats <- count_strats(rundat$known_strategies, strats = strats)
# hist(rundat$n_strats)


tic() # about 3 hours
strats <- c("stick to current", "abandon land", "change to pasture", "change to maize", "change to vegetable", "plant coffee", "buy cow", "buy feed",
            "buy fertiliser", "sell cow", "buy herbicides", "buy both fertiliser and herbicides")

inil <- future_map(tickfiles, ~c(
  read_delim(.x) %>%
    mutate(year = as.numeric(as.character(year))) %>%
    filter(year == 0) %>%
    mutate(n_strats = count_strats(known_strategies, strats = strats)) %>%
    select(runID, hhid, income_outcome_t1end, food_outcome_t1end, leisure_outcome_t1end, 
           income_threshold_t1start, food_threshold_t1start, leisure_threshold_t1start,
           maizeland, fallowland, vegetableland, privatepasture, coffeeland, n_strats) %>%
    rename(income_initial = income_outcome_t1end, food_initial = food_outcome_t1end, leisure_initial = leisure_outcome_t1end, 
           income_threshold_initial = income_threshold_t1start, food_threshold_initial = food_threshold_t1start, leisure_threshold_initial = leisure_threshold_t1start,
           maizeland_initial = maizeland, fallowland_initial = fallowland, vegland_initial = vegetableland, pasture_initial = privatepasture, coffeeland_initial = coffeeland) %>%
    mutate_if(is.character, str_trim) %>%
    distinct() %>%
    mutate_if(is_all_numeric,as.numeric)
  
))

initial <- bind_rows(inil)
write_csv(initial, file = "./R_Output/initial_quicker.csv")
rm(inil)

rundat <- left_join(rundat, initial)
write_csv(rundat, file = "./R_Output/rundat_quicker.csv")
toc()
hist(rundat$maize_cv, breaks = 100, col = "orange")
table(rundat$Utility_calculation)
rm(initial)

tic()
tickdat <- tickfiles %>%
  future_map_dfr( ~read_delim(.x) %>%
                    
                    mutate(year = as.numeric(as.character(year))) %>%
                    
                    # Look at outcomes at end of simulation
                    filter(year == 24) %>%
                    
                    select( 
                      
                      # Inputs
                      runID, hhid, #year, Strategy_order, Make_decisions, Utility_calculation, Threshold_type, Internal_external, income_on, food_on, leisure_on, Repetition, pop_region_woreda, 
                      
                      # Outputs            
                      income_outcome_t1end, income_threshold_t1start, food_outcome_t1end, food_threshold_t1start,
                      leisure_outcome_t1end, leisure_threshold_t1start, food_purchases, maizeproduction, costs_of_production,
                      earnings_from_sales, savings, `value_of_consumed-products`, maize_consumed, chosen_strategy, maize_0fert_0herb:coffee_3fert_1herb, 
                      maizeland:fallowland, maizeproduction, herd,
                      maizesales,vegetablesales,coffeesales,buttersales,oxensales,bullsales,cowsales,earnings_from_sales)) %>%
  
  # Make sure data has the right structure
  mutate_if(is.character, str_trim) %>%
  distinct() %>%
  mutate_if(is_all_numeric,as.numeric)

write_csv(tickdat, file = "./R_Output/tickdat_quicker.csv")

toc()

tic()
dat <- full_join(rundat, tickdat) %>% 
  mutate(pop_region_zone = paste0("p", Population, "_r", region, "_z", woreda)) %>%
  filter(scenario %in% ascens) # keep only sensible experiments (~remove duplicates)

hist(dat$maize_cv, breaks = 100, col = "red")
table(dat$Utility_calculation)

dat <- dat %>% 
  mutate(rowid = 1:nrow(dat)) %>% 
  group_by(hhid, runID) %>% 
  filter(rowid == max(rowid)) %>% 
  ungroup() # This is to remove earlier writes of the same run

write_csv(dat, file = "./R_Output/dat_quicker.csv")
hist(dat$maize_cv, breaks = 100, col = "pink")
table(dat$Utility_calculation)
#rm(list = c("tickdat", "rundat"))
toc()

tic()
dat <- dat %>% mutate_if(is.character, str_trim)

dat <- dat %>%  
  mutate(Utility_calculation = ifelse(Make_decisions == "random draw", "no utility", Utility_calculation), # outcomes will be the same whichever utility calculation is chosen, utility calculation is not used in random decision making
         Internal_external = ifelse(Threshold_type != "dynamic", "no adaptation", Internal_external), #If thresholds are not dynamic, they are not adapted one way or the other
         Strategy_order = ifelse(Make_decisions == "optimising", "order irrelevant", Strategy_order), #In optimising it does not matter which order you loop through strategies, the best one is chosen anyway
         #income_threshold_t1start = ifelse(Threshold_type == "infinite", NA, income_threshold_t1start), #Cannot compare this threshold numerically with the other thresholds
         #food_threshold_t1start = ifelse(Threshold_type == "infinite", NA, food_threshold_t1start),
         #leisure_threshold_t1start = ifelse(Threshold_type == "infinite", NA, leisure_threshold_t1start),
         #income_threshold_t1start = ifelse(!income_on, NA, income_threshold_t1start), #If the aspiration dimension is not driving decision-making
         #food_threshold_t1start = ifelse(!food_on, NA, food_threshold_t1start),
         #leisure_threshold_t1start = ifelse(!leisure_on, NA, leisure_threshold_t1start),
         n_dimensions = income_on + food_on + leisure_on,
         Utility_calculation = ifelse(n_dimensions <= 1, "no utility", Utility_calculation),
         Internal_external = ifelse(n_dimensions == 0, "no adaptation", Internal_external),
         Strategy_order = ifelse(n_dimensions == 0, "order irrelevant", Strategy_order),
         Threshold_type = ifelse(n_dimensions == 0, "thresholds irrelevant", Threshold_type),
         Make_decisions = ifelse(n_dimensions == 0, "decision making irrelevant", Make_decisions),
         decision_order = paste0(Make_decisions, "_", Strategy_order),
         threshold_adjustment = paste0(Threshold_type, "_", Internal_external))

dat$decision_order <- gsub("-", "_", dat$decision_order)
dat$threshold_adjustment <- gsub("-", "_", dat$threshold_adjustment)
dat$Utility_calculation <- gsub("-", "_", dat$Utility_calculation)

hist(dat$maize_cv, breaks = 100, col = "purple")
table(dat$Utility_calculation)

write_csv(dat, file = "./R_Output/dat_quicker.csv")

toc()

tic()
i_mean <- mean(dat$income_threshold_t1start, na.rm = TRUE)
food_mean <- mean(dat$food_threshold_t1start, na.rm = TRUE)
leisure_mean <- mean(dat$leisure_threshold_t1start, na.rm = TRUE)
i_sd <- sd(dat$income_threshold_t1start, na.rm = TRUE)
food_sd <- sd(dat$food_threshold_t1start, na.rm = TRUE)
leisure_sd <- sd(dat$leisure_threshold_t1start, na.rm = TRUE)


# Calculate aggregated 
dat <- dat %>%
  mutate(maize_loans = ifelse(food_purchases == 0 | savings > 0, 0, abs(savings_t1start) - abs(savings) - costs_of_production),
         maize_loans = abs(maize_loans),
         aspiration_index = 1/n_dimensions * ( 
           ifelse(is.na(income_threshold_t1start), 0, (income_threshold_t1start - i_mean) / i_sd) + 
             ifelse(is.na(food_threshold_t1start), 0, (food_threshold_t1start - food_mean) / food_sd) + 
             ifelse(is.na(leisure_threshold_t1start), 0, (leisure_threshold_t1start - leisure_mean) / leisure_sd )), # Calculation taken from Bernard 2014
         cash_lu = coffeeland + vegetableland,
         intensive_lu = maize_2fert_0herb + maize_2fert_1herb + maize_3fert_0herb + maize_3fert_1herb +           
           veg_2fert_0herb + veg_2fert_1herb + veg_3fert_0herb + veg_3fert_1herb + 
           coffee_2fert_0herb + coffee_2fert_1herb + coffee_3fert_0herb + coffee_3fert_1herb,
         asp_dims = paste0("I=", income_on, "_F=", food_on, "_L=", leisure_on),
         income_gini = ineq(income_outcome_t1end, type="Gini"),
         income_atk = ineq(income_outcome_t1end, type="Atkinson"),
         savings_gini = ineq(savings, type="Gini"),
         savings_atk = ineq(savings, type="Atkinson"),
         food_gini = ineq(food_outcome_t1end, type="Gini"),
         food_atk = ineq(food_outcome_t1end, type="Atkinson"),
         food2_gini = ineq(`value_of_consumed-products`, type="Gini"),
         food2_atk = ineq(`value_of_consumed-products`, type="Atkinson")
  )

dat$asp_dims <- gsub("FALSE", "F", dat$asp_dims)
dat$asp_dims <- gsub("TRUE", "T", dat$asp_dims)

hist(dat$maize_cv, breaks = 100, col = "blue")
table(dat$Utility_calculation)

toc()

write_csv(dat, file = "./R_Output/dat_quicker.csv")

rm(rundat)
rm(tickdat)
