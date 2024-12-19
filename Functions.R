# Data processing
scale_log <- function(x){ifelse(abs(x) != x, -log(abs(x) + 1), +log(x + 1))}

# Count known strategies
strats <- c("stick to current", "abandon land", "change to pasture", "change to maize", "change to vegetable", "plant coffee", "buy cow", "buy feed",
            "buy fertiliser", "sell cow", "buy herbicides", "buy both fertiliser and herbicides")

count_strats <- function(text, strats) {
  
  # Initialize the count
  strats_count <- 0
  
  # Remove brackets
  segment <- gsub("\\[|\\]", "", text)
    
  # Count occurrences of each target word in the segment
  for (word in strats) {
    # Count occurrences of the word or phrase
    strats_count <- strats_count + grepl(word, segment)
  }
  
  return(strats_count)
}

# rundat$n_strats <- count_strats(rundat$known_strategies, strats = strats)
# hist(rundat$n_strats)

processdatas <- function(dat){
  dat <- dat %>% filter(Threshold_type != "infinite", n_dimensions > 0) %>%
    mutate(b_no = ifelse(Threshold_type == "static", 0, b_no),
           optimism = ifelse(Threshold_type == "static", 1, optimism))
  
  dat$log_income_outcome_t1end <- scale_log(dat$income_outcome_t1end)
  dat$log_income_initial <- scale_log(dat$income_initial)
  dat$log_savings <- scale_log(dat$savings)
  dat$log_maizeloans<- scale_log(dat$maize_loans)
  dat$hhmembers <- as.numeric(dat$hhmembers) ; dat$farmland <- as.numeric(dat$farmland)
  
  
  dat <- dat %>% mutate(income_outcome_t1end = income_outcome_t1end/100000, #100k ETB
                        income_initial = income_initial/100000, #100k ETB
                        income_cv = income_cv/100000, #100k ETB
                        savings = savings/100000, #100k ETB
                        maize_loans = -maize_loans/(hhmembers*1000), #1k ETB per hh member
                        `value_of_consumed_products` = `value_of_consumed.products`/(hhmembers*1000),  #1k ETB per hh member 
                        maizeproduction = maizeproduction/1000, #tonnes
                        maize_cv = maize_cv/1000, #tonnes
                        lu_div = maize_0fert_0herb^2/farmland^2 + maize_1fert_0herb^2/farmland^2 + maize_2fert_0herb^2/farmland^2 + maize_3fert_0herb^2/farmland^2 + 
                          maize_1fert_1herb^2/farmland^2 + maize_2fert_1herb^2/farmland^2 + maize_3fert_1herb^2/farmland^2 + 
                          veg_0fert_0herb^2/farmland^2 + veg_1fert_0herb^2/farmland^2 + veg_2fert_0herb^2/farmland^2 + veg_3fert_0herb^2/farmland^2 + 
                          veg_1fert_1herb^2/farmland^2 + veg_2fert_1herb^2/farmland^2 + veg_3fert_1herb^2/farmland^2 +
                          coffee_0fert_0herb^2/farmland^2 + coffee_1fert_0herb^2/farmland^2 + coffee_2fert_0herb^2/farmland^2 + coffee_3fert_0herb^2/farmland^2 + 
                          coffee_1fert_1herb^2/farmland^2 + coffee_2fert_1herb^2/farmland^2 + coffee_3fert_1herb^2/farmland^2 +
                          privatepasture^2/farmland^2 + fallowland^2/farmland^2, # Land use diversity index, see Comer 2015
                        sales_div = maizesales^2/earnings_from_sales^2 + vegetablesales^2/earnings_from_sales^2 + coffeesales^2/earnings_from_sales^2 + 
                          buttersales^2/earnings_from_sales^2 + oxensales^2/earnings_from_sales^2 + bullsales^2/earnings_from_sales^2 + cowsales^2/earnings_from_sales^2
  )
  
  dat <- dat %>% group_by(runID) %>%
    mutate(income_gini = ineq(income_outcome_t1end, type="Gini"),
           income_atk = ineq(income_outcome_t1end, type="Atkinson"),
           income_var = ineq(income_outcome_t1end, type="var"),
           mean_income = mean(income_outcome_t1end),
           savings_gini = ineq(savings, type="Gini"),
           savings_atk = ineq(savings, type="Atkinson"),
           savings_var = ineq(income_outcome_t1end, type="var"),
           mean_savings = mean(savings),
           food_gini = ineq(food_outcome_t1end, type="Gini"),
           food_atk = ineq(food_outcome_t1end, type="Atkinson"),
           food_var = ineq(income_outcome_t1end, type="var"),
           food2_gini = ineq(`value_of_consumed_products`, type="Gini"),
           food2_atk = ineq(`value_of_consumed_products`, type="Atkinson"),
           food2_var = ineq(`value_of_consumed_products`, type="var"),
           mean_food2 = mean(`value_of_consumed_products`),
           mean_maizeproduction = mean(maizeproduction),
           mean_intensive_lu = sum(intensive_lu)/sum(farmland),
           mean_cash_lu = sum(cash_lu)/sum(farmland),
           totalherd = sum(herd),
           mean_deficit = mean(maize_loans),
           n_deficit = length(which(maize_loans < 0)),
           n_nocow = length(which(herd == 0)),
           n_indebted = length(which(savings < 0)),
           mean_lu_div = sum(maize_0fert_0herb)^2/sum(farmland)^2 + sum(maize_1fert_0herb)^2/sum(farmland)^2 + sum(maize_2fert_0herb)^2/sum(farmland)^2 + sum(maize_3fert_0herb)^2/sum(farmland)^2 + 
             sum(maize_1fert_1herb)^2/sum(farmland)^2 + sum(maize_2fert_1herb)^2/sum(farmland)^2 + sum(maize_3fert_1herb)^2/sum(farmland)^2 + 
             sum(veg_0fert_0herb)^2/sum(farmland)^2 + sum(veg_1fert_0herb)^2/sum(farmland)^2 + sum(veg_2fert_0herb)^2/sum(farmland)^2 + sum(veg_3fert_0herb)^2/sum(farmland)^2 + 
             sum(veg_1fert_1herb)^2/sum(farmland)^2 + sum(veg_2fert_1herb)^2/sum(farmland)^2 + sum(veg_3fert_1herb)^2/sum(farmland)^2 +
             sum(coffee_0fert_0herb)^2/sum(farmland)^2 + sum(coffee_1fert_0herb)^2/sum(farmland)^2 + sum(coffee_2fert_0herb)^2/sum(farmland)^2 + sum(coffee_3fert_0herb)^2/sum(farmland)^2 + 
             sum(coffee_1fert_1herb)^2/sum(farmland)^2 + sum(coffee_2fert_1herb)^2/sum(farmland)^2 + sum(coffee_3fert_1herb)^2/sum(farmland)^2 +
             sum(privatepasture)^2/sum(farmland)^2 + sum(fallowland)^2/sum(farmland)^2, # Land use diversity index, see Comer 2015
           mean_sales_div = sum(maizesales)^2/sum(earnings_from_sales)^2 + sum(vegetablesales)^2/sum(earnings_from_sales)^2 + sum(coffeesales)^2/sum(earnings_from_sales)^2 + 
             sum(buttersales)^2/sum(earnings_from_sales)^2 + sum(oxensales)^2/sum(earnings_from_sales)^2 + sum(bullsales)^2/sum(earnings_from_sales)^2 + sum(cowsales)^2/sum(earnings_from_sales)^2,
           farmland_run = sum(farmland)) %>% ungroup() %>%
    mutate(intensive_lu = intensive_lu/farmland,
           cash_lu = cash_lu/farmland,
           b_no = ifelse(Threshold_type == "static", NA, b_no),
           optimism = ifelse(Threshold_type == "static", NA, optimism),
           Utility_calculation = ifelse(Utility_calculation == "no utility", NA, Utility_calculation),
           maize_consumed = maize_consumed/hhmembers)
  
  ini <- dat %>% group_by(Repetition,
                   yield_mean, yield_sd, price_mean, price_sd, 
                   b_no, optimism) %>% 
    summarise(maizeland_initial_run = round(sum(maizeland_initial)/sum(farmland), digits = 2), 
              vegland_initial_run = round(sum(vegland_initial)/sum(farmland), digits = 2),   
              coffeeland_initial_run = round(sum(coffeeland_initial)/sum(farmland), digits = 2),   
              pasture_initial_run = round(sum(pasture_initial)/sum(farmland), digits = 2)) 
  
  dat <- full_join(dat, ini)
  
  
  return(dat)
  
}

# Regressions
makedatas <- function(fulldata, y, xes){
  
  # select variables
  testdata <- fulldata %>% select(all_of(c(y, xes))) %>% distinct()
  
  # turn categorical variables into dummies
  testdata <- testdata %>% 
    mutate(util_weight = Utility_calculation == "weighted_sum", # weighted sum = baseline,
           util_CBx = Utility_calculation == "cobb_douglas*", 
           util_CBt = Utility_calculation == "cobb_douglas+",
           util_CBlog = Utility_calculation == "cobb_douglasln",
           util_rank = Utility_calculation == "rank_sum",
           
           optimise = decision_order == "optimising_order irrelevant", # optimising = baseline,
           random_random = decision_order == "random draw_random",
           random_defined = decision_order == "random draw_defined_by_similarity",
           satisfice_defined = decision_order == "satisficing_defined_by_similarity",
           satisfice_random = decision_order == "satisficing_random",
           
           asp_Ioo = asp_dims == "I=T_F=F_L=F", 
           asp_oFo = asp_dims == "I=F_F=T_L=F", 
           asp_ooL = asp_dims == "I=F_F=F_L=T",
           asp_IFo = asp_dims == "I=T_F=T_L=F",
           asp_IoL = asp_dims == "I=T_F=F_L=T",
           asp_oFL = asp_dims == "I=F_F=T_L=T",
           asp_IFL = asp_dims == "I=T_F=T_L=T", # all three = baseline,
           
           stat = threshold_adjustment == "static_no adaptation",
           dyn_inter = threshold_adjustment == "dynamic_internal only", # internal only = baseline,
           dyn_exter = threshold_adjustment == "dynamic_external only",
           dyn_both = threshold_adjustment == "dynamic_internal and external") %>% 
    
    select(-Utility_calculation, -decision_order, -asp_dims, -threshold_adjustment) %>% # remove original categorical variables, use dummies
    mutate(across(where(is.numeric), ~ scale(.x))) # scale data
  
}

library(tidyselect)
is_all_numeric <- function(x) {!any(is.na(suppressWarnings(as.numeric(na.omit(x))))) & is.character(x)}

makedatas2 <- function(fulldata, y, xes){
  
  # select variables
  testdata <- fulldata %>% dplyr::select(all_of(c(y, xes))) %>% distinct() %>% mutate_if(is_all_numeric, as.numeric)
  
  # turn categorical variables into dummies
  testdata <- testdata %>% 
    mutate(a1_optimise = decision_order == "optimised selection_random", # optimising = baseline,
           a1_random_random = decision_order == "random draw_random",
           a1_random_defined = decision_order == "random draw_defined_by_similarity",
           a0_satisfice_defined = decision_order == "sequential loop_defined_by_similarity",
           a1_satisfice_random = decision_order == "sequential loop_random",
           
           b1_asp_Ioo = asp_dims == "I=T_F=F_L=F", 
           b1_asp_oFo = asp_dims == "I=F_F=T_L=F", 
           b1_asp_ooL = asp_dims == "I=F_F=F_L=T",
           b1_asp_IFo = asp_dims == "I=T_F=T_L=F",
           b1_asp_IoL = asp_dims == "I=T_F=F_L=T",
           b1_asp_oFL = asp_dims == "I=F_F=T_L=T",
           b0_asp_IFL = asp_dims == "I=T_F=T_L=T", # all three = baseline,
           
           c1_stat = threshold_adjustment == "static_no adaptation",
           c1_dyn_inter = threshold_adjustment == "dynamic_internal only", # internal only = baseline,
           c1_dyn_exter = threshold_adjustment == "dynamic_external only",
           c0_dyn_both = threshold_adjustment == "dynamic_internal and external") %>% 
    
    dplyr::select(-decision_order, -asp_dims, -threshold_adjustment) %>% # remove original categorical variables, use dummies
    mutate(across(where(is.numeric), ~ scale(.x))) # scale data
  
}

# Create a function to construct the sparse block diagonal covariance matrix
create_sparse_cov_matrix_block <- function(n_list, cov_matrix) {
  total_length <- sum(n_list)
  
  row_indices <- c()
  col_indices <- c()
  values <- c()
  
  start_i <- 1
  for (i in seq_along(n_list)) {
    end_i <- start_i + n_list[i] - 1
    start_j <- 1
    for (j in seq_along(n_list)) {
      end_j <- start_j + n_list[j] - 1
      
      block_value <- cov_matrix[i, j]
      # Only add non-zero blocks
      if (block_value != 0) {
        # Create row and column indices for the block
        row_indices <- c(row_indices, rep(start_i:end_i, each = n_list[j]))
        col_indices <- c(col_indices, rep(start_j:end_j, times = n_list[i]))
        values <- c(values, rep(block_value, n_list[i] * n_list[j]))
      }
      
      start_j <- end_j + 1
    }
    start_i <- end_i + 1
  }
  
  # Construct sparse matrix
  sparse_cov_matrix <- sparseMatrix(
    i = row_indices,
    j = col_indices,
    x = values,
    dims = c(total_length, total_length)
  )
  
  return(sparse_cov_matrix)
}

# Define the function to create sparse block diagonal covariance matrix for a subset of blocks
create_sparse_cov_matrix_block_parallel <- function(n_list, cov_matrix, block_indices) {
  row_indices <- c()
  col_indices <- c()
  values <- c()
  
  for (block in block_indices) {
    block <- unlist(block)
    i <- block[1]
    j <- block[2]
    
    start_i <- ifelse(i == 1, 0, sum(n_list[1:(i-1)]) + 1)
    end_i <- start_i + n_list[i] 
    start_j <- ifelse(j == 1, 0, sum(n_list[1:(j-1)]) + 1)
    end_j <- start_j + n_list[j]
    
    block_value <- cov_matrix[i, j]
    # Only add non-zero blocks
    if (block_value != 0) {
      # Create row and column indices for the block
      row_indices <- c(row_indices, rep(start_i:end_i, each = n_list[j]))
      col_indices <- c(col_indices, rep(start_j:end_j, times = n_list[i]))
      values <- c(values, rep(block_value, n_list[i] * n_list[j]))
    }
  }
  
  # Construct sparse matrix for this subset of blocks
  sparse_cov_matrix <- sparseMatrix(
    i = row_indices,
    j = col_indices,
    x = values,
    dims = c(sum(n_list), sum(n_list))
  )
  
  return(sparse_cov_matrix)
}

# Function to process blocks in smaller chunks
process_in_chunks <- function(block_indices, chunk_size) {
  num_blocks <- length(block_indices)
  chunk_indices <- split(1:num_blocks, ceiling(seq_along(1:num_blocks) / chunk_size))
  sparse_cov_matrix_list <- list()
  
  for (chunk in chunk_indices) {
    cl <- makeCluster(num_cores)
    clusterExport(cl, c("n_list", "cov_matrix", "create_sparse_cov_matrix_block_parallel"))
    clusterEvalQ(cl, library(Matrix))
    
    chunk_blocks <- block_indices[chunk]
    chunk_sparse_cov_matrix_list <- parLapply(cl, chunk_blocks, function(block) {
      create_sparse_cov_matrix_block_parallel(n_list, cov_matrix, list(block))
    })
    
    stopCluster(cl)
    sparse_cov_matrix_list <- c(sparse_cov_matrix_list, chunk_sparse_cov_matrix_list)
  }
  
  return(sparse_cov_matrix_list)
}

# Debug function to check and print indices
check_indices <- function(n_list, block_indices) {
  for (block in block_indices) {
    block <- unlist(block)
    i <- block[1]
    j <- block[2]
    
    start_i <- ifelse(i == 1, 0, sum(n_list[1:(i-1)]) + 1)
    end_i <- start_i + n_list[i] 
    start_j <- ifelse(j == 1, 0, sum(n_list[1:(j-1)]) + 1)
    end_j <- start_j + n_list[j]
    
    cat("Block: (", i, ", ", j, ") => Indices: [", start_i, "-", end_i, "] x [", start_j, "-", end_j, "]\n")
  }
}


full_regression <- function(b){
  
  # Structural
  bdec1 <- b[1]; bdec2 <- b[2]; bdec3 <- b[3]; bdec4 <- b[4]; bdec5 <- b[5]
  basp1 <- b[6]; basp2 <- b[7]; basp3 <- b[8]; basp4 <- b[9]; basp5 <- b[10]; basp6 <- b[11]; basp7 <- b[12]
  badj1 <- b[13]; badj2 <- b[14]; badj3 <- b[15]; badj4 <- b[16];
  buti1 <- b[17]; buti2 <- b[18]; buti3 <- b[19]; buti4 <- b[20]; buti5 <- b[21]
  
  b_dec <- c(bdec1, bdec2, bdec3, bdec4, bdec5)
  b_asp <- c(basp1, basp2, basp3, basp4, basp5, basp6, basp7)
  b_adj <- c(badj1, badj2, badj3, badj4)
  b_uti <- c(buti1, buti2, buti3, buti4, buti5)
  
  # Global
  bb <- b[22]; bopt <- b[23]; bpm <- b[24]; bpsd <- b[25]; 
  bym <- b[26]; bysd <- b[27]; bml <- b[28]
  
  b_global <- c(bb, bopt, bpm, bpsd, bym, bysd, bml)
  
  # Household
  bfarm <- b[29]; bhh <- b[30]; bfld <- b[31]; binc <- b[32]; bfood <- b[33]; 
  blsr <- b[34]; brii <- b[35]
  
  b_household <- c(bfarm, bhh, bfld, binc, bfood, blsr, brii)
  
  # Model A
  Adec <- as.matrix(cbind(A$random_random, A$random_defined, A$satisfice_random, A$satisfice_defined, A$optimise))
  Aasp <- as.matrix(cbind(A$asp_Ioo, A$asp_oFo, A$asp_ooL, A$asp_IFo, A$asp_IoL, A$asp_oFL, A$asp_IFL))
  Aadj <- as.matrix(cbind(A$stat, A$dyn_inter, A$dyn_exter, A$dyn_both))
  #drop utility calculations
  Aglobal <- as.matrix(cbind(A$price_mean, A$price_sd, A$yield_mean, A$yield_sd, A$maizeland_initial_run)) #drop b and optimism
  Ahousehold <- as.matrix(cbind(A$farmland, A$hhmembers, A$fields, A$log_income_initial, A$food_initial, A$leisure_initial, A$relativeimportanceincome))
  
  ya <- rowSums(b_dec*Adec) + rowSums(b_asp*Aasp) + rowSums(b_adj*Aadj) + rowSums(b_global[3:7]*Aglobal) + rowSums(b_household*Ahousehold)
  aobs <- t(A[,which(colnames(A) == y)])
  amean <- mean(aobs, na.rm = TRUE)
  
  Bdec <- as.matrix(cbind(B$satisfice_random, B$satisfice_defined, B$optimise)) # drop random random and random defined
  Basp <- as.matrix(cbind(B$asp_IFo, B$asp_IoL, B$asp_oFL, B$asp_IFL)) # drop Ioo, oFo and ooL
  Badj <- as.matrix(cbind(B$stat, B$dyn_inter, B$dyn_exter, B$dyn_both))
  Buti <- as.matrix(cbind(B$util_weight, B$util_rank, B$util_CBt, B$util_CBx, B$util_CBlog))
  Bglobal <- as.matrix(cbind(B$price_mean, B$price_sd, B$yield_mean, B$yield_sd, B$maizeland_initial_run)) #drop b and optimism
  Bhousehold <- as.matrix(cbind(B$farmland, B$hhmembers, B$fields, B$log_income_initial, B$food_initial, B$leisure_initial, B$relativeimportanceincome))
  
  yb <- rowSums(b_dec[3:5]*Bdec) + rowSums(b_asp[4:7]*Basp) + rowSums(b_adj*Badj) + rowSums(b_uti*Buti) + rowSums(b_global[3:7]*Bglobal) + rowSums(b_household*Bhousehold)
  bobs <- t(B[,which(colnames(B) == y)])
  bmean <- mean(bobs, na.rm = TRUE)
  
  Cdec <- as.matrix(cbind(C$random_random, C$random_defined, C$satisfice_random, C$satisfice_defined, C$optimise))
  Casp <- as.matrix(cbind(C$asp_Ioo, C$asp_oFo, C$asp_ooL, C$asp_IFo, C$asp_IoL, C$asp_oFL, C$asp_IFL))
  Cadj <- as.matrix(cbind(C$dyn_inter, C$dyn_exter, C$dyn_both)) # drop static
  #drop utility calculations
  Cglobal <- as.matrix(cbind(C$b_no, C$optimism, C$price_mean, C$price_sd, C$yield_mean, C$yield_sd, C$maizeland_initial_run))
  Chousehold <- as.matrix(cbind(C$farmland, C$hhmembers, C$fields, C$log_income_initial, C$food_initial, C$leisure_initial, C$relativeimportanceincome))
  
  yc <- rowSums(b_dec*Cdec) + rowSums(b_asp*Casp) + rowSums(b_adj[2:4]*Cadj) + rowSums(b_global*Cglobal) + rowSums(b_household*Chousehold)
  cobs <- t(C[,which(colnames(C) == y)])
  cmean <- mean(cobs, na.rm = TRUE)
  
  Ddec <- as.matrix(cbind(D$satisfice_random, D$satisfice_defined, D$optimise)) # drop random random and random defined
  Dasp <- as.matrix(cbind(D$asp_IFo, D$asp_IoL, D$asp_oFL, D$asp_IFL)) # drop Ioo, oFo and ooL
  Dadj <- as.matrix(cbind(D$dyn_inter, D$dyn_exter, D$dyn_both)) # drop static
  Duti <- as.matrix(cbind(D$util_weight, D$util_rank, D$util_CBt, D$util_CBx, D$util_CBlog))
  Dglobal <- as.matrix(cbind(D$b_no, D$optimism, D$price_mean, D$price_sd, D$yield_mean, D$yield_sd, D$maizeland_initial_run))
  Dhousehold <- as.matrix(cbind(D$farmland, D$hhmembers, D$fields, D$log_income_initial, D$food_initial, D$leisure_initial, D$relativeimportanceincome))
  
  yd <- rowSums(b_dec[3:5]*Ddec) + rowSums(b_asp[4:7]*Dasp) + rowSums(b_adj[2:4]*Dadj) + rowSums(b_uti*Duti) + rowSums(b_global*Dglobal) + rowSums(b_household*Dhousehold)
  dobs <- t(D[,which(colnames(D) == y)])
  dmean <- mean(dobs, na.rm = TRUE)
  
  SSRa <- sum((ya - A[,which(colnames(A) == y)])^2, na.rm = TRUE) ; wa <- (nrow(A) - nrow(B) - nrow(C) + nrow(D)) / nrow(A) ; 
  TSSa <- sum((aobs - amean)^2, na.rm  = TRUE); Ra <- 1 - (SSRa/TSSa)
  
  SSRb <- sum((yb - B[,which(colnames(B) == y)])^2, na.rm = TRUE) ; wb <- (nrow(B) - nrow(D)) / nrow(A); 
  TSSb <- sum((bobs - bmean)^2, na.rm  = TRUE); Rb <- 1 - (SSRb/TSSb)
  
  SSRc <- sum((yc - C[,which(colnames(C) == y)])^2, na.rm = TRUE) ; wc <- (nrow(C) - nrow(D)) / nrow(A); 
  TSSc <- sum((cobs - cmean)^2, na.rm  = TRUE); Rc <- 1 - (SSRc/TSSc)
  
  SSRd <- sum((yd - D[,which(colnames(D) == y)])^2, na.rm = TRUE) ; wd <- nrow(D)/nrow(A) ; 
  TSSd <- sum((dobs - dmean)^2, na.rm  = TRUE); Rd <- 1 - (SSRd/TSSd)
  
  #SSRy <- SSRa*wa + SSRb*wb + SSRc*wc + SSRd*wd
  Ry <- Ra*wa + Rb*wb + Rc*wc + Rd*wd 
  
  return(-Ry) # Miminise negative R^2 = maximise R^2
}

drop1_regression <- function(b, dropme){
  
  parnames <- c(rep("decision_order", 5), rep("asp_dims", 7), rep("threshold_adjustment", 4), rep("Utility_calculation", 5), #repeated number of categories: 5, 7, 4, 5
                "b_no", "optimism", "price_mean", "price_sd", "yield_mean", "yield_sd", "maizeland_initial_run",
                "farmland", "hhmembers", "fields", "income_initial_t1end", "food_initial_t1end", "leisure_initial_t1end", "relativeimportanceincome")
  
  drop_index <- which(dropme == parnames)
  b_indexes <- 1:35
  i <- b_indexes - ifelse(b_indexes > max(drop_index), b_indexes - length(drop_index), b_indexes)
  
  # Structural
  if(dropme != "decision_order"){ bdec1 <- b[1]; bdec2 <- b[2]; bdec3 <- b[3]; bdec4 <- b[4]; bdec5 <- b[5]
  }else{ bdec1 <- 0; bdec2 <- 0; bdec3 <- 0; bdec4 <- 0; bdec5 <- 0 }
  if(dropme != "asp_dims"){ basp1 <- b[i[6]]; basp2 <- b[i[7]]; basp3 <- b[i[8]]; basp4 <- b[i[9]]; basp5 <- b[i[10]]; basp6 <- b[i[11]]; basp7 <- b[i[12]]
  }else{ basp1 <- 0; basp2 <- 0; basp3 <- 0; basp4 <- 0; basp5 <- 0; basp6 <- 0; basp7 <- 0 }
  if(dropme != "threshold_adjust"){ badj1 <- b[i[13]]; badj2 <- b[i[14]]; badj3 <- b[i[15]]; badj4 <- b[i[16]];
  }else{ badj1 <- 0; badj2 <- 0; badj3 <- 0; badj4 <- 0 }
  if(dropme != "Utility_calculation"){ buti1 <- b[i[17]]; buti2 <- b[i[18]]; buti3 <- b[i[19]]; buti4 <- b[i[20]]; buti5 <- b[i[21]]
  }else{ buti1 <- 0; buti2 <- 0; buti3 <- 0; buti4 <- 0; buti5 <- 0 }
  
  b_dec <- c(bdec1, bdec2, bdec3, bdec4, bdec5)
  b_asp <- c(basp1, basp2, basp3, basp4, basp5, basp6, basp7)
  b_adj <- c(badj1, badj2, badj3, badj4)
  b_uti <- c(buti1, buti2, buti3, buti4, buti5)
  
  # Global
  bb <- ifelse(dropme == "b_no", b[i[22]], 0); bopt <- ifelse(dropme == "optimism", b[i[23]], 0);
  bpm <- ifelse(dropme == "price_mean", b[i[24]], 0); bpsd <- ifelse(dropme == "price_sd", b[i[25]], 0); 
  bym <- ifelse(dropme == "yield_mean", b[i[26]], 0); bysd <- ifelse(dropme == "yield_sd", b[i[27]], 0); 
  bml <- ifelse(dropme == "maizeland_initial_run", b[i[28]], 0)
  
  b_global <- c(bb, bopt, bpm, bpsd, bym, bysd, bml)
  
  # Household
  bfarm <- ifelse(dropme == "farmland", b[i[29]], 0); bhh <- ifelse(dropme == "hhmembers", b[i[30]], 0); bfld <- ifelse(dropme == "fields", b[i[31]], 0)
  binc <- ifelse(dropme == "log_income_initial", b[i[32]], 0); bfood <- ifelse(dropme == "food_initial", b[i[33]], 0)
  blsr <- ifelse(dropme == "leisure_initial", b[i[34]], 0); brii <- ifelse(dropme == "relativeimportanceincome", b[i[35]], 0)
  
  b_household <- c(bfarm, bhh, bfld, binc, bfood, blsr, brii)
  
  # Model A
  Adec <- as.matrix(cbind(A$random_random, A$random_defined, A$satisfice_random, A$satisfice_defined, A$optimise))
  Aasp <- as.matrix(cbind(A$asp_Ioo, A$asp_oFo, A$asp_ooL, A$asp_IFo, A$asp_IoL, A$asp_oFL, A$asp_IFL))
  Aadj <- as.matrix(cbind(A$stat, A$dyn_inter, A$dyn_exter, A$dyn_both))
  #drop utility calculations
  Aglobal <- as.matrix(cbind(A$price_mean, A$price_sd, A$yield_mean, A$yield_sd, A$maizeland_initial_run)) #drop b and optimism
  Ahousehold <- as.matrix(cbind(A$farmland, A$hhmembers, A$fields, A$log_income_initial, A$food_initial, A$leisure_initial, A$relativeimportanceincome))
  
  ya <- rowSums(b_dec*Adec) + rowSums(b_asp*Aasp) + rowSums(b_adj*Aadj) + rowSums(b_global[3:7]*Aglobal) + rowSums(b_household*Ahousehold)
  aobs <- t(A[,which(colnames(A) == y)])
  amean <- mean(aobs, na.rm = TRUE)
  
  Bdec <- as.matrix(cbind(B$satisfice_random, B$satisfice_defined, B$optimise)) # drop random random and random defined
  Basp <- as.matrix(cbind(B$asp_IFo, B$asp_IoL, B$asp_oFL, B$asp_IFL)) # drop Ioo, oFo and ooL
  Badj <- as.matrix(cbind(B$stat, B$dyn_inter, B$dyn_exter, B$dyn_both))
  Buti <- as.matrix(cbind(B$util_weight, B$util_rank, B$util_CBt, B$util_CBx, B$util_CBlog))
  Bglobal <- as.matrix(cbind(B$price_mean, B$price_sd, B$yield_mean, B$yield_sd, B$maizeland_initial_run)) #drop b and optimism
  Bhousehold <- as.matrix(cbind(B$farmland, B$hhmembers, B$fields, B$log_income_initial, B$food_initial, B$leisure_initial, B$relativeimportanceincome))
  
  yb <- rowSums(b_dec[3:5]*Bdec) + rowSums(b_asp[4:7]*Basp) + rowSums(b_adj*Badj) + rowSums(b_uti*Buti) + rowSums(b_global[3:7]*Bglobal) + rowSums(b_household*Bhousehold)
  bobs <- t(B[,which(colnames(B) == y)])
  bmean <- mean(bobs, na.rm = TRUE)
  
  Cdec <- as.matrix(cbind(C$random_random, C$random_defined, C$satisfice_random, C$satisfice_defined, C$optimise))
  Casp <- as.matrix(cbind(C$asp_Ioo, C$asp_oFo, C$asp_ooL, C$asp_IFo, C$asp_IoL, C$asp_oFL, C$asp_IFL))
  Cadj <- as.matrix(cbind(C$dyn_inter, C$dyn_exter, C$dyn_both)) # drop static
  #drop utility calculations
  Cglobal <- as.matrix(cbind(C$b_no, C$optimism, C$price_mean, C$price_sd, C$yield_mean, C$yield_sd, C$maizeland_initial_run))
  Chousehold <- as.matrix(cbind(C$farmland, C$hhmembers, C$fields, C$log_income_initial, C$food_initial, C$leisure_initial, C$relativeimportanceincome))
  
  yc <- rowSums(b_dec*Cdec) + rowSums(b_asp*Casp) + rowSums(b_adj[2:4]*Cadj) + rowSums(b_global*Cglobal) + rowSums(b_household*Chousehold)
  cobs <- t(C[,which(colnames(C) == y)])
  cmean <- mean(cobs, na.rm = TRUE)
  
  Ddec <- as.matrix(cbind(D$satisfice_random, D$satisfice_defined, D$optimise)) # drop random random and random defined
  Dasp <- as.matrix(cbind(D$asp_IFo, D$asp_IoL, D$asp_oFL, D$asp_IFL)) # drop Ioo, oFo and ooL
  Dadj <- as.matrix(cbind(D$dyn_inter, D$dyn_exter, D$dyn_both)) # drop static
  Duti <- as.matrix(cbind(D$util_weight, D$util_rank, D$util_CBt, D$util_CBx, D$util_CBlog))
  Dglobal <- as.matrix(cbind(D$b_no, D$optimism, D$price_mean, D$price_sd, D$yield_mean, D$yield_sd, D$maizeland_initial_run))
  Dhousehold <- as.matrix(cbind(D$farmland, D$hhmembers, D$fields, D$log_income_initial, D$food_initial, D$leisure_initial, D$relativeimportanceincome))
  
  yd <- rowSums(b_dec[3:5]*Ddec) + rowSums(b_asp[4:7]*Dasp) + rowSums(b_adj[2:4]*Dadj) + rowSums(b_uti*Duti) + rowSums(b_global*Dglobal) + rowSums(b_household*Dhousehold)
  dobs <- t(D[,which(colnames(D) == y)])
  dmean <- mean(dobs, na.rm = TRUE)
  
  SSRa <- sum((ya - A[,which(colnames(A) == y)])^2, na.rm = TRUE) ; wa <- (nrow(A) - nrow(B) - nrow(C) + nrow(D)) / nrow(A) ; 
  TSSa <- sum((aobs - amean)^2, na.rm  = TRUE); Ra <- 1 - (SSRa/TSSa)
  
  SSRb <- sum((yb - B[,which(colnames(B) == y)])^2, na.rm = TRUE) ; wb <- (nrow(B) - nrow(D)) / nrow(A); 
  TSSb <- sum((bobs - bmean)^2, na.rm  = TRUE); Rb <- 1 - (SSRb/TSSb)
  
  SSRc <- sum((yc - C[,which(colnames(C) == y)])^2, na.rm = TRUE) ; wc <- (nrow(C) - nrow(D)) / nrow(A); 
  TSSc <- sum((cobs - cmean)^2, na.rm  = TRUE); Rc <- 1 - (SSRc/TSSc)
  
  SSRd <- sum((yd - D[,which(colnames(D) == y)])^2, na.rm = TRUE) ; wd <- nrow(D)/nrow(A) ; 
  TSSd <- sum((dobs - dmean)^2, na.rm  = TRUE); Rd <- 1 - (SSRd/TSSd)
  
  #SSRy <- SSRa*wa + SSRb*wb + SSRc*wc + SSRd*wd
  Ry <- Ra*wa + Rb*wb + Rc*wc + Rd*wd 
  
  return(-Ry) # Miminise negative R^2 = maximise R^2
  
}

get_starting <- function(dropme = "none", addme = "none", fullmodel){
  
  parnames <- c(rep("decision_order", 5), rep("asp_dims", 7), rep("threshold_adjustment", 4), rep("Utility_calculation", 5), #repeated number of categories: 5, 7, 4, 5
                "b_no", "optimism", "price_mean", "price_sd", "yield_mean", "yield_sd", "maizeland_initial_run",
                "farmland", "hhmembers", "fields", "income_initial_t1end", "food_initial_t1end", "leisure_initial_t1end", "relativeimportanceincome")
  
  coefnames <- c("do:random_random", "do:random_defined", "do:satisfice_random", "do:satisfice_defined", "do:optimise",
                 "asp:Ioo", "asp:oFo", "asp:ooL", "asp:IFo", "asp:IoL", "asp:oFL", "asp:IFL",
                 "ta:stat", "ta:dyn_inter", "ta:dyn_exter", "ta:dyn_both", 
                 "condition 1 --> util:weighted", "condition 1 --> util:rank", "condition 1 --> util_CBx", "condition 1 --> util:CBt", "condition 1 --> util:CBlog",  
                 "condition 2 --> b", "condition 2 --> optimism",
                 "price_mean", "price_sd", 
                 "yield_mean", "yield_sd", "maizeland_initial_run", 
                 "farmland", "hhmembers", "fields", "log_income_initial", 
                 "food_initial", "leisure_initial", "relativeimportanceincome")
  
  if(!dropme %in% parnames & !addme %in% parnames){
    warning(paste("specify which variable to add or drop, one of:", paste(parnames, collapse = " ")))
  }
  
  if(dropme %in% parnames & addme %in% parnames){
    warning("cannot drop one and add one at the same time")}
  
  if(dropme %in% parnames){
    keep <- which(dropme != parnames)}
  
  if(addme %in% parnames){
    keep <- which(addme == parnames)}
  
  starting <- fullmodel$par[keep]
  names(starting) <- coefnames[keep]
  return(starting)
  
}

add1_regression <- function(b, addme){
  
  # Structural
  if(addme == "decision_order"){ bdec1 <- b[1]; bdec2 <- b[2]; bdec3 <- b[3]; bdec4 <- b[4]; bdec5 <- b[5]
  }else{ bdec1 <- 0; bdec2 <- 0; bdec3 <- 0; bdec4 <- 0; bdec5 <- 0 }
  if(addme == "asp_dims"){basp1 <- b[1]; basp2 <- b[2]; basp3 <- b[3]; basp4 <- b[4]; basp5 <- b[5]; basp6 <- b[6]; basp7 <- b[7]
  }else{ basp1 <- 0; basp2 <- 0; basp3 <- 0; basp4 <- 0; basp5 <- 0; basp6 <- 0; basp7 <- 0 }
  if(addme == "threshold_adjust"){ badj1 <- b[1]; badj2 <- b[2]; badj3 <- b[3]; badj4 <- b[4];
  }else{ badj1 <- 0; badj2 <- 0; badj3 <- 0; badj4 <- 0 }
  if(addme == "Utility_calculation"){ buti1 <- b[1]; buti2 <- b[2]; buti3 <- b[3]; buti4 <- b[4]; buti5 <- b[5]
  }else{ buti1 <- 0; buti2 <- 0; buti3 <- 0; buti4 <- 0; buti5 <- 0 }
  
  b_dec <- c(bdec1, bdec2, bdec3, bdec4, bdec5)
  b_asp <- c(basp1, basp2, basp3, basp4, basp5, basp6, basp7)
  b_adj <- c(badj1, badj2, badj3, badj4)
  b_uti <- c(buti1, buti2, buti3, buti4, buti5)
  
  # Global
  bb <- ifelse(addme == "b_no", b, 0); bopt <- ifelse(addme == "optimism", b, 0);
  bpm <- ifelse(addme == "price_mean", b, 0); bpsd <- ifelse(addme == "price_sd", b, 0); 
  bym <- ifelse(addme == "yield_mean", b, 0); bysd <- ifelse(addme == "yield_sd", b, 0); 
  bml <- ifelse(addme == "maizeland_initial_run", b, 0)
  
  b_global <- c(bb, bopt, bpm, bpsd, bym, bysd, bml)
  
  # Household
  bfarm <- ifelse(addme == "farmland", b, 0); bhh <- ifelse(addme == "hhmembers", b, 0); bfld <- ifelse(addme == "fields", b, 0)
  binc <- ifelse(addme == "log_income_initial", b, 0); bfood <- ifelse(addme == "food_initial", b, 0)
  blsr <- ifelse(addme == "leisure_initial", b, 0); brii <- ifelse(addme == "relativeimportanceincome", b, 0)
  
  b_household <- c(bfarm, bhh, bfld, binc, bfood, blsr, brii)
  
  # Model A
  Adec <- as.matrix(cbind(A$random_random, A$random_defined, A$satisfice_random, A$satisfice_defined, A$optimise))
  Aasp <- as.matrix(cbind(A$asp_Ioo, A$asp_oFo, A$asp_ooL, A$asp_IFo, A$asp_IoL, A$asp_oFL, A$asp_IFL))
  Aadj <- as.matrix(cbind(A$stat, A$dyn_inter, A$dyn_exter, A$dyn_both))
  #drop utility calculations
  Aglobal <- as.matrix(cbind(A$price_mean, A$price_sd, A$yield_mean, A$yield_sd, A$maizeland_initial_run)) #drop b and optimism
  Ahousehold <- as.matrix(cbind(A$farmland, A$hhmembers, A$fields, A$log_income_initial, A$food_initial, A$leisure_initial, A$relativeimportanceincome))
  
  ya <- rowSums(b_dec*Adec) + rowSums(b_asp*Aasp) + rowSums(b_adj*Aadj) + rowSums(b_global[3:7]*Aglobal) + rowSums(b_household*Ahousehold)
  aobs <- t(A[,which(colnames(A) == y)])
  amean <- mean(aobs, na.rm = TRUE)
  
  Bdec <- as.matrix(cbind(B$satisfice_random, B$satisfice_defined, B$optimise)) # drop random random and random defined
  Basp <- as.matrix(cbind(B$asp_IFo, B$asp_IoL, B$asp_oFL, B$asp_IFL)) # drop Ioo, oFo and ooL
  Badj <- as.matrix(cbind(B$stat, B$dyn_inter, B$dyn_exter, B$dyn_both))
  Buti <- as.matrix(cbind(B$util_weight, B$util_rank, B$util_CBt, B$util_CBx, B$util_CBlog))
  Bglobal <- as.matrix(cbind(B$price_mean, B$price_sd, B$yield_mean, B$yield_sd, B$maizeland_initial_run)) #drop b and optimism
  Bhousehold <- as.matrix(cbind(B$farmland, B$hhmembers, B$fields, B$log_income_initial, B$food_initial, B$leisure_initial, B$relativeimportanceincome))
  
  yb <- rowSums(b_dec[3:5]*Bdec) + rowSums(b_asp[4:7]*Basp) + rowSums(b_adj*Badj) + rowSums(b_uti*Buti) + rowSums(b_global[3:7]*Bglobal) + rowSums(b_household*Bhousehold)
  bobs <- t(B[,which(colnames(B) == y)])
  bmean <- mean(bobs, na.rm = TRUE)
  
  Cdec <- as.matrix(cbind(C$random_random, C$random_defined, C$satisfice_random, C$satisfice_defined, C$optimise))
  Casp <- as.matrix(cbind(C$asp_Ioo, C$asp_oFo, C$asp_ooL, C$asp_IFo, C$asp_IoL, C$asp_oFL, C$asp_IFL))
  Cadj <- as.matrix(cbind(C$dyn_inter, C$dyn_exter, C$dyn_both)) # drop static
  #drop utility calculations
  Cglobal <- as.matrix(cbind(C$b_no, C$optimism, C$price_mean, C$price_sd, C$yield_mean, C$yield_sd, C$maizeland_initial_run))
  Chousehold <- as.matrix(cbind(C$farmland, C$hhmembers, C$fields, C$log_income_initial, C$food_initial, C$leisure_initial, C$relativeimportanceincome))
  
  yc <- rowSums(b_dec*Cdec) + rowSums(b_asp*Casp) + rowSums(b_adj[2:4]*Cadj) + rowSums(b_global*Cglobal) + rowSums(b_household*Chousehold)
  cobs <- t(C[,which(colnames(C) == y)])
  cmean <- mean(cobs, na.rm = TRUE)
  
  Ddec <- as.matrix(cbind(D$satisfice_random, D$satisfice_defined, D$optimise)) # drop random random and random defined
  Dasp <- as.matrix(cbind(D$asp_IFo, D$asp_IoL, D$asp_oFL, D$asp_IFL)) # drop Ioo, oFo and ooL
  Dadj <- as.matrix(cbind(D$dyn_inter, D$dyn_exter, D$dyn_both)) # drop static
  Duti <- as.matrix(cbind(D$util_weight, D$util_rank, D$util_CBt, D$util_CBx, D$util_CBlog))
  Dglobal <- as.matrix(cbind(D$b_no, D$optimism, D$price_mean, D$price_sd, D$yield_mean, D$yield_sd, D$maizeland_initial_run))
  Dhousehold <- as.matrix(cbind(D$farmland, D$hhmembers, D$fields, D$log_income_initial, D$food_initial, D$leisure_initial, D$relativeimportanceincome))
  
  yd <- rowSums(b_dec[3:5]*Ddec) + rowSums(b_asp[4:7]*Dasp) + rowSums(b_adj[2:4]*Dadj) + rowSums(b_uti*Duti) + rowSums(b_global*Dglobal) + rowSums(b_household*Dhousehold)
  dobs <- t(D[,which(colnames(D) == y)])
  dmean <- mean(dobs, na.rm = TRUE)
  
  SSRa <- sum((ya - A[,which(colnames(A) == y)])^2, na.rm = TRUE) ; wa <- (nrow(A) - nrow(B) - nrow(C) + nrow(D)) / nrow(A) ; 
  TSSa <- sum((aobs - amean)^2, na.rm  = TRUE); Ra <- 1 - (SSRa/TSSa)
  
  SSRb <- sum((yb - B[,which(colnames(B) == y)])^2, na.rm = TRUE) ; wb <- (nrow(B) - nrow(D)) / nrow(A); 
  TSSb <- sum((bobs - bmean)^2, na.rm  = TRUE); Rb <- 1 - (SSRb/TSSb)
  
  SSRc <- sum((yc - C[,which(colnames(C) == y)])^2, na.rm = TRUE) ; wc <- (nrow(C) - nrow(D)) / nrow(A); 
  TSSc <- sum((cobs - cmean)^2, na.rm  = TRUE); Rc <- 1 - (SSRc/TSSc)
  
  SSRd <- sum((yd - D[,which(colnames(D) == y)])^2, na.rm = TRUE) ; wd <- nrow(D)/nrow(A) ; 
  TSSd <- sum((dobs - dmean)^2, na.rm  = TRUE); Rd <- 1 - (SSRd/TSSd)
  
  #SSRy <- SSRa*wa + SSRb*wb + SSRc*wc + SSRd*wd
  Ry <- Ra*wa + Rb*wb + Rc*wc + Rd*wd 
  
  return(-Ry) # Miminise negative R^2 = maximise R^2
  
}

# ols
ols_fit <- function(formula, data) {
  fit <- lm(formula, data)
  list(coef = coef(fit), resid = residuals(fit))
}

gls_fit <- function(X, y, cov_matrix) {
  W <- solve(cov_matrix)
  XtW <- t(X) %*% W
  beta_hat <- solve(XtW %*% X) %*% XtW %*% y
  return(beta_hat)
}

# Function to perform FGLS
ols <- function(formulas, data_list, max_iter = 10, tol = 1e-6) {
  
  # Create the design matrix # 
  Xes <- lapply(1:length(formulas), function(i) {
    model_matrix <- model.matrix(formulas[[i]], data_list[[i]])
    #(colnames(model_matrix) <- make.names(colnames(model_matrix)))
    model_matrix
  })
  
  # Adjust design matrices to have the same column names (order and missing columns)
  all_column_names <- unique(unlist(lapply(Xes, colnames)))
  
  Xes <- lapply(Xes, function(matrix) {
    missing_columns <- setdiff(all_column_names, colnames(matrix))
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        matrix <- cbind(matrix, rep(0, nrow(matrix)))
        colnames(matrix)[ncol(matrix)] <- col
      }
    }
    # Reorder columns to match the full set of column names
    matrix <- matrix[, all_column_names, drop = FALSE]
    matrix
  })
  
  # Combine the design matrices by rows
  X <- do.call(rbind, Xes) 
  
  # Combine response variables
  y <- unlist(lapply(data_list, function(data) data$yvar))
  
  rmid <- !is.na(y) ;  y <- y[rmid] ;  weights <- weights[rmid] # Remove nas (nas are present in income_atk and food_atk)
  
  # Perform OLS estimation on the combined data
  ols_model <- lm(y ~ 0 + ., data = as.data.frame(X), weights = weights) # 0 + because the design matrix already has an intercept
  print(out <- summary(ols_model))
  
  # Extract coefficients and their standard errors
  coefficients <- coef(ols_model)
  std_errors <- sqrt(diag(vcov(ols_model)))
  
  n <- length(coefficients)
  
  # Print the results
  result <- data.frame(Indicator = rep(ind, n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n))
  return(result)
}

included_row_indices <- function(formulas, data_list){
  # Initialize an empty list to store row indices with missing values
  included_row_indices <- list()
  
  # Loop over each dataset and its corresponding formula
  for (i in 1:length(formulas)) {
    # Get the names of variables used in the formula
    formula_vars <- all.vars(formulas[[i]])
    
    # Subset the dataset to include only the columns in the formula
    dataset_subset <- data_list[[i]][ , formula_vars, drop = FALSE]
    
    # Identify rows with missing values
    included_rows <- which(complete.cases(dataset_subset))
    
    # Store the row indices in the list
    included_row_indices[[i]] <- included_rows
  }
  
  # Assign names for easy reference (e.g., "A", "B", "C", "D")
  names(included_row_indices) <- names(data_list)
  
  # Display missing row indices for each dataset
  return(included_row_indices)
}

ols_jackknife <- function(formulas, data_list, max_iter = 10, tol = 1e-6) {
  
  olsresults <- data.frame(Indicator = NA, Predictor = NA, Model = NA,
                       Coefficient = NA, Estimate = NA, StdError = NA,
                       R2 = NA, R2adj = NA, Fstat = NA, pval = NA)
  
  # Create the design matrix # 
  Xes <- lapply(1:length(formulas), function(i) {
    model_matrix <- model.matrix(formulas[[i]], data_list[[i]])
    model_matrix
  })
  
  missing_counts <- lapply(data_list, function(data) colSums(is.na(data)))
  missing_counts  # This shows missing value counts for each variable in each dataset
  
  # Alternatively, you can count the rows with any missing values in each formula's variables
  missing_rows <- sapply(1:length(formulas), function(i) {
    formula_vars <- all.vars(formulas[[i]])
    sum(complete.cases(data_list[[i]][ , formula_vars]))
  })
  missing_rows  # Shows how many rows are complete for each dataset/formula pair
  
  included <- included_row_indices(data_list = data_list, formulas = formulas)
  included
  
  # Adjust design matrices to have the same column names (order and missing columns)
  all_column_names <- unique(unlist(lapply(Xes, colnames)))
  
  Xes <- lapply(Xes, function(matrix) {
    missing_columns <- setdiff(all_column_names, colnames(matrix))
    if (length(missing_columns) > 0) {
      for (col in missing_columns) {
        matrix <- cbind(matrix, rep(0, nrow(matrix)))
        colnames(matrix)[ncol(matrix)] <- col
      }
    }
    # Reorder columns to match the full set of column names
    matrix <- matrix[, all_column_names, drop = FALSE]
    matrix
  })
  
  # Combine the design matrices by rows
  X <- do.call(rbind, Xes) 
  X <- as.data.frame(X)
  
  # Combine response variables
  y <- unlist(lapply(1:4, function(index) data_list[[index]]$yvar[included[[index]]]))
  n1 <- nrow(Xes[[1]]) ; n2 <- nrow(Xes[[2]]) ; n3 <- nrow(Xes[[3]]) ; n4 <- nrow(Xes[[4]]) ; n <- sum(n1, n2, n3, n4)
  weights <- c(
    rep( (n1 - n2 - n3 + n4) / n1, n1),  # Weights for A
    rep( (n2 - n4) / n1, n2),     # Weights for B
    rep( (n3 - n4) / n1, n3),     # Weights for C
    rep( (n4) /n1, n4)            # Weights for D
  )
  #if(length(y) != nrow(X)){rmid <- !is.na(y) ;  y <- y[rmid] ;  weights <- weights[rmid]} # Remove nas (nas are present in income_atk and food_atk)
  #if(length(y) != nrow(X)){rmid <- unlist(misses) ;  y <- y[rmid] ;  weights <- weights[rmid]} # Remove nas (nas are present in income_atk and food_atk)
  
  # Null model
  null_model <- lm(y ~ 1, data = X)
  out <- summary(null_model)
  coefficients <- coef(null_model)
  std_errors <- sqrt(diag(vcov(null_model)))
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("None", n), Model = rep("Null", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = NA, pval = ps)
  olsresults <- rbind(olsresult, olsresults)
  
  # Add one models
  notcols <- c("(Intercept)", "a1_random_random", "a1_random_defined", "a1_satisfice_random", "a1_optimise", "a0_satisfice_defined",
               "b0_asp_IFL", "b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL", "b1_asp_IFo", "b1_asp_IoL", "b1_asp_oFL", "c1_stat", "c1_dyn_inter", 
               "c1_dyn_exter", "c0_dyn_both", "Utility_calculationcobb_douglas*", "Utility_calculationcobb_douglas+", 
               "Utility_calculationcobb_douglasln", "Utility_calculationrank_sum")
  
  add_cols <- all_column_names[!all_column_names %in% notcols]
  
  for(xvar in add_cols){
    add_model <- lm(y ~ X[[xvar]], weights = weights) # 0 + because the design matrix already has an intercept
    out <- summary(add_model)
    coefficients <- coef(add_model)
    std_errors <- sqrt(diag(vcov(add_model)))
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep(xvar, n), Model = rep("Add one", n), 
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), 
                         Fstat = rep(out$fstatistic[1], n), pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
  }
  
  # Add decision-making method and order
  a1 <- c("a1_optimise", "a1_random_random", "a1_random_defined", "a1_satisfice_random")
  add_model <- lm(y ~ ., data = X[a1], weights = weights) 
  out <- summary(add_model)
  coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("a1_decisionmaking_order", n), Model = rep("Add one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Add aspiration dimensions
  b1 <- c("b1_asp_Ioo", "b1_asp_oFo", "b1_asp_ooL", "b1_asp_IFo", "b1_asp_IoL","b1_asp_oFL")
  add_model <- lm(y ~ ., data = X[b1], weights = weights) 
  out <- summary(add_model)
  coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model)))
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("b1_asp_dims", n), Model = rep("Add one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Add aspiration threshold type and adjustment
  c1 <- c("c1_stat", "c1_dyn_inter", "c1_dyn_exter")
  add_model <- lm(y ~ ., data = X[c1], weights = weights) 
  out <- summary(add_model)
  coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model)))
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("c1_threshold_adjustment", n), Model = rep("Add one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Add Utility calculation
  d1 <- c("Utility_calculationcobb_douglas*", "Utility_calculationcobb_douglas+", "Utility_calculationcobb_douglasln", "Utility_calculationrank_sum")
  add_model <- lm(y ~ ., data = X[d1], weights = weights) 
  out <- summary(add_model)
  coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("Utility_calculation", n), Model = rep("Add one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Full model
  full_model <- lm(y ~ 0 + ., data = X, weights = weights) # 0 + because the design matrix already has an intercept
  out <- summary(full_model)
  coefficients <- coef(full_model)
  std_errors <- sqrt(diag(vcov(full_model)))
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("All", n), Model = rep("Full", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  olsresults <- rbind(olsresult, olsresults)
  
  # Drop one models
  for(xvar in add_cols){
    drop_model <- lm(y ~ 0 + ., data = X[!colnames(X) %in% xvar], weights = weights) # 0 + because the design matrix already has an intercept
    out <- summary(drop_model)
    coefficients <- coef(drop_model)
    std_errors <- sqrt(diag(vcov(drop_model)))
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep(xvar, n), Model = rep("Drop one", n), 
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
  }
  
  # Drop decision-making method and order
  drop_model <- lm(y ~ ., data = X[!colnames(X) %in% c(a1, "(Intercept)")], weights = weights) 
  out <- summary(drop_model)
  coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("a1_decisionmaking_order", n), Model = rep("Drop one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Drop aspiration dimensions
  drop_model <- lm(y ~ ., data = X[!colnames(X) %in% c(b1, "(Intercept)")], weights = weights) 
  out <- summary(drop_model)
  coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("b1_asp_dims", n), Model = rep("Drop one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Drop aspiration threshold type and adjustment
  drop_model <- lm(y ~ ., data = X[!colnames(X) %in% c(c1, "(Intercept)")], weights = weights)
  out <- summary(drop_model)
  coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("c1_threshold_adjustment", n), Model = rep("Drop one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  # Drop Utility calculation
  drop_model <- lm(y ~ ., data = X[!colnames(X) %in% c(d1, "(Intercept)")], weights = weights) 
  out <- summary(drop_model)
  coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
  ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
  n <- length(coefficients)
  
  olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("Utility_calculation", n), Model = rep("Drop one", n), 
                       Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                       R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                       pval = ps)
  
  olsresults <- rbind(olsresult, olsresults)
  
  return(olsresults)
}

subset_jackknife <- function(formulas, data_list, max_iter = 10, tol = 1e-6) {
  
  olsresults <- data.frame(Indicator = NA, Predictor = NA, Model = NA, Data = NA,
                        Coefficient = NA, Estimate = NA, StdError = NA,
                        R2 = NA, R2adj = NA, Fstat = NA, pval = NA)
  
  for(subset in c("A", "B", "C", "D")){
    
    y <-  data_list[[subset]][["yvar"]]
    xs <- model.matrix(formulas[[subset]], data_list[[subset]])
    included <- included_row_indices(data_list = data_list[subset], formulas = formulas[subset])
    y <- unlist(lapply(subset, function(index) data_list[[subset]]$yvar[included[[subset]]]))
    
    #if(length(y) != nrow(xs)){rmid <- !is.na(y) ;  y <- y[rmid] ;  weights <- weights[rmid]} # Remove nas (nas are present in income_atk and food_atk)
    xs <- xs[,-1] #remove intercept
    
    # Null model
    null_model <- lm(y ~ 1)
    out <- summary(null_model)
    coefficients <- coef(null_model) ; std_errors <- sqrt(diag(vcov(null_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("None", n), Model = rep("Null", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = NA, pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Add one models
    all_column_names <- colnames(xs)
    struct <- grepl("((a|b|c)(0|1)_|Utility|rank_sum)", all_column_names)
    add_cols <- all_column_names[struct == 0]
    
    for(xvar in add_cols){
      add_model <- lm(y ~ xs[,colnames(xs)==xvar]) #
      out <- summary(add_model)
      coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
      ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
      n <- length(coefficients)
      
      olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep(xvar, n), Model = rep("Add one", n), Data = rep(subset, n),
                           Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                           R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n),
                           pval = ps)
      
      olsresults <- rbind(olsresult, olsresults)
    }
    
    # Add decision-making method and order
    a1 <- all_column_names[grepl("a1_", all_column_names)]
    add_model <- lm(y ~ xs[,colnames(xs) %in% a1]) 
    out <- summary(add_model)
    coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("a1_decisionmaking_order", n), Model = rep("Add one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Add aspiration dimensions
    b1 <- all_column_names[grepl("b1_", all_column_names)]
    add_model <- lm(y ~ xs[,colnames(xs) %in% b1]) 
    out <- summary(add_model)
    coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("b1_asp_dims", n), Model = rep("Add one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Add aspiration threshold type and adjustment
    c1 <- all_column_names[grepl("c1_", all_column_names)]
    add_model <- lm(y ~ xs[,colnames(xs) %in% c1]) 
    out <- summary(add_model)
    coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("c1_threshold_adjustment", n), Model = rep("Add one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Add Utility calculation
    d1 <- all_column_names[grepl("Utility_calculation", all_column_names)]
    if(length(d1)>0){
      add_model <- lm(y ~ xs[,colnames(xs) %in% d1]) 
      out <- summary(add_model)
      coefficients <- coef(add_model) ; std_errors <- sqrt(diag(vcov(add_model))) 
      ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
      n <- length(coefficients)
      
      olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("Utility_calculation", n), Model = rep("Add one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
      
      olsresult$pval[!is.na(olsresult$Estimate)] <- ps
      olsresults <- rbind(olsresult, olsresults)
    }
    
    # Full model
    full_model <- lm(y ~ xs) 
    out <- summary(full_model)
    coefficients <- coef(full_model)
    std_errors <- sqrt(diag(vcov(full_model)))
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("All", n), Model = rep("Full", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Drop one models
    
    for(xvar in add_cols){
      drop_model <- lm(y ~ xs[,!all_column_names %in% xvar]) # 
      out <- summary(drop_model)
      coefficients <- coef(drop_model)
      std_errors <- sqrt(diag(vcov(drop_model)))
      ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
      n <- length(coefficients)
      
      olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep(xvar, n), Model = rep("Drop one", n), Data = rep(subset, n),
                           Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                           R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                           pval = ps)
      
      olsresults <- rbind(olsresult, olsresults)
      
    }
    
    # Drop decision-making method and order
    drop_model <- lm(y ~ xs[,!all_column_names %in% a1]) 
    out <- summary(drop_model)
    coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("a1_decisionmaking_order", n), Model = rep("Drop one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Drop aspiration dimensions
    drop_model <- lm(y ~ xs[,!all_column_names %in% b1]) 
    out <- summary(drop_model)
    coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("b1_asp_dims", n), Model = rep("Drop one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Drop aspiration threshold type and adjustment
    drop_model <- lm(y ~ xs[,!all_column_names %in% c1]) 
    out <- summary(drop_model)
    coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model))) 
    ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
    n <- length(coefficients)
    
    olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("c1_threshold_adjustment", n), Model = rep("Drop one", n), Data = rep(subset, n),
                         Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                         R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                         pval = ps)
    
    olsresults <- rbind(olsresult, olsresults)
    
    # Drop Utility calculation
    if (length(d1) > 0){
      drop_model <- lm(y ~ xs[,!all_column_names %in% d1]) 
      out <- summary(drop_model)
      coefficients <- coef(drop_model) ; std_errors <- sqrt(diag(vcov(drop_model)))
      ps <- out$coefficients[colnames(out$coefficients) == "Pr(>|t|)"]
      n <- length(coefficients)
      
      olsresult <- data.frame(Indicator = rep(ind, n), Predictor = rep("Utility_calculation", n), Model = rep("Drop one", n), Data = rep(subset, n),
                           Coefficient = names(coefficients), Estimate = coefficients, StdError = std_errors,
                           R2 = rep(out$r.squared, n), R2adj = rep(out$adj.r.squared, n), Fstat = rep(out$fstatistic[1], n), 
                           pval = ps)
      
      olsresults <- rbind(olsresult, olsresults)
    }
  }
  
  
  return(olsresults)
}

# Point range plots
contplot <- function(PREDICTOR, OUTCOME, DAT, COL = "gray", ...) {
  # The contplot() function is provided with no express or implied warranty.
  
  # Plotting OUTCOME vs. a continuous predictor
  # OUTCOME is a string indicating the outcome
  # PREDICTOR is a string indicating the predictor
  # DAT is the dataframe containing this data
  # ... allows you to pass additional arguments to plot below
  # Assign to y and x
  y <- DAT[[OUTCOME]]
  x <- DAT[[PREDICTOR]]
  # Scatterplot with regression line
  plot(x, y, las = 1, pch = 20, col=COL,
       font.lab = 2, font.axis = 2, ...)
  abline(lm(y ~ x), col = "red", lwd = 2)
}

catplot <- function(PREDICTOR, OUTCOME, DAT, COL = "gray", ...) {
  # The catplot() function is provided with no express or implied warranty.
  
  # Plotting OUTCOME vs. a categorical predictor
  # OUTCOME is a string indicating the outcome
  # PREDICTOR is a string indicating the predictor
  # DAT is the dataframe containing this data
  # ... allows you to pass additional arguments to plot below
  # Assign to y and x
  y <- DAT[[OUTCOME]]
  x <- DAT[[PREDICTOR]]
  # Plot
  ggplot(DAT, aes(x = x, y = y)) + 
    geom_point(color = "grey")
  
  LEVELS <- levels(x)
  NX     <- length(LEVELS)
  axis(1, at = 1:NX, labels = LEVELS, font = 2)
  # Compute mean y at each level of x
  MEANS <- tapply(y, x, mean, na.rm = T)
  # Add means to the plot
  points(1:NX, MEANS, col = "black", pch = 20, cex = 3)
  lines( 1:NX, MEANS, col = "red")  
  
  
}

cont_scatter <- function(data, inputnm, outputnm, transf = FALSE){
  data <- as.data.frame(data)
  outputcol <- which(colnames(data) == outputnm)
  inputcol <- which(colnames(data) == inputnm)
  p <- ggplot(data, aes(y = data[,outputcol], x = data[,inputcol])) + 
    geom_point(color = "grey") + 
    # geom_smooth(method = "lm") +
    xlab(inputnm) + ylab(outputnm) + 
    theme_classic()
  
  if(transf){p <- p + scale_y_log10()}
  
  print(ggMarginal(p, type = "densigram"))
}

library(meantables)
library(ggrepel)
library(tinter)
library(multcompView)

cat_scatter <- function(data, inputnm, outputnm, colnm = inputnm, transf = FALSE, ymax = FALSE, ymin = FALSE,
                        figtitle = "", legendtitle = xtitle, showlegend = TRUE, xtitle = inputnm, ytitle = ouputnm, labels = FALSE){
  data <- as.data.frame(data)
  outputcol <- which(colnames(data) == outputnm)
  inputcol <- which(colnames(data) == inputnm) 
  colcol <- which(colnames(data) == colnm) ; xvec <- data[which(colnames(data) == colnm)] 
  
  sumdat <- data.frame(input = data[,inputcol], output = data[,outputcol], col = data[,colcol]) %>% 
    group_by(input, col) %>% 
    mean_table(output)
  
  sumdat <- sumdat[!is.na(sumdat$group_2_cat),]
  
  avdat <- data.frame(output = data[,outputcol], col = data[,colcol]) %>% 
    group_by(col) %>% 
    mean_table(output)
  
  avdat <- avdat[!is.na(avdat$group_cat),] ; if(max(avdat$mean>=100)){avdat$mean <- round(avdat$mean)} ; if(max(avdat$mean) < 100 & min(avdat$mean > 5)){avdat$mean <- round(avdat$mean, digits = 1)}
  
  # Perform ANOVA and Tukey's HSD post-hoc test
  anova_result <- aov(data[,outputcol] ~ data[,colcol])
  tukey_result <- TukeyHSD(anova_result)
  
  # Extract the letters based on Tukey's HSD results
  tukey_letters <- multcompLetters(tukey_result$`data[, colcol]`[, "p adj"] < 0.05)
  letters <- data.frame(group_cat = names(tukey_letters$Letters), letters = tukey_letters$Letters) ; avdat <- full_join(avdat, letters)
  letters <- data.frame(group_2_cat = names(tukey_letters$Letters), letters = tukey_letters$Letters) ; sumdat <- full_join(sumdat, letters)
  
  p <- ggplot(sumdat, aes(x=group_2_cat, y=mean, col = letters)) +
    geom_col(position = "dodge", colour = "white", fill = "white") +
    geom_linerange(aes(ymin=lcl, ymax=ucl), position = position_jitterdodge(seed = 42)) + ggtitle(figtitle) +
    geom_point(size = 5, shape = 20, position = position_jitterdodge(seed = 42)) + ylab(ytitle) + xlab(xtitle) +
    geom_point(size = 13, data = avdat, aes(y=mean, x = group_cat), colour = "deepskyblue", shape = 10) + 
    geom_point(size = 13, data = avdat, aes(y=mean, x = group_cat), colour = "deepskyblue", alpha = 0.4) + 
    geom_text_repel(size = 14, data = avdat, aes(y=mean, x = group_cat, label = mean), colour = "deepskyblue", box.padding = 0.95, fontface = "bold", seed = 1) + 
    geom_text_repel(size = 14, data = avdat, aes(y=mean, x = group_cat, label = mean), colour = "deepskyblue4", box.padding = 0.95, seed = 1) + 
    geom_text(size = 14, data = avdat, aes(y=max(sumdat$mean) + abs(max(sumdat$mean))*0.1, x = group_cat, label = letters)) + 
    scale_color_manual(values=darken(grey.colors(length(unique(sumdat$letters))), amount = 0.3)) + 
    theme_classic() + labs(col = legendtitle) 
  
  if(length(labels)>1){p <- p + scale_x_discrete(labels=labels)}
  if(transf){p <- p + scale_y_log10()}
  if(!showlegend){p <- p + theme(legend.position = "none")}
  if(ymax!=FALSE & ymin==FALSE){p <- p + ylim(0,ymax)}
  if(ymax!=FALSE & ymin!=FALSE){p <- p + ylim(ymin,ymax)}
  if(ymax==FALSE & ymin!=FALSE){p <- p + ylim(ymin,NA)}
  
  p <- p + theme(axis.text = element_text(size = 28), axis.title = element_text(size = 32))    
  
  print(p)
}

glob_scatter <- function(data, inputnm, outputnm, colnm = inputnm, transf = FALSE, ymax = FALSE, ymin = FALSE,
                         figtitle = "", legendtitle = xtitle, showlegend = TRUE, xtitle = inputnm, ytitle = ouputnm, labels = FALSE){
  data <- as.data.frame(data)
  outputcol <- which(colnames(data) == outputnm) ; yvec <- data[which(colnames(data) == outputnm)]
  inputcol <- which(colnames(data) == inputnm) 
  colcol <- which(colnames(data) == colnm) ; xvec <- data[which(colnames(data) == colnm)] 
  boxdat <- data.frame(y = yvec, x = xvec) ; colnames(boxdat) <- c("y", "x")
  sumdat <- data.frame(input = data[,inputcol], output = data[,outputcol], col = data[,colcol]) %>% 
    group_by(input, col) %>% 
    mean_table(output)
  
  sumdat <- sumdat[!is.na(sumdat$group_2_cat),]
  avdat <- data.frame(output = data[,outputcol], col = data[,colcol]) %>% 
    group_by(col) %>% 
    mean_table(output)
  
  avdat <- avdat[!is.na(avdat$group_cat),] ; if(max(avdat$mean>=100)){avdat$mean <- round(avdat$mean)} ; if(max(avdat$mean) < 100 & min(avdat$mean > 5)){avdat$mean <- round(avdat$mean, digits = 1)}
  
  p <- ggplot(sumdat, aes(x=group_2_cat, y=mean)) +
    geom_linerange(aes(ymin=lcl, ymax=ucl), position = position_jitter(seed = 42), colour = "grey30") + ggtitle(figtitle) +
    geom_point(size = 5, shape = 20, position = position_jitter(seed = 42), colour = "grey30") + ylab(ytitle) + xlab(xtitle) +
    geom_smooth(colour = "grey30", method = "lm") + 
    geom_point(size = 13, data = avdat, aes(y=mean, x = group_cat), colour = "deepskyblue", shape = 10) + 
    geom_point(size = 13, data = avdat, aes(y=mean, x = group_cat), colour = "deepskyblue", alpha = 0.2) + 
    #geom_text_repel(size = 10, data = avdat, aes(y=mean, x = group_cat, label = mean), 
    #                colour = "red", box.padding = 0.5, max.overlaps = Inf) + 
    geom_text_repel(size = 14, data = avdat, aes(y=mean, x = group_cat, label = mean), colour = "deepskyblue", box.padding = 0.95, fontface = "bold", seed = 1) + 
    geom_text_repel(size = 14, data = avdat, aes(y=mean, x = group_cat, label = mean), colour = "deepskyblue4", box.padding = 0.95, seed = 1) + 
    theme_classic() + labs(col = legendtitle) +
    theme(axis.text = element_text(size = 28), axis.title = element_text(size = 32))
  
  if(labels != FALSE){p <- p + scale_x_discrete(labels=labels)}
  if(transf){p <- p + scale_y_log10()}
  if(!showlegend){p <- p + theme(legend.position = "none")}
  if(ymax!=FALSE & ymin==FALSE){p <- p + ylim(0,ymax)}
  if(ymax!=FALSE & ymin!=FALSE){p <- p + ylim(ymin,ymax)}
  if(ymax==FALSE & ymin!=FALSE){p <- p + ylim(ymin,NA)}
  
  print(p)
}

hh_scatter <- function(data, inputnm, outputnm, colnm = inputnm, transf = FALSE, ymax = FALSE, ymin = FALSE, from, to, low = NA, high = NA, zoom = FALSE, 
                       cut = FALSE, sampledata = FALSE,
                       figtitle = "", legendtitle = xtitle, showlegend = TRUE, xtitle = inputnm, ytitle = ouputnm, labels = FALSE){
  data <- as.data.frame(data)
  outputcol <- which(colnames(data) == outputnm) ; yvec <- data[which(colnames(data) == outputnm)]
  inputcol <- which(colnames(data) == inputnm) 
  colcol <- which(colnames(data) == colnm) ; xvec <- data[which(colnames(data) == colnm)] 
  
  sumdat <- data.frame(input = data[,inputcol], output = data[,outputcol], col = data[,colcol]) %>% 
    group_by(input, col) %>% 
    mean_table(output)
  
  sumdat <- sumdat[!is.na(sumdat$group_2_cat),]
  if(sampledata){sumdat <- sumdat[sample(1:nrow(sumdat), round(nrow(sumdat)/100)),]}
  
  avdat <- data.frame(output = data[,outputcol], col = data$Population) %>% 
    group_by(col) %>% 
    mean_table(output)
  
  avdat <- avdat[!is.na(avdat$group_cat),] ; if(max(avdat$mean>=100)){avdat$mean <- round(avdat$mean)} ; if(max(avdat$mean) < 100 & min(avdat$mean > 5)){avdat$mean <- round(avdat$mean, digits = 1)}
  
  p <- ggplot(sumdat, aes(x=group_2_cat, y=mean)) + ylab(ytitle) + xlab(xtitle) +
    geom_linerange(aes(ymin=lcl, ymax=ucl), position = position_jitter(seed = 42), colour = "grey50", alpha = 0.4) + ggtitle(figtitle) +
    geom_point(size = 5, shape = 20, position = position_jitter(seed = 42), colour = "grey30", 
               alpha = 0.5) + ylab(ytitle) + xlab(xtitle) +
    geom_smooth(colour = "grey30", method = "lm") + 
    # geom_point(size = 13, data = avdat, aes(y=mean, x = group_cat), colour = "deepskyblue", shape = 10) + 
    # geom_point(size = 13, data = avdat, aes(y=mean, x = group_cat), colour = "deepskyblue", alpha = 0.4) + 
    # #geom_text_repel(size = 10, data = avdat, aes(y=mean, x = group_cat, label = mean), 
    #                colour = "red", box.padding = 0.5, max.overlaps = Inf) + 
    # geom_text_repel(size = 14, data = avdat, aes(y=mean, x = group_cat, label = mean), colour = "deepskyblue", box.padding = 0.95, fontface = "bold", seed = 1) + 
    # geom_text_repel(size = 14, data = avdat, aes(y=mean, x = group_cat, label = mean), colour = "deepskyblue4", box.padding = 0.95, seed = 1) + 
    theme_classic() + labs(col = legendtitle) +
    theme(axis.text = element_text(size = 28), axis.title = element_text(size = 32))
  
  if(length(labels)>1){p <- p + scale_x_discrete(labels=labels)}
  if(transf){p <- p + scale_x_log10()}
  if(!showlegend){p <- p + theme(legend.position = "none")}
  if(ymax!=FALSE & ymin==FALSE){p <- p + ylim(0,ymax)}
  if(ymax!=FALSE & ymin!=FALSE){p <- p + ylim(ymin,ymax)}
  if(ymax==FALSE & ymin!=FALSE){p <- p + ylim(ymin,NA)}
  if(zoom){print(p +  geom_magnify(from = from, to = to))}else(print(p))
  if(cut){print(p + coord_cartesian(xlim = c(from,to), ylim = c(low,high)))}
  
}

cat_scaled <- function(data, inputnm, outputnms, colnm = inputnm, transf = FALSE, yticks = TRUE){ 
  data <- as.data.frame(data)
  outputcols <- which(colnames(data) %in% outputnms)
  inputcol <- which(colnames(data) == inputnm)
  colcol <- which(colnames(data) == colnm)
  if(colcol != inputcol){
    colnames(data)[colcol] <- "col"
  }else{ 
    data$col <- data[,inputcol]
    colcol <- which(colnames(data) == "col")
  }
  colnames(data)[inputcol] <- "input" 
  sumdat <- data[,c(outputcols, inputcol, colcol)]
  sumdat <- sumdat %>%
    pivot_longer(!c(input, col), names_to = "indicator", values_to = "outcome") %>% 
    group_by(input, col, indicator) %>%
    summarise(mean = mean(outcome)) %>%
    pivot_wider(names_from = "indicator", values_from = "mean")
  
  p <- ggparcoord(data = sumdat, scale = "uniminmax", showPoints = TRUE,
                  columns = 3:(2 + length(outputcols)),
                  groupColumn = "col") + 
    scale_color_brewer(palette="Dark2") +
    theme_classic()
  
  if(transf){p <- p + scale_y_log10()}
  if(!yticks){p <- p + theme(axis.text.x=element_blank())}
  
  print(p)
}
