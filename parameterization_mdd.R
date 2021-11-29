# load libraries and read in data -----------------------------------------
library(tidyverse) 

# setting path to data files
file_path <- "./data_files/" 

# creating age variable values equal midpoint of our age groups
age <- c(19.5,seq(23.5,63.5,5))

# data for primary analysis
df_mdd <- read.csv(paste0(file_path, "rates_mdd.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_smk <- read.csv(paste0(file_path, "rates_mdd_smk.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_hl <- read.csv(paste0(file_path, "rates_mdd_hl.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_social <- read.csv(paste0(file_path, "rates_mdd_social.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_joint <- read.csv(paste0(file_path, "rates_mdd_joint.csv")) %>%
  mutate(across(everything(), ~./100))

# data for sensitivity analysis
df_mdd_sens <- read.csv(paste0(file_path, "rates_mdd_sensitivity.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_sens_smk <- read.csv(paste0(file_path, "rates_mdd_sensitivity_smk.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_sens_hl <- read.csv(paste0(file_path, "rates_mdd_sensitivity_hl.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_sens_social <- read.csv(paste0(file_path, "rates_mdd_sensitivity_social.csv")) %>%
  mutate(across(everything(), ~./100))

cf_df_mdd_sens_joint <- read.csv(paste0(file_path, "rates_mdd_sensitivity_joint.csv")) %>%
  mutate(across(everything(), ~./100))


# estimate parameters to predict transition rates -------------------------
# primary analyses
# observed data mdd: from not having to having MDD: linear
model_mdd <- list()
for (i in c(1:4)){
  y <- df_mdd[,i]
  model_mdd[[i]] <- lm(y ~ age)
}

# observed data mdd: from having to not having MDD: quadratic
model_mdd.r <- list()
for (i in c(5:8)){
  y <- df_mdd[,i]
  model_mdd.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd smk: from not having to having MDD: linear 
cf_model_mdd_smk <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_smk[,i]
  cf_model_mdd_smk[[i]] <- lm(y ~ age)
}

# counterfactual data mdd smk: having to not having MDD: quadratic
cf_model_mdd_smk.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_smk[,i]
  cf_model_mdd_smk.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd hl: from not having to having MDD: linear 
cf_model_mdd_hl <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_hl[,i]
  cf_model_mdd_hl[[i]] <- lm(y ~ age)
}

# counterfactual data mdd hl: having to not having MDD: quadratic
cf_model_mdd_hl.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_hl[,i]
  cf_model_mdd_hl.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd social: from not having to having MDD: linear
cf_model_mdd_social <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_social[,i]
  cf_model_mdd_social[[i]] <- lm(y ~ age)
}

# counterfactual data mdd social: having to not having MDD: quadratic
cf_model_mdd_social.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_social[,i]
  cf_model_mdd_social.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd joint: from not having to having MDD: linear
cf_model_mdd_joint <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_joint[,i]
  cf_model_mdd_joint[[i]] <- lm(y ~ age)
}

# counterfactual data mdd joint: having to not having MDD: quadratic
cf_model_mdd_joint.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_joint[,i]
  cf_model_mdd_joint.r[[i]] <- lm(y ~ age + I(age^2))
}

# sensitivity analyses
# observed data mdd: from not having to having MDD: linear
model_mdd_sens <- list()
for (i in c(1:4)){
  y <- df_mdd_sens[,i]
  model_mdd_sens[[i]] <- lm(y ~ age)
}

# observed data mdd: from having to not having MDD: quadratic
model_mdd_sens.r <- list()
for (i in c(5:8)){
  y <- df_mdd_sens[,i]
  model_mdd_sens.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd smk: from not having to having MDD: linear 
cf_model_mdd_sens_smk <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_sens_smk[,i]
  cf_model_mdd_sens_smk[[i]] <- lm(y ~ age)
}

# counterfactual data mdd smk: having to not having MDD: quadratic
cf_model_mdd_sens_smk.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_sens_smk[,i]
  cf_model_mdd_sens_smk.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd hl: from not having to having MDD: linear 
cf_model_mdd_sens_hl <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_sens_hl[,i]
  cf_model_mdd_sens_hl[[i]] <- lm(y ~ age)
}

# counterfactual data mdd hl: having to not having MDD: quadratic
cf_model_mdd_sens_hl.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_sens_hl[,i]
  cf_model_mdd_sens_hl.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd social: from not having to having MDD: linear
cf_model_mdd_sens_social <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_sens_social[,i]
  cf_model_mdd_sens_social[[i]] <- lm(y ~ age)
}

# counterfactual data mdd social: having to not having MDD: quadratic
cf_model_mdd_sens_social.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_sens_social[,i]
  cf_model_mdd_sens_social.r[[i]] <- lm(y ~ age + I(age^2))
}

# counterfactual data mdd joint: from not having to having MDD: linear
cf_model_mdd_sens_joint <- list()
for (i in c(1:4)){
  y <- cf_df_mdd_sens_joint[,i]
  cf_model_mdd_sens_joint[[i]] <- lm(y ~ age)
}

# counterfactual data mdd joint: having to not having MDD: quadratic
cf_model_mdd_sens_joint.r <- list()
for (i in c(5:8)){
  y <- cf_df_mdd_sens_joint[,i]
  cf_model_mdd_sens_joint.r[[i]] <- lm(y ~ age + I(age^2))
}

# functions to estimate transition rates using model coefficients ---------
# calTime and duration are needed even though we don't use those parameters

# functions using observed data mdd  -------------------------------------
# observed data mdd: from not having MDD to having MDD
# males with low education
nomdd_mdd.lo.m <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[1]])[1]
  a <- coef(model_mdd[[1]])[2]

  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with low education
nomdd_mdd.lo.f <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[2]])[1]
  a <- coef(model_mdd[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# males with high education
nomdd_mdd.hi.m <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[3]])[1]
  a <- coef(model_mdd[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
nomdd_mdd.hi.f <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[4]])[1]
  a <- coef(model_mdd[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# oberved data mdd: from having MDD to not having MDD
# males with low education
mdd_nomdd.lo.m <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[5]])[1]
  a  <- coef(model_mdd.r[[5]])[2]
  a2 <- coef(model_mdd.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
mdd_nomdd.lo.f <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[6]])[1]
  a  <- coef(model_mdd.r[[6]])[2]
  a2 <- coef(model_mdd.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
mdd_nomdd.hi.m <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[7]])[1]
  a  <- coef(model_mdd.r[[7]])[2]
  a2 <- coef(model_mdd.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with high education
mdd_nomdd.hi.f <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[8]])[1]
  a  <- coef(model_mdd.r[[8]])[2]
  a2 <- coef(model_mdd.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 


# functions using counterfactual data mdd smk  ---------------------------
# counterfactual data mdd smk: from not having MDD to having MDD
# males with low education
cf_smk_nomdd_mdd.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_smk[[1]])[1]
  a <- coef(cf_model_mdd_smk[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_smk_nomdd_mdd.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_smk[[2]])[1]
  a <- coef(cf_model_mdd_smk[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_smk_nomdd_mdd.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_smk[[3]])[1]
  a <- coef(cf_model_mdd_smk[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_smk_nomdd_mdd.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_smk[[4]])[1]
  a <- coef(cf_model_mdd_smk[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd smk: from having MDD to not having MDD
# males with low education
cf_smk_mdd_nomdd.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_smk.r[[5]])[1]
  a  <- coef(cf_model_mdd_smk.r[[5]])[2]
  a2 <- coef(cf_model_mdd_smk.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_smk_mdd_nomdd.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_smk.r[[6]])[1]
  a  <- coef(cf_model_mdd_smk.r[[6]])[2]
  a2 <- coef(cf_model_mdd_smk.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_smk_mdd_nomdd.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_smk.r[[7]])[1]
  a  <- coef(cf_model_mdd_smk.r[[7]])[2]
  a2 <- coef(cf_model_mdd_smk.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)  
  return(rate)
} 

# females with high education
cf_smk_mdd_nomdd.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_smk.r[[8]])[1]
  a  <- coef(cf_model_mdd_smk.r[[8]])[2]
  a2 <- coef(cf_model_mdd_smk.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 


# functions using counterfactual data mdd hl ----------------------------
# counterfactual data mdd hl: from not having MDD to having MDD
# males with low education
cf_hl_nomdd_mdd.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_hl[[1]])[1]
  a <- coef(cf_model_mdd_hl[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_hl_nomdd_mdd.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_hl[[2]])[1]
  a <- coef(cf_model_mdd_hl[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_hl_nomdd_mdd.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_hl[[3]])[1]
  a <- coef(cf_model_mdd_hl[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_hl_nomdd_mdd.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_hl[[4]])[1]
  a <- coef(cf_model_mdd_hl[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd hl: from having MDD to not having MDD
# males with low education
cf_hl_mdd_nomdd.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_hl.r[[5]])[1]
  a  <- coef(cf_model_mdd_hl.r[[5]])[2]
  a2 <- coef(cf_model_mdd_hl.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_hl_mdd_nomdd.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_hl.r[[6]])[1]
  a  <- coef(cf_model_mdd_hl.r[[6]])[2]
  a2 <- coef(cf_model_mdd_hl.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_hl_mdd_nomdd.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_hl.r[[7]])[1]
  a  <- coef(cf_model_mdd_hl.r[[7]])[2]
  a2 <- coef(cf_model_mdd_hl.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with high education
cf_hl_mdd_nomdd.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_hl.r[[8]])[1]
  a  <- coef(cf_model_mdd_hl.r[[8]])[2]
  a2 <- coef(cf_model_mdd_hl.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 


# functions using counterfactual data mdd social ---------------------------
# counterfactual data mdd social: from not having MDD to having MDD
# males with low education
cf_social_nomdd_mdd.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_social[[1]])[1]
  a <- coef(cf_model_mdd_social[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_social_nomdd_mdd.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_social[[2]])[1]
  a <- coef(cf_model_mdd_social[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_social_nomdd_mdd.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_social[[3]])[1]
  a <- coef(cf_model_mdd_social[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_social_nomdd_mdd.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_social[[4]])[1]
  a <- coef(cf_model_mdd_social[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd social: from having MDD to not having MDD
# males with low education
cf_social_mdd_nomdd.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_social.r[[5]])[1]
  a  <- coef(cf_model_mdd_social.r[[5]])[2]
  a2 <- coef(cf_model_mdd_social.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_social_mdd_nomdd.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_social.r[[6]])[1]
  a  <- coef(cf_model_mdd_social.r[[6]])[2]
  a2 <- coef(cf_model_mdd_social.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_social_mdd_nomdd.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_social.r[[7]])[1]
  a  <- coef(cf_model_mdd_social.r[[7]])[2]
  a2 <- coef(cf_model_mdd_social.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 

# females with high education
cf_social_mdd_nomdd.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_social.r[[8]])[1]
  a  <- coef(cf_model_mdd_social.r[[8]])[2]
  a2 <- coef(cf_model_mdd_social.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 

# functions using counterfactual data mdd joint --------------------------
# counterfactual data mdd joint: from not having MDD to having MDD
# males with low education
cf_joint_nomdd_mdd.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_joint[[1]])[1]
  a <- coef(cf_model_mdd_joint[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_joint_nomdd_mdd.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_joint[[2]])[1]
  a <- coef(cf_model_mdd_joint[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_joint_nomdd_mdd.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_joint[[3]])[1]
  a <- coef(cf_model_mdd_joint[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_joint_nomdd_mdd.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_joint[[4]])[1]
  a <- coef(cf_model_mdd_joint[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd joint: from having MDD to not having MDD
# males with low education
cf_joint_mdd_nomdd.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_joint.r[[5]])[1]
  a  <- coef(cf_model_mdd_joint.r[[5]])[2]
  a2 <- coef(cf_model_mdd_joint.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_joint_mdd_nomdd.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_joint.r[[6]])[1]
  a  <- coef(cf_model_mdd_joint.r[[6]])[2]
  a2 <- coef(cf_model_mdd_joint.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_joint_mdd_nomdd.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_joint.r[[7]])[1]
  a  <- coef(cf_model_mdd_joint.r[[7]])[2]
  a2 <- coef(cf_model_mdd_joint.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 

# females with high education
cf_joint_mdd_nomdd.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_joint.r[[8]])[1]
  a  <- coef(cf_model_mdd_joint.r[[8]])[2]
  a2 <- coef(cf_model_mdd_joint.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 


# rate functions for sensitivity analyses ---------------------------------
# functions using observed data mdd: sensitivity analysis
# observed data mdd: from not having MDD to having MDD
# males with low education
nomdd_mdd.sens.lo.m <- function(age,calTime,duration){
  
  b <- coef(model_mdd_sens[[1]])[1]
  a <- coef(model_mdd_sens[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with low education
nomdd_mdd.sens.lo.f <- function(age,calTime,duration){
  
  b <- coef(model_mdd_sens[[2]])[1]
  a <- coef(model_mdd_sens[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# males with high education
nomdd_mdd.sens.hi.m <- function(age,calTime,duration){
  
  b <- coef(model_mdd_sens[[3]])[1]
  a <- coef(model_mdd_sens[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
nomdd_mdd.sens.hi.f <- function(age,calTime,duration){
  
  b <- coef(model_mdd_sens[[4]])[1]
  a <- coef(model_mdd_sens[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# oberved data mdd: from having MDD to not having MDD
# males with low education
mdd_nomdd.sens.lo.m <- function(age,calTime,duration){
  
  b  <- coef(model_mdd_sens.r[[5]])[1]
  a  <- coef(model_mdd_sens.r[[5]])[2]
  a2 <- coef(model_mdd_sens.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
mdd_nomdd.sens.lo.f <- function(age,calTime,duration){
  
  b  <- coef(model_mdd_sens.r[[6]])[1]
  a  <- coef(model_mdd_sens.r[[6]])[2]
  a2 <- coef(model_mdd_sens.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
mdd_nomdd.sens.hi.m <- function(age,calTime,duration){
  
  b  <- coef(model_mdd_sens.r[[7]])[1]
  a  <- coef(model_mdd_sens.r[[7]])[2]
  a2 <- coef(model_mdd_sens.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with high education
mdd_nomdd.sens.hi.f <- function(age,calTime,duration){
  
  b  <- coef(model_mdd_sens.r[[8]])[1]
  a  <- coef(model_mdd_sens.r[[8]])[2]
  a2 <- coef(model_mdd_sens.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 


# functions using counterfactual data mdd smk: sensitivity analysis
# counterfactual data mdd smk: from not having MDD to having MDD
# males with low education
cf_smk_nomdd_mdd.sens.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_smk[[1]])[1]
  a <- coef(cf_model_mdd_sens_smk[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_smk_nomdd_mdd.sens.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_smk[[2]])[1]
  a <- coef(cf_model_mdd_sens_smk[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_smk_nomdd_mdd.sens.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_smk[[3]])[1]
  a <- coef(cf_model_mdd_sens_smk[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_smk_nomdd_mdd.sens.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_smk[[4]])[1]
  a <- coef(cf_model_mdd_sens_smk[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd smk: from having MDD to not having MDD
# males with low education
cf_smk_mdd_nomdd.sens.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_smk.r[[5]])[1]
  a  <- coef(cf_model_mdd_sens_smk.r[[5]])[2]
  a2 <- coef(cf_model_mdd_sens_smk.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_smk_mdd_nomdd.sens.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_smk.r[[6]])[1]
  a  <- coef(cf_model_mdd_sens_smk.r[[6]])[2]
  a2 <- coef(cf_model_mdd_sens_smk.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_smk_mdd_nomdd.sens.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_smk.r[[7]])[1]
  a  <- coef(cf_model_mdd_sens_smk.r[[7]])[2]
  a2 <- coef(cf_model_mdd_sens_smk.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)  
  return(rate)
} 

# females with high education
cf_smk_mdd_nomdd.sens.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_smk.r[[8]])[1]
  a  <- coef(cf_model_mdd_sens_smk.r[[8]])[2]
  a2 <- coef(cf_model_mdd_sens_smk.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 


# functions using counterfactual data mdd hl: sensitivity analysis 
# counterfactual data mdd hl: from not having MDD to having MDD
# males with low education
cf_hl_nomdd_mdd.sens.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_hl[[1]])[1]
  a <- coef(cf_model_mdd_sens_hl[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_hl_nomdd_mdd.sens.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_hl[[2]])[1]
  a <- coef(cf_model_mdd_sens_hl[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_hl_nomdd_mdd.sens.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_hl[[3]])[1]
  a <- coef(cf_model_mdd_sens_hl[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_hl_nomdd_mdd.sens.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_hl[[4]])[1]
  a <- coef(cf_model_mdd_sens_hl[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd hl: from having MDD to not having MDD
# males with low education
cf_hl_mdd_nomdd.sens.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_hl.r[[5]])[1]
  a  <- coef(cf_model_mdd_sens_hl.r[[5]])[2]
  a2 <- coef(cf_model_mdd_sens_hl.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_hl_mdd_nomdd.sens.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_hl.r[[6]])[1]
  a  <- coef(cf_model_mdd_sens_hl.r[[6]])[2]
  a2 <- coef(cf_model_mdd_sens_hl.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_hl_mdd_nomdd.sens.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_hl.r[[7]])[1]
  a  <- coef(cf_model_mdd_sens_hl.r[[7]])[2]
  a2 <- coef(cf_model_mdd_sens_hl.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with high education
cf_hl_mdd_nomdd.sens.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_hl.r[[8]])[1]
  a  <- coef(cf_model_mdd_sens_hl.r[[8]])[2]
  a2 <- coef(cf_model_mdd_sens_hl.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 


# functions using counterfactual data mdd social: sensitivity analysis 
# counterfactual data mdd social: from not having MDD to having MDD
# males with low education
cf_social_nomdd_mdd.sens.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_social[[1]])[1]
  a <- coef(cf_model_mdd_sens_social[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_social_nomdd_mdd.sens.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_social[[2]])[1]
  a <- coef(cf_model_mdd_sens_social[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_social_nomdd_mdd.sens.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_social[[3]])[1]
  a <- coef(cf_model_mdd_sens_social[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_social_nomdd_mdd.sens.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_social[[4]])[1]
  a <- coef(cf_model_mdd_sens_social[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd social: from having MDD to not having MDD
# males with low education
cf_social_mdd_nomdd.sens.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_social.r[[5]])[1]
  a  <- coef(cf_model_mdd_sens_social.r[[5]])[2]
  a2 <- coef(cf_model_mdd_sens_social.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_social_mdd_nomdd.sens.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_social.r[[6]])[1]
  a  <- coef(cf_model_mdd_sens_social.r[[6]])[2]
  a2 <- coef(cf_model_mdd_sens_social.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_social_mdd_nomdd.sens.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_social.r[[7]])[1]
  a  <- coef(cf_model_mdd_sens_social.r[[7]])[2]
  a2 <- coef(cf_model_mdd_sens_social.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 

# females with high education
cf_social_mdd_nomdd.sens.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_social.r[[8]])[1]
  a  <- coef(cf_model_mdd_sens_social.r[[8]])[2]
  a2 <- coef(cf_model_mdd_sens_social.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 

# functions using counterfactual data mdd joint: sensitivity analysis 
# counterfactual data mdd joint: from not having MDD to having MDD
# males with low education
cf_joint_nomdd_mdd.sens.lo.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_joint[[1]])[1]
  a <- coef(cf_model_mdd_sens_joint[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# females with low education
cf_joint_nomdd_mdd.sens.lo.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_joint[[2]])[1]
  a <- coef(cf_model_mdd_sens_joint[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age) 
  return(rate)
} 

# males with high education
cf_joint_nomdd_mdd.sens.hi.m <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_joint[[3]])[1]
  a <- coef(cf_model_mdd_sens_joint[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# females with high education
cf_joint_nomdd_mdd.sens.hi.f <- function(age,calTime,duration){
  
  b <- coef(cf_model_mdd_sens_joint[[4]])[1]
  a <- coef(cf_model_mdd_sens_joint[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)  
  return(rate)
} 

# counterfactual data mdd joint: from having MDD to not having MDD
# males with low education
cf_joint_mdd_nomdd.sens.lo.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_joint.r[[5]])[1]
  a  <- coef(cf_model_mdd_sens_joint.r[[5]])[2]
  a2 <- coef(cf_model_mdd_sens_joint.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# females with low education
cf_joint_mdd_nomdd.sens.lo.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_joint.r[[6]])[1]
  a  <- coef(cf_model_mdd_sens_joint.r[[6]])[2]
  a2 <- coef(cf_model_mdd_sens_joint.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# males with high education
cf_joint_mdd_nomdd.sens.hi.m <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_joint.r[[7]])[1]
  a  <- coef(cf_model_mdd_sens_joint.r[[7]])[2]
  a2 <- coef(cf_model_mdd_sens_joint.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 

# females with high education
cf_joint_mdd_nomdd.sens.hi.f <- function(age,calTime,duration){
  
  b  <- coef(cf_model_mdd_sens_joint.r[[8]])[1]
  a  <- coef(cf_model_mdd_sens_joint.r[[8]])[2]
  a2 <- coef(cf_model_mdd_sens_joint.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2) 
  return(rate)
} 


# mortality rates ---------------------------------------------------------
# mortality is set to 0
mortRates <- function(age,calTime,duration){
  rate <- 0
  return(rate)
}


# checking model fit ------------------------------------------------------
# observed data mdd
observed_mdd <- c(df_mdd[,1], df_mdd[,2], df_mdd[,3],
                   df_mdd[,4], df_mdd[,5], df_mdd[,6],
                   df_mdd[,7], df_mdd[,8])
fit_mdd <- c(nomdd_mdd.lo.m(age), nomdd_mdd.lo.f(age),
              nomdd_mdd.hi.m(age), nomdd_mdd.hi.f(age),
              mdd_nomdd.lo.m(age), mdd_nomdd.lo.f(age),
              mdd_nomdd.hi.m(age), mdd_nomdd.hi.f(age))
sex <- c("Male","Female")
education <- c("Low education","Low education","High education","High education")
transition_mdd <- c(rep("Incidence",4),rep("Remittance",4))
check_mdd <- data.frame(age=age, observed=observed_mdd, fit=fit_mdd,
                         sex=rep(sex,each=10), 
                         education=rep(education,each=10),
                         transition=rep(transition_mdd,each=10))

# calculating the mean absolute error
check_mdd %>%
  group_by(transition, sex, education) %>%
  summarise(mae = mean(abs(observed-fit))) %>%
  ungroup()

# plotting observed vs expected values
ggplot(check_mdd)+
  geom_point(aes(age,observed,group=sex,color=sex))+
  geom_line(aes(age,fit,group=sex,color=sex))+
  ylab("Transition rates") +
  xlab("Age (years)") +
  facet_grid(education~transition)
ggsave("./plots/transition_fit_mdd.pdf", plot = last_plot())

rm(observed_mdd, fit_mdd, check_mdd, transition_mdd, 
   sex, education)



