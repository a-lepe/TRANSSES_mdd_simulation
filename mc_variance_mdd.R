# load libraries ----------------------------------------------------------
library(MicSim)
library(tidyverse)
library(scales)

# setting path to data files
file_path <- "./data_files/" 
mc_path <- "./mc_files/"

# defining parameters used to repeat simulations with different N
max_iter <- 20
min_N <- 100000
max_N <- 500000 
step_size <- 100000
n_iter <- length(seq(from = min_N, to = max_N, by = step_size))
n_cores <- 24

# creating age variable values equal midpoint of our age groups
# for use in the parameterization
age <- c(19.5,seq(23.5,63.5,5))


# MDD parameterization ----------------------------------------------------

# loading the data
df_mdd <- read.csv(paste0(file_path, "rates_mdd.csv")) %>%
  mutate(across(everything(), ~./100))

# estimating parameters to predict incidence rates 
model_mdd <- list()
for (i in c(1:4)){
  y <- df_mdd[,i]
  model_mdd[[i]] <- lm(y ~ age)
}

# estimating parameters to predict remittance rates 
model_mdd.r <- list()
for (i in c(5:8)){
  y <- df_mdd[,i]
  model_mdd.r[[i]] <- lm(y ~ age + I(age^2))
}


# creating functions that will be used to estimate transition
# probabilities in the microsimulation

# incidence: males with low education
nomdd_mdd.lo.m <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[1]])[1]
  a <- coef(model_mdd[[1]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# incidence: females with low education
nomdd_mdd.lo.f <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[2]])[1]
  a <- coef(model_mdd[[2]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# incidence: males with high education
nomdd_mdd.hi.m <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[3]])[1]
  a <- coef(model_mdd[[3]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# incidence: females with high education
nomdd_mdd.hi.f <- function(age,calTime,duration){
  
  b <- coef(model_mdd[[4]])[1]
  a <- coef(model_mdd[[4]])[2]
  
  rate <- ifelse(age < 18, 0, b + a*age)
  return(rate)
} 

# remittance: males with low education
mdd_nomdd.lo.m <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[5]])[1]
  a  <- coef(model_mdd.r[[5]])[2]
  a2 <- coef(model_mdd.r[[5]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# remittance: females with low education
mdd_nomdd.lo.f <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[6]])[1]
  a  <- coef(model_mdd.r[[6]])[2]
  a2 <- coef(model_mdd.r[[6]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# remittance: males with high education
mdd_nomdd.hi.m <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[7]])[1]
  a  <- coef(model_mdd.r[[7]])[2]
  a2 <- coef(model_mdd.r[[7]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 

# remittance: females with high education
mdd_nomdd.hi.f <- function(age,calTime,duration){
  
  b  <- coef(model_mdd.r[[8]])[1]
  a  <- coef(model_mdd.r[[8]])[2]
  a2 <- coef(model_mdd.r[[8]])[3]
  
  rate <- ifelse(age < 18, 0, b + a*age + a2*age^2)
  return(rate)
} 


# mortality rates ---------------------------------------------------------

# mortality is set to 0 in all simulations
mortRates <- function(age,calTime,duration){
  rate <- 0
  return(rate)
}


# MDD MC variance ---------------------------------------------------------

# creating data frames to store results for life course prevalence, 
# mean age of onset, and mean number of years spent with MDD

mc_prev_mdd <- data.frame(matrix(ncol = n_iter + 3, nrow = max_iter*4))
colnames(mc_prev_mdd) <- c(as.character(1:n_iter), "iteration", "education", "gender") 
mc_prev_mdd$iteration <- rep(1:max_iter, each = 4)
mc_prev_mdd$education <- rep(c("High", "High", "Low", "Low"), max_iter)
mc_prev_mdd$gender <- rep(c("Female", "Male", "Female", "Male"), max_iter)

mc_age_mdd <- data.frame(matrix(ncol = n_iter + 3, nrow = max_iter*4))
colnames(mc_age_mdd) <- c(as.character(1:n_iter), "iteration", "education", "gender") 
mc_age_mdd$iteration <- rep(1:max_iter, each = 4)
mc_age_mdd$education <- rep(c("High", "High", "Low", "Low"), max_iter)
mc_age_mdd$gender <- rep(c("Female", "Male", "Female", "Male"), max_iter)

mc_time_mdd <- data.frame(matrix(ncol = n_iter + 3, nrow = max_iter*4))
colnames(mc_time_mdd) <- c(as.character(1:n_iter), "iteration", "education", "gender") 
mc_time_mdd$iteration <- rep(1:max_iter, each = 4)
mc_time_mdd$education <- rep(c("High", "High", "Low", "Low"), max_iter)
mc_time_mdd$gender <- rep(c("Female", "Male", "Female", "Male"), max_iter)

# setting variable to iterate through columns of df that store results
# each column represents a different sample size
col_num <- 1

# for loop to iterate through desired sample sizes
for (N in seq(from = min_N, to = max_N, by = step_size)) {
  # defining initial pop MDD
  ## birth cohort 2000 (they enter simulation in 2018 at age 18)
  set.seed(20210607)
  initBirthDatesRange <- chron(dates=c("1/1/2000","31/12/2000"), format=c(dates="d/m/Y"),out.format=c(dates="d/m/year"))
  birthDates <- dates(initBirthDatesRange[1] + runif(N, min=0, max=diff(initBirthDatesRange)))
  
  # setting minimum and maximum age 
  minage <- 18
  maxage <- 65
  
  # simulation horizon
  simHorizon <- setSimHorizon(startDate="01/01/2018", endDate="31/12/2065")
  
  # initial states: intersection between gender, education level and health status
  states_mdd <- c("nomdd.lo.m","nomdd.lo.f","nomdd.hi.m","nomdd.hi.f",
                  "mdd.lo.m","mdd.lo.f","mdd.hi.m","mdd.hi.f")
  
  states_mdd_sens <- c("nomdd.lo.m","nomdd.lo.f","nomdd.hi.m","nomdd.hi.f",
                       "mdd.lo.m","mdd.lo.f","mdd.hi.m","mdd.hi.f",
                       "nomdd2.lo.m","nomdd2.lo.f","nomdd2.hi.m","nomdd2.hi.f",
                       "mdd2.lo.m","mdd2.lo.f","mdd2.hi.m","mdd2.hi.f")
  
  # absorbing states: dead (must be included even though mortality rate set to zero)
  absStates <- "dead"
  
  # all states 
  stateSpace_mdd <- expand.grid(states_mdd)
  stateSpace_mdd_sens <- expand.grid(states_mdd_sens)
  
  # prevalence of MDD in starting pop depends on a stochastic process driven by 
  # percentages provided by regressions. assume equal distribution among men and 
  # women and high and low education
  m.lo.mdd <- sample(x=c("mdd.lo.m","nomdd.lo.m"),
                     prob=c(.0122,1-.0122),size=N/4,replace=TRUE) 
  
  f.lo.mdd <- sample(x=c("mdd.lo.f","nomdd.lo.f"),
                     prob=c(.0191,1-.0191),size=N/4,replace=TRUE)
  
  m.hi.mdd <- sample(x=c("mdd.hi.m","nomdd.hi.m"),
                     prob=c(.0014,1-.0014),size=N/4,replace=TRUE)
  
  f.hi.mdd <- sample(x=c("mdd.hi.f","nomdd.hi.f"),
                     prob=c(.0022,1-.0022),size=N/4,replace=TRUE)
  
  initStates_mdd <- c(m.lo.mdd, f.lo.mdd, m.hi.mdd, f.hi.mdd)
  
  initPop_mdd <- data.frame(ID=1:N, birthDate=birthDates,initState=initStates_mdd)
  
  # absorbing state for all models. this is needed even though we don't account for mortality
  absTransitions <- c("dead","mortRates")
  
  # MDD observed data: transition pattern and assignment of functions specifying transition rates
  healthTrMatrix_mdd <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                "mdd.lo.m->nomdd.lo.m","mdd.lo.f->nomdd.lo.f",
                                "mdd.hi.m->nomdd.hi.m","mdd.hi.f->nomdd.hi.f"),
                              c("nomdd_mdd.lo.m","nomdd_mdd.lo.f",
                                "nomdd_mdd.hi.m","nomdd_mdd.hi.f",
                                "mdd_nomdd.lo.m","mdd_nomdd.lo.f",
                                "mdd_nomdd.hi.m","mdd_nomdd.hi.f"))
  allTransitions_mdd <- healthTrMatrix_mdd
  transitionMatrix_mdd <- buildTransitionMatrix(allTransitions=allTransitions_mdd,
                                                stateSpace=stateSpace_mdd, 
                                                absTransitions=absTransitions)
  
  # setting variable to iterate through rows of df storing results
  # starts off as 1:4 because there are 4 groups per iteration:
  # females with high education, males with high education, 
  # females with low education, and males with low education
  mc_rows <- 1:4
  
  # for loop to repeat simulation with a set N. 
  # max_iter determines the number of times the analysis is run
  for (iter in 1:max_iter) {
    
    # running simulations -----------------------------------------------------
    seed = N + 100 * iter
    # mdd primary analyses
    simpop_mdd <- micSimParallel(initPop=initPop_mdd, transitionMatrix=transitionMatrix_mdd, absStates=absStates,
                                 maxAge=maxage,simHorizon=simHorizon, cores = n_cores, seeds = seed) 
    
    # converting output to spell format 
    # first duplicating data
    msm_mdd <- simpop_mdd %>% arrange(ID, transitionTime)
    
    # creating index to identify each transition
    msm_mdd <- msm_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
    
    # only selecting first row per individual and adding labels for 
    # gender, education and health status at start of model
    obs1_mdd <- msm_mdd %>%
      filter(index == 1) %>%
      mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
             education = if_else(str_detect(initState, ".lo."), "Low", "High"),
             health = if_else(str_detect(initState, "nomdd."), "NoMDD", "MDD"),
             agestart = minage)
    
    
    # recoding health status 
    msm_mdd$health <-  if_else(str_detect(msm_mdd$To, "nomdd."), "NoMDD", "MDD")
    
    # rearrange data from msm format to spell format (one line per state)
    obs1_mdd <- obs1_mdd %>%
      mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))
    
    gendereduc_mdd <- select(obs1_mdd,ID,gender,education)
    
    obs1_mdd <- obs1_mdd %>% 
      select(ID,agestart,agestop,health)
    
    otherobs_mdd <- list()
    for (i in 1:max(msm_mdd$index)){
      a <- select(filter(msm_mdd,index==i),ID,transitionAge,health)
      a <- rename(a,agestart=transitionAge)
      b <- select(filter(msm_mdd,index==i+1),ID,transitionAge)
      b <- rename(b,agestop=transitionAge)
      
      otherobs_mdd[[i]] <- left_join(a,b,by="ID")
    }
    
    otherobs_mdd <- bind_rows(otherobs_mdd)
    otherobs_mdd$agestop <- ifelse(is.na(otherobs_mdd$agestop),maxage,otherobs_mdd$agestop)
    
    spell_mdd <- bind_rows(obs1_mdd,otherobs_mdd)
    spell_mdd <- spell_mdd %>%
      left_join(gendereduc_mdd, by = "ID") %>%
      arrange(ID, agestart) %>%
      drop_na(.) %>%
      mutate(length = agestop - agestart)
    
    #assign index 
    spell_mdd <- spell_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
    
    #estimating the required metrics
    mc_prev_mdd[mc_rows,col_num] <- spell_mdd %>%
      mutate(health_num = if_else(health == "MDD", 1, 0)) %>%
      # arranges so that instances of MDD appear first
      arrange(health) %>%
      # only keeps first row per individual
      distinct(ID, .keep_all = T) %>%
      group_by(education, gender) %>%
      summarise(prev = mean(health_num) * 100, .groups = "drop")  %>%
      select(prev)
    
    mc_age_mdd[mc_rows,col_num] <- spell_mdd %>%
      # if no MDD when entering cohort then first instance
      # would be recorded at index 2
      filter(index == 2, health == "MDD") %>%
      group_by(education, gender) %>%
      summarise(age_onset = mean(agestart), .groups = "drop") %>%
      select(age_onset)
    
    mc_time_mdd[mc_rows,col_num] <- spell_mdd %>%
      filter(health == "MDD") %>%
      group_by(education, gender) %>%
      summarise(time_with = sum(length)/length(unique(ID)), .groups = "drop") %>%
      select(time_with)
    
    mc_rows <- mc_rows + 4
  }
  
  col_num <- col_num + 1
}

# exporting df, so I can analyze results later
save(mc_prev_mdd, file = paste0(mc_path, "mc_prev_mdd.rda"))
save(mc_age_mdd, file = paste0(mc_path, "mc_age_mdd.rda"))
save(mc_time_mdd, file = paste0(mc_path, "mc_time_mdd.rda"))

# Analyzing the monte carlo variance --------------------------------------

# loading files
load(paste0(mc_path, "mc_prev_mdd.rda"))
load(paste0(mc_path, "mc_age_mdd.rda"))
load(paste0(mc_path, "mc_time_mdd.rda"))

# calculating the standard deviation of the population-level parameters
# across the various sample sizes, and then plotting this
mc_prev_mdd %>%
  group_by(education, gender) %>%
  summarise(across(1:n_iter, ~sd(.)), .groups = "drop") %>%
  pivot_longer(3:ncol(.)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(name) * step_size, y = value, group = education, color = education)) +
  facet_grid(gender ~ .) +
  scale_x_continuous(labels = comma) +
  labs(y = "Standard deviation*", x = "Sample size", 
       colour = "Education")
ggsave(filename = "MDD MC variance prev.pdf", path = "./plots", 
       plot = last_plot(), width = 7, height = 5, unit = "in")

mc_age_mdd %>%
  group_by(education, gender) %>%
  summarise(across(1:n_iter, ~sd(.))) %>%
  pivot_longer(3:ncol(.)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(name) * step_size, y = value, group = education, color = education)) +
  facet_grid(gender ~ .) +
  scale_x_continuous(labels = comma) +
  labs(y = "Standard deviation*", x = "Sample size", 
       colour = "Education")
ggsave(filename = "MDD MC variance onset.pdf", path = "./plots", 
       plot = last_plot(), width = 7, height = 5, unit = "in")

mc_time_mdd %>%
  group_by(education, gender) %>%
  summarise(across(1:n_iter, ~sd(.))) %>%
  pivot_longer(3:ncol(.)) %>%
  ggplot() +
  geom_line(aes(x = as.numeric(name) * step_size, y = value, group = education, color = education)) +
  facet_grid(gender ~ .) +
  scale_x_continuous(labels = comma) +
  labs(y = "Standard deviation*", x = "Sample size", 
       colour = "Education")
ggsave(filename = "MDD MC variance duration.pdf", path = "./plots", 
       plot = last_plot(), width = 7, height = 5, unit = "in")



