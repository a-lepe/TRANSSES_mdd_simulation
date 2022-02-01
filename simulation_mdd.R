# (first run script "parameterization_mdd.R")
# load libraries ----------------------------------------------------------
library(MicSim)
library(tidyverse)
library(writexl)

# setting path to data files
file_path <- "./data_files/" 

# defining initial pop MDD -----------------------------------------------
# Setting sample size to 500K
N <- 10^5 * 5

# birth cohort 2000 (they enter simulation in 2018 at age 18)
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

# creating initial population
initPop_mdd <- data.frame(ID=1:N, birthDate=birthDates,initState=initStates_mdd)

# absorbing state for all models. this is needed even though we don't account for mortality
absTransitions <- c("dead","mortRates")


# transition matrices for primary analysis --------------------------------
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

# MDD counterfactual data smk: transition pattern and assignment of functions specifying transition rates
cf_smk_healthTrMatrix_mdd <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                      "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                      "mdd.lo.m->nomdd.lo.m","mdd.lo.f->nomdd.lo.f",
                                      "mdd.hi.m->nomdd.hi.m","mdd.hi.f->nomdd.hi.f"),
                                    c("cf_smk_nomdd_mdd.lo.m","cf_smk_nomdd_mdd.lo.f",
                                      "cf_smk_nomdd_mdd.hi.m","cf_smk_nomdd_mdd.hi.f",
                                      "cf_smk_mdd_nomdd.lo.m","cf_smk_mdd_nomdd.lo.f",
                                      "cf_smk_mdd_nomdd.hi.m","cf_smk_mdd_nomdd.hi.f"))
cf_smk_allTransitions_mdd <- cf_smk_healthTrMatrix_mdd
cf_smk_transitionMatrix_mdd <- buildTransitionMatrix(allTransitions=cf_smk_allTransitions_mdd,
                                                      stateSpace=stateSpace_mdd,
                                                      absTransitions=absTransitions)

# MDD counterfactual data hl: transition pattern and assignment of functions specifying transition rates
cf_hl_healthTrMatrix_mdd <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                     "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                     "mdd.lo.m->nomdd.lo.m","mdd.lo.f->nomdd.lo.f",
                                     "mdd.hi.m->nomdd.hi.m","mdd.hi.f->nomdd.hi.f"),
                                   c("cf_hl_nomdd_mdd.lo.m","cf_hl_nomdd_mdd.lo.f",
                                     "cf_hl_nomdd_mdd.hi.m","cf_hl_nomdd_mdd.hi.f",
                                     "cf_hl_mdd_nomdd.lo.m","cf_hl_mdd_nomdd.lo.f",
                                     "cf_hl_mdd_nomdd.hi.m","cf_hl_mdd_nomdd.hi.f"))
cf_hl_allTransitions_mdd <- cf_hl_healthTrMatrix_mdd
cf_hl_transitionMatrix_mdd <- buildTransitionMatrix(allTransitions=cf_hl_allTransitions_mdd,
                                                     stateSpace=stateSpace_mdd,
                                                     absTransitions=absTransitions)

# MDD counterfactual data social: transition pattern and assignment of functions specifying transition rates
cf_social_healthTrMatrix_mdd <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                        "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                        "mdd.lo.m->nomdd.lo.m","mdd.lo.f->nomdd.lo.f",
                                        "mdd.hi.m->nomdd.hi.m","mdd.hi.f->nomdd.hi.f"),
                                      c("cf_social_nomdd_mdd.lo.m","cf_social_nomdd_mdd.lo.f",
                                        "cf_social_nomdd_mdd.hi.m","cf_social_nomdd_mdd.hi.f",
                                        "cf_social_mdd_nomdd.lo.m","cf_social_mdd_nomdd.lo.f",
                                        "cf_social_mdd_nomdd.hi.m","cf_social_mdd_nomdd.hi.f"))
cf_social_allTransitions_mdd <- cf_social_healthTrMatrix_mdd
cf_social_transitionMatrix_mdd <- buildTransitionMatrix(allTransitions=cf_social_allTransitions_mdd,
                                                        stateSpace=stateSpace_mdd,
                                                        absTransitions=absTransitions)

# MDD counterfactual data joint: transition pattern and assignment of functions specifying transition rates
cf_joint_healthTrMatrix_mdd <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                        "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                        "mdd.lo.m->nomdd.lo.m","mdd.lo.f->nomdd.lo.f",
                                        "mdd.hi.m->nomdd.hi.m","mdd.hi.f->nomdd.hi.f"),
                                      c("cf_joint_nomdd_mdd.lo.m","cf_joint_nomdd_mdd.lo.f",
                                        "cf_joint_nomdd_mdd.hi.m","cf_joint_nomdd_mdd.hi.f",
                                        "cf_joint_mdd_nomdd.lo.m","cf_joint_mdd_nomdd.lo.f",
                                        "cf_joint_mdd_nomdd.hi.m","cf_joint_mdd_nomdd.hi.f"))
cf_joint_allTransitions_mdd <- cf_joint_healthTrMatrix_mdd
cf_joint_transitionMatrix_mdd <- buildTransitionMatrix(allTransitions=cf_joint_allTransitions_mdd,
                                                        stateSpace=stateSpace_mdd,
                                                        absTransitions=absTransitions)

rm(healthTrMatrix_mdd, allTransitions_mdd,
   cf_smk_healthTrMatrix_mdd, cf_smk_allTransitions_mdd,
   cf_hl_healthTrMatrix_mdd, cf_hl_allTransitions_mdd,
   cf_social_healthTrMatrix_mdd, cf_social_allTransitions_mdd,
   cf_joint_healthTrMatrix_mdd, cf_joint_allTransitions_mdd)

# transition matrix for sensitivity analyses ------------------------------
# MDD observed data: transition pattern and assignment of functions specifying transition rates
healthTrMatrix_mdd_sens <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                   "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                   "mdd.lo.m->nomdd2.lo.m","mdd.lo.f->nomdd2.lo.f",
                                   "mdd.hi.m->nomdd2.hi.m","mdd.hi.f->nomdd2.hi.f",
                                   "nomdd2.lo.m->mdd2.lo.m","nomdd2.lo.f->mdd2.lo.f",
                                   "nomdd2.hi.m->mdd2.hi.m","nomdd2.hi.f->mdd2.hi.f",
                                   "mdd2.lo.m->nomdd2.lo.m","mdd2.lo.f->nomdd2.lo.f",
                                   "mdd2.hi.m->nomdd2.hi.m","mdd2.hi.f->nomdd2.hi.f"),
                                 c("nomdd_mdd.lo.m","nomdd_mdd.lo.f",
                                   "nomdd_mdd.hi.m","nomdd_mdd.hi.f",
                                   "mdd_nomdd.lo.m","mdd_nomdd.lo.f",
                                   "mdd_nomdd.hi.m","mdd_nomdd.hi.f",
                                   "nomdd_mdd.sens.lo.m","nomdd_mdd.sens.lo.f",
                                   "nomdd_mdd.sens.hi.m","nomdd_mdd.sens.hi.f",
                                   "mdd_nomdd.sens.lo.m","mdd_nomdd.sens.lo.f",
                                   "mdd_nomdd.sens.hi.m","mdd_nomdd.sens.hi.f"))
allTransitions_mdd_sens <- healthTrMatrix_mdd_sens
transitionMatrix_mdd_sens <- buildTransitionMatrix(allTransitions=allTransitions_mdd_sens,
                                                   stateSpace=stateSpace_mdd_sens, 
                                                   absTransitions=absTransitions)

# MDD counterfactual data smk: transition pattern and assignment of functions specifying transition rates
cf_smk_healthTrMatrix_mdd_sens <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                          "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                          "mdd.lo.m->nomdd2.lo.m","mdd.lo.f->nomdd2.lo.f",
                                          "mdd.hi.m->nomdd2.hi.m","mdd.hi.f->nomdd2.hi.f",
                                          "nomdd2.lo.m->mdd2.lo.m","nomdd2.lo.f->mdd2.lo.f",
                                          "nomdd2.hi.m->mdd2.hi.m","nomdd2.hi.f->mdd2.hi.f",
                                          "mdd2.lo.m->nomdd2.lo.m","mdd2.lo.f->nomdd2.lo.f",
                                          "mdd2.hi.m->nomdd2.hi.m","mdd2.hi.f->nomdd2.hi.f"),
                                        c("cf_smk_nomdd_mdd.lo.m","cf_smk_nomdd_mdd.lo.f",
                                          "cf_smk_nomdd_mdd.hi.m","cf_smk_nomdd_mdd.hi.f",
                                          "cf_smk_mdd_nomdd.lo.m","cf_smk_mdd_nomdd.lo.f",
                                          "cf_smk_mdd_nomdd.hi.m","cf_smk_mdd_nomdd.hi.f",
                                          "cf_smk_nomdd_mdd.sens.lo.m","cf_smk_nomdd_mdd.sens.lo.f",
                                          "cf_smk_nomdd_mdd.sens.hi.m","cf_smk_nomdd_mdd.sens.hi.f",
                                          "cf_smk_mdd_nomdd.sens.lo.m","cf_smk_mdd_nomdd.sens.lo.f",
                                          "cf_smk_mdd_nomdd.sens.hi.m","cf_smk_mdd_nomdd.sens.hi.f"))
cf_smk_allTransitions_mdd_sens <- cf_smk_healthTrMatrix_mdd_sens
cf_smk_transitionMatrix_mdd_sens <- buildTransitionMatrix(allTransitions=cf_smk_allTransitions_mdd_sens,
                                                          stateSpace=stateSpace_mdd_sens,
                                                          absTransitions=absTransitions)

# MDD counterfactual data hl: transition pattern and assignment of functions specifying transition rates
cf_hl_healthTrMatrix_mdd_sens <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                         "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                         "mdd.lo.m->nomdd2.lo.m","mdd.lo.f->nomdd2.lo.f",
                                         "mdd.hi.m->nomdd2.hi.m","mdd.hi.f->nomdd2.hi.f",
                                         "nomdd2.lo.m->mdd2.lo.m","nomdd2.lo.f->mdd2.lo.f",
                                         "nomdd2.hi.m->mdd2.hi.m","nomdd2.hi.f->mdd2.hi.f",
                                         "mdd2.lo.m->nomdd2.lo.m","mdd2.lo.f->nomdd2.lo.f",
                                         "mdd2.hi.m->nomdd2.hi.m","mdd2.hi.f->nomdd2.hi.f"),
                                       c("cf_hl_nomdd_mdd.lo.m","cf_hl_nomdd_mdd.lo.f",
                                         "cf_hl_nomdd_mdd.hi.m","cf_hl_nomdd_mdd.hi.f",
                                         "cf_hl_mdd_nomdd.lo.m","cf_hl_mdd_nomdd.lo.f",
                                         "cf_hl_mdd_nomdd.hi.m","cf_hl_mdd_nomdd.hi.f",
                                         "cf_hl_nomdd_mdd.sens.lo.m","cf_hl_nomdd_mdd.sens.lo.f",
                                         "cf_hl_nomdd_mdd.sens.hi.m","cf_hl_nomdd_mdd.sens.hi.f",
                                         "cf_hl_mdd_nomdd.sens.lo.m","cf_hl_mdd_nomdd.sens.lo.f",
                                         "cf_hl_mdd_nomdd.sens.hi.m","cf_hl_mdd_nomdd.sens.hi.f"))
cf_hl_allTransitions_mdd_sens <- cf_hl_healthTrMatrix_mdd_sens
cf_hl_transitionMatrix_mdd_sens <- buildTransitionMatrix(allTransitions=cf_hl_allTransitions_mdd_sens,
                                                         stateSpace=stateSpace_mdd_sens,
                                                         absTransitions=absTransitions)

# MDD counterfactual data social: transition pattern and assignment of functions specifying transition rates
cf_social_healthTrMatrix_mdd_sens <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                             "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                             "mdd.lo.m->nomdd2.lo.m","mdd.lo.f->nomdd2.lo.f",
                                             "mdd.hi.m->nomdd2.hi.m","mdd.hi.f->nomdd2.hi.f",
                                             "nomdd2.lo.m->mdd2.lo.m","nomdd2.lo.f->mdd2.lo.f",
                                             "nomdd2.hi.m->mdd2.hi.m","nomdd2.hi.f->mdd2.hi.f",
                                             "mdd2.lo.m->nomdd2.lo.m","mdd2.lo.f->nomdd2.lo.f",
                                             "mdd2.hi.m->nomdd2.hi.m","mdd2.hi.f->nomdd2.hi.f"),
                                           c("cf_social_nomdd_mdd.lo.m","cf_social_nomdd_mdd.lo.f",
                                             "cf_social_nomdd_mdd.hi.m","cf_social_nomdd_mdd.hi.f",
                                             "cf_social_mdd_nomdd.lo.m","cf_social_mdd_nomdd.lo.f",
                                             "cf_social_mdd_nomdd.hi.m","cf_social_mdd_nomdd.hi.f",
                                             "cf_social_nomdd_mdd.sens.lo.m","cf_social_nomdd_mdd.sens.lo.f",
                                             "cf_social_nomdd_mdd.sens.hi.m","cf_social_nomdd_mdd.sens.hi.f",
                                             "cf_social_mdd_nomdd.sens.lo.m","cf_social_mdd_nomdd.sens.lo.f",
                                             "cf_social_mdd_nomdd.sens.hi.m","cf_social_mdd_nomdd.sens.hi.f"))
cf_social_allTransitions_mdd_sens <- cf_social_healthTrMatrix_mdd_sens
cf_social_transitionMatrix_mdd_sens <- buildTransitionMatrix(allTransitions=cf_social_allTransitions_mdd_sens,
                                                             stateSpace=stateSpace_mdd_sens,
                                                             absTransitions=absTransitions)

# MDD counterfactual data joint: transition pattern and assignment of functions specifying transition rates
cf_joint_healthTrMatrix_mdd_sens <- cbind(c("nomdd.lo.m->mdd.lo.m","nomdd.lo.f->mdd.lo.f",
                                            "nomdd.hi.m->mdd.hi.m","nomdd.hi.f->mdd.hi.f",
                                            "mdd.lo.m->nomdd2.lo.m","mdd.lo.f->nomdd2.lo.f",
                                            "mdd.hi.m->nomdd2.hi.m","mdd.hi.f->nomdd2.hi.f",
                                            "nomdd2.lo.m->mdd2.lo.m","nomdd2.lo.f->mdd2.lo.f",
                                            "nomdd2.hi.m->mdd2.hi.m","nomdd2.hi.f->mdd2.hi.f",
                                            "mdd2.lo.m->nomdd2.lo.m","mdd2.lo.f->nomdd2.lo.f",
                                            "mdd2.hi.m->nomdd2.hi.m","mdd2.hi.f->nomdd2.hi.f"),
                                          c("cf_joint_nomdd_mdd.lo.m","cf_joint_nomdd_mdd.lo.f",
                                            "cf_joint_nomdd_mdd.hi.m","cf_joint_nomdd_mdd.hi.f",
                                            "cf_joint_mdd_nomdd.lo.m","cf_joint_mdd_nomdd.lo.f",
                                            "cf_joint_mdd_nomdd.hi.m","cf_joint_mdd_nomdd.hi.f",
                                            "cf_joint_nomdd_mdd.sens.lo.m","cf_joint_nomdd_mdd.sens.lo.f",
                                            "cf_joint_nomdd_mdd.sens.hi.m","cf_joint_nomdd_mdd.sens.hi.f",
                                            "cf_joint_mdd_nomdd.sens.lo.m","cf_joint_mdd_nomdd.sens.lo.f",
                                            "cf_joint_mdd_nomdd.sens.hi.m","cf_joint_mdd_nomdd.sens.hi.f"))
cf_joint_allTransitions_mdd_sens <- cf_joint_healthTrMatrix_mdd_sens
cf_joint_transitionMatrix_mdd_sens <- buildTransitionMatrix(allTransitions=cf_joint_allTransitions_mdd_sens,
                                                            stateSpace=stateSpace_mdd_sens,
                                                            absTransitions=absTransitions)

rm(healthTrMatrix_mdd_sens, allTransitions_mdd_sens,
   cf_smk_healthTrMatrix_mdd_sens, cf_smk_allTransitions_mdd_sens,
   cf_hl_healthTrMatrix_mdd_sens, cf_hl_allTransitions_mdd_sens,
   cf_social_healthTrMatrix_mdd_sens, cf_social_allTransitions_mdd_sens,
   cf_joint_healthTrMatrix_mdd_sens, cf_joint_allTransitions_mdd_sens, 
   stateSpace_mdd_sens, absTransitions)


# running simulations -----------------------------------------------------
# supplying the micsim function with the required input created in the code above

# mdd primary analyses
simpop_mdd <- micSimParallel(initPop=initPop_mdd, transitionMatrix=transitionMatrix_mdd, 
                             absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                             cores = 80, seeds = 20210607) 

cf_smk_simpop_mdd <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_smk_transitionMatrix_mdd, 
                                     absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                     cores = 80, seeds = 20210607) 

cf_hl_simpop_mdd <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_hl_transitionMatrix_mdd, 
                                    absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                    cores = 80, seeds = 20210607) 

cf_social_simpop_mdd <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_social_transitionMatrix_mdd, 
                                       absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                       cores = 80, seeds = 20210607) 

cf_joint_simpop_mdd <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_joint_transitionMatrix_mdd, 
                                       absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                       cores = 80, seeds = 20210607)

# mdd sensitivity analyses
simpop_mdd_sens <- micSimParallel(initPop=initPop_mdd, transitionMatrix=transitionMatrix_mdd_sens, 
                                  absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                  cores = 80, seeds = 20210607) 

cf_smk_simpop_mdd_sens <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_smk_transitionMatrix_mdd_sens, 
                                         absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                         cores = 80, seeds = 20210607) 

cf_hl_simpop_mdd_sens <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_hl_transitionMatrix_mdd_sens, 
                                        absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                        cores = 80, seeds = 20210607) 

cf_social_simpop_mdd_sens <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_social_transitionMatrix_mdd_sens, 
                                            absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                            cores = 80, seeds = 20210607) 

cf_joint_simpop_mdd_sens <- micSimParallel(initPop=initPop_mdd, transitionMatrix=cf_joint_transitionMatrix_mdd_sens, 
                                           absStates=absStates, maxAge=maxage, simHorizon=simHorizon, 
                                           cores = 80, seeds = 20210607)


# converting output to spell format ---------------------------------------
# preparing datasets: prefix "cf_XXX_" gives which counterfactual this is

# saving/loading output of simulation, so it doesn't have to be rerun delete "#" if want to save
save(simpop_mdd, file = paste0(file_path, "simpop_mdd.rda"))
save(cf_smk_simpop_mdd, file = paste0(file_path, "cf_smk_simpop_mdd.rda"))
save(cf_hl_simpop_mdd, file = paste0(file_path, "cf_hl_simpop_mdd.rda"))
save(cf_social_simpop_mdd, file = paste0(file_path, "cf_social_simpop_mdd.rda"))
save(cf_joint_simpop_mdd, file = paste0(file_path, "cf_joint_simpop_mdd.rda"))

save(simpop_mdd_sens, file = paste0(file_path, "simpop_mdd_sens.rda"))
save(cf_smk_simpop_mdd_sens, file = paste0(file_path, "cf_smk_simpop_mdd_sens.rda"))
save(cf_hl_simpop_mdd_sens, file = paste0(file_path, "cf_hl_simpop_mdd_sens.rda"))
save(cf_social_simpop_mdd_sens, file = paste0(file_path, "cf_social_simpop_mdd_sens.rda"))
save(cf_joint_simpop_mdd_sens, file = paste0(file_path, "cf_joint_simpop_mdd_sens.rda"))

load(paste0(file_path, "simpop_mdd.rda"))
load(paste0(file_path, "cf_smk_simpop_mdd.rda"))
load(paste0(file_path, "cf_hl_simpop_mdd.rda"))
load(paste0(file_path, "cf_social_simpop_mdd.rda"))
load(paste0(file_path, "cf_joint_simpop_mdd.rda"))

load(paste0(file_path, "simpop_mdd_sens.rda"))
load(paste0(file_path, "cf_smk_simpop_mdd_sens.rda"))
load(paste0(file_path, "cf_hl_simpop_mdd_sens.rda"))
load(paste0(file_path, "cf_social_simpop_mdd_sens.rda"))
load(paste0(file_path, "cf_joint_simpop_mdd_sens.rda"))

# first duplicating data
msm_mdd <- simpop_mdd %>% arrange(ID, transitionTime)
cf_smk_msm_mdd <- cf_smk_simpop_mdd %>% arrange(ID, transitionTime)
cf_hl_msm_mdd <- cf_hl_simpop_mdd %>% arrange(ID, transitionTime)
cf_social_msm_mdd <- cf_social_simpop_mdd %>% arrange(ID, transitionTime)
cf_joint_msm_mdd <- cf_joint_simpop_mdd %>% arrange(ID, transitionTime)

msm_mdd_sens <- simpop_mdd_sens %>% arrange(ID, transitionTime)
cf_smk_msm_mdd_sens <- cf_smk_simpop_mdd_sens %>% arrange(ID, transitionTime)
cf_hl_msm_mdd_sens <- cf_hl_simpop_mdd_sens %>% arrange(ID, transitionTime)
cf_social_msm_mdd_sens <- cf_social_simpop_mdd_sens %>% arrange(ID, transitionTime)
cf_joint_msm_mdd_sens <- cf_joint_simpop_mdd_sens %>% arrange(ID, transitionTime)

# creating index to identify each transition
msm_mdd <- msm_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_smk_msm_mdd <- cf_smk_msm_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_hl_msm_mdd <- cf_hl_msm_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_social_msm_mdd <- cf_social_msm_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_joint_msm_mdd <- cf_joint_msm_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()

msm_mdd_sens <- msm_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_smk_msm_mdd_sens <- cf_smk_msm_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_hl_msm_mdd_sens <- cf_hl_msm_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_social_msm_mdd_sens <- cf_social_msm_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_joint_msm_mdd_sens <- cf_joint_msm_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()

# only selecting first row per individual and adding labels for 
# gender, education and health status at start of model
obs1_mdd <- msm_mdd %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd."), "NoMDD", "MDD"),
         agestart = minage)

cf_smk_obs1_mdd <- cf_smk_msm_mdd %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd."), "NoMDD", "MDD"),
         agestart = minage)

cf_hl_obs1_mdd <- cf_hl_msm_mdd %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd."), "NoMDD", "MDD"),
         agestart = minage)

cf_social_obs1_mdd <- cf_social_msm_mdd %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd."), "NoMDD", "MDD"),
         agestart = minage)

cf_joint_obs1_mdd <- cf_joint_msm_mdd %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd."), "NoMDD", "MDD"),
         agestart = minage)

obs1_mdd_sens <- msm_mdd_sens %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd"), "NoMDD", "MDD"),
         agestart = minage)

cf_smk_obs1_mdd_sens <- cf_smk_msm_mdd_sens %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd"), "NoMDD", "MDD"),
         agestart = minage)

cf_hl_obs1_mdd_sens <- cf_hl_msm_mdd_sens %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd"), "NoMDD", "MDD"),
         agestart = minage)

cf_social_obs1_mdd_sens <- cf_social_msm_mdd_sens %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd"), "NoMDD", "MDD"),
         agestart = minage)

cf_joint_obs1_mdd_sens <- cf_joint_msm_mdd_sens %>%
  filter(index == 1) %>%
  mutate(gender = if_else(str_detect(initState, ".m$"), "Male", "Female"), 
         education = if_else(str_detect(initState, ".lo."), "Low", "High"),
         health = if_else(str_detect(initState, "nomdd"), "NoMDD", "MDD"),
         agestart = minage)

# recoding health status 
msm_mdd$health <-  if_else(str_detect(msm_mdd$To, "nomdd."), "NoMDD", "MDD")
cf_smk_msm_mdd$health <-  if_else(str_detect(cf_smk_msm_mdd$To, "nomdd."), "NoMDD", "MDD")
cf_hl_msm_mdd$health <-  if_else(str_detect(cf_hl_msm_mdd$To, "nomdd."), "NoMDD", "MDD")
cf_social_msm_mdd$health <-  if_else(str_detect(cf_social_msm_mdd$To, "nomdd."), "NoMDD", "MDD")
cf_joint_msm_mdd$health <-  if_else(str_detect(cf_joint_msm_mdd$To, "nomdd."), "NoMDD", "MDD")

msm_mdd_sens$health <-  if_else(str_detect(msm_mdd_sens$To, "nomdd"), "NoMDD", "MDD")
cf_smk_msm_mdd_sens$health <-  if_else(str_detect(cf_smk_msm_mdd_sens$To, "nomdd"), "NoMDD", "MDD")
cf_hl_msm_mdd_sens$health <-  if_else(str_detect(cf_hl_msm_mdd_sens$To, "nomdd"), "NoMDD", "MDD")
cf_social_msm_mdd_sens$health <-  if_else(str_detect(cf_social_msm_mdd_sens$To, "nomdd"), "NoMDD", "MDD")
cf_joint_msm_mdd_sens$health <-  if_else(str_detect(cf_joint_msm_mdd_sens$To, "nomdd"), "NoMDD", "MDD")

# rearrange data from msm format to spell format (one line per state)
# primary analyses
# mdd observed
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

# mdd counterfactual smk
cf_smk_obs1_mdd <- cf_smk_obs1_mdd %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_smk_gendereduc_mdd <- select(cf_smk_obs1_mdd,ID,gender,education)

cf_smk_obs1_mdd <- select(cf_smk_obs1_mdd,ID,agestart,agestop,health)

cf_smk_otherobs_mdd <- list()
for (i in 1:max(cf_smk_msm_mdd$index)){
  a <- select(filter(cf_smk_msm_mdd,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_smk_msm_mdd,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_smk_otherobs_mdd[[i]] <- left_join(a,b,by="ID")
}

cf_smk_otherobs_mdd <- bind_rows(cf_smk_otherobs_mdd)
cf_smk_otherobs_mdd$agestop <- ifelse(is.na(cf_smk_otherobs_mdd$agestop),maxage,cf_smk_otherobs_mdd$agestop)

cf_smk_spell_mdd <- bind_rows(cf_smk_obs1_mdd,cf_smk_otherobs_mdd)
cf_smk_spell_mdd <- cf_smk_spell_mdd %>%
  left_join(gendereduc_mdd, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual hl
cf_hl_obs1_mdd <- cf_hl_obs1_mdd %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_hl_gendereduc_mdd <- select(cf_hl_obs1_mdd,ID,gender,education)

cf_hl_obs1_mdd <- select(cf_hl_obs1_mdd,ID,agestart,agestop,health)

cf_hl_otherobs_mdd <- list()
for (i in 1:max(cf_hl_msm_mdd$index)){
  a <- select(filter(cf_hl_msm_mdd,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_hl_msm_mdd,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_hl_otherobs_mdd[[i]] <- left_join(a,b,by="ID")
}

cf_hl_otherobs_mdd <- bind_rows(cf_hl_otherobs_mdd)
cf_hl_otherobs_mdd$agestop <- ifelse(is.na(cf_hl_otherobs_mdd$agestop),maxage,cf_hl_otherobs_mdd$agestop)

cf_hl_spell_mdd <- bind_rows(cf_hl_obs1_mdd,cf_hl_otherobs_mdd)
cf_hl_spell_mdd <- cf_hl_spell_mdd %>%
  left_join(gendereduc_mdd, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual social
cf_social_obs1_mdd <- cf_social_obs1_mdd %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_social_gendereduc_mdd <- select(cf_social_obs1_mdd,ID,gender,education)

cf_social_obs1_mdd <- select(cf_social_obs1_mdd,ID,agestart,agestop,health)

cf_social_otherobs_mdd <- list()
for (i in 1:max(cf_social_msm_mdd$index)){
  a <- select(filter(cf_social_msm_mdd,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_social_msm_mdd,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_social_otherobs_mdd[[i]] <- left_join(a,b,by="ID")
}

cf_social_otherobs_mdd <- bind_rows(cf_social_otherobs_mdd)
cf_social_otherobs_mdd$agestop <- ifelse(is.na(cf_social_otherobs_mdd$agestop),maxage,cf_social_otherobs_mdd$agestop)

cf_social_spell_mdd <- bind_rows(cf_social_obs1_mdd,cf_social_otherobs_mdd)
cf_social_spell_mdd <- cf_social_spell_mdd %>%
  left_join(gendereduc_mdd, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual joint
cf_joint_obs1_mdd <- cf_joint_obs1_mdd %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_joint_gendereduc_mdd <- select(cf_joint_obs1_mdd,ID,gender,education)

cf_joint_obs1_mdd <- select(cf_joint_obs1_mdd,ID,agestart,agestop,health)

cf_joint_otherobs_mdd <- list()
for (i in 1:max(cf_joint_msm_mdd$index)){
  a <- select(filter(cf_joint_msm_mdd,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_joint_msm_mdd,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_joint_otherobs_mdd[[i]] <- left_join(a,b,by="ID")
}

cf_joint_otherobs_mdd <- bind_rows(cf_joint_otherobs_mdd)
cf_joint_otherobs_mdd$agestop <- ifelse(is.na(cf_joint_otherobs_mdd$agestop),maxage,cf_joint_otherobs_mdd$agestop)

cf_joint_spell_mdd <- bind_rows(cf_joint_obs1_mdd,cf_joint_otherobs_mdd)
cf_joint_spell_mdd <- cf_joint_spell_mdd %>%
  left_join(gendereduc_mdd, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# sensitivity analyses
# mdd observed
obs1_mdd_sens <- obs1_mdd_sens %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

gendereduc_mdd_sens <- select(obs1_mdd_sens,ID,gender,education)

obs1_mdd_sens <- obs1_mdd_sens %>% 
  select(ID,agestart,agestop,health)

otherobs_mdd_sens <- list()
for (i in 1:max(msm_mdd_sens$index)){
  a <- select(filter(msm_mdd_sens,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(msm_mdd_sens,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  otherobs_mdd_sens[[i]] <- left_join(a,b,by="ID")
}

otherobs_mdd_sens <- bind_rows(otherobs_mdd_sens)
otherobs_mdd_sens$agestop <- ifelse(is.na(otherobs_mdd_sens$agestop),maxage,otherobs_mdd_sens$agestop)

spell_mdd_sens <- bind_rows(obs1_mdd_sens,otherobs_mdd_sens)
spell_mdd_sens <- spell_mdd_sens %>%
  left_join(gendereduc_mdd_sens, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual smk
cf_smk_obs1_mdd_sens <- cf_smk_obs1_mdd_sens %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_smk_gendereduc_mdd_sens <- select(cf_smk_obs1_mdd_sens,ID,gender,education)

cf_smk_obs1_mdd_sens <- select(cf_smk_obs1_mdd_sens,ID,agestart,agestop,health)

cf_smk_otherobs_mdd_sens <- list()
for (i in 1:max(cf_smk_msm_mdd_sens$index)){
  a <- select(filter(cf_smk_msm_mdd_sens,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_smk_msm_mdd_sens,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_smk_otherobs_mdd_sens[[i]] <- left_join(a,b,by="ID")
}

cf_smk_otherobs_mdd_sens <- bind_rows(cf_smk_otherobs_mdd_sens)
cf_smk_otherobs_mdd_sens$agestop <- ifelse(is.na(cf_smk_otherobs_mdd_sens$agestop),maxage,cf_smk_otherobs_mdd_sens$agestop)

cf_smk_spell_mdd_sens <- bind_rows(cf_smk_obs1_mdd_sens,cf_smk_otherobs_mdd_sens)
cf_smk_spell_mdd_sens <- cf_smk_spell_mdd_sens %>%
  left_join(gendereduc_mdd_sens, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual hl
cf_hl_obs1_mdd_sens <- cf_hl_obs1_mdd_sens %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_hl_gendereduc_mdd_sens <- select(cf_hl_obs1_mdd_sens,ID,gender,education)

cf_hl_obs1_mdd_sens <- select(cf_hl_obs1_mdd_sens,ID,agestart,agestop,health)

cf_hl_otherobs_mdd_sens <- list()
for (i in 1:max(cf_hl_msm_mdd_sens$index)){
  a <- select(filter(cf_hl_msm_mdd_sens,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_hl_msm_mdd_sens,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_hl_otherobs_mdd_sens[[i]] <- left_join(a,b,by="ID")
}

cf_hl_otherobs_mdd_sens <- bind_rows(cf_hl_otherobs_mdd_sens)
cf_hl_otherobs_mdd_sens$agestop <- ifelse(is.na(cf_hl_otherobs_mdd_sens$agestop),maxage,cf_hl_otherobs_mdd_sens$agestop)

cf_hl_spell_mdd_sens <- bind_rows(cf_hl_obs1_mdd_sens,cf_hl_otherobs_mdd_sens)
cf_hl_spell_mdd_sens <- cf_hl_spell_mdd_sens %>%
  left_join(gendereduc_mdd_sens, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual social
cf_social_obs1_mdd_sens <- cf_social_obs1_mdd_sens %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_social_gendereduc_mdd_sens <- select(cf_social_obs1_mdd_sens,ID,gender,education)

cf_social_obs1_mdd_sens <- select(cf_social_obs1_mdd_sens,ID,agestart,agestop,health)

cf_social_otherobs_mdd_sens <- list()
for (i in 1:max(cf_social_msm_mdd_sens$index)){
  a <- select(filter(cf_social_msm_mdd_sens,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_social_msm_mdd_sens,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_social_otherobs_mdd_sens[[i]] <- left_join(a,b,by="ID")
}

cf_social_otherobs_mdd_sens <- bind_rows(cf_social_otherobs_mdd_sens)
cf_social_otherobs_mdd_sens$agestop <- ifelse(is.na(cf_social_otherobs_mdd_sens$agestop),maxage,cf_social_otherobs_mdd_sens$agestop)

cf_social_spell_mdd_sens <- bind_rows(cf_social_obs1_mdd_sens,cf_social_otherobs_mdd_sens)
cf_social_spell_mdd_sens <- cf_social_spell_mdd_sens %>%
  left_join(gendereduc_mdd_sens, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

# mdd counterfactual joint
cf_joint_obs1_mdd_sens <- cf_joint_obs1_mdd_sens %>%
  mutate(agestop = if_else(is.na(transitionAge), maxage, transitionAge))

cf_joint_gendereduc_mdd_sens <- select(cf_joint_obs1_mdd_sens,ID,gender,education)

cf_joint_obs1_mdd_sens <- select(cf_joint_obs1_mdd_sens,ID,agestart,agestop,health)

cf_joint_otherobs_mdd_sens <- list()
for (i in 1:max(cf_joint_msm_mdd_sens$index)){
  a <- select(filter(cf_joint_msm_mdd_sens,index==i),ID,transitionAge,health)
  a <- rename(a,agestart=transitionAge)
  b <- select(filter(cf_joint_msm_mdd_sens,index==i+1),ID,transitionAge)
  b <- rename(b,agestop=transitionAge)
  
  cf_joint_otherobs_mdd_sens[[i]] <- left_join(a,b,by="ID")
}

cf_joint_otherobs_mdd_sens <- bind_rows(cf_joint_otherobs_mdd_sens)
cf_joint_otherobs_mdd_sens$agestop <- ifelse(is.na(cf_joint_otherobs_mdd_sens$agestop),maxage,cf_joint_otherobs_mdd_sens$agestop)

cf_joint_spell_mdd_sens <- bind_rows(cf_joint_obs1_mdd_sens,cf_joint_otherobs_mdd_sens)
cf_joint_spell_mdd_sens <- cf_joint_spell_mdd_sens %>%
  left_join(gendereduc_mdd_sens, by = "ID") %>%
  arrange(ID, agestart) %>%
  drop_na(.) %>%
  mutate(length = agestop - agestart)

#assign index for each state per individual
spell_mdd <- spell_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_smk_spell_mdd <- cf_smk_spell_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_hl_spell_mdd <- cf_hl_spell_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_social_spell_mdd <- cf_social_spell_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_joint_spell_mdd <- cf_joint_spell_mdd %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()

spell_mdd_sens <- spell_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_smk_spell_mdd_sens <- cf_smk_spell_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_hl_spell_mdd_sens <- cf_hl_spell_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_social_spell_mdd_sens <- cf_social_spell_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()
cf_joint_spell_mdd_sens <- cf_joint_spell_mdd_sens %>% group_by(ID) %>% mutate(index=1:n()) %>% ungroup()

# merging results from the simulations and creating a variables sim and analysis 
# to differentiate the simulations. table order variable is meant to help with
# formatting of output when summarizing results later on
mydata_mdd <- bind_rows(mutate(spell_mdd, sim = "observed", analysis = "Primary"), 
                        mutate(cf_smk_spell_mdd, sim = "counterfactual smoking", analysis = "Primary"),
                        mutate(cf_hl_spell_mdd, sim = "counterfactual health literacy", analysis = "Primary"),
                        mutate(cf_social_spell_mdd, sim = "counterfactual social", analysis = "Primary"),
                        mutate(cf_joint_spell_mdd, sim = "counterfactual joint", analysis = "Primary"),
                        mutate(spell_mdd_sens, sim = "observed", analysis = "Sensitivity"), 
                        mutate(cf_smk_spell_mdd_sens, sim = "counterfactual smoking", analysis = "Sensitivity"),
                        mutate(cf_hl_spell_mdd_sens, sim = "counterfactual health literacy", analysis = "Sensitivity"),
                        mutate(cf_social_spell_mdd_sens, sim = "counterfactual social", analysis = "Sensitivity"),
                        mutate(cf_joint_spell_mdd_sens, sim = "counterfactual joint", analysis = "Sensitivity")) %>%
  mutate(sim = factor(sim, levels = c("observed", "counterfactual smoking",
                                      "counterfactual health literacy", "counterfactual social",
                                      "counterfactual joint")),
         analysis = factor(analysis, levels = c("Primary", "Sensitivity")),
         table_order = case_when(analysis == "Primary" & sim == "observed" & education == "High" ~ 0,
                           analysis == "Primary" & sim == "observed" & education == "Low"  ~ 1,
                           analysis == "Primary" & sim == "counterfactual smoking"  & education == "Low"  ~ 2,
                           analysis == "Primary" & sim == "counterfactual health literacy"  & education == "Low"  ~ 3,
                           analysis == "Primary" & sim == "counterfactual social"  & education == "Low"  ~ 4,
                           analysis == "Primary" & sim == "counterfactual joint"  & education == "Low"  ~ 5,
                           analysis == "Sensitivity" & sim == "observed" & education == "High" ~ 6,
                           analysis == "Sensitivity" & sim == "observed" & education == "Low"  ~ 7,
                           analysis == "Sensitivity" & sim == "counterfactual smoking"  & education == "Low"  ~ 8,
                           analysis == "Sensitivity" & sim == "counterfactual health literacy"  & education == "Low"  ~ 9,
                           analysis == "Sensitivity" & sim == "counterfactual social"  & education == "Low"  ~ 10,
                           analysis == "Sensitivity" & sim == "counterfactual joint"  & education == "Low"  ~ 11))

# exporting data so I can reload it later on
save(mydata_mdd, file = paste0(file_path, "mydata_mdd.rda"))

load(paste0(file_path, "mydata_mdd.rda"))

# Analyze results ---------------------------------------------------------
# Prevalence --------------------------------------------------------------
# life course prevalence aka proportion who ever had MDD. filtering by
# index <= 2 to avoid duplicate individuals. 
lc_prev_mdd <- mydata_mdd %>%
  filter(index <= 2, health == "MDD", !is.na(table_order)) %>% 
  group_by(analysis, sim, education, gender) %>%
  summarise(proportion_MDD = n()/(N*.25) * 100, .groups = "drop") %>%
  pivot_wider(names_from = c(analysis, gender), names_glue = "{analysis} {gender} (n%)", 
              values_from = proportion_MDD);lc_prev_mdd

# reformatting data frames to use in bar plots later on
lc_prev_mdd_diff <- lc_prev_mdd %>%
  mutate(across(where(is.numeric), ~ .x - first(.))) %>%
  filter(education == "Low") %>% 
  rename(Primary_Female = `Primary Female (n%)`, 
         Primary_Male = `Primary Male (n%)`,
         Sensitivity_Female = `Sensitivity Female (n%)`, 
         Sensitivity_Male = `Sensitivity Male (n%)`) %>%
  pivot_longer(where(is.numeric), 
               names_to = c("analysis","sex"),
               names_pattern = "(.+)_(.+)",
               values_to = "lc_prev"); lc_prev_mdd_diff 

# plotting the prevalence of MDD at each age by sex and education
# using observed data
tmp_data <- mydata_mdd %>%
  filter(analysis == "Primary", sim == "observed")

# initialize list to store results, and then estimating the age-
# specific prevalence rates in yearly increments from the 
# minimum to maximum age. 
prev_mdd <- list()
for (i in minage:maxage){
  prev_mdd[[i]] <- tmp_data %>%
    filter((health == "MDD" & agestart <= i & agestop >= i)) %>%
    group_by(gender, education) %>%
    summarise(prev = n()/(N*.25) * 100, .groups = "keep") %>%
    mutate(age = i) %>%
    ungroup()
}

# merging prevalence estimates into one data frame
prev_mdd <- bind_rows(prev_mdd)

# plotting the age-specific prevalence of MDD stratified
# by education and sex
prev_mdd %>%
  ggplot() +
  geom_line(aes(age, prev, color=gender, 
                linetype = education), size = 1)+
  scale_y_continuous(breaks = c(2,4,6)) +
  labs(y = "Prevalence of MDD %",
       x = "Age (years)",
       colour = "Sex",
       linetype = "Education") +
  theme_bw() + 
  theme(legend.position = "right",
        legend.box.margin = margin(0,0,0,0),
        plot.title.position = "plot", 
        plot.caption.position =  "plot",
        plot.margin = margin(0,0,0,0)) 
ggsave("./plots/age_specific_prev_mdd.pdf", plot = last_plot(),
       width = 6, height = 4, units = "in")


rm(tmp_data)

# Age of onset ------------------------------------------------------------
# age at onset if no MDD age 18. filtering by index == 2 to avoid duplicate 
# individuals, and exclude individuals who had MDD when they entered the
# cohort
mean_onset_mdd <- mydata_mdd %>%
  filter(index == 2, health == "MDD", !is.na(table_order)) %>%
  group_by(analysis, sim, education, gender) %>%
  summarise(age_onset = mean(agestart), .groups = "drop") %>%
  pivot_wider(names_from = c(analysis, gender), names_glue = "{analysis} {gender} (years)", 
              values_from = age_onset); mean_onset_mdd

#reformatting data frames to use in bar plots later on
mean_onset_mdd_diff <- mean_onset_mdd %>%
  mutate(across(where(is.numeric), ~ (.x - first(.)) * -1)) %>%
  filter(education == "Low") %>% 
  rename(Primary_Female = `Primary Female (years)`, 
         Primary_Male = `Primary Male (years)`,
         Sensitivity_Female = `Sensitivity Female (years)`, 
         Sensitivity_Male = `Sensitivity Male (years)`) %>%
  pivot_longer(where(is.numeric), 
               names_to = c("analysis","sex"),
               names_pattern = "(.+)_(.+)",
               values_to = "mean_onset"); mean_onset_mdd_diff 


# mean duration of MDD ---------------------------------------------------
# mean duration of MDD between ages 18 and 65
# simply summing up total time spent with MDD and dividing by number of
# unique individuals who ever had MDD in each strata.
duration_of_mdd <- mydata_mdd %>%
  filter(health == "MDD", !is.na(table_order)) %>%
  group_by(analysis, sim, education, gender) %>%
  summarise(duration_MDD = sum(length)/length(unique(ID)), .groups = "drop") %>%
  pivot_wider(names_from = c(analysis, gender), names_glue = "{analysis} {gender} (years)", 
              values_from = duration_MDD) %>%
  arrange(sim); duration_of_mdd


#reformatting data frames to use in bar plots later on
duration_of_mdd_diff <- duration_of_mdd %>%
  mutate(across(where(is.numeric), ~ .x - first(.))) %>%
  filter(education == "Low") %>% 
  rename(Primary_Female = `Primary Female (years)`, 
         Primary_Male = `Primary Male (years)`,
         Sensitivity_Female = `Sensitivity Female (years)`, 
         Sensitivity_Male = `Sensitivity Male (years)`) %>%
  pivot_longer(where(is.numeric), 
               names_to = c("analysis","sex"),
               names_pattern = "(.+)_(.+)",
               values_to = "duration"); duration_of_mdd_diff 


# barplots for differences between groups ---------------------------------
# primary analysis
lc_prev_mdd_dif_plot <- lc_prev_mdd_diff %>%
  filter(analysis == "Primary") %>%
  mutate(sim = as.factor(case_when(sim == "observed" ~ "Observed data",
                                   sim == "counterfactual smoking" ~ "Counterfactual smoking behaviour",
                                   sim == "counterfactual health literacy" ~ "Counterfactual health literacy",
                                   sim == "counterfactual social" ~ "Counterfactual quality of social contacts",
                                   sim == "counterfactual joint" ~ "Counterfactual joint effect"))) %>%
  group_by(sex) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  mutate(lc_prev_percent = if_else(sim == "Observed data", "Reference",
                                   sprintf("-%1.1f%%", lc_prev_percent))) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = lc_prev, y =  reorder(sim, -lc_prev)), 
           stat = "identity", fill = "dark gray") +
  geom_text(aes(x = lc_prev, y =  reorder(sim, -lc_prev),
                label = lc_prev_percent),
            hjust = 1,
            size = 3) +
  facet_grid(sex~.) +
  labs(y = "Simulation*", x = "Educational difference in life course prevalence of MDD (% points)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.9)); lc_prev_mdd_dif_plot
ggsave("./plots/lc_prev_mdd_dif_plot.pdf", plot = lc_prev_mdd_dif_plot)

mean_onset_mdd_dif_plot <- mean_onset_mdd_diff %>%
  filter(analysis == "Primary") %>%
  mutate(sim = as.factor(case_when(sim == "observed" ~ "Observed data",
                                   sim == "counterfactual smoking" ~ "Counterfactual smoking behaviour",
                                   sim == "counterfactual health literacy" ~ "Counterfactual health literacy",
                                   sim == "counterfactual social" ~ "Counterfactual quality of social contacts",
                                   sim == "counterfactual joint" ~ "Counterfactual joint effect"))) %>%
  group_by(sex) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  mutate(mean_onset_percent = if_else(sim == "Observed data", "Reference",
                                      sprintf("-%1.1f%%", mean_onset_percent))) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = mean_onset, y =  reorder(sim, -mean_onset)), 
           stat = "identity", fill = "dark gray") +
  geom_text(aes(x = mean_onset, y =  reorder(sim, -mean_onset),
                label = mean_onset_percent),
            hjust = 1,
            size = 3) +
  facet_grid(sex~.) +
  labs(y = "Simulation*", x = "Educational difference in mean age of onset of MDD (years)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.9)); mean_onset_mdd_dif_plot
ggsave("./plots/mean_onset_mdd_dif_plot.pdf", plot = mean_onset_mdd_dif_plot)

mean_duration_mdd_dif_plot <- duration_of_mdd_diff %>%
  filter(analysis == "Primary") %>%
  mutate(sim = as.factor(case_when(sim == "observed" ~ "Observed data",
                                   sim == "counterfactual smoking" ~ "Counterfactual smoking behaviour",
                                   sim == "counterfactual health literacy" ~ "Counterfactual health literacy",
                                   sim == "counterfactual social" ~ "Counterfactual quality of social contacts",
                                   sim == "counterfactual joint" ~ "Counterfactual joint effect"))) %>%
  group_by(sex) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  mutate(duration_percent = if_else(sim == "Observed data", "Reference",
                                    sprintf("-%1.1f%%", duration_percent))) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = duration, y =  reorder(sim, -duration)), 
           stat = "identity", fill = "dark gray") +
  geom_text(aes(x = duration, y =  reorder(sim, -duration),
                label = duration_percent),
            hjust = 1,
            size = 3) +
  facet_grid(sex~.) +
  labs(y = "Simulation*", x = "Educational difference in mean duration of MDD (years)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.9)); mean_duration_mdd_dif_plot
ggsave("./plots/mean_duration_mdd_dif_plot.pdf", plot = mean_duration_mdd_dif_plot)

# sensitivity analysis
lc_prev_mdd_dif_plot_sens <- lc_prev_mdd_diff %>%
  filter(analysis == "Sensitivity") %>%
  mutate(sim = as.factor(case_when(sim == "observed" ~ "Observed data",
                                   sim == "counterfactual smoking" ~ "Counterfactual smoking behaviour",
                                   sim == "counterfactual health literacy" ~ "Counterfactual health literacy",
                                   sim == "counterfactual social" ~ "Counterfactual quality of social contacts",
                                   sim == "counterfactual joint" ~ "Counterfactual joint effect"))) %>%
  group_by(sex) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  mutate(lc_prev_percent = if_else(sim == "Observed data", "Reference",
                                   sprintf("-%1.1f%%", lc_prev_percent))) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = lc_prev, y =  reorder(sim, -lc_prev)), 
           stat = "identity", fill = "dark gray") +
  geom_text(aes(x = lc_prev, y =  reorder(sim, -lc_prev),
                label = lc_prev_percent),
            hjust = 1,
            size = 3) +
  facet_grid(sex~.) +
  labs(y = "Simulation*", x = "Educational difference in life course prevalence of MDD (% points)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.9)); lc_prev_mdd_dif_plot_sens
ggsave("./plots/lc_prev_mdd_dif_plot_sens.pdf", plot = lc_prev_mdd_dif_plot_sens)

mean_onset_mdd_dif_plot_sens <- mean_onset_mdd_diff %>%
  filter(analysis == "Sensitivity") %>%
  mutate(sim = as.factor(case_when(sim == "observed" ~ "Observed data",
                                   sim == "counterfactual smoking" ~ "Counterfactual smoking behaviour",
                                   sim == "counterfactual health literacy" ~ "Counterfactual health literacy",
                                   sim == "counterfactual social" ~ "Counterfactual quality of social contacts",
                                   sim == "counterfactual joint" ~ "Counterfactual joint effect"))) %>%
  group_by(sex) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  mutate(mean_onset_percent = if_else(sim == "Observed data", "Reference",
                                      sprintf("-%1.1f%%", mean_onset_percent))) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = mean_onset, y =  reorder(sim, -mean_onset)), 
           stat = "identity", fill = "dark gray") +
  geom_text(aes(x = mean_onset, y =  reorder(sim, -mean_onset),
                label = mean_onset_percent),
            hjust = 1,
            size = 3) +
  facet_grid(sex~.) +
  labs(y = "Simulation*", x = "Educational difference in mean age of onset of MDD (years)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.9)); mean_onset_mdd_dif_plot_sens
ggsave("./plots/mean_onset_mdd_dif_plot_sens.pdf", plot = mean_onset_mdd_dif_plot_sens)

mean_duration_mdd_dif_plot_sens <- duration_of_mdd_diff %>%
  filter(analysis == "Sensitivity") %>%
  mutate(sim = as.factor(case_when(sim == "observed" ~ "Observed data",
                                   sim == "counterfactual smoking" ~ "Counterfactual smoking behaviour",
                                   sim == "counterfactual health literacy" ~ "Counterfactual health literacy",
                                   sim == "counterfactual social" ~ "Counterfactual quality of social contacts",
                                   sim == "counterfactual joint" ~ "Counterfactual joint effect"))) %>%
  group_by(sex) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  mutate(duration_percent = if_else(sim == "Observed data", "Reference",
                                    sprintf("-%1.1f%%", duration_percent))) %>%
  ungroup() %>%
  ggplot() +
  geom_bar(aes(x = duration, y =  reorder(sim, -duration)), 
           stat = "identity", fill = "dark gray") +
  geom_text(aes(x = duration, y =  reorder(sim, -duration),
                label = duration_percent),
            hjust = 1,
            size = 3) +
  facet_grid(sex~.) +
  labs(y = "Simulation*", x = "Educational difference in mean duration of MDD (years)") +
  theme_bw() + 
  theme(legend.position = "bottom",
        legend.box.margin = margin(0,0,0,0),
        plot.margin = margin(0,0,0,0),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(hjust = 0.9)); mean_duration_mdd_dif_plot_sens
ggsave("./plots/mean_duration_mdd_dif_plot_sens.pdf", plot = mean_duration_mdd_dif_plot_sens)


# merging various population-level parameters into one data frame and
# calculating percentage change of all parameters in counterfactuals
percent_diff_mdd <- lc_prev_mdd_diff %>%
  relocate(lc_prev, .after = last_col()) %>%
  left_join(mean_onset_mdd_diff, by = c("sim", "sex", "analysis", "education")) %>%
  left_join(duration_of_mdd_diff, by = c("sim", "sex", "analysis", "education")) %>%
  group_by(sex, analysis) %>%
  mutate(across(where(is.numeric), 
                .fns = list(percent = ~ round((abs((.x - first(.))))/first(.) * 100, 1)))) %>%
  ungroup() %>%
  mutate(across(5:7, ~round(.,3))) %>%
  relocate(lc_prev_percent, .after = lc_prev) %>%
  relocate(mean_onset_percent, .after = mean_onset) %>%
  arrange(analysis, sex, -lc_prev_percent) 

write_xlsx(percent_diff_mdd, path = "percent_diff_mdd.xlsx")
