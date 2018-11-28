#######################
#######################
## mouse simulator
## Modified from the code used to create the graphs in Foley and Marjoram,
## "Sure enough: efficient Bayesian learning and choice"
## It will run as written
####################
####################

# library(nplr)
library(nnet)
library(tidyverse)
library(Hmisc)

set.seed(5872)

folder <- "C:/!Vladi/Manuscripts/MDRE" #or directly set the working directory to the right path
setwd(folder)

####Functions
Which_vol <- function(OldPostMean1, OldPostMean2, OldPostVar1, OldPostVar2){
    # calculate P(x from environment 1 > x from environment 2) and sample env 1 with that prob.
    # distribution of env1 - 2env2 is Normal(OldPostMean1-OldPostMean2,OldPostVar1+OldPostVar2)
    PSample1 <- pnorm(0, mean = OldPostMean1 - OldPostMean2,
                      sd = sqrt(OldPostVar1 + OldPostVar2), lower.tail = FALSE)
    if (is.na(PSample1)) browser() # debug when value out of bounds is returned
    p <- runif(1, 0, 1)
    if (p < PSample1) {
        return(1)
    }else{
        return(2)
    }
}

vols <- c(1, 1)
probs <- c(0.5, 0.2)

vols <- Memory$OldPostMean
probs <- Memory$OldPr
Which_val(vols, probs)

Which_val <- function(vols, probs, cv = 0.7) {

  # p <- runif(1, 0, 1)
  n_options <- length(vols)
  #
  # if (p < lapse) return(sample(1:n_options, 1) - 1)


  vals <- vols * probs
  vals <- rnorm(vals, mean = vals, sd = cv * abs(vals))
  res <- which(vals == max(vals))

  if (length(res) > 1) return(sample(1:n_options, 1))
  return(res)

}

pnorm(0, mean = 0, sd = sqrt(0), lower.tail = FALSE)
cv <- 0.7
means <- c(20, 50, 100, 0.2, 0.2, 0.2)
sds <- (cv*means)
dims <- c("v","v","v","p","p","p")

means <- c(-2, 0, 0, 0)
sds <- cv*means
dims <- c("v","v","p","p")

Which_lgr(means, sds, dims, "p")
Which_log(means, dims)



Which_product(means, sds, dims)

# comm_mean <- 5/20*0.5
# common <- rnorm(1000, mean = comm_mean, sd = cv*comm_mean)
# separate <- rnorm(1000, mean = 5/20, sd = cv*5/20) * rnorm(1000, mean = 0.5, sd = cv*0.5)

# plot(density(common))
# lines(density(separate), col = "darkblue")

Which_product <- function(means, sds, dims){

  if (!all((unique(dims)) %in% c("v", "p"))) stop("Dimensions (dims) must be of type 'v' and 'p'")

  Samples <- data.frame(means = means, sds = abs(sds), dims = dims) %>%
    rowwise() %>%
    mutate(val = rnorm(1, mean = means, sd = sds))

  values <- Samples %>%
    filter(dims == "v") %>%
    select(val) *
    Samples %>%
    filter(dims == "p") %>%
    select(val)

  return(which.is.max(values$val))

}

Which_log <- function(means, dims, cv){

  if (!all((unique(dims)) %in% c("v", "p"))) stop("Dimensions (dims) must be of type 'v' and 'p'")

  Samples <- data.frame(means = means, dims = dims) %>%
    mutate(means = if_else(means < (-1), -0.99, means)) # set values that would be out of range
  # to permissible value

  m_v <- Samples %>%
    filter(dims == "v") %>%
    select(means)
  m_p <- Samples %>%
    filter(dims == "p") %>%
    select(means)
  values <- as.data.frame(log(1 + m_v + m_p + m_v*m_p)) %>%
    mutate(sds = abs(means*cv)) %>%
    rowwise() %>%
    mutate(val = rnorm(1, mean = means, sd = sds))

  return(which.is.max(values$val))

}

Which_randdim <- function(means, sds, dims, weightp=0.5){

  if (!all((unique(dims)) %in% c("v", "p"))) stop("Dimensions (dims) must be of type 'v' and 'p'")

  Samples <- data.frame(means = means, sds = abs(sds), dims = dims) %>%
    rowwise() %>%
    mutate(val = rnorm(1, mean = means, sd = sds))

  p <- runif(1, 0, 1)
  if (p < weightp) {
    dim <- "p"
  }else{
    dim <- "v"
  }
  values <- Samples %>%
    filter(dims == dim) %>%
    select(val)
  return(which.is.max(values$val))
}


Which_WTA <- function(means, sds, dims){

  if (!all((unique(dims)) %in% c("v", "p"))) stop("Dimensions (dims) must be of type 'v' and 'p'")

  Samples <- data.frame(means = means, sds = abs(sds), dims = dims) %>%
    rowwise() %>%
    mutate(val = rnorm(1, mean = means, sd = sds))
  dim <- Samples %>%
    ungroup() %>%
    group_by(dims) %>%
    summarise(max = max(means), min = min(means), rint = 2*(max - min)/(max + min)) %>%
    filter(rint == max(rint)) %>%
    pull(dims) %>%
    as.character()

  if (length(dim) != 1) return(Which_randdim(means, sds, dims))

  values <- Samples %>%
    filter(dims == dim) %>%
    select(val)

  return(which.is.max(values$val))
}

Which_lxgr <- function(means, sds, dims, dim, threshold = 0.7){
  #lexicographic rule with one dimension checked first, then if it is not informative, check the other
  if (!all((unique(dims)) %in% c("v", "p"))) stop("Dimensions (dims) must be of type 'v' and 'p'")

  Samples <- data.frame(means = means, sds = abs(sds), dims = dims) %>%
    rowwise() %>%
    mutate(val = rnorm(1, mean = means, sd = sds))

  maxrint <- Samples %>%
    ungroup() %>%
    filter(dims == dim) %>%
    summarise(max = max(means), min = min(means), rint = 2*(max - min)/(max + min)) %>%
    pull(rint)
  if (is.na(maxrint)) maxrint = 0
  if (maxrint > threshold) {
    values <- Samples %>%
      filter(dims == dim) %>%
      select(val)
  }else{
    values <- Samples %>%
      filter(dims != dim) %>%
      select(val)
  }
  return(which.is.max(values$val))
}

voldispense_list <- function(prob, vol, i){
  i = i %% 20
  if (i == 0) i = 20

  prob_seqs <- data.frame(
    p0.2 = c(1,0,0,0,1,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0),
    p0.3 = c(1,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,1,0,0,0),
    p0.5 = c(1,0,1,1,0,1,0,1,1,0,1,0,0,1,0,0,1,0,1,0),
    p0.7 = c(1,1,0,1,1,1,0,1,1,1,0,1,0,1,1,0,1,1,1,0),
    p0.8 = c(1,1,1,0,1,1,1,1,1,0,1,1,0,1,1,1,1,1,1,0),
    per7_l = c(0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
    per7_h = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0),
    per0.5 = c(1,0.25,1,1,0.25,1,0.25,1,1,0.25,1,0.25,0.25,1,0.25,0.25,1,0.25,1,0.25),
    per0.2 = c(1,0.25,0.25,0.25,1,0.25,0.25,0.25,0.25,1,0.25,0.25,0.25,1,0.25,0.25,0.25,0.25,0.25,0.25),
    per0.8 = c(1,1,1,0.25,1,1,1,1,1,0.25,1,1,0.25,1,1,1,1,1,1,0.25),
    per0.25 = c(1,0,0.25,0.25,0,1,0,0.25,1,0,0.25,0,0,0.25,0,0,1,0,1,0))

    p <- prob
    vol_v <- prob_seqs %>%
      select(!!as.name(p)) %>%
      slice(i) %>%
      unlist()
  return(vol_v*vol)
}



voldispense_prob <- function(prob, vol){
  p <- runif(1, 0, 1)
  if (p > prob) {
    return(0)
  }else{
    return(vol)
  }
}


# generic voldispense function
voldispense <- function(prob, vol, i = 1){
  if (suppressWarnings(is.na(as.numeric(prob)))) {
    return(voldispense_list(prob, vol, i))
  }else{
    return(voldispense_prob(prob, vol))
  }

}

beeYourself <- function(i, recordAFrame, Env, Regime, Choice, MuVar){
  # the function that does all the work, for a single visit to a feeder
  # takes a mouse, asks it what it thinks
  # has it visit a feeder
  # updates the beliefs of the mouse
  # outputs an updated record of all this fun
  Memory <- data.frame(OldPostMean = c(recordAFrame[i,"Est1"], recordAFrame[i,"Est2"]),
                       OldPostVar = c(recordAFrame[i,"Var1"], recordAFrame[i,"Var2"]),
                       OldPr = c(recordAFrame[i,"Pr1"], recordAFrame[i,"Pr2"]),
                       OldEP = c(recordAFrame[i,"EP1"], recordAFrame[i,"EP2"]),
                       pPostVar = c(recordAFrame[i,"pPostVar1"], recordAFrame[i,"pPostVar2"]))

    rew <- 0
    alpha <- 0

    Which <- switch(Choice,
                    `1` = Which_product(c(Memory$OldPostMean, Memory$OldPr),
                                     c(Memory$OldPostMean*cv, Memory$OldPr*cv),
                                     c("v", "v", "p", "p")), #hurdle
                    `2` = Which_randdim(c(Memory$OldPostMean, Memory$OldPr),
                                        c(Memory$OldPostMean*cv, Memory$OldPr*cv),
                                        c("v", "v", "p", "p")), #random dimension
                    `3` = Which_WTA(c(Memory$OldPostMean, Memory$OldPr),
                                    c(Memory$OldPostMean*cv, Memory$OldPr*cv),
                                    c("v", "v", "p", "p")), #winner takes all
                    `4` = Which_lxgr(c(Memory$OldPostMean, Memory$OldPr),
                                     c(Memory$OldPostMean*cv, Memory$OldPr*cv),
                                     c("v", "v", "p", "p"), "p"), #prob first
                    `5` = Which_lxgr(c(Memory$OldPostMean, Memory$OldPr),
                                     c(Memory$OldPostMean*cv, Memory$OldPr*cv),
                                     c("v", "v", "p", "p"), "v"), #vol first
                    `6` = Which_vol(Memory$OldPostMean[1],Memory$OldPostMean[2],
                                    Memory$OldPostVar[1], Memory$OldPostVar[2]), #vol only,
                    # #retreival without scalar property
                    # `7` = Which_log(c(Memory$OldPostMean, Memory$OldPr),
                    #                 c("v", "v", "p", "p"), cv) #logproduct rule
                    #                                     #retreival without scalar property
                    `7` = Which_val(Memory$OldPostMean, Memory$OldPr, cv) #valproduct rule
                    )

    if (i > 1) {
      recordAFrame[(i), "CumVisTo1"] <- recordAFrame[(i - 1), "CumVisTo1"] + 2 - Which
    }else{
      recordAFrame[(i), "CumVisTo1"] <- 2 - Which
    }

    ########
    # sample data
    DataPt <- voldispense(Env$prob[Which], Env$EnvMean[Which], i)

    if (DataPt > 0) rew <- 1

    # perceptual scalar property, can be switched off if cv is set to 0
    DataPt <- rnorm(1, mean = DataPt, sd = cv*DataPt)

    # no record of rewards, but of perceived rewards:
    recordAFrame[(i), paste("E", Which, "M", sep = "")] <- DataPt

    if (Which == 1) {
      alpha <- 1/(recordAFrame[(i),"CumVisTo1"])
    }else{
      alpha <- 1/(i - recordAFrame[(i),"CumVisTo1"])
    }
    Memory$OldPr[Which] <- Memory$OldPr[Which] + (rew - Memory$OldPr[Which])*alpha

    Memory$pPostVar[Which] <-  1/(1/Memory$pPostVar[Which] + 1/Env$pVar[Which])
    Memory$OldEP[Which] <- (Memory$OldEP[Which]/Memory$pPostVar[Which] +
                              rew/Env$pVar[Which])/(1/Memory$pPostVar[Which] + 1/Env$pVar[Which])
    # update posterior for sampled site
    if (Choice == 6 | DataPt != 0) { # do not update if reward is zero, except for Choice type 6
      Memory$OldPostMean[Which] <-
        (Memory$OldPostMean[Which]/Memory$OldPostVar[Which] + DataPt/Env$volVar[Which]) /
        (1/Memory$OldPostVar[Which] + 1/Env$volVar[Which])
      Memory$OldPostVar[Which] <-  1/(1/Memory$OldPostVar[Which] + 1/Env$volVar[Which])
    }
    # perform other updates

    if (Regime == 1) {
        # Move Mus and add noise to posterior for movement of Mus
        Env <- Env %>%
          rowwise() %>%
          mutate(EnvMean = EnvMean + rnorm(1, 0, sqrt(MuVar)))
        Memory <- Memory %>% mutate(OldPostVar = OldPostVar + MuVar,
                                    pPostVar = pPostVar + MuVar)
    }else if (Regime == 2) {
        # in this case the means don't move, but we slowly forget what we knew about the other site
      Memory$OldPostVar[3 - Which] <- Memory$OldPostVar[3 - Which] + MuVar
      Memory$pPostVar[3 - Which] <- Memory$pPostVar[3 - Which] + MuVar
    }else if (Regime == 3) {
        # the means don't move, but we slowly forget what we knew; and add a bit of uncertainty re future of current site
      Memory <- Memory %>%
        mutate(OldPostVar = OldPostVar + MuVar,
                                  pPostVar = pPostVar + MuVar)
    }else if (Regime == 4) {
      # the means don't move, but we slowly forget what we knew;
      # and add scalar property as uncertainty re future of current site
      Memory <- Memory %>%
        mutate(OldPostVar = OldPostVar + OldPostMean*cv,
                                  pPostVar = pPostVar + OldPr*cv)
    }
    # keep track of the posterior SDs
    # if (Env$EnvMean[2] < 0)   browser()

    recordAFrame[(i + 1),"E1M"] <- Env$EnvMean[1]
    recordAFrame[(i + 1),"E2M"] <- Env$EnvMean[2]
    recordAFrame[(i + 1),"Est1"] <- Memory$OldPostMean[1]
    recordAFrame[(i + 1),"Est2"] <- Memory$OldPostMean[2]
    recordAFrame[(i + 1),"Var1"] <- Memory$OldPostVar[1]
    recordAFrame[(i + 1),"Var2"] <- Memory$OldPostVar[2]
    recordAFrame[(i),"VisitsTo1"] <- 2 - Which
    recordAFrame[(i + 1),"Pr1"] <- Memory$OldPr[1]
    recordAFrame[(i + 1),"Pr2"] <- Memory$OldPr[2]
    recordAFrame[(i + 1),"EP1"] <- Memory$OldEP[1]
    recordAFrame[(i + 1),"EP2"] <- Memory$OldEP[2]
    recordAFrame[(i + 1),"pPostVar1"] <- Memory$pPostVar[1]
    recordAFrame[(i + 1),"pPostVar2"] <- Memory$pPostVar[2]

    return(recordAFrame)
}

makeRecords <- function(NoIts, block){
    #Create the data frame which will contain all the recorded info
    # for one bee in a simulation
    totIts <- NoIts + 1 # need a null row at the end, to not crash
    recordBlock <- c(1:totIts)
    recordBlock <- recordBlock/block
    recordBlock <- ceiling(recordBlock)
    recordBlock <- recordBlock*block
    recordAFrame <- data.frame(recordBlock,
                            VisitsTo1 = rep(0, totIts), # Vector to use to plot moving average
                            CumVisTo1 = rep(0, totIts), # vector to track the cumulative visits to patch 1
                            PVisit1 = rep(-9, totIts), # vector to keep track of prob. of visiting patch 1
                            E1M = rep(-9, totIts), # vector to keep track of Environ1Mu
                            E2M = rep(-9, totIts),  # vector to keep track of Environ2Mu
                            Est1 = rep(-9, totIts), # vector to keep track of Estimated mean for patch 1
                            Est2 = rep(-9, totIts), # vector to keep track of Estimated mean for patch 2
                            Var1 = rep(-9, totIts), # vectors to keep track of sd of posteriors
                            Var2 = rep(-9, totIts), # vectors to keep track of sd of posteriors
                            Pr1 = rep(-9, totIts), # vector to keep track of probability of reward of patch 1
                            Pr2 = rep(-9, totIts), # vector to keep track of probability of reward of patch 2
                            EP1 = rep(-9, totIts), # vector to keep track of the estimate for the probability at patch 1
                            EP2 = rep(-9, totIts), # vector to keep track of the estimate for the probability at patch 2
                            pPostVar1 = rep(-9, totIts), # vectors to keep track of sd of the posteriors for the probability
                            pPostVar2 = rep(-9, totIts) # vectors to keep track of sd of the posteriors for the probability
    )
    return(recordAFrame)
}
paramset <- 1

RunBeeSim <- function(conditions, paramset){
    # set parameters, from the data frame of possible conditions
    # then runs a simulation from those parameters.
    NoIts <- conditions[paramset, "NoIts"]
    block <- conditions[paramset, "block"]

    recordAFrame <- makeRecords(NoIts, block)

    Env <- data.frame(EnvMean = c(conditions[paramset,"Environ1Mu"], conditions[paramset,"Environ2Mu"]),
                      volVar = c(conditions[paramset,"vol1Var"], conditions[paramset,"vol2Var"]),
                      prob = c(conditions[paramset,"prob1"], conditions[paramset,"prob2"]),
                      pVar = c(conditions[paramset,"p1Var"], conditions[paramset,"p2Var"]),
                      stringsAsFactors = F)
    Regime <- conditions[paramset,"Regime"]
    MuVar <- conditions[paramset,"MuVar"]
    Choice <- conditions[paramset, "Choice_type"]

    for (j in 1:2) {
      recordAFrame[1, paste("E", j, "M", sep = "")] <- Env$EnvMean[j]
      recordAFrame[1, paste("Est", j, sep = "")] <- conditions[paramset, paste("OldPostMean", j, sep = "")]
      recordAFrame[1, paste("Var", j, sep = "")] <- conditions[paramset, paste("OldPostVar", j, sep = "")]
      recordAFrame[1, paste("Pr", j, sep = "")] <- 0
      recordAFrame[1, paste("EP", j, sep = "")] <- 0
      recordAFrame[1, paste("pPostVar", j, sep = "")] <- conditions[paramset, paste("pPostVar", j, sep = "")]
    }

    #run the simulation
    for (i in 1:NoIts) {
      recordAFrame <- beeYourself(i, recordAFrame, Env, Regime, Choice, MuVar)
        # choose which environment to sample
    }
    recordAFrame$pset <- paramset
    recordAFrame <- recordAFrame %>% select(pset, everything())
    #return the records of all the visits, conditions, and beliefs.
    return(recordAFrame[1:(nrow(recordAFrame) - 1),])
}

##########################
##########################
#Do all the things:
#Initialise


sigmavol <-  1
sigmaprob <-  1


u2 <- 0.008 #asymptote level, the smaller, the smaller the "lapse rate"
sigma2 <- 5 #learning rate for vol, the smaller, the faster the learning
pvar <- 5 #learning rate for prob, the smaller, the faster the learning
cv <-  0.7 # coefficient of variation

nreps <- 100 # number of individuals
ntrials <- 240

# n_models <- 7

#3 exps MDRE
#
conditions <- data.frame("paramset" = 1:20,
                         "prob1" = c(c(0.5, 0.5, 0.2, 0.5, 0.5, 0.2),
                                     c(1, 1, 0.2, 1, 1, 0.2),
                                     c(0.5, 0.5, 0.5, 0.5, 0.2, 0.5, 0.7, 0.8)),
                         "prob2" = c(c(0.2, 0.2, 0.2, 0.5, 0.2, 0.5),
                                     c(0.2, 0.2, 0.2, 1, 0.2, 1),
                                     c(0.2, 0.2, 0.2, 0.2, 0.2, 0.5, 0.7, 0.8)),
                         "p1Var" = pvar, "p2Var" = pvar,
                         "Environ1Mu" = c(c(0.2, 1, 1, 1, 1, 1),
                                          c(0.2, 1, 1, 1, 1, 1),
                                          c(0.2, 0.5, 0.75, 1, 1, 1, 1, 1)),
                         "Environ2Mu" = c(c(0.2, 1, 0.2, 0.2, 0.2, 0.2),
                                          c(0.2, 1, 0.2, 0.2, 0.2, 0.2),
                                          c(0.2, 0.5, 0.75, 1, 0.2, 0.2, 0.2, 0.2)),
                         "cond" = c(rep(c("BPV1", "BPV2", "BVP1", "BVP2", "C", "I"), 2),
                                    c("PV1", "PV2", "PV3", "PV4", "VP1", "VP2", "VP3", "VP4")),
                         "experiment" = c(rep(1:3, each = 6), 3, 3),
                         "vol1Var" = sigma2, "vol2Var" = sigma2, "Regime" = 3,
                         "Choice_type" = 7,
                         "MuVar" = u2,
                         "OldPostMean1" =  0,
                         "OldPostMean2" = 0,
                         "OldPostVar1" = sigmavol,"OldPostVar2" = sigmavol,
                         "pPostVar1" = sigmaprob, "pPostVar2" = sigmaprob,
                         "NoIts" = ntrials, "block" = 30, stringsAsFactors = F)


#### experiment 1 of MDRE
# conditions <- data.frame("paramset" = 1:(n_models*6),
#                          "prob1" = rep(c(0.2, 0.5, 0.5, 0.5, 0.5, 0.2),
#                                        n_models),
#                          "prob2" = rep(c(0.2, 0.5, 0.2, 0.2, 0.2, 0.5),
#                                        n_models),
#                          "p1Var" = pvar, "p2Var" = pvar,
#                          "Environ1Mu" = rep(c(1, 1, 0.2, 1, 1, 1),
#                                             n_models),
#                          "Environ2Mu" = rep(c(0.2, 0.2, 0.2, 1, 0.2, 0.2),
#                                             n_models),
#                          "vol1Var" = sigma2, "vol2Var" = sigma2, "Regime" = 3,
#                          "Choice_type" = rep(1:n_models, each = 6),
#                          "MuVar" = u2,
#                          "OldPostMean1" =  0,
#                          "OldPostMean2" = 0,
#                          "OldPostVar1" = sigmavol,"OldPostVar2" = sigmavol,
#                          "pPostVar1" = sigmaprob, "pPostVar2" = sigmaprob,
#                          "NoIts" = ntrials, "block" = 30, stringsAsFactors = F)

#### experiment 2 of MDRE
# conditions <- data.frame("paramset" = c(1:42),
#                          "prob1" = rep(c(0.2, 1, 1, 1, 1, 0.2),7),
#                          "prob2" = rep(c(0.2, 1, 0.2, 0.2, 0.2, 1),7),
#                          "p1Var" = pvar, "p2Var" = pvar,
#                          "Environ1Mu" = rep(c(1, 1, 0.2, 1, 1, 1),7),
#                          "Environ2Mu" = rep(c(0.2, 0.2, 0.2, 1, 0.2, 0.2),7),
#                          "vol1Var" = sigma2, "vol2Var" = sigma2,"Regime" = 3,
#                          "Choice_type" = rep(1:7,each = 6),
#                          "MuVar" = u2,
#                          "OldPostMean1" =  0,
#                          "OldPostMean2" = 0,
#                          "OldPostVar1" = sigmavol,"OldPostVar2" = sigmavol,
#                          "pPostVar1" = sigmaprob, "pPostVar2" = sigmaprob,
#                          "NoIts" = ntrials, "block" = 30, stringsAsFactors = F)


#### experiment 3 of MDRE

# conditions <- data.frame("paramset" = c(1:56),
#                          "prob1" = rep(c(0.2, 0.5, 0.7, 0.8, 0.5, 0.5, 0.5, 0.5),7),
#                          "prob2" = rep(c(0.2, 0.5, 0.7, 0.8, 0.2, 0.2, 0.2, 0.2),7),
#                          "p1Var" = pvar, "p2Var"= pvar,
#                          "Environ1Mu" = rep(c(1, 1, 1, 1, 0.2, 0.5, 0.75, 1),7),
#                          "Environ2Mu" = rep(c(0.2, 0.2, 0.2, 0.2, 0.2, 0.5, 0.75, 1),7),
#                          "vol1Var" = sigma2, "vol2Var" = sigma2,"Regime" = 3,
#                          "Choice_type" = rep(1:7,each = 8),
#                          "MuVar" = u2,
#                          "OldPostMean1" = 0,
#                          "OldPostMean2" = 0,
#                          "OldPostVar1" = sigmavol,"OldPostVar2" = sigmavol,
#                          "pPostVar1" = sigmaprob, "pPostVar2" = sigmaprob,
#                          "NoIts" = ntrials, "block" = 30, stringsAsFactors = F)


# nchunks <- ntrials/30/2

# axSpot <- c(1:nchunks)
# axSpot <- axSpot*60
# axLabs <- axSpot

results <- c()
aggResults <- c()
n_data <- nrow(conditions)

pb <- winProgressBar(title = "progress bar", min = 0, max = n_data*nreps)

for (j in 1:n_data) {


    beeExploring <- cbind(nrep = 1, RunBeeSim(conditions, j))
    for (i in 2:nreps) {
      setWinProgressBar(pb, i, title =
                          paste(round(((j - 1)*nreps + i) /
                                        (n_data*nreps)*100, 1), "% done"))
      aaa <-  cbind(nrep = i, RunBeeSim(conditions, j))
      beeExploring <- rbind(beeExploring,aaa)
    }

   summResults <- beeExploring %>% group_by(recordBlock) %>%
     summarise(visitMean = mean(VisitsTo1),
               visitSD = sd(VisitsTo1),
               est1mean = mean(Est1),
               est2mean = mean(Est2),
               Var1mean = mean(Var1),
               Var2mean = mean(Var2))

   aggResults <- rbind(aggResults, beeExploring)


    # if (j == 1) {
    #   plot(visitMean~recordBlock, data = summResults, xlim = c(0,ntrials),
    #        ylim = c(0, 1), pch = 16, col = "black", bg = "black",
    #        xlab = "Visits", ylab = "Prop visits to higher reward",
    #        xaxt = "n", cex.lab = 1.8, cex.axis = 1.5, cex = 1.2)
    # }else{
    #   points(visitMean~recordBlock, data = summResults, xlim = c(0,ntrials),
    #        ylim = c(0, 1), pch = 16, col = j, bg = j,
    #        xlab = "Visits", ylab = "Prop visits to higher reward",
    #        xaxt = "n", cex.lab = 1.8, cex.axis = 1.5, cex = 1.2)
    # }
    #
    # axis(1, at = axSpot, cex.axis = 1.5)
    # with(data = summResults, expr = errbar(recordBlock, visitMean,
    #                                     visitMean + (visitSD/(sqrt(30*nreps))),
    #                                     visitMean - (visitSD/(sqrt(30*nreps))), add = T))

}
close(pb)#close progress bar

aggResults <- aggResults %>%
  group_by(pset, nrep, recordBlock) %>%
  mutate(performance = mean(VisitsTo1))

aggResults

# cond_names <- tibble(num_code = 1:6,  cond = factor(c("BVP1", "BVP2", "BPV1", "BPV2", "C", "I"),
# levels = c("BVP1", "BVP2", "BPV1", "BPV2", "C", "I")))
#
# cond_names_exp3 <- tibble(num_code = 1:8,
#                       cond = factor(c("VP1", "VP2", "VP3", "VP4", "PV1", "PV2", "PV3", "PV4")))
#
# cond_tab <- conditions %>%
#   select(paramset, Choice_type) %>%
#   rename(pset = paramset) %>%
#   group_by(Choice_type) %>%
#   mutate(num_code = 1:n())
#
#
# cond_tab <- cond_tab %>%
#   left_join(cond_names_exp3) %>%
#   select(-num_code)
#
# # only take last 60 visits from 240
# summaries <- aggResults %>%
#   filter(recordBlock > 179) %>%
#   inner_join(cond_tab) %>%
#   group_by(pset, nrep, Choice_type, cond_names) %>%
#   summarise(performance = mean(performance))


cond_tab <- conditions %>%
  select(experiment, cond, pset = paramset)

agg_summs <- aggResults %>%
  filter(recordBlock > 179) %>%
  inner_join(cond_tab) %>%
  mutate(Choice_type = 7) %>%
  group_by(pset, nrep, Choice_type, cond, experiment) %>%
  summarise(performance = mean(performance))

sims_data <- sims_data %>% filter(Choice_type != 7) %>% bind_rows(agg_summs)

sims_data <- sims_data %>% filter(experiment == 1) %>% mutate(experiment = 4) %>%
  bind_rows(sims_data)

write.table(aggResults, file = "results1.csv", sep = ";", row.names = FALSE)

write.table(summaries, file = "summ1.csv", sep = ";", row.names = FALSE)

# summaries <- summaries %>% mutate(performance = if_else(cond_names == "I", 1 - performance,
#                                                         performance))

# summaries <- summaries %>% ungroup() %>%
#   mutate(Choice_type = factor(Choice_type))

#


# summaries %>%
#   ggplot(aes(cond_names, performance, color = Choice_type)) +
#   stat_summary(fun.data = "mean_cl_boot")
#
#
#
# summaries %>%
#   filter(Choice_type == 1) %>%
#   ggplot(aes(cond_names, performance, group = nrep)) + geom_line()
#
#
# summaries %>%
#   ggplot(aes(cond_names, performance, group = nrep)) + geom_line(alpha = 1/10) +
#   facet_grid(Choice_type~.)
#
#
#
# ### sim of volume discrimination at p = 1 and p = 0.2
# # rints <- conditions %>% select(Environ1Mu, Environ2Mu) %>%
# #   mutate(rint = 2*abs(Environ1Mu - Environ2Mu)/(Environ1Mu + Environ2Mu)) %>% select(rint)
# # results <- results %>% mutate(prob = as.factor(c(rep(1,7), rep(0.2,7)))) %>% bind_cols(rints)
# # DBA %>% ggplot(aes(rint, Performance)) +
# #   stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
# #                geom = "crossbar", width = 0.2, color="gray") +
# #   geom_line(aes(xv, pfun), data=pf) +
# #   geom_point(aes(rint1, performance, color = prob), data=results) +
# #   xlab("relative intensity") + ylab("discrimination performance") +
# #   coord_cartesian(ylim = c(0.46,0.92))
#
#
# rints <- conditions %>%
#   select(prob1, prob2) %>%
#   mutate(rint = 2*abs(prob1 - prob2)/(prob1 + prob2)) %>%
#   select(rint)
# results <- results %>%
#   mutate(vol = as.factor(c(rep(20, 7), rep(4, 7)))) %>%
#   bind_cols(rints)
# DBA %>%
#   ggplot(aes(rint, Performance)) +
#   stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
#                geom = "crossbar", width = 0.2, color = "gray") +
#   geom_line(aes(xv, pfun), data = pf) +
#   geom_point(aes(rint1, performance, color = vol), data = results) +
#   xlab("relative intensity") + ylab("discrimination performance") +
#   coord_cartesian(ylim = c(0.46, 0.92))
