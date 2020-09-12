library(tidyverse)
library(assertthat)
library(lubridate)
library(rlang)
############# Main simulations
###### helper functions

# calculate the relative intensity difference / mean ratio
get_rint <- function(vec) { # generalized relative intensity function for n inputs
  if (max(vec) == min(vec) & min(vec) == 0) return(0)
  length(vec) * (max(vec) - min(vec)) / sum(vec)
}

# transform a single probability to subjective probability , see Constantinople et al. 2019
get_weighted_probs <- function(p, beta, delta) {
  get_w_p_hlp <- function(p, beta = beta, delta = delta) {
    if (p == 0) return(0)
    exp(-beta*(-log(p))^delta)
  }
  map_dbl(p, get_w_p_hlp, beta = beta, delta = delta)
}

# for multiple probabilities get the cumulative weights
get_cum_weights <- function(p, beta, delta) {
  p_1 <- get_weighted_probs(p[1], beta = beta, delta = delta)
  p_cum <- get_weighted_probs(cumsum(p), beta = beta, delta = delta) -
    get_weighted_probs(lag(cumsum(p), default = 0), beta = beta, delta = delta)
  p_cum[1] <- p_1
  p_cum
}

# convert a vector of vols and probabilities to a list of reward properties
to_rew_list <- function(vols, probs) {
  sp <- sum(probs)

  assert_that(sp <= 1, msg = "Probabilities must sum to at most one 1")

  if (sp < 1) {
  vols <- c(vols, 0)
  probs <- c(probs, 1 - sp)
  }

  list(vols = vols, probs = probs)
}


# scalar expected value utility
u_sut <- function(rew_ls, gamma, lapse, transform_probs = FALSE) {
  vols <- pluck(rew_ls, "vols")
  probs <- pluck(rew_ls, "probs")

  if (transform_probs) probs <- cum_weights(probs, beta = beta, delta = delta)

  u <- probs *
    rnorm(length(vols), mean = vols, sd = lapse + abs(vols) * gamma)
  sum(u)
}

# two-scalar utility
u_sut2 <- function(rew_ls, gamma, lapse, transform_probs = FALSE) {
  vols <- pluck(rew_ls, "vols")
  probs <- pluck(rew_ls, "probs")

  if (transform_probs) probs <- get_cum_weights(probs, beta = beta, delta = delta)


  u <- rnorm(length(vols), mean = vols, sd = lapse + abs(vols) * gamma) *
    rnorm(length(probs), mean = probs, sd = lapse + abs(probs) * gamma)
  sum(u)
}

# random noncompensatory utility (from random dimension with scalar property)
u_rnonc <- function(rew_ls, gamma, lapse, p_vol = 0.5, transform_probs = FALSE, subj_val = FALSE) {
  vols <- pluck(rew_ls, "vols")
  probs <- pluck(rew_ls, "probs")

  if (transform_probs & !subj_val) probs <- get_cum_weights(probs, beta = beta, delta = delta)

  p <- runif(1, 0, 1)

  if (p < p_vol) { # p_vol is the probability to select the volume dimension
    u <- vols[1]
  } else {
    u <- probs[1]
  }
  if (subj_val) { # if the scalar property was already applied return as is
    return(u)
  } # otherwise apply scalar property
  rnorm(1, mean = u, sd = lapse + abs(u) * gamma)
}

# generic utility function that takes the first volume and probability outputs
# for each option applies the scalar property and returns one sample per option
u_all <- function(options_ls, args) {
  gamma <- pluck(args, "gamma")
  lapse <- pluck(args, "lapse")

  modify_depth(options_ls, 2, pluck(1)) %>%
    modify_depth(2, ~rnorm(1, mean = ., sd = lapse + abs(.) * gamma))
}


update_args <- function(options_ls, u_fun = c("u_sal_wta", "u_sal_vfirst", "u_sal_pfirst"), args) {

  u_fun <- match.arg(u_fun)

  sal_vol <- map(options_ls, pluck("vols")) %>%
    map_dbl(1) %>%
    get_rint()

  sal_prob <- map(options_ls, pluck("probs")) %>%
    map_dbl(1) %>%
    get_rint()

  v_threshold <- pluck(args, "v_threshold")
  if (is_null(v_threshold)) v_threshold <- 0.8
  p_threshold <- pluck(args, "p_threshold")
  if (is_null(p_threshold)) p_threshold <- 0.8



  p_vol <- case_when(
      # u_fun == "u_sal_wta" & sal_prob == sal_vol ~ 0.5,
      u_fun == "u_sal_wta" & sal_vol > sal_prob ~ 1,
      u_fun == "u_sal_wta" & sal_vol < sal_prob ~ 0,
      u_fun == "u_sal_vfirst" & sal_vol > v_threshold ~ 1,
      u_fun == "u_sal_vfirst" & sal_vol < v_threshold &
        sal_prob > p_threshold ~ 0,
      u_fun == "u_sal_pfirst" & sal_prob > p_threshold ~ 0,
      u_fun == "u_sal_pfirst" & sal_prob < p_threshold &
        sal_vol > v_threshold ~ 1,
      TRUE ~ 0.5
    )

  list_modify(args, p_vol = p_vol, subj_val = TRUE, v_threshold = NULL, p_threshold = NULL)
}

u_fun <- "u_sal_vfirst"

args <- list(gamma = gamma, lapse = lapse, v_threshold = v_threshold, p_threshold = p_threshold)


# main choice function that takes a list of options, utility function and optional arguments and returns the selected option
choose_SUT <- function(options_ls, u_fun = c("u_sut", "u_sut2", "u_rnonc", "u_sal_wta", "u_sal_vfirst", "u_sal_pfirst"), args) {

  u_fun <- match.arg(u_fun)

  if (str_detect(u_fun, "sal")) {
    options_ls <- u_all(options_ls, args)
    args <- update_args(options_ls, u_fun, args)
    u_fun <- "u_rnonc"
  }

  u <- map_dbl(options_ls, ~exec(u_fun, rew_ls = ., !!!args))

  choice <- which(u == max(u))

  if (length(choice) > 1) return(sample(choice, 1))

  choice
}

# take a table with experimental conditions and expand it to include the desired number of decisions and individuals
exp_tbl <- function(data_tbl, n_pokes = 100, n_inds = 100) {
  # check to see if 'cond' column exists
  exp_hlp <- function(data_tbl, n) {
    data_tbl %>%
      slice(rep(1:n(), each = n))
  }

  data_tbl %>%
    exp_hlp(n_pokes) %>%
    group_by(cond, experiment) %>%
    mutate(n_poke = 1:n()) %>%
    exp_hlp(n_inds) %>%
    group_by(cond, experiment, n_poke) %>%
    mutate(ind = 1:n()) %>%
    arrange(ind, experiment, cond, n_poke) %>%
    ungroup()
}


#### free parameters for initialization
lapse <- 0
beta <- 0.5
delta <- 0.9
gamma_sev <- 1.05
gamma_2scal <- 0.65
gamma_ru <- 0.05
p_vol <- 0.5
gamma_wta <- 0.7
gamma_pfirst <- 0.95
gamma_vfirst <- 0.5
p_threshold <- v_threshold <- 0.8

n_pokes <- 100
n_inds <- 100
sesoi <- 0.1

#### define experimental conditions
tdre1 <- tibble(cond = c("BPV1", "BPV2", "BVP1", "BVP2", "C", "I"),
               vol1 = c(4, 20, 4, 4, 4, 4),
               vol2 = c(4, 20, 20, 20, 20, 20),
               prob1 = c(0.2, 0.2, 0.2, 0.5, 0.2, 0.5),
               prob2 = c(0.5, 0.5, 0.2, 0.5, 0.5, 0.2),
               experiment = 1)

tdre2 <- tibble(cond = c("BPV1", "BPV2", "BVP1", "BVP2", "C", "I"),
               vol1 = c(4, 20, 4, 4, 4, 4),
               vol2 = c(4, 20, 20, 20, 20, 20),
               prob1 = c(0.2, 0.2, 0.2, 1, 0.2, 1),
               prob2 = c(1, 1, 0.2, 1, 1, 0.2),
               experiment = 2)

tdre3 <- tibble(cond = c("PV1", "PV2", "PV3", "PV4", "VP1", "VP2", "VP3", "VP4"),
               vol1 = c(4, 10, 15, 20, 4, 4, 4, 4),
               vol2 = c(4, 10, 15, 20, 10, 10, 10, 10),
               prob1 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.5, 0.7, 0.8),
               prob2 = c(0.5, 0.5, 0.5, 0.5, 0.2, 0.5, 0.7, 0.8),
               experiment = 3)

tdre <- tdre1 %>%
  bind_rows(tdre2, tdre3) %>%
  mutate(cond_num = if_else(vol1 == vol2,
                            vol1 / 20,
                            prob1),
         option_1 = map2(vol1, prob1, to_rew_list),
         option_2 = map2(vol2, prob2, to_rew_list))

###### calculate the main simulations
set.seed(42)
res_test <- tdre %>%
  exp_tbl(n_pokes = n_pokes, n_inds = n_inds) %>%
  rowwise() %>%
  mutate(choice_sev = choose_SUT(list(option_1, option_2),
                                 u_fun = "u_sut",
                                 list(gamma = gamma_sev, lapse = lapse)),
         choice_2scal = choose_SUT(list(option_1, option_2),
                                  u_fun = "u_sut2",
                                  list(gamma = gamma_2scal, lapse = lapse)),
         choice_rnonc = choose_SUT(list(option_1, option_2),
                                u_fun = "u_rnonc",
                                list(gamma = gamma_ru, lapse = lapse, p_vol = p_vol)),
         choice_wta = choose_SUT(list(option_1, option_2),
                                 u_fun = "u_sal_wta",
                                 list(gamma = gamma_wta, lapse = lapse)),
         choice_pfirst = choose_SUT(list(option_1, option_2),
                                    u_fun = "u_sal_pfirst",
                                    list(gamma = gamma_pfirst, lapse = lapse,
                                         p_threshold = p_threshold,
                                         v_threshold = v_threshold)),
        choice_vfirst = choose_SUT(list(option_1, option_2),
                                    u_fun = "u_sal_vfirst",
                                    list(gamma = gamma_vfirst, lapse = lapse,
                                         p_threshold = p_threshold,
                                         v_threshold = v_threshold))
         )

# tidy up results and save output
simulations <- res_test %>%
  select(ind, experiment, cond, contains("choice")) %>%
  pivot_longer(cols = contains("choice"), names_to = "model", values_to = "choice2") %>%
  group_by(ind, experiment, cond, model) %>%
  summarise(performance = mean(choice2 == 2)) %>%
  mutate(model = str_remove(model, "choice_"))

write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
            dec = ".", sep = ";", row.names = FALSE)


############# scalar property of the volume dispensing mechanism is too low to explain the scalar property in the choice of the mice
vol_errors <- read.csv2(file = "analysis/data/metadata/vol_measurements.csv",
                       header = TRUE, dec = ".", sep = ";", na.strings = "NA") %>%
  mutate(steps = factor(steps),
         cage = factor(cage)) %>%
  rowwise() %>%
  mutate(sub_vol = rnorm(1, mean = vol, sd = gamma_vfirst*vol)) # subjectively perceived volumes

vol_errors %>%
  group_by(cage) %>%
  summarise(mean = mean(ulstep), sd = sd(ulstep))

steps <- rep(c(12, 30, 60, 3, 6, 13), each = 1000)
# simulate 100 rewards from each each cage and for each  number of steps
sim_vols <- tibble(rep = 1:length(steps), steps = steps,
                   cage = rep(1:2, each = length(steps)/2)) %>%
  rowwise() %>%
  mutate(vol = ifelse(cage == 2, rnorm(1, mean = 1.56*steps, sd = 0.239*steps),
                      rnorm(1, mean = 0.33*steps, sd = 0.0336*steps)),
         sub_vol = rnorm(1, mean = vol, sd = gamma_vfirst*vol),
         steps = factor(steps),
         cage = factor(cage))


vol_errors %>%
  ggplot() +
  geom_density(aes(vol, fill = steps)) +
  facet_grid(cage ~ .) +
  geom_density(data = sim_vols, aes(vol, fill = steps), size = 1.2, alpha = 0.2)

# same as previous, but with added subjective distributions, in dashed lines
vol_errors %>%
  ggplot() +
  geom_density(aes(vol, fill = steps)) +
  facet_grid(cage ~ .) +
  geom_density(aes(sub_vol, fill = steps), size = 1.2,
               linetype = 2, alpha = 0.1) +
  geom_density(data = sim_vols, aes(vol, fill = steps), size = 1.2, alpha = 0.2) +
  theme_bw()

sim_vols %>%
  ggplot() +
  geom_density(aes(vol, fill = steps, linetype = cage), size = 1.2, alpha = 0.2) +
  theme_bw()


############# Sensitivity tests for gamma and other free parameters

miceChoices <- read.csv2(file = "analysis/data/ChoicesOutput.csv", header = TRUE, dec = ".", sep = ";",
                         na.strings = "NA") %>%
  mutate(vol = if_else(cohort == 2, vol * 4.8, vol))

miceChoices <- miceChoices %>%
  filter(hour(DateTime) < 9 | hour(DateTime) > 15) %>% # due to older program version the initial visits
  # before the starting could not be flagged and therefore could not be removed by load.R
  # therefore they need to be removed here
  group_by(IdLabel, day) %>%
  mutate(count = 1:n(),
         revcount = n():1,
         DateTime = as.POSIXct(DateTime),
         cohort = factor(cohort),
         relcount = cumsum(rel)) %>%
  ungroup()

summaries <- miceChoices %>%
  group_by(cond, IdLabel, experiment, cohort) %>%
  # take all visits after the first 150 visits at the relevant dispensers:
  filter(relcount > 150) %>%
  summarise(performance = mean(prof, na.rm = TRUE), sampling = 1 - mean(rel),
            Ntot = n(), Nrel = sum(rel), successes_perf = sum(prof, na.rm = T),
            successes_sampl =  Ntot - Nrel) %>%
  ungroup() %>%
  mutate(cohort = factor(cohort))

observed <- summaries %>%
  filter(str_detect(cond, "BP"), experiment %in% c(1, 4)) %>%
  group_by(cond) %>%
  summarise(obs_perf = round(mean(performance), 2))

obs_perf <- observed %>%
  pull(obs_perf) %>%
  mean() %>%
  round(1)

gammas <- seq(0.05, 2, by = 0.05)
p_vols <- seq(0, 1, by = 0.05)

# help function to calculate deviances
get_deviances_from_sims <- function(tbl, ...) {
  group_pars <- enquos(...)

  pars_to_chr <- map_chr(group_pars, quo_text)
  gamma_index <- which(pars_to_chr == "gamma")
  group_no_gamma <- group_pars[-gamma_index]

  tbl %>%
    group_by(!!!group_pars) %>%
    select(starts_with("choice")) %>%
    summarise_all(~mean(. == 2)) %>%
    group_by(!!!group_no_gamma) %>%
    nest() %>%
    mutate(pred_perf = map(data, ~predict(loess(choice ~ gamma, .)))) %>%
    unnest(c(data, pred_perf)) %>%
    left_join(observed) %>%
    rename(performance = choice) %>%
    mutate(deviance = (pred_perf - obs_perf)^2)
}

########## fits for sev, 2scal, wta
set.seed(15)
gen_pars <- cross_df(list(cond = c("BPV1", "BPV2"),
                        gamma = gammas)) %>%
  rowwise() %>%
  mutate(args = map(gamma, ~list(gamma = .x, lapse = 0)))

bl_gen_sims <- tdre %>%
  filter(str_detect(cond, "BP"), experiment == 1) %>%
  left_join(gen_pars) %>%
  exp_tbl(n_pokes = 10)


bl_gen_sims <- bl_gen_sims  %>%
  rowwise() %>%
  mutate(choice_wta = choose_SUT(list(option_1, option_2),
                                u_fun = "u_sal_wta",
                                args = args),
         choice_2scal = choose_SUT(list(option_1, option_2),
                                   u_fun = "u_sut2",
                                   args = args),
         choice_sev = choose_SUT(list(option_1, option_2),
                                 u_fun = "u_sut",
                                 args = args))

dev_gen_sims <- bl_gen_sims %>%
  select(cond, gamma, contains("choice")) %>%
  pivot_longer(cols = -(1:2), names_to = "model", values_to = "choice",
             names_pattern = "choice_(.*)") %>%
  arrange(model, gamma, choice) %>%
  get_deviances_from_sims(cond, gamma, model) %>%
  select(cond, gamma, model, performance, deviance) %>%
  arrange(model, gamma, cond)

write.table(dev_gen_sims, file = paste0(getwd(),"/analysis/data/sensitivity_gen.csv"),
            dec = ".", sep = ";", row.names = FALSE)

######### fit for pfirst
set.seed(33)
lxgr_pars <- cross_df(list(cond = c("BPV1", "BPV2"),
                         gamma = gammas, par = seq(0.05, 1, by = 0.05))) %>%
  rowwise() %>%
  mutate(args = map2(gamma, par, ~list(gamma = .x, lapse = 0, p_threshold = .y, v_threshold = .y)))

bl_lxgr_sims <- tdre %>%
  filter(str_detect(cond, "BP"), experiment == 1) %>%
  left_join(lxgr_pars) %>%
  exp_tbl(n_pokes = 10)

bl_pfirst_sims <- bl_lxgr_sims %>%
  rowwise() %>%
  mutate(choice = choose_SUT(list(option_1, option_2),
                                u_fun = "u_sal_pfirst",
                                args = args))

dev_pfirst_sims <- get_deviances_from_sims(bl_pfirst_sims, gamma, cond, par)

write.table(dev_pfirst_sims, file = paste0(getwd(),"/analysis/data/sensitivity_pfirst.csv"),
            dec = ".", sep = ";", row.names = FALSE)

############ fit for vfirst
set.seed(101)
bl_vfirst_sims <- bl_lxgr_sims %>%
  rowwise() %>%
  mutate(choice = choose_SUT(list(option_1, option_2),
                                u_fun = "u_sal_vfirst",
                                args = args))

dev_vfirst_sims <- get_deviances_from_sims(bl_vfirst_sims, gamma, cond, par)

write.table(dev_vfirst_sims, file = paste0(getwd(),"/analysis/data/sensitivity_vfirst.csv"),
            dec = ".", sep = ";", row.names = FALSE)
######### fit for nonc
set.seed(92)
rnonc_pars <- cross_df(list(cond = c("BPV1", "BPV2"),
                         gamma = gammas, par = p_vols)) %>%
  rowwise() %>%
  mutate(args = map2(gamma, par, ~list(.x, 0, .y)))

bl_rnonc_sims <- tdre %>%
  filter(str_detect(cond, "BP"), experiment == 1) %>%
  left_join(rnonc_pars) %>%
  exp_tbl(n_pokes = 10)

bl_rnonc_sims <- bl_rnonc_sims %>%
  rowwise() %>%
  mutate(choice = choose_SUT(list(option_1, option_2),
                                u_fun = "u_rnonc",
                                args = args))

dev_rnonc_sims <- get_deviances_from_sims(bl_rnonc_sims, gamma, cond, par)

write.table(dev_rnonc_sims, file = paste0(getwd(),"/analysis/data/sensitivity_rnonc.csv"),
            dec = ".", sep = ";", row.names = FALSE)
