library(tidyverse)
library(assertthat)

#### helper functions

# sample from a list of n options (represented by a vector of probabilities) with replacement
prob_sample <- function(probs, n_sims = 1) {
  sample(length(probs), size = n_sims, prob = probs, replace = TRUE)
}

# calculate the relative intensity difference / mean ratio
get_rint <- function(vec) { # generalized relative intensity function for n inputs
  if (max(vec) == min(vec) & min(vec) == 0) return(0)
  length(vec) * (max(vec) - min(vec)) / sum(vec)
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



# # stochastic prob_sample
# st_sample <- function(probs) {
#   p <- runif(1, 0, 1)
#   upper_probs <- cumsum(probs)
#   lower_probs <- lag(upper_probs, default = 0)
#   which(map2_lgl(.x = lower_probs, .y = upper_probs, ~between(p, .x, .y)))
# }

### Cumulative Prospect Theory functions
get_utility <- function(v, alpha, ref, kappa) {
  get_utility_hlp <- function(v, alpha = 1, ref = 0, kappa = 1) {
    if (v > ref) {
      return((v - ref)^alpha)
    } else {
      return(-kappa*(ref - v)^alpha)
    }
  }
  map_dbl(v, get_utility_hlp, alpha = alpha, ref = ref, kappa = kappa)
}

get_weighted_probs <- function(p, beta, delta) {
  get_w_p_hlp <- function(p, beta = beta, delta = delta) {
    if (p == 0) return(0)
    exp(-beta*(-log(p))^delta)
  }
  map_dbl(p, get_w_p_hlp, beta = beta, delta = delta)
}

get_cum_weights <- function(p, beta, delta) {
  p_1 <- get_weighted_probs(p[1], beta = beta, delta = delta)
  p_cum <- get_weighted_probs(cumsum(p), beta = beta, delta = delta) -
    get_weighted_probs(lag(cumsum(p), default = 0), beta = beta, delta = delta)
  p_cum[1] <- p_1
  p_cum
}

# rew_ls <- to_rew_list(vols, probs)

get_cum_utility <- function(rew_ls, ref, maxu) {

  sum(get_utility(rew_ls$vols, alpha = alpha, ref = ref, kappa = kappa) *
        get_cum_weights(rew_ls$probs, beta = beta, delta = delta)) / maxu

}

# decision function from Constantinople et al. 2019
choose_CPT <- function(rew_ls1, rew_ls2, temp, lapse, ref, maxu) {

  delta_value <- get_cum_utility(rew_ls1, ref = ref, maxu = maxu) - get_cum_utility(rew_ls2, ref = ref, maxu = maxu)

  prob1 <- lapse + ((1 - 2*lapse)/(1 + exp(-1 / temp * delta_value)))
  p <- runif(1, 0, 1)

  if (p > prob1) return(2)

  1
}

# u_misses <- function(rew_ls, gamma, lapse, transform_probs = FALSE) {
#   vols <- pluck(rew_ls, "vols")
#   probs <- pluck(rew_ls, "probs")
#
#   if (transform_probs) probs <- cum_weights(probs, beta = beta, delta = delta)
#
#
#   misses <- 1 / (probs + 0.1 * rnorm(1))
#   u <- vols / misses
#   u <- rnorm(length(u), mean = u, sd = lapse + abs(u) * gamma)
#   sum(u)
# }

# scalar expected value
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

  if (transform_probs) probs <- cum_weights(probs, beta = beta, delta = delta)


  u <- rnorm(length(vols), mean = vols, sd = lapse + abs(vols) * gamma) *
    rnorm(length(probs), mean = probs, sd = lapse + abs(probs) * gamma)
  sum(u)
}


u_sutn <- function(rew_ls, gamma, lapse, transform_probs = FALSE) {
  vols <- pluck(rew_ls, "vols")
  probs <- pluck(rew_ls, "probs")

  if (transform_probs) probs <- cum_weights(probs, beta = beta, delta = delta)

  u <- rnorm(length(vols), mean = vols, sd = lapse + abs(vols) * gamma)
  u[prob_sample(probs)]
}


# scalar utility from random dimension
u_rnonc <- function(rew_ls, gamma, lapse, p_vol = 0.5, transform_probs = FALSE, subj_val = FALSE) {
  vols <- pluck(rew_ls, "vols")
  probs <- pluck(rew_ls, "probs")

  if (transform_probs & !subj_val) probs <- cum_weights(probs, beta = beta, delta = delta)

  p <- runif(1, 0, 1)

  if (p < p_vol) {
    dim <- vols[1]
  } else {
    dim <- probs[1]
  }
  if (subj_val) {
    return(dim)
  }
  rnorm(1, mean = dim, sd = lapse + abs(dim) * gamma)
}

# tibble(n = 1:1000) %>%
#   rowwise() %>%
#   mutate(half = u_sut(to_rew_list(vols = vols, probs = probs), gamma = gamma, lapse = lapse),
#          whole = u_sut(to_rew_list(vols = vols1, probs = probs1), gamma = gamma, lapse = lapse)) %>%
#   ggplot() +
#   geom_boxplot(aes(1, half)) +
#   geom_boxplot(aes(3, whole))

#
options_ls

u_all(SUT_conds$option_1[4], list(gamma = gamma, lapse = lapse, u_fun = "u_sal_vfirst"))


get_salience <- function(options_ls) {

  sal_best_v <- map(options_ls, pluck("vols")) %>%
    map_dbl(pluck(1)) %>%
    get_rint()

  sal_best_p <- map(options_ls, pluck("probs")) %>%
    map_dbl(pluck(1)) %>%
    get_rint()

  sal_worst_v <- map(options_ls, pluck("vols")) %>%
    map_dbl(pluck(length(.))) %>%
    get_rint()

 list(sal_best_v = sal_best_v, sal_best_p = sal_best_p,
       sal_worst_v = sal_worst_v)
}



# in multioutcome cases, the volume first goes for best reward, second best reward, etc. until 0
# probability first goes for prob of best reward, prob of second best, etc. until 0
u_all <- function(options_ls, args) {
  gamma <- pluck(args, "gamma")
  lapse <- pluck(args, "lapse")
  # vols <- map(options_ls, pluck("vols"))

  modify_depth(options_ls, 2, pluck(1)) %>%
    modify_depth(2, ~rnorm(1, mean = ., sd = lapse + abs(.) * gamma))

  # modify_depth(options_ls, 3, ~rnorm(1, mean = ., sd = lapse + abs(.) * gamma))
}

get_rint(c(0.05, 0.002))
update_args <- function(options_ls, u_fun = c("u_sal_wta", "u_sal_vfirst", "u_sal_pfirst"), args) {

  u_fun <- match.arg(u_fun)

  sal_vol <- map(options_ls, pluck("vols")) %>%
    map_dbl(1) %>%
    get_rint()

  sal_prob <- map(options_ls, pluck("probs")) %>%
    map_dbl(1) %>%
    get_rint()
  # saliences <- get_salience(options_ls)

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

SUT_conds$option_1[4]
u_fun <- "u_sal_vfirst"
options_ls <- c(SUT_conds$option_1[21], SUT_conds$option_2[21])
args <- list(gamma = gamma, lapse = lapse, v_threshold = v_threshold, p_threshold = p_threshold)

choose_SUT(c(SUT_conds$option_1[21], SUT_conds$option_2[21]), u_fun = "u_sal_vfirst", args = list(gamma = gamma, lapse = lapse, v_threshold = v_threshold, p_threshold = p_threshold))
# args have to be a list
choose_SUT <- function(options_ls, u_fun = c("u_sut", "u_sut2", "u_rnonc", "u_sal_wta", "u_sal_vfirst", "u_sal_pfirst"), args) {

  u_fun <- match.arg(u_fun)

  if (str_detect(u_fun, "sal")) {
    options_ls <- u_all(options_ls, args)
    args <- update_args(options_ls, u_fun, args)
    u_fun <- "u_rnonc"
  }
map_dbl(options_ls, ~exec("u_rnonc", rew_ls = ., !!!args))
  u <- map_dbl(options_ls, ~exec(u_fun, rew_ls = ., !!!args))

  choice <- which(u == max(u))

  if (length(choice) > 1) return(sample(choice, 1))

  choice
}

# gamma <- 0.5
#
option_1 <- to_rew_list(c(20, 0), c(0.2, 0.8))
option_2 <- to_rew_list(c(4, 0), c(0.5, 0.5))

options_ls <- list(option_1, option_2)


# update_args(options_ls, u_fun = "u_sal_vfirst", list(gamma = gamma, lapse = lapse, v_threshold = 0.7, p_threshold = 0.7))

# choose_SUT(options_ls, u_fun = "u_sal_vfirst", list(gamma = gamma, lapse = lapse, v_threshold = v_threshold, p_threshold = p_threshold))
#

lapse <- 0
gamma <- 0.95
# salience sims#########
n_sim <- 10000
tibble(conds = factor(c(1, 2, 3, 4, 5, 6)), vol1 = c(4, 4, 20, 4, 4, 4), vol2 = c(4, 20, 20, 4, 4, 20),
       prob1 = c(0.2, 0.2, 1, 0.2, 0.2, 0.2), prob2 = c(0.2, 1, 1, 1, 0.5, 0.5)) %>%
  slice(rep(1:n(), each = n_sim)) %>%
  mutate_if(is.numeric, ~rnorm(., mean = ., sd = lapse + gamma * .)) %>%
  rowwise() %>%
  mutate(sal_vol = get_rint(c(vol1, vol2)),
         sal_prob = get_rint(c(prob1, prob2))) %>%
  ungroup() %>%
  group_by(conds) %>%
  summarise(overt_vol = sum(sal_vol > 0.8)/n_sim,
            overt_prob = sum(sal_prob > 0.8)/n_sim,
            vol_over_prob = sum(sal_vol > sal_prob)/n_sim)
(1 - 0.286) * 0.86

# choose_WTA <- function(means, dims = 2, gamma, lapse) {
#
#   # p <- runif(1, 0, 1)
#   # n_options <- length(means)/dims
#   #
#   # if (p < lapse) return(sample(1:n_options, 1) - 1)
#   # gamma <- gamma - 0.2
#   vals <- rnorm(means, mean = means, sd = lapse + gamma * abs(means))
#   vals <- dim_splitter(vals, dims)
#   salience <- map_dbl(vals, get_rint)
#
#   salience <- ifelse(is.nan(salience), 0, salience)
#   if (sd(salience) == 0) return(choose_randdim(means, dims = dims, gamma = gamma, lapse = lapse))
#
#   dim_salient <- which(salience == max(salience))
#   res <- vals[[dim_salient]]
#   return(which(res == max(res)) - 1)
# }



exp_tbl <- function(data_tbl, n_pokes = 100, n_inds = 100) {
  # check to see if 'cond' column exists
  exp_hlp <- function(data_tbl, n) {
    data_tbl %>%
      slice(rep(1:n(), each = n))
  }

  # tbl_size <- nrow(data_tbl) * n_choices * n_inds
  # m <- matrix(0, nrow = tbl_size, ncol = n_options)
  # colnames(m) <- map_chr(c(1:n_options), ~paste0("est", .x))

  data_tbl %>%
    exp_hlp(n_pokes) %>%
    group_by(cond, experiment) %>%
    mutate(n_poke = 1:n()) %>%
    exp_hlp(n_inds) %>%
    group_by(cond, experiment, n_poke) %>%
    mutate(ind = 1:n()) %>%
    arrange(ind, experiment, cond, n_poke) %>%
    # bind_cols(as_tibble(m)) %>%
    ungroup()
}


#### free parameters
v_max <- 20
alpha <- 0.7
kappa <- 1.7
beta <- 0.5
delta <- 0.7
# temp <- 2
temp <- 1.8
lapse <- 0
ref <- 0
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
u_max <- get_utility(v_max, alpha, ref, kappa)
sesoi <- 0.1


#### experiment 1
mdre1 <- tibble(cond = c("BPV1", "BPV2", "BVP1", "BVP2", "C", "I"),
               vol1 = c(4, 20, 4, 4, 4, 4),
               vol2 = c(4, 20, 20, 20, 20, 20),
               prob1 = c(0.2, 0.2, 0.2, 0.5, 0.2, 0.5),
               prob2 = c(0.5, 0.5, 0.2, 0.5, 0.5, 0.2),
               experiment = 1)

mdre2 <- tibble(cond = c("BPV1", "BPV2", "BVP1", "BVP2", "C", "I"),
               vol1 = c(4, 20, 4, 4, 4, 4),
               vol2 = c(4, 20, 20, 20, 20, 20),
               prob1 = c(0.2, 0.2, 0.2, 1, 0.2, 1),
               prob2 = c(1, 1, 0.2, 1, 1, 0.2),
               experiment = 2)

mdre3 <- tibble(cond = c("PV1", "PV2", "PV3", "PV4", "VP1", "VP2", "VP3", "VP4"),
               vol1 = c(4, 10, 15, 20, 4, 4, 4, 4),
               vol2 = c(4, 10, 15, 20, 10, 10, 10, 10),
               prob1 = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.5, 0.7, 0.8),
               prob2 = c(0.5, 0.5, 0.5, 0.5, 0.2, 0.5, 0.7, 0.8),
               experiment = 3)

mdre <- mdre1 %>%
  bind_rows(mdre2, mdre3) %>%
  mutate(cond_num = if_else(vol1 == vol2,
                            vol1 / 20,
                            prob1),
         option_1 = map2(vol1, prob1, to_rew_list),
         option_2 = map2(vol2, prob2, to_rew_list))

set.seed(42)


res_test <- mdre %>%
  # slice(1) %>%
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
         # choice_cpt = choose_CPT(option_1, option_2, temp = temp,
         #                         lapse = lapse, ref = ref, maxu = u_max),
         choice_vfirst = choose_SUT(list(option_1, option_2),
                                    u_fun = "u_sal_vfirst",
                                    list(gamma = gamma_vfirst, lapse = lapse,
                                         p_threshold = p_threshold,
                                         v_threshold = v_threshold))
         )


simulations <- res_test %>%
  select(ind, experiment, cond, cond_num, contains("choice")) %>%
  pivot_longer(cols = contains("choice"), names_to = "model", values_to = "choice2") %>%
  group_by(ind, experiment, cond, cond_num, model) %>%
  summarise(performance = mean(choice2 == 2)) %>%
  mutate(model = str_remove(model, "choice_"))

write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
            dec = ".", sep = ";")


summ_sims <- simulations %>%
  group_by(experiment, cond, model) %>%
  summarise(m_perf = median(performance))

summ_sims <- summ_sims %>%
  filter(experiment == 1) %>%
  ungroup() %>%
  mutate(experiment = 4) %>%
  bind_rows(summ_sims) %>%
  arrange(experiment, cond, model)

emp_perf <- summaries %>%
  filter(!str_detect(cond, "[r]")) %>%
  select(IdLabel, cohort, experiment, cond, performance)

emp_perf <- emp_perf %>%
  filter(cond == "BVP1") %>%
  mutate(experiment = 2) %>%
  bind_rows(emp_perf) %>%
  arrange(IdLabel, cohort, cond)

devs <- summ_sims %>%
  full_join(emp_perf) %>%
  mutate(deviance = (m_perf - performance)^2,
         dev = abs(m_perf - performance))


RMSEs <- devs %>%
  filter(!(str_detect(cond, "BP") & experiment != 2)) %>%
  group_by(experiment, model) %>%
  summarise(RMSE = sqrt(mean(deviance, na.rm = TRUE))) %>%
  arrange(experiment, RMSE)

rmse_ranks <- function(tibb) {
  tibb %>%
    mutate(rank = rank(RMSE)) %>%
    ungroup() %>%
    select(-RMSE) %>%
    spread(experiment, model)
}

ranks <- RMSEs %>%
  rmse_ranks()
ranks



tibble(vol1 = 5, vol2 = 3) %>%
  mutate(vols = list(vol1, vol2))

option_1 <- to_rew_list(c(20, 5), c(0.5, 0.5))
option_2 <- to_rew_list(12.5, 1)
option_3 <- to_rew_list(c(12.5, 12.5), c(0.5, 0.5))
options_ls <- list(option_1, option_2)

to_rew_list(c(2, 3, 4), c(0.2, 0.1, 0.7))
list(vols = c(2, 3, 4), probs = c(0.2, 0.1, 0.7))


tibble(n = 1:1000) %>%
  rowwise() %>%
  mutate(u_1 = u_sut(option_1, lapse = lapse, gamma = gamma_sut),
         u_2 = u_sut(option_2, lapse = lapse, gamma = gamma_sut),
         u_3 = u_sut(option_3, lapse = lapse, gamma = gamma_sut)) %>%
  ggplot() +
  geom_boxplot(aes(1, u_1)) +
  geom_boxplot(aes(2, u_2)) +
  geom_boxplot(aes(3, u_3))
# update_args(options_ls, u_fun = "u_sal_vfirst", list(gamma = gamma, lapse = lapse, v_threshold = 0.7, p_threshold = 0.7))

choose_SUT(options_ls, u_fun = "u_sal_vfirst", list(gamma = gamma_wta, lapse = lapse, v_threshold = v_threshold, p_threshold = p_threshold))




lapse <- 0
# gamma <- 0.46
gamma <- 0.65
n_mice <- 100
n_choices <- 100
dims <- 4

# means <- c(20, 12.5, 0.8, 0.5, 5, 12.5, 0.2, 0.5)
# means <- c(5, 12.5, 1, 1, 5, 12.5, 1, 1)
means <- c(5, 12.5, 0.5, 0.5, 5, 12.5, 0.5, 0.5)
means <- c(20, 12.5, 0.5, 0.5, 20, 12.5, 0.5, 0.5)

tibble(n = 1:1000) %>%
  rowwise() %>%
  mutate(choice = choose_WTA(means, dims = dims, gamma = gamma, lapse = lapse)) %>%
  ungroup() %>%
  summarise(performance = mean(choice))


means <- c(5, 12.5, 1, 1, 5, 12.5)
means <- c(20, 12.5, 1, 1, 20, 12.5)


means <- c(20, 12.5, 0.95, 0.5, 5, 12.5)



to_rew_list2 <- function(max, sec, probm, probsec) {
  sp <- probm + probsec

  assert_that(sp <= 1, msg = "Probabilities must sum to at most one 1")

  if (round(sp, 3) < 1) {
    vols <- c(max, sec, 0)
    probs <- c(probm, probsec, 1 - sp)
  } else {
    vols <- c(max, sec)
    probs <- c(probm, probsec)
  }

  list(vols = vols, probs = probs)
}


SUT_conds <- tibble(experiment = 1,
                    max = c(5, rep(20, 20)),
                    max2 = 12.5,
                    probm = c(0.5, 0.5, seq(0.05, 0.95, by = 0.05)),
                    probm2 = 1,
                    sec = c(5, 20, rep(5, 19)),
                    sec2 = 12.5,
                    probsec = c(0.5, 0.5, seq(0.95, 0.05, by = -0.05)),
                    probsec2 = 0,
                    cond = c(0, 1, seq(0.05, 0.95, by = 0.05))) %>%
  mutate(option_1 = pmap(list(max, sec, probm, probsec), to_rew_list2),
         option_2 = pmap(list(max2, sec2, probm2, probsec2),
                         to_rew_list2)) %>%
  rowwise() %>%
  mutate(
    return = max * probm + sec * probsec,
    return2 = max2 * probm2 + sec2 * probsec2,
    rintmax = get_rint(c(max, max2)),
    rintprobm = get_rint(c(probm, probm2)),
    rintsec = get_rint(c(sec, sec2)),
    rintprobs = get_rint(c(probsec, probsec2))) %>%
  ungroup()


set.seed(42)


res_SUT <- SUT_conds %>%
  # slice(1) %>%
  exp_tbl(n_pokes = n_pokes, n_inds = n_inds) %>%
  rowwise() %>%
  mutate(choice_sev = choose_SUT(list(option_1, option_2),
                                 u_fun = "u_sut",
                                 list(gamma = gamma_sut, lapse = lapse)),
         choice_2scal = choose_SUT(list(option_1, option_2),
                                   u_fun = "u_sut2",
                                   list(gamma = gamma_2scal, lapse = lapse)),
         choice_nonc = choose_SUT(list(option_1, option_2),
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
         choice_cpt = choose_CPT(option_1, option_2, temp = temp,
                                 lapse = lapse, ref = ref, maxu = u_max),
         choice_vfirst = choose_SUT(list(option_1, option_2),
                                    u_fun = "u_sal_vfirst",
                                    list(gamma = gamma_vfirst, lapse = lapse,
                                         p_threshold = p_threshold,
                                         v_threshold = v_threshold))
  )


SUT_conds$option_1[4]



simulations_SUT <- res_SUT %>%
  select(ind, experiment, cond, contains("choice")) %>%
  pivot_longer(cols = contains("choice"), names_to = "model", values_to = "choice2") %>%
  group_by(ind, experiment, cond,  model) %>%
  summarise(performance = mean(choice2 == 2)) %>%
  mutate(model = str_remove(model, "choice_"))

write.table(simulations_SUT, file = paste0(getwd(),"/analysis/data/simulations_SUT.csv"),
            dec = ".", sep = ";")


simulations_SUT <- read.csv2(file =  paste0(getwd(),"/analysis/data/simulations_SUT.csv"), dec = ".", sep = ";")


simulations_SUT %>%
  ggplot(aes(cond, performance, color = model)) +
  stat_summary() +
  stat_smooth(method = lm, se = FALSE)



# simulations %>%
#   ggplot() +
#   geom_density(aes(performance), size = 1.2) + facet_grid(model ~ cond)


sesoi <- 0.1
pred_lines <- simulations %>%
  filter(cond %in% c(0, 1)) %>%
  group_by(model, cond) %>%
  summarise(performance = mean(performance),
            min_perf = performance - sesoi,
            max_perf = performance + sesoi)

alpha_n <- 0.05 / (3 - 1)

simulations %>%
  ggplot() +
  stat_summary(aes(cond, performance),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - alpha_n), size = 0.5) +
  facet_grid(model ~ .) +
  stat_summary(data = pred, aes(cond, cert),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - alpha_n), size = 0.3, color = "cornflowerblue") +
  geom_line(data = pred_line, aes(cond, mean_cert), color = "cornflowerblue") +
  geom_line(data = pred_lines, aes(cond, performance)) +
  geom_line(data = pred_lines, aes(cond, min_perf), linetype = 2) +
  geom_line(data = pred_lines, aes(cond, max_perf), linetype = 2) +
  theme_bw()

m1_pred <- pred_lines %>% filter(model == 1)

# Rosenstroem et al.'s prediction s < m < b choicevariable = pismall * prefsmall + pibig * prefbig

SUTpred <- simulations %>%
  filter(cond %in% c(0, 1)) %>%
  group_by(model, cond) %>%
  summarise(performance = mean(performance)) %>%
  spread(cond, performance) %>%
  right_join(tibble(model = rep(1:5, each = 101),
                    prob = rep(seq(0, 1, by = 0.01), 5))) %>%
  # mutate(pred = 1 - (1 - `0`)*(1 - prob) - prob*(1 - `1`)) %>%
  mutate(pred = 1 - (1 - `0`)*(1 - prob) - prob*(1 - `1`)) %>%
  filter(model < 4)

SUTpred %>%
  ggplot(aes(prob, pred, color = as.factor(model))) + geom_line()

simulations %>%
  ggplot(aes(color = as.factor(model))) +
  stat_summary(aes(cond, performance),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - alpha_n), size = 0.5) +
  # geom_line(data = m1_pred, aes(cond, performance)) +
  # geom_line(data = m1_pred, aes(cond, min_perf), linetype = 2) +
  # geom_line(data = m1_pred, aes(cond, max_perf), linetype = 2) +
  geom_line(data = SUTpred, aes(prob, pred, color = as.factor(model))) +
  theme_bw()
# stat_summary(data = pred, aes(cond, cert),
#              fun.data = mean_cl_boot,
#              fun.args = list(conf.int = 1 - alpha_n), size = 0.3, color = "darkred")


################
################ CHAG experiment

# lapse <- 0.32
# cv <- 0.46
gamma <- 0.8
n_mice <- 100
n_choices <- 100
dims <- 4

means <- c(20, 8, 0.25, 0.5, 5, 0, 0.25, 0)

tibble(n = 1:1000) %>%
  rowwise() %>%
  mutate(choice = choose_SUT_n(means, dims = dims, gamma = gamma, lapse = lapse)) %>%
  ungroup() %>%
  summarise(performance = mean(choice))


CHAG_conds  <- tibble(experiment = rep(c(1, 2), each = 20),
       max = 20,
       max2 = rep(seq(8, 17.5, by = 0.5), 2),
       probm = rep(c(0.5, 0.25), each = 20),
       probm2 = 1,
       sec = 5,
       sec2 = c(seq(8, 17.5, by = 0.5), rep(0, 20)),
       probsec = rep(c(0.5, 0.25), each = 20),
       probsec2 = 0,
       # name = c("H", "G", "A", "C", "H/2", "G/2", "A/2", "C/2"),
       cond = rep(seq(8, 17.5, by = 0.5), 2)) %>%
  mutate(option_1 = pmap(list(max, sec, probm, probsec), to_rew_list2),
         option_2 = pmap(list(max2, sec2, probm2, probsec2),
                         to_rew_list2)) %>%
  rowwise() %>%
  mutate(
    return = max * probm + sec * probsec,
    return2 = max2 * probm2 + sec2 * probsec2,
    rintmax = get_rint(c(max, max2)),
    rintprobm = get_rint(c(probm, probm2)),
    rintsec = get_rint(c(sec, sec2)),
    rintprobs = get_rint(c(probsec, probsec2))) %>%
  ungroup()

# CHAG_conds <- tibble(experiment = rep(c(1, 2), each = 20),
#                      # name = c("H", "G", "A", "C", "H/2", "G/2", "A/2", "C/2"),
#                      cond = rep(seq(8, 17.5, by = 0.5), 2),
#                      max = 20,
#                      max2 = rep(seq(8, 17.5, by = 0.5), 2),
#                      probm = rep(c(0.5, 0.25), each = 20),
#                      probm2 = 0.5,
#                      sec = 5,
#                      sec2 = c(seq(8, 17.5, by = 0.5), rep(0, 20)),
#                      probs = rep(c(0.5, 0.25), each = 20),
#                      probs2 = 0.5) %>%
#   rowwise() %>%
#   mutate(return = max * probm + sec * probs,
#          return2 = max2 * probm2 + sec2 * probs2,
#          rintmax = get_rint(c(max, max2)),
#          rintprobm = get_rint(c(probm, probm2)),
#          rintsec = get_rint(c(sec, sec2)),
#          rintprobs = get_rint(c(probs, probs2)))

# CHAG_model_list <- list("choose_hurdle_n", "choose_SUT_n", "choose_prob_first", "choose_priority")
# # "choose_expected", "choose_hurdle_n",, , "choose_prob_first", "choose_priority")

set.seed(42)

res_CHAG <- CHAG_conds %>%
  # slice(1) %>%
  exp_tbl(n_pokes = n_pokes, n_inds = n_inds) %>%
  rowwise() %>%
  mutate(choice_sev = choose_SUT(list(option_1, option_2),
                                 u_fun = "u_sut",
                                 list(gamma = gamma_sut, lapse = lapse)),
         choice_2scal = choose_SUT(list(option_1, option_2),
                                   u_fun = "u_sut2",
                                   list(gamma = gamma_2scal, lapse = lapse)),
         choice_nonc = choose_SUT(list(option_1, option_2),
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
         choice_cpt = choose_CPT(option_1, option_2, temp = temp,
                                 lapse = lapse, ref = ref, maxu = u_max),
         choice_vfirst = choose_SUT(list(option_1, option_2),
                                    u_fun = "u_sal_vfirst",
                                    list(gamma = gamma_vfirst, lapse = lapse,
                                         p_threshold = p_threshold,
                                         v_threshold = v_threshold))
  )


SUT_conds$option_1[4]



simulations_CHAG <- res_CHAG %>%
  select(ind, experiment, cond, contains("choice")) %>%
  pivot_longer(cols = contains("choice"), names_to = "model", values_to = "choice2") %>%
  group_by(ind, experiment, cond,  model) %>%
  summarise(performance = mean(choice2 == 2)) %>%
  mutate(model = str_remove(model, "choice_"))

# write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
#             dec = ".", sep = ";")

# CHAG_lines <- CHAG_sims %>%
#   filter(cond %in% c(max(cond), min(cond))) %>%
#   group_by(model, cond) %>%
#   summarise(performance = mean(performance)) %>%
#   spread(cond, performance) %>%
#   right_join(tibble(model = rep(1:5, each = 101),
#                     prob = rep(seq(0, 1, by = 0.01), 5))) %>%
#   mutate(pred = 1 - (1 - `0`)*(1 - prob) - prob*(1 - `1`))


simulations_CHAG %>%
  mutate(prob = ifelse(experiment == 1, 1, 0.5)) %>%
  ggplot() +
  stat_summary(aes(cond, performance),
               fun.data = mean_cl_boot,
               fun.args = list(0.95), size = 0.5) +
  stat_smooth(aes(cond, performance), method = lm) +
  facet_grid(model ~ experiment) +
  geom_hline(yintercept = 0.5, linetype = 2)

# stat_summary(data = Perf, aes(cert, performance),
  #              fun.data = mean_cl_boot,
  #              fun.args = list(conf.int = 1 - alpha_n), size = 0.3, color = "darkred") +
  # geom_hline(yintercept = 0.5, linetype = 2)

# m1_pred <- pred_lines %>% filter(model == 2)
#
# simulations %>%
#   ggplot(aes(color = as.factor(model))) +
#   stat_summary(aes(cond, performance),
#                fun.data = mean_cl_boot,
#                fun.args = list(conf.int = 1 - alpha_n), size = 0.5) +
#   geom_line(data = m1_pred, aes(cond, performance)) +
#   geom_line(data = m1_pred, aes(cond, min_perf), linetype = 2) +
#   geom_line(data = m1_pred, aes(cond, max_perf), linetype = 2)
#

### Rosenstroem et al.'s geometric mean example
gamma <- 0.5

# G_conds <- tibble(experiment = 1,
#                   cond = 1,
#                   max = 5,
#                   max2 = 5^(1/3),
#                   probm = 1/3,
#                   probm2 = 0.5,
#                   sec = 1,
#                   sec2 = 5^(1/3),
#                   probs = 2/3,
#                   probs2 = 0.5) %>%
#
G_conds <- tibble(experiment = 1,
                  cond = 1,
                  max = 20,
                  max2 = 10,
                  probm = 0.5,
                  probm2 = 0.5,
                  sec = 5,
                  sec2 = 10,
                  probs = 0.5,
                  probs2 = 0.5) %>%
  rowwise() %>%
  mutate(return = max * probm + sec * probs,
         return2 = max2 * probm2 + sec2 * probs2,
         rintmax = get_rint(c(max, max2)),
         rintprobm = get_rint(c(probm, probm2)),
         rintsec = get_rint(c(sec, sec2)),
         rintprobs = get_rint(c(probs, probs2)))


G_model_list <- list("choose_expected", "choose_hurdle_n", "choose_SUT_n",
                     "choose_randdim", "choose_WTA", "choose_prob_first", "choose_priority")

set.seed(42)
G_sims <- map2_df(G_model_list,
                     as.list(1:length(G_model_list)),
                     ~modeller(G_conds, n_mice = n_mice, n_choices = n_choices,
                               choice_fun = .x,
                               input_vec = c(max, max2, probm, probm2, sec, sec2, probs, probs2),
                               model_num = .y, dims = dims, gamma = gamma, lapse = lapse))


G_sims %>%
  ggplot(aes(model, performance)) + stat_summary(fun.data = mean_cl_boot,
                                                 fun.args = list(0.95), size = 0.5)


cv <- 0.3
mean <- 10
mean2 <- 0.3

est_cv <- function(x) {
  sd(x)/mean(x)
}

distrib <- tibble(mean = mean, mean2 = mean, cv = seq(0, 1, length.out = 100)) %>%
  rowwise() %>%
  mutate(est = est_cv(rnorm(10000, mean = mean, sd = mean*cv) * rnorm(10000, mean = mean2, sd = mean2*cv)),
         pred_cv = 1.24*cv + 0.49*cv^2)

distrib %>%
  ggplot(aes(cv, est)) + geom_point() + geom_line() +
  stat_smooth(method = lm) +
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1) +
  geom_point(aes(cv, pred_cv), color = "red")


lm(data = distrib, cv ~ est + I(est^2))

(cvest <- sd(distrib)/mean(distrib))
sqrt(cvest)

var1 <- (mean*cv)^2
var2 <- (mean2*cv)^2

sqrt(var1*var2 + var1*mean2^2 + var2*mean^2)

sqrt((mean*cv)^2 + (mean2*cv)^2)/(mean*mean2)

plot(density(distrib))


#### scalar property of the volume dispensing mechanism is too low to explain the scalar property in the choice of the mice
vol_errors <- read.csv2(file = "analysis/data/vol_measurements.csv",
                       header = TRUE, dec = ".", sep = ";", na.strings = "NA") %>%
  mutate(steps = factor(steps),
         cage = factor(cage))

vol_errors %>%
  ggplot(aes(dispenser, ulstep, color = cage)) +
  stat_summary(fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95), size = 0.3)


vol_errors %>%
  ggplot(aes(dispenser, ulstep, color = steps)) +
  stat_summary(fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95), size = 0.3) +
  facet_wrap(. ~ cage)


vol_errors %>%
  group_by(cage) %>%
  summarise(mean = mean(ulstep), sd = sd(ulstep))


steps <- rep(c(12, 30, 60, 3, 6, 13), each = 100)
sim_vols <- tibble(rep = 1:length(steps), steps = steps,
                   cage = rep(1:2, each = length(steps)/2)) %>%
  rowwise() %>%
  mutate(vol = ifelse(cage == 2, rnorm(1, mean = 1.56*steps, sd = 0.239*steps),
                      rnorm(1, mean = 0.33*steps, sd = 0.0336*steps)),
         sub_vol = rnorm(1, mean = vol, sd = cv*vol),
         steps = factor(steps),
         cage = factor(cage))


vol_errors %>%
  ggplot() +
  geom_density(aes(vol, fill = steps)) +
  facet_grid(cage ~ .) +
  geom_density(data = sim_vols, aes(vol, fill = steps), size = 1.2, alpha = 0.2)
  # geom_density(data = sim_vols, aes(sub_vol, fill = steps), size = 1.2, linetype = 2, alpha = 0.1)


sim_vols %>%
  # filter(steps %in% c(13, 60)) %>%
  ggplot() +
  geom_density(aes(vol, fill = steps, linetype = cage), size = 1.2, alpha = 0.2) +
  theme_bw()

########### psychometric function for probability discrimination
###########
###########
dims <- 2
# lapse <- 0.09
# cv <- 0.87
n_mice <- 100
n_choices <- 100


# sim_conds <- tibble(experiment = 1, vol = 0.2, vol2 = 1,
#                      prob = 1, prob2 = c(0.8, 0.66, 0.5, 0.33, 0.2, 0.17, 0.14, 0.11, 0.09),
#                      cond = prob2)

PF_conds <- tibble(experiment = 1, vol = 20, vol2 = 20,
                   prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5), prob2 = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.6, 0.7, 0.8, 0.9)) %>%
  rowwise() %>%
  mutate(cond = get_rint(c(prob, prob2)))

model_list <- list("choose_val", "choose_hurdle", "choose_SUT",
                   "choose_WTA", "choose_prob_first", "choose_lxgr")

set.seed(42)
sims_PF <- map2_df(model_list,
                       as.list(1:length(model_list)),
                       ~modeller(PF_conds, n_mice = n_mice, n_choices = n_choices,
                                 choice_fun = .x, input_vec = c(vol, vol2, prob, prob2),
                                 model_num = .y, dims = dims, gamma = gamma, lapse = lapse))


sims_PF %>%
  ggplot(aes(cond, performance, color = as.factor(model))) +
  stat_summary(fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95)) + geom_smooth(method = loess)

PF_conds %>%
  select(prob, prob2, cond) %>%
  right_join(sims_PF) %>%
  filter(prob %in% c(0.2, 0.5), prob2 %in% c(0.5, 0.8)) %>%
  ggplot(aes(cond, performance, color = as.factor(model))) +
  stat_summary(fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95)) + geom_smooth(method = lm)

tibble(prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), vol = 20,
       ev = prob * vol,
       mmstreak = c(9.65, 4.95, 3.4, 2.45, 1.95, 1.7, 1.45, 1.25, 1.1),
       eff = vol/mmstreak) %>%
  ggplot(aes(ev, eff)) + geom_point() + geom_smooth(method = lm)



tibble(n = 1:1000) %>%
  rowwise() %>%
  mutate(norm = rnorm(1, mean = 20, sd = gamma*20),
         norm2 = 0.5*norm,
         norm3 = 0.2*norm,
         norm4 = 0.8*norm) %>%
  ggplot() +
  # geom_density(aes(norm), fill = "green", alpha = 0.2) +
  geom_density(aes(norm2), fill = "blue", alpha = 0.2) +
  geom_density(aes(norm3), fill = "orange", alpha = 0.2) +
  geom_density(aes(norm4), fill = "violet", alpha = 0.2)



exp1 %>%
  filter(cond == "I") %>%
  mutate(cage = factor(ifelse(cohort == 2, 2, 1))) %>%
  ggplot() + geom_point(aes(x = 1, performance, color = cage)) +
  stat_summary(aes(x = 1, performance, color = cage), fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - 2*(alpha_1)), size = 1.2, alpha = 0.4) +
  geom_hline(yintercept = 0.6, linetype = 2) +
  geom_hline(yintercept = 0.4, linetype = 2) +
  theme_bw() + scale_color_viridis_d()





#######SUT2

# lapse <- 0.32
# cv <- 0.46
n_mice <- 100
n_choices <- 100
dims <- 4

SUT2_conds <- tibble(experiment = 1,
                     cond = c("varprobvsfixed", "varvolvsfixed", "varvolvsvarprob"),
                     # cond = c(0, 1, 0.2, 0.5, 0.8),
                     max = c(12, 12, 20),
                     max2 = 20,
                     probm = c(0.5, 0.5, 0.3),
                     probm2 = c(1/3, 0.3, 1/3),
                     sec = c(12, 12, 20),
                     sec2 = c(16, 20, 16),
                     probs = c(0.5, 0.5, 0.3),
                     probs2 = c(1/3, 0.3, 1/3)) %>%
  mutate(return = max * probm + sec * probs,
         return2 = max2 * probm2 + sec2 * probs2)

model_list <- list("choose_expected", "choose_hurdle_n", "choose_SUT_n", "choose_randdim",
                   "choose_WTA", "choose_prob_first", "choose_priority")

set.seed(42)
sims_SUT2 <- map2_df(model_list,
                   as.list(1:length(model_list)),
                   ~modeller(SUT2_conds, n_mice = n_mice, n_choices = n_choices,
                             choice_fun = .x, input_vec = c(max, max2, probm, probm2, sec, sec2, probs, probs2),
                             model_num = .y, dims = dims, gamma = gamma, lapse = lapse))


sims_SUT2 %>%
  filter(model < 6) %>%
  ggplot(aes(cond, performance, color = as.factor(model))) +
  stat_summary(fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95)) +
  geom_hline(yintercept = 0.55, linetype = 2) +
  geom_hline(yintercept = 0.45, linetype = 2)



############# gamma fits

observed <- summaries %>%
  filter(str_detect(cond, "BP"), experiment %in% c(1, 4)) %>%
  # ungroup() %>%
  group_by(cond) %>%
  summarise(obs_perf = round(mean(performance), 2))

obs_pref <- observed %>%
  pull(obs_perf) %>%
  mean() %>%
  round(1)

gammas <- seq(0.05, 2, by = 0.05)
p_vols <- seq(0, 1, by = 0.05)

########## fits for sev, 2scal, wta
set.seed(15)
gen_pars <- cross_df(list(cond = c("BPV1", "BPV2"),
                        gamma = gammas)) %>%
  rowwise() %>%
  mutate(args = map(gamma, ~list(gamma = .x, lapse = 0)))

bl_gen_sims <- mdre %>%
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

summ_gen_sims <- bl_gen_sims %>%
  group_by(gamma, cond) %>%
  summarise(choice_wta = mean(choice_wta == 2),
            choice_2scal = mean(choice_2scal == 2),
            choice_sev = mean(choice_sev == 2)) %>%
  group_by(cond) %>%
  nest() %>%
  mutate(pred_wta = map(data, ~predict(loess(choice_wta ~ gamma, .))),
         pred_2scal = map(data, ~predict(loess(choice_2scal ~ gamma, .))),
         pred_sev = map(data, ~predict(loess(choice_sev ~ gamma, .)))) %>%
  unnest(c(data, pred_wta, pred_2scal, pred_sev)) %>%
  left_join(observed) %>%
  mutate(deviance_wta = (pred_wta - obs_pref)^2,
         deviance_2scal = (pred_2scal - obs_pref)^2,
         deviance_sev = (pred_sev - obs_pref)^2,)

summ_gen_sims <- summ_gen_sims %>%
  select(-starts_with("pred"), -obs_perf) %>%
  pivot_longer(cols = -(1:2), names_to = c("type", "model"), values_to = "value",
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  arrange(model, gamma, cond)

write.table(summ_gen_sims, file = paste0(getwd(),"/analysis/data/sensitivity_gen.csv"),
            dec = ".", sep = ";")
######### fit for pfirst
set.seed(33)
lxgr_pars <- cross_df(list(cond = c("BPV1", "BPV2"),
                         gamma = gammas, par = seq(0.05, 1, by = 0.05))) %>%
  rowwise() %>%
  mutate(args = map2(gamma, par, ~list(gamma = .x, lapse = 0, p_threshold = .y, v_threshold = .y)))

bl_lxgr_sims <- mdre %>%
  filter(str_detect(cond, "BP"), experiment == 1) %>%
  left_join(lxgr_pars) %>%
  exp_tbl(n_pokes = 10)

p_threshold <- 0.8

bl_pfirst_sims <- bl_lxgr_sims %>%
  rowwise() %>%
  mutate(choice = choose_SUT(list(option_1, option_2),
                                u_fun = "u_sal_pfirst",
                                args = args))


summ_pfirst_sims <- bl_pfirst_sims %>%
  group_by(gamma, par, cond) %>%
  summarise(choice = mean(choice == 2)) %>%
  group_by(cond, par) %>%
  nest() %>%
  mutate(pred_perf = map(data, ~predict(loess(choice ~ gamma, .)))) %>%
  unnest(c(data, pred_perf)) %>%
  left_join(observed) %>%
  mutate(deviance = (pred_perf - obs_pref)^2)

write.table(summ_pfirst_sims, file = paste0(getwd(),"/analysis/data/sensitivity_pfirst.csv"),
            dec = ".", sep = ";")

############ fit for vfirst
set.seed(101)
bl_vfirst_sims <- bl_lxgr_sims %>%
  rowwise() %>%
  mutate(choice = choose_SUT(list(option_1, option_2),
                                u_fun = "u_sal_vfirst",
                                args = args))

summ_vfirst_sims <- bl_vfirst_sims %>%
  group_by(gamma, par, cond) %>%
  summarise(choice = mean(choice == 2)) %>%
  group_by(cond, par) %>%
  nest() %>%
  mutate(pred_perf = map(data, ~predict(loess(choice ~ gamma, .)))) %>%
  unnest(c(data, pred_perf)) %>%
  left_join(observed) %>%
  mutate(deviance = (pred_perf - obs_pref)^2)

write.table(summ_vfirst_sims, file = paste0(getwd(),"/analysis/data/sensitivity_vfirst.csv"),
            dec = ".", sep = ";")
######### fit for nonc
set.seed(92)
rnonc_pars <- cross_df(list(cond = c("BPV1", "BPV2"),
                         gamma = gammas, par = p_vols)) %>%
  rowwise() %>%
  mutate(args = map2(gamma, par, ~list(.x, 0, .y)))

bl_rnonc_sims <- mdre %>%
  filter(str_detect(cond, "BP"), experiment == 1) %>%
  left_join(rnonc_pars) %>%
  exp_tbl(n_pokes = 10)

bl_rnonc_sims <- bl_rnonc_sims %>%
  rowwise() %>%
  mutate(choice = choose_SUT(list(option_1, option_2),
                                u_fun = "u_rnonc",
                                args = args))

summ_rnonc_sims <- bl_rnonc_sims %>%
  group_by(gamma, par, cond) %>%
  summarise(choice = mean(choice == 2)) %>%
  group_by(par, cond) %>%
  nest() %>%
  mutate(pred_perf = map(data, ~predict(loess(choice ~ gamma, .)))) %>%
  unnest(c(data, pred_perf)) %>%
  left_join(observed) %>%
  mutate(deviance = (pred_perf - obs_pref)^2)

# scales::show_col(viridis::viridis_pal()(7))
write.table(summ_rnonc_sims, file = paste0(getwd(),"/analysis/data/sensitivity_rnonc.csv"),
            dec = ".", sep = ";")
