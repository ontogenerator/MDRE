library(tidyverse)

dim_splitter <- function(x, dims) {
  if (length(x) %% dims != 0) stop("Means have to be a multiple of Dimensions (dims)")

  if (dims > 1) return(unname(split(x, ceiling(seq_along(x)/(length(x)/dims)))))
  x
}

sampler <- function(probs, n_sims = 1) {
  sample(length(probs), size = n_sims, prob = probs, replace = TRUE)
}

rint <- function(vec) { # generalized relative intensity function for n inputs
  length(vec) * (max(vec) - min(vec)) / sum(vec)
}

Which_hurdle <- function(means, dims = 2, cv, lapse) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)
  cv <- cv*0.75 - 0.1*cv

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  res <- map_dbl(transpose(vals), ~prod(unlist(.)))
  return(which(res == max(res)) - 1)

}

Which_val <- function(means, dims = 2, cv, lapse) {
  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- dim_splitter(means, dims)

  vals <- map_dbl(transpose(vals), ~prod(unlist(.)))
  res <- rnorm(vals, mean = vals, sd = cv * abs(vals))
  return(which(res == max(res)) - 1)

}


Which_SUT <- function(means, dims = 2, cv, lapse) {
  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- dim_splitter(means, dims)

  vals[[1]] <- rnorm(vals[[1]], mean = vals[[1]], sd = cv * abs(vals[[1]]))

  res <- map_dbl(transpose(vals), ~prod(unlist(.)))
  # res <- rnorm(vals, mean = vals, sd = cv * abs(vals))
  return(which(res == max(res)) - 1)

}


# p <- 0.1
# cv <- 0.7
# dims <- 2
#
# means <- c(20, 20, 0.5, 0.7)
#
# tibble(n = 1:1000) %>%
#   rowwise() %>%
#   mutate(choice = Which_hurdle(means)) %>%
#   ungroup() %>%
#   summarise(performance = mean(choice))

# Which_log <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {
#
#   p <- runif(1, 0, 1)
#   n_options <- length(means)/dims
#
#   if (p < lapse) return(sample(1:n_options, 1) - 1)
#
#   # vals <- rnorm(means, mean = means, sd = cv * abs(means))
#   vals <- dim_splitter(means, dims)
#
#   product <- map_dbl(transpose(vals), ~prod(unlist(.)))
#   sum <- map_dbl(transpose(vals), ~sum(unlist(.)))
#   vals <- log(product + 1)
#   res <- rnorm(vals, mean = vals, sd = cv * abs(vals))
#
#   return(which(res == max(res)) - 1)
# }

Which_randdim <- function(means, dims = 2, cv, lapse, weightp = rep(1, dims)) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  res <- vals[[sampler(weightp)]]

  if (length(which(res == max(res))) > 1) {
    return(sample(1:n_options, 1) - 1)
  } else {
    return(which(res == max(res)) - 1)
  }
}

Which_WTA <- function(means, dims = 2, cv, lapse) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)
  salience <- map_dbl(vals, rint)

  salience <- ifelse(is.nan(salience), 0, salience)
  if (sd(salience) == 0) return(Which_randdim(means, dims = dims, cv = cv, lapse = lapse))

  dim_salient <- which(salience == max(salience))
  res <- vals[[dim_salient]]
  return(which(res == max(res)) - 1)
}


Which_lxgr <- function(means, dims = 2, cv = cv, lapse = lapse, threshold = 0.8, reverse = FALSE) {
  # lexicographic rule with one dimension checked first,
  # then if it is not informative, check the other, the dimensions should be listed
  # in their lexicographic order

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)
  salience <- map_dbl(vals, rint)

  if (reverse) {
    salience <- rev(salience)
    vals <- rev(vals)
  }

  while (length(salience) > 0) {

    if (salience[[1]] > threshold) {
      res <- vals[[1]]
      return(which(res == max(res)) - 1)

    } else {
      salience <- salience[-1]
      vals <- vals[-1]
    }
  }
  if (length(salience) == 0) return(Which_randdim(means, dims = dims, cv = cv, lapse = lapse))
}

Which_prob_first <- partial(Which_lxgr, reverse = TRUE)

modeller <- function(input_tb, n_mice, n_choices, choice_fun,
                     input_vec,  model_num, dims, cv, lapse) {

  input_expr <- enquos(input_vec)

  input_tb %>%
    rowwise() %>%
    mutate(mod = list(map(1:(n_mice*n_choices),
                          ~ rlang::exec(choice_fun, !!!input_expr, dims = dims, cv = cv, lapse = lapse)))) %>%
    unnest(mod) %>%
    mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(input_tb)),
           model = model_num) %>%
    group_by(id, experiment, cond, model) %>%
    summarise(performance = mean(as.numeric(mod)))
}

dims <- 2
lapse <- 0.09
cv <- 0.87
n_mice <- 100
n_choices <- 100

sim_conds <- conds_tab %>%
  filter(cond == "BVP1") %>%
  mutate(experiment = 2) %>%
  bind_rows(conds_tab) %>%
  arrange(experiment, cond)

# sim_conds <- tibble(experiment = 1, vol = 0.2, vol2 = 1,
#                     prob = 1, prob2 = c(0.8, 0.66, 0.5, 0.33, 0.2, 0.17, 0.14, 0.11, 0.09),
#                     cond = prob2)

model_list <- list("Which_val", "Which_hurdle", "Which_randdim",
                   "Which_WTA", "Which_prob_first", "Which_lxgr")

# model_list <- list("Which_val", "Which_hurdle", "Which_SUT",
#                    "Which_WTA", "Which_prob_first", "Which_lxgr")

set.seed(42)
simulations <- map2_df(model_list,
                  as.list(1:length(model_list)),
                  ~modeller(sim_conds, n_mice = n_mice, n_choices = n_choices,
                            choice_fun = .x, input_vec = c(vol, vol2, prob, prob2),
                            model_num = .y, dims = dims, cv = cv, lapse = lapse))

write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
           dec = ".", sep = ";")

# write.table(simulations, file = "C:/Users/Vladi/Documents/MDRE/analysis/data/simulations2.csv",
#            dec = ".", sep = ";")

Which_priority <-
  function(means, dims = 3, cv = cv, lapse = lapse, threshold = 0.6) {

    p <- runif(1, 0, 1)
    n_options <- length(means)/dims

    if (p < lapse) return(sample(1:n_options, 1) - 1)

    vals <- rnorm(means, mean = means, sd = cv * abs(means))
    vals <- dim_splitter(vals, dims)
    salience <- map_dbl(vals, rint)

    while (length(salience) > 0) {

      if (salience[[1]] > threshold) {
        res <- vals[[1]]
        return(which(res == max(res)) - 1)

      } else {
        salience <- salience[-1]
        vals <- vals[-1]
      }
    }
    if (length(salience) == 0) return(Which_randdim(means, dims, cv = cv, lapse = lapse))
}



Which_expected <- function(means, dims = 4, cv = cv, lapse = lapse) {
  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- dim_splitter(means, dims)
  vals <- map_dbl(transpose(vals), ~(unlist(.[1]) * unlist(.[2]) +
                                       unlist(.[3]) * unlist(.[4])))
  res <- rnorm(vals, mean = vals, sd = cv * abs(vals))
  return(which(res == max(res)) - 1)
}

Which_hurdle_n <- function(means, dims = 4, cv = cv, lapse = lapse) {
  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)
  cv <- cv*0.75 - 0.1*cv

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  res <- map_dbl(transpose(vals), ~(unlist(.[1]) * unlist(.[2]) +
                                      unlist(.[3]) * unlist(.[4])))
  return(which(res == max(res)) - 1)
}


Which_SUT_n <- function(means, dims = 4, cv = cv, lapse = lapse) {
  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)
  # cv <- cv*0.75 - 0.1*cv
  vals <- dim_splitter(means, dims)
  vals[[1]] <- rnorm(vals[[1]], mean = vals[[1]], sd = cv * abs(vals[[1]]))
  vals[[3]] <- rnorm(vals[[3]], mean = vals[[3]], sd = cv * abs(vals[[3]]))
  # vals <- rnorm(means, mean = means, sd = cv * abs(means))

  res <- map_dbl(transpose(vals), ~(unlist(.[1]) * unlist(.[2]) +
                                      unlist(.[3]) * unlist(.[4])))
  return(which(res == max(res)) - 1)

}


lapse <- 0.32
cv <- 0.46
n_mice <- 100
n_choices <- 100
dims <- 4

# means <- c(c(20, 12.5, 0.8, 0.5, 5, 12.5, 0.2, 0.5))
# means <- c(c(5, 12.5, 1, 1, 5, 12.5, 1, 1))

# Which_priority(means, dims = dims, cv = cv, lapse = lapse, threshold = 0.1)

# tibble(n = 1:1000) %>%
#   rowwise() %>%
#   mutate(choice = Which_hurdle_n(means, dims = dims, cv = cv, lapse = lapse)) %>%
#   ungroup() %>%
#   summarise(performance = mean(choice))

SUT_conds <- tibble(experiment = 1,
                    names = c("BC", "AB", "B_var0.2", "B_var0.5", "B_var0.8"),
                    cond = c(0, 1, 0.2, 0.5, 0.8),
                    max = c(5, 20, 20, 20, 20),
                    max2 = 12.5,
                    probm = c(0.5, 0.5, 0.2, 0.5, 0.8),
                    probm2 = 0.5,
                    sec = c(5, 20, 5, 5, 5),
                    sec2 = 12.5,
                    probs = c(0.5, 0.5, 0.8, 0.5, 0.2),
                    probs2 = 0.5) %>%
  mutate(return = max * probm + sec * probs,
         return2 = max2 * probm2 + sec2 * probs2)

SUT_model_list <- list("Which_expected", "Which_hurdle_n", "Which_SUT_n", "Which_randdim",
                       "Which_WTA", "Which_prob_first", "Which_priority")

set.seed(42)
simulations <- map2_df(SUT_model_list,
                       as.list(1:length(SUT_model_list)),
                       ~modeller(SUT_conds, n_mice = n_mice, n_choices = n_choices,
                                 choice_fun = .x,
                                 input_vec = c(max, max2, probm, probm2, sec, sec2, probs, probs2),
                                 model_num = .y, dims = dims, cv = cv, lapse = lapse))

# write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
#             dec = ".", sep = ";")

# simulations %>%
#   ggplot() +
#   geom_density(aes(performance), size = 1.2) + facet_grid(model ~ cond)
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
               fun.args = list(conf.int = 1 - alpha_n), size = 0.3, color = "darkred") +
  geom_line(data = pred_lines, aes(cond, performance)) +
  geom_line(data = pred_lines, aes(cond, min_perf), linetype = 2) +
  geom_line(data = pred_lines, aes(cond, max_perf), linetype = 2)

m1_pred <- pred_lines %>% filter(model == 3)

simulations %>%
  ggplot(aes(color = as.factor(model))) +
  stat_summary(aes(cond, performance),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - alpha_n), size = 0.5) +
  geom_line(data = m1_pred, aes(cond, performance)) +
  geom_line(data = m1_pred, aes(cond, min_perf), linetype = 2) +
  geom_line(data = m1_pred, aes(cond, max_perf), linetype = 2) +
  stat_summary(data = pred, aes(cond, cert),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - alpha_n), size = 0.3, color = "darkred")



################
################ CHAG experiment

lapse <- 0.32
cv <- 0.46
n_mice <- 100
n_choices <- 100
dims <- 4


# means <- c(20, 8, 0.25, 0.5, 5, 0, 0.25, 0)

CHAG_conds <- tibble(experiment = c(1, 1, 1, 1, 2, 2, 2, 2),
                    name = c("H", "G", "A", "C", "H/2", "G/2", "A/2", "C/2"),
                    cond = c(8, 10, 12.5, 17.5, 8, 10, 12.5, 17.5),
                    max = 20,
                    max2 = c(8, 10, 12.5, 17.5, 8, 10, 12.5, 17.5),
                    probm = c(0.5, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25),
                    probm2 = 0.5,
                    sec = 5,
                    sec2 = c(8, 10, 12.5, 17.5, 0, 0, 0, 0),
                    probs = c(0.5, 0.5, 0.5, 0.5, 0.25, 0.25, 0.25, 0.25),
                    probs2 = 0.5) %>%
  rowwise() %>%
  mutate(return = max * probm + sec * probs,
         return2 = max2 * probm2 + sec2 * probs2,
         rintmax = rint(c(max, max2)),
         rintprobm = rint(c(probm, probm2)),
         rintsec = rint(c(sec, sec2)),
         rintprobs = rint(c(probs, probs2)))

CHAG_model_list <- list("Which_expected", "Which_hurdle_n", "Which_SUT_n", "Which_randdim",
                       "Which_WTA", "Which_prob_first", "Which_priority")

set.seed(42)
CHAG_sims <- map2_df(CHAG_model_list,
                       as.list(1:length(SUT_model_list)),
                       ~modeller(CHAG_conds, n_mice = n_mice, n_choices = n_choices,
                                 choice_fun = .x,
                                 input_vec = c(max, max2, probm, probm2, sec, sec2, probs, probs2),
                                 model_num = .y, dims = dims, cv = cv, lapse = lapse))

# write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
#             dec = ".", sep = ";")

CHAG_sims %>%
  mutate(prob = ifelse(experiment == 1, 1, 0.5)) %>%
  ggplot() +
  stat_summary(aes(cond, performance),
               fun.data = mean_cl_boot,
               fun.args = list(0.95), size = 0.5) +
  stat_smooth(aes(cond, performance), method = lm) +
  facet_grid(model ~ experiment) +
  stat_summary(data = Perf, aes(cert, performance),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 1 - alpha_n), size = 0.3, color = "darkred") +
  geom_hline(yintercept = 0.5, linetype = 2)

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
#
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
lapse <- 0.09
cv <- 0.87
n_mice <- 100
n_choices <- 100


# sim_conds <- tibble(experiment = 1, vol = 0.2, vol2 = 1,
#                      prob = 1, prob2 = c(0.8, 0.66, 0.5, 0.33, 0.2, 0.17, 0.14, 0.11, 0.09),
#                      cond = prob2)

PF_conds <- tibble(experiment = 1, vol = 20, vol2 = 20,
                   prob = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.5, 0.5, 0.5, 0.5), prob2 = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.6, 0.7, 0.8, 0.9)) %>%
  rowwise() %>%
  mutate(cond = rint(c(prob, prob2)))

model_list <- list("Which_val", "Which_hurdle", "Which_SUT",
                   "Which_WTA", "Which_prob_first", "Which_lxgr")

set.seed(42)
sims_PF <- map2_df(model_list,
                       as.list(1:length(model_list)),
                       ~modeller(PF_conds, n_mice = n_mice, n_choices = n_choices,
                                 choice_fun = .x, input_vec = c(vol, vol2, prob, prob2),
                                 model_num = .y, dims = dims, cv = cv, lapse = lapse))


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
  mutate(norm = rnorm(1, mean = 20, sd = cv*20),
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

