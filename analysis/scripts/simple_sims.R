library(tidyverse)
library(purrr)


# bind_rows(cvs, cvs_vol) %>%
#   filter(cohort != 2) %>%
#   ungroup() %>%
#   summarise(cv = mean(cv))


dim_splitter <- function(x, dims) {
  if (length(x) %% dims != 0) stop("Means have to be a multiple of Dimensions (dims)")

  if (dims > 1) return(unname(split(x, ceiling(seq_along(x)/(length(x)/dims)))))
  x
}
# map(vals, magrittr::extract, 1:n_options)

lapse <- 0.1

Which_hurdle <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  res <- unlist(map(transpose(vals), ~prod(unlist(.))))
  return(which(res == max(res)) - 1)

}

# vols <- c(10, 20, 50)
# probs <- c(0.6, 0.3, 0.05)

Which_val <- function(vols, probs, cv = 0.7, lapse = 0.1) {

  p <- runif(1, 0, 1)
  n_options <- length(vols)

  if (p < lapse) return(sample(1:n_options, 1) - 1)
  vals <- vols * probs
  res <- rnorm(vals, mean = vals, sd = cv * abs(vals))

  return(which(res == max(res)) - 1)

}

# p <- 0.1
# cv <- 0.7
# means <- c(0.2, 0.5, 0.1, 20, 10, 80)
# dims <- 2
Which_log <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  # vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(means, dims)

  product <- unlist(map(transpose(vals), ~prod(unlist(.))))
  sum <- unlist(map(transpose(vals), ~sum(unlist(.))))
  vals <- log(product + 1)
  res <- rnorm(vals, mean = vals, sd = cv * abs(vals))

  return(which(res == max(res)) - 1)
}

Which_randdim <- function(means, dims = 2, cv = 0.7, lapse = 0.1, weightp = 0.5){

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  if (weightp == 1/(length(means)/dims)) {
    res <- vals[[sample(1:dims, 1)]]
  } else {
    p <- runif(1, 0, 1)
    if (p < weightp) {
      res <- vals[[1]]
    } else {
      res <- vals[[2]]
    }
  }
  return(which(res == max(res)) - 1)

}

# a <- c(2,3,4,6,7,8,0.1,0.2,0.4) #3d 3o
# b <- c(2,3,0.1,0.2) #2d 2o
# c <- c(1,0.2) #2d 1o
# d <- c(15,32,180,90,0,0.3) #3d 2o
# e <- c(0.2,0.5) #1d 2o
#
# dims <- 3
#
# length(d)/3
# ceiling(seq_along(a)/2)
#
Which_WTA <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)
  salience <- map(vals, ~length(means)/dims*(max(.) - min(.))/sum(.)) %>%
    unlist()

  if (sd(salience) == 0) return(Which_randdim(means, dims, cv))

  dim_salient <- which(salience == max(salience))
  res <- vals[[dim_salient]]
  return(which(res == max(res)) - 1)
}


Which_lxgr <- function(means, dims = 2, cv = 0.7, lapse = 0.1, threshold = 0.7, reverse = FALSE) {
  # lexicographic rule with one dimension checked first,
  # then if it is not informative, check the other, the dimensions should be listed
  # in their lexicographic order

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)
  salience <- map(vals, ~length(means)/dims*(max(.) - min(.))/sum(.)) %>%
    unlist()

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
  if (length(salience) == 0) return(Which_randdim(means, dims, cv))
}


means <- c(.8, .5, .2, 2, 13, 25)
Which_hurdle(means, cv = 0)

n_mice <- 100
n_choices <- 100
#### model 1

lapse <- 0.1
cv <- 0.7

sim_conds <- conds_tab %>%
  filter(cond == "BVP1") %>%
  mutate(experiment = 2) %>%
  bind_rows(conds_tab) %>%
  arrange(experiment, cond)

model1 <- sim_conds %>%
  rowwise() %>%
  mutate(model1 = list(rerun(n_mice*n_choices, Which_hurdle(c(vol, vol2, prob, prob2), cv = cv, lapse = lapse)))) %>%
  unnest(model1) %>%
  mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
         model = 1) %>%
  group_by(id, experiment, cond, model) %>%
  summarise(performance = mean(as.numeric(model1)))

model2 <- sim_conds %>%
  rowwise() %>%
  mutate(model2 = list(rerun(n_mice*n_choices, Which_randdim(c(vol, vol2, prob, prob2), cv = cv, lapse = lapse)))) %>%
  unnest(model2) %>%
  mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
         model = 2) %>%
  group_by(id, experiment, cond, model) %>%
  summarise(performance = mean(as.numeric(model2)))


model3 <- sim_conds %>%
  rowwise() %>%
  mutate(model3 = list(rerun(n_mice*n_choices, Which_WTA(c(vol, vol2, prob, prob2), cv = cv, lapse = lapse)))) %>%
  unnest(model3) %>%
  mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
         model = 3) %>%
  group_by(id, experiment, cond, model) %>%
  summarise(performance = mean(as.numeric(model3)))

# Which_lxgr(c(0.2, 0.5, 20, 4), cv = cv, lapse = lapse)

model4 <- sim_conds %>%
  rowwise() %>%
  mutate(model4 = list(rerun(n_mice*n_choices, Which_lxgr(c(vol, vol2, prob, prob2), cv = cv, lapse = lapse, reverse = TRUE)))) %>%
  unnest(model4) %>%
  mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
         model = 4) %>%
  group_by(id, experiment, cond, model) %>%
  summarise(performance = mean(as.numeric(model4)))

model5 <- sim_conds %>%
  rowwise() %>%
  mutate(model5 = list(rerun(n_mice*n_choices, Which_lxgr(c(vol, vol2, prob, prob2), cv = cv, lapse = lapse)))) %>%
  unnest(model5) %>%
  mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
         model = 5) %>%
  group_by(id, experiment, cond, model) %>%
  summarise(performance = mean(as.numeric(model5)))


model6 <- sim_conds %>%
  rowwise() %>%
  mutate(model6 = list(rerun(n_mice*n_choices, Which_val(c(vol, vol2), c(prob, prob2), cv = cv, lapse = lapse)))) %>%
  unnest(model6) %>%
  mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
         model = 6) %>%
  group_by(id, experiment, cond, model) %>%
  summarise(performance = mean(unlist(model6)))

# model7 <- sim_conds %>%
#   rowwise() %>%
#   mutate(model7 = list(rerun(n_mice*n_choices, Which_log(c(vol, vol2, prob, prob2), cv = cv, lapse = lapse)))) %>%
#   unnest(model7) %>%
#   mutate(id = rep(rep(1:n_mice, each = n_choices), nrow(sim_conds)),
#          model = 7) %>%
#   group_by(id, experiment, cond, model) %>%
#   summarise(performance = mean(as.numeric(model7)))



models <- bind_rows(model1, model2, model3, model4, model5, model6)

summ_simple_sims <- models %>%
  group_by(experiment, cond, model) %>%
  summarise(medperf = median(performance))

summ_simple_sims <- summ_simple_sims %>%
  filter(experiment == 1) %>%
  ungroup() %>%
  mutate(experiment = 4) %>%
  bind_rows(summ_simple_sims) %>%
  arrange(experiment, cond, model)

devs <- summ_simple_sims %>%
  full_join(emp_perf) %>%
  mutate(deviance = (medperf - performance)^2)

rmsds <- devs %>%
  filter(!str_detect(cond, "BP")) %>%
  group_by(experiment, model) %>%
  summarise(RMSD = sqrt(mean(deviance, na.rm = TRUE))) %>%
  arrange(experiment, RMSD)

ranks <- rmsds %>%
  mutate(rank = rank(RMSD)) %>%
  ungroup() %>%
  select(-RMSD) %>%
  spread(experiment, model)


rmsds_nocoh2 <- devs %>%
  filter(cohort != 2) %>%
  group_by(experiment, model) %>%
  summarise(RMSD = sqrt(mean(deviance, na.rm = TRUE))) %>%
  arrange(experiment, RMSD)

ranks_nocoh2 <- rmsds_nocoh2 %>%
  mutate(rank = rank(RMSD)) %>%
  ungroup() %>%
  select(-RMSD) %>%
  spread(experiment, model)

exp1 <- exp1 %>%
  group_by(cond) %>%
  mutate(medperf = median(performance))

ggplot() +
  geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 1)) +
  facet_grid(model ~ cond) +
  geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp1) +
  theme_bw() + scale_fill_viridis_d() +
  scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
  geom_vline(aes(xintercept = medperf), linetype = 2, data = exp1) +
  geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
               filter(experiment == 1)) +
  labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))

exp2 <- exp2 %>%
  group_by(cond) %>%
  mutate(medperf = median(performance))

ggplot() +
  geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 2)) +
  facet_grid(model ~ cond) +
  geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp2) +
  theme_bw() + scale_fill_viridis_d() +
  scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
  geom_vline(aes(xintercept = medperf), linetype = 2, data = exp2) +
  geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
               filter(experiment == 2)) +
  labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))


exp3 <- exp3 %>%
  group_by(cond) %>%
  mutate(medperf = median(performance))

ggplot() +
  geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 3)) +
  facet_grid(model ~ cond) +
  geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp3) +
  theme_bw() + scale_fill_viridis_d() +
  scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
  geom_vline(aes(xintercept = medperf), linetype = 2, data = exp3) +
  geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
               filter(experiment == 3)) +
  labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))


exp4 <- exp4 %>%
  group_by(cond) %>%
  mutate(medperf = median(performance))

ggplot() +
  geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 1)) +
  facet_grid(model ~ cond) +
  geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp4) +
  theme_bw() + scale_fill_viridis_d() +
  scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
  geom_vline(aes(xintercept = medperf), linetype = 2, data = exp4) +
  geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
               filter(experiment == 1)) +
  labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))
