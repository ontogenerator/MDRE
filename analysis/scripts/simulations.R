library(tidyverse)

dim_splitter <- function(x, dims) {
  if (length(x) %% dims != 0) stop("Means have to be a multiple of Dimensions (dims)")

  if (dims > 1) return(unname(split(x, ceiling(seq_along(x)/(length(x)/dims)))))
  x
}

sampler <- function(probs, n_sims = 1) {
  sample(length(probs), size = n_sims, prob = probs, replace = TRUE)
}


Which_hurdle <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  res <- map_dbl(transpose(vals), ~prod(unlist(.)))
  return(which(res == max(res)) - 1)

}

Which_val <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {
  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- dim_splitter(means, dims)

  vals <- map_dbl(transpose(vals), ~prod(unlist(.)))
  res <- rnorm(vals, mean = vals, sd = cv * abs(vals))
  return(which(res == max(res)) - 1)

}

# p <- 0.1
# cv <- 0.7
# dims <- 2
#
# means <- c(0.5, 0.7, 1, 1)
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

Which_randdim <- function(means, dims = 2, cv = 0.7, lapse = 0.1, weightp = rep(1, dims)){

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)

  res <- vals[[sampler(weightp)]]

  return(which(res == max(res)) - 1)
}

Which_WTA <- function(means, dims = 2, cv = 0.7, lapse = 0.1) {

  p <- runif(1, 0, 1)
  n_options <- length(means)/dims

  if (p < lapse) return(sample(1:n_options, 1) - 1)

  vals <- rnorm(means, mean = means, sd = cv * abs(means))
  vals <- dim_splitter(vals, dims)
  salience <- map_dbl(vals, ~length(means)/dims*(max(.) - min(.))/sum(.))

  if (sd(salience) == 0) return(Which_randdim(means, dims, cv))

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
  salience <- map_dbl(vals, ~length(means)/dims*(max(.) - min(.))/sum(.))

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
cv <- 0.92
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

set.seed(42)
simulations <- map2_df(model_list,
                  as.list(1:length(model_list)),
                  ~modeller(sim_conds, n_mice = n_mice, n_choices = n_choices,
                            choice_fun = .x, input_vec = c(vol, vol2, prob, prob2),
                            model_num = .y, dims = dims, cv = cv, lapse = lapse))

write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
           dec = ".", sep = ";")


Which_priority <-
  function(means, dims = 3, cv = cv, lapse = lapse, threshold = 0.1) {

    p <- runif(1, 0, 1)
    n_options <- length(means)/dims

    if (p < lapse) return(sample(1:n_options, 1) - 1)

    vals <- rnorm(means, mean = means, sd = cv * abs(means))
    vals <- dim_splitter(vals, dims)
    # salience <- map_dbl(vals, ~length(means)/dims*(max(.) - min(.))/sum(.))

    # if (reverse) {
    #   # salience <- rev(salience)
    #   vals <- rev(vals)
    # }

    while (length(vals) > 0) {

      if (salience[[1]] > threshold) {
        res <- vals[[1]]
        return(which(res == max(res)) - 1)

      } else {
        salience <- salience[-1]
        vals <- vals[-1]
      }
    }
    if (length(vals) == 0) return(Which_randdim(means, dims, cv))
}

lapse <- 0.09
cv <- 0.7
n_mice <- 100
n_choices <- 100
dims <- 3


SUT_conds <- tibble(experiment = 1,
                    names = c("BC", "AB", "B_var0.2", "B_var0.5", "B_var0.8"),
                    cond = c(0, 1, 0.2, 0.5, 0.8),
                    min = c(5, 20, 5, 5, 5),
                    min2 = 12.5,
                    prob = c(1, 1, 0.2, 0.5, 0.8),
                    prob2 = 1,
                    vol = c(5, 20, 20, 20, 20),
                    vol2 = 12.5) %>%
  mutate(return = min * prob,
         return2 = min2 * prob2)
# sim_conds <- conds_tab %>%
#   filter(cond == "BVP1") %>%
#   mutate(experiment = 2) %>%
#   bind_rows(conds_tab) %>%
#   arrange(experiment, cond)

SUT_model_list <- list("Which_randdim", "Which_WTA", "Which_prob_first", "Which_lxgr")

set.seed(42)
simulations <- map2_df(SUT_model_list,
                       as.list(1:length(SUT_model_list)),
                       ~modeller(SUT_conds, n_mice = n_mice, n_choices = n_choices,
                                 choice_fun = .x,
                                 input_vec = c(min, min2, prob, prob2, vol, vol2),
                                 model_num = .y, dims = dims, cv = cv, lapse = lapse))

# write.table(simulations, file = paste0(getwd(),"/analysis/data/simulations.csv"),
#             dec = ".", sep = ";")




simulations %>%
  ggplot() +
  geom_density(aes(performance), size = 1.2) + facet_grid(model ~ cond)


simulations %>%
  ggplot() +
  stat_summary(aes(cond, performance),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95), size = 0.8) +
  stat_summary(data = prediction, aes(cond, cert),
               fun.data = mean_cl_boot,
               fun.args = list(conf.int = 0.95), size = 0.8, color = "darkgreen") +
  facet_grid(model ~ .) +
  geom_abline(
    intercept = coef(lm(cert ~ cond, Perfpl %>% filter(cond %in% c(1,0))))[1],
    slope = coef(lm(cert ~ cond, Perfpl %>% filter(cond %in% c(1,0))))[2]
  )

# summ_simple_sims <- simulations %>%
#   group_by(experiment, cond, model) %>%
#   summarise(medperf = median(performance))
#
# summ_simple_sims <- summ_simple_sims %>%
#   filter(experiment == 1) %>%
#   ungroup() %>%
#   mutate(experiment = 4) %>%
#   bind_rows(summ_simple_sims) %>%
#   arrange(experiment, cond, model)
#
# devs <- summ_simple_sims %>%
#   full_join(emp_perf) %>%
#   mutate(deviance = (medperf - performance)^2)
#
# rmsds <- devs %>%
#   filter(!str_detect(cond, "BP")) %>%
#   group_by(experiment, model) %>%
#   summarise(RMSD = sqrt(mean(deviance, na.rm = TRUE))) %>%
#   arrange(experiment, RMSD)
#
# ranks <- rmsds %>%
#   mutate(rank = rank(RMSD)) %>%
#   ungroup() %>%
#   select(-RMSD) %>%
#   spread(experiment, model)
#
#
# rmsds_nocoh2 <- devs %>%
#   filter(!str_detect(cond, "BP"), cohort != 2) %>%
#   group_by(experiment, model) %>%
#   summarise(RMSD = sqrt(mean(deviance, na.rm = TRUE))) %>%
#   arrange(experiment, RMSD)
#
# ranks_nocoh2 <- rmsds_nocoh2 %>%
#   mutate(rank = rank(RMSD)) %>%
#   ungroup() %>%
#   select(-RMSD) %>%
#   spread(experiment, model)
#
#
#
# exp1 <- exp1 %>%
#   group_by(cond) %>%
#   mutate(medperf = median(performance))
#
# ggplot() +
#   geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 1)) +
#   facet_grid(model ~ cond) +
#   geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp1) +
#   theme_bw() + scale_fill_viridis_d() +
#   scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
#   geom_vline(aes(xintercept = medperf), linetype = 2, data = exp1) +
#   geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
#                filter(experiment == 1)) +
#   labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))
#
# exp2 <- exp2 %>%
#   group_by(cond) %>%
#   mutate(medperf = median(performance))
#
# ggplot() +
#   geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 2)) +
#   facet_grid(model ~ cond) +
#   geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp2) +
#   theme_bw() + scale_fill_viridis_d() +
#   scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
#   geom_vline(aes(xintercept = medperf), linetype = 2, data = exp2) +
#   geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
#                filter(experiment == 2)) +
#   labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))
#
#
# exp3 <- exp3 %>%
#   group_by(cond) %>%
#   mutate(medperf = median(performance))
#
# ggplot() +
#   geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 3)) +
#   facet_grid(model ~ cond) +
#   geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp3) +
#   theme_bw() + scale_fill_viridis_d() +
#   scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
#   geom_vline(aes(xintercept = medperf), linetype = 2, data = exp3) +
#   geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
#                filter(experiment == 3)) +
#   labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))
#
#
# exp4 <- exp4 %>%
#   group_by(cond) %>%
#   mutate(medperf = median(performance))
#
# ggplot() +
#   geom_density(aes(performance), size = 1.2, data = models %>% filter(experiment == 1)) +
#   facet_grid(model ~ cond) +
#   geom_density(aes(performance, fill = cohort), alpha = 0.2, data = exp4) +
#   theme_bw() + scale_fill_viridis_d() +
#   scale_x_continuous(name = "discrimination performance", breaks = c(0, 0.5, 1)) +
#   geom_vline(aes(xintercept = medperf), linetype = 2, data = exp4) +
#   geom_vline(aes(xintercept = medperf), linetype = 3, size = 1.2, data = summ_simple_sims %>%
#                filter(experiment == 1)) +
#   labs(fill = "cohort") + guides(color = FALSE) + theme(strip.text.y = element_text(angle = 0))
#
