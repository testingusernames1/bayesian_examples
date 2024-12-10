library(dplyr)
library(rstan)

# Read in data
modeling_df <- read.csv("C:/Users/npbig/Downloads/df.csv")
modeling_df <- modeling_df %>% arrange(date)
modeling_df <-   modeling_df %>% mutate(across(-c(event_name,course_name,player_name,date,teetime), as.numeric))
modeling_df <- modeling_df %>% subset(total_pts > 0)
modeling_df$dg_id_mapped <- as.integer(factor(modeling_df$dg_id))

# Split train and test 
train_sample <- modeling_df %>% subset(date >= '2022-01-01')

# Assuming `modeling_df` is your data frame and `dg_id` and `course` are the columns
train_sample$dg_id_mapped <- as.integer(factor(train_sample$dg_id))
train_sample$course_id_mapped <- as.integer(factor(train_sample$course_num))

model <- stan_model("C:/Users/npbig/Downloads/player_effect.stan")

# Average score by player ID
mu0_prior <- train_sample %>%
  filter(date > '2024-01-01') %>%
  group_by(dg_id_mapped) %>% 
  summarise(meanscore = mean(score))

# Fill missing mu0with flat 71
dg_id_mapped <- as.data.frame(1:length(unique(train_sample$dg_id_mapped)))
names(dg_id_mapped) <- 'dg_id_mapped'
mu0_prior_filled <- full_join(mu0_prior,dg_id_mapped)
mu0_prior_filled$meanscore[is.na(mu0_prior_filled$meanscore)] <- 71

data <- list(N = nrow(train_sample),
             G = length(unique(train_sample$dg_id_mapped)),
             y = train_sample$score,
             g = train_sample$dg_id_mapped,
             mu0 = mu0_prior_filled$meanscore)

fit <- sampling(model,data,chains=1,iter=2000)

posterior_samples <- extract(fit)
#saveRDS(posterior_samples,'C:/Users/npbig/Downloads/playerlevel_posterior_samples_tightprior.rds')
#saveRDS(fit,'C:/Users/npbig/Downloads/player_level_fit_player_tightprior.rds')

# Mapping dg_id and course_id___________________________________________________ 
summary_stat <- summary(fit)$summary

test_sample <- modeling_df %>% subset(date > '2023-01-01')
test_sample <- test_sample %>% select(-dg_id_mapped)
dgid_joiner <- train_sample %>% select(player_name,dg_id_mapped) 
dgid_joiner <- unique(dgid_joiner)

course_joiner <- train_sample %>% select(course_num,course_id_mapped)
course_joiner <- unique(course_joiner)
test_sample <- test_sample %>% left_join(dgid_joiner)

test_sample <- test_sample %>% left_join(course_joiner)

# Grab Samples from the Posterior________________________________________________________
test <- test_sample %>% filter(player_name == "Scheffler, Scottie" & date == "2024-04-17")
n_new <- 1
gnew <- test$dg_id_mapped

y_new_samples <- c()
for (i in 1:10000) {
  lambda <- mean(posterior_samples$lambda[,gnew])
  y_new_samples[i] <- rpois(1,lambda)
}

hist(y_new_samples)