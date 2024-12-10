library(dplyr)
library(rstan)

# Read in data
modeling_df <- read.csv("C:/Users/npbig/Downloads/df.csv")
modeling_df <- modeling_df %>% arrange(date)
modeling_df <-   modeling_df %>% mutate(across(-c(event_name,course_name,player_name,date,teetime), as.numeric))
modeling_df <- modeling_df %>% subset(total_pts > 0)
modeling_df$dg_id_mapped <- as.integer(factor(modeling_df$dg_id))

# Split train and test 
train_sample <- modeling_df %>% subset(date >= '2023-01-01')

# Map golfer ID and course ID for stan format
train_sample$dg_id_mapped <- as.integer(factor(train_sample$dg_id))
train_sample$course_id_mapped <- as.integer(factor(train_sample$course_num))

model <- stan_model("C:/Users/npbig/Downloads/mixedeffects_playereffect.stan")

# Average score by player ID
mu0_prior <- train_sample %>%
  group_by(dg_id_mapped) %>% 
  summarise(meanscore = mean(score))

# Scoring standard deviation by player ID
sigma0_prior <- train_sample %>%
  group_by(dg_id_mapped) %>% 
  summarise(sdscore = sd(score))

# When we have a small number of observations our standard deviation is either 
# missing, less than one, or zero. These dont mesh well with our log transform
# and therefore we will set it to the global average
sigma0_prior$sdscore[is.na(sigma0_prior$sdscore)] <- mean(na.omit(sigma0_prior$sdscore))
sigma0_prior$sdscore[sigma0_prior$sdscore == 0] <- mean(na.omit(sigma0_prior$sdscore))
sigma0_prior$sdscore[sigma0_prior$sdscore < 1] <- mean(na.omit(sigma0_prior$sdscore))

data <- list(N = nrow(train_sample),
             G = length(unique(train_sample$dg_id_mapped)),
             y = train_sample$score,
             P = 2,
             x = train_sample %>% select(sg_putt_mean_6,
                                         sg_t2g_mean_6),
             g = train_sample$dg_id_mapped,
             mu0 = log(mu0_prior$meanscore),
             sigma0 = log(sigma0_prior$sdscore))

fit <- sampling(model,data,chains=1,iter=2000)

posterior_samples <- extract(fit)
saveRDS(posterior_samples,'C:/Users/npbig/Downloads/posterior_samples.rds')
saveRDS(fit,'C:/Users/npbig/Downloads/fit_player.rds')

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

# Generating NEw Samples________________________________________________________
test <- test_sample %>% filter(player_name == "Couples, Fred" & date == "2024-04-14")
n_new <- 1
gnew <- test$dg_id_mapped
cnew <- test$course_id_mapped
x_new <- test %>% select(sg_total_mean_6, 
                         sg_putt_mean_6,
                         sg_t2g_mean_6,
                         sg_app_mean_6)
y_new_samples <- c()
for (i in 1:10000) {
  mu_g <- mean(posterior_samples$mu_g[,gnew])
  beta <- colMeans(posterior_samples$beta)
  lambda <- mu_g + sum(beta*x_new)
  y_new_samples[i] <- rpois(1,exp(lambda))
}

hist(y_new_samples)