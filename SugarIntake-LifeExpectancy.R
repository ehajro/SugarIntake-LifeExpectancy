# The relationship between sugar intake and life expectancy - Code

# libraries
library(readr)
library(runjags)
require(tidyverse)
require(coda)
# load all of the separate datasets
SugarIntake <- read.csv("sugar_per_person_g_per_day.csv")
LifeExpectancy <- read.csv("lex.csv")


# keep only the year 2018 for Sugar Intake
SugarIntake2018 <- na.omit(select(SugarIntake, "country", "X2018"))
summary(SugarIntake2018)
# sugar intake data -> divide into five categories based on quantiles with category 1 representing the lowest 20% of sugar intake and cat. 5 representing the highest 20%
thresholds <- quantile(SugarIntake2018$X2018, probs = seq(0, 1, by = 0.2))
SugarIntakeCategories <- cut(SugarIntake2018$X2018, breaks = thresholds, labels = FALSE)
SugarIntake2018 <- cbind(SugarIntake2018, SugarIntakeCategories)



# keep only the life expectancy data for 2018
LifeExpectancy2018 <- na.omit(select(LifeExpectancy, "country", "X2018"))
summary(LifeExpectancy2018)



# create a new dataset by merging the Sugar Intake and the Life Expectancy datasets for year 2018
# merge by common variable "country"
LifeExpectancyBySugarIntake <- merge(LifeExpectancy2018, SugarIntake2018, by="country", suffixes = c("LifeExpectancy", "SugarIntake"))
# only keep the columns for Life Expectancy in year 2018 and the Sugar Intake Categories in our dataset
LifeExpectancyBySugarIntake <- LifeExpectancyBySugarIntake[, c("X2018LifeExpectancy", "SugarIntakeCategories")]
LifeExpectancyBySugarIntake <- na.omit(LifeExpectancyBySugarIntake)
# change the name of variable "X2018LifeExpectancy" to "LifeExp"
names(LifeExpectancyBySugarIntake)[names(LifeExpectancyBySugarIntake) == "X2018LifeExpectancy"] <- "LifeExp"
LifeExpectancyBySugarIntake$SugarIntakeCategories = as.factor(LifeExpectancyBySugarIntake$SugarIntakeCategories)


# A plot that shows the distribution of the life expectancy variable accross the 5 sugar intake categories that have been created
ggplot(LifeExpectancyBySugarIntake, aes(LifeExp, color = SugarIntakeCategories)) +
  geom_density() + labs(title = "Density plot of Life Expectancy") + 
  theme_grey(base_size = 10, base_family = "") 



# calculate the mean life expectancy for each sugar intake category
tapply(LifeExpectancyBySugarIntake$LifeExp, LifeExpectancyBySugarIntake$SugarIntakeCategories, mean)
# calculate the standard deviation of life expectancy for each sugar intake category
tapply(LifeExpectancyBySugarIntake$LifeExp, LifeExpectancyBySugarIntake$SugarIntakeCategories, sd)


# JAGS Script for the Hierarchical Model
modelString <-"
model {
## likelihood
for (i in 1:N){
y[i] ~ dnorm(mu_j[category[i]], invsigma2)
}

## priors
for (j in 1:J){
mu_j[j] ~ dnorm(mu, invtau2)
}
invsigma2 ~ dgamma(a_g, b_g)
sigma <- sqrt(pow(invsigma2, -1))

## hyperpriors
mu ~ dnorm(mu0, 1/g0^2)
invtau2 ~ dgamma(a_t, b_t)
tau <- sqrt(pow(invtau2, -1))
}
"


#Pass the data and hyperparameter values to JAGS:
y = LifeExpectancyBySugarIntake$LifeExp   
category = LifeExpectancyBySugarIntake$SugarIntakeCategories 
N = length(y)  
J = length(unique(category)) 

initsfunction <- function(chain){
  .RNG.seed <- c(1,2)[chain]
  .RNG.name <- c("base::Super-Duper",
                 "base::Wichmann-Hill")[chain]
  return(list(.RNG.seed=.RNG.seed,
              .RNG.name=.RNG.name))
}

the_data <- list("y" = y, "category" = category, "N" = N, "J" = J, 
                 "mu0" = 70, "g0" = 5, 
                 "a_t" = 1, "b_t" = 1,
                 "a_g" = 1, "b_g" = 1)


# Run the JAGS code for this model:
posterior <- run.jags(modelString,
                      n.chains = 1,
                      data = the_data,
                      monitor = c("mu", "tau", "mu_j", "sigma"),
                      adapt = 1000,
                      burnin = 5000,
                      sample = 5000,
                      thin = 3, 
                      inits = initsfunction)


# JAGS Output of the Hierarchical Model
#Obtain posterior summaries of all parameters:
summary(posterior) 

# Diagnostic plots
par(mfrow = c(3, 3))
plot(posterior, vars = c("mu_j[1]", "mu_j[2]","mu_j[3]","mu_j[4]","mu_j[5]","mu",
                         "tau","sigma"))


# To compute the posterior means:
Posterior_Means <- summary(posterior)[, 4]
Means <- data.frame(Mean = Posterior_Means[3:(4 + J - 2)])
Means$Title <- c("Sugar Intake Category 1 (Min-20th percentile)", "Sugar Intake Category 2 (20th-40th percentile)", "Sugar Intake Category 3 (40th-60th percentile)",
                 "Sugar Intake Category 4 (60th-80th percentile)", "Sugar Intake Category 5 (80th percentile-Max)")
Means



# 90% credible intervals for the means
Credible_intervals <- summary(posterior)[, c(1,3)]
Intervals <- data.frame(Interval = Credible_intervals[3:(4 + J - 2),])
Intervals$Title <- c("Sugar Intake Category 1 (Min-20th percentile)", "Sugar Intake Category 2 (20th-40th percentile)", "Sugar Intake Category 3 (40th-60th percentile)",
                     "Sugar Intake Category 4 (60th-80th percentile)", "Sugar Intake Category 5 (80th percentile-Max)")
Intervals

# The posterior samples of the mean parameter for each sugar intake category, convert to mcmc object
Category1mean_draws <- as.mcmc(posterior, vars = "mu_j[1]")
Category2mean_draws <- as.mcmc(posterior, vars = "mu_j[2]")
Category3mean_draws <- as.mcmc(posterior, vars = "mu_j[3]")
Category4mean_draws <- as.mcmc(posterior, vars = "mu_j[4]")
Category5mean_draws <- as.mcmc(posterior, vars = "mu_j[5]")


# The posterior probability that the mean life expectancy for Sugar Intake category 1 is lower than that of category 2
mean(Category1mean_draws < Category2mean_draws)

# The posterior probability that the mean life expectancy for Sugar Intake category 2 is lower than that of category 3
mean(Category2mean_draws < Category3mean_draws)


# The posterior probability that the mean life expectancy for Sugar Intake category 3 is lower than that of category 4
mean(Category3mean_draws < Category4mean_draws)

# The posterior probability that the mean life expectancy for Sugar Intake category 4 is lower than that of category 5
mean(Category4mean_draws < Category5mean_draws)

#The posterior probability that the mean life expectancy for Sugar Intake category 1 is lower than that of category 5
mean(Category1mean_draws < Category5mean_draws)


#Posterior distribution of ratio  (the proportion of total variance explained by group-level variance)
tau_draws <- as.mcmc(posterior, vars = "tau")
sigma_draws <- as.mcmc(posterior, vars = "sigma")
R <- tau_draws^2/(tau_draws^2 + sigma_draws^2)

df <- as.data.frame(R)

quantile(R, c(0.025, 0.975))