if (!require("readxl")) install.packages("readxl",dependencies = T)
if (!require("tidyverse")) install.packages("readxl",dependencies = T)
if (!require("brms")) install.packages("readxl",dependencies = T)
if (!require("metafor")) install.packages("readxl",dependencies = T)


# Data preparation --------------------------------------------------------
# download and load data
library(readxl)

crp <- read_excel("crp.xlsx")

library(tidyverse)
crp <- crp %>% mutate_at(c(names(crp)[c(3,4,6,7)]), as.numeric)

crp$lnVR <- log(crp$t_sd  / crp$c_sd) + 1/(2*(crp$n_t-1)) - 1/(2*(crp$n_c-1))
crp$lnVR_0 <- log(crp$t_sd_0  / crp$c_sd_0) + 1/(2*(crp$n_t-1)) - 1/(2*(crp$n_c-1))
crp$varVR <- 1/(2*(crp$n_t-1)) + 1/(2*(crp$n_c-1))
crp$seVR <- sqrt(crp$varVR)
crp$lnRR <- log(crp$t_m / crp$c_m)
crp$sdRR <- sqrt((crp$t_sd^2/(crp$n_t*crp$t_m^2)) + (crp$c_sd^2)/(crp$n_c*crp$c_m^2))


# CRP Results -------------------------------------------------------------


#Table 1
colSums(crp[,c("n_t","n_c")]) - c(12+133,25+131)

table(crp$Smoke)
round(prop.table(table(crp$Smoke)),2)

table(crp$ROB)
round(prop.table(table(crp$ROB)),2)

table(crp$Comp)
round(prop.table(table(crp$Comp)),2)



#table 2

library(brms)
priors.naive <- c(
  prior(normal(0,0.5), class = Intercept), 
  prior(cauchy(0,1), class = b, coef = lnRR),
  prior(cauchy(0,1), class = sd)
  )

#model 1
br <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0+(1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = crp,
  seed=123)
summary(br)
exp(fixef(br))

#model 2
br2 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0+Comp+(1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = crp,
  seed=123)
summary(br2)
exp(fixef(br2))

#model 2
br3 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0+Comp+Smoke+(1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = crp,
  seed=123)
summary(br3)
exp(fixef(br3))

#model 4
br4 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0+Comp+Smoke+ROB+(1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = crp,
  seed=123)
summary(br4)
exp(fixef(br4))


#Table 3

p_values <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)

set.seed(12345)
i_values <- exp(posterior_samples(br, pars = "b_Intercept"))
set.seed(12345)
i_values = as.matrix(sample_n(i_values,1000))


# Create a matrix to store the results
result_table <- matrix(NA, nrow = length(p_values), ncol = length(i_values))
final <- matrix(NA, nrow = length(p_values), ncol = 3)


for (p_index in seq_along(p_values)) {
  
  results <- ifelse(i_values^2 - 1 < 0 & p_values[p_index] >=0 ,0,
                    ifelse(i_values^2 - 1 < 0 & p_values[p_index] < 0, 
                    weighted.mean(crp$c_sd, crp$n_c) * (abs(p_values[p_index]) - sqrt(p_values[p_index]^2 - abs(i_values^2 - 1))),
                    weighted.mean(crp$c_sd, crp$n_c) * (sqrt(i_values^2 - 1 + p_values[p_index]^2) - p_values[p_index])
  ))
    # Check for NA values and replace with 0
    # Store the results in the table
    result_table[p_index, ] <- results
    result_table[is.na(result_table)] <- 0
    
    final[p_index, 1] = round(quantile(as.matrix(result_table[p_index, ]),c(0.5)),2)
    final[p_index, 2 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.025)),2)
    final[p_index, 3 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.975)),2)
    
    }
final

library(metafor)
meta_crp = escalc(measure= "MD",
                     n1i = n_t, #n inter
                     n2i = n_c, #n cntrl
                     m1i = t_m,  #mean inter
                     m2i = c_m, # mean cntrl
                     sd1i = t_sd , #sd inter
                     sd2i = c_sd , # sd contrl
                     data= crp,
                     slab=Study,
                     ri=ri
) 
res_crp = rma(yi,vi,data=meta_crp)

