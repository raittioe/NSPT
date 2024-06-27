if (!require("readxl")) install.packages("readxl",dependencies = T)
if (!require("tidyverse")) install.packages("readxl",dependencies = T)
if (!require("brms")) install.packages("readxl",dependencies = T)
if (!require("metafor")) install.packages("readxl",dependencies = T)


# Data preparation --------------------------------------------------------

library(readxl)
data <- read_excel("data.xlsx")

library(tidyverse)
data= data %>% pivot_wider(names_from = Group,values_from= c(N:PD3_6_sd))

data <- data %>% mutate_at(c(names(data)[3:length(names(data))]), as.numeric)
data$N = data$N_I + data$N_C
data$ri = 0
data$Smoke = data$Smoke/data$N
data$Smoke = factor(ifelse(data$Smoke > 0,1,
                     ifelse(data$Smoke == 0,0,NA )))

meta3pd = data[!is.na(data$PD_3_m_C) ,]

meta3pd$PD_I_sd = meta3pd$PD_3_sd_I
meta3pd$PD_I = meta3pd$PD_3_m_I
meta3pd$PD_C = meta3pd$PD_3_m_C
meta3pd$PD_C_sd = meta3pd$PD_3_sd_C
meta3pd$PD_0_I = meta3pd$PD_0_m_I
meta3pd$PD_0_C = meta3pd$PD_0_m_C

meta3pd$lnVR <- log(meta3pd$PD_3_sd_I  / meta3pd$PD_3_sd_C) + 1/(2*(meta3pd$N_I-1)) - 1/(2*(meta3pd$N_C-1))
meta3pd$lnVR_0 <- log(meta3pd$PD_0_sd_I  / meta3pd$PD_0_sd_C) + 1/(2*(meta3pd$N_I-1)) - 1/(2*(meta3pd$N_C-1))
meta3pd$varVR <- 1/(2*(meta3pd$N_I-1)) + 1/(2*(meta3pd$N_C-1))
meta3pd$seVR <- sqrt(meta3pd$varVR)
meta3pd$lnRR <- log(meta3pd$PD_3_m_I / meta3pd$PD_3_m_C)
meta3pd$sdRR <- sqrt((meta3pd$PD_3_sd_I^2/(meta3pd$N_I*meta3pd$PD_3_m_I^2)) + (meta3pd$PD_3_sd_C^2)/(meta3pd$N_C*meta3pd$PD_3_m_C^2))
meta3pd$fup = 3 #follow-up time


meta6pd = data[!is.na(data$PD_6_m_C) ,]

meta6pd$PD_I_sd = meta6pd$PD_6_sd_I
meta6pd$PD_C_sd = meta6pd$PD_6_sd_C
meta6pd$PD_I = meta6pd$PD_6_m_I
meta6pd$PD_C = meta6pd$PD_6_m_C
meta6pd$PD_0_I = meta6pd$PD_0_m_I
meta6pd$PD_0_C = meta6pd$PD_0_m_C

meta6pd$lnVR <- log(meta6pd$PD_6_sd_I  / meta6pd$PD_6_sd_C) + 1/(2*(meta6pd$N_I-1)) - 1/(2*(meta6pd$N_C-1))
meta6pd$lnVR_0 <- log(meta6pd$PD_0_sd_I  / meta6pd$PD_0_sd_C) + 1/(2*(meta6pd$N_I-1)) - 1/(2*(meta6pd$N_C-1))
meta6pd$varVR <- 1/(2*(meta6pd$N_I-1)) + 1/(2*(meta6pd$N_C-1))
meta6pd$seVR <- sqrt(meta6pd$varVR)
meta6pd$lnRR <- log(meta6pd$PD_6_m_I / meta6pd$PD_6_m_C)
meta6pd$sdRR <- sqrt((meta6pd$PD_6_sd_I^2/(meta6pd$N_I*meta6pd$PD_6_m_I^2)) + (meta6pd$PD_6_sd_C^2)/(meta6pd$N_C*meta6pd$PD_6_m_C^2))
meta6pd$fup = 6 #follow-up time


metapd = rbind(meta3pd,meta6pd)
metapd$fup = factor(metapd$fup)
metapd$measure = "PD"

#CAL
meta3cal = data[!is.na(data$CAL_3_m_C) ,]

meta3cal$CAL_I_sd = meta3cal$CAL_3_sd_I
meta3cal$CAL_C_sd = meta3cal$CAL_3_sd_C
meta3cal$CAL_I = meta3cal$CAL_3_m_I
meta3cal$CAL_C = meta3cal$CAL_3_m_C
meta3cal$CAL_0_I = meta3cal$CAL_0_m_I
meta3cal$CAL_0_C = meta3cal$CAL_0_m_C

meta3cal$lnVR <- log(meta3cal$CAL_3_sd_I  / meta3cal$CAL_3_sd_C) + 1/(2*(meta3cal$N_I-1)) - 1/(2*(meta3cal$N_C-1))
meta3cal$lnVR_0 <- log(meta3cal$CAL_0_sd_I  / meta3cal$CAL_0_sd_C) + 1/(2*(meta3cal$N_I-1)) - 1/(2*(meta3cal$N_C-1))
meta3cal$varVR <- 1/(2*(meta3cal$N_I-1)) + 1/(2*(meta3cal$N_C-1))
meta3cal$seVR <- sqrt(meta3cal$varVR)
meta3cal$lnRR <- log(meta3cal$CAL_3_m_I / meta3cal$CAL_3_m_C)
meta3cal$sdRR <- sqrt((meta3cal$CAL_3_sd_I^2/(meta3cal$N_I*meta3cal$CAL_3_m_I^2)) + (meta3cal$CAL_3_sd_C^2)/(meta3cal$N_C*meta3cal$CAL_3_m_C^2))
meta3cal$fup = 3 #follow-up time

meta6cal = data[!is.na(data$CAL_6_m_C) ,]

meta6cal$CAL_I_sd = meta6cal$CAL_6_sd_I
meta6cal$CAL_C_sd = meta6cal$CAL_6_sd_C
meta6cal$CAL_I = meta6cal$CAL_6_m_I
meta6cal$CAL_C = meta6cal$CAL_6_m_C
meta6cal$CAL_0_I = meta6cal$CAL_0_m_I
meta6cal$CAL_0_C = meta6cal$CAL_0_m_C
meta6cal$lnVR <- log(meta6cal$CAL_6_sd_I  / meta6cal$CAL_6_sd_C) + 1/(2*(meta6cal$N_I-1)) - 1/(2*(meta6cal$N_C-1))
meta6cal$lnVR_0 <- log(meta6cal$CAL_0_sd_I  / meta6cal$CAL_0_sd_C) + 1/(2*(meta6cal$N_I-1)) - 1/(2*(meta6cal$N_C-1))
meta6cal$varVR <- 1/(2*(meta6cal$N_I-1)) + 1/(2*(meta6cal$N_C-1))
meta6cal$seVR <- sqrt(meta6cal$varVR)
meta6cal$lnRR <- log(meta6cal$CAL_6_m_I / meta6cal$CAL_6_m_C)
meta6cal$sdRR <- sqrt((meta6cal$CAL_6_sd_I^2/(meta6cal$N_I*meta6cal$CAL_6_m_I^2)) + (meta6cal$CAL_6_sd_C^2)/(meta6cal$N_C*meta6cal$CAL_6_m_C^2))
meta6cal$fup = 6 #follow-up time

metacal = rbind(meta3cal,meta6cal)
metacal$fup = factor(metacal$fup)
metacal$measure = "CAL"


#pd3

meta3pd3 = data[!is.na(data$PD3_3_m_C) ,]

meta3pd3$PD3_I_sd = meta3pd3$PD3_3_sd_I
meta3pd3$PD3_I = meta3pd3$PD3_3_m_I
meta3pd3$PD3_C = meta3pd3$PD3_3_m_C
meta3pd3$PD3_C_sd = meta3pd3$PD3_3_sd_C
meta3pd3$PD3_0_I = meta3pd3$PD3_0_m_I
meta3pd3$PD3_0_C = meta3pd3$PD3_0_m_C

meta3pd3$lnVR <- log(meta3pd3$PD3_3_sd_I  / meta3pd3$PD3_3_sd_C) + 1/(2*(meta3pd3$N_I-1)) - 1/(2*(meta3pd3$N_C-1))
meta3pd3$lnVR_0 <- log(meta3pd3$PD3_0_sd_I  / meta3pd3$PD3_0_sd_C) + 1/(2*(meta3pd3$N_I-1)) - 1/(2*(meta3pd3$N_C-1))
meta3pd3$varVR <- 1/(2*(meta3pd3$N_I-1)) + 1/(2*(meta3pd3$N_C-1))
meta3pd3$seVR <- sqrt(meta3pd3$varVR)
meta3pd3$lnRR <- log(meta3pd3$PD3_3_m_I / meta3pd3$PD3_3_m_C)
meta3pd3$sdRR <- sqrt((meta3pd3$PD3_3_sd_I^2/(meta3pd3$N_I*meta3pd3$PD3_3_m_I^2)) + (meta3pd3$PD3_3_sd_C^2)/(meta3pd3$N_C*meta3pd3$PD3_3_m_C^2))
meta3pd3$fup = 3 #follow-up time


meta6pd3 = data[!is.na(data$PD3_6_m_C) ,]

meta6pd3$PD3_I_sd = meta6pd3$PD3_6_sd_I
meta6pd3$PD3_C_sd = meta6pd3$PD3_6_sd_C
meta6pd3$PD3_I = meta6pd3$PD3_6_m_I
meta6pd3$PD3_C = meta6pd3$PD3_6_m_C
meta6pd3$PD3_0_I = meta6pd3$PD3_0_m_I
meta6pd3$PD3_0_C = meta6pd3$PD3_0_m_C

meta6pd3$lnVR <- log(meta6pd3$PD3_6_sd_I  / meta6pd3$PD3_6_sd_C) + 1/(2*(meta6pd3$N_I-1)) - 1/(2*(meta6pd3$N_C-1))
meta6pd3$lnVR_0 <- log(meta6pd3$PD3_0_sd_I  / meta6pd3$PD3_0_sd_C) + 1/(2*(meta6pd3$N_I-1)) - 1/(2*(meta6pd3$N_C-1))
meta6pd3$varVR <- 1/(2*(meta6pd3$N_I-1)) + 1/(2*(meta6pd3$N_C-1))
meta6pd3$seVR <- sqrt(meta6pd3$varVR)
meta6pd3$lnRR <- log(meta6pd3$PD3_6_m_I / meta6pd3$PD3_6_m_C)
meta6pd3$sdRR <- sqrt((meta6pd3$PD3_6_sd_I^2/(meta6pd3$N_I*meta6pd3$PD3_6_m_I^2)) + (meta6pd3$PD3_6_sd_C^2)/(meta6pd3$N_C*meta6pd3$PD3_6_m_C^2))
meta6pd3$fup = 6 #follow-up time


metapd3 = rbind(meta3pd3,meta6pd3)
metapd3$fup = factor(metapd3$fup)
metapd3$measure = "PD3"

#BOP%

meta3bop = data[!is.na(data$BOP_3_m_C) ,]

meta3bop$BOP_I_sd = meta3bop$BOP_3_sd_I
meta3bop$BOP_I = meta3bop$BOP_3_m_I
meta3bop$BOP_C = meta3bop$BOP_3_m_C
meta3bop$BOP_C_sd = meta3bop$BOP_3_sd_C
meta3bop$BOP_0_I = meta3bop$BOP_0_m_I
meta3bop$BOP_0_C = meta3bop$BOP_0_m_C

meta3bop$lnVR <- log(meta3bop$BOP_3_sd_I  / meta3bop$BOP_3_sd_C) + 1/(2*(meta3bop$N_I-1)) - 1/(2*(meta3bop$N_C-1))
meta3bop$lnVR_0 <- log(meta3bop$BOP_0_sd_I  / meta3bop$BOP_0_sd_C) + 1/(2*(meta3bop$N_I-1)) - 1/(2*(meta3bop$N_C-1))
meta3bop$varVR <- 1/(2*(meta3bop$N_I-1)) + 1/(2*(meta3bop$N_C-1))
meta3bop$seVR <- sqrt(meta3bop$varVR)
meta3bop$lnRR <- log(meta3bop$BOP_3_m_I / meta3bop$BOP_3_m_C)
meta3bop$sdRR <- sqrt((meta3bop$BOP_3_sd_I^2/(meta3bop$N_I*meta3bop$BOP_3_m_I^2)) + (meta3bop$BOP_3_sd_C^2)/(meta3bop$N_C*meta3bop$BOP_3_m_C^2))
meta3bop$fup = 3 #follow-up time


meta6bop = data[!is.na(data$BOP_6_m_C) ,]

meta6bop$BOP_I_sd = meta6bop$BOP_6_sd_I
meta6bop$BOP_C_sd = meta6bop$BOP_6_sd_C
meta6bop$BOP_I = meta6bop$BOP_6_m_I
meta6bop$BOP_C = meta6bop$BOP_6_m_C
meta6bop$BOP_0_I = meta6bop$BOP_0_m_I
meta6bop$BOP_0_C = meta6bop$BOP_0_m_C

meta6bop$lnVR <- log(meta6bop$BOP_6_sd_I  / meta6bop$BOP_6_sd_C) + 1/(2*(meta6bop$N_I-1)) - 1/(2*(meta6bop$N_C-1))
meta6bop$lnVR_0 <- log(meta6bop$BOP_0_sd_I  / meta6bop$BOP_0_sd_C) + 1/(2*(meta6bop$N_I-1)) - 1/(2*(meta6bop$N_C-1))
meta6bop$varVR <- 1/(2*(meta6bop$N_I-1)) + 1/(2*(meta6bop$N_C-1))
meta6bop$seVR <- sqrt(meta6bop$varVR)
meta6bop$lnRR <- log(meta6bop$BOP_6_m_I / meta6bop$BOP_6_m_C)
meta6bop$sdRR <- sqrt((meta6bop$BOP_6_sd_I^2/(meta6bop$N_I*meta6bop$BOP_6_m_I^2)) + (meta6bop$BOP_6_sd_C^2)/(meta6bop$N_C*meta6bop$BOP_6_m_C^2))
meta6bop$fup = 6 #follow-up time


metabop = rbind(meta3bop,meta6bop)
metabop$fup = factor(metabop$fup)
metabop$measure = "BOP"

rm(meta3bop)
rm(meta3cal)
rm(meta3pd3)
rm(meta3pd)
rm(meta6bop)
rm(meta6cal)
rm(meta6pd3)
rm(meta6pd)


#Table 1
length(unique(metapd$Study)) #trials
length(metapd$Study) #follow-ups

table(metapd$Smoke)
round(prop.table(table(metapd$Smoke)),2)
table(metapd$ROB)
round(prop.table(table(metapd$ROB)),2)
table(metapd$Comp)
round(prop.table(table(metapd$Comp)),2)

length(unique(metacal$Study)) #trials
length(metacal$Study) #follow-ups

table(metacal$Smoke)
round(prop.table(table(metacal$Smoke)),2)
table(metacal$ROB)
round(prop.table(table(metacal$ROB)),2)
table(metacal$Comp)
round(prop.table(table(metacal$Comp)),2)


length(unique(metapd3$Study)) #trials
length(metapd3$Study) #follow-ups

table(metapd3$Smoke)
round(prop.table(table(metapd3$Smoke)),2)
table(metapd3$ROB)
round(prop.table(table(metapd3$ROB)),2)
table(metapd3$Comp)
round(prop.table(table(metapd3$Comp)),2)

length(unique(metabop$Study)) #trials
length(metabop$Study) #follow-ups

table(metabop$Smoke)
round(prop.table(table(metabop$Smoke)),2)
table(metabop$ROB)
round(prop.table(table(metabop$ROB)),2)
table(metabop$Comp)
round(prop.table(table(metabop$Comp)),2)


# CAL results -------------------------------------------------------------

# table 2

library(brms)
priors.naive <- c(
  prior(normal(0,0.5), class = Intercept), 
  prior(cauchy(0,1), class = b, coef = lnRR),
  prior(cauchy(0,1), class = sd)
  )

#model 1
br <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metacal,
  seed=123
  ) 
summary(br)
exp(fixef(br))

#model 2
br2 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metacal,
  seed=123
) 
summary(br2)
exp(fixef(br2))

#model 3
br3 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metacal,
  seed=123
) 
summary(br3)
exp(fixef(br3))

#model 4
br4 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ROB+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metacal,
  seed=123
) 
summary(br4)
exp(fixef(br4))


#Table 3

# Define the values for p and i
p_values <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
set.seed(12345)
i_values <- exp(posterior_samples(br, pars = "b_Intercept"))
set.seed(12345)
i_values = as.matrix(sample_n(i_values,1000))
hist(i_values)


# Create a matrix to store the results
result_table <- matrix(NA, nrow = length(p_values), ncol = length(i_values))
final <- matrix(NA, nrow = length(p_values), ncol = 3)


for (p_index in seq_along(p_values)) {
  
  results <-        ifelse(i_values^2 - 1 < 0 & p_values[p_index] >=0,0,
                    ifelse(i_values^2 - 1 < 0 & p_values[p_index] < 0, 
                    weighted.mean(metacal$CAL_C_sd, metacal$N_C) * (abs(p_values[p_index]) - sqrt(p_values[p_index]^2 - abs(i_values^2 - 1))),
                    weighted.mean(metacal$CAL_C_sd, metacal$N_C) * (sqrt(i_values^2 - 1 + p_values[p_index]^2) - p_values[p_index]))
  )
  
  result_table[p_index, ] <- results
  result_table[is.na(result_table)] <- 0
  
  final[p_index, 1] = round(quantile(as.matrix(result_table[p_index, ]),c(0.5)),2)
  final[p_index, 2 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.025)),2)
  final[p_index, 3 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.975)),2)
  
}
final

#mean difference Table 3
library(metafor)
meta_cal_md = escalc(measure= "MD",
                     n1i = N_I, #n inter
                     n2i = N_C, #n cntrl
                     m1i = CAL_I,  #mean inter
                     m2i = CAL_C, # mean cntrl
                     sd1i = CAL_I_sd , #sd inter
                     sd2i = CAL_C_sd , # sd contrl
                     data= metacal,
                     slab=Study,
                     ri=ri
) 
rma(yi,vi,data=meta_cal_md)


# PPD results ---------------------------------------------------------------------

#Table 2
colSums(metapd[ (!is.na(metapd$PD_3_m_I) | !is.na(metapd$PD_6_m_I)) ,c("N_I","N_C")])

library(brms)
priors.naive <- c(
  prior(normal(0,0.5), class = Intercept), 
  prior(cauchy(0,1), class = b, coef = lnRR),
  prior(cauchy(0,1), class = sd)
)

#model 1
br <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd,
  seed=123
) 

summary(br)
exp(fixef(br))

br2 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+ (1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd,
  seed=123
) 
summary(br2)
exp(fixef(br2))

br3 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd,
  seed=123
) 
summary(br3)
exp(fixef(br3))

br4 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ ROB+(1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd,
  seed=123
) 
summary(br4)
exp(fixef(br4))


#Table 3

# Define the values for p and i
p_values <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
set.seed(12345)
i_values <- exp(posterior_samples(br, pars = "b_Intercept"))
set.seed(12345)
i_values = as.matrix(sample_n(i_values,1000))
hist(i_values)

# Create a matrix to store the results
result_table <- matrix(NA, nrow = length(p_values), ncol = length(i_values))
final <- matrix(NA, nrow = length(p_values), ncol = 3)

for (p_index in seq_along(p_values)) {
  
  results <-        ifelse(i_values^2 - 1 < 0 & p_values[p_index] >=0,0,
                           ifelse(i_values^2 - 1 < 0 & p_values[p_index] < 0, 
                                  weighted.mean(metapd$PD_C_sd, metapd$N_C) * (abs(p_values[p_index]) - sqrt(p_values[p_index]^2 - abs(i_values^2 - 1))),
                                  weighted.mean(metapd$PD_C_sd, metapd$N_C) * (sqrt(i_values^2 - 1 + p_values[p_index]^2) - p_values[p_index]))
  )
  
  result_table[p_index, ] <- results
  result_table[is.na(result_table)] <- 0
  
  final[p_index, 1] = round(quantile(as.matrix(result_table[p_index, ]),c(0.5)),2)
  final[p_index, 2 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.025)),2)
  final[p_index, 3 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.975)),2)
  
}
final

#mean difference Table 3
library(metafor)
meta_pd_md = escalc(measure= "MD",
                     n1i = N_I, #n inter
                     n2i = N_C, #n cntrl
                     m1i = PD_I,  #mean inter
                     m2i = PD_C, # mean cntrl
                     sd1i = PD_I_sd , #sd inter
                     sd2i = PD_C_sd , # sd contrl
                     data= metapd,
                     slab=Study,
                     ri=ri
) 
rma(yi,vi,data=meta_pd_md)


# PD3 results --------------------------------------------------------------------
#Table 2
colSums(metapd3[ (!is.na(metapd3$PD3_3_m_I) | !is.na(metapd3$PD3_6_m_I)) ,c("N_I","N_C")])

library(brms)
priors.naive <- c(
  prior(normal(0,0.5), class = Intercept), 
  prior(cauchy(0,1), class = b, coef = lnRR),
  prior(cauchy(0,1), class = sd)
)

#model 1
br <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + (1|Study), # (1 | Level3 / Level2)
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd3,
  seed=123
   ,control = list(adapt_delta = 0.99) # for PD3
) 

summary(br)
exp(fixef(br))

br2 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd3,
  seed=123
  ,control = list(adapt_delta = 0.99) # for PD3
) 
summary(br2)
exp(fixef(br2))

br3 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ (1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd3,
  seed=123
  ,control = list(adapt_delta = 0.999) # for PD3
) 
summary(br3)
exp(fixef(br3))

br4 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ROB+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metapd3,
  seed=123
  ,control = list(adapt_delta = 0.999) # for PD3
) 
summary(br4)
exp(fixef(br4))



#Table 3

# Define the values for p and i
p_values <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
set.seed(12345)
i_values <- exp(posterior_samples(br, pars = "b_Intercept"))
set.seed(12345)
i_values = as.matrix(sample_n(i_values,1000))
hist(i_values)

# Create a matrix to store the results
result_table <- matrix(NA, nrow = length(p_values), ncol = length(i_values))
final <- matrix(NA, nrow = length(p_values), ncol = 3)

for (p_index in seq_along(p_values)) {
  
  results <-        ifelse(i_values^2 - 1 < 0 & p_values[p_index] >=0,0,
                           ifelse(i_values^2 - 1 < 0 & p_values[p_index] < 0, 
                                  weighted.mean(metapd3$PD3_C_sd, metapd3$N_C) * (abs(p_values[p_index]) - sqrt(p_values[p_index]^2 - abs(i_values^2 - 1))),
                                  weighted.mean(metapd3$PD3_C_sd, metapd3$N_C) * (sqrt(i_values^2 - 1 + p_values[p_index]^2) - p_values[p_index]))
  )
  
  result_table[p_index, ] <- results
  result_table[is.na(result_table)] <- 0
  
  final[p_index, 1] = round(quantile(as.matrix(result_table[p_index, ]),c(0.5)),2)
  final[p_index, 2 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.025)),2)
  final[p_index, 3 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.975)),2)
  
}
final

#mean difference Table 3
library(metafor)
meta_pd3_md = escalc(measure= "MD",
                    n1i = N_I, #n inter
                    n2i = N_C, #n cntrl
                    m1i = PD3_I,  #mean inter
                    m2i = PD3_C, # mean cntrl
                    sd1i = PD3_I_sd , #sd inter
                    sd2i = PD3_C_sd , # sd contrl
                    data= metapd3,
                    slab=Study,
                    ri=ri
) 
rma(yi,vi,data=meta_pd3_md)


# BOP% results --------------------------------------------------------------------
#Table 2
colSums(metabop[ (!is.na(metabop$BOP_3_m_I) | !is.na(metabop$BOP_6_m_I)) ,c("N_I","N_C")])

library(brms)
priors.naive <- c(
  prior(normal(0,0.5), class = Intercept), 
  prior(cauchy(0,1), class = b, coef = lnRR),
  prior(cauchy(0,1), class = sd)
)

#model 1
br <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metabop,
  seed=123
) 

summary(br)
exp(fixef(br))

br2 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metabop,
  seed=123
) 
summary(br2)
exp(fixef(br2))

br3 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ (1|Study), 
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metabop,
  seed=123
) 
summary(br3)
exp(fixef(br3))

br4 <- brm(
  lnVR|se(seVR) ~ lnRR + lnVR_0 + Comp+Smoke+ROB+ (1|Study),
  prior = priors.naive,
  chains=1,
  iter=10000,
  data = metabop,
  seed=123
) 
summary(br4)
exp(fixef(br4))



#Table 3

# Define the values for p and i
p_values <- c(-1, -0.8, -0.6, -0.4, -0.2, 0, 0.2, 0.4, 0.6, 0.8, 1)
set.seed(12345)
i_values <- exp(posterior_samples(br, pars = "b_Intercept"))
set.seed(12345)
i_values = as.matrix(sample_n(i_values,1000))
hist(i_values)


result_table <- matrix(NA, nrow = length(p_values), ncol = length(i_values))
final <- matrix(NA, nrow = length(p_values), ncol = 3)

for (p_index in seq_along(p_values)) {
  
  results <-        ifelse(i_values^2 - 1 < 0 & p_values[p_index] >=0,0,
                           ifelse(i_values^2 - 1 < 0 & p_values[p_index] < 0, 
                                  weighted.mean(metabop$BOP_C_sd, metabop$N_C) * (abs(p_values[p_index]) - sqrt(p_values[p_index]^2 - abs(i_values^2 - 1))),
                                  weighted.mean(metabop$BOP_C_sd, metabop$N_C) * (sqrt(i_values^2 - 1 + p_values[p_index]^2) - p_values[p_index]))
  )
  
  result_table[p_index, ] <- results
  result_table[is.na(result_table)] <- 0
  
  final[p_index, 1] = round(quantile(as.matrix(result_table[p_index, ]),c(0.5)),2)
  final[p_index, 2 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.025)),2)
  final[p_index, 3 ] = round(quantile(as.matrix(result_table[p_index, ]),c(0.975)),2)
  
}
final

#mean difference Table 3
library(metafor)
meta_bop_md = escalc(measure= "MD",
                     n1i = N_I, #n inter
                     n2i = N_C, #n cntrl
                     m1i = BOP_I,  #mean inter
                     m2i = BOP_C, # mean cntrl
                     sd1i = BOP_I_sd , #sd inter
                     sd2i = BOP_C_sd , # sd contrl
                     data= metabop,
                     slab=Study,
                     ri=ri
) 
rma(yi,vi,data=meta_bop_md)
