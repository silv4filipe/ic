library(survival)
library(GTDL)
library(ggplot2)
library(ggpubr)
library(survminer)
library(MASS)
library(tidyverse)
library(bbmle)
library(stats4)


SPcovid <- read.csv('SPcovid')
glimpse(SPcovid)


attach(SPcovid)






## Residuos de Schoenfeld


cox_covid <- coxph(Surv(illness_time, censure) ~ sex + age_group + ethnicities + schooling + zone + asthma + cardiopathy + diabetes + dyspnea + hepatic + hematologic + neurologic + obesity + pneumopathy + renal + 
                     ventilatory_support + icu, data = SPcovid)

modelo0 <- stepAIC(cox_covid)


summary(modelo0)


cox_covid1 <- coxph(Surv(illness_time, censure) ~ age_group + schooling + neurologic + ventilatory_support + icu, 
                    data = SPcovid)

modelo1 <- stepAIC(cox_covid1)

summary(modelo1)


cox_covid2 <- coxph(Surv(illness_time, censure) ~ sex + icu + obesity +
               diabetes + cardiopathy + asthma, 
             data = SPcovid)

modelo2 <- stepAIC(cox_covid2)

summary(modelo2)
summary(cox_covid2)


cox_covid3 <- coxph(Surv(illness_time, censure) ~ age_group + schooling + neurologic + ventilatory_support + icu + cardiopathy + asthma + diabetes, 
                    data = SPcovid)

modelo3 <- stepAIC(cox_covid3)

summary(modelo3)

summary(cox_covid3)


cox_covid4 <- coxph(Surv(illness_time, censure) ~ age_group + schooling + neurologic + ventilatory_support + icu + dyspnea, 
                    data = SPcovid)

modelo4 <- stepAIC(cox_covid4)

summary(modelo4)

summary(cox_covid4)



cox_covid5 <- coxph(Surv(illness_time, censure) ~ age_group + schooling + neurologic + ventilatory_support + icu + renal, 
                    data = SPcovid)

modelo5 <- stepAIC(cox_covid5)

summary(modelo5)

summary(cox_covid5)


cox_covid6 <- coxph(Surv(illness_time, censure) ~ age_group + schooling + neurologic + ventilatory_support + icu + zone, 
                    data = SPcovid)

modelo6 <- stepAIC(cox_covid6)

summary(modelo6)

summary(cox_covid6)



cox_covid7 <- coxph(Surv(illness_time, censure) ~ icu + obesity + diabetes + cardiopathy + asthma + neurologic + ventilatory_support + icu, 
                    data = SPcovid)

modelo7 <- stepAIC(cox_covid7)

summary(modelo7)

summary(cox_covid7)

#-----GTDL--------------------------------------------------------------------#

time <- illness_time
censur <- censure

formula <- ~ age_group + schooling + neurologic + ventilatory_support + icu

start <- c(0.64542, -0.83769,  -0.23224, -0.67919, -0.11877,
           0.07557, 0.35822, -0.70366, -1.08073, 0.17387,1, 1)

fit.model <- mle2.GTDL(t = time, start = start, 
                       formula = formula, censur = censur)
fit.model


time1 <- illness_time
censur1 <- censure

# asma

formula <- ~ age_group + schooling + neurologic + ventilatory_support +
  icu + cardiopathy + asthma

start1 <- c(0.65956, -0.86473,  -0.23119, -0.68160, -0.11615, 0.08203, 0.35665,
            -0.70540, -1.08088, 0.17615, -0.07576, -0.10382, 1, 1)

fit.model1 <- mle2.GTDL(t = time1, start = start1, 
                        formula = formula, censur = censur1)
fit.model1


# diabetes

formula <- ~ age_group + schooling + neurologic + ventilatory_support +
  icu + cardiopathy + diabetes

start1 <- c(0.65956, -0.86473,  -0.23119, -0.68160, -0.11615, 0.08203, 0.35665,
            -0.70540, -1.08088, 0.17615, -0.07576, 0.434048, 1, 1)


fit.model1 <- mle2.GTDL(t = time1, start = start1, 
                       formula = formula, censur = censur1)
fit.model1

# dyspnea 

formula <- ~ age_group + schooling + neurologic + ventilatory_support +
  icu + dyspnea

start1 <- c(1.9117, 0.4473,  0.7948, 0.5113, 0.8868, 1.0718, 1.4361,
            0.4964, 0.3451, 1.1845, 1.0980, 1, 1)


fit.model1 <- mle2.GTDL(t = time1, start = start1, 
                        formula = formula, censur = censur1)
fit.model1

# renal

formula <- ~ age_group + schooling + neurologic + ventilatory_support +
  icu + renal

start1 <- c(0.64431, -0.84511,  -0.22508, -0.68102, -0.11207, 0.07853, 0.35467,
            -0.69968, -1.07578, 0.17367, 0.13717, 1, 1)


fit.model1 <- mle2.GTDL(t = time1, start = start1, 
                        formula = formula, censur = censur1)
fit.model1



# Envelope


formula <- ~ age_group + schooling + neurologic + ventilatory_support + icu

r <- random.quantile.GTDL(t = time, formula = formula,
                     pHat = fit.model$Coefficients[,1], censur = censur)
r
