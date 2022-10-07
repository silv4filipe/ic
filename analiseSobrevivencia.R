library(survival)
library(GTDL)
library(ggplot2)
library(ggpubr)
library(survminer)
library(MASS)
library(tidyverse)
library(bbmle)
library(stats4)
data(SPcovid)
str(SPcovid)
glimpse(SPcovid)


attach(SPcovid)

#### Gráficos

# sex

km <- survfit(Surv(illness_time,censure) ~ sex, data = SPcovid)
summary(km)$table

 

ggsurvplot(km, risk.table = F,
           size = 1,
           legend = "none",
           legend.title = "Sex",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("Male", "Female"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           surv.median.line = "hv",
           risk.table = F,
           risk.table.col = "strata",
           legend.title = "Sex",
           ggtheme = theme_bw(),
           legend.labs = c("Male", "Female"),
           fun = "cumhaz", data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



# Age group

km <- survfit(Surv(illness_time,censure) ~ age_group, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           legend = "none",
           palette = c("#F0E257", "#43BAF8", "#FFBAF8"),
           legend.title = "Age group",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("Adult", "Elderly", 'Young'),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#F0E257", "#43BAF8", "#FFBAF8"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Age group",
           ggtheme = theme_bw(),
           legend.labs = c("Adult", "Elderly", 'Young'),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# Ethicities

km <- survfit(Surv(illness_time,censure) ~ ethnicities, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           legend = "none",
           palette = c("#1367ED", "#97F092", "#7C2BED", "#ED42A4"),
           legend.title = "Ethicities",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("Asian", "Black", 'Parda', 'White'),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#1367ED", "#97F092", "#7C2BED", "#ED42A4"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Ethicities",
           ggtheme = theme_bw(),
           legend.labs = c("Asian", "Black", 'Parda', 'White'),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# schooling

km <- survfit(Surv(illness_time,censure) ~ schooling, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           legend = "none",
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           legend.title = "Schooling",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("elementary school", "high school", 'higher education', 'middle-school', 'without schooling'),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#F0E207", "#43BAF0", "#7289F0", "#F034C2", "#F08D18"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Schooling",
           ggtheme = theme_bw(),
           legend.labs = c("elementary school", "high school", 'higher education', 'middle-school', 'without schooling'),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# zone

km <- survfit(Surv(illness_time,censure) ~ zone, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           legend = "none",
           palette = c("#EDE713", "#1FEDC6"),
           legend.title = "Zone",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("countryside", "urban"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#EDE713", "#1FEDC6"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Zone",
           ggtheme = theme_bw(),
           legend.labs = c("countryside", "urban"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# asthma

km <- survfit(Surv(illness_time,censure) ~ asthma, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           legend = "none",
           palette = c("#11F872", "#1DF0EC"),
           legend.title = "Asthma",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#11F872", "#1DF0EC"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Asthma",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# cardiopathy

km <- survfit(Surv(illness_time,censure) ~ cardiopathy, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#0FD99C", "#0FA7D9"),
           legend = "none",
           legend.title = "Cardiopathy",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#0FD99C", "#0FA7D9"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Cardiopathy",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# diabetes

km <- survfit(Surv(illness_time,censure) ~ diabetes, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#FAC0EE", "#6125D9"),
           legend = "none",
           legend.title = "Diabetes",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#FAC0EE", "#6125D9"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Diabetes",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# dyspnea

km <- survfit(Surv(illness_time,censure) ~ dyspnea, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#BBF0B7", "#F09ECF"),
           legend = "none",
           legend.title = "Dyspnea",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#BBF0B7", "#F09ECF"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Dyspnea",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# hepatic

km <- survfit(Surv(illness_time,censure) ~ hepatic, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#90A5F0", "#F03764"),
           legend = "none",
           legend.title = "Hepatic",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#90A5F0", "#F03764"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Hepatic",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# neurologic

km <- survfit(Surv(illness_time,censure) ~ neurologic, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#3EECF0", "#E0F090"),
           legend = "none",
           legend.title = "Neurologic",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#3EECF0", "#E0F090"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Neurologic",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# ventilatory_support

km <- survfit(Surv(illness_time,censure) ~ ventilatory_support, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#F0E257", "#43BAF8", '#687AF0'),
           legend = "none",
           legend.title = "Ventilatory Support",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("invasive", "non-invasive",'not apply'),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#F0E257", "#43BAF8", '#687AF0'),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Ventilatory Support",
           ggtheme = theme_bw(),
           legend.labs = c("invasive", "non-invasive",'not apply'),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# icu

km <- survfit(Surv(illness_time,censure) ~ icu, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#FF9E29", "#86AA00"),
           legend = "none",
           legend.title = "ICU",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#FF9E29", "#86AA00"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "ICU",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))

# Obesity

km <- survfit(Surv(illness_time,censure) ~ obesity, data = SPcovid)
summary(km)$table



ggsurvplot(km, risk.table = F,
           size = 1,
           palette = c("#F06C77", "#E56BF0"),
           legend = "none",
           legend.title = "Obesity",
           pval = FALSE,
           ggtheme = theme_bw(),
           legend.labs = c("No", "Yes"),
           data = SPcovid,
           xlab = 'Tempo de Sobrevivência (em Dias)',
           ylab = 'Probabilidade de Sobrevivência',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))



ggsurvplot(km, conf.int = F,
           size = 1,
           legend = "none",
           palette = c("#F06C77", "#E56BF0"),
           surv.median.line = "hv",
           risk.table = F, risk.table.col = "strata",
           legend.title = "Obesity",
           ggtheme = theme_bw(),
           legend.labs = c("Yes", "No"),
           fun = "cumhaz",
           data = SPcovid,
           xlab = 'Tempo (em Dias)',
           ylab = 'Risco acumulado (H)',
           font.x = c(25, face = "bold"), 
           font.y = c(25, face = "bold"), font.tickslab = c(25))


# Teste log Rank

fit <- survdiff(Surv(illness_time, censure) ~ age_group, rho=0)
print(fit)

fit <- survdiff(Surv(illness_time, censure) ~ ethnicities, rho=0)
print(fit)

fit <- survdiff(Surv(illness_time, censure) ~ hepatic, rho=0)
print(fit)

fit <- survdiff(Surv(illness_time, censure) ~ schooling, rho=0)
print(fit)

fit <- survdiff(Surv(illness_time, censure) ~ sex, rho=0)
print(fit)

fit <- survdiff(Surv(illness_time, censure) ~ zone, rho=0)
print(fit)

###

cox_covid <- coxph(Surv(illness_time, censure) ~ , data = SPcovid)

plot(cox.zph(cox_covid))

## Residuos de Schoenfeld


cox_covid <- coxph(Surv(illness_time, censure) ~ age_group + schooling + neurologic + ventilatory_support + icu, data = SPcovid)

modelo0 <- stepAIC(cox_covid)

summary(cox_covid)
summary(modelo0)


par(mfrow = c(2,3))
plot(cox.zph(cox_covid))


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

start <- c(0.64542, -0.83769, -0.23224, -0.67919, -0.11877, 0.07557, 0.35822, 
           -0.70366, -1.08073,0.17387, 1, 1)

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
