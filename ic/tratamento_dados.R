require(dplyr)
require(lubridate)
require(tidyverse)


# Leitura da base de dados
df <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/SRAG_2020.csv", sep = ';')

str(df)

# Filtragem para pacientes classificados com covid e que foram internados
df_covid <- df |> filter(CLASSI_FIN == 5  & HOSPITAL == 1)

# Tratando as colunas que envolvem tempo

head(df_covid$DT_INTERNA)
head(df_covid$DT_EVOLUCA)
head(df_covid$DT_ENTUTI)
head(df_covid$DT_SAIDUTI)
head(df_covid$DT_SIN_PRI)

df_covid$DT_INTERNA <- mdy(paste(df_covid$DT_INTERNA))
df_covid$DT_EVOLUCA <- mdy(paste(df_covid$DT_EVOLUCA))
df_covid$DT_ENTUTI <- mdy(paste(df_covid$DT_ENTUTI))
df_covid$DT_SAIDUTI <- mdy(paste(df_covid$DT_SAIDUTI))
df_covid$DT_SIN_PRI <- mdy(paste(df_covid$DT_SIN_PRI))


# Dropando NA's das variaveis DT_INTERNA e DT_EVOLUCA

df_covid <- drop_na(df_covid, DT_INTERNA)
df_covid <- drop_na(df_covid, DT_EVOLUCA)
df_covid <- drop_na(df_covid, DT_SIN_PRI)

# Criação de variaveis
df_covid <- df_covid %>% mutate(dias_internado = DT_EVOLUCA - DT_INTERNA)
df_covid <- df_covid %>% mutate(dias_p_sintomas = DT_EVOLUCA - DT_SIN_PRI)
df_covid <- df_covid %>% mutate(dias_uti = DT_SAIDUTI - DT_ENTUTI)
df_covid <- df_covid %>% mutate(tempo_sin_int = DT_INTERNA - DT_SIN_PRI)


df_covid$dias_internado <- as.integer(df_covid$dias_internado)
df_covid$dias_p_sintomas <- as.integer(df_covid$dias_p_sintomas)
df_covid$tempo_sin_int <- as.integer(df_covid$tempo_sin_int)

## faixa etaria

df_covid['FX_ETARIA'] <- factor(
  cut(df_covid$NU_IDADE_N, breaks = c(0,19, 59, Inf)),
  labels = c('young', 'adult', 'elderly'),
)


# Seleção de colunas
df2_covid <- df_covid |> select(
  dias_p_sintomas,
  dias_internado,
  CS_SEXO,
  FX_ETARIA,
  CS_RACA,
  CS_ESCOL_N,
  CS_ZONA,
  ASMA,
  CARDIOPATI,
  DIABETES,
  DISPNEIA,
  HEPATICA,
  HEMATOLOGI,
  NEUROLOGIC,
  OBESIDADE,
  PNEUMOPATI,
  RENAL,
  SUPORT_VEN,
  UTI,
  EVOLUCAO,
  
)


# Observando e retirando NA's  
sapply(df2_covid, function(x) sum(is.na(x)))

df2_covid <- drop_na(df2_covid, names(df2_covid))

# retirando linhas com registros 9 e na coluna CSRACA com registro 5

summary(df2_covid)
table(df2_covid$CS_RACA)

df3_covid <- df2_covid |> filter(
  
  CS_SEXO != 9 & CS_RACA != 9 & CS_RACA != 5 & CS_ESCOL_N != 9 & ASMA != 9 &
    CARDIOPATI != 9 & DIABETES != 9 & DISPNEIA != 9 & HEPATICA != 9
  & HEMATOLOGI != 9 &  NEUROLOGIC != 9 & OBESIDADE  != 9 & PNEUMOPATI != 9 &
    RENAL  != 9 &  SUPORT_VEN  != 9 & UTI  != 9
  
)

summary(df3_covid)

sapply(df3_covid, function(x) sum(is.na(x)))

View(df3_covid)

# Categorizando e alterando labels de variavéis

names(df3_covid)

df3_covid$CS_SEXO <- factor(df3_covid$CS_SEXO, labels = c('female','male'), levels = c('F','M')) # 0 - Feminino 1 - Masculino

df3_covid$CS_RACA <- factor(df3_covid$CS_RACA, labels = c('white', 'black', 'asian', 'parda'), levels = 1:4)

df3_covid$CS_ESCOL_N[df3_covid$CS_ESCOL_N == 5] <- 0 # aos que não se aplicam foram convertidos em sem escolaridade

df3_covid$CS_ESCOL_N <- factor(df3_covid$CS_ESCOL_N, labels = c('without-schooling','elementary-school', 'middle-school', 'high-school', 'higher-education'), levels = 0:4)

df3_covid$CS_ZONA[df3_covid$CS_ZONA == 3] <- 2

df3_covid$CS_ZONA <- factor(df3_covid$CS_ZONA, labels = c('urban', 'countryside'), levels = 1:2)

df3_covid$ASMA <- factor(df3_covid$ASMA, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$CARDIOPATI <- factor(df3_covid$CARDIOPATI, labels = c(1,0) , levels = 1:2) # 1 - yes 0 - no

df3_covid$DIABETES <- factor(df3_covid$DIABETES, labels = c(1,0) , levels = 1:2) # 1 - yes 0 - no

df3_covid$DISPNEIA <- factor(df3_covid$DISPNEIA, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$HEPATICA <- factor(df3_covid$HEPATICA, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$HEMATOLOGI <- factor(df3_covid$HEMATOLOGI, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$NEUROLOGIC <- factor(df3_covid$NEUROLOGIC, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$OBESIDADE <- factor(df3_covid$OBESIDADE, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$PNEUMOPATI <- factor(df3_covid$PNEUMOPATI, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$RENAL <- factor(df3_covid$RENAL, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$SUPORT_VEN <- factor(df3_covid$SUPORT_VEN, labels = c('invasive', 'non-invasive', 'not-apply') , levels = 1:3)

df3_covid$UTI <- factor(df3_covid$UTI, labels = c(1,0) , levels = 1:2) # 1 - yes 0- no

df3_covid$EVOLUCAO <- factor(df3_covid$EVOLUCAO, labels = c(0,1) , levels = 1:2) # 0 - cura 1 - óbito


# Mudança de nome das colunas


SPcovid <- rename(df3_covid, illness_time = dias_p_sintomas,
               sex = CS_SEXO, age_group = FX_ETARIA,
               ethnicities = CS_RACA, schooling = CS_ESCOL_N,
               obesity = OBESIDADE, diabetes = DIABETES, cardiopathy = CARDIOPATI,
               asthma = ASMA, censure = EVOLUCAO, 
               icu = UTI, zone = CS_ZONA,  dyspnea = DISPNEIA, hepatic = HEPATICA,
               hematologic = HEMATOLOGI, renal = RENAL, pneumopathy = PNEUMOPATI,
               hospitalized_time = dias_internado, neurologic = NEUROLOGIC, ventilatory_support = SUPORT_VEN)

#Criação do arquivo em CSV
write.table(SPcovid, file = 'SPcovid', sep = ',')
