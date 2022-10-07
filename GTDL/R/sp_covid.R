#' @title SP_covid
#'
#' @description Registration of hospitalized patients with
#'Severe Acute Respiratory Syndrome (SARS) and which were confirmed later
#'be affected with SARS-CoV-2 (COVID-19). 
#' 
#' @docType data
#' @keywords dataSets
#' @name SP_covid
#' @usage data(SP_covid)
#' @format This data frame contains a \code{tibble} with 15 columns:
#' \itemize{
#' \item {first_symptoms:} {Date of registration of the first symptoms.}
#' \item {hospitalization:} {Hospitalization registration date.}
#' \item {censure_date:} {Censure registration date.}
#' \item {time_symptoms:} {Full days between the date of hospitalization and the 
#' date of censure.}
#' \item {time_hospitalization:} {Full days between first symptoms and 
#' the date of censure.}
#' \item {censure:} {Cure = 0, Death = 1.}
#' \item {age_group:} {Young = 0-19 years, Adult = 20-59 years,
#'  Elderly = 60+ years}
#' \item {ethnicities:} {White,Black,Asian,Pardo,Indigenous}
#' \item {schooling:} {Illiterate = 0, Elementary School = 1, Middle School = 2,
#'High School = 3, College = 4}
#' \item {sex:} {male = 0, female = 1}
#' \item {icu:} {Were you admitted to the ICU? no = 0, yes = 1}
#' \item {obesity:} {Does the patient have obesity? no = 0, yes = 1}
#' \item {diabetes:} {Does the patient have diabetes mellitus? no = 0, yes = 1}
#' \item {cardiopathy:} {Does the patient have chronic cardiovascular disease?,
#'  no = 0, yes = 1}
#' \item {asthma:} {Does the patient have asthma? no = 0, yes = 1}
#' }
#' 
#' @note 
#' Fundacao Seade, dados-covid-sp. 03-11-2021,Source:SIVEP-Gripe 
#' GitHub repository - https://github.com/seade-R/dados-covid-sp
#' 
#'
#' @examples
#'
#' library(survival)
#' data(SP_covid)
#' head(SP_covid)
#' 
#' time <- SP_covid$time_symptoms
#' cens <- SP_covid$censure
#' mod <- Surv(time,cens)
#' plot(mod)
#' 
#'
"SP_covid"
