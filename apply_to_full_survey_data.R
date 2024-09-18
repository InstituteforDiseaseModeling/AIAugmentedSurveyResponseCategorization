## Roy Burstein
## Apply results of LLM-categorization to full survey data and plot 



### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ SET UP

# load libraries
library(tidyverse)
library(data.table)
library(readxl)
library(sysfonts)
library(sf)
library(showtext)

# get the working directories
setwd('C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/code/AIAugmentedSurveyResponseCategorization')
ecvdir  <- 'C:/data/ecv/data' # data directory for ECV data
datdir  <- 'C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/VaccineResponseEmbeddingApproach'

# shapefile for making the map
pshp   <- readRDS('./inputs/province_shapefile.RDS')

# load in the three survey datasets
d20 <- readRDS(file.path(ecvdir, "ECV 2020/Base_ECV_Finale.RDS"))
d21 <- readRDS(file.path(ecvdir, "ECV 2022/dataset/ECV_2021_Enfants.RDS"))
d22 <- readRDS(file.path(ecvdir, "ECV 2023/ECV_2022_Enfants.RDS"))
d20 <- as.data.table(d20)
# Note: ecv 2023 was completed after our initial study, so added here but not included in validation data
d23 <- as.data.table(haven::read_dta(file.path(ecvdir, "ECV 2024/ECV_2023_Vaccination_V4_Menages_Mere_Enfants_Dataset.dta")))

d20[, `:=` (survey='ecv2020',rowid=paste0("ecv2020_", 1:.N))]
d21[, `:=` (survey='ecv2021',rowid=paste0("ecv2021_", 1:.N))]
d22[, `:=` (survey='ecv2022',rowid=paste0("ecv2022_", 1:.N))]
d23[, `:=` (survey='ecv2023',rowid=paste0("ecv2023_", 1:.N))]

d20[, reason := qa401311_autre]
d21[, reason := vs102_autre]
d22[, reason := vs102_autre]
d23[, reason := vs102_autre]

# load codebook
cb <- fread("./inputs/codebook_ecv.csv", header = T)
cb <- cb[names!=""]
cb[,var2023:=var2022]

# load the possible categories
response_options <- data.table(readxl::read_excel("./inputs/response options.xlsx"))
response_options[, index := 1:.N]
response_options <- response_options[, .(index, reason_category)]

# load category standardizations
drc_to_std <- fread("./inputs/mapping_drc_std.csv")
std_to_gh  <- fread("./inputs/unique_demand_indicators_eghmapping_RB_edited.csv")

# read in results of LLM categorization
 # these are the llm results from the full model run from josh
 # TODO: Give josh the new data from d23
llmcat   <- fread("./inputs/FT_Predictions.csv")
fullresp <- fread('./inputs/ecv_reasons_novx.csv') # ecv_reasons_novx_with_ecv2023.csv
llmcat   <- merge(llmcat,fullresp[,.(survey,random_order,reason)],by='random_order',all.x=T)
setnames(llmcat,'FT_Category','ai_cat')


 
for(i in 1:nrow(newresp)){
  message(i)
  if(newresp$translation[i]==''){
    newresp$translation[i] <- 
    create_chat_completion(
      model = 'gpt-4o', temperature = 1,
      messages = list(
        list('role' = 'system','content' = 'You are a French to English translator, only provide translations.'),
        list('role' = 'user',  'content' = newresp$reason[i])
      ))$choices$message.content
  }
}

write.csv(newresp,'./inputs/ecv_reasons_novx_with_ecv2023.csv')

### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ CLEANING/PREP

# standardize and label category options according to EGH framework
response_options <- merge(response_options, drc_to_std[,.( Correlated.Standardized.Reason,drc_reason_cat)], 
                          by.x='index', by.y = "drc_reason_cat", all.x = TRUE)
response_options <- merge(response_options, std_to_gh[,.(EGH.domain,Correlated.Standardized.Reason)], 
                          by='Correlated.Standardized.Reason', all.x = TRUE)

response_options[, EGH.domain := factor(EGH.domain, levels = 
                                          c('Intent to vaccinate',
                                            'Community access',
                                            'Facility readiness',
                                            'Contraindication',
                                            'Other'))]


