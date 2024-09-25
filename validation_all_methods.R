##  Roy Burstein 
## 'Just ask' prompting approaches: ZS, FS (50, 400, 800), CoT, FT (FT model is trained elsewhere but run here in the same framework)


### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ SET UP


# Libraries
library(data.table)
library(tidyverse)
library(openai)
library(httr)
library(jsonlite)

# get the working directory
setwd('C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/code/AIAugmentedSurveyResponseCategorization')

# Set the API key
api_key <- readLines("./openaikey.txt") # note: file is gitignored
Sys.setenv(OPENAI_API_KEY = api_key)


# Load the data -- reasons given for non vacction
reasons_full <- fread("./ecv_reasons_novx.csv")       # note: file is gitignored (ask RB to share if needed)
reasons_full <- reasons_full[!is.na(roy_categorized)] # only validation set kept
reasons_full[reason == '', reason := 'NO RESPONSE']   # minor cleaning for valid input

# pull in system roles we will be using, run the r script
source("system_prompts.R")


### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### GET MODEL OUTPUTS FOR ALL TEST DATA!

# Note: test data is 800:999, training data is 1:799
testdataidx <- 800:999

# For all but FT run on stable version of gpt-4o
model <- "gpt-4o-2024-08-06"

# For FT Validation, this is the model that was fine tuned on 18-SEPT-2024:
ft_modname <- 'ft:gpt-4o-2024-08-06:gates-foundation::A8wDZ0kI'


### ------ 
### ------ ZERO SHOT
### No training examples

for(i in testdataidx){
  message(i)
  reasons_full[random_order==i, 
               zs_gpt4o_categorized := 
                 create_chat_completion(
                   model = model, 
                   temperature = 0, 
                   messages = list(
                     list('role' = 'system','content' = system_role),
                     list('role' = 'user',  'content' = reasons_full$reason[i])
                   ))$choices$message.content]
}

# write output (will do this after each one, just in case)
write.csv(reasons_full[random_order%in%testdataidx][, .(roy_categorized, zs_gpt4o_categorized)],paste0('validation_results_',today(),'.csv'))


### ------ 
### ------ CHAIN OF THOUGHT
for(i in testdataidx){
  message(i)
  reasons_full[random_order==i, 
               CoT_gpt4o_raw := 
                 create_chat_completion(
                   model = model, 
                   temperature = 0, 
                   messages = list(
                     list('role' = 'system','content' = system_role_COT),
                     list('role' = 'user',  'content' = reasons_full$reason[i])
                   ))$choices$message.content]
}

# get result out from CoT completion (its always after a semicolon ;)
reasons_full[, CoT_gpt4o_categorized := sapply(strsplit(CoT_gpt4o_raw, ";"), function(x) x[2])]
reasons_full$CoT_gpt4o_categorized # visually inspect to make sure that worked

# write output (will do this after each one, just in case)
write.csv(reasons_full[random_order%in%testdataidx][, .(roy_categorized, zs_gpt4o_categorized, CoT_gpt4o_categorized)],paste0('validation_results_',today(),'.csv'))



### ------ FINE TUNED
### Note, this model was fine-tuned in the finetune.R script

# do completion
for(i in testdataidx){
  message(i)
  reasons_full[random_order==i, 
               FT800_gpt4o_categorized := 
                 create_chat_completion(
                   model = ft_modname, 
                   temperature = 0, 
                   messages = list(
                     list('role' = 'system','content' = system_role),
                     list('role' = 'user',  'content' = reasons_full$reason[i])
                   ))$choices$message.content]
}

# write output (will do this after each one, just in case)
write.csv(reasons_full[random_order%in%testdataidx][, .(roy_categorized, zs_gpt4o_categorized, 
                                                        CoT_gpt4o_categorized, FT800_gpt4o_categorized)],paste0('validation_results_',today(),'.csv'))


### ------ 
### ------ FEW SHOT (WITH 50, 400, 800 TRAINING EXAMPLES),


# generic system role + FS
getFSsysrole <- function(original_role=system_role,n){
  tmp <- paste0('\nuser: ',reasons_full$reason, '\nassistant:', reasons_full$roy_categorized)
  paste0(
    original_role, '
    You are now going to be given ', n, ' examples of responses and their categorizations.  
    You will be asked to categorize the next response based on the examples you have seen. 
    Here are the examples:
    ',paste0(tmp[1:n],collapse=''))
}


# do completion for some few shots
for(nshot in c(20, 50, 400, 800)){
  message(paste0('-----------> SHOTS!', nshot))
  for(i in testdataidx){
    message(i)
    col_name <- paste0('FS', nshot, '_gpt4o_categorized')  
    reasons_full[random_order == i, 
                 (col_name) := 
                   create_chat_completion(
                     model = model, 
                     temperature = 0, 
                     messages = list(
                       list('role' = 'system','content' = getFSsysrole(system_role, nshot) ),
                       list('role' = 'user',  'content' = reasons_full$reason[i])
                     ))$choices$message.content]
  }
}

# write output (will do this after each one, just in case)
write.csv(reasons_full[random_order%in%testdataidx][, .(roy_categorized, zs_gpt4o_categorized, 
                                                        CoT_gpt4o_categorized, FT800_gpt4o_categorized,
                                                        FS20_gpt4o_categorized, FS50_gpt4o_categorized,
                                                        FS400_gpt4o_categorized, FS800_gpt4o_categorized)],
          paste0('validation_results_',today(),'.csv'))


### --- ---- --- ---
### --- TEST ZS with 1o mini,, just to see
for(i in testdataidx){
  message(i)
  reasons_full[random_order==i, 
               zs_o1mini_categorized := 
                 create_chat_completion(
                   model = 'o1-mini-2024-09-12', 
                   #temperature = 0, 
                   messages = list(
                     list('role' = 'user',  'content' = paste0(system_role,'User: ',reasons_full$reason[i],'Assistant: '))
                   ))$choices$message.content]
}


# zs accuracy
mean(reasons_full[random_order%in%testdataidx]$zs_o1mini_categorized == reasons_full[random_order%in%testdataidx]$roy_categorized)


### ------ 
### ------ VECTOR EMBEDDING APPROACH
## Note: this was done in a separate script (nlp_approach.R), just bringing in the results here. 

# load here
nlpval <- fread('./inputs/NLP_Validation_19SEPT2024_gpt4o.csv')

# prepare it to merge
nlpval$NLP_gpt4o_categorized <- as.character(nlpval$category_ai)
nlpval$random_order <- nlpval$rowid
nlpval <- nlpval[, .(random_order , NLP_gpt4o_categorized)]

# merge
reasons_full <- merge(reasons_full, nlpval, by='random_order', all.x=T)

# write output 
val_dat <- reasons_full[random_order%in%testdataidx][, .(roy_categorized, zs_gpt4o_categorized, 
                                                         CoT_gpt4o_categorized, FT800_gpt4o_categorized,
                                                         FS20_gpt4o_categorized, FS50_gpt4o_categorized,
                                                         FS400_gpt4o_categorized, FS800_gpt4o_categorized,
                                                         NLP_gpt4o_categorized, zs_o1mini_categorized)]
write.csv(val_dat, paste0('validation_results_',today(),'.csv'))






### ------ 
### ------ ACCURACY
### Validation for each of the variables in val_dat as table

# get accuracy
acc <- data.table(method = colnames(val_dat)[2:ncol(val_dat)])
for(m in acc$method)  acc[method == m, accuracy := mean(val_dat[[m]] == val_dat$roy_categorized)]
(acc <- acc[order(accuracy)])

# method accuracy
# 1:   NLP_gpt4o_categorized    0.615
# 2:    zs_gpt4o_categorized    0.715
# 3:   zs_o1mini_categorized    0.720
# 4:   CoT_gpt4o_categorized    0.735
# 5:  FS20_gpt4o_categorized    0.760
# 6:  FS50_gpt4o_categorized    0.795
# 7: FS400_gpt4o_categorized    0.815
# 8: FS800_gpt4o_categorized    0.830
# 9: FT800_gpt4o_categorized    0.835




### ------ 
### ------ SAVE DISCORDANT RESULTS IN A FILE SO I CAN REVIEW FOR ACC CEILING CALCULATION

# bring together reasons and results, include translations
val_dat <- fread('validation_results_2024-09-19.csv')
val_dat_long <- cbind(reasons_full[random_order%in%testdataidx,.(random_order,translation)], val_dat)

# make it long
val_dat_long <- melt(val_dat_long[,-c('V1')], id.vars = c('random_order','translation','roy_categorized'), 
                variable.name = 'method', value.name = 'ai_categorized')

# keep only discordancies
discord <- discordO[roy_categorized != ai_categorized]

# unique combos only to reduce work load
discord$method <- discord$random_order <- NULL
discord <- unique(discord)
nrow(discord)

# merge in category labels for the roy_ and ai_ categorized variables
response_options <- data.table(readxl::read_excel("response options.xlsx"))[,.(reason_category)]
response_options[, index := 1:.N]
discord <- merge(discord, response_options, by.x='roy_categorized', by.y='index', all.x=T)
setnames(discord, 'reason_category', 'roy_category')
discord <- merge(discord, response_options, by.x='ai_categorized', by.y='index', all.x=T)
setnames(discord, 'reason_category', 'ai_category')

# make and interactive ask for each discordancy to accept or reject the AI's categorization
run_accept_interactive <- FALSE
if(run_accept_interactive == TRUE){
  accept <- sapply(1:nrow(discord), function(i){
    message(paste0(i, ' of ', nrow(discord)))
    message(discord$translation[i])
    message(paste0('AI: ',discord$ai_category[i]))
    message(paste0('    <ROY: ',discord$roy_category[i],'>'))
    message('Accept? (1=yes, 0=no) (hold ENTER to end)')
    return(readline())
  })
  
  discord$accept <- as.numeric(accept)
  
  # save the discordant results with my decisions dated today
  write.csv(discord, paste0('discordant_results_RBaccept_',today(),'.csv'))
} else {
  discord <- fread('discordant_results_RBaccept_2024-09-25.csv')
}


## apply the accept criteria to get new accuracy ceilings for each method. 
new_val <- merge(val_dat_long, discord[,.(translation,roy_categorized,ai_categorized,accept)], by=c('translation','roy_categorized','ai_categorized'), all.x=T)
new_val[ai_categorized==roy_categorized, accept := 1]

new_val[method!='zs_o1mini_categorized',
        .(precice_accuracy = mean(ai_categorized == roy_categorized),
           accuracy_ceiling = mean(accept == 1)),
        by=method][order(precice_accuracy)]

