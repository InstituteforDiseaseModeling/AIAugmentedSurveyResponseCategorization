## Roy Burstein
## Fine tune with gpt-4o


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
datdir <- 'C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/VaccineResponseEmbeddingApproach'

# Set the API key
api_key <- readLines("./openaikey.txt") # note: file is gitignored
Sys.setenv(OPENAI_API_KEY = api_key)


# Load the data -- reasons given for non vacction
reasons_full <- fread("./ecv_reasons_novx.csv") # note: file is gitignored (ask RB to share if needed)
reasons_full <- reasons_full[!is.na(roy_categorized)] # only validation set kept
reasons_full[reason == '', reason := 'NO RESPONSE'] # minor cleaning for valid input

# load the input -- all category options (pre-determined set of 43)
response_options <- data.table(readxl::read_excel("response options.xlsx"))
response_options[, index := 1:.N]



### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ DEFINE SYSTEM PROMPT

system_prompt <-
"You are an assistant helping to categorize responses from parents that have decided to not vaccinate their children.  
The survey that was given to these parents had pre-defined categories for why they have decided to not vaccinate their children.  
The following are the categories: 
[1] Vaccination site too far 
[2] Vaccination schedule not known 
[3] mother too busy 
[4] Family problems, including maternal illness 
[5] Sick child, not sent
[6] Sick child, sent but not vaccinated
[7] Long wait
[8] Rumors
[9] Don't believe in vaccination
[10] Fear of side effects
[11] Site and/or time of vaccination not known
[12] Ignore the need for vaccination
[13] Ignores need to return for 2nd or 3rd dose
[14] Bad ideas about contraindications
[15] Inappropriate timing of vaccination
[16] Absent vaccinator
[17] Vaccine not available
[18] Vaccination session canceled
[19] High cost of vaccination or SMC session
[20] Sick child, brought in but did not receive vaccination
[21] Religious censorship
[22] Negative attitude of the spouse, father or guardian of the child towards vaccination
[23] Provider on strike
[24] COVID-19 lockdown
[25] Travel or displacement
[26] War, armed conflict, ethnic conflict
[27] Fear of COVID vaccine
[28] Don't know the reason
[29] Vaccine not mentioned on card
[30] Other
[31] COVID-19 (fear of catching, or vague)
[32] Child too young to have completed schedule
[33] Vague or unclear response
[34] Working in the field/agricultural work
[35] Claims to have received all vaccines afterall
[36] Vaccine-related items not available (syringe, cards)
[37] Forgot
[38] Insufficient number of children at session 
[39] 'Negligence' 
[40] Born in a different place/home, possible inability to vaccinate related to that
[41] Missed birth dose
[42] Issues with health workers (negatve experience, no reminder, and distrust)
[43] No card to remind of appointment or dates not on card

Your role as the assistant is to assess the unstructured responses from the survey of parents.
If a reason in the response likely corresponds to one of the categories, provide only the category number 
If a response does not fit into other categories and a new category needs to be created, provide the number 30 
Only output one category number matching the most relevant category.  
    
Here are two examples:

user:  We don't live in this country. 
assistant:  30
    
user:  I could not find the time.
assistant: 3
"

### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ CLEAN TRAINING DATA
### Note: must be in JSON Format 
###       ex: {"prompt": "Translate the following English text to French:\n\nEnglish: Hello, how are you?\nFrench:", "completion": " Bonjour, comment ça va ?"}
###       Training set (first 800 of reasons file)



# Function to escape special characters in JSON
escape_json <- function(text) {
  text <- gsub('\\', '\\\\', text, fixed = TRUE)  # Escape backslashes
  text <- gsub('"', '\\"', text, fixed = TRUE)    # Escape double quotes
  text <- gsub('\n', '     ', text, fixed = TRUE)   # Replace newlines with \n
  text <- gsub('\r', '\\r', text, fixed = TRUE)   # Replace carriage returns with \r
  text <- gsub('\t', '\\t', text, fixed = TRUE)   # Replace tabs with \t
  return(text)
}

# Create JSONL entries
reasons_full[, jsonl := sprintf(
  '{"messages": [{"role": "system", "content": "%s"},{"role": "user", "content": "%s"},{"role": "assistant", "content": "%s"} ]}',
  escape_json(system_prompt),
  escape_json(reason),
  escape_json(roy_categorized)
)]

# reasons_full[, jsonl := sprintf(
#   '{"prompt": "%s", "completion": "%s"}',
#   reason, roy_categorized
# )]

# Write to JSONL file
trainingdata_filepath <- "training_data.jsonl"
writeLines(reasons_full[random_order%in%1:800]$jsonl, con = trainingdata_filepath)




### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ UPLOAD TRAINING DATA FILE

base_url <- "https://api.openai.com/v1"

# Upload the file
response <- POST(
  url = paste0(base_url, "/files"),
  add_headers(`Authorization` = paste("Bearer", api_key)),
  body = list(
    purpose = "fine-tune",
    file = upload_file(trainingdata_filepath, type = "application/jsonl")
  ),
  encode = "multipart"
)


# Handle the response
if (status_code(response) == 200) {
  file_info <- content(response, as = "parsed")
  file_id <- file_info$id
  cat("File uploaded successfully. File ID:", file_id, "\n")
} else {
  stop("File upload failed: ", content(response, "text"))
}




### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ INITIATE FINE-TUNING

# Define the model to fine-tune
model_to_finetune <- "gpt-4o-2024-08-06"

# Start fine-tuning
fine_tune_response <- POST(
  url = paste0(base_url, "/fine_tuning/jobs"),
  add_headers(`Authorization` = paste("Bearer", api_key)),
  body = toJSON(list(
    training_file = file_id,
    model = model_to_finetune #,
    #n_epochs = 4,  
    #prompt_loss_weight = 0.01
  ), auto_unbox = TRUE),
  encode = "json"
)

# Handle the response
if (status_code(fine_tune_response) == 200) {
  fine_tune_info <- content(fine_tune_response, as = "parsed")
  fine_tune_id <- fine_tune_info$id
  cat("Fine-tuning started successfully. Fine-tune ID:", fine_tune_id, "\n")
} else {
  stop("Fine-tuning initiation failed: ", content(fine_tune_response, "text"))
}

## JOB ID: ftjob-hp04OVTdpAbPyf1VoZ0v70fW 

# Monitor progress
check_fine_tune_status <- function(fine_tune_id, api_key) {
  status_response <- GET(
    url = paste0(base_url, "/fine_tuning/jobs/", fine_tune_id),
    add_headers(`Authorization` = paste("Bearer", api_key))
  )
  
  if (status_code(status_response) == 200) {
    status_info <- content(status_response, as = "parsed")
    return(status_info)
  } else {
    stop("Failed to fetch fine-tune status: ", content(status_response, "text"))
  }
}

repeat {
  status_info <- check_fine_tune_status(fine_tune_id, api_key)
  status <- status_info$status
  cat("Current status:", status, "\n")
  if (status %in% c("succeeded", "failed")) {
    break
  }
  Sys.sleep(10)  # Wait for some seconds before checking again
}

if (status == "succeeded") {
  fine_tuned_model <- status_info$fine_tuned_model
  cat("Fine-tuning succeeded. Model:", fine_tuned_model, "\n")
} else {
  cat("Fine-tuning failed. Details:", status_info$error$message, "\n")
}






