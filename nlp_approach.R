## Roy Burstein
## LLM Categorization using embedding vectors and clustering, with LLM to name the clusters


### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ SET UP

# Libraries
library(data.table)
library(tidyverse)
library(openai)
library(dbscan)
library(mclust)

# get the working directory
setwd('C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/code/AIAugmentedSurveyResponseCategorization')
datdir <- 'C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/VaccineResponseEmbeddingApproach'

# Set the API key
Sys.setenv(OPENAI_API_KEY = readLines("./openaikey.txt")) # note: file is gitignored

# Load the data -- reasons given for non vacction
reasons_full <- fread("./ecv_reasons_novx.csv") # note: file is gitignored (ask RB to share if needed)
reasons_full <- reasons_full[!is.na(roy_categorized)] # only validation set kept
reasons_full[reason == '', reason := 'NO RESPONSE'] # minor cleaning for valid input

# load the input -- all category options (pre-determined set of 43)
response_options <- data.table(readxl::read_excel("response options.xlsx"))
response_options[, index := 1:.N]


### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ EMBEDDINGS
## extract or pre-load the embeddings for each response (reason given)

preloaded_embeddings <- TRUE
if(preloaded_embeddings==FALSE){
  
  # extract the embedding from openai. using the small model for now
  e <- 
    create_embedding(
      model = "text-embedding-3-small", # OR "text-embedding-3-large"
      input = reasons_full$reason
    )
  
  # munge the output string split the embedding column on comma  
  embasmatrix <- function(em){
    veclength <- length(gregexpr(",", em[1])[[1]]) + 1 # 1536 for short ones
    em[,paste0('e',1:veclength):=tstrsplit(embedding,split=', ')]
    em[, e1 := substr(e1,3,nchar(e1))]
    em[[paste0('e',veclength)]] <- gsub(')','',em[[paste0('e',veclength)]])
    em[,embedding:=NULL]
    em[] <- lapply(em, as.numeric)
    return(em)
  }
  em <- embasmatrix(as.data.table(e$data[3]))
  
  saveRDS(em, file.path(datdir,'embeddingsSmall_validationset_27APR2024.rds'))
} else {
  em <- readRDS(file.path(datdir,'embeddingsSmall_validationset_27APR2024.rds'))
}

# em is a list of 1536 columns, each a numeric vector of length 1000, cbind them
em <- as.data.table(do.call(cbind,em))




### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ ASSIGN CLUSTERS
### Note: you can choose number of clusters and method here
### we pre-tested and chose to go with GMM and 200 clusters for the final run

# set method and number of clusters
set.seed(12345)
cl_method <- 'gmm' # can be 'kmeans', 'hier', 'gmm'
nclusts   <- 200   # can be any number

# Set up functions for various cluster approaches
kmeans_clust <- function(dat, k){  kmeans(dat, centers=k, iter.max=1000)$cluster }
hier_clust   <- function(dat, k){  hclust(dist(dat), method='ward.D2') %>% cutree(k) }
gmm_clust    <- function(dat, k){  Mclust(dat, G=k)$classification } 

# Run the clustering for the 3 different methods for each nclust
# Note, we are running on the full embeddings, but PCA first is a plausible way to speed it up
# Runs in 20 seconds or so on the full embeddings
clusts <- data.table(rowid    = reasons_full$random_order)
clusts$cluster <- get(paste0(cl_method,'_clust'))(em, nclusts)
clusts$method  <- cl_method
clusts$nclusts <- nclusts

# this will be done for each cluster, so we set up a function to call here. 
# Note that we will use up to 25 examples per cluster to get the label
labelcluster_gpt <- 
  function(clustervar,cl,
           llm_mod,
           dat=reasons_full,
           TEMP=0,
           seed=123,
           nexamples=25){
    
    # Get the cluster
    tmp <- dat[which(clustervar==cl)]
    if(nrow(tmp)<nexamples)  nexamples <- nrow(tmp)
    
    examples <- paste0(sample(tmp$reason,nexamples),
                       collapse = '; ')
    
    # Create a prompt
    sysrole <- 'You are an assistant who looks at example responses to a survey 
    question and describes their common thematic meaning.
    
    The survey was conducted in DRC and asked parents why their child did not receive all their recommended vaccines.

    I will provide you a list of survey responses and a comprehensive list of thematic labels. 
    You can only choose from one of these. 
    
    You will only return the number associated with the chosen label, nothing else.'
    
    prompt <- 'Responses have been pre-screened and should all fit under one cohesive theme.  
    Here are the example responses, separated by semicolons:
   '
    prompt <- paste0(prompt,paste0(examples,collapse='; '),collapse = '\n')
    prompt <- paste0(prompt,'
                   Here is a list of possible labels separated by semicolns:
                   ',collapse='\n')
    prompt <- paste0(prompt, paste0(response_options$reason_category,collapse='; '),collapse = '\n')
    
    # ask gpt
    response <- 
      create_chat_completion(
        temperature = TEMP, 
        model = llm_mod,
        messages = list(
          list(
            'role'='assistant',
            'content'=sysrole
          ),
          list(
            'role'='user',
            'content'=prompt
          ))
      )
    message(paste('TOTAL TOKENS: ',response$usage$total_tokens))
    
    return(response$choices$message.content)
  }


# set llm model to use
llm_model <- 'gpt-4o'

# loop through all clusters and label them using the function
preload_catassign <- FALSE # re-run or preload?
if(preload_catassign == FALSE){
  cat_assiged <- data.table() 
  for(meth in c(cl_method)){
    message(paste0('METHOD:', meth))
    tmp <- data.table(method = meth)
    for(nclust in nclusts){
      message(paste0('.... NUMBER OF CLUSTERS:', nclust))
      tmp[, nclusts := nclust]
      for(clnum in 1:nclust){
        message(paste0('.... .... CLUSTER NUMBER:', clnum))
        set.seed(12345)
        tmp[, cluster:=clnum]
        tmp$category_ai <-  
          as.character(labelcluster_gpt(clustervar=clusts[method==meth&nclusts==nclust]$cluster,
                                        cl=clnum,
                                        llm_mod=llm_model,
                                        TEMP=0))
        cat_assiged <- rbind(cat_assiged,tmp)
      }
    }
  }
  # clean possible brackets that sometimes appear in outputs
  cat_assiged[,category_ai:=as.numeric(gsub('\\[|\\]','',category_ai))]
  cat_assiged$llm <- llm_model
  saveRDS(cat_assiged, file.path(datdir,paste0('tmp/cat_assiged_13SEPT2024',llm_model,'.rds')))
} else {
  cat_assiged <- readRDS(file.path(datdir,paste0('tmp/cat_assiged_13SEPT2024',llm_model,'.rds')))
}






### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ VALIDATION CHECK
### Merge back the AI-assigned categories with the Human-assigned categories and get accuracy

# merge and write output
catcompare <- merge(clusts, reasons_full[,c('random_order','roy_categorized','roy_notableresponse')], 
                    by.x='rowid', by.y='random_order')
catcompare <- merge(catcompare, cat_assiged, by = c('method','nclusts','cluster'))
write.csv(catcompare, 'NLP_Validation_RB13SEPT2024o1mini.csv')


# get accuracy
mean(catcompare$roy_categorized == catcompare$category_ai)
mean(catcompare[rowid>800]$roy_categorized == catcompare[rowid>800]$category_ai)




