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
library(openai)

# colors and plotting set up
font_add("Garamond", "GARA.TTF")
showtext_auto()
theme_set(theme_classic() + theme(text = element_text(size=25,family = "Garamond")))

EGHCOLORS <- 
  c(white  = '#FFFFFF',
    teal   = '#0098A7',
    blue   = '#0C4456',
    orange = '#FF7700',
    dgrey  = '#4B4B4B',
    lgrey1 = '#768194',
    lgrey2 = '#E6EAEC',
    lgrey3 = '#F0F4F6',
    pink   = '#F9927D')
eghcl <- function(color) unname(EGHCOLORS[color])

IARCOLORS <-
  c(`Intent to vaccinate` = eghcl('blue'),
    `Community access`    = eghcl('teal'),
    `Facility readiness`  = eghcl('orange'),
    `Contraindication`    = 'pink',
    `Other`               = eghcl('lgrey1'))




# get the working directories
setwd('C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/code/AIAugmentedSurveyResponseCategorization')
ecvdir  <- 'C:/data/ecv/data' # data directory for ECV data
datdir  <- 'C:/Users/royb/OneDrive - Bill & Melinda Gates Foundation/VaccineResponseEmbeddingApproach'

# Set the API key
api_key <- readLines("./openaikey.txt") # note: file is gitignored
Sys.setenv(OPENAI_API_KEY = api_key)

# get system prompts for LLM
source('system_prompts.R')

# shapefile for making the map
pshp   <- readRDS('./inputs/province_shapefile.RDS')
zshp   <- readRDS('./inputs/hz_shapefile.RDS')


# load the possible categories
response_options <- data.table(readxl::read_excel("./inputs/response options.xlsx"))
response_options[, index := 1:.N]
response_options <- response_options[, .(index, reason_category)]

# load category standardizations
drc_to_std <- fread("./inputs/mapping_drc_std.csv")
std_to_gh  <- fread("./inputs/unique_demand_indicators_eghmapping_RB_edited.csv")

# standardize response options
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
response_options[, c_index := as.character(index)]



### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ ECV PREPPING


# load codebook
cb <- fread("./inputs/codebook_ecv.csv", header = T)
cb <- cb[names!=""]
cb[,var2023:=var2022] # these surveys are basically the same, for our purposes


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


## Mark Vaccination status; Zero-dose (Gavi definition of penta1==0)
labvxstat <- function(d){
  d[, zd := case_when(penta1_merg==1~0,penta1_merg==2~1)]
  d[, ui := as.numeric(penta1_merg==1&(var_merg==2|penta3_merg==2))]
  d[, vxstatus := 'Complete']; d[ui==1, vxstatus := 'Under-Immunized']; d[zd==1, vxstatus := 'Zero-Dose']
  return(d$vxstatus)
}
labvxstat20 <- function(d){
  d[, zd := case_when(penta1combinew==1~0,penta1combinew==0~1)]
  d[, ui := as.numeric(penta1combinew==1&(varcombinew==0|penta1combinew==0))]
  d[, vxstatus := 'Complete']; d[ui==1, vxstatus := 'Under-Immunized']; d[zd==1, vxstatus := 'Zero-Dose']
  return(d$vxstatus)
}
d20$vxstatus <- labvxstat20(d20)
d21$vxstatus <- labvxstat(d21)
d22$vxstatus <- labvxstat(d22)
d23$vxstatus <- labvxstat(d23)


## Get age
getage <- function(y){
  d <- get(paste0("d",y))
  var <- cb[names=='c_agem'][[paste0('var20',y)]]
  return(as.numeric(d[[var]]))
}
d20$age <- getage('20')
d21$age <- getage('21')
d22$age <- getage('22')
d23$age <- getage('23')

# GEOGRAPHY
cb[grepl('prov|zone',names),]
getvar <- function(var,y){
  d <- get(paste0("d",y))
  var <- cb[names==var][[paste0('var20',y)]]
  return(d[[var]])
}
d20$ZS <- getvar('zone','20')
d21$ZS <- getvar('zone','21')
d22$ZS <- getvar('zone','22')
d23$ZS <- getvar('zone','23')
d20$province <- getvar('province','20')
d21$province <- getvar('province','21')
d22$province <- getvar('province','22')
d23$province <- getvar('province','23')

# Interviewer
d20[, interviewer := paste0('ecv2020 / ', enqueteur)]
d21[, interviewer := paste0('ecv2021 / ', q110)]
d22[, interviewer := paste0('ecv2022 / ', q110)]
d23[, interviewer := paste0('ecv2023 / ', q110)]






### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ APPLY LLM-CATEGORIZATION TO FULL SURVEY DATA

# set this if you've run this before and want to just load the results to save on API costs
pre_loadcategories <- TRUE

if(pre_loadcategories == FALSE){
  
  # get list of unique reasons given across all surveys
  all_reasons_categorized <- data.table(reason = unique(c(d20$reason, d21$reason, d22$reason, d23$reason)),
                                        ai_categorized = '')
  all_reasons_categorized[reason == '', reason := 'NO RESPONSE']   # minor cleaning for valid input
  all_reasons_categorized <- all_reasons_categorized[!is.na(reason)]
  all_reasons_categorized[, idx := 1:.N]
  
  # apply the categorization to each response 
  # we will use the fine-tuned model for this as it had the best accuracy
  ft_modname <- 'ft:gpt-4o-2024-08-06:gates-foundation::A8wDZ0kI'
  
  for(i in all_reasons_categorized$idx){
    message(sprintf("Processing reason %d of %d", i, nrow(all_reasons_categorized)))
    
    all_reasons_categorized[idx==i, 
                 ai_categorized := 
                   create_chat_completion(
                     model = ft_modname, 
                     temperature = 0, 
                     messages = list(
                       list('role' = 'system','content' = system_role),
                       list('role' = 'user',  'content' = all_reasons_categorized[idx==i]$reason)
                     ))$choices$message.content]
  }
  
  write.csv(all_reasons_categorized, "./inputs/all_reasons_categorized.csv")

} else {
  all_reasons_categorized <- fread("./inputs/all_reasons_categorized.csv")
}






### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ MERGE ALL ECVs INTO ONE DATASET


# merge in the categorized reasons to the full list of responses
kv <- c('survey','reason', 'age', 'vxstatus','province','ZS','rowid')
all_d <- rbind(d20[,kv,with=F], d21[,kv,with=F], d22[,kv,with=F], d23[,kv,with=F])[!is.na(reason)&reason!=""]

all_d <- merge(all_d, all_reasons_categorized[,.(reason,ai_categorized)], by='reason', all.x=TRUE)

# merge with reason labels
all_d <- merge(all_d, response_options, by.x='ai_categorized', by.y='c_index', all.x=TRUE)


all_d[,freason_category := factor(reason_category, 
                                  levels = rev(response_options[order(index)]$reason_category))]




### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ REPLICATE ORIGINAL PLOT WITH OTHER COLORED IN NOW




## -- COMPARE 'OTHER' WITH MARKED CATEGORIES
# note. differences in categories across surveys need to be minded
# start with 2021 and 2022 since they are set up easy and the same
# up to reason 22 they are the same, Other (24==30), and 23==22 (repeat)
reas_cited <- rbind(d21[,.(survey,province,ZS,rowid,vs102,age,vxstatus,interviewer,vs89,qa206,reason)], 
                    d22[,.(survey,province,ZS,rowid,vs102,age,vxstatus,interviewer,vs89,qa206,reason)],
                    d23[,.(survey,province,ZS,rowid,vs102,age,vxstatus,interviewer,vs89,qa206,reason)])
rco <- copy(reas_cited)
reas_cited[vs102==23, vs102 := 22]
reas_cited[vs102==24, vs102 := 30]
reas_cited <- reas_cited[!is.na(vs102)]

reas_cited <- merge(reas_cited, all_reasons_categorized[,.(ai_categorized,reason)], by='reason', all.x = TRUE)
reas_cited[,OTHER := ifelse(vs102==30,'Free-Text','Selected')]
reas_cited[OTHER=='Free-Text', vs102 := ai_categorized]; reas_cited$ai_categorized <- NULL
reas_cited[, OTHER := factor(OTHER, levels = c('Selected','Free-Text'))]
reas_cited <- merge(reas_cited, response_options[,.(index,reason_category,EGH.domain)], 
                    by.x = "vs102", by.y = "index", all.x = TRUE)[order(rowid)]

reas_cited[,freason_category := factor(reason_category, 
                                       levels = rev(response_options[order(index)]$reason_category))]

png("./figs/reasons_zd_BOTH.png", width=10, height=6, units='in', res=300)
ggplot(reas_cited[!is.na(freason_category)& vxstatus=='Zero-Dose']) +
  geom_bar(aes(y = freason_category, fill = EGH.domain)) +  
  labs(title = "Reasons for Non-Vaccination (Zero-Dose, ECV2021-23)",
       x = "", y = "", fill = 'EGH Domain') +
  facet_wrap(.~OTHER, scales = 'free_x')+
  theme(axis.text = element_text(size=30, lineheight = 0.2)) +  
  scale_fill_manual(values = IARCOLORS)
dev.off()

# replicate plot sowing other as most cat
x<- copy(reas_cited)[vxstatus=='Zero-Dose'] #&survey=='ecv2021'&age>=12]
x[OTHER=='Free-Text', freason_category := 'Other']
x[, freason_category := str_wrap(freason_category, width = 50)]
x[, freason_category := gsub('\\[\\d+\\]','',freason_category)]
x[,freason_category := factor(freason_category, 
                              levels = x[,.(n=.N), by=freason_category][order(n)]$freason_category)]
x <- x[,.(n=.N), by=.(EGH.domain,freason_category)]
x[,pct:=n/sum(n)]

# show as percentage in x axis
png("./figs/reasons_zd_all.png", width=10, height=6, units='in', res=300)
ggplot(x[!is.na(freason_category)]) +
  geom_bar(aes(y = freason_category, x=pct*100, fill = EGH.domain),
           stat='identity',position='stack') +
  theme(axis.text = element_text(size=35, lineheight = 0.2)) +  
  labs(title = "Cited Reasons for Non-Vaccination (ZD ECV2021-23)",
       x = "Percent of Responses", y = "", fill = 'EGH Domain') +
  scale_fill_manual(values = IARCOLORS)
dev.off()


### Note: 2020 survey is set up differently, different base variable options
###       not including it in this plot for now. 
names(d20)[grepl('qa4013',names(d20))]

### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ PLOT DISTRIBUTION OF CATEGORIZED REASONS


# plot the distribution of reasons
png("./figs/other_reasons_distribution_zd.png", width=10, height=6, units='in', res=300)
ggplot(all_d[!is.na(reason_category) & vxstatus=='Zero-Dose']) + 
  geom_bar(aes(y = freason_category, fill = EGH.domain)) +
  theme(text = element_text(size=37)) +
  labs(title = "Categorized 'other' Reasons for ZD",
       x = "Number of responses among ZD households", 
       y = "", fill = 'EGH Domain') +
  scale_fill_manual(values = IARCOLORS)
dev.off()

png("./figs/other_reasons_distribution_zd.png", width=10, height=6, units='in', res=300)
ggplot(all_d[!is.na(reason_category) & vxstatus=='Zero-Dose' & survey!='ecv2020']) + 
  geom_bar(aes(y = freason_category, fill = EGH.domain)) +
  theme(text = element_text(size=37)) +
  labs(title = "Categorized 'other' Reasons for ZD (ECV 2021-23)",
       x = "Number of responses among un- or under-vaccinated households", 
       y = "", fill = 'EGH Domain') +
  scale_fill_manual(values = IARCOLORS)
dev.off()

# how many are in new categories
mean(all_d$ai_categorized>22,na.rm=T) # 89.5%
mean(all_d[vxstatus=='Zero-Dose' & survey!='ecv2020']$ai_categorized>22 ,na.rm=T) # 82%








### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------ 
### ------ REASONS OVER TIME


## Line PLOTS,, GR
all_d[ai_categorized %in% c(24,27,31), r := 'Covid-related']
all_d[ai_categorized %in% c(32),       r := 'Too young to have\ncompleted schedule']
all_d[ai_categorized %in% c(23),       r := 'Provider strike']
all_d[is.na(r), r:='OTHER']

tmp <- all_d[!is.na(r), # & vxstatus=='Zero-Dose',
            .(n=.N), by = .(survey,r)]
tmp[,pct:=n/sum(n), by=survey]

png("./figs/other_reasons_change_time.png", width=10, height=6, units='in', res=300)
ggplot(tmp[r!='OTHER'],
       aes(x = survey, y= pct,  group = r,color=r)) +
  geom_line( lwd = 5, lineend = 'round',alpha=0.7) +
  geom_point(size=5) +
  labs(color='',
       x = "", y = "Percent of 'other' Responses among un- or under-vaccinated") +
  facet_wrap(.~r, scales='free_y') +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,NA)) +
  theme(legend.position = 'none', 
        text = element_text(size=50)) +
  # reduce linespacing in facet labels
  scale_color_manual(values = c('#8E3E63','#003285','#799351')) 
dev.off()




### ------ ------ ------ ------ ------ ------ ------ ------ ------ ------
### ------ MAP OF CATEGORIZED REASONS

# Define a health zone mapping function
maphz <- function(var        = '', 
                  label      = '',
                  data       = d,
                  collapse   = TRUE,
                  provtext   = TRUE,
                  lims       = c(0,NA),
                  lowcolor   = 'white', 
                  highcolor  = 'magenta',
                  naval      = 'lightgrey'){
  
  data$var <- data[[var]]
  
  if(collapse == TRUE){
    tmp <- data[age>11,.(var=mean(var,na.rm=T),n=.N),by=.(province,ZS)]
    
    if(provtext==TRUE){
      tmp1 <- data[age>11, .(pct=round(mean(var),3)*100), by = province]
      tmp1 <- merge(pshp,tmp1,by='province',all.x=T)
      pts <- tmp1 %>% 
        mutate(x = purrr::map_dbl(geometry, ~st_centroid(.x)[[1]]),
               y = purrr::map_dbl(geometry, ~st_centroid(.x)[[2]]))
    }
  } else {
    tmp <- copy(data)
  }
  
  if(!is.na(lims[2])) tmp[var>lims[2],var := lims[2]]
  
  tmp <- merge(zshp,tmp,by='ZS',all.x=T)
  
  g=ggplot(tmp) +
    geom_sf(aes(fill=var),color=NA,lwd=0.1) +
    geom_sf(data=pshp,fill=NA,color='#5e5e5e',lwd=0.2) +
    
    scale_fill_gradient(low=lowcolor,high=highcolor,
                        label=scales::percent,
                        limits=lims,
                        name='', #str_wrap(label,12), 
                        na.value = naval) +
    theme(axis.ticks = element_blank(),
          axis.line  = element_blank(),
          axis.text  = element_blank(),
          text       = element_text(size=35)) +
    ggtitle(label) +
    xlab('') + ylab('')
  
  if(collapse==TRUE & provtext==TRUE)
    g <- g + geom_text(data=pts, 
                       aes(x,y,label=paste0(round(pct),'%')), size = 5)
  
  print(g)
  
} 


png("./figs/map_conflic.png", width=4, height=4, units='in', res=300)
all_d[, test := as.numeric(ai_categorized=='26')]
maphz(data=all_d, var='test', 
      label='Percent citing conflict as main reason for un-/under-vaccination', 
      provtext = F,
      highcolor=eghcl('teal'),naval='white')
dev.off()



