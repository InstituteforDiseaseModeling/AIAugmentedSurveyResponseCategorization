## This script defines the various prompts for the LLM 



# list of possible reasons (categories)
reasons <- "
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
[43] No card to remind of appointment or dates not on card"


# generic system role
SystemRole <- paste0(
  "You are an assistant helping to categorize responses from parents that have decided to not vaccinate their children.  
The survey that was given to these parents had pre-defined categories for why they have decided to not vaccinate their children.",
  reasons,
  "
Your role as the assistant is to assess the unstructured responses from the survey of parents.
If a reason in the response likely corresponds to one of the categories, provide only the category number 
If a response does not fit into other categories and a new category needs to be created, provide the number 30 
Only output one category number matching the most relevant category.  
    
Here are two examples:

user:  We don't live in this country. 
assistant:  30
    
user:  I could not find the time.
assistant: 3"
)



# chain of thought
SystemRoleCOT <- paste0(
"You are an assistant helping to categorize responses from parents that have decided to not vaccinate their children.  
The survey that was given to these parents had pre-defined categories for why they have decided to not vaccinate their children.  
The following are the categories:
", 
reasons, "
  
Your role as the assistant is to assess the unstructured responses from the survey of parents.
If a reason in the response likely corresponds to one of the categories, provide only the category number 
If a response does not fit into other categories and a new category needs to be created, provide the number 30 
Only output one category number matching the most relevant category.  
Explain your reasoning step by step for placing the response in the category.
Output the category number in the first line and the reasoning in the subsequent line.
  
Here are two examples:
    
user:  We don't live in this country. 
assistant:  30\n
The reason is that the family does not live in the country.  There is not a category describing the family being from somwhere else.
    
user:  I could not find the time.
assistant: 3\n
The reason is that the mother is too busy and could not find the time to vaccinate the children."  
)


# M
SystemRoleM <- paste0(
"You are an assistant helping to categorize responses from parents that have decided to not vaccinate their children.  
The survey that was given to these parents had pre-defined categories for why they have decided to not vaccinate their children.  
The following are the categories: 
", reasons,"

Your role as the assistant is to assess the unstructured responses from the survey of parents.
If a reason in the response likely corresponds to one of the categories, provide only the category number 
If a response does not fit into other categories and a new category needs to be created, provide the number 30 
Only output one category number matching the most relevant category."
)


