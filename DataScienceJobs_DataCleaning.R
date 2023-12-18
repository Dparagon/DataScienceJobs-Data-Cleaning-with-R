             --- Data Science Jobs Data Cleaning ---

# DATA PREPARATION
setwd("D:/R Activity")

# Loading libraries
library(tidyverse)
library(janitor)

#-- Loading data
datascience_jobs <- read_csv("DS_jobs_uncleaned.csv")

View(datascience_jobs)

str(datascience_jobs)


# DATA CLEANING

#-- Formatting column headers
datascience_jobs <- datascience_jobs %>%  clean_names()

#--  Job Title column
datascience_jobs %>% select(job_title) %>% distinct()

datascience_jobs <- datascience_jobs %>%
             mutate(job_title = str_replace(job_title, '\\(Sr\\.\\) Data Scientist', 'Senior Data Scientist'),
                    job_title = str_replace(job_title, 'Health Plan Data Analyst, Sr', 'Health Plan Data Analyst Senior'),
                    job_title = str_replace(job_title, 'Jr\\. Business Data Analyst \\(position added 6/12/2020\\)', 'Junior Business Data Analyst'),
                    job_title = str_replace(job_title, 'Jr\\. Data Engineer', 'Junior Data Engineer'),
                    job_title = str_replace(job_title, 'Machine Learning Engineer, Sr\\.', 'Machine Learning Engineer Senior'),
                    job_title = str_replace(job_title, 'Sr Data Analyst', 'Senior Data Analyst'),
                    job_title = str_replace(job_title, 'Sr Data Engineer \\(Sr BI Developer\\)', 'Senior Data Engineer (Senior BI Developer)'),
                    job_title = str_replace(job_title, 'Sr Data Scientist', 'Senior Data Scientist'),
                    job_title = str_replace(job_title, 'Sr Scientist - Extractables & Leachables', 'Senior Scientist - Extractables & Leachables'),
                    job_title = str_replace(job_title, 'Sr Data Analyst', 'Senior Data Analyst'),
                    job_title = str_replace(job_title, 'Sr\\. ML/Data Scientist - AI/NLP/Chatbot', 'Senior ML/Data Scientist - AI/NLP/Chatbot'),
                    job_title = str_replace(job_title, 'Sr\\. Research Associate/ Scientist, NGS prep & Molecular Genomics',
                                                       'Senior Research Associate/ Scientist, NGS prep & Molecular Genomics'),
                    job_title = str_replace(job_title, 'ELISA RESEARCH SCIENTIST \\(CV-15\\)', 'Elisa Research Assistant'),
                    job_title = str_replace(job_title, 'ENGINEER - COMPUTER SCIENTIST - RESEARCH COMPUTER SCIENTIST - SIGNAL PROCESSING - SAN ANTONIO OR',
                                                       'Engineer - Computer Scientist'))
#-- Salary Estimate column
datascience_jobs %>% select(salary_estimate) %>% distinct() 

datascience_jobs <- datascience_jobs %>%
                    mutate(salary_estimate = str_sub(salary_estimate, 1, 11),
                           salary_estimate = gsub("\\(","",salary_estimate))

#-- Company Name column
datascience_jobs %>% select(company_name) %>% distinct() 


datascience_jobs <- datascience_jobs %>%
                    mutate(company_name = str_sub(company_name, 1, nchar(company_name) - 4))

#-- Size column
datascience_jobs %>% select(size) %>% distinct() 

datascience_jobs <- datascience_jobs %>%
                    mutate(size = ifelse(size == '-1', 'Unknown', gsub('to', '-', size)))


#-- Type of Ownership column
datascience_jobs %>% select(type_of_ownership) %>% distinct() 

datascience_jobs <- datascience_jobs %>%
                    mutate(type_of_ownership = case_when(type_of_ownership == '-1' ~ 'Unknown',
                                                         type_of_ownership == 'Company - Private' ~ 'Private Company',
                                                         type_of_ownership == 'Company - Public' ~ 'Public Company',
                                                         TRUE ~ type_of_ownership))

#-- Revenue column
datascience_jobs %>% select(revenue) %>% distinct() 


datascience_jobs <- datascience_jobs %>%
                     mutate(revenue = case_when(revenue == '-1' ~ 'Unknown',
                                                grepl('Non', revenue) ~ 'Unknown',
                                                TRUE ~ gsub('to', '-', revenue)))

#-- Competitors column
datascience_jobs %>% select(competitors) %>% distinct() 


datascience_jobs <- datascience_jobs %>%
                    mutate(competitors = ifelse(competitors == '-1', 'Unknown', competitors))

# DROPPING COLUMN
datascience_jobs <- datascience_jobs %>% 
                    select(-index)

# DATA REVIEW
View(datascience_jobs)

# SAVING DATA
write_csv(datascience_jobs,"DS_jobs_cleaned.csv")
