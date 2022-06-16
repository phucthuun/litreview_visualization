# This script preprocess the complex dataset 

  directory = dirname(getSourceEditorContext()$path) %>% str_extract(pattern = '[^litreview_visualization]+') %>% file.path("litreview_visualization")
 
  {
    data_path = file.path(directory,'00_Data xlsx/')
    funcon_path = file.path(directory,'03_Best-script/00_FunCon/')
    result_path = file.path(directory,'04_Results/')
    descriptives_path = file.path(directory,'05_Descriptives/')
  }

# Load packages
{
  library(dplyr)
  library(tidyverse)
  library(readxl)
  library(fastDummies)
  library(reshape2)
  library(stringr)
  library(writexl)
}

# Read data set
reviewtable <- read_excel(file.path(data_path,"Lit_Review_QC.xlsx", sep = ""))


# GET DATA =====================================================================

# Retrieve dataset

reviewtable <- reviewtable %>%
  # select dimensions of interest
  # select(Article_ID, Task_number, Authors, Title, Year,
  #        starts_with("Age_"), Design, `Eye tracking`, starts_with('Imaging_'), 
  #        Task_type, `To-be-remembered info`, 
  #        Encoding_instruction, starts_with("Repetition_of_encoding_trials_"), starts_with("Retrieval_mode_"), 
  #        Similarity_Manipulation_Encoding_1, Similarity_Manipulation_Encoding_2,
  #        Similarity_Manipulation_Retrieval_1, Similarity_Manipulation_Retrieval_2,
  #        starts_with("Cue manipulation"), starts_with("_stimuli_"),
  #        starts_with("_modality_"),
  #        starts_with("Delay")) %>% 
  # create entry_id to identify entries
  mutate(Entry_ID = paste(Article_ID, Task_number, sep = ", "), .after = Task_number) %>%
  select(-c(Article_ID, Task_number))%>%
  # rename some dimensions
  rename(c(Age_18 = Age_18_and_above,
           Eye_tracking_1 = `Eye tracking`,
           To_be_remembered_information = `To-be-remembered info`, 
           Cue_manipulation_1 = `Cue manipulation_1`, Cue_manipulation_2 = `Cue manipulation_2`,
           Stimuli_1 = `_stimuli_1`,
           Stimuli_2 = `_stimuli_2`,
           Stimuli_3 = `_stimuli_3`,
           Modality_1 = `_modality_1`,
           Modality_2 = `_modality_2`,
           Modality_3 = `_modality_3`))

#....................................................................................................
# Variable                              | Data type                                | Transform to    
# ...................................................................................................
# Entry_ID                              | character                                |.                
# Age                                   | numerical, binary                        |binary           
# Design                                | numerical, non-binary categorical        |binary           
# Eye tracking                          | numerical, binary                        |binary           
# Imaging_structural/functional         | numerical, non-binary categorical        |binary           
# Task_type                             | numerical, non-binary categorical        |binary           
# To-be-remembered-info                 | numerical, non-binary categorical        |binary           
# Encoding_instruction                  | numerical, non-binary categorical        |binary           
# Repetition_of_encoding                | numerical, non-binary categorical        |binary           
# Retrieval_mode                        | numerical, non-binary categorical        |binary           
# Similiarity_Manipulation_Encoding     | numerical, non-binary categorical        |binary           
# Similiarity_Manipulation_Retrieval    | numerical, non-binary categorical        |binary           
# Cue manipulation                      | numerical, non-binary categorical        |binary           
# Stimuli                               | numerical, non-binary categorical        |binary           
# Modality                              | numerical, non-binary categorical        |binary           
# Delay                                 | numerical, not binned                    |binary           
#....................................................................................................


# PREPROCESSING ================================================================

# Recode imaging
reviewtable <- reviewtable %>% 
  # manual recoding of value
  mutate(Imaging_structure_1 = recode(Imaging_structure_1, `0` = -999),
         Imaging_structure_2 = recode(Imaging_structure_2, `0` = -999),
         Imaging_function_1 = recode(Imaging_function_1, `0` = -999),
         Imaging_function_2 = recode(Imaging_function_2, `0` = -999))

# Recode Similarity_Manipulation_Encoding/Retrieval and Cue manipulation----
# idea: if manipulation1 is 1, then manipulation2 is -999.
# else, while manipulation1 is >1, if manipulation2 is 1, than manipulation2 is -999:

reviewtable <- reviewtable %>%
  mutate(Similarity_Manipulation_Encoding_2 = case_when(Similarity_Manipulation_Encoding_1 == 1 ~ -999,
                                                        Similarity_Manipulation_Encoding_1 != 1 & Similarity_Manipulation_Encoding_2 == 1 ~ -999,
                                                        Similarity_Manipulation_Encoding_1 != 1 & Similarity_Manipulation_Encoding_2 != 1 ~ Similarity_Manipulation_Encoding_2),
         Similarity_Manipulation_Retrieval_2 = case_when(Similarity_Manipulation_Retrieval_1 == 1 ~ -999,
                                                         Similarity_Manipulation_Retrieval_1 != 1 & Similarity_Manipulation_Retrieval_2 == 1 ~ -999,
                                                         Similarity_Manipulation_Retrieval_1 != 1 & Similarity_Manipulation_Retrieval_2 != 1 ~ Similarity_Manipulation_Retrieval_2),
         Cue_manipulation_2 = case_when(Cue_manipulation_1 == 1 ~ -999,
                                        Cue_manipulation_1 != 1 & Cue_manipulation_2 == 1 ~ -999,
                                        Cue_manipulation_1 != 1 & Cue_manipulation_2 != 1 ~ Cue_manipulation_2))



# Recode Repetition_of_encoding_trials ----
# idea: repetition == 1 >> one-shot,
# repetition > 1 >> multishot,
# in percentage (e.g., 80% = 0.8) or elsely defined >> criterion (specified),
# unspecified >> -999

RoET.input <- c(reviewtable$Repetition_of_encoding_trials_1,
                reviewtable$Repetition_of_encoding_trials_2,
                reviewtable$Repetition_of_encoding_trials_3,
                reviewtable$Repetition_of_encoding_trials_4) %>% unique()
RoET.999 <- RoET.input[RoET.input %in% c('unspecified','unknown','-999','-999.0')]
RoET.1 <- RoET.input[!is.na(as.numeric(RoET.input)) & as.numeric(RoET.input)==1] # oneshot
RoET.2 <- RoET.input[!is.na(as.numeric(RoET.input)) & as.numeric(RoET.input)>1] # multishot
RoET.3 <- setdiff(RoET.input, c(RoET.1,RoET.2,RoET.999))

reviewtable <- reviewtable %>% mutate_at(vars(starts_with('Repetition_of_encoding')),
                                         funs(case_when(. %in% RoET.999 ~ '-999',
                                                        # 'criterion'
                                                        . %in% RoET.3 ~ '3',
                                                        #'multishot'
                                                        . %in% RoET.2 ~ '2',
                                                        # 'oneshot'
                                                        . %in% RoET.1 ~ '1') %>% as.numeric()))


# Recode delays ----
reviewtable <- reviewtable %>%
  mutate(Delay1min = str_split_fixed(Delay_1, " - ", 2)[,1], Delay1max = str_split_fixed(Delay_1, " - ", 2)[,2],
         Delay2min = str_split_fixed(Delay_2, " - ", 2)[,1], Delay2max = str_split_fixed(Delay_2, " - ", 2)[,2],
         Delay3min = str_split_fixed(Delay_3, " - ", 2)[,1], Delay3max = str_split_fixed(Delay_3, " - ", 2)[,2],
         Delay4min = str_split_fixed(Delay_4, " - ", 2)[,1], Delay4max = str_split_fixed(Delay_4, " - ", 2)[,2],
         Delay5min = str_split_fixed(Delay_5, " - ", 2)[,1], Delay5max = str_split_fixed(Delay_5, " - ", 2)[,2],
         Delay6min = str_split_fixed(Delay_6, " - ", 2)[,1], Delay6max = str_split_fixed(Delay_6, " - ", 2)[,2]) %>%
  mutate(Delay_1 = case_when(Delay1max != '' ~ Delay1max, Delay1max == '' ~ Delay1min) %>% as.numeric(),
         Delay_2 = case_when(Delay2max != '' ~ Delay2max, Delay2max == '' ~ Delay2min) %>% as.numeric(),
         Delay_3 = case_when(Delay3max != '' ~ Delay3max, Delay3max == '' ~ Delay3min) %>% as.numeric(),
         Delay_4 = case_when(Delay4max != '' ~ Delay4max, Delay4max == '' ~ Delay4min) %>% as.numeric(),
         Delay_5 = case_when(Delay5max != '' ~ Delay5max, Delay5max == '' ~ Delay4min) %>% as.numeric(),
         Delay_6 = case_when(Delay6max != '' ~ Delay6max, Delay6max == '' ~ Delay6min))

c(reviewtable$Delay_1,
  reviewtable$Delay_2,
  reviewtable$Delay_3,
  reviewtable$Delay_4,
  reviewtable$Delay_5,
  reviewtable$Delay_6) %>% unique()

reviewtable <- reviewtable%>% select(-c(ends_with('max'), ends_with('min')))

reviewtable <- reviewtable%>%
  mutate_at(vars(starts_with('Delay_')),
            funs(case_when(
              # 'immediate'
              . >=0      & . < 1 ~ 1, 
              # '1-10min'
              . >= 1     & . <= 11 ~ 2, 
              #'11-30min'
              . > 10     & . <= 30 ~ 3, 
              #'31-60min',
              . > 30     & . <= 60 ~ 4, 
              #'61min-12h'
              . > 60     & . <= 720 ~ 5, 
              #'12-24h'
              . > 720    & . <= 1440 ~ 6, 
              #'24-48h'
              . > 1440   & . <= 2880 ~ 7, 
              #'49h-1w'
              . > 2880   & . <= 10080 ~ 8,
              #'1.01wk-1mth'
              . > 10080  & . <= 43800 ~ 9,
              #'1-3mth'
              . > 43800  & . <= 131400 ~ 10, 
              #'3mth-1y'
              . > 131400 & . <= 525600 ~ 11, 
              #'1-2y'
              . > 525600 & . <= 1051200 ~ 12, 
              #'2+y'
              . > 1051200 ~ 13)))



# Define indentifier variables
identifier_var <- c("Entry_ID")

# Define NAs
reviewtable[reviewtable==-999] <- NA



# Quality check----

# 1. stimulus w/o modality and vice versa
dfCheck <- reviewtable %>%
  filter(is.na(Stimuli_1) & is.na(Modality_1) == F |
           is.na(Stimuli_2) & is.na(Modality_2) == F |
           is.na(Stimuli_3) & is.na(Modality_3) == F |
           is.na(Stimuli_1) == F & is.na(Modality_1) |
           is.na(Stimuli_2) == F & is.na(Modality_2) |
           is.na(Stimuli_3) == F & is.na(Modality_3)) %>%
  pull(Entry_ID)


dfCheck1 <- reviewtable %>%
  filter(   Stimuli_1 == 11 |Stimuli_2 == 11 |Stimuli_3 == 11   ) %>%
  pull(Entry_ID)


setdiff(reviewtable$Entry_ID, dfCheck)


# SAVE DATA FILE ----

writexl::write_xlsx(reviewtable, file.path(data_path, "Lit_Review_Preprocessed.xlsx"))
message('Create ', file.path(data_path, "Lit_Review_Preprocessed.xlsx"))
