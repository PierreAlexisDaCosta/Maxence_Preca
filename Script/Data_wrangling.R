# Load and inspect the data ####
raw_data <-
  readxl::read_excel("./Raw_data/Enquête précarité 2023 - ANEMF (réponses).xlsx")

raw_data %>% colnames()
pre_processed_data <-
  raw_data %>%
  select(-Horodateur) %>%
  filter(`Donnez-vous votre consentement pour que nous analysions vos réponses ? Elles seront supprimées de notre base de données à la fin de leur traitement.` == "Oui") %>%
  select(-`Donnez-vous votre consentement pour que nous analysions vos réponses ? Elles seront supprimées de notre base de données à la fin de leur traitement.`) %>%
  as_tibble()

str(pre_processed_data)
pre_processed_data %>% tbl_summary()
summary(raw_data)
