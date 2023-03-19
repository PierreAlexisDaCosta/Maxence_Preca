pacman::p_load(ggupset)
pacman::p_load(UpSetR)

Autre_bourse <-
  pre_processed_data %>%
  select(Bourse_mérite:Autres_aides)

Autre_bourse %>% tbl_summary

Autre_bourse_1 <- 
  Autre_bourse %>% 
  # convert the "yes" and "no" values into 1s and 0s
  mutate(Bourse_mérite = 
           ifelse(Bourse_mérite == "Une bourse au mérite", 1, 0), 
         Allocation_logement	 = 
           ifelse(Allocation_logement == "Une allocation logement", 1, 0),
         Allocation_familiale = 
           ifelse(Allocation_familiale == "Une allocation familiale", 
                  yes = 1, 
                  no = 0),
         Allocation_chômage = 
           ifelse(Allocation_chômage == "Une allocation chômage", 1, 0),
         Autres_aides = 
           ifelse(Autres_aides == "Autre", 1, 0),
         Aucune = 
           ifelse(Aucune == "Aucune de ces aides", 1, 0)
       )


Autre_bourse_1 %>%
  as.data.frame() %>%
  UpSetR::upset(nset = 6,
                nintersects = 12,
                order.by = "freq",
                empty.intersections = "on",  
                number.angles = 0,
                point.size = 3.5,
                line.size = 2, 
                mainbar.y.label = "Difficultés Combinaison",
                sets.x.label = "Etudiant avec difficultés")


