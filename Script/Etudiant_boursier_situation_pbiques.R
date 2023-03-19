pacman::p_load(ggupset)
pacman::p_load(UpSetR)
require(ggplot2); require(plyr); require(gridExtra); require(grid);

Situation_pbique <-
  pre_processed_data %>%
  select(Obligation_prêt_étudiant : Aucun_problème) %>% 
  na.omit()

# Financement %>% tbl_summary
Situation_pbique %>% tbl_summary
# wit setupR ####
Situation_pbique_1 <- 
  Situation_pbique %>% 
  # convert the "yes" and "no" values into 1s and 0s
  mutate(Obligation_prêt_étudiant = 
           ifelse(Obligation_prêt_étudiant == "Obligation d'un prêt étudiant", 1, 0), 
         Obligation_emprun_proche	 = 
           ifelse(Obligation_emprun_proche == "Obligation_emprun_proche", 1, 0),
         Découvert_bancaire = 
           ifelse(Découvert_bancaire == "Découvert bancaire", 
                  yes = 1, 
                  no = 0),
         Difficulté_loyer = 
           ifelse(Difficulté_loyer == "Difficulté à payer son loyer", 1, 0),
         Impossibilité_loyer = 
           ifelse(Impossibilité_loyer == "Impossibilité à payer son loyer", 1, 0),
         Cessation_location_logement = 
           ifelse(Cessation_location_logement == "Cessation de location de logement", 1, 0),
         Saut_de_repas = 
           ifelse(Saut_de_repas == "Saut de repas", 1, 0),
         Diminution_hygiène = 
           ifelse(Diminution_hygiène == "Diminution de l'hygiène de vie", 1, 0),
         Renonciation_soins = 
           ifelse(Renonciation_soins == " Renonciation à des soins", 1, 0),
         Renonciation_loisirs = 
           ifelse(Renonciation_loisirs == "Renonciation à des loisirs", 1, 0),
         Impossibilité_vacances = 
           ifelse(Impossibilité_vacances == "Impossibilité de partir en vacances", 1, 0)#,
         # Aucun_problème = 
         #   ifelse(Aucun_problème == "Cela n'entraîne aucune de ces situations", 1, 0)
         )


Situation_pbique_1 %>%
  as.data.frame() %>%
  UpSetR::upset(nset = 11,
                nintersects = 10,
                order.by = "freq",
                empty.intersections = "on",  
                number.angles = 0,
                point.size = 3.5,
                line.size = 2, 
                mainbar.y.label = "Difficultés Combinaison",
                sets.x.label = "Etudiant avec difficultés")

