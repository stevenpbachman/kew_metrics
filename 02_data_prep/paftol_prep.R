
devtools::install_github("matildabrown/rWCVP")

library(rWCVP)
library(dplyr)
library(rWCVPdata)
devtools::install_github("matildabrown/rWCVPdata")


global_summary <- wcvp_summary()

summary <- global_summary$Summary


tm <- taxonomic_mapping

acc_sp <- names %>% select(family, genus, species, taxon_status, taxon_rank) %>% filter(taxon_status == "Accepted",
                                                                                        taxon_rank == "Species")


acc_sp_groups <- acc_sp %>% left_join(tm, by = "family") 

acc_sp_groups_count <- acc_sp_groups %>% group_by(higher) %>% count()


names <- rWCVPdata::wcvp_names %<%

ranks <- names %>% group_by(taxon_rank) %>% count()
