
library(tidyverse)
library(jsonlite)
library(janitor)

full_data <- jsonlite::fromJSON("https://interactive.zeit.de/g/cronjobs/dpa-wahl-daten/de-2021/live/full-data.json")

# full_data %>% glim

per_con <- full_data$resultsPerConstituency

wk <- per_con$result$latest$parties  %>%
    # map(as.data.frame) %>% 
    set_names(per_con$electoralId) %>%
    imap_dfr(~{
        raw <- as.data.frame(.x) %>% mutate(electoralId = unique(.y)) 
        
        # print(raw)
        
        if("result" %in% colnames(raw)){
            results <- raw  %>% 
                pluck("result") %>% 
                pluck("votes") 
            
            
            first_dat <- results  %>% 
                pluck("first") %>% 
                set_names(~paste0("first_", .x))
            
            
            second_dat <- results  %>% 
                pluck("second")  %>% 
                set_names(~paste0("second_", .x))
            
            raw <- bind_cols(first_dat, second_dat) %>% 
                bind_cols(select(raw, party_id = id, electoralId)) 
        }
        
        return(raw)}
    )

btw21 <- per_con %>% 
    mutate(electoralId = as.character(electoralId)) %>%
    select(-result) %>% 
    left_join(wk) %>% 
    as_tibble() %>% 
    # unnest_wider(result) %>% 
    janitor::clean_names() %>%
    mutate(party = case_when(
        party_id == "par_de_2" ~ "CDU",
        party_id == "par_de_1" ~ "SPD",
        party_id == "par_de_4" ~ "FDP",
        party_id == "par_de_3" ~ "Grüne",
        party_id == "par_de_6" ~ "AfD",
        party_id == "par_de_7" ~ "Freie Wähler",
        party_id == "par_de_5" ~ "Linke",
        party_id == "par_de_12" ~ "Tierschutzpartei",
        party_id == "par_de_13" ~ "Die Partei",
        party_id == "par_de_9" ~ "Piraten",
        party_id == "par_de_14" ~ "ÖDP",
        party_id == "par_de_39" ~ "Humanisten",
        party_id == "par_de_75" ~ "V-Partei³",
        party_id == "par_de_11" ~ "NPD",
        party_id == "par_de_74" ~ "DiB",
        party_id == "par_de_28" ~ "MLPD",
        party_id == "par_de_8" ~ "CSU",
        party_id == "par_de_55" ~ "Tierschutzallianz",
        party_id == "par_de_33" ~ "Bayernpartei",
        party_id == "par_de_16" ~ "Bündnis C",
        party_id == "par_de_42" ~ "Gesundheitsforschung",
        party_id == "par_de_51" ~ "SGP",
        party_id == "par_de_77" ~ "du.",
        party_id == "par_de_27" ~ "DKP",
        party_id == "par_de_34" ~ "Menschliche Welt",
        # party_id == "par_de_0" ~ "",
        party_id == "par_de_70" ~ "BüSo",
        T ~ NA_character_
    )) %>% 
    drop_na(party) 

# print(btw21)


btw21_wide <- btw21 %>% 
    pivot_wider(id_cols = id, names_from = party, values_from = c("first_absolute", "first_percent",
                                                                  "second_absolute", "second_percent")) %>%
    janitor::clean_names() %>% 
    # View()
    left_join(btw21 %>% distinct(id, electoral_id, name, .keep_all = T) %>% 
                  select(-first_percent:-second_difference, -party_id)) %>% 
    select(electoral_id:type, everything()) 


saveRDS(btw21, "data/btw21.rds")
saveRDS(btw21_wide, "data/btw21_wide.rds")


write_csv(btw21 %>% select(-candidates), file = "data/btw21.csv")
write_csv(btw21_wide %>% select(-candidates), file = "data/btw21_wide.csv")
