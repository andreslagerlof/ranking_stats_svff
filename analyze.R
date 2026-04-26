
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(janitor)

# Read data ---------------------------------------------------------------

ranking_all_25 <- readRDS("ranking_all.rds")

# Spara resultatet som CSV
write_csv(ranking_all_25, "ranking_all_25.csv")


# Clean data --------------------------------------------------------------

# Funktion för att extrahera svensk klubbförkortning
extrahera_klubb <- function(klubb_str) {
  
  # Om strängen innehåller komma, ta den del som börjar med en svensk regionkod
  # Svenska regionkoder: ST, SY, MS, VS, ÖS
  if (str_detect(klubb_str, ",")) {
    klubb_str <- klubb_str |>
      str_split(",") |>
      pluck(1) |>
      str_trim() |>
      keep(~ str_detect(.x, "^(ST|SY|MS|VS|ÖS)\\s"))
    
    # Om ingen svensk klubb hittas, returnera NA
    if (length(klubb_str) == 0) return(NA_character_)
    klubb_str <- klubb_str[[1]]
  }
  
  # Extrahera förkortningen = det andra ordet (efter regionkoden)
  klubb_str |>
    str_extract("(?<=^(ST|SY|MS|VS|ÖS)\\s)\\S+")
}

# Applicera på Klubb/Klubbar-kolumnen
df_all <- df_all |>
  mutate(klubb = map_chr(`Klubb/Klubbar`, extrahera_klubb))

# Kontrollera resultatet
df_all |>
  distinct(`Klubb/Klubbar`, klubb) |>
  arrange(`Klubb/Klubbar`)

# Korrigera data typ

df_all_clean <- df_all |> 
  clean_names()

# Spara resultatet som CSV
write_csv(df_all_clean, "df_all_clean.csv")

df_clean <- df_all_clean |> 
  mutate(
    plats = as.numeric(plats),
    poang = poang |> str_replace(",", ".") |> as.numeric()
  ) |> 
  select(-c(overforda_poang, klubb_klubbar))


# Analyze -----------------------------------------------------------------

## Topprankade per klubb, alla

# Antal topp 10 rankade per klubb

df_clean |> 
  filter(plats < 11) |> 
  group_by(klubb) |> 
  count(klubb, sort = TRUE)

# Antal topp 4 rankade per klubb

df_clean |> 
  filter(plats < 5) |> 
  group_by(klubb) |> 
  count(klubb, sort = TRUE)

## Topprankade per klubb, unika personer

# Antal topp 10 rankade per klubb

df_clean |> 
  filter(plats < 11) |> 
  slice_min(plats, by = namn, with_ties = FALSE) |> 
  count(klubb, sort = TRUE)

# Antal topp 4 rankade per klubb


df_clean |> 
  filter(plats < 5) |> 
  slice_min(plats, by = namn, with_ties = FALSE) |> 
  count(klubb, sort = TRUE)

## Topprankade uppdelat på kön

df_clean |> 
  filter(plats < 11) |> 
  group_by(klubb, gender) |> 
  count(klubb, sort = TRUE)
