
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(readxl)


# ----- 1. INSTÄLLNINGAR OCH INLOGGNING ----

my_username <- Sys.getenv("OPHARDT_USERNAME")
my_password <- Sys.getenv("OPHARDT_PASSWORD")
login_url   <- "https://fencing.ophardt.online/en/login"

# Starta sessionen
my_session <- session(login_url)

# Hitta inloggningsformuläret (enligt din tidigare analys var det nr 3)
form <- html_form(my_session)[[3]]

# Fyll i formuläret med de fältnamn vi hittade (_username och _password)
filled_form <- html_form_set(form, 
                          `_username` = my_username, 
                          `_password` = my_password)

# Skicka in och spara den inloggade sessionen
my_session <- session_submit(my_session, filled_form)



# Läs in url:er för damer, herrar i olika ålderskategorier ----------------


# Läs in URL-fil med metadata för varje rankingtabell
urls <- read_excel("webbadresser.xlsx", sheet = "Blad2")


# Skapa funktion för att web scrapa ---------------------------------------


# Funktion för att skrapa en rankingtabell från Ophardt Online
#
# Tabellerna innehåller nästlade undertabeller med tävlingsresultat per fäktare,
# vilket gör att antalet <td>-celler per rad varierar. Funktionen hanterar detta
# genom att söka bakifrån efter den cell som innehåller en landskod (t.ex. "SWE"),
# och hämtar därefter Klubb/Klubbar och Född från de två efterföljande cellerna.
#
# Argument:
#   ranking_url  URL till rankingtabellen
#   weapon       Vapenklass (t.ex. "epee")
#   gender       Kön (t.ex. "women" eller "men")
#   age          Ålderskategori (t.ex. "sen", "u17", "u20", "u23")
#
# Returnerar en tibble med kolumnerna:
#   Plats, Poäng, Överförda poäng, Namn, Nation, Klubb/Klubbar, Född,
#   weapon, gender, age

scrape_ranking <- function(ranking_url, weapon, gender, age) {
  
  # Hämta sidan med inloggad session och sätt encoding till UTF-8
  page_content <- session_jump_to(my_session, ranking_url)$response |>
    httr::content(encoding = "UTF-8")
  
  page_content |>
    html_element("table.rankingbody") |>
    html_elements("tr") |>
    map(function(row) {
      
      # Hämta alla <td>-celler i raden
      cells <- html_elements(row, "td")
      n     <- length(cells)
      
      # Hoppa över rader med för få celler (t.ex. header-rader)
      if (n < 30) return(NULL)
      
      # Extrahera första raden av text från varje cell (ignorera undertabellens text)
      cell_texts <- map_chr(
        cells,
        ~ html_text(.x, trim = TRUE) |>
          str_extract("^[^\n]+") |>
          replace_na("")
      )
      
      # Hitta index för Nation-cellen genom att söka bakifrån efter landskod,
      # t.ex. "SWE" eller "SWE POL" (fäktare med dubbelt medborgarskap)
      nation_idx <- detect_index(
        cell_texts,
        ~ str_detect(.x, "^[A-Z]{2,3}(\\s+[A-Z]{2,3})*$"),
        .dir = "backward"
      )
      
      # Returnera NULL om ingen landskod hittas
      if (nation_idx == 0) return(NULL)
      
      # Bygg tibble med de sju huvudkolumnerna plus metadata
      tibble(
        Plats             = html_text(cells[[1]], trim = TRUE),
        Poäng             = html_text(cells[[2]], trim = TRUE),
        `Överförda poäng` = html_text(cells[[3]], trim = TRUE),
        Namn              = html_text(cells[[4]], trim = TRUE) |>
          str_extract("^[^\n]+") |>
          str_trim(),
        Nation            = cell_texts[[nation_idx]],
        `Klubb/Klubbar`   = cell_texts[[nation_idx + 1]],
        Född              = cell_texts[[nation_idx + 2]],
        weapon            = weapon,
        gender            = gender,
        age               = age
      )
    }) |>
    list_rbind()
}


# Iterera för att skapa resultatet ----------------------------------------


# Iterera över alla URL:er och sammanfoga resultaten till en tibble
df_all <- urls |>
  pmap(function(weapon, gender, age, url) {
    scrape_ranking(
      ranking_url = url,
      weapon      = weapon,
      gender      = gender,
      age         = age
    )
  }) |>
  list_rbind()


# Spara resultatet som CSV
write_csv(df_all, "ranking_all.csv")

# Spara som RDS fil
saveRDS(df_all, "ranking_all.rds")
