# extraire les absences des élus au COnseil de Paris des pdf -------------------

pacman::p_load(pdftools, tidyverse, lubridate, rvest, xml2, purrr)
list.files("../")
url = "https://www.paris.fr/pages/comptes-rendus-et-debats-et-deliberations-du-conseil-224"

# identifier les liens pdf d'une page internet

f.scraplinks <- function(url){
    webpage <- xml2::read_html(url)
    url_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_attr("href") 
    link_ <- webpage %>%
        rvest::html_nodes("a") %>%
        rvest::html_text()
    return(tibble(link_, url_))
}

webLiens <- f.scraplinks(url)
webLiens <- webLiens %>% filter(str_detect(link_,"Séance du Conseil de Paris ") &
                         str_detect(link_, "(2021)") &
                         !str_detect(link_, "(Annexe|Addenda)"))

url_ <- c("https://cdn.paris.fr/paris/2021/10/25/dce5274767d0fda43f35e33c8fd344db.pdf",
          "https://cdn.paris.fr/paris/2021/08/03/e2dab1fc3bc26825de900b74d4ffbc06.pdf",
          "https://cdn.paris.fr/paris/2021/06/14/b4f24b6fa0997ca8137241dc48aa3b9f.pdf",  #juin
          "https://cdn.paris.fr/paris/2021/04/23/1455aa6725032472a5e910bbe1e56e69.pdf",
          "https://cdn.paris.fr/paris/2021/03/22/dd0810f9e657d907b2cff77d2d6536c4.pdf",
          "https://cdn.paris.fr/paris/2021/02/12/932e03a17b81fd0caebf0a77eb61b3c8.pdf")


status <- c("Present(e)", "Absent(e)", "Excuse(e) au sens du reglement", "Excuse(e)")



# extract and read pdf -----------------------------------------------------
#df <- map(webLiens$url_, ~pdf_text(.))
df <- url_ %>% map(.f = ~pdf_text(.))
#df[[5]][[32]]

df.results <- tibble()

for (k in 1:length(df)){
  print(paste0("k = ", k))
    if (is.null(length(df[[k]]))==TRUE) next
  
    #if (sum(str_detect(df[[k]], "DECEMBRE 2020")) > 0)  next
    if (sum(str_detect(df[[k]], "(Absent|Excusé|Présents :)")) == 0) next
    #if (sum(str_detect(df[[k]], "mercredi 18 novembre 2020")) > 0) next
  
   df.tmp <- df[[k]] %>% keep(str_detect(., "(Absent|Excusé|Présents :)")) %>%
                        discard(str_detect(.,"(Votant|votant)"))

   
# extraire la date du conseil
    vec.date = str_extract(df.tmp, "(Lundi |Mardi |Mercredi |Jeudi |Vendredi )([:blank:]|[:graph:])+( Matin| Après-midi)")
    #vec.tmp = map_if(vec.date, 
    #                  str_locate(pattern = "[0-9]{4}") %>% as_tibble() %>% pull(),
    #                  str_replace(pattern = " - ", replacement = " 2021 - "))
                      
    
    
# extraire présents, excusés et absents
    s = str_locate(df.tmp, "(M. |Mme )")[,1] #première personne présente
    e = str_length(df.tmp)
    excr = str_locate(df.tmp, "(Excusés au sens du règlement :\n|Excusé au sens du règlement :\n|Excusées au sens du règlement :\n|Excusée au sens du règlement :\n)")
    exc = str_locate(df.tmp, "(Excusés :\n |Excusé :\n|Excusées :\n|Excusée :\n)")
    abs =  str_locate(df.tmp, "(Absents :\n |Absent :\n|Absente :\n|Absentes :\n)")

    vec.abs = str_sub(df.tmp, start = abs[,2]+2, end = -10) %>%
              str_trim(.) %>% str_replace_all(., "\n", " ") %>% 
              str_split(., ", ")

    vec.exc = str_sub(df.tmp, start=exc[,2]+1, end=pmin(abs[,1], e, na.rm=TRUE)-5) %>%
              str_trim(.) %>% str_replace_all(., "\n", " ")%>% 
              str_split(., ", ")

    vec.excr= str_sub(df.tmp, start=excr[,2]+1, end=pmin(exc[,1],abs[,1],e,na.rm=TRUE)-5) %>%
              str_trim(.) %>% str_replace_all(., "\n", " ") %>% 
              str_split(., ", ")

    vec.present  = str_sub(df.tmp, start=s, end=pmin(excr[,1], exc[,1],abs[,1],e, na.rm=TRUE)-2) %>%
              str_trim(.) %>% str_replace_all(., "\\n", " ") %>% 
              str_split(., ", ")


    x <- tibble(vec.present, vec.abs, vec.excr, vec.exc)

    for (i in 1:length(status)){
      for (j in 1:length(vec.date)){
          df.results <- tibble(x[[i]][[j]], vec.date[j], status[i]) %>%
                        bind_rows(df.results)
      }
    }
    print(dim(df.results))
}


# 2. Finaliser et convertir les champs noms en Civilité, Prénom, Nom ____________________________

# fine tuning the output, trim, remove dots...
names(df.results) <- c("Nom", "Date", "Status")
df.results <- df.results %>% 
  filter(!is.na(Nom)) %>%
  mutate(Nom = str_trim(Nom, side = "both"),
         Nom = ifelse(str_detect(Nom, "[:punct:]$")==TRUE,
                      str_sub(Nom, start=1, end=str_length(Nom)-1),
                      Nom))

# nécessite de dupliquer le champs, supprimer les accents, identifier les spérations dans deux vecteus et appliquer au champs initial

df.results <- df.results %>%  
  mutate(Civilite = str_extract(Nom, pattern = "(M. |Mme)") %>% str_trim(), .before = "Nom",
         Nom = str_replace(Nom, pattern = "(M. |Mme )", replacement = "")) 

# locate either: de, d' ou 2 lettre min suivi d'un espace suive de deux lettres maj
sep1 <- df.results %>% mutate(Nom2 = stringi::stri_trans_general(Nom, "Latin-ASCII")) %>%   #new column to remove all accents
  pull() %>%
  str_locate(pattern = "(((?<=[[:lower:]]{1})[[:blank:]](?=[[:upper:]]{2,}))| de | d\\')") %>% 
  as_tibble() %>% pull(start)


df.results <- df.results %>% 
  mutate(Prenom = str_sub(Nom, start=1, end=sep1) %>% str_trim(side="both"), .before="Nom",
         Nom = str_sub(Nom, start=sep1, end=-1) %>% str_trim(side="both")) 





df.results <- df.results %>% group_by(Civilite, Prenom, Nom) %>% # require a unique ID on names to work !!!
          mutate(ID = cur_group_id()) %>%
          pivot_wider(names_from = Date, values_from = Status)
glimpse(df.results)

df.results %>% writexl::write_xlsx("Absence des elus au conseil de Paris.xlsx")
rm(abs, exc, excr, vec.abs, vec.exc, vec.excr, vec.present, e,i,j,k,s, sep1, u, vec.date, vec.tmp)


# Bas de page ------------------------------------------------  


# k=3 cannot parse date : quickfix : vec.date <- dmy(paste0(str_sub(df.tmp, s[,1], e[,2]-2)," - ", "2021"))


# 1. récupérer les dates des Conseils sous forme de liste__________________________
jours <- str_extract_all(webLiens$link_, "( [1-3][0-9] | [1-3][0-9],| [1-9] | [1-9],)") %>%
        map(~ str_replace_all(.x,"\\,","")) %>%  map(~ str_trim(.x))
annee <- str_extract(webLiens$link_, "([1-2][0-9]{3})")
mois <- str_extract(webLiens$link_, "( [0-9]{1,2} [:alpha:]{4,} [1-2][0-9]{3})") %>%
        dmy() %>% month(., label = TRUE)

conseilDates <- pmap(list(jours, mois, annee), ~ paste(..1, ..2, ..3))
rm(jours, annee, mois)

                 
      




