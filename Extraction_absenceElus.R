# extraire les absences des élus au COnseil de Paris des pdf -------------------

pacman::p_load(pdftools, tidyverse, lubridate, rvest, xml2, purrr)
pacman::p_load(stringdist)
list.files("../")
url = "https://www.paris.fr/pages/comptes-rendus-et-debats-et-deliberations-du-conseil-224"
lienElus = "https://parisdata.opendatasoft.com/explore/dataset/conseillers-de-paris/information/?disjunctive.civilite&disjunctive.arrondissement&disjunctive.cp&disjunctive.cm&disjunctive.acronyme&disjunctive.groupe&disjunctive.president_de_groupe&disjunctive.fonction_dans_l_executif&disjunctive.liste_d_origine"


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
                         str_detect(link_, "(2020|2021)") &
                         !str_detect(link_, "(Annexe|Addenda)"))

url_ <- c("https://cdn.paris.fr/paris/2021/10/25/dce5274767d0fda43f35e33c8fd344db.pdf",
          "https://cdn.paris.fr/paris/2021/08/03/e2dab1fc3bc26825de900b74d4ffbc06.pdf",
          "https://cdn.paris.fr/paris/2021/06/14/b4f24b6fa0997ca8137241dc48aa3b9f.pdf",  #juin
          "https://cdn.paris.fr/paris/2021/04/23/1455aa6725032472a5e910bbe1e56e69.pdf",
          "https://cdn.paris.fr/paris/2021/03/22/dd0810f9e657d907b2cff77d2d6536c4.pdf",
          "https://cdn.paris.fr/paris/2021/02/12/932e03a17b81fd0caebf0a77eb61b3c8.pdf",
          "https://cdn.paris.fr/paris/2021/01/04/671ad39f64909599b76b0580e86e9dbe.pdf",
          "https://cdn.paris.fr/paris/2020/11/30/aeedd144f678b5ff2d8bbf5b861f5d57.pdf",
          "https://cdn.paris.fr/paris/2020/10/28/730a45308a1bcdfd12346bea7dfe83b8.pdf",
          "https://cdn.paris.fr/paris/2020/08/11/9f909bac569f5cae6490d89ff1ab71e8.pdf",  #23 juillet 2020
          "https://cdn.paris.fr/paris/2020/07/08/a5050c7a9eb25cc46f4a1341eaa12ab7.pdf",
          "https://cdn.paris.fr/paris/2020/05/26/52373d795f728681a1d408d498d50422.pdf")


status <- c("Present(e)",  "Excuse(e) au sens du reglement", "Excuse(e)", "Absent(e)")
code_status <- c(" ", #starts with a blank
                 "Excusé[[:lower:]]{0,2} au sens du règlement : ",
                 "Excusé[[:lower:]]{0,2} : ",
                 "Absent[[:lower:]]{0,2} : ")


# parse liste des élus
listElus <- read_csv2(lienElus)
listElus <- read_csv2("conseillers-de-paris.csv")


# extract and read pdf -----------------------------------------------------
#df <- map(webLiens$url_, ~pdf_text(.))
df <- url_ %>% map(.f = ~pdf_text(.))


df.results <- tibble()

for (k in 1:length(df)){
  print(paste0("k = ", k))
    if (is.null(length(df[[k]]))==TRUE) next
  
    #if (sum(str_detect(df[[k]], "DECEMBRE 2020")) > 0)  next
    if (sum(str_detect(df[[k]], "Listes des membres présents")) == 0) next
    #if (sum(str_detect(df[[k]], "mercredi 18 novembre 2020")) > 0) next
  
   df.tmp <- df[[k]] %>% discard(str_detect(.,"(Votant|votant|\\.\\.\\.\\.)")) %>%
     keep(str_detect(., "(Absent|Excusé|présents)")) 
   
   # require collapse, detect date and separate based on date location totherwise generates na
   df.tmp <- df.tmp %>% str_c(., collapse=" ") %>%
          str_replace_all(., pattern = "\\\n", replacement = " ") %>%
          str_replace_all(., "[:blank:]{2,}", " ") %>%
          str_replace_all(., "(MM\\.|M\\.|Mme |Mmes )", "")
   
   vec.date <- df.tmp %>% 
          str_extract_all(., "(Lundi |Mardi |Mercredi |Jeudi |Vendredi )([:blank:]|[:graph:]){5,20}( Matin| Après-midi)")
   
   tmp <- df.tmp %>% str_sub(., start=str_locate(., "(Lundi |Mardi |Mercredi |Jeudi |Vendredi )")[,1] , end=-1) %>%
          str_replace_all(., "[0-9]{1,3}", "") %>% 
          str_split(., "(Lundi |Mardi |Mercredi |Jeudi |Vendredi )([:blank:]|[:graph:]){5,20}( Matin| Après-midi)",
                  simplify = TRUE)
  
  if (str_detect(df.tmp, "3 juillet 2020")==TRUE){ 
      vec.date <- str_extract_all(df.tmp, "Vendredi 3 juillet 2020")
      tmp <- str_sub(df.tmp, start=str_locate(df.tmp, "Vendredi ")[,1] , end=-1) %>%
        str_replace_all(., "[0-9]{1,3}", "") %>% 
        str_split(., "Vendredi  juillet ", simplify = TRUE)
  }
   
  tmp <- tmp[-1]
 
    
# extract names with status   
for (i in 1:4){
   start = str_locate(tmp, code_status[i])
   l.point = str_locate(tmp, "(?<=[:upper:]{2})\\.")
   vec <- str_sub(tmp, start =start[,2], end = l.point[,1]-1) %>% 
          str_split(., ",") %>% map(~str_trim(.))
   tmp[!is.na(vec)] <- str_sub(tmp[!is.na(vec)], start=l.point[!is.na(vec),2]+1, end=-1) %>% 
      str_trim()
   assign(paste0("vec.", status[i]), vec)
}

# expand names, date and status into database
   
   x <- tibble(Nom=list(`vec.Present(e)`, `vec.Excuse(e) au sens du reglement`,
                        `vec.Excuse(e)`, `vec.Absent(e)`),
               Status=status, Date=vec.date) %>% 
     unnest(cols = c(Date, Nom)) %>%
     unnest(cols=c(Nom))
   
    df.results <- df.results %>% bind_rows(x)
    print(dim(x))
    print(dim(df.results))
    
    rm(x, df.tmp, l.point,vec,
       `vec.Absent(e)`, vec.date, `vec.Excuse(e)`, `vec.Excuse(e) au sens du reglement`, 
       `vec.Present(e)`)
}


# 2. Finaliser et convertir les champs noms en Civilité, Prénom, Nom ____________________________

# fine tuning the output, trim, remove dots...
df.results <- df.results %>% 
  filter(!is.na(Nom)) %>%
  mutate(Nom = str_trim(Nom, side = "both"),
         Nom = ifelse(str_detect(Nom, "[:punct:]$")==TRUE,
                      str_sub(Nom, start=1, end=str_length(Nom)-1),
                      Nom))

df.results <- df.results %>% 
            mutate(Date = str_replace(Date, "(?<=(1er|2|3|4) juin )", "2021 "),
                   Date = str_replace(Date, "(?<=(23|24) juillet )", "2020 "),
                   Date = str_replace(Date, "(?<=(17|18) novembre )", "2020 "),
                   Date = str_replace(Date, "(?<=(7|8) octobre )", "2020 "))

# locate either: de, d' ou 2 lettre min suivi d'un espace suive de deux lettres maj
sep1 <- df.results %>% mutate(Nom2 = stringi::stri_trans_general(Nom, "Latin-ASCII")) %>%   #new column to remove all accents
  pull() %>%
  str_locate(pattern = "(((?<=[[:lower:]]{1})[[:blank:]](?=[[:upper:]]{2,}))| de | d\\')") %>% 
  as_tibble() %>% pull(start)


df.results <- df.results %>% 
  mutate(Prenom = str_sub(Nom, start=1, end=sep1) %>% str_trim(side="both"), .before="Nom",
         Nom = str_sub(Nom, start=sep1) %>% str_trim(side="both")) 

# apply fuzzy matching to link to official names from listElus
df.results <- listElus %>% 
  rename(Prenom = "Prénom") %>%
  stringdist_full_join(df.results, by=c("Nom", "Prenom")) %>%
  rename(Nom = Nom.x, Prenom = Prenom.x)

##### fill NA with Nom.y et Prenom.y
df.results <- df.results %>% 
  mutate(Nom = ifelse(is.na(Nom), Nom.y, Nom),
         Prenom = ifelse(is.na(Nom), Prenom.y, Prenom),
         Conseillers = ifelse(is.na(Nom), paste(Nom, Prenom), Conseillers)
  )


#df.results <- df.results %>% group_by(Civilite, Prenom, Nom) %>% # require a unique ID on names to work !!!
#          mutate(ID = cur_group_id()) %>%
#          pivot_wider(names_from = Date, values_from = Status)
glimpse(df.results)

df.results %>% select(-c(Nom.y, Prenom.y)) %>%
      write_csv2("Absence des elus au conseil de Paris.csv")
rm(tmp, start, i,k, sep1)


# Quality checks_______________________________________________________________
# require fuzzy matching sur la liste des élus pour consolider les résultats
# test de la librairie fuzzyjoin
# quality check
df.results %>% group_by(Nom, Prenom) %>% summarise(N=n()) %>% View()

list_dates <- df.results %>% select(Date) %>% distinct() 

df.results %>% select(Nom, Date) %>% filter(Nom == "TONOLLI") %>%
  right_join(list_dates) %>% View()







## nice plot ___________________________________________________________________
# require parse date to order them and plot by presence
width = 10
height = (9/16) * width

df.tmp <- df.results %>% 
  mutate(Date = str_replace(Date, "Vendredi 3 juillet 2020", "Vendredi 3 juillet 2020 - Jour")) %>%
  separate(Date, c("Date", "Periode"), sep=" - ") %>%
  mutate(Date = dmy(Date))

df.tmp <- df.tmp %>% 
  mutate(datePeriod = case_when(
    Periode == "Matin" ~ paste0(Date, " ", " Matin"),
    Periode == "Après-midi" ~ paste0(Date, " ", "Après-midi"),
    TRUE ~ as.character(Date)
    ))

df.tmp %>% group_by(datePeriod, Status) %>% 
  summarize(value = n()) %>%
  ggplot(aes(x= datePeriod, y= value, fill=Status))+
    geom_col()+
  theme_minimal() + 
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Présence et Absence des élus au Conseil de Paris", y="Nombre d\'élus")+
  theme(axis.text.x = element_text(size=6,angle=90),
        legend.title = element_text(size=8),
        legend.text = element_text(size=8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
  
  ggsave("Absence des élus par session du Conseil de Paris.jpg" , width = width, height=height)

df.results %>% filter(Status != "Present(e)") %>% 
  group_by(Prenom, Nom)  %>% summarize(value = n()) %>%
  ungroup() %>%
  slice_max(order_by = value, n=30) %>%
  ggplot(aes(x= fct_reorder(paste(Prenom, Nom),value), y= value, fill=Status))+
  geom_col(fill="blue")+  coord_flip()+
  theme_minimal() + 
  scale_fill_brewer(palette = "RdBu")+ 
  labs(title = "Top de la non-présence des élus au Conseil de Paris", y="Nombre de Excusés ou Absent")+
  theme(axis.text.x = element_text(size=8),
        legend.title = element_text(size=8),
        legend.text = element_text(size=6),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave("Top des absences des élus au Conseil de Paris.jpg" , width = width, height=height)




# Bas de page ------------------------------------------------  
