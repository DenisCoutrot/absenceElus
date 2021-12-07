# absenceElus
L'objectif du projet est de parser les pdf de la mairie de Paris pour suivre les absences des élus au Conseil municipale

Source url = "https://www.paris.fr/pages/comptes-rendus-et-debats-et-deliberations-du-conseil-224"

Parser les pdf

Identifie les pages des membres présents / excusés et absents

Extrait chacune de ces catégories

Formatte le tout dans des nested tibble puis crée une BDD qui est ensuite exportée en csv.

On obtient une base de données des présences / absences depuis le début de la mandature Hidalgo,
ainsi que des graphiques


