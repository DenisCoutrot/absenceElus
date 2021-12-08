# absence des élus au Conseil de Paris

L'objectif du projet est de parser les compte-rendus sommaire des séance du Conseil de Paris afin de suivre les absences des élus au Conseil municipal. En effet, cette information n'est pas rendue disponible en open data. Les élus émargent en début de chaque demi-journée du Conseil, mais n'ont pas besoin d'émarger durant les différentes commissions, ce qui fait que l'absentéisme pourrait être en réalité beaucoup plus important.

## Methodologie

Les comptes-rendus sont disponibles sur l'url = "https://www.paris.fr/pages/comptes-rendus-et-debats-et-deliberations-du-conseil-224"
J'ai récupéré les pdf depuis le début de la mandature Hidalgo puis je les ai parsé afin de ne conserver que les pages relatives aux présences / absences. 
J'ai ensuite extrait chaque nom avec son statut: présent, absent, excusé au sens du réglement (?), excusé. 
Le tout est formatté et joint par fuzzy matching à la base de données des élus. 

Les premières analyses ci-dessous ont été faites par et pour Anticor.

<p align="center">
  <img src="Absence des élus par session du Conseil de Paris.jpg" width=676 height=450>
</p> 

## De la présence des élu·e·s au Conseil de Paris depuis la nouvelle Mandature

Remarque préalable : 
La base de données distingue Les « présents », « excusés au sens du RI », « excusés » et «
absents » tel qu’écrit dans les délibérations.
La distinction entre « excusés au sens du règlement intérieur » et « excusés » semble relevé
d’une nuance sémantique qui ne se retrouve pas dans le règlement intérieur(RI)[1], puisque celui-ci distingue 2 catégories :
* les absences excusées au sens du RI
* les absences non excusées.
Que signifit donc « excusés » ?
Par ailleurs aucun élément sur l’assiduité des élus dans les commissions n’existe.

L’émargement est effectué au debut de chaque demi-journée : Ainsi **62 demi-journées ont eu
lieu depuis le 3 juillet 2020**.

### 93,5 % de présents au Conseil de Paris depuis le début de la mandature soit

* 6,5 % d’absences toutes catégories confondues
* 1,7 % d’absences non justifiées ( « excusés » + « absents »)
* Le groupe politique le plus absent : Changer Paris (Républicains, Centristes et Indépendants) (73 demi- journées hors Excuse(e) au sens du règlement)
* 2eme groupe : Paris en commun (44 demi- journées hors Excuse(e) au sens du règlement )
* 3eme groupe : MDE (Modem, Démocrates et Écologistes) (21 demi- journées hors Excuse(e) au sens du règlement )

<p align="center">
  <img src="Top des absences des élus au Conseil de Paris.jpg" width=676 height=450>
</p> 

**L’élu le plus absent : Gérard Loureiro Conseiller du 197me arrondissement, groupe Changer Paris**
Fonction exécutive : non
54 demi journées d’absences
dont 54 excusées au sens du RI sur 62

**Anne HIDALGO Groupe Paris en Commun**
fonction exécutive : Maire de Paris
25 demi journées excusées
dont 25 au sens du RI / 62

**Elisabeth STIBBE Groupe Changer paris**
fonction exécutive /NON
conseillère du 13eme
23 demi journées d’absences
dont 4 non excusées 7 excusées et 12 excusées au sens du RI

**Béatrice PATRIE Groupe Communiste et citoyen**
FONCTION EXÉCUTIVE / NON
conseillère du 13eme
25 demi journées « excusées au sens du RI » / 62

**Emmanuel MESSAS Groupe Changer Paris**
fonction exécutive : non
19 demi journées d’absences
dont 1 demi journée absent 9 « demis journées excusées » et 9 demi-journées « excusées au
sens du RI »

### LE TOP 3 DES ÉLUS ABSENTS ( hors excusés au sens du Règlement Intérieur )
* Mr Pierre CASANOV (élu du 5ème) (Modem, Démocrates et Écologistes ) :17 demi-journées
d’absences
* Mme Agnès EVREN élue du 15ème (Changer Paris) : 13 demi-journées d’absences
* Mme Delphine TERLIZZI élue du 11ème (Paris en Commun) : 12 demi -journées d’absences


[1] https://cdn.paris.fr/paris/2021/02/01/aecf83d0af3e20218270be9e0624fa57.pdf


