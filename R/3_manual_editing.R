library(tidyverse)
library(sf)
library(data.table)
library(stringdist)
library(stringi)
source(file = "R/functions.R")

#### City Directories data ####
studied_data <- read_rds(file = "data/intermediary_datasets/studied_data_2.rds")
selecting_geolocated_points <- studied_data %>%
  filter(source.publication_date != 1891) %>% # not used
  filter(source.book %ni% c('Bottin3_1855', 'Cambon_almgene_1839', 'Deflandre_1829', 'Bottin2_1842')) # different directory collection

rm(studied_data)

# creating phonetic index and applying it on data to group activities patterns which are phonetically alike
selecting_geolocated_points <- selecting_geolocated_points %>%
  mutate(act_new = str_replace_all(string=activities, pattern = ',', replacement='')) %>%
  mutate(act_new = str_trim(string=act_new, side='both')) %>%
  mutate(act_new = stri_trans_general(str = act_new, id = "Latin-ASCII")) %>%
  mutate(act_new = str_to_lower(act_new)) %>%
  mutate(act_phoenetic = stringdist::phonetic(x = act_new))

selecting_geolocated_points <- data.table::setDT(selecting_geolocated_points)[, freq_max:= max_freq(act_new), by = act_phoenetic][] %>%
  st_as_sf()


## group review:
grouping_by_hand <- read.csv(file = "data/group_block_review.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  as_tibble()

### manual editing from analysis of dataframes: critics of soundex index ####
### creation of a better dataset
table_base <- selecting_geolocated_points %>%
  left_join(y = grouping_by_hand, by = "freq_max")
table_base_review <- table_base
## manual observation from info analysis (example) : 
## table_base_review %>% st_drop_geometry() %>% filter(freq_max == 'bottier') %>% group_by(act_new, freq_max, NAICS) %>% count() %>% view()
## then detecting pattern and replacing manually NAICS categories attribution

table_base_review <- table_base_review %>%
  # specification of cremier and cremeries
  mutate(NAICS = case_when(freq_max %in% c('cremicr', 'cremier') & str_detect(string = act_new, pattern = "char") == FALSE
                           & str_detect(string = act_new, pattern = "meri") == TRUE 
                           & source.publication_date > 1860 ~ 'Full-Service Restaurants',
                           freq_max %in% c('cremicr', 'cremier') 
                           & str_detect(string = act_new, pattern = "char") == TRUE ~ 'Manufacturing',
                           freq_max %in% c('cremicr', 'cremier') & str_detect(string = act_new, pattern = "char") == FALSE
                           & str_detect(string = act_new, pattern = "meri") == TRUE 
                           & source.publication_date < 1860 ~ 'Food stores',
                           freq_max %in% c('cremicr', 'cremier') & str_detect(string = act_new, pattern = "char") == FALSE
                           & str_detect(string = act_new, pattern = "meri") == FALSE ~ 'Food stores',
                           TRUE ~ NAICS)) %>%
  # specification of "appartements meubles" in max freq
  mutate(NAICS = case_when(freq_max == "appartements meubles"
                           & str_detect(string = act_new, pattern = "apprt|appret|aypreteur|appret.|appr. de") == TRUE ~ 'Manufacturing', # appret of
                           freq_max == "appartements meubles"
                           & str_detect(string = act_new, pattern = "appart") == TRUE ~ 'Building and Car Rental',
                           freq_max == "appartements meubles"
                           & str_detect(string = act_new, pattern = "affretement") == TRUE ~ 'Trade Agents and Brokers', # commisionnaire + affretement
                           freq_max == "appartements meubles"
                           & str_detect(string = act_new, pattern = "abr. ") == TRUE ~ 'Manufacturing', # those are fabr. of something bad OCR
                           freq_max == "appartements meubles"
                           & str_detect(string = act_new, pattern = "abratr|Abratr|Aibratr|aibratr") == TRUE ~ 'Publishing Industries', # librairies
                           TRUE ~ NAICS
  )) %>%
  # specification of "cabinet de lecture" in max freq
  mutate(NAICS = case_when(freq_max == "cabinet de lecture" 
                           & str_detect(string = act_new, pattern = "cben|Cben|cvent|Cvent") == FALSE ~ 'Administrative and Legal Services', # general
                           freq_max == "cabinet de lecture"
                           & str_detect(string = act_new, pattern = "cben|Cben") == TRUE ~ 'Home Furniture and Supplies Manufacturers/Retailers', # ebeniste
                           freq_max == "cabinet de lecture"
                           & str_detect(string = act_new, pattern = "cvent|Cvent") == TRUE ~ 'Home Furniture and Supplies Manufacturers/Retailers', # eventailliste
                           TRUE ~ NAICS
  ))%>%
  # specification of "chef d'institution" in max freq
  mutate(NAICS = case_when(freq_max == "chef d'institution" 
                           & str_detect(string = act_new, pattern = "cabtnet") == TRUE ~ 'Administrative and Legal Services', # cabinet de
                           freq_max == "chef d'institution"
                           & str_detect(string = act_new, pattern = "cafd|cafe|caf-|Cafe|cufe") == TRUE ~ 'Full-Service Restaurants', # cafe
                           freq_max == "chef d'institution"
                           & str_detect(string = act_new, pattern = "en ret") == TRUE ~ 'No Activity, Living of Income', # retraite
                           freq_max == "chef d'institution"
                           & str_detect(string = act_new, pattern = "capit|apitai|capttai|capitame|caputaine|cupit|chef") == TRUE ~ 'Public Administration', # capitaine
                           TRUE ~ NAICS
  )) %>%
  # specification of "chef de bat." in max freq
  mutate(NAICS = case_when(freq_max == "chef de bat." 
                           & str_detect(string = act_new, pattern = "cab.|Cab.") == TRUE ~ 'Administrative and Legal Services', # cabinet de
                           freq_max == "chef de bat."
                           & str_detect(string = act_new, pattern = "cafd|cafe|caf-|Cafe|cufe|ca fe") == TRUE ~ 'Full-Service Restaurants', # cafe
                           freq_max == "chef de bat."
                           & str_detect(string = act_new, pattern = "en ret") == TRUE ~ 'No Activity, Living of Income', # retraite
                           freq_max == "chef de bat."
                           & str_detect(string = act_new, pattern = "chef de bat") == TRUE ~ 'Public Administration', # chef de battaillon
                           freq_max == "chef de bat."
                           & str_detect(string = act_new, pattern = "chef de bur|chef du bur") == TRUE ~ 'Administrative and Legal Services', # chef de bureau
                           freq_max == "chef de bat."
                           & str_detect(string = act_new, pattern = "coup") == TRUE ~ 'Cosmetics, Beauty Supplies, and Perfume Retailers', # coupeur de poils
                           TRUE ~ NAICS
  )) %>%
  # specification of "courtier en vins" in max freq
  mutate(NAICS = case_when(freq_max == "courtier en vins" 
                           & str_detect(string = act_new, pattern = "corderie|Corderie|Cordier|cordier") == TRUE ~ 'Manufacturing',
                           freq_max == "courtier en vins"
                           & str_detect(string = act_new, pattern = "court") == TRUE ~ 'Trade Agents and Brokers', # courtier en marchandises
                           freq_max == "courtier en vins"
                           & str_detect(string = act_new, pattern = "d'ass|de banq|de bourse|en banq|en bunq") == TRUE ~ 'Finance and Insurance', # courtier en assurrances/banque
                           freq_max == "courtier en vins"
                           & str_detect(string = act_new, pattern = "curateur") == TRUE ~ 'Administrative and Legal Services',
                           TRUE ~ NAICS
  )) %>%
  # specification of "marechal-ferrant" in max freq
  mutate(NAICS = case_when(freq_max == "marechal-ferrant" 
                           & str_detect(string = act_new, pattern = "errant|crrant|ferr") == TRUE ~ 'Manufacturing', # marechal-ferrant
                           freq_max == "marechal-ferrant" 
                           & str_detect(string = act_new, pattern = "veter|xpert") == TRUE ~ 'Health Practitioners', # marechal-veterinaire
                           freq_max == "marechal-ferrant"
                           & str_detect(string = act_new, pattern = "march a|march. a|marche a") == TRUE ~ 'Clothing and clothing accessories manufacturers/retailers', # marchande de toilette
                           freq_max == "marechal-ferrant"
                           & str_detect(string = act_new, pattern = "ling") == TRUE ~ 'Home Furniture and Supplies Manufacturers/Retailers', # mercerie-lingerie
                           TRUE ~ NAICS
  )) %>%
  # specification of "produits chimiques" in max freq
  mutate(NAICS = case_when(freq_max == "produits chimiques" 
                           & str_detect(string = act_new, pattern = "chimi|chi-") == TRUE ~ 'Manufacturing',
                           freq_max == "produits chimiques" 
                           & str_detect(string = act_new, pattern = "maceu|macent") == TRUE ~ 'Manufacturing', # produit pharmaceutique
                           freq_max == "produits chimiques"
                           & str_detect(string = act_new, pattern = "aliment|altment|atiment|conserves ali") == TRUE ~ 'Food stores',
                           TRUE ~ NAICS
  )) %>%
  # specification of "sculpteur-statuaire" in max freq
  mutate(NAICS = case_when(freq_max == "sculpteur-statuaire" 
                           & str_detect(string = act_new, pattern = "salpet") == TRUE ~ 'Manufacturing', #salpetrier
                           freq_max == "sculpteur-statuaire" 
                           & str_detect(string = act_new, pattern = "sculp|seulp|sulp|soulp|Sculp") == TRUE ~ 'Manufacturing', # produit pharmaceutique
                           freq_max == "sculpteur-statuaire"
                           & str_detect(string = act_new, pattern = "stat|stut") == TRUE ~ 'Arts, Entertainment, and Recreation', # marchande de toilette
                           TRUE ~ NAICS
  )) %>%
  # specification of "boulanger" and "boutanger" in max freq
  mutate(NAICS = case_when(freq_max == "boulanger" & str_detect(string = act_new, pattern = "blanch.|blanchis|blanct|blamcui|blunch|btanch|balein") == TRUE
                           | freq_max == "boutanger" 
                           & str_detect(string = act_new, pattern = "blanch.|blanchis|blanct|blamcui|blunch|btanch|balein") == TRUE ~ 'Home Furniture and Supplies Manufacturers/Retailers', #blanchisserie and baleine
                           freq_max == "boulanger" & str_detect(string = act_new, pattern = "boutons") == TRUE
                           | freq_max == "boutanger" 
                           & str_detect(string = act_new, pattern = "boutons") == TRUE ~ 'Clothing and clothing accessories manufacturers/retailers',
                           freq_max == "boulanger" & str_detect(string = act_new, pattern = "batanc|balanc") == TRUE
                           | freq_max == "boutanger" 
                           & str_detect(string = act_new, pattern = "batanc|balanc") == TRUE ~ 'Manufacturing',
                           freq_max == "boulanger" & str_detect(string = act_new, pattern = "betons|bitum") == TRUE
                           | freq_max == "boutanger" 
                           & str_detect(string = act_new, pattern = "betons|bitum") == TRUE ~ 'Construction and Public Works', # beton or bitume
                           TRUE ~ NAICS
  )) %>%
  # specification in "agent de chance" freq_max:
  mutate(NAICS = case_when(freq_max == "agent de change" 
                           & str_detect(string = act_new, 
                                        pattern = "affai|faires|affuir|commercial|commerce|commiss|laires|taires|allatres|altaires") == FALSE ~ 
                             'Trade Agents and Brokers', 
                           freq_max == "agent de change" & str_detect(string = act_new, pattern = "assurances|financ") == TRUE ~ 'Finance and Insurance', 
                           TRUE ~ NAICS)) %>%
  # specification in "architecte" freq_max:
  mutate(NAICS = case_when(freq_max == "architecte" 
                           & str_detect(string = act_new, pattern = "airecteur|airect.|airccteur|archidiacre") == TRUE ~ NA, # directeur et archidiacre
                           freq_max == "architecte" & str_detect(string = act_new, pattern = "arocat|aroc") == TRUE ~ 'Administrative and Legal Services', # avocat 
                           TRUE ~ NAICS)) %>%
  # specification in "artiste-peintre" freq_max:
  mutate(NAICS = case_when(freq_max == "artiste-peintre" 
                           & str_detect(string = act_new, pattern = "artic|ardoise") == TRUE ~ 'Other retailers', # articles de... et ardoises
                           TRUE ~ NAICS)) %>%
  # specification in "banquiers" freq_max
  mutate(NAICS = case_when(freq_max == "banquiers" 
                           & str_detect(string = act_new, pattern = "bains") == TRUE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "beurre et oeufs" freq_max
  mutate(NAICS = case_when(freq_max == "beurre et oeufs" 
                           & str_detect(string = act_new, pattern = "place|ptacem") == TRUE ~ "Trade Agents and Brokers", # bureau de placement
                           freq_max == "beurre et oeufs" 
                           & str_detect(string = act_new, pattern = "pap") == TRUE ~ "Administrative and Legal Services", # bureau de papier timbre
                           TRUE ~ NAICS)) %>%
  # specification in "bijoutier en or" freq_max
  mutate(NAICS = case_when(freq_max == "bijoutier en or" 
                           & str_detect(string = act_new, pattern = "bois") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           freq_max == "bijoutier en or" 
                           & str_detect(string = act_new, pattern = "bouc") == TRUE ~ "Food stores", # boucher
                           TRUE ~ NAICS)) %>%
  # specification in "bijoutier en or" freq_max
  mutate(NAICS = case_when(freq_max == "bois et charbons" 
                           & str_detect(string = act_new, pattern = "bijout") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           freq_max == "bois et charbons" 
                           & str_detect(string = act_new, pattern = "biscuits") == TRUE ~ "Food stores",
                           TRUE ~ NAICS))


table_base_review <- table_base_review %>%
  # specification in "bottier" freq_max
  mutate(NAICS = case_when(freq_max == "bottier" 
                           & str_detect(string = act_new, pattern = "batar|batdr") == TRUE ~ "Other retailers", # bazar
                           TRUE ~ NAICS)) %>%
  # specification in "boucher" freq_max
  mutate(NAICS = case_when(freq_max == "boucher" 
                           & str_detect(string = act_new, pattern = "bazar|buzar") == TRUE ~ "Other retailers",
                           freq_max == "boucher" 
                           & str_detect(string = act_new, pattern = "bij") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "brocanteur" freq_max
  mutate(NAICS = case_when(freq_max == "brocanteur" 
                           & str_detect(string = act_new, pattern = "biere|beurre") == TRUE ~ "Food stores",
                           freq_max == "brocanteur" 
                           & str_detect(string = act_new, pattern = "bureau") == TRUE ~ NA,
                           freq_max == "brocanteur" 
                           & str_detect(string = act_new, pattern = "brasse") == TRUE ~ "Manufacturing", # brasserie, since 1878 at least, sometimes retailer too
                           TRUE ~ NAICS)) %>%
  # specification in "broderies" freq_max
  mutate(NAICS = case_when(freq_max == "broderies" 
                           & str_detect(string = act_new, pattern = "beurre") == TRUE ~ "Food stores",
                           freq_max == "broderies" 
                           & str_detect(string = act_new, pattern = "bordur|bourret") == TRUE ~ 'Manufacturing',
                           freq_max == "broderies" 
                           & str_detect(string = act_new, pattern = "bur") == TRUE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "bronzes" freq_max
  mutate(NAICS = case_when(freq_max == "bronzes" 
                           & str_detect(string = act_new, pattern = "bar") == TRUE ~ "Full-Service Restaurants",
                           freq_max == "bronzes" 
                           & str_detect(string = act_new, pattern = "brunis") == TRUE ~ 'Manufacturing', # brunisseur/brunisseuse
                           TRUE ~ NAICS)) %>%
  # specification in "brossier" freq_max
  mutate(NAICS = case_when(freq_max == "brossier" 
                           & str_detect(string = act_new, pattern = "brass") == TRUE ~ 'Manufacturing', # brasseur
                           TRUE ~ NAICS)) %>%
  # specification in "cafe du commerce" freq_max 
  mutate(NAICS = case_when(freq_max == "cafe du commerce" 
                           & str_detect(string = act_new, pattern = "capit|chef") == TRUE ~ "Public Administration", # capitaine from capit.
                           freq_max == "cafe du commerce" 
                           & str_detect(string = act_new, pattern = "cptc") == TRUE ~ "Food stores", # bad ocr of epicier
                           freq_max == "cafe du commerce" 
                           & str_detect(string = act_new, pattern = "capote|chap") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # capotes and chapeau
                           freq_max == "cafe du commerce" 
                           & str_detect(string = act_new, pattern = "captss") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", # bad OCR from tapissier
                           TRUE ~ NAICS)) %>%
  # specification in "cafe-restaurant" freq_max 
  mutate(NAICS = case_when(freq_max == "cafe-restaurant" 
                           & str_detect(string = act_new, pattern = "coif") == TRUE ~ 'Cosmetics, Beauty Supplies, and Perfume Retailers', # coiffeur
                           freq_max == "cafe-restaurant" 
                           & str_detect(string = act_new, pattern = "coffre") == TRUE ~ "Other retailers", # coffres-forts
                           freq_max == "cafe-restaurant" 
                           & str_detect(string = act_new, pattern = "cuivr") == TRUE ~ "Manufacturing", # cuivrage
                           freq_max == "cafe-restaurant" 
                           & str_detect(string = act_new, pattern = "couvr") == TRUE ~ "Construction and Public Works", # couvreur
                           TRUE ~ NAICS))

table_base_review <- table_base_review %>%
  # specification in "cafes" freq_max
  mutate(NAICS = case_when(freq_max == "cafes" 
                           & str_detect(string = act_new, pattern = "chev") == TRUE ~ "Other retailers", # chevaux (also bad OCR of cheveux)
                           freq_max == "cafes" 
                           & str_detect(string = act_new, pattern = "chap") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # chapeaux
                           TRUE ~ NAICS)) %>%
  # specification in "cbeniste" freq_max
  mutate(NAICS = case_when(freq_max == "cbeniste" 
                           & str_detect(string = act_new, pattern = "cabin") == TRUE ~ "Administrative and Legal Services", # cabinet de lecture
                           freq_max == "cbeniste" 
                           & str_detect(string = act_new, pattern = "caf") == TRUE ~ "Full-Service Restaurants", # cafe
                           freq_max == "cbeniste" 
                           & str_detect(string = act_new, pattern = "cap") == TRUE ~ "Public Administration", # capitaine
                           TRUE ~ NAICS)) %>%
  # specification in "chapelier" freq_max
  mutate(NAICS = case_when(freq_max == "chapelier" 
                           & str_detect(string = act_new, pattern = "coif") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers", # coiffeur
                           freq_max == "chapelier" 
                           & str_detect(string = act_new, pattern = "cheval") == TRUE ~ NA, # chevalier: est-ce militaire comme emploi dans les Annuaires?
                           TRUE ~ NAICS)) %>%
  # specification in "charbonnier et vins" freq_max
  mutate(NAICS = case_when(freq_max == "charbonnier et vins" 
                           & str_detect(string = act_new, pattern = "carfon|crepi") == TRUE ~ "Manufacturing", # cartonnier, crepin (cordonnerie)
                           freq_max == "cafe-restaurant" 
                           & str_detect(string = act_new, pattern = "carpen|charpe") == TRUE ~ "Construction and Public Works", # charpentier
                           TRUE ~ NAICS)) %>%
  # specification in "charcutier" freq_max
  mutate(NAICS = case_when(freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "carross|cerct|creuset|cuir|carrea") == TRUE ~ "Manufacturing", # carosserie, cerclage (voiture). cuirs, carreaux
                           freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "crista") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", # cristallerie and cristaux
                           freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "charge") == TRUE 
                           & str_detect(string = act_new, pattern = "langue") == FALSE  ~ "Administrative and Legal Services", # charge d affaires de XX (a place)
                           freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "cours|ecole") == TRUE ~ "Educational Services",
                           freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "corse|corscts|cirage") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # corsets, cirage
                           freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "curios") == TRUE ~ "Other retailers", # curiosite
                           freq_max == "charcutier" 
                           & str_detect(string = act_new, pattern = "correcteur") == TRUE ~ "Publishing Industries", # correcteur
                           TRUE ~ NAICS)) %>%
  # specification in "chaudronnier" freq_max
  mutate(NAICS = case_when(freq_max == "chaudronnier" 
                           & str_detect(string = act_new, pattern = "cxternat") == TRUE ~ "Educational Services", # externat
                           TRUE ~ NAICS)) %>%
  # specification in "chaussures" freq_max
  mutate(NAICS = case_when(freq_max == "chaussures" 
                           & str_detect(string = act_new, pattern = "caissier") == TRUE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "chemisier" freq_max
  mutate(NAICS = case_when(freq_max == "chemisier" 
                           & str_detect(string = act_new, 
                                        pattern = "cassation|greff|tribunal|commissaire|commis") == TRUE ~ "Administrative and Legal Services", 
                           # conseiller cour, commissaire repartiteur adjoint, conservateur des hypotheques, cons. referendaire a la cour des comptes, greffier
                           freq_max == "chemisier" 
                           & str_detect(string = act_new, pattern = "police|potice|pottce|roy.|royal|repartiteur|repart.|hypotheq|referendaire|cour des comptes|conserv. |conservat") == TRUE ~ "Public Administration", # conservateur
                           freq_max == "chemisier" 
                           & str_detect(string = act_new, pattern = "priseur|priscur|changeur") == TRUE ~ "Trade Agents and Brokers", # commissaire-priseur, changeur
                           freq_max == "chemisier" 
                           & str_detect(string = act_new, pattern = "conserve") == TRUE ~ "Food stores", # conserves
                           freq_max == "chemisier" 
                           & str_detect(string = act_new, pattern = "censeur") == TRUE ~ "Educational Services", # censeur lycee, college, etc.
                           TRUE ~ NAICS))




table_base_review <- table_base_review %>%
  # specification in "chirurgien-dentiste" freq_max
  mutate(NAICS = case_when(freq_max == "chirurgien-dentiste" 
                           & str_detect(string = act_new, pattern = "corroyeur") == TRUE ~ "Manufacturing", #corroyeur
                           freq_max == "chirurgien-dentiste" 
                           & str_detect(string = act_new, pattern = "carrie") == TRUE ~ "Construction and Public Works", # carrier
                           freq_max == "chirurgien-dentiste" 
                           & str_detect(string = act_new, pattern = "cirier") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "colffeur" freq_max
  mutate(NAICS = case_when(freq_max == "colffeur" 
                           & str_detect(string = act_new, pattern = "colle|coul") == TRUE ~ "Other retailers", # colle forte
                           TRUE ~ NAICS)) %>%
  # specification in "comestibles" freq_max
  mutate(NAICS = case_when(freq_max == "comestibles" 
                           & str_detect(string = act_new, pattern = "chemister|canne") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", 
                           # bad OCR of chemisier, canne
                           freq_max == "comestibles" 
                           & str_detect(string = act_new, pattern = "chimist|mecanic") == TRUE ~ "Engineering Services", # chimiste, constructeur-mecanicien
                           freq_max == "comestibles" 
                           & str_detect(string = act_new, pattern = "commiss.|priseur|priscur|chang|marchan") == TRUE 
                           & str_detect(string = act_new, pattern = "commissa|police|marine|courriers|hosp|chancetier|chancetter|cons. ") == FALSE ~ "Trade Agents and Brokers", 
                           # bad ocr of commissaire-priseur, change, commissionnaire en marchandises
                           freq_max == "comestibles" 
                           & str_detect(string = act_new, pattern = "commissa|police|marine|courriers|hosp|chancetier|chancetter|cons. ") == TRUE ~ "Public Administration", 
                           # commissaire, chancelier, conseiller au
                           freq_max == "comestibles" 
                           & str_detect(string = act_new, pattern = "voiture|vottures|constructeur|constr") == TRUE ~ "Manufacturing", # constructeur de voitures, constructeur
                           freq_max == "comestibles" 
                           & str_detect(string = act_new, pattern = "coeniste|cheniste|cannage") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", 
                           # bad ocr of ebeniste, cannage de chaise
                           TRUE ~ NAICS)) %>%
  # specification in "confiseur" freq_max
  mutate(NAICS = case_when(freq_max == "confiseur" 
                           & str_detect(string = act_new, pattern = "confection|confectton|confect.|confec.|fection") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # confections pour... dames, messieurs
                           freq_max == "confiseur" 
                           & str_detect(string = act_new, pattern = "composit|compostteur|compostleur|composti") == TRUE ~ "Arts, Entertainment, and Recreation", # compositeur de musique
                           freq_max == "confiseur" 
                           & str_detect(string = act_new, pattern = "compagnie") == TRUE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "conseiller referendaire a la cour des comptes" freq_max
  mutate(NAICS = case_when(freq_max == "conseiller referendaire a la cour des comptes" 
                           & str_detect(string = act_new, pattern = "commiss") == TRUE ~ "Trade Agents and Brokers", # commissionnaires
                           TRUE ~ NAICS)) %>%
  # specification in "conturiere" freq_max
  mutate(NAICS = case_when(freq_max == "conturiere" 
                           & str_detect(string = act_new, pattern = "controleur|control.|controteur|amiral") == TRUE ~ "Public Administration", # controleur des contributions, contre-amiral
                           freq_max == "conturiere" 
                           & str_detect(string = act_new, pattern = "chandron") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", # bad ocr chaudronnier
                           freq_max == "conturiere" 
                           & str_detect(string = act_new, pattern = "chantier|chantter|cintreur") == TRUE ~ "Manufacturing",
                           freq_max == "conturiere" 
                           & str_detect(string = act_new, pattern = "cntrepr") == TRUE ~ "Construction and Public Works", # bad ocr of entreprise de maconnerie, etc
                           TRUE ~ NAICS)) %>%
  # specification in "cordonnier" freq_max
  mutate(NAICS = case_when(freq_max == "cordonnier" 
                           & str_detect(string = act_new, pattern = "carton") == TRUE ~ "Manufacturing", # cartonnier
                           freq_max == "cordonnier" 
                           & str_detect(string = act_new, pattern = "cartomancien") == TRUE ~ 'Arts, Entertainment, and Recreation', # cartomancienne
                           freq_max == "cordonnier" 
                           & str_detect(string = act_new, pattern = "cure de") == TRUE ~ NA, 
                           freq_max == "cordonnier" 
                           & str_detect(string = act_new, pattern = "court") == TRUE ~ "Trade Agents and Brokers", # courtier
                           TRUE ~ NAICS))


table_base_review <- table_base_review %>%
  # specification in "corroyeur" freq_max
  mutate(NAICS = case_when(freq_max == "corroyeur" 
                           & str_detect(string = act_new, pattern = "courrier") == TRUE ~ "Public Administration",
                           TRUE ~ NAICS)) %>%
  # specification in "couleurs" freq_max
  mutate(NAICS = case_when(freq_max == "couleurs" 
                           & str_detect(string = act_new, pattern = "collier") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           freq_max == "couleurs" 
                           & str_detect(string = act_new, pattern = " et ") == TRUE ~ "Wholesalers Trade",
                           TRUE ~ NAICS)) %>%
  # specification in "couteurs et vernis" freq_max
  mutate(NAICS = case_when(freq_max == "couteurs et vernis" 
                           & str_detect(string = act_new, pattern = "couteur|cotoriste") == TRUE 
                           & str_detect(string = act_new, pattern = " ") == FALSE ~ "Arts, Entertainment, and Recreation",
                           freq_max == "couteurs et vernis" 
                           & str_detect(string = act_new, pattern = "cadre") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           freq_max == "couteurs et vernis" 
                           & str_detect(string = act_new, pattern = "cidre") == TRUE ~ "Food stores",
                           freq_max == "couteurs et vernis" 
                           & str_detect(string = act_new, pattern = "couturicre|cottur|couturi|cottier") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", 
                           # bad ocr of couturiere and collier
                           freq_max == "couteurs et vernis" 
                           & str_detect(string = act_new, pattern = "cutrs|chaudieres") == TRUE ~ "Manufacturing", # bad ocr of cuirs et crepins
                           freq_max == "couteurs et vernis" 
                           & str_detect(string = act_new, pattern = "directeur") == TRUE ~ NA, 
                           TRUE ~ NAICS)) %>%
  # specification in "couturiere" freq_max
  mutate(NAICS = case_when(freq_max == "couturiere" 
                           & str_detect(string = act_new, pattern = "chtrur|cttrurg|ctururg|catrur") == TRUE ~ "Health Practitioners", # bad ocr of chirurgien-dentiste
                           TRUE ~ NAICS)) %>%
  # specification in "cpicier" freq_max
  mutate(NAICS = case_when(freq_max == "cpicier" 
                           & str_detect(string = act_new, pattern = "coiffcur|coufcur|coifcur|coi fcur|coif") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers",
                           freq_max == "cpicier" 
                           & str_detect(string = act_new, pattern = "cafe|cafc|cremerie") == TRUE ~ "Full-Service Restaurants",
                           freq_max == "cpicier" 
                           & str_detect(string = act_new, pattern = "chapcaur") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # bad ocr of chapeau
                           freq_max == "cpicier" 
                           & str_detect(string = act_new, pattern = "chef") == TRUE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "cpicter" freq_max
  mutate(NAICS = case_when(freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "coiff|parfum") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers",
                           freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "cafe|cafc|cufe|cufc") == TRUE ~ "Full-Service Restaurants",
                           freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "chapeau|cheveu") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "chef|chapctain") == TRUE ~ NA,
                           freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "chauffage") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "chevaux") == TRUE ~ "Building and Car Rental", # chevaux et voitures
                           freq_max == "cpicter" 
                           & str_detect(string = act_new, pattern = "copics|copie|copiste") == TRUE ~ "Publishing Industries", # copies et copiste
                           TRUE ~ NAICS)) %>%
  # specification in "dentelles" freq_max
  mutate(NAICS = case_when(freq_max == "dentelles" 
                           & str_detect(string = act_new, pattern = "facul") == TRUE ~ "Educational Services", # doyen de la faculte
                           TRUE ~ NAICS)) %>%
  # specification in "dentiste" freq_max
  mutate(NAICS = case_when(freq_max == "dentiste" 
                           & str_detect(string = act_new, pattern = "profess") == TRUE ~ "Educational Services", # doyen des professeurs
                           freq_max == "dentiste" 
                           & str_detect(string = act_new, pattern = "avoc") == TRUE ~ "Administrative and Legal Services", # doyen des avocats
                           freq_max == "dentiste" 
                           & str_detect(string = act_new, pattern = "doyen") == TRUE 
                           & str_detect(string = act_new, pattern = "avoc|profess") == FALSE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "depute du nord" freq_max
  mutate(NAICS = case_when(freq_max == "depute du nord" 
                           & str_detect(string = act_new, pattern = "the") == TRUE ~ "Food stores",
                           freq_max == "depute du nord" 
                           & str_detect(string = act_new, pattern = "tabac|depot") == TRUE ~ "Other retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "dessinateur" freq_max
  mutate(NAICS = case_when(freq_max == "dessinateur" 
                           & str_detect(string = act_new, pattern = "dess") == FALSE ~ NA,
                           TRUE ~ NAICS))


table_base_review <- table_base_review %>%
  # specification in "docteur-medecin" freq_max
  mutate(NAICS = case_when(freq_max == "docteur-medecin" 
                           & str_detect(string = act_new, pattern = "en droi|en drot|en dron|en aroi|en arot|en arou|en drou|en drut") == TRUE ~ "Administrative and Legal Services", #docteur en droit
                           freq_max == "docteur-medecin" 
                           & str_detect(string = act_new, pattern = "travaux") == TRUE ~ "Construction and Public Works",
                           freq_max == "docteur-medecin" 
                           & str_detect(string = act_new, pattern = "imprimes|impr.") == TRUE ~ "Publishing Industries",
                           freq_max == "docteur-medecin" 
                           & str_detect(string = act_new, pattern = "automa") == TRUE ~ "Other retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "ebeniste" freq_max
  mutate(NAICS = case_when(freq_max == "ebeniste" 
                           & str_detect(string = act_new, pattern = "epong|eping") == TRUE ~ "Other retailers", # eponges et epinglier
                           TRUE ~ NAICS)) %>%
  # specification in "emballeur" freq_max
  mutate(NAICS = case_when(freq_max == "emballeur" 
                           & str_detect(string = act_new, pattern = "banque") == TRUE ~ "Finance and Insurance", # employe a la banque
                           freq_max == "emballeur" 
                           & str_detect(string = act_new, pattern = "employe|empl.|emplove") == TRUE 
                           & str_detect(string = act_new, pattern = "banque") == FALSE ~ "Public Administration", 
                           # employe
                           freq_max == "emballeur" 
                           & str_detect(string = act_new, pattern = "emploi|enfileu|enfile|enveloppes") == TRUE ~ "Manufacturing", # emploi general de caoutchou, enfileuse de perles
                           TRUE ~ NAICS)) %>%
  # specification in "epicter" freq_max
  mutate(NAICS = case_when(freq_max == "epicter" 
                           & str_detect(string = act_new, pattern = "eveque") == TRUE ~ NA,
                           TRUE ~ NAICS)) %>%
  # specification in "estampes" freq_max
  mutate(NAICS = case_when(freq_max == "estampes" 
                           & str_detect(string = act_new, pattern = "ecote") == TRUE ~ "Educational Services",
                           TRUE ~ NAICS)) %>%
  # specification in "facteur de pianos" freq_max
  mutate(NAICS = case_when(freq_max == "facteur de pianos" 
                           & str_detect(string = act_new, pattern = "puano|accordeur|piano|ptano|plano|orgue|cordeon|harpes|pranos|pia- nos|instrume|flute") == FALSE ~ "Trade Agents and Brokers",
                           TRUE ~ NAICS)) %>%
  # specification in "fromages en gros" freq_max
  mutate(NAICS = case_when(freq_max == "fromages en gros" 
                           & str_detect(string = act_new, pattern = "frange|forme|fournea|fourncau|freins") == TRUE ~ "Other retailers",
                           freq_max == "fromages en gros" 
                           & str_detect(string = act_new, pattern = "f. |frangier") == TRUE ~ "Manufacturing", # fabricant de
                           freq_max == "fromages en gros" 
                           & str_detect(string = act_new, pattern = "fourncur|fournisse") == TRUE ~ "Wholesalers Trade", # fournisseur
                           freq_max == "fromages en gros" 
                           & str_detect(string = act_new, pattern = "francois|francaise|francatse|franco-") == TRUE ~NA, # societe ? francaise de
                           TRUE ~ NAICS)) %>%
  # specification in "fruitier" freq_max
  mutate(NAICS = case_when(freq_max == "fruitier" 
                           & str_detect(string = act_new, pattern = "f. |fahr.|faor.|ford") == TRUE ~ "Manufacturing", # bad ocr of fab. de
                           freq_max == "fruitier" 
                           & str_detect(string = act_new, pattern = "fouruitures") == TRUE ~ "Wholesalers Trade", # bad ocr of fourniture
                           TRUE ~ NAICS)) %>%
  # specification in "grainetier" freq_max
  mutate(NAICS = case_when(freq_max == "grainetier" 
                           & str_detect(string = act_new, pattern = "hotel|hotet|botel|grand h|grand k|grand not|taverne") == TRUE ~ "Building and Car Rental", 
                           freq_max == "grainetier" 
                           & str_detect(string = act_new, pattern = "gerant|rabbin|bains") == TRUE ~ NA,
                           freq_max == "grainetier" 
                           & str_detect(string = act_new, pattern = "grand ba|grand de|magasms") == TRUE ~ "Other retailers",  # grand bazar, depot
                           freq_max == "grainetier" 
                           & str_detect(string = act_new, pattern = "grand bo|cafe|ca fe|cafc|caft") == TRUE ~ "Full-Service Restaurants",  # bouillon, cafe
                           freq_max == "grainetier" 
                           & str_detect(string = act_new, pattern = "brasserie|distillerie|fabr.|fabri") == TRUE ~ "Manufacturing",
                           freq_max == "grainetier" 
                           & str_detect(string = act_new, pattern = "chancelier") == TRUE ~ "Public Administration",
                           TRUE ~ NAICS)) %>%
  # specification in "grains et fourrages" freq_max
  mutate(NAICS = case_when(freq_max == "grains et fourrages" 
                           & str_detect(string = act_new, pattern = "garniss") == TRUE ~ "Manufacturing", # garnisseur de
                           freq_max == "grains et fourrages" 
                           & str_detect(string = act_new, pattern = "gerance|gerans") == TRUE ~ "Trade Agents and Brokers", # gerance de propriete, d'immeuble
                           TRUE ~ NAICS))



table_base_review <- table_base_review %>%
  # specification in "graveur sur metaux" freq_max
  mutate(NAICS = case_when(freq_max == "graveur sur metaux" 
                           & str_detect(string = act_new, pattern = "greffier|greifier") == TRUE ~ "Administrative and Legal Services",
                           TRUE ~ NAICS)) %>%
  # specification in "h. de lettres" freq_max
  mutate(NAICS = case_when(freq_max == "h. de lettres" 
                           & str_detect(string = act_new, pattern = "hotel|hdtel|hatel|hiotel|hitel|hutel") == TRUE ~ "Building and Car Rental",
                           TRUE ~ NAICS)) %>%
  # specification in "h. garni" freq_max
  mutate(NAICS = case_when(freq_max == "h. garni" 
                           & str_detect(string = act_new, pattern = "huissier") == TRUE ~ "Administrative and Legal Services",
                           TRUE ~ NAICS)) %>%
  # specification in "hortoger" freq_max
  mutate(NAICS = case_when(freq_max == "hortoger" 
                           & str_detect(string = act_new, pattern = "horticu|hortcu|horti|horttcu|hor ti") == TRUE ~ "Related to Agricultural Activities", # horticulteur
                           TRUE ~ NAICS)) %>%
  # specification in "hortoger" freq_max
  mutate(NAICS = case_when(freq_max == "hotel garni" 
                           & str_detect(string = act_new, pattern = "hutles") == TRUE ~ "Other retailers", # bad ocr huiles et graisses
                           TRUE ~ NAICS)) %>%
  # specification in "huissier" freq_max
  mutate(NAICS = case_when(freq_max == "huissier" 
                           & str_detect(string = act_new, pattern = "gar") == TRUE ~ "Building and Car Rental", # bad ocr h. garni
                           TRUE ~ NAICS)) %>%
  # specification in "imprimeur-lithographe" freq_max
  mutate(NAICS = case_when(freq_max == "imprimeur-lithographe" 
                           & str_detect(string = act_new, pattern = "importa|importithion|importuieur") == TRUE ~ "Wholesalers Trade", # importateur
                           freq_max == "imprimeur-lithographe" 
                           & str_detect(string = act_new, pattern = "impermeab") == TRUE ~ "Manufacturing", # impermeabilisation
                           TRUE ~ NAICS)) %>%
  # specification in "ingenieur" freq_max
  mutate(NAICS = case_when(freq_max == "ingenieur" 
                           & str_detect(string = act_new, pattern = "assur|vie|incend") == TRUE ~ "Finance and Insurance", # incendie assurances, incendie et sur la vie
                           TRUE ~ NAICS))




table_base_review <- table_base_review %>%
  # specification in "inspecteur des finances" freq_max
  mutate(NAICS = case_when(freq_max == "inspecteur des finances" 
                           & str_detect(string = act_new, pattern = "ing |ing.|ingd|ingt") == TRUE ~ "Engineering Services", # ingenieur
                           TRUE ~ NAICS)) %>%
  # specification in "institution" freq_max
  mutate(NAICS = case_when(freq_max == "institution" 
                           & str_detect(string = act_new, pattern = "ing |ing.") == TRUE ~ "Engineering Services",
                           freq_max == "institution" 
                           & str_detect(string = act_new, pattern = "instrume|trument|instr.|instru") == TRUE ~ "Other retailers", # instruments de..
                           freq_max == "institution" 
                           & str_detect(string = act_new, pattern = "installation") == TRUE ~ NA, # place for... something
                           TRUE ~ NAICS)) %>%
  # specification in "liquoriste" freq_max
  mutate(NAICS = case_when(freq_max == "liquoriste" 
                           & str_detect(string = act_new, pattern = "loucurs") == TRUE ~ "Building and Car Rental", # bad ocr loueur de voitures
                           TRUE ~ NAICS)) %>%
  # specification in "macon" freq_max
  mutate(NAICS = case_when(freq_max == "macon" 
                           & str_detect(string = act_new, pattern = "me ccin|meac|meaecin|meec") == TRUE ~ "Health Practitioners", # bad ocr medecin
                           freq_max == "macon" 
                           & str_detect(string = act_new, pattern = "mecan") == TRUE ~ "Engineering Services", # mecanicien abbr
                           TRUE ~ NAICS)) %>%
  # specification in "maison meublee" freq_max
  mutate(NAICS = case_when(freq_max == "maison meublee" 
                           & str_detect(string = act_new, pattern = "macon") == TRUE ~ "Construction and Public Works", # macon
                           freq_max == "maison meublee" 
                           & str_detect(string = act_new, pattern = "mecan") == TRUE ~ "Engineering Services", # mecanicien abbr
                           freq_max == "maison meublee" 
                           & str_detect(string = act_new, pattern = "machine") == TRUE ~ "Other retailers",
                           freq_max == "maison meublee" 
                           & str_detect(string = act_new, pattern = "meac|meaecin|meec") == TRUE ~ "Health Practitioners", # bad ocr medecin
                           TRUE ~ NAICS)) %>%
  # specification in "march. de meubles" freq_max
  mutate(NAICS = case_when(freq_max == "march. de meubles" 
                           & str_detect(string = act_new, pattern = "bonneterie|casquettes|chale|chap|chauss|aussure|coiffe|fan|mode|parapl|parapt|sabo|toilet|tottel|totle|tottet|toiette|toitet|habi|merc.|merct|tailleu") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # bad ocr mercier
                           freq_max == "march. de meubles" 
                           & str_detect(string = act_new, pattern = "meubl|bois a bruler|bois a ruvrer|chaise|charb|chiff|drap|faience|falence|fourru|linger|matel|meubie|soiri") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", 
                           freq_max == "march. de meubles" 
                           & str_detect(string = act_new, pattern = "beurre|ca |cafc|cafe|legum|poiss|pomme|sauciss|sel|the|vins|abat|fromag|volail|volait|scargo|taba") == TRUE ~ "Food stores", 
                           freq_max == "march. de meubles" 
                           & str_detect(string = act_new, pattern = "taba") == TRUE ~ "Other retailers", 
                           freq_max == "march. de meubles" 
                           & str_detect(string = act_new, pattern = "ferrant|serrant|jerrant|marechart") == TRUE ~ "Manufacturing", TRUE ~ NAICS)) %>%
  # specification in "maroquinerie" freq_max
  mutate(NAICS = case_when(freq_max == "maroquinerie" 
                           & str_detect(string = act_new, pattern = "bonneterie|casquettes|chale|chap|chauss|aussure|coiffe|mode|parapl|parapt|sabo|toilet|tottel|totle|tottet|toiette|toitet|habi|merc.|merct|merce|tailleu") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           freq_max == "maroquinerie" 
                           & str_detect(string = act_new, pattern = "mrcanicicn") == TRUE ~ "Manufacturing", #bad ocr mecanicien
                           freq_max == "maroquinerie" 
                           & str_detect(string = act_new, pattern = "meubl|bois a bruler|bois a ruvrer|chaise|charb|chiff|drap|faience|falence|fourru|linger|matel|meubie|soiri") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", 
                           freq_max == "maroquinerie" 
                           & str_detect(string = act_new, pattern = "beurre|ca |cafc|cafe|legum|poiss|pomme|sauciss|sel|the|vins|abat|fromag|volail|volait|tailleu|scargo") == TRUE ~ "Food stores", 
                           freq_max == "maroquinerie" 
                           & str_detect(string = act_new, pattern = "taba") == TRUE ~ "Other retailers", 
                           TRUE ~ NAICS))


table_base_review <- table_base_review %>%
  # specification in "mecanicien" freq_max
  mutate(NAICS = case_when(freq_max == "mecanicien" 
                           & str_detect(string = act_new, pattern = "macon") == TRUE ~ "Construction and Public Works", 
                           freq_max == "mecanicien" 
                           & str_detect(string = act_new, pattern = "macon") == TRUE ~ NA, # maison speciale as compagnie
                           freq_max == "mecanicien" 
                           & str_detect(string = act_new, pattern = "maison gar|maison m") == TRUE ~ "Building and Car Rental", # maison garnie, maison meublee
                           freq_max == "mecanicien" 
                           & str_detect(string = act_new, pattern = "ma- chine|machi|muchin") == TRUE ~ "Other retailers",
                           freq_max == "mecanicien" & str_detect(string = act_new, pattern = "ma- chine|machi|muchin") == FALSE
                           & str_detect(string = act_new, pattern = "coudre|condre") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", # machine a coudre
                           TRUE ~ NAICS)) %>%
  # specification in "medecin" freq_max
  mutate(NAICS = case_when(freq_max == "medecin" 
                           & str_detect(string = act_new, pattern = "mdcon") == TRUE ~ "Construction and Public Works", # bad ocr macon
                           freq_max == "medecin" 
                           & str_detect(string = act_new, pattern = "mats|mats.|matson|mdison|mutson|matison") == TRUE ~ "Building and Car Rental", # bad ocr maison meuble
                           freq_max == "medecin" 
                           & str_detect(string = act_new, pattern = "metaux") == TRUE ~ "Manufacturing",
                           freq_max == "medecin"
                           & str_detect(string = act_new, pattern = "mode|modc") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", 
                           TRUE ~ NAICS)) %>%
  # specification in "medectn" freq_max
  mutate(NAICS = case_when(freq_max == "medectn" 
                           & str_detect(string = act_new, pattern = "md de|mds de") == TRUE ~ "Other retailers", # abbr. of marchands de
                           freq_max == "medectn" 
                           & str_detect(string = act_new, pattern = "modcteur") == TRUE ~ "Arts, Entertainment, and Recreation", # bad ocr of modeleur
                           freq_max == "medectn" 
                           & str_detect(string = act_new, pattern = "metaux") == TRUE ~ "Manufacturing",
                           freq_max == "medectn"
                           & str_detect(string = act_new, pattern = "modiste|modes") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", 
                           TRUE ~ NAICS)) %>%
  # specification in "menuisier" freq_max
  mutate(NAICS = case_when(freq_max == "menuisier" 
                           & str_detect(string = act_new, pattern = "manucur") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers", # manucure
                           freq_max == "menuisier" 
                           & str_detect(string = act_new, pattern = "menceri") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # bad ocr of mercerie
                           TRUE ~ NAICS)) %>%
  # specification in "menuister" freq_max
  mutate(NAICS = case_when(freq_max == "menuister" 
                           & str_detect(string = act_new, pattern = "ministre") == TRUE ~ "Public Administration",
                           freq_max == "menuister" 
                           & str_detect(string = act_new, pattern = "manege|mancege") == TRUE ~ "Arts, Entertainment, and Recreation",
                           freq_max == "menuister" 
                           & str_detect(string = act_new, pattern = "mine|minc") == TRUE ~ "Trade Agents and Brokers",
                           freq_max == "menuister" 
                           & str_detect(string = act_new, pattern = "institution|instttution|institutr") == TRUE ~ "Educational Services", # bad ocr of mercerie
                           freq_max == "menuister" 
                           & str_detect(string = act_new, pattern = "acture") == TRUE ~ "Manufacturing", # bad ocr of manufacture with white space (manu acture)
                           freq_max == "menuister" & str_detect(string = act_new, pattern = "bronze|caoutchou|selleri|sieg|suspens|tube|bross") == FALSE
                           & str_detect(string = act_new, pattern = "manus de|manus.") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # manus. as manual creation
                           freq_max == "menuister" 
                           & str_detect(string = act_new, pattern = "bronze|caoutchou|selleri|sieg|suspens|tube|bross") == TRUE ~ "Manufacturing",
                           TRUE ~ NAICS))




table_base_review <- table_base_review %>%
  # specification in "mercerie" freq_max
  mutate(NAICS = case_when(freq_max == "mercerie" 
                           & str_detect(string = act_new, pattern = "maraicher") == TRUE ~ "Related to Agricultural Activities", 
                           freq_max == "mercerie" 
                           & str_detect(string = act_new, pattern = "marcchaur|marecaai|marech.|marechai") == TRUE ~ "Manufacturing", # bad ocr of marrechal ferrand
                           freq_max == "mercerie" 
                           & str_detect(string = act_new, pattern = "march.") == TRUE ~ "Other retailers", # abbr. of marchand
                           TRUE ~ NAICS)) %>%
  # specification in "miroitier" freq_max
  mutate(NAICS = case_when(freq_max == "miroitier" 
                           & str_detect(string = act_new, pattern = "maire|maitr") == TRUE ~ "Public Administration", 
                           TRUE ~ NAICS)) %>%
  # specification in "modes" freq_max
  mutate(NAICS = case_when(freq_max == "modes" 
                           & str_detect(string = act_new, pattern = "taux|metau") == TRUE ~ "Manufacturing", 
                           freq_max == "modes" 
                           & str_detect(string = act_new, pattern = "medec.|medeci|medecu|medec|medcc|medce|dec.|madec") == TRUE ~ "Health Practitioners", 
                           TRUE ~ NAICS)) %>%
  # specification in "negts" freq_max
  mutate(NAICS = case_when(freq_max == "negts" 
                           & str_detect(string = act_new, pattern = "ncttoyage") == TRUE ~ NA, 
                           freq_max == "negts" 
                           & str_detect(string = act_new, pattern = "nouccautes|nouveau|nouucaut|noycau|ucaute|nousc") == TRUE ~ "Other retailers", #bad ocr of nouveautes
                           TRUE ~ NAICS)) %>%
  # specification in "nourrisseur" freq_max
  mutate(NAICS = case_when(freq_max == "nourrisseur" 
                           & str_detect(string = act_new, pattern = "norioger|noriwge|norworg") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", # bad ocr of horloger 
                           freq_max == "nourrisseur" 
                           & str_detect(string = act_new, pattern = "merceri|nercerie|nercier|merccr|merecr|nerec") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # mercerie
                           TRUE ~ NAICS)) %>%
  # specification in "opticien" freq_max
  mutate(NAICS = case_when(freq_max == "opticien" 
                           & str_detect(string = act_new, pattern = "sante") == TRUE ~ "Health Practitioners", # officier de sante
                           freq_max == "opticien" 
                           & str_detect(string = act_new, pattern = "garde") == TRUE ~ NA, # office de garde-meuble
                           freq_max == "opticien" 
                           & str_detect(string = act_new, pattern = "place") == TRUE ~ "Finance and Insurance", # office de placement
                           TRUE ~ NAICS)) %>%
  # specification in "papiers peints" freq_max
  mutate(NAICS = case_when(freq_max == "papiers peints" 
                           & str_detect(string = act_new, pattern = "pcints|peilht|peint|peit|pemt|petnis|peuits|pewt") == TRUE ~ "Wholesalers Trade",
                           freq_max == "papiers peints" 
                           & str_detect(string = act_new, pattern = "cigarette|cigaretle") == TRUE ~ "Other retailers",
                           freq_max == "papiers peints" 
                           & str_detect(string = act_new, pattern = "paveur") == TRUE ~ "Construction and Public Works",
                           freq_max == "papiers peints" 
                           &freq_max == "papiers peints" 
                           & str_detect(string = act_new, pattern = "propric") == TRUE ~ "No Activity, Living of Income", # bad ocr of proprietaire
                           freq_max == "papiers peints" 
                           & str_detect(string = act_new, pattern = "negecier|negocier") == TRUE ~ "Trade Agents and Brokers", # pouvoir speciale de negocier les mariages
                           TRUE ~ NAICS))




table_base_review <- table_base_review %>%
  # specification in "parapluies" freq_max
  mutate(NAICS = case_when(freq_max == "parapluies" 
                           & str_detect(string = act_new, pattern = "prop") == TRUE ~ "No Activity, Living of Income", 
                           freq_max == "parapluies" 
                           & str_detect(string = act_new, pattern = "prof") == TRUE ~ "Educational Services", 
                           freq_max == "parapluies" 
                           & str_detect(string = act_new, pattern = "fleur|fteur") == TRUE ~ "Other retailers", 
                           TRUE ~ NAICS)) %>%
  # specification in "paraptutes" freq_max
  mutate(NAICS = case_when(freq_max == "parapluies" 
                           & str_detect(string = act_new, pattern = "prop") == TRUE ~ "No Activity, Living of Income", 
                           freq_max == "parapluies" 
                           & str_detect(string = act_new, pattern = "prof") == TRUE ~ "Educational Services", 
                           freq_max == "parapluies" 
                           & str_detect(string = act_new, pattern = "fleur|fteur") == TRUE ~ "Other retailers", 
                           TRUE ~ NAICS)) %>%
  # specification in "paraptutes" freq_max
  mutate(NAICS = case_when(freq_max == "paraptutes" 
                           & str_detect(string = act_new, pattern = "prof|etude|tarpe|ctude|tycee") == TRUE ~ "Educational Services", 
                           freq_max == "paraptutes" & str_detect(string = act_new, pattern = "prof|etude|tarpe|ctude|tycee") == FALSE # because prefet du college
                           & str_detect(string = act_new, pattern = "prefet|pref.|maire|prafet|prifet") == TRUE ~ "Public Administration", 
                           freq_max == "paraptutes" 
                           & str_detect(string = act_new, pattern = "propiet|propt|prop.") == TRUE ~ "No Activity, Living of Income", 
                           freq_max == "paraptutes" 
                           & str_detect(string = act_new, pattern = "parf.|parfit|parfut") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers", # abbr or bad ocr of parfumeur
                           TRUE ~ NAICS)) %>%
  # specification in "parfumeur" freq_max
  mutate(NAICS = case_when(freq_max == "parfumeur" 
                           & str_detect(string = act_new, pattern = "prof.|prof") == TRUE ~ "Educational Services", 
                           freq_max == "parfumeur"
                           & str_detect(string = act_new, pattern = "provincia") == TRUE ~ NA, 
                           freq_max == "parfumeur" 
                           & str_detect(string = act_new, pattern = "propnietaire|prop.|prop") == TRUE ~ "No Activity, Living of Income", 
                           freq_max == "parfumeur" 
                           & str_detect(string = act_new, pattern = "papapi|papape|parapfn") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # bad ocr of parapluie
                           TRUE ~ NAICS)) %>%
  # specification in "patissier" freq_max
  mutate(NAICS = case_when(freq_max == "patissier" 
                           & str_detect(string = act_new, pattern = "paotogra|pfotogr|phdtogr|tograveur|phetogr|togravur|ograplc|photo|poho") == TRUE ~ "Arts, Entertainment, and Recreation", 
                           freq_max == "patissier"
                           & str_detect(string = act_new, pattern = "potcri|poticr|potisscur|potisseu|pottsscur|pottsseur|ptaqueur") == TRUE ~ "Manufacturing",
                           # bad ocr of potier, polisseur, plaqueur
                           freq_max == "patissier" 
                           & str_detect(string = act_new, pattern = "pedicur|pedtcur|paetcur") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers", # pedicure
                           freq_max == "patissier" 
                           & str_detect(string = act_new, pattern = "restaura|trait|trat") == TRUE ~ "Full-Service Restaurants",
                           freq_max == "patissier" 
                           & str_detect(string = act_new, pattern = "ptaceur") == TRUE ~ "Trade Agents and Brokers", # bad ocr of placeur
                           TRUE ~ NAICS)) %>%
  # specification in "peintre-artiste" freq_max
  mutate(NAICS = case_when(freq_max == "peintre-artiste" 
                           & str_detect(string = act_new, pattern = "batim|bat.|balim|barim|bdtim") == TRUE ~ "Construction and Public Works", 
                           freq_max == "peintre-artiste"
                           & str_detect(string = act_new, pattern = "voitu|armoi|panter") == TRUE ~ "Manufacturing", # peintre en voiture, armoire et panier
                           TRUE ~ NAICS)) %>%
  # specification in "pensionnat" freq_max
  mutate(NAICS = case_when(freq_max == "pensionnat" 
                           & str_detect(string = act_new, pattern = "chevau") == TRUE ~ "Related to Agricultural Activities", 
                           freq_max == "pensionnat"
                           & str_detect(string = act_new, pattern = "mecan|ingenie") == TRUE ~ "Engineering Services",
                           freq_max == "pensionnat"
                           & str_detect(string = act_new, pattern = "sante") == TRUE ~ "Health Practitioners",
                           freq_max == "pensionnat" & str_detect(string = act_new, pattern = "pensionnat|pansionnat") == FALSE
                           & str_detect(string = act_new, pattern = "pension|pansion") == TRUE ~ "Building and Car Rental",
                           TRUE ~ NAICS)) %>%
  # specification in "pharmacien" freq_max
  mutate(NAICS = case_when(freq_max == "pharmacien" 
                           & str_detect(string = act_new, pattern = "prince") == TRUE ~ "No Activity, Living of Income", 
                           freq_max == "pharmacien"
                           & str_detect(string = act_new, pattern = "prumassie|prume|prano|pouare|hommes|machine") == TRUE ~ "Other retailers", 
                           # bad ocr plumassier, plume, piano, pour... 
                           freq_max == "pharmacien"
                           & str_detect(string = act_new, pattern = "prem. secrt.|avoue|agree|notatre|notaire|avoc") == TRUE ~ "Administrative and Legal Services",
                           freq_max == "pharmacien" 
                           & str_detect(string = act_new, pattern = "primceur|promages") == TRUE ~ "Food stores", # bad ocr primeur and fromages
                           TRUE ~ NAICS)) %>%
  # specification in "plumassier" freq_max
  mutate(NAICS = case_when(freq_max == "plumassier" 
                           & str_detect(string = act_new, pattern = "paillon") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # link to orfevrerie
                           freq_max == "plumassier"
                           & str_detect(string = act_new, pattern = "plancu") == TRUE ~ "Manufacturing", # bad ocr of planeur de ...
                           freq_max == "plumassier"
                           & str_detect(string = act_new, pattern = "planis|plans") == TRUE ~ "Arts, Entertainment, and Recreation",  # bad ocr of pianiste and plans et reliefs
                           TRUE ~ NAICS))



table_base_review <- table_base_review %>%
  # specification in "porcetaines et cristaux" freq_max
  mutate(NAICS = case_when(freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "parquet|pierre|marbres|bitume|perstenne") == TRUE ~ "Construction and Public Works",
                           # specificity
                           freq_max == "porcetaines et cristaux" & str_detect(string = act_new, pattern = "cours|president de la") == FALSE
                           & str_detect(string = act_new, pattern = "cour|royau|avoue|tribun|compte|trib.|chambre|biblio|cham-|la commiss|republi|gislation") == TRUE ~ "Public Administration",
                           freq_max == "porcetaines et cristaux" & str_detect(string = act_new, pattern = "cour|royau|avoue|tribun|compte|trib.|chambre|bibliot|cham-|la commiss|republi|gislation") == FALSE
                           & str_detect(string = act_new, pattern = "president|pres.|pres") == TRUE ~ "Administrative and Legal Services",
                           freq_max == "porcetaines et cristaux" & str_detect(string = act_new, pattern = "cours|president de la") == FALSE
                           & str_detect(string = act_new, pattern = "banque|mutuali|mutuati|mutue|prevoya") == TRUE ~ "Finance and Insurance",
                           # usual
                           freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "billards|parasot") == TRUE ~ "Other retailers", # bad ocr parasolerie
                           freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "prestidigitateur") == TRUE ~ "Arts, Entertainment, and Recreation",
                           freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "paristen") == TRUE ~ NA,
                           freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "restaura") == TRUE ~ "Full-Service Restaurants",
                           freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "bijou|bouton|perle|perruq") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           freq_max == "porcetaines et cristaux"
                           & str_detect(string = act_new, pattern = "litho|titho|thograp|imprim") == TRUE ~ "Publishing Industries",
                           freq_max == "porcetaines et cristaux" 
                           & str_detect(string = act_new, pattern = "pro ess|pros.") == TRUE ~ "Educational Services",
                           TRUE ~ NAICS)) %>%
  # specification in "prop." freq_max
  mutate(NAICS = case_when(freq_max == "prop." 
                           & str_detect(string = act_new, pattern = "parap") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # parapluie
                           freq_max == "prop."
                           & str_detect(string = act_new, pattern = "prof.") == TRUE ~ "Educational Services", # professeur
                           TRUE ~ NAICS)) %>%
  # specification in "proprietaire" freq_max
  mutate(NAICS = case_when(freq_max == "proprietaire" 
                           & str_detect(string = act_new, pattern = "parap") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers", # parapluie
                           freq_max == "proprietaire"
                           & str_detect(string = act_new, pattern = "pair") == TRUE ~ "Public Administration", # pair
                           freq_max == "proprietaire"
                           & str_detect(string = act_new, pattern = "parf") == TRUE ~ "Cosmetics, Beauty Supplies, and Perfume Retailers", # parfumeur
                           freq_max == "proprietaire"
                           & str_detect(string = act_new, pattern = "perfor") == TRUE ~ "Manufacturing", # perforeur
                           freq_max == "proprietaire"
                           & str_detect(string = act_new, pattern = "prepar|prof") == TRUE ~ "Educational Services", # preparateur or professeur
                           TRUE ~ NAICS)) %>%
  # specification in "receveur de rentes" freq_max
  mutate(NAICS = case_when(freq_max == "receveur de rentes" 
                           & str_detect(string = act_new, pattern = "rentes") == TRUE ~ "Finance and Insurance", 
                           freq_max == "receveur de rentes" ~ "Public Administration", 
                           freq_max == "receveur de rentes" 
                           & str_detect(string = act_new, pattern = "resfau") == TRUE ~ "Full-Service Restaurants", # bad ocr restaurant
                           freq_max == "receveur de rentes" 
                           & str_detect(string = act_new, pattern = "rieux|roues") == TRUE ~ "Manufacturing", 
                           TRUE ~ NAICS))


table_base_review <- table_base_review %>%
  # specification in "rentier" freq_max
  mutate(NAICS = case_when(freq_max == "rentier" 
                           & str_detect(string = act_new, pattern = "rentrayeur|reintur|rinture|roun|rouen|room|roon") == TRUE ~ "Manufacturing", # rentrayeur, bad ocr of teinturerie, rouennerie
                           freq_max == "rentier" 
                           & str_detect(string = act_new, pattern = "reintre|tableau") == TRUE ~ "Arts, Entertainment, and Recreation", # bad ocr of peintre (tableau)
                           TRUE ~ NAICS)) %>%
  # specification in "representant de commerce" freq_max
  mutate(NAICS = case_when(freq_max == "representant de commerce" 
                           & str_detect(string = act_new, pattern = "reperceuse|reperccu|reperceur|repriseus|reperc.|repercen|raprise|raveur") == TRUE ~ "Manufacturing", 
                           # reperceur et reperceuse, repriseuse, bad ocr of graveur sur metaux
                           freq_max == "representant de commerce" 
                           & str_detect(string = act_new, pattern = "refer. c. des comptes|comptes|peuple|sceau|refer. chanc") == TRUE ~ "Public Administration", 
                           # referendaire a la cour des comptes, referant du peuple, referendaire au sceau, referendaire chancellerie
                           TRUE ~ NAICS)) %>%
  # specification in "restaurateur" freq_max
  mutate(NAICS = case_when(freq_max == "restaurateur" 
                           & str_detect(string = act_new, pattern = "tableaux|tapisserie|antiquite|lableaux|livre|meuble|porcelaine|armure|ta-|lapisse|lapissc|tapisser") == TRUE ~ "Manufacturing", 
                           # restaurateur de tableaux, tapisserie, porcelaine, livre, meuble, etc.
                           freq_max == "restaurateur" 
                           & str_detect(string = act_new, pattern = "ministere|poste") == TRUE ~ "Public Administration", 
                           freq_max == "restaurateur" 
                           & str_detect(string = act_new, pattern = "rente") == TRUE ~ "Finance and Insurance", # receveur de rentes
                           freq_max == "restaurateur" 
                           & str_detect(string = act_new, pattern = "recteur|ructeur") == TRUE ~ "Educational Services",
                           freq_max == "restaurateur" 
                           & str_detect(string = act_new, pattern = "hotel|h. garni|taverne|hotet|hiatel|htote|notet") == TRUE ~ "Manufacturing", # Building and Car Rental
                           TRUE ~ NAICS)) %>%
  # specification in "robes et manteaux" freq_max
  mutate(NAICS = case_when(freq_max == "robes et manteaux" 
                           & str_detect(string = act_new, pattern = "rpicter") == TRUE ~ "Food stores", # bad ocr epicier
                           freq_max == "robes et manteaux" 
                           & str_detect(string = act_new, pattern = "rapisster") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers", # bad ocr tapissier
                           freq_max == "robes et manteaux" 
                           & str_detect(string = act_new, pattern = "rafstneur|repicter") == TRUE ~ "Manufacturing", # bad ocr rafineur de petrole
                           freq_max == "robes et manteaux" 
                           & str_detect(string = act_new, pattern = "comptes") == TRUE ~ "Public Administration", # referendaire a la cour des comptes
                           TRUE ~ NAICS)) %>%
  # specification in "sage-femme" freq_max
  mutate(NAICS = case_when(freq_max == "sage-femme" 
                           & str_detect(string = act_new, pattern = "suspension|suspensoir|pensions|sacs") == TRUE ~ "Other retailers",
                           freq_max == "sage-femme" 
                           & str_detect(string = act_new, pattern = "banq|financ|fuanc") == TRUE ~ "Finance and Insurance", # sous-chef a la banque, sous-chef finances
                           # est-ce aux finances ? dans ce cas, public administration
                           freq_max == "sage-femme" 
                           & str_detect(string = act_new, pattern = "soie") == TRUE ~ "Clothing and clothing accessories manufacturers/retailers",
                           freq_max == "sage-femme" 
                           & str_detect(string = act_new, pattern = "ministere|interieur|enreg.|enregistrement|ponts|min.|minist|misis|miuis|maison du|inter|etat|artillerie") == TRUE ~ "Public Administration", 
                           # sous-chef au ministere, a l'interieur, a l'enregistrements, ponts-et-chaussees, maison du roi
                           TRUE ~ NAICS)) %>%
  # specification in "secretaire d'ambassade" freq_max
  mutate(NAICS = case_when(freq_max == "secretaire d'ambassade" 
                           & str_detect(string = act_new, pattern = "sucre") == TRUE ~ "Food stores",
                           TRUE ~ NAICS)) %>%
  # specification in "serrurier" freq_max
  mutate(NAICS = case_when(freq_max == "serrurier" 
                           & str_detect(string = act_new, pattern = "meuble") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "tabac" freq_max
  mutate(NAICS = case_when(freq_max == "tabac" 
                           & str_detect(string = act_new, pattern = "tapiss|tupis") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "tabac et vins" freq_max
  mutate(NAICS = case_when(freq_max == "tabac et vins" 
                           & str_detect(string = act_new, pattern = "tapisster|tapis|tupis|tapss") == TRUE ~ "Home Furniture and Supplies Manufacturers/Retailers",
                           freq_max == "tabac et vins" 
                           & str_detect(string = act_new, pattern = "tvoca|avoc") == TRUE ~ "Administrative and Legal Services",
                           freq_max == "tabac et vins" 
                           & str_detect(string = act_new, pattern = "tabict") == TRUE ~ "Manufacturing", # bad ocr of tabletier
                           TRUE ~ NAICS)) %>%
  # specification in "tabletier" freq_max
  mutate(NAICS = case_when(freq_max == "tabletier" 
                           & str_detect(string = act_new, pattern = "hote|hute|hole|hotc|hate|note|hofe|hole") == TRUE ~ "Building and Car Rental", # table d'hote
                           TRUE ~ NAICS)) %>%
  # specification in "tailleur" freq_max
  mutate(NAICS = case_when(freq_max == "tailleur" 
                           & str_detect(string = act_new, pattern = "tolier|toleri|toler|toller|ttileur|ttleur|tttleur|tuileri|tuileur|tuilieur|tuiiller|tuilleur|tti|ttl") == TRUE ~ "Construction and Public Works", 
                           # tolier et tuileur
                           TRUE ~ NAICS))



table_base_review <- table_base_review %>%
  # specification in "tailleur" freq_max
  mutate(NAICS = case_when(freq_max == "tailleurs" 
                           & str_detect(string = act_new, pattern = "tolier|tuileur") == TRUE ~ "Construction and Public Works", 
                           freq_max == "tailleurs" 
                           & str_detect(string = act_new, pattern = "toileri") == TRUE ~ "Manufacturing", # toilerie
                           TRUE ~ NAICS)) %>%
  # specification in "tapissier" freq_max
  mutate(NAICS = case_when(freq_max == "tapissier" 
                           & str_detect(string = act_new, pattern = "typographe|typo") == TRUE ~ "Publishing Industries", 
                           freq_max == "tapissier" 
                           & str_detect(string = act_new, pattern = "tabac") == TRUE ~ "Other retailers",
                           TRUE ~ NAICS)) %>%
  # specification in "traiteur" freq_max
  mutate(NAICS = case_when(freq_max == "traiteur" 
                           & str_detect(string = act_new, pattern = "truit|trutt|truut|truter") == TRUE ~ "Food stores", # truitier
                           freq_max == "traiteur" 
                           & str_detect(string = act_new, pattern = "toueur") == TRUE ~ "Building and Car Rental", # bad ocr loueur de...
                           freq_max == "traiteur" 
                           & str_detect(string = act_new, pattern = "trottoir") == TRUE ~ "Construction and Public Works", 
                           freq_max == "traiteur" 
                           & str_detect(string = act_new, pattern = "tordeur|tordre|tordir") == TRUE ~ "Manufacturing", # tordeur de metaux, etc
                           freq_max == "traiteur" 
                           & str_detect(string = act_new, pattern = "tartrifuge") == TRUE ~ "Other retailers", # detartrant
                           TRUE ~ NAICS)) %>%
  # specification in "vins en gros" freq_max
  mutate(NAICS = case_when(freq_max == "vins en gros" & str_detect(string = act_new, pattern = "gros|qros") == FALSE
                           & str_detect(string = act_new, pattern = "emporter|bouteille|boutecil|detail|detal|detali|beuteil|bouateil|bonteil|botcel|bouf|boulei|boulci|bout|cercle|delai|delal|detai|detat|detan") == TRUE ~ "Food stores", 
                           freq_max == "vins en gros" 
                           & str_detect(string = act_new, pattern = "commiss") == TRUE ~ "Trade Agents and Brokers",
                           TRUE ~ NAICS)) %>%
  # specification in "vins-restaurant" freq_max
  mutate(NAICS = case_when(freq_max == "vins-restaurant"
                           & str_detect(string = act_new, pattern = "hotel|hotet|notel|hotei|katel|hate|holet|hotef|garni|hotc|maison|role|rote") == TRUE ~ "Building and Car Rental", 
                           freq_max == "vins-restaurant" 
                           & str_detect(string = act_new, pattern = "traiteur|restaur|tratt|traeur|trau|trail|traieur|trait.|trat|cafe|raiteur") == TRUE ~ "Full-Service Restaurants",
                           freq_max == "vins-restaurant" 
                           & str_detect(string = act_new, pattern = "vannicr") == TRUE ~ "Manufacturing", # vannier
                           TRUE ~ NAICS)) %>%
  # specification in "vins-traiteur" freq_max
  mutate(NAICS = case_when(freq_max == "vins-traiteur"
                           & str_detect(string = act_new, pattern = "hotel|hotet|notel|hotei|katel|hate|holet|hotef|garni|hotc|maison|role|rote") == TRUE ~ "Building and Car Rental", 
                           freq_max == "vins-traiteur" 
                           & str_detect(string = act_new, pattern = "traiteur|restaur|tratt|traeur|trau|trail|traieur|trait.|trat|cafe") == TRUE ~ "Full-Service Restaurants",
                           TRUE ~ NAICS)) %>%
  mutate(NAICS = if_else(str_detect(string = act_new, pattern = "gros|en qros| en pros| en \\(ros|en dros|en fros|en tros|en gras"), 'Wholesalers Trade', NAICS))

## writting output
write_rds(x = table_base_review, file = "data/intermediary_datasets/data_extraction.rds", compress = 'gz')
