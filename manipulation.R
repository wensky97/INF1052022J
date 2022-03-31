#############L####################
#### MANIPULATION DE DONNEES #####
##################################

##### Travailler avec le jeux de donnees donnees.csv

df_sal<-read.csv("donnees.csv")  # importer les donnees
str(df_sal)                      # explorer l'objet df_sal

# On veut que le sexe contienne des donnees de type factor
df_sal$sexe<-as.factor(df_sal$sexe) # modifier le type de donnees
str(df_sal)                         # explorer l'objet df_sal

# On va faire des calculs statistiques
mean(df_sal$age)                    # calcul de la moyenne
sd(df_sal$age)                      # calcul de l'ecart-type
median(df_sal$age)                  # calcul de la mediane
summary(df_sal$age)                 # resume des statistiques

# on veut faire une regression lineare du salaire sur l'age et le sexe
reg<-lm(data=df_sal,sal~age+sexe)   # regression lineare
reg                                 # les resultats
reg_detail<-summary(reg)            # resultats detailles de la reg
reg_detail                          # les resultats detailles s'affichent
reg_detail$r.squared                # R carre
reg_detail$coefficients             # les coefficients
reg_detail$coefficients[1:3,c(1,4)] # afficher les coef et probabilites


##########################################################
##### On va utiliser dplyr pour manipuler les donnees ####
library(dplyr)

# filtrer le data frame pour retenir que les donnees des femmes
df_femme<-df_sal%>%
  filter(sexe=="F")
View(df_femme)            # pour voir les donnees de df_femme

# filtrer le data frame pour retenir que les donnees des hommes
df_homme<-df_sal%>%
  filter(sexe=="H")
View(df_homme)           # pour voir les donnees de df_homme


# selectionner les colonnes sexe et sal d'un data frame
df_colSS<-df_sal%>%
  select(sexe,sal)
View(df_colSS)           # pour voir les donnees de df_colSS

# resumer pour calculer une statistique des salaires
df_sal%>%
  summarise(mean(sal))   # calcul de la moyenne du salaire

# calculer la moyenne du salaire des hommes
df_sal%>%
  filter(sexe=="H")%>%  # filtrer que les donnees des hommes
  summarise(mean(sal))  # calculer la moyenne du salaire des hommes

# Grouper les donnees
df_groupSexe<-df_sal%>%
  group_by(sexe)
View(df_groupSexe)

# Calculer la moyenne du salaire pour les femmes et les hommes
df_sal%>%
  group_by(sexe)%>%
  summarise(SalaireMoyen=mean(sal))

# calculer l'ecart-type de l'age par sexe
df_sal%>%
  group_by(sexe)%>%
  summarise(mean(age))

# Compter le nombre d'observation avec le verbe count
df_sal%>%
  group_by(sexe)%>%
  count()

# pour faire d'autres exercices, 
# aller sur ce lien: 
