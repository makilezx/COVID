install.packages("haven", dependencies = T)
install.packages("psych", dependencies = T)
install.packages("car", dependencies = T)
install.packages("Hmisc", dependencies = T)
install.packages("lm.beta", dependencies = T)
install.packages("ggplot2", dependencies = T)
install.packages("dplyr", dependencies = T)


library(haven)
library(psych)
library(car)
library(Hmisc)
library(lm.beta) 		
library(dplyr)


#matrica
matrica <- read_spss(file.choose())
colnames(matrica)

matrica

#MODEL///postovanje mera
#soc.dist
#1 korak /// HEXACO
soc.dist_korak1 <- lm(soc.dist ~ H + E + X + A + C + O, data = matrica)
summary(soc.dist_korak1)
#2 korak  /// DT
soc.dist_korak2 <- lm(soc.dist ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(soc.dist_korak2)
anova(soc.dist_korak1, soc.dist_korak2)
lm.beta(soc.dist_korak1)
lm.beta(soc.dist_korak2)

#hig
#1 korak /// HEXACO
hig_korak1 <- lm(hig ~ H + E + X + A + C + O, data = matrica)
summary(hig_korak1)
#2 korak  /// DT
hig_korak2 <- lm(hig ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(hig_korak2)
anova(hig_korak1, hig_korak2)
lm.beta(hig_korak1)
lm.beta(hig_korak2)


#MODEL////procena korisnosti mera

#1 korak /// hexaco
procena_korisnosti_mera_korak1 <- lm( korisnost.mera ~ H + E + X + A + C + O, data = matrica)
summary(procena_korisnosti_mera_korak1)
#2 korak  /// dt
procena_korisnosti_mera_korak2 <- lm(korisnost.mera ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(procena_korisnosti_mera_korak2)
anova(procena_korisnosti_mera_korak1, procena_korisnosti_mera_korak2)
lm.beta(procena_korisnosti_mera_korak1)
lm.beta(procena_korisnosti_mera_korak2)


#MODEL///stavovi o merama 

#post.mera
#1 korak /// hexaco
post.mera_korak1 <- lm(postovanje.mera ~ H + E + X + A + C + O, data = matrica)
summary(post.mera_korak1)
#2 korak  /// dt
post.mera_korak2 <- lm(postovanje.mera ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(post.mera_korak2)
anova(post.mera_korak1, post.mera_korak2)
lm.beta(post.mera_korak1)
lm.beta(post.mera_korak2)

#emoc.rek
#1 korak /// hexaco
emoc.rek_korak1 <- lm(emoc.rek ~ H + E + X + A + C + O, data = matrica)
summary(emoc.rek_korak1)
#2 korak  /// dt
emoc.rek_korak2 <- lm(emoc.rek ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(emoc.rek_korak2)
anova(emoc.rek_korak1, emoc.rek_korak2)
lm.beta(emoc.rek_korak1)
lm.beta(emoc.rek_korak2)



#MODEL///procena rizika

#rizik zdravlje
#1 korak /// hexaco
rizik.zdravlje_korak1 <- lm( rizik.zdravlje ~ H + E + X + A + C + O, data = matrica)
summary(rizik.zdravlje_korak1 )
#2 korak  /// dt
rizik.zdravlje_korak2 <- lm(rizik.zdravlje ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(rizik.zdravlje_korak2)  
anova(rizik.zdravlje_korak1 , rizik.zdravlje_korak2)
lm.beta(rizik.zdravlje_korak1)
lm.beta(rizik.zdravlje_korak2)

#rizik glob
#1 korak /// hexaco
rizik.glob_korak1 <- lm( rizik.glob ~ H + E + X + A + C + O, data = matrica)
summary(rizik.glob_korak1 )
#2 korak  /// dt
rizik.glob_korak2 <- lm(rizik.glob ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(rizik.glob_korak2)  
anova(rizik.glob_korak1 , rizik.glob_korak2)
lm.beta(rizik.glob_korak1)
lm.beta(rizik.glob_korak2)




#MODEL///strah

#1 korak /// hexaco
strah_korak1 <- lm(strah ~ H + E + X + A + C + O, data = matrica)
summary(strah_korak1)
#2 korak  /// dt
strah_korak2 <- lm(strah ~ H + E + X + A + C + O + M + P + N, data = matrica)
summary(strah_korak2)
anova(strah_korak1, strah_korak2)
lm.beta(strah_korak1)
lm.beta(strah_korak2)
