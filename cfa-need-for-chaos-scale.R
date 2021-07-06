library(lavaan)  
library(psych)  
library(semPlot) 
library(haven)
library(psych)


#ucitavanje matrice
data <- read_spss(file.choose())


#check
colnames(data)

#NFC
#ociscena_matrica_STuP.sav
nfc <- subset(data, select=c(35:45))
colnames(nfc)



#varijanta sa inicijalnih 11 ajtema
#nfc.model.all.items<-'X=~ NFC_1 + NFC_2$ + NFH_3 + NFC_4 + NFC_5 + NFC_6 + NFC_7$ + NFC_8$ + NFC_9 + NFC_10 + NFC_11'
#u nastavku je koriscena varijanta sa 3 izbacena ajtema

nfc.model<-'X=~ NFC_1 + NFH_3 + NFC_4 + NFC_5 + NFC_6 + NFC_9 + NFC_10 + NFC_11'


nfc.model.ml <- cfa(nfc.model,        
                    data = nfc,   
                    std.lv = TRUE,     
                    estimator = "ML") 

summary(nfc.model.a1, standardized = TRUE, fit.measures = TRUE)

nfc.model.dwls <- cfa(nfc.model,        
                    data = nfc,   
                    std.lv = TRUE,     
                    estimator = "DWLS") 

summary(nfc.model.dwls, standardized = TRUE, fit.measures = TRUE)


nfc.model.wls <- cfa(nfc.model,        
                    data = nfc,   
                    std.lv = TRUE,     
                    estimator = "WLS") 

summary(nfc.model.wls, standardized = TRUE, fit.measures = TRUE)


nfc.model.wlsmv <- cfa(nfc.model,        
                     data = nfc,   
                     std.lv = TRUE,     
                     estimator = "WLSMV") 

summary(nfc.model.wlsmv, standardized = TRUE, fit.measures = TRUE)


nfc.model.mlm <- cfa(nfc.model,        
                       data = nfc,   
                       std.lv = TRUE,     
                       estimator = "MLM") 

summary(nfc.model.mlm, standardized = TRUE, fit.measures = TRUE)


nfc.model.mlr<- cfa(nfc.model,        
                     data = nfc,   
                     std.lv = TRUE,     
                     estimator = "MLR") 

summary(nfc.model.mlr, standardized = TRUE, fit.measures = TRUE)

#GRAF
graf <- semPaths(nfc.model.a1, 
                       what = "std",              
                       layout = "tree",        
                       residuals = T,              
                       title = TRUE,               
                       style = "lisrel",            
                       color = list (lat= "gray94",        
                                     man = "royalblue1"))  
