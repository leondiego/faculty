library(pacman)
p_load(foreign,survival,KMsurv,nlme,muhaz,TH.data,ggsci,
       ggplot2,ggfortify,proto,GGally,survminer,corrplot,
       dplyr,My.stepwise,car,foreign,MASS,ggpubr)
#-------------------Análisis exploratorio
data(std);?std;head(std);attach(std)
std<-std[,-1]#quitamos esta variable porque son las observaciones
std$time<-as.numeric(std$time);std$rinfct<-as.numeric(std$rinfct)

#**************************************************************************************
#**********************************ANALISIS DESCRIPTIVO********************************
#**************************************************************************************

Porcentajes.NoSi<-function(variable,factor){
        sub<- subset(std, variable == factor)
        no<-sum(table(sub$time, sub$rinfct)[,1])
        si<-sum(table(sub$time, sub$rinfct)[,2]) 
        porcentajeno<-as.character(round((no/(no+si)),4))
        porcentajesi<-as.character(round((si/(no+si)),4))
        
        return(c(paste(porcentajeno,"% no reeinfectadas"),paste(porcentajesi,"% reeinfectadas")))
}

#grafica pastel para las razas
raza<-ggplot(std, aes(x = "", fill = race)) + 
        geom_bar(width = 1) +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        scale_fill_manual(values=c("#ede59a","#d5c455"))+
        labs(fill="raza", 
             x=NULL, 
             y=NULL, 
             title="Raza de las pacientes", 
             caption = "B:negra, W:blanca")

raza + coord_polar(theta = "y", start=0)
Porcentajes.NoSi(race,"W")
Porcentajes.NoSi(race,"B")

#pastel para estado marital
estado<-ggplot(std, aes(x = "", fill = marital)) + 
        geom_bar(width = 1) +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        scale_fill_manual(values=c("#f2ffdf", "#ffca61","#ffec85"))+
        labs(fill="Estado marital", 
             x=NULL, 
             y=NULL, 
             title="Estado marital de las pacientes", 
             caption = "D: divorciada/separada, M: casada, S: soltera")

estado + coord_polar(theta = "y", start=0)
Porcentajes.NoSi(marital,"S")
Porcentajes.NoSi(marital,"M")
Porcentajes.NoSi(marital,"D")

#pastel para infección inicial
infec<-ggplot(std, aes(x = "", fill = iinfct)) + 
        geom_bar(width = 1) +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        scale_fill_manual(values=c("#b2e672","#0092ca", "#f96b85"))+
        labs(fill="Tipo de infección inicial", 
             x=NULL, 
             y=NULL, 
             title="Infección inicial", 
             caption = "1: Gonorrea, 2: Clamidia, 3: Ambos")
infec + coord_polar(theta = "y", start=0)

Porcentajes.NoSi(iinfct,1)
Porcentajes.NoSi(iinfct,2)

#pastel para el uso del condón
condon<-ggplot(std, aes(x = "", fill = condom)) + 
        geom_bar(width = 1) +
        theme(axis.line = element_blank(), 
              plot.title = element_text(hjust=0.5)) + 
        scale_fill_manual(values=c("#fcf9f9","#0092ca", "#ff8ba7"))+
        labs(fill="Uso de condón", 
             x=NULL, 
             y=NULL, 
             title="Uso de condón", 
             caption = "1: siempre, 2: aveces, 3: nunca")

condon + coord_polar(theta = "y", start=0)
Porcentajes.NoSi(condom,1)
Porcentajes.NoSi(condom,2)
Porcentajes.NoSi(condom,3)

#grafica de barras para edades
edades<-table(age)
names(edades) <- c("13","14","15","16","17","18","19","20","21","22","23","24","25",
                   "26","27","28","29","30","31","32","33","34","35","36","37","38",
                   "39","40","41","43","44","46","48")
c <- barplot(edades,col=rainbow(30), main="Edad de las pacientes al momento de la infección inicial", 
             col.main="black", ylab="Frecuencia absoluta", col.lab="dark#0092ca")
text(x=c, y=c(edades), labels=c(edades), pos=3, col="dark#0092ca", cex=1.5, xpd=TRUE)

#menores de 16 años
sub<-subset(std, age <16)
sum(table(sub$time, sub$rinfct)[,1]);sum(table(sub$time, sub$rinfct)[,2])
#round(30/(30+42),2) 42% no presentan reinfeccion

#mayor o igual a 16 años
sub<-subset(std, age >15)
sum(table(sub$time, sub$rinfct)[,1]);sum(table(sub$time, sub$rinfct)[,2]) 
#500/(805) 62% no presentan reinfecion


#grafica de barras para el número de parejas
NumPar<-table(npartner)
names(NumPar) <- c("0","1","2","3","4","5","6","10","19")
b <- barplot(NumPar,col=rainbow(15), main="Número de parejas sexuales, en los ultimos 30 días.", 
             col.main="black", ylab="Frecuencia absoluta", col.lab="dark#0092ca")
text(x=b, y=c(NumPar), labels=c(NumPar), pos=3, col="dark#0092ca", cex=1.5, xpd=TRUE)

sub<-subset(std, npartner ==0)
#41 / 29, 58% no reinfectadas
sub<-subset(std, npartner ==1)
#373/234, 61% no reinfectadas
sub<-subset(std, npartner ==2)
#85 / 61 58% no reinfectadas
sub<-subset(std, npartner ==3)
#23 y 16 58% no reinfectadas
sub<-subset(std, npartner >3)
#8 y 7 53% no reinfectadas

sub<-subset(std, npartner <4)
sum(table(sub$time, sub$rinfct)[,1]);sum(table(sub$time, sub$rinfct)[,2]) 
#522/(522+340) 60% no reinfectadas

sub<-subset(std, npartner >3)
sum(table(sub$time, sub$rinfct)[,1]);sum(table(sub$time, sub$rinfct)[,2]) 
#8/15 53% no reinfectadas


#grafica de barras para la escuela
summary(yschool)
escuela<-table(yschool)
names(escuela) <- c("6"," 7"," 8"," 9","10","11","12","13","14","15","16","17","18")
d <- barplot(escuela,col=rainbow(40), main="Años de escolaridad de las pacientes", 
             col.main="black", ylab="Frecuencia absoluta", col.lab="darkred")
text(x=d, y=c(escuela), labels=c(escuela), pos=3, col="darkred", cex=1.5, xpd=TRUE)
#menos de 8 años de escuela
sub<-subset(std, yschool <8);sum(table(sub$time, sub$rinfct)[,1]);sum(table(sub$time, sub$rinfct)[,2]) 
#de 8 a 15 años
sub<-subset(std, yschool<16)
sub2<-subset(sub,yschool>7)
sum(table(sub2$time, sub2$rinfct)[,1]);sum(table(sub2$time, sub2$rinfct)[,2]) 
334/(510+334)
#más de 15 años de escuela
sub<-subset(std, yschool > 15);sum(table(sub$time, sub$rinfct)[,1]);sum(table(sub$time, sub$rinfct)[,2]) 


#**************************************************************************************
#******************************ANALISIS DE VARIABLES***********************************
#**********************que variables pueden afectar a la supervivencia*****************
#*
#-----Ajuste del modelo K-M-----
std$time<-as.numeric(std$time)
std$rinfct<-as.numeric(std$rinfct)

kmfit <- survfit(Surv(std$time, std$rinfct, type = 'right') ~ 1, data = std, conf.type="plain", type = "kaplan-meier", conf.int = 0.95)
summary(kmfit)
plot(kmfit, col=c("#A74AEE","#4D0386","#4D0386"), main="Ajuste del modelo Kaplan - Meier", las=1, lwd=2, mark.time = T)

#grafica con la mediana e individuos en riesgo
ggsurvplot(kmfit,
           conf.int = TRUE,palette ="#fcbf1e",
           risk.table = TRUE, 
           risk.table.col = "strata",
           linetype = "strata", 
           surv.median.line = "hv", 
           ggtheme = theme_bw() 
           )
kmfitOS <- survfit(Surv(std$time, std$rinfct, type = 'right') ~ os30d, data = std, conf.type="plain", type = "kaplan-meier", conf.int = 0.95)
ggsurvplot(kmfitOS,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE,
           risk.table.col = "strata",
           linetype = "strata",
           surv.median.line = "hv",
           ggtheme = theme_bw(),
           palette = "startrek")

#-----Particionamos la base 1 Condom----- 

#1.uso de condon siempre vs. (usalmente o nunca)
std_si = subset(std, std$condom == 1)
sum(table(std_si$time, std_si$rinfct)[,1]);sum(table(std_si$time, std_si$rinfct)[,2])
33/(33+21) #61.11 de los pacientes que siempre usan condón no tienen reeinfeccion
#Hay menos reeinfecciones si siempre se usa condon 33/21

#2.Condom$Never vs. (Condom$yes v Condom$Sometime)
std_no = subset(std, std$condom == 3)
sum(table(std_no$time, std_no$rinfct) [,1]);sum(table(std_no$time, std_no$rinfct) [,2])
#196/(196+116) 62.82% de los pacientes que nunca usan condón no tienn reeinfeccion
#Hay menos reeinfecciones si nunca se usa condon(?) 196/116

#3.Condom$Sometime vs. (Condom$yes v Condom$Never)
std_some = subset(std, std$condom == 2)
sum(table(std_some$time, std_some$rinfct)[,1]);sum(table(std_some$time, std_some$rinfct)[,2])
#301/(301+210) 58% de las pacientes que usan condon aveces no tienen reeinfeccion
#hay menos reeinfecciones si aveces se usa condón 301/210

#pero en casi todas es la mitad de reeinfectadas con no reeinfectadas

#Estimador K-M para los que si usan condon
ajuste_KM_si = survfit(Surv(std_si$time, std_si$rinfct,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = std_si)
ajuste_KM_si
summary(ajuste_KM_si)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_si, main = "Estimador Kaplan-Meier con condon", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Para la población que no usa condon
ajuste_KM_no = survfit(Surv(std_no$time, std_no$rinfct,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = std_no)
ajuste_KM_no
summary(ajuste_KM_no)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_no, main = "Estimador Kaplan-Meier sin condon", xlab =  "t",
     col = "darkorange", ylab = "S(t)", lwd = 3,
     conf.int = TRUE, las=1)

#Estimador K-M para los que a veces usan condon
ajuste_KM_some = survfit(Surv(std_some$time, std_some$rinfct,
                              type = "right") ~ 1, type = "kaplan-meier", 
                         conf.type = "log-log", conf.int = 0.95, data = std_some)
ajuste_KM_some
summary(ajuste_KM_some)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_some, main = "Estimador Kaplan-Meier condon a veces", xlab =  "t",
     col = "#f96b85", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)

# Comparamos los estimadores para cada poblacion:
plot(ajuste_KM_no, xlab =  "t", col = "#0092ca", 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE, xlim = c(0,1600))
par(new = TRUE)
plot(ajuste_KM_si, xlab =  "t", col = "#b2e672", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
par(new = TRUE)
plot(ajuste_KM_some, xlab =  "t", col = "#f96b85", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
title("Estimadores K-M: Uso de condón")
legend(x = "bottomleft", c("Sin condon", "Con condon", "A veces"), lty = 1,
       col = c("#0092ca", "#b2e672", "#f96b85"), bty = "n") 


# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std$condom, rho = 0, data = std)
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(std$time, std$rinfct) ~ std$condom, rho = 1, data = std)

#Sí hay diferencias entre usar condon siempre, ocasionalmente o nunca

#--------------------------------------------------------Usar siempre vs usar aveces
aux<-subset(std,std$condom!=3) #se quita el no usar nunca
aux$condom
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$condom, rho = 0, data = std) #p= 0.8972
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$condom, rho = 1, data = std) #p= 0.8227

#--------------------------------------------------------Usar siempre vs nunca usar
aux<-subset(std,std$condom!=2) #se quita el no usar aveces
aux$condom
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$condom, rho = 0, data = std) #p= 0.2311 
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$condom, rho = 1, data = std) #p= 0.1172 

#--------------------------------------------------------Usar aveces vs nunca usar
aux<-subset(std,std$condom!=1) #se quita el usar siempre
aux$condom
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$condom, rho = 0, data = std)# 0.003129 
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$condom, rho = 1, data = std) #0.0023



#.....Particion 2 (Marital)-----


#1.Divorciada vs. (casada o soltera)
std_d = subset(std, std$marital == 'D')
sum(table(std_d$time, std_d$rinfct)[,1] );sum(table(std_d$time, std_d$rinfct)[,2] )
#44/(44+16) 73.33% de las divorciadas no tienen reeinfeccion
#Casi no hay reinfeccionesen las divorciadas


#2.Casada vs. (Divorciada o  Separada)
std_m = subset(std, std$marital == 'M')
sum(table(std_m$time, std_m$rinfct)[,1]);sum(table(std_m$time, std_m$rinfct)[,2]) 
#19/(19+9) 67.85% de las casaas no tienen reeinfeccion
#Hay pocas reinfecciones en pacientes casadas


#3.Soltera vs. (Divorciada o Casada)
std_s = subset(std, std$marital == 'S')
sum(table(std_s$time, std_s$rinfct)[,1] );sum(table(std_s$time, std_s$rinfct)[,2] )
#Presencia de más reinfecciones en pacientes solteras 467/322
467/(467+332) #58.44% de pacientes solteras NO tienen reeinfeccion
332/(467+332) #41.55% de pacientes solteras tiene reeinfeccion


#Estimador K-M para pacientes Divorciadas/Separadas
ajuste_KM_d = survfit(Surv(std_d$time, std_d$rinfct,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = std_d)
ajuste_KM_d
summary(ajuste_KM_d)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_d, main = "Estimador Kaplan-Meier-Divorcida/Separada", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Para la población casada
ajuste_KM_m = survfit(Surv(std_m$time, std_m$rinfct,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = std_m)
ajuste_KM_m
summary(ajuste_KM_m)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_m, main = "Estimador Kaplan-Meier- Casada", xlab =  "t",
     col = "darkorange", ylab = "S(t)", lwd = 3,
     conf.int = TRUE, las=1)

#Estimador K-M para pacientes solteras
ajuste_KM_s = survfit(Surv(std_s$time, std_s$rinfct,
                              type = "right") ~ 1, type = "kaplan-meier", 
                         conf.type = "log-log", conf.int = 0.95, data = std_s)
ajuste_KM_s
summary(ajuste_KM_s)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_s, main = "Estimador Kaplan-Meier - Soltera", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Comparamos los estimadores para cada poblacion:
plot(ajuste_KM_d, xlab =  "t", col = "#0092ca", 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE, xlim = c(0,1600))
par(new = TRUE)
plot(ajuste_KM_m, xlab =  "t", col = "#b2e672", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
par(new = TRUE)
plot(ajuste_KM_s, xlab =  "t", col = "#f96b85", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
title("Estimadores Kaplan - Meier - Estado Marital")
legend(x = "bottomleft", c("Divorciada/Separada", "Casada", "Soltera"), lty = 1,
       col = c("#0092ca", "#b2e672", "#f96b85"), bty = "n")


# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std$marital, rho = 0, data = std)


# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(std$time, std$rinfct) ~ std$marital, rho = 1, data = std)

#Se rechaza que las supervivencias sean iguales
#Hay diferencia entre estar casada, soltera o divorciada

#---------------------------------------- Diferencias entre estar soltera y casada
aux <- subset(std, std$marital != 'D')
aux$marital
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$marital, rho = 0, data = std) #p= 0.123 
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$marital, rho = 1, data = std) #0.09164 

#---------------------------------Diferencias entre estar soltera y divorcidada
aux <- subset(std, std$marital != 'M')
aux$marital
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$marital, rho = 0, data = std) #0.0496
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$marital, rho = 1, data = std) #0.09164 

#---------------------------------Diferencias entre estar casada y divorcidada
aux <- subset(std, std$marital != 'S')
aux$marital
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$marital, rho = 0, data = std) #0.889
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$marital, rho = 1, data = std) #0.8395 


#.....Particion 3 (iinfct)-----
#1: gonorrea, 2: clamidia, 3: ambos

#1.Gonorrea vs. (clamidia o ambos)
std_g = subset(std, std$iinfct == 1)
sum(table(std_g$time, std_g$rinfct)[,1]);sum(table(std_g$time, std_g$rinfct)[,2])
#73/(67+73) # 52.14% de las pacientes con gonorrea tuvo reeinfeccion

#2.Clamidia vs. (gonorrea o ambos) 
std_cla = subset(std, std$iinfct == 2)
sum(table(std_cla$time, std_cla$rinfct)[,1]);sum(table(std_cla$time, std_cla$rinfct)[,2])
#261/(261+135) 65% de las pacientes con clamidia NO tuvo reeinfeccion

#3. Ambos vs. (clamidia o gonorrea)
std_CoG = subset(std, std$iinfct == 3) 
sum(table(std_CoG$time, std_CoG$rinfct)[,1]);sum(table(std_CoG$time, std_CoG$rinfct)[,2])
#202/(202+139) 59% de los infectados con ambas enfermedades no tuviero reeinfeccion

#Estimador K-M para pacientes con gonorrea
ajuste_KM_gonorrea = survfit(Surv(std_g$time, std_g$rinfct,
                           type = "right") ~ 1, type = "kaplan-meier", 
                      conf.type = "log-log", conf.int = 0.95, data = std_d)
ajuste_KM_gonorrea
summary(ajuste_KM_gonorrea)

# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_gonorrea, main = "Estimador Kaplan-Meier-Gonorrea", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Para las pacientes con clamidia
ajuste_KM_clamidia = survfit(Surv(std_cla$time, std_cla$rinfct,
                           type = "right") ~ 1, type = "kaplan-meier", 
                      conf.type = "log-log", conf.int = 0.95, data = std_m)
ajuste_KM_clamidia 
summary(ajuste_KM_clamidia )
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_clamidia , main = "Estimador Kaplan-Meier- Clamidia", xlab =  "t",
     col = "darkorange", ylab = "S(t)", lwd = 3,
     conf.int = TRUE, las=1)

#Estimador K-M para pacientes con ambas enfermedades
ajuste_KM_clamYgon = survfit(Surv(std_CoG$time, std_CoG$rinfct,
                           type = "right") ~ 1, type = "kaplan-meier", 
                      conf.type = "log-log", conf.int = 0.95, data = std_s)

ajuste_KM_clamYgon
summary(ajuste_KM_clamYgon)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_clamYgon, main = "Estimador Kaplan-Meier - Clamidia y Gonorrea", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Comparamos los estimadores para cada poblacion:
plot(ajuste_KM_gonorrea, xlab =  "t", col = "#0092ca", 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE, xlim = c(0,1600))
par(new = TRUE)
plot(ajuste_KM_clamidia, xlab =  "t", col = "#b2e672", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
par(new = TRUE)
plot(ajuste_KM_clamYgon, xlab =  "t", col = "#f96b85", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
title("Estimadores K-M: Infeccion inicial")
legend(x = "bottomleft", c("Gonorrea", "Clamidia", "Ambas"), lty = 1,
       col = c("#0092ca", "#b2e672", "#f96b85"), bty = "n")


# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std$iinfct, rho = 0, data = std)

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(std$time, std$rinfct) ~ std$iinfct, rho = 1, data = std)

#Se rechaza que las supervivencias sean iguales
#Hay diferencia entre tener clamidia, gonorrea o ambas

#---------------------------------------- Diferencias entre tener gonorrea y clamidia
aux <- subset(std, std$iinfct != 3)
aux$iinfct
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$iinfct, rho = 0, data = std) #0.0031
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$iinfct, rho = 1, data = std) # 0.0119


#---------------------------------------- Diferencias entre tener gonorrea y ambas
aux <- subset(std, std$iinfct != 2)
aux$iinfct
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$iinfct, rho = 0, data = std) #0.0415
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$iinfct, rho = 1, data = std) #0.05138


#---------------------------------------- Diferencias entre tener clamidia y ambas
aux <- subset(std, std$iinfct != 1)
aux$iinfct
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$iinfct, rho = 0, data = std) #0.3326 
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$iinfct, rho = 1, data = std) #0.5116


##-----Particion 4 (Oral Sex 30 days)-----
#Oral sex within 30 days

#Población que practicó sexo oral los últimos 30 días
std_si = subset(std, std$os30d == 1)
table(std_si$time, std_si$rinfct)
#(subset(std_si, std_si$time==3)) Hay dos reinfecciones para la variable os30d

#Población que NO practicó sexo oral los últimos 30 días
std_no = subset(std, std$os30d == 0)
table(std_no$time, std_no$rinfct)
#(subset(std_no, std_no$time==367)) Hay tres reinfecciones para quines no practicaban sexo oral los últimos 30 días

#Estimador K-M para los que si practican el sexo oral
ajuste_KM_si = survfit(Surv(std_si$time, std_si$rinfct,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = std_si)
ajuste_KM_si
summary(ajuste_KM_si)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_si, main = "Estimador Kaplan-Meier oral sex", xlab =  "t",
     col = "purple", ylab = "S(t)", lwd =3,
     conf.int = TRUE, las=1)


# Para la población que no practica sexo oral
ajuste_KM_no = survfit(Surv(std_no$time, std_no$rinfct,
                            type = "right") ~ 1, type = "kaplan-meier", 
                       conf.type = "log-log", conf.int = 0.95, data = std_no)
ajuste_KM_no
summary(ajuste_KM_no)
# Graficamos el estimador K-M con bandas de confianza:
plot(ajuste_KM_no, main = "Estimador Kaplan-Meier no oral sex", xlab =  "t",
     col = "darkorange", ylab = "S(t)", lwd = 3,
     conf.int = TRUE, las=1)

# Comparamos los estimadores para cada poblacion:
plot(ajuste_KM_no, xlab =  "t", col = "blue", 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE, xlim = c(0,1600))
par(new = TRUE)
plot(ajuste_KM_si, xlab =  "t", col = "red", xlim = c(0,1600), 
     ylab = "S(t)", lwd = 2.5, conf.int = FALSE)
title("Estimadores Kaplan - Meier")
legend(x = "bottomleft", c("Oral Sex", "No Oral Sex"), lty = 1,
       col = c("blue", "red"), bty = "n") 


# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std$os30d, rho = 0, data = std)


# Prueba log-rank (Peto-Peto -> Gehan-Wilcoxon):
survdiff(Surv(std$time, std$rinfct) ~ std$os30d, rho = 1, data = std)

#Se rechaza la prueba de que las distribuciones sean iguales
#Sí hay afectación en el tiempo de reinfección si se practica o no el sexo oral


#Veamos si hay diferencia entre los grupos de las variables que tenemos:-----
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL<-function(variable){
  # Prueba log-rank (Mantel-Haenszel):
  MH<-survdiff(Surv(std$time, std$rinfct) ~ factor(variable), rho = 0, data = std)
  return(MH)
}
Pruebas.Dif.PoblacionesPETOPETO<-function(variable){
  # Prueba log-rank (Peto-Peto -> Gehan-Wilcoxon):
  PP<-survdiff(Surv(std$time, std$rinfct) ~ factor(variable), rho = 1, data = std)
  return(PP)
}
Grafica.KM<-function(variable,cola){
  ajuste <- survfit(Surv(time,rinfct) ~ variable, data = std)
  grafica<-ggsurv(ajuste, main=cola)
  return(grafica)
}
#---------------------------------------------------------------------------Race
Grafica.KM(race, "Estimadores Kaplan - Meier: Raza")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(race) #0.002659 
Pruebas.Dif.PoblacionesPETOPETO(race) #0.007284 
#Se RECHAZA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------os12m  
Grafica.KM(os12m,"Estimadores Kaplan - Meier: Sexo oral (12 meses)")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(os12m) #4.743e-06 
Pruebas.Dif.PoblacionesPETOPETO(os12m) #8.934e-06  
#Se RECHAZA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------os30d
Grafica.KM(os30d,"Estimadores Kaplan - Meier: Sexo oral (30 días)")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(os30d) #2.034e-05 
Pruebas.Dif.PoblacionesPETOPETO(os30d) #6.37e-05  
#Se RECHAZA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------rs12m
Grafica.KM(rs12m, "Estimadores Kaplan - Meier: Sexo anal (12 meses)")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(rs12m) #0.1565 
Pruebas.Dif.PoblacionesPETOPETO(rs12m) #0.1235  
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------rs30d
Grafica.KM(rs30d,"Estimadores Kaplan - Meier: Sexo anal (30 días)")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(rs30d) #0.4285 
Pruebas.Dif.PoblacionesPETOPETO(rs30d) #0.4269   
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------abdpain
Grafica.KM(abdpain,"Estimadores Kaplan - Meier: Dolor abdominal")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(abdpain) #0.03942
Pruebas.Dif.PoblacionesPETOPETO(abdpain) #0.03136
#Se RECHAZA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------discharge
Grafica.KM(discharge,"Estimadores Kaplan - Meier: Signo de secreción")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(discharge) #0.09178 
Pruebas.Dif.PoblacionesPETOPETO(discharge) #0.1044
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------dysuria
Grafica.KM(dysuria,"Estimadores Kaplan - Meier: Signo de Disuria")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(dysuria) #0.08781 
Pruebas.Dif.PoblacionesPETOPETO(dysuria) #0.1161 
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------itch
Grafica.KM(itch, "Estimadores Kaplan - Meier: Signo de picazón")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(itch) #p= 0.7606
Pruebas.Dif.PoblacionesPETOPETO(itch) #0.8168  
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------lesion
Grafica.KM(lesion,"Estimadores Kaplan - Meier: Signo de lesión")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(lesion) #0.6838 
Pruebas.Dif.PoblacionesPETOPETO(lesion) #0.5504  
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------rash
Grafica.KM(rash,"Estimadores Kaplan - Meier: Signo de erupción")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(rash) #0.6426 
Pruebas.Dif.PoblacionesPETOPETO(rash) #0.8689  
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------lymph
Grafica.KM(lymph, "Estimadores Kaplan - Meier: Signo de linfa")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(lymph) #0.8289
Pruebas.Dif.PoblacionesPETOPETO(lymph) #0.7044
#Se ACEPTA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------vagina
Grafica.KM(vagina,"Estimadores Kaplan - Meier: Examen vaginal")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(vagina) #0.006801 
Pruebas.Dif.PoblacionesPETOPETO(vagina) # p= 0.003196 
#Se RECHAZA la prueba de que las distribuciones sean iguales

#------------------------------------------------------------------------dchexam
Grafica.KM(dchexam,"Estimadores Kaplan - Meier: Secreción en el examen")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(dchexam) #0.0558
Pruebas.Dif.PoblacionesPETOPETO(dchexam) #0.04309
#Una prueba acepta y otra rechaza xD

#------------------------------------------------------------------------abnode
Grafica.KM(abnode, "Estimadores Kaplan - Meier: Ganglio anormal")
Pruebas.Dif.PoblacionesMANTEL_HAENSZEL(abnode) #0.5121 
Pruebas.Dif.PoblacionesPETOPETO(abnode) # p= 0.6757 
#Se ACEPTA la prueba de que las distribuciones sean iguales


#Tenemos que hacer una partición de las variables continuas, para volverlas categoricas
#y poder analizar las mismas cosas (años de estudios, edad, número de parejas)
# y calcula el modelo de cox para ver si estas variables son significativas

#---------------------------edad
summary(age) #va de 13 a 48 
AgeCat<-factor(ifelse(age<=17, "13-17", ifelse(age<=22, "17-22", ">=23")))
table(AgeCat)
edadesCat<-survfit(Surv(time,rinfct)~factor(AgeCat),type = "kaplan-meier",conf.type="plain",data=std)
plot(edadesCat,conf.int=F,xlab="tiempo", ylab="Supervivencia",
     col=c("#0092ca", "#b2e672", "#f96b85"),main = "Estimadores K-M: Edad")
legend("topright", legend=names(table(AgeCat)), lty=1, col=c("#0092ca", "#b2e672", "#f96b85"))



#---------------------------------------- Diferencias tener 17 o menos años
std2<-data.frame(std,AgeCat)

# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std2$AgeCat, rho = 0, data = std) #2e-05 

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(std$time, std$rinfct) ~ std2$AgeCat, rho = 1, data = std) #2e-05 

#Se rechaza que las supervivencias sean iguales
#Hay diferencia entre las edades categorizadas

#---------------------------------------- Diferencias entre 13-17 con más de 23
aux <- subset(std2, std2$AgeCat != "17-22")
aux$AgeCat

# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$AgeCat, rho = 0, data = std) #5e-04

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$AgeCat, rho = 1, data = std) # p= 0.001 

#---------------------------------------- Diferencias entre 13-17 con 17-22
aux <- subset(std2, std2$AgeCat != ">=23")
aux$AgeCat
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$AgeCat, rho = 0, data = std) #p= 2e-05 
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$AgeCat, rho = 1, data = std) #p= 1e-05 

#---------------------------------------- Diferencias entre  17-22 con +36
aux <- subset(std2, std2$AgeCat != "13-22")
aux$AgeCat
# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$AgeCat, rho = 0, data = std) #p= 2e-05 
# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(aux$time, aux$rinfct) ~ aux$AgeCat, rho = 1, data = std) #p= 2e-05 


#---------------------------años de escolaridad
head(std)
summary(yschool) #va de 6 a 18
#6-11
#+12
YSCat<-factor(ifelse(yschool<=11, "6-11", ifelse(yschool<=18, "12-18", " ")))
table(YSCat)

escuelacat<-survfit(Surv(time,rinfct)~factor(YSCat),type = "kaplan-meier",conf.type="plain",data=std)
plot(escuelacat,conf.int=F,xlab="tiempo", ylab="Supervivencia",
     col=c("#0092ca", "#f96b85"),main = "Estimadores K-M: Años de escolaridad")
legend("topright", legend=names(table(YSCat)), lty=1, col=c("#0092ca", "#b2e672", "#f96b85"))
#---------------------------------------- Diferencias tener 17 o menos años
std3<-data.frame(std,YSCat)

# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std3$YSCat, rho = 0, data = std) #0.001 

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(std$time, std$rinfct) ~ std3$YSCat, rho = 1, data = std) #9e-04 
#Sí hay diferencia entre la supervivencia de las pacientes con más de 11 años 
#de escolaridad que con las de menos años


#---------------------------numero de parejas
summary(npartner)

ParCat<-factor(ifelse(npartner<=1, "0-1", ifelse(npartner<=20, ">=2", "+3")))
table(ParCat)
colita<-survfit(Surv(time,rinfct)~factor(ParCat),type = "kaplan-meier",conf.type="plain",data=std)
plot(colita,conf.int=F,xlab="tiempo", ylab="Supervivencia",
     col=c("#0092ca", "#f96b85"),main = "Estimadores K-M: Número de parejas")
legend("topright", legend=names(table(ParCat)), lty=1, col=c("#0092ca", "#f96b85"))

#---------------------------------------- Diferencias entre poblaciones
std4<-data.frame(std,ParCat)

# Prueba log-rank (Mantel-Haenszel):
survdiff(Surv(std$time, std$rinfct) ~ std4$ParCat, rho = 0, data = std) #0.2

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(Surv(std$time, std$rinfct) ~ std4$ParCat, rho = 1, data = std) #0.4
#A primera vista no hay diferencia entre las poblaciones


#**************************************************************************
#*********************************PARTE DE COX *****************************
#**************************************************************************

survtime <- Surv(std$time, std$rinfct)

################ CAMBIO DE VARIABLES ################
std1 <- std
std1$iinfct2 <- as.character(std$iinfct)
std1$iinfct2[std$iinfct != "2"] <- 0
std1$iinfct2[std$iinfct == "2"] <- 1
std1$iinfct2 <- as.factor(std1$iinfct2)

####################### MODELO COX PH ##############################
#coef: Coeficiente estimado de la beta.
#exp(coef): Exponencial elevado al coeficiente beta estimado.
#se(coef): La desviación estÃ¡ndar de la estimaciÃ³n.
#z: Valor del estad??stico para prueba Wald de beta igual a cero.
#p: P-valor de la prueba Wald, beta igual a cero.
head(std)
coxfit1 <- coxph(survtime ~ (as.factor(race) + age + yschool + as.factor(iinfct) + npartner + 
                               os12m + os30d + rs12m + rs30d + abdpain + discharge + dysuria
                             + itch + lesion + rash + lymph + vagina + dchexam + abnode + marital 
                             + as.factor(condom)), data = std1)
summary(coxfit1)
#Segun el summary, las unicas variables significativas son dchexam, vagina, iinfct y yschool
#El summary nos proporciona con los valores estimados de las bi, asÃ? como
#para exp(bi) con intervalos de confianza al 95% para exp(bi)
extractAIC(coxfit1) #edf = 24, AIC = 4121.164

variables <- c(variable.names(std))
My.stepwise.coxph(Time = "time", Status = "rinfct", 
                  variable.list = variables[3:21], data = std)
#Sin embargo 'condom' es un factor por lo que corremos otro modelo tomandola como tal y 
#ademÃ¡s incluimos las primeras dos variables

coxfit2 <- coxph(survtime ~ (yschool + os30d + vagina + as.factor(condom) + abdpain +
                               dchexam + iinfct2 + as.factor(race) + 
                               as.factor(marital)), data = std1)
summary(coxfit2) #race y marital realmente no son significativas
extractAIC(coxfit2) #edf = 8, AIC = 4103.531
vif(coxfit2) #Variable condom causa un poco de problemas

coxfit3 <- coxph(survtime ~ (yschool + os30d + vagina + as.factor(condom) + abdpain + dchexam + iinfct2), data = std1)
summary(coxfit3) #condom tampoco es significativa
extractAIC(coxfit3) #edf = 8, AIC = 4101.218
vif(coxfit3) #condom causa un poco de problemas

coxfit4 <- coxph(survtime ~ (vagina + yschool + dchexam  + iinfct2 + abdpain + os30d), data = std1)
summary(coxfit4)
round(confint(coxfit4), 5)
extractAIC(coxfit4) #edf = 6, AIC = 4103.907
vif(coxfit4) #OK

################## Interacción entre as variables ###################
# Realizando tests para comprobar si las dos poblaciones
#son iguales.
# Entonces, nos interesa contrastar:
# Ho: So(t) = S1(t) para todo t > 0       vs.
# H1: So(t) != S1(t) para algun t > 0; 
#donde So, y S1 corresponde a las funciones de supervivencia 

# Prueba log-rank (Mantel-Haenszel):
survdiff(survtime ~ (vagina + yschool + dchexam  + iinfct2 + abdpain + os30d), rho = 0, 
         data = std1)
# Obtenemos que p-value = <2e-16 < 0.05 = nivel de significancia.
# Por lo que rechazo Ho a un nivel de confianza del 0.95. 

# Prueba log-rank (Peto-Peto -> Gehan-Wilxocon):
survdiff(survtime ~ (vagina + yschool + dchexam  + iinfct2 + abdpain + os30d), rho = 1, 
         data = std1)
# Obtenemos que p-value = <2e-16 < 0.05 = nivel de significancia.
# Por lo que rechazo Ho a un nivel de confianza del 0.95.

#Dado que el p-value es super pequeño, podemos decir que las variables
#si interactuan entre si, es decir rechazo que la supervivencia sea igual.
#Asi que si se tiene efecto en el modelo.


################## Verificación de supuestos#######################
#Las observaciones deben ser independientes y la tasa de riesgo debe ser 
#constante a lo largo del tiempo; es decir, la proporcionalidad de los riesgos 
#de un caso a otro no debe variar en función del tiempo. 
#El último supuesto se conoce como el supuesto de *proporcionalidad de los riesgos*.

################ VERIFICACIÃN DE SUPUESTOS Y CÃLCULO DE EXP(Beta_i) ################

# Utilizamos la funcion cox.zph para hacer el test al modelo
#Ho: los riesgos son proporcionales vs 
#H1: los riesgos no son proporcionales .
test <- cox.zph(coxfit4)
ggcoxzph(test)
test$table
#El P-Value global es de 0.3989, entonces No se rechaza los riesgos proporcionales
#Todos los p-values > 0.05, por lo que no hay evidencia suficiente para
#rechazar a H0. i.e. no se rechaza que el cociente de riesgos es
#independiente del tiempo.
plot(test[1], col = c("deeppink3","red2"), xlab = "Time (t)", 
     main = "Variable 'vagina: Examen de vagina' en el tiempo")
plot(test[2], col = c("chartreuse3","midnightblue"), xlab = "Time (t)", 
     main = "Variable 'yschool: Escolaridad' en el tiempo")
plot(test[3], col = c("turquoise2","yellow"), xlab = "Time (t)", 
     main = "Variable 'dchexam: Secreción en el examen' en el tiempo")
plot(test[4], col = c("purple3","orange"), xlab = "Time (t)", 
     main = "Variable 'iinfct2: Infeccion inicial' en el tiempo")
plot(test[5], col = c("indianred", "gold3"), xlab = "Time (t)", 
     main = "Varibale 'abdpain: Dolor abdominal' en el tiempo")
plot(test[6], col = c("royalblue3", "lightsalmon1"), xlab = "Time (t)", 
     main = "Variable 'os30d: Sexo en los últimos 30 d??as' en el tiempo")

#Grafica donde se muestra la estimación de las Beta_i para cada tiempo


##################### Ajuste global de los datos ####################
####################### (Residuos Cox - Snell) ######################
#En el caso de que se cumpla la hipÃ³tesis de riesgos proporcionales 
#los residuos deben agruparse de forma aleatoria a ambos lados del 
#valor 0 del eje Y

ggcoxdiagnostics(coxfit4, type = "schoenfeld", cox.scale = "time")

#################### Linealidad de las covariables ##################
###################### (Residuos de martingalas) ####################
##NO TIENE SENTIDO PUES LA MAYORIA DE LAS VARIABLES SON BINARIAS. 


######################### Residuos de devianza ######################

#Recordemos que la devianza de un modelo de regresion es el estadadistico que se
#utiliza para cuantificar hasta que punto el modelo actual que hemos estimado se
#aleja de un modelo teorico que se ajustase perfectamente a nuestros datos.
dev = residuals(coxfit4, type = "deviance")
min(dev)
max(dev)
summary(dev)

# Graficamos
plot(dev, col = "purple", main = "Residuos de devianza",
     xlab = "ID", ylab = "dev", pch = 20, ylim = c(-3,3))
abline(h = c(-1, 1), col = "plum")
abline(h = c(-2.5, 2.5), col = "lightsalmon")

# Casos extremos:
dev[abs(dev) >= 1] # Posicion

############################ Residuos DfBeta ########################

#La especificaciÃ³n del argumento type = dfbeta produce una matriz 
#de cambios estimados en los coeficientes de regresiÃ³n al eliminar cada 
#observaciÃ³n a su vez; asimismo, type = dfbetas produce los cambios estimados 
#en los coeficientes divididos por sus errores estÃ¡ndar 

dfbeta = resid(coxfit4, type = "dfbeta")
summary(dfbeta)

# graficamos para cada covariable
plot(dfbeta[,1], col ="purple", pch = 20, main = "Dfbeta - Examen de vagina ",
     ylab = "Dfbeta.Ex_Vagina")

plot(dfbeta[,2], col ="purple", pch = 20, main = "Dfbeta - Escolaridad",
     ylab = "Dfbeta.Escolaridad")

plot(dfbeta[,3], col ="purple", pch = 20, main = "Dfbeta - Secreción en el examen",
     ylab = "Dfbeta.Sec_examen")

plot(dfbeta[,4], col ="purple", pch = 20, main = "Dfbeta - Infeccion inicial ",
     ylab = "Dfbeta.In_inicial")

plot(dfbeta[,5], col ="purple", pch = 20, main = "Dfbeta - Dolor abdominal ",
     ylab = "Dfbeta.Dolor_abd")

plot(dfbeta[,6], col ="purple", pch = 20, main = "Dfbeta - Sexo en los últimos 30 d??as",
     ylab = "Dfbeta.Sexo_30 d??as")







