#Tarea 1 MAT281

#Problema 3 ####################################################################

#a
library(naivebayes)

datos= read.table(file="C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/Escritorio/gustos_musicales.txt", header=TRUE, sep= ",")
attach(datos)

pinkfloyd <- c(0,0,0,0,0,1,1,0,1,1)
thebeatles <- c(1,1,0,0,0,0,1,1,1,0)
queen <- c(0,1,0,0,0,0,1,1,1,0)
oasis <- c(1,1,0,0,0,0,0,0,0,0)
radiohead <- c(1,0,1,1,0,0,1,0,1,1)
greenday <- c(1,0,1,0,0,0,0,0,0,0)
thestrokes <- c(0,0,1,0,0,0,0,0,0,0)
linkinpark <- c(0,0,0,1,0,0,0,0,0,0)
foofighters <- c(0,0,0,1,0,1,0,0,0,0)
coldplay <- c(1,1,1,1,1,0,0,0,0,0)

x <- matrix( c(pinkfloyd, thebeatles, queen, oasis, radiohead, greenday, thestrokes, linkinpark, foofighters, coldplay),nrow = 10, ncol = 10)
colnames(x)=c("pinkfloyd","thebeatles","queen","oasis","radiohead","greenday","thestrokes","linkinpark","foofighters","coldplay")
y=as.factor(datos$etiqueta)
modelobnb <- bernoulli_naive_bayes(x=x , y=y )
#distribución marginal
coef(modelobnb)
cor(x)

#b
preferencias <-matrix(c(0,0,0,1,0,1,1,1,0,1), nrow=1, ncol=10)
colnames(preferencias)=c("pinkfloyd","thebeatles","queen","oasis","radiohead","greenday","thestrokes","linkinpark","foofighters","coldplay")
predict(modelobnb,type = "prob",newdata=preferencias)


#Problema 4 ####################################################################

datos2= read.table(file=("C:/Users/usuario/OneDrive - Universidad Técnica Federico Santa María/Escritorio/datos_heart_disease.txt"), header=TRUE, sep= ",")
attach(datos2)
data=data.frame(chd,sbp,tobacco,ldl,famhist,obesity,alcohol,age)

data$famhist <- ifelse(data$famhist == "Absent",0,1)
modelo_log<-glm(chd~sbp+tobacco+ldl+famhist+obesity+alcohol+age,family=binomial(link="logit"))
summary(modelo_log)

#eliminamos alcohol
modelo_log1<-glm(chd~sbp+tobacco+ldl+famhist+obesity+age,family=binomial(link="logit")) 
summary(modelo_log1)
#eliminamos obesity
modelo_log2<-glm(chd~sbp+tobacco+ldl+famhist+age,family=binomial(link="logit"))
summary(modelo_log2)
#eliminamos sbp
modelo_log3<-glm(chd~tobacco+ldl+famhist+age,family=binomial(link="logit")) 
summary(modelo_log3)




