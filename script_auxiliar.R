set.seed(123)
load("dataset_cardio.RData")
require(FactoMineR)
require(dplyr)
require(flextable)
require(factoextra)
dataset<-as.data.frame(dataset)
dataset[ , colnames(dataset)] <- lapply(dataset[ , colnames(dataset)], as.character)

dataset$Tipo.De.Leito<-"OUTRO_TIPO_DE_LEITO"
dataset[ , colnames(dataset)] <- lapply(dataset[ , colnames(dataset)], as.factor)

dataset<-na.omit(dataset)
res.mca <- MCA (dataset, graph = FALSE)

library("factoextra")
eig.val <- get_eigenvalue(res.mca)
# head(eig.val)

#fviz_mca_biplot (res.mca, repel = TRUE, 
#                ggtheme = theme_minimal())


var <- get_mca_var(res.mca)

acm1<-fviz_mca_var (res.mca, choice = "mca.cor",
                    repel = TRUE, 
                    ggtheme = theme_minimal ())



acm2<-fviz_mca_var (res.mca,
                    repel = T, 
                    ggtheme = theme_minimal ())

dataset[dataset=="NA"]<-NA
dataset[ , colnames(dataset)] <- lapply(dataset[ , colnames(dataset)], as.factor)

dataset$Estadosolicitacao<-droplevels(dataset$Estadosolicitacao)
dataset$Estadosolicitacao<-as.factor(dataset$Estadosolicitacao)
dataset<-na.omit(dataset)



acm3<-fviz_cos2(res.mca, choice = "var", axes = 1:2)

save(acm1,file="ACM1.RData")
save(acm2,file="ACM2.RData")
save(acm3,file="ACM3.RData")

####

load("dataset_cardio.RData")



a<-klaR::NaiveBayes(dataset,dataset$Estadosolicitacao)
a<-lapply(colnames(dataset), function(x){as.data.frame(a[["tables"]][[x]])})
a<-dplyr::bind_rows(a[[1]],a[[2]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]],a[[8]],a[[9]],a[[10]])
nb<-subset(a,a$Freq>0.5)[,1:3]
teste<-a[,1:3]
teste$Peso<-as.integer(teste$Freq*100)
teste<-data.frame(grupo=teste$grouping[is.na(teste$Peso)==F&teste$Peso>0],Variavel=teste$var[is.na(teste$Peso)==F&teste$Peso>0],Peso=teste$Peso[is.na(teste$Peso)==F&teste$Peso>0])
teste$Variavel<-as.character(teste$Variavel)
a$var<-as.character(a$var)
a$var[a$var=="NAO"]<-"SEM MANDATO"

teste$Variavel[teste$Variavel=="NAO"]<-"SEM MANDATO"
cancelada<-subset(teste,teste$grupo=="CANCELADA")
alta<-subset(teste,teste$grupo=="ALTA")
cancelada<-subset(cancelada,cancelada$Variavel!="CANCELADA")
alta<-subset(alta,alta$Variavel!="ALTA")
cancelada<-subset(cancelada,cancelada$Variavel!="ALTA")
alta<-subset(alta,alta$Variavel!="CANCELADA")
km<-klaR::kmodes(as.matrix(dataset),2)$modes

km<-subset(km,select = c(Estadosolicitacao,Tem.Mandado.Judicial, Carater.Internacao ,Central.Regulacao , Especialidade,Tipo.De.Leito ,      Procedimento   ,     Municipio.Paciente ,Ano.Evento, faixa))

tabelakm<-flextable::htmltools_value(flextable::flextable(km) %>% flextable::bg(km$Estadosolicitacao=='ALTA',bg="lightblue")%>% flextable::bg(km$Estadosolicitacao=='CANCELADA',bg="lightpink") %>% bold(i=1,j=1) %>% bold(i=2,j=1)  )

save(cancelada,file="cancelada.RData")

save(km,file="KM.RData")
save(tabelakm,file="TABELA_KM.RData")

################################################3333
library(neuralnet)
library(rpart.plot)
library(caret)

indexes=createDataPartition(dataset$Estadosolicitacao, p=.95, list = F)
train = dataset[indexes, ]
test = dataset[-indexes, ]

#AD

library(caret)
library(Amelia)
library(pROC)
library(mgcv)


# Definindo a semente
set.seed(123)

# Vamos utilizar uma validação-cruzada 10-fold
ctrl <- trainControl(method = "cv", 
                     number = 10,
                     summaryFunction = twoClassSummary, 
                     classProbs = TRUE)

# Temos que mudar a variável - quando a gente usa twoClassSummary
levels(train$Estadosolicitacao) <- c("ALTA", "CANCELADA")
levels(test$Estadosolicitacao) <- c("ALTA", "CANCELADA")

dtFit <- train( Estadosolicitacao ~ .,
                method     = "rpart2", # usa profundidade máxima (caso rpart, o parâmetro seria o de complexidade)
                tuneLength = 20,
                trControl  = ctrl,
                data = dataset) 

graficoad<-rpart.plot(dtFit$finalModel,  
                      extra = 4, # informações extras nos nós
                      type = 4,  # tipo de gráfico
                      box.palette = "RdYlGn") # cor

save(dtFit,file="Graficoad.RData")

##### tabela nb

tabelanb<-htmltools_value(flextable::flextable(a[,1:3])%>% theme_zebra())

save(tabelanb,file="tabelanb.RData")
