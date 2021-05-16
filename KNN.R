rm(list=ls(all=TRUE)) #Remove objetos da memória do R

#Instala bibliotecas necessarias
install.packages('mlbench') #biblioteca mlbench disponibiliza varios conjunto de dados, incluindo o Pima Indians Diabetes
install.packages('caret',dependencies = TRUE) #biblioteca que tem enorme quantidade de ferramentas e algoritmos para trabalhar com machine learning
install.packages('rpart') #rpart traz o algoritmo de arvore de decisao
install.packages('rpart.plot') #o rpart.plot serve para visualizar a arvore de decisao gerado pelo rpart

#Carrega as bibliotecas
library(mlbench)
library(caret)
library(rpart)
library(rpart.plot)

#Carrega o conjunto de dados na memoria do R
data(PimaIndiansDiabetes)

#Armazena o conjunto de dados PimaIndiansDiabetes em um data frame com o nome dados
dados <- PimaIndiansDiabetes

#visualiza a media (mean) e outras estatisticas descritivas das variaveis
summary(dados)

#Separa conjunto de dados para treino e teste para hold-out
index <- createDataPartition(dados$diabetes, #Variavel resposta
                             p = 0.8, #Definir percentual para treino
                             list = F #Manter list = F
)
treino <- dados[index,]
teste <- dados[-index,]

#Remove objetos da memória do R
rm(list=ls(all=TRUE))

#Instala bibliotecas
install.packages('mlbench') #biblioteca com conjunto de dados Pima Indians Diabetes
install.packages('caret',dependencies = TRUE) #biblioteca para trabalhar com machine learning
install.packages('rpart') #rpart traz o algoritmo decision tree
install.packages('rpart.plot') #visualizar a arvore de decisao

#Carrega as bibliotecas
library(mlbench)
library(caret)
library(rpart)
library(rpart.plot)

#Carrega o conjunto de dados PimaIndiansDiabetes na memoria do R
data(PimaIndiansDiabetes)

#Armazena o conjunto de dados PimaIndiansDiabetes em um data frame com o nome dados
dataframe <- PimaIndiansDiabetes

#visualiza as estatisticas descritivas das variaveis do conjunto de dados
summary(dataframe)

#Separa conjunto de dados para treino e teste no método de hold-out
conjunto <- createDataPartition(dataframe$diabetes, #Variavel resposta do conjunto de dados
                                p = 0.8, #Definir percentual para treino em 80%
                                list = F #Manter lista
)

#
base_treino <- dataframe[conjunto,]
base_teste <- dataframe[-conjunto,]

#Planta a seamente
set.seed(1)

#Treina KNN com normalizacao min-max aplicada aos dados
knn_minmax <- train(diabetes ~.,
                          data = base_treino,
                          method = 'knn',
                          preProcess = c('range'))

#Visualiza resumo do treinamento
knn_minmax 

#Realiza predição nos dados separados para teste
predicoes <- predict(knn_minmax, newdata = base_teste)

confusionMatrix(predicoes,
                base_teste$diabetes,
                positive = 'pos')
