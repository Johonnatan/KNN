#Remove objetos da memória do R
rm(list=ls(all=TRUE))

#Instala bibliotecas
install.packages('mlbench') #biblioteca com conjunto de dados Pima Indians Diabetes
install.packages('caret',dependencies = TRUE) #biblioteca para trabalhar com machine learning

#Carrega as bibliotecas
library(mlbench)
library(caret)

#Carrega o conjunto de dados PimaIndiansDiabetes na memoria do R
data(PimaIndiansDiabetes)

#Armazena o conjunto de dados PimaIndiansDiabetes em um data frame
dataframe <- PimaIndiansDiabetes

#visualiza as estatisticas descritivas das variaveis do conjunto de dados
summary(dataframe)

#Separa conjunto de dados para treino e teste no método de hold-out
conjunto <- createDataPartition(dataframe$diabetes, #Variavel resposta do conjunto de dados
                                p = 0.8, #Definir percentual para treino em 80%
                                list = F #Manter lista
)

#Separa a base de treino e teste
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
