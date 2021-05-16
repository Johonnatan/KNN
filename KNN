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

####--- Treina KNN com normalizacao min-max
set.seed(1)

knn_model_minmax <- train(diabetes ~.,
                          data = treino,
                          method = 'knn',
                          preProcess = c('range')) #normalizacao min-max

knn_model_minmax #Visualiza resumo do treinamento

#A partir do algoritmo treinado, faz predicao nos dados separados para teste
predicoes_knn_minmax <- predict(knn_model_minmax, newdata = teste)

confusionMatrix(predicoes_knn_minmax,
                teste$diabetes,
                positive = 'pos')

####--- Treina KNN com padronizacao z-score
set.seed(1)

knn_model_zscore <- train(diabetes ~.,
                          data = treino,
                          method = 'knn',
                          preProcess = c('center','scale')) #padronizacao z-score

knn_model_zscore #Visualiza resumo do treinamento

#A partir do algoritmo treinado, faz predicao nos dados separados para teste
predicoes_knn_z <- predict(knn_model_zscore, newdata = teste)

confusionMatrix(predicoes_knn_z,
                teste$diabetes,
                positive = 'pos')
