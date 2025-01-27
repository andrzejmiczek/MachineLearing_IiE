

dane <- read.csv("Heart2.csv", sep=";")

dane[,c(1,3,4,6,8)] <- scale(dane[,c(1,3,4,6,8)])
str(dane)
dane$heartDisease <- as.factor(dane$heartDisease)
set.seed(123)
ind <- sample(2, nrow(dane), replace = TRUE, prob = c(0.7, 0.3))
train_set <- dane[ind==1,]
test_set <- dane[ind==2,]
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)


#svm  jądro liniowe
svm_linear <- train(heartDisease~.,train_set,method ="svmLinear",
                    trControl= train_control,tuneLength =10)
svm_linear$bestTune
#predykcja na zbiorze treningowym 
pred1 <- predict(svm_linear,train_set)
confusionMatrix(pred1,train_set$heartDisease)
#predykcja na zbiorze testowym 
pred2 <- predict(svm_linear,test_set)
matrix_linear <- confusionMatrix(pred2,test_set$heartDisease)

#dobor parametru c 
svm_grid <- train(heartDisease~.,
                    train_set,
                    method ="svmLinear",
                    trControl= train_control,
                    tuneGrid = expand.grid(C = seq(001, 2, length = 20))
                    #preProcess = c("center", "scale")
                    )
plot(svm_grid)
svm_grid$bestTune
test_predgrid <- predict(svm_grid,test_set)
confusionMatrix(test_predgrid,test_set$heartDisease)


#jądro radialne
svm_Radial <- train(heartDisease~.,train_set,method ="svmRadial",
                    trControl= train_control,tuneLength =10)

svm_Radial$bestTune
#predykcja na zbiorze treningowym 
pred1 <- predict(svm_Radial,train_set)
confusionMatrix(pred1,train_set$heartDisease)
#predykcja na zbiorze testowym 
pred2 <- predict(svm_Radial,test_set)
matrix_radial <- confusionMatrix(pred2,test_set$heartDisease)


#dobor parametru c  i sigma
svm_grid <- train(heartDisease~.,
                  train_set,
                  method ="svmRadial",
                  trControl= train_control,
                  tuneGrid = expand.grid(C = seq(0.001, 2, length = 20), sigma = c(0.1, 0.5, 1, 2))
                  #preProcess = c("center", "scale")
)

plot(svm_grid)
svm_grid$bestTune
test_predgrid <- predict(svm_grid,test_set)
confusionMatrix(test_predgrid,test_set$heartDisease)

#jądro wielomianowe
svm_Poly <- train(heartDisease~.,train_set,method ="svmPoly",
                    trControl= train_control,tuneLength =3)

svm_Poly$bestTune
#predykcja na zbiorze treningowym 
pred1 <- predict(svm_Poly,train_set)
confusionMatrix(pred1,train_set$heartDisease)
#predykcja na zbiorze testowym 
pred2 <- predict(svm_Poly,test_set)
matrix_Poly <- confusionMatrix(pred2,test_set$heartDisease)


#dobor parametrów
svm_grid <- train(heartDisease~.,
                  train_set,
                  method ="svmPoly",
                  TrControl= train_control,
                  tuneGrid = expand.grid(
                  C = seq(0.001, 2, length = 20),
                  degree = c(2, 3, 4),  
                  scale = c(0.1, 0.5, 1, 2)) #nie jestem pewien czy mozna tunowac ten 
                  #parametr poniewaz odpowiada za scalowanie danych, a nasze dane posiadaja zmienne binarne których nie powinno sie scalowac
  #preProcess = c("center", "scale")
)

plot(svm_grid)
svm_grid$bestTune
test_predgrid <- predict(svm_grid,test_set)
confusionMatrix(test_predgrid,test_set$heartDisease)

#model regresji logitycznej
formula <- heartDisease~.
model <- glm(formula , data = train_set, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set, type = 'response')

#Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
matrix_Regresja <- confusionMatrix(predicted_classes, test_set$heartDisease)

#porównanie 
wyniki <- data.frame(
  Model = c("Linear", "Radial", "Poly", "Regresja"),
  Accuracy = c(
    matrix_linear$overall[1],
    matrix_radial$overall[1],
    matrix_Poly$overall[1],
    matrix_Regresja$overall[1]
  ),
  Specificity = c(
    matrix_linear$byClass[2],
    matrix_radial$byClass[2],
    matrix_Poly$byClass[2],
    matrix_Regresja$byClass[2]
  )
)





