library(VIM)
library(caret)
library(psych)
library(mlr)
# Wczytanie danych --------------------------------------------------------
dane<-read.csv("stroke.csv", sep = ";")


# Zamiana na wartosci numeryczne ------------------------------------------
str(dane)
dane[,2]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane[,2])))
dane[,8]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane[,8])))
dane[,9]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane[,9])))

#wyswietlenie brakow 
braki_w_kolumnach <- colSums(is.na(dane))

#eliminacja brakow
dane_S<-na.omit(dane)

# Modyfikacja danych i budowa modeli 
# Zbior stroke 
dummy_data <- dummyVars(~ work_type, data = dane_S)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_S))
dane_S <- cbind(dane_S, dummy_data_transformed)

dummy_data <- dummyVars(~ Residence_type, data = dane_S)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_S))
dane_S <- cbind(dane_S, dummy_data_transformed)

dummy_data <- dummyVars(~ smoking_status, data = dane_S)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_S))
dane_S <- cbind(dane_S, dummy_data_transformed)

dane_S$gender <- ifelse(dane_S$gender == "Male", 1, 0)
dane_S$ever_married <- ifelse(dane_S$ever_married == "Yes", 1, 0)
dane_S$Stroke <- dane_S$stroke
dane_S <- dane_S[,-c(6,7,10,11,16,17,22)]
str(dane_S$age)
dane_S$Stroke <- as.factor(dane_S$Stroke)
colnames(dane_S)[13] <- "col_13"
colnames(dane_S)[14] <- "col_14"
str(dane_S)
#sprawdzenie udziału obserwacji w klasie pozytywnej i negatywbnej 
table(dane_S$Stroke)

dane_S[,c(2,6,7)] <- scale(dane_S[,c(2,6,7)])


#podział danych 
set.seed(123)
ind <- sample(2, nrow(dane_S), replace = TRUE, prob = c(0.7, 0.3))
train_set <- dane_S[ind==1,]
test_set <- dane_S[ind==2,]

train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
#budowa modeli 

#model 1 regresja logistyczna
# Regresja logistyczna
formula <- Stroke~.
model <- glm(formula , data = train_set, family = binomial)

# Predykcja na zbiorze testowym
predictions_test <- predict(model, test_set, type = 'response')
# Predykcja na zbiorze treningowym
predictions_train <- predict(model, train_set, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_train <- ifelse(predictions_train >= 0.5, 1,0)
predicted_classes_train <- as.factor(predicted_classes_train)

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_test <- ifelse(predictions_test >= 0.5, 1,0)
predicted_classes_test <- as.factor(predicted_classes_test)

#macierz train data
confusionMatrix(predicted_classes_train, train_set$Stroke)

#macierz test data
confusionMatrix1 <- confusionMatrix(predicted_classes_test, test_set$Stroke)

#model2 svm jadro liniowe
#jądro liniowe

svm_Linear <- caret::train(Stroke~.,train_set,method ="svmLinear",
                    trControl= train_control,tuneLength =10)

svm_Linear$bestTune
#predykcja na zbiorze treningowym 
pred1 <- predict(svm_Linear,train_set)
confusionMatrix(pred1,train_set$Stroke)
#predykcja na zbiorze testowym 
pred2 <- predict(svm_Linear,test_set)
confusionMatrix2 <- confusionMatrix(pred2,test_set$Stroke)
#model wgl nie bierze pod uwage w przewidywaniu klasy pozytywnej

confusionMatrix1
confusionMatrix2

#tworzenie tasku

str(dane_S)
table(dane_S$Stroke)
task <- makeClassifTask(data = dane_S, target = "Stroke")
#oversampling 
dane_over <- oversample(task,22,"1")
dane_over <- getTaskData(dane_over)
table(dane_over$Stroke)
#undersampling 
dane_under <- undersample(task,0.05,"0")
dane_under <- getTaskData(dane_under)
table(dane_under$Stroke)


#podział danych oversampling
set.seed(123)
ind <- sample(2, nrow(dane_over), replace = TRUE, prob = c(0.7, 0.3))
train_set1 <- dane_over[ind==1,]
test_set1 <- dane_over[ind==2,]

#podział danych oversampling
set.seed(123)
ind <- sample(2, nrow(dane_under), replace = TRUE, prob = c(0.7, 0.3))
train_set2 <- dane_under[ind==1,]
test_set2 <- dane_under[ind==2,]

#modele oparte na oversamplingu

#model 1 regresja logistyczna
# Regresja logistyczna
formula <- Stroke~.
model <- glm(formula , data = train_set1, family = binomial)

# Predykcja na zbiorze testowym
predictions_test <- predict(model, test_set1, type = 'response')
# Predykcja na zbiorze treningowym
predictions_train <- predict(model, train_set1, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_train <- ifelse(predictions_train >= 0.5, 1,0)
predicted_classes_train <- as.factor(predicted_classes_train)

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_test <- ifelse(predictions_test >= 0.5, 1,0)
predicted_classes_test <- as.factor(predicted_classes_test)

#macierz train data
confusionMatrix(predicted_classes_train, train_set1$Stroke)
#macierz test data
confusionMatrix3 <-confusionMatrix(predicted_classes_test, test_set1$Stroke)

#model2 svm jadro liniowe
#jądro liniowe

svm_Linear <- caret::train(Stroke~.,train_set1,method ="svmLinear",
                    trControl= train_control,tuneLength =10)

svm_Linear$bestTune
#predykcja na zbiorze treningowym 
pred1 <- predict(svm_Linear,train_set1)
confusionMatrix(pred1,train_set1$Stroke)
#predykcja na zbiorze testowym 
pred2 <- predict(svm_Linear,test_set1)
confusionMatrix4 <- confusionMatrix(pred2,test_set1$Stroke)

#modele oparte na undersamplingu
#model 1 regresja logistyczna
# Regresja logistyczna
formula <- Stroke~.
model <- glm(formula , data = train_set2, family = binomial)

# Predykcja na zbiorze testowym
predictions_test <- predict(model, test_set2, type = 'response')
# Predykcja na zbiorze treningowym
predictions_train <- predict(model, train_set2, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_train <- ifelse(predictions_train >= 0.5, 1,0)
predicted_classes_train <- as.factor(predicted_classes_train)

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_test <- ifelse(predictions_test >= 0.5, 1,0)
predicted_classes_test <- as.factor(predicted_classes_test)

#macierz train data
confusionMatrix(predicted_classes_train, train_set2$Stroke)
#macierz test data
confusionMatrix5 <-confusionMatrix(predicted_classes_test, test_set2$Stroke)

#model2 svm jadro liniowe
#jądro liniowe

svm_Linear <- caret::train(Stroke~.,train_set2,method ="svmLinear",
                           trControl= train_control,tuneLength =10)
svm_Linear$bestTune


#predykcja na zbiorze treningowym 
pred1 <- predict(svm_Linear,train_set2)
confusionMatrix(pred1,train_set2$Stroke)
#predykcja na zbiorze testowym 
pred2 <- predict(svm_Linear,test_set2)
confusionMatrix6 <- confusionMatrix(pred2,test_set2$Stroke)

#porownanie wynikow 
confusionMatrix3
confusionMatrix4
confusionMatrix5
confusionMatrix6



# Przykładowe dane





proporcje_stroke <- c(0.1, 0.2, 0.3, 0.4)

# Liczba obserwacji w nowym zbiorze danych
liczba_obserwacji <- 2000

# Utworzenie nowych zbiorów danych
nowe_zbiory <- list()

set.seed(42)  # Ustawienie ziarna dla powtarzalności wyników losowego próbkowania

for (proporcja in proporcje_stroke) {
  # Liczba obserwacji dla każdej grupy w nowym zbiorze danych zgodnie z proporcją
  liczba_obserwacji_stroke <- round(proporcja * liczba_obserwacji)
  liczba_obserwacji_bez_stroke <- round((1 - proporcja) * liczba_obserwacji)
  
  # Podział danych na grupy
  grupa_stroke <- dane_S[dane_S$Stroke == 1, ]
  grupa_bez_stroke <- dane_S[dane_S$Stroke == 0, ]
  
  # Losowe próbkowanie z odpowiednimi proporcjami
  nowa_grupa_stroke <- grupa_stroke[sample(nrow(grupa_stroke), size = liczba_obserwacji_stroke, replace = TRUE), ]
  nowa_grupa_bez_stroke <- grupa_bez_stroke[sample(nrow(grupa_bez_stroke), size = liczba_obserwacji_bez_stroke, replace = TRUE), ]
  
  # Utworzenie nowego zbioru danych
  nowy_zbior_danych <- rbind(nowa_grupa_stroke, nowa_grupa_bez_stroke)
  
  # Dodanie nowego zbioru danych do listy
  nowe_zbiory[[paste0("proporcja_", proporcja)]] <- nowy_zbior_danych
}

# Wyświetlenie liczności zmiennej Stroke w każdym z nowych zbiorów danych
for (i in seq_along(nowe_zbiory)) {
  cat("Nowy zbiór danych", i, ":\n")
  print(table(nowe_zbiory[[i]]$Stroke))
  cat("\n")
}

dane01 <- nowe_zbiory[[1]]
dane02 <- nowe_zbiory[[2]]
dane03 <- nowe_zbiory[[3]]
dane04 <- nowe_zbiory[[4]]





#over sampling wyszedl lepiej wiec wybieramy ta metode 
task <- makeClassifTask(data = dane01, target = "Stroke")
#oversampling 
dane01_over <- oversample(task,9,"1")
dane01_over <- getTaskData(dane01_over)
table(dane01_over$Stroke)

task <- makeClassifTask(data = dane02, target = "Stroke")
#oversampling 
dane02_over <- oversample(task,4,"1")
dane02_over <- getTaskData(dane02_over)
table(dane02_over$Stroke)

task <- makeClassifTask(data = dane03, target = "Stroke")
#oversampling 
dane03_over <- oversample(task,2.35,"1")
dane03_over <- getTaskData(dane03_over)
table(dane03_over$Stroke)

task <- makeClassifTask(data = dane04, target = "Stroke")
#oversampling 
dane04_over <- oversample(task,1.5,"1")
dane04_over <- getTaskData(dane04_over)
table(dane04_over$Stroke)


str(dane03)

dane01
dane02
dane03
dane04

dane01_over
dane02_over
dane03_over
dane04_over
 


datasets <- list(dane01, dane02, dane03, dane04, dane01_over, dane02_over, dane03_over, dane04_over)

# Wektor z oryginalnymi nazwami zbiorów danych
nazwy_zbiorow <- c("dane01", "dane02", "dane03", "dane04", "dane01_over", "dane02_over", "dane03_over", "dane04_over")

# Lista do przechowywania wyników
wyniki <- list()

# Pętla po zbiorach danych
for (i in seq_along(datasets)) {
  # Podział danych
  set.seed(123)
  ind <- sample(2, nrow(datasets[[i]]), replace = TRUE, prob = c(0.7, 0.3))
  train_set <- datasets[[i]][ind == 1, ]
  test_set <- datasets[[i]][ind == 2, ]
  
  # Budowa modelu regresji logistycznej
  formula <- Stroke ~ .
  model <- glm(formula, data = train_set, family = binomial)
  
  # Predykcja na zbiorze testowym
  predictions_test <- predict(model, test_set, type = 'response')
  predicted_classes_test <- ifelse(predictions_test >= 0.5, 1, 0)
  predicted_classes_test <- as.factor(predicted_classes_test)
  # Macierz błędów na zbiorze testowym
  confusion_matrix <- confusionMatrix(predicted_classes_test, test_set$Stroke)
  
  nazwa_zbioru <- nazwy_zbiorow[i]
  wyniki[[nazwa_zbioru]] <- confusion_matrix
}

# Wyświetlenie wyników
for (nazwa_zbioru in names(wyniki)) {
  cat("Macierz błędów dla", nazwa_zbioru, ":\n")
  print(wyniki[[nazwa_zbioru]])
  cat("\n")
}
