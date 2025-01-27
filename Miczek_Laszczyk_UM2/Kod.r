library(VIM)
library(caret)
library(psych)

# Wczytanie danych --------------------------------------------------------
dane_S<-read.csv("zad2_Stroke.csv", sep = ";")
dane_A<-read.csv("zad2_Airline.csv", sep = ";")
dane_H<-read.csv("zad2_Heart.csv", sep = ";")

# Zamiana na wartosci numeryczne ------------------------------------------
str(dane_S)
dane_S[,2]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_S[,2])))
dane_S[,8]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_S[,8])))
dane_S[,9]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_S[,9])))

str(dane_H)
dane_H[,10]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_H[,10])))
#wyswietlenie brakow 
braki_w_kolumnachS <- colSums(is.na(dane_S))
braki_w_kolumnachH <- colSums(is.na(dane_H))
braki_w_kolumnachA <- colSums(is.na(dane_A))

# Metoda I ----------------------------------------------------------------
dane_S1<-na.omit(dane_S)
dane_A1<-na.omit(dane_A)
dane_H1<-na.omit(dane_H)



# Modyfikacja danych i budowa modeli 
# Zbior stroke 
dummy_data <- dummyVars(~ work_type, data = dane_S1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_S1))
dane_S1 <- cbind(dane_S1, dummy_data_transformed)

dummy_data <- dummyVars(~ Residence_type, data = dane_S1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_S1))
dane_S1 <- cbind(dane_S1, dummy_data_transformed)

dummy_data <- dummyVars(~ smoking_status, data = dane_S1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_S1))
dane_S1 <- cbind(dane_S1, dummy_data_transformed)

dane_S1$gender <- ifelse(dane_S1$gender == "Male", 1, 0)
dane_S1$ever_married <- ifelse(dane_S1$ever_married == "Yes", 1, 0)
dane_S1$Stroke <- dane_S1$stroke
dane_S1 <- dane_S1[,-c(6,7,10,11,16,17)]
str(dane_S1$age)
dane_S1$Stroke <- as.factor(dane_S1$Stroke)
# Podzial na zbior uczacy i testowy dla zbioru stroke
set.seed(123)
ind <- sample(2, nrow(dane_S1), replace = TRUE, prob = c(0.7, 0.3))
train_set1 <- dane_S1[ind==1,]
test_set1  <- dane_S1[ind==2,]

# Regresja logistyczna
formula <- Stroke~.
model <- glm(formula , data = train_set1, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set1, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set1$Stroke)


# Zbior Heart
str(dane_H1)
dane_H1$Sex <- ifelse(dane_H1$Sex == "M", 1, 0)
dane_H1$ExerciseAngina <- ifelse(dane_H1$ExerciseAngina == "N",1,0)
dane_H1$Oldpeak <- as.numeric(gsub(",", ".", gsub("\\.", "", dane_H1$Oldpeak)))
dane_H1$HeartDisease <- as.factor(dane_H1$HeartDisease)


dummy_data <- dummyVars(~ ChestPainType, data = dane_H1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_H1))
dane_H1 <- cbind(dane_H1, dummy_data_transformed)

dummy_data <- dummyVars(~ RestingECG, data = dane_H1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_H1))
dane_H1 <- cbind(dane_H1, dummy_data_transformed)

dummy_data <- dummyVars(~ ST_Slope, data = dane_H1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_H1))
dane_H1 <- cbind(dane_H1, dummy_data_transformed)

dane_H1$heartDisease <- dane_H1$HeartDisease
dane_H1 <- dane_H1[,-c(3,7,11,12,16,19,22)]
str(dane_H1)

# Podzial na zbior uczacy i testowy heart 
set.seed(123)
ind <- sample(2, nrow(dane_H1), replace = TRUE, prob = c(0.7, 0.3))
train_set2 <- dane_H1[ind==1,]
test_set2  <- dane_H1[ind==2,]

# Model 
formula <- heartDisease~.
model <- glm(formula , data = train_set2, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set2, type = 'response')

#Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set2$heartDisease)

# Zbior Airline 
str(dane_A1)

dane_A1$satisfaction <- as.factor(dane_A1$satisfaction)
dane_A1$satisfaction <- as.integer(dane_A1$satisfaction)-1
dane_A1$satisfaction <- as.factor(dane_A1$satisfaction)

dane_A1$Gender <- ifelse(dane_A1$Gender == "Male",1,0)
dane_A1$Customer.Type <- ifelse(dane_A1$Customer.Type == "Loyal Customer",1,0)
dane_A1$Type.of.Travel <- ifelse(dane_A1$Type.of.Travel == "Personal Travel",1,0)

dummy_data <- dummyVars(~ Class, data = dane_A1)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_A1))
dane_A1 <- cbind(dane_A1, dummy_data_transformed)

dane_A1$Satisfaction <- dane_A1$satisfaction 
dane_A1 <- dane_A1[,-c(1,6,26)]
str(dane_A1)

# Podzial na zbior uczacy i testowy heart 
set.seed(123)
ind <- sample(2, nrow(dane_A1), replace = TRUE, prob = c(0.7, 0.3))
train_set3 <- dane_A1[ind==1,]
test_set3  <- dane_A1[ind==2,]

# Model 
formula <- Satisfaction~.
model <- glm(formula , data = train_set3, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set3, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set3$Satisfaction)



# Sprawdzenie wartosci odstających ----------------------------------------
dane_S3<-na.omit(dane_S)
dane_A3<-na.omit(dane_A)
dane_H3<-na.omit(dane_H)
# Funkcja do wykrywania wartosci odstających na podstawie odchylenia standardowego
detect_outliers_std <- function(series, n_std = 3) {
  mean_val <- mean(series)
  std_val <- sd(series)
  lower_bound <- mean_val - n_std * std_val
  upper_bound <- mean_val + n_std * std_val
  outliers <- (series < lower_bound) | (series > upper_bound)
  return(data.frame(Liczba_Obserwacji = sum(outliers), Dolna_Granica = lower_bound, Gorna_Granica = upper_bound))
}


# Obliczenie wartosci odstajacych dla kazdego zbioru danych ---------------
# Funkcje do wykrywania wartosci odstających dla kazdej kolumny
outliers_std <- lapply(dane_A3, detect_outliers_std)
# Tworzenie ramki danych
outliers_std_df <- do.call(rbind, outliers_std)
outliers_std_df$Nazwa_Zmiennej <- rownames(outliers_std_df)
rownames(outliers_std_df) <- NULL
# Wyswietlenie wynikowych ramek danych
print("\nOutliers Standard Deviation:")
print(outliers_std_df) 

# Funkcje do wykrywania wartosci odstających dla kazdej kolumny
outliers_std <- lapply(dane_H3, detect_outliers_std)
# Tworzenie ramki danych dla obu metod
outliers_std_df <- do.call(rbind, outliers_std)
outliers_std_df$Nazwa_Zmiennej <- rownames(outliers_std_df)
rownames(outliers_std_df) <- NULL
# Wyswietlenie wynikowych ramek danych
print("\nOutliers Standard Deviation:")
print(outliers_std_df)

# Funkcje do wykrywania wartosci odstających dla kazdej kolumny
outliers_std <- lapply(dane_S3, detect_outliers_std)
# Tworzenie ramki danych dla obu metod
outliers_std_df <- do.call(rbind, outliers_std)
outliers_std_df$Nazwa_Zmiennej <- rownames(outliers_std_df)

rownames(outliers_std_df) <- NULL
# Wyswietlenie wynikowych ramek danych
print("\nOutliers Standard Deviation:")
print(outliers_std_df)



# Metoda II ---------------------------------------------------------------

# Dane_A
sum(is.na(dane_A$Customer.Type))
unique(dane_A$Customer.Type)
table(dane_A$Customer.Type)
dane_A$Customer.Type[is.na(dane_A$Customer.Type)] <- sample (c('Loyal Customer','disloyal Customer'), prob=c(.80,.20))

sum(is.na(dane_A[,4]))
dane_A$Age[is.na(dane_A[,4])]<-round(mean(dane_A[,4],na.rm=TRUE),0)
unique(dane_A[,4])
table(dane_A[,4])

sum(is.na(dane_A[,11]))
unique(dane_A[,11])
table(dane_A[,11])
dane_A$Gate.location[is.na(dane_A[,11])]<-3
sum(is.na(dane_A[,11]))

sum(is.na(dane_A[,23]))
median(dane_A1[,23])
unique(dane_A[,23])
table(dane_A[,23])
dane_A$Arrival.Delay.in.Minutes[is.na(dane_A[,23])]<-median(dane_A1[,23])
sum(is.na(dane_A[,23]))

sum(is.na(dane_A))

# Dane_H
sum(is.na(dane_H[,1]))
unique(dane_H[,1])
table(dane_H[,1])
dane_H$Age[is.na(dane_H[,1])]<-round(mean(dane_H[,1],na.rm=TRUE),0)
sum(is.na(dane_H[,1]))

sum(is.na(dane_H[,5]))
unique(dane_H[,5])
table(dane_H[,5])
dane_H$Cholesterol[is.na(dane_H[,5])]<-median(dane_H1[,5])
sum(is.na(dane_H[,5]))

sum(is.na(dane_H[,7]))
unique(dane_H[,7])
table(dane_H[,7])
dane_H$RestingECG[is.na(dane_H[,7])]<-"Normal"
sum(is.na(dane_H[,7]))

sum(is.na(dane_H[,9]))
unique(dane_H[,9])
table(dane_H[,9])
dane_H$ExerciseAngina[is.na(dane_H$ExerciseAngina)] <- sample (c('N','Y'), prob=c(.6,.4))
sum(is.na(dane_H[,9]))

sum(is.na(dane_H[,10]))
unique(dane_H[,10])
table(dane_H[,10])
dane_H$Oldpeak[is.na(dane_H[,10])]<-median(dane_H1[,10])
sum(is.na(dane_H[,10]))

sum(is.na(dane_H))

# Dane_S
sum(is.na(dane_S[,4]))
unique(dane_S[,4])
table(dane_S[,4])
dane_S$heart_disease[is.na(dane_S[,4])]<-0
sum(is.na(dane_S[,4]))

sum(is.na(dane_S[,6]))
unique(dane_S[,6])
table(dane_S[,6])
dane_S$work_type[is.na(dane_S[,6])]<-"Private"
sum(is.na(dane_S[,6]))

sum(is.na(dane_S[,8]))
unique(dane_S[,8])
table(dane_S[,8])
dane_S$avg_glucose_level[is.na(dane_S[,8])]<-round(mean(dane_H[,8],na.rm=TRUE),0)
sum(is.na(dane_S[,8]))

sum(is.na(dane_S[,9]))
unique(dane_S[,9])
table(dane_S[,9])
dane_S$bmi[is.na(dane_S[,9])]<-median(dane_S1[,9])
sum(is.na(dane_S[,9]))

sum(is.na(dane_S))


# Budowa modelu dla metody II ----------------------------------------------
dane_SII<-dane_S
dane_AII<-dane_A
dane_HII<-dane_H
#dane_SII
#dane_AII
#dane_HII


# Modyfikacja danych i budowa modeli 
# Zbior stroke 
dummy_data <- dummyVars(~ work_type, data = dane_SII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_SII))
dane_SII <- cbind(dane_SII, dummy_data_transformed)

dummy_data <- dummyVars(~ Residence_type, data = dane_SII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_SII))
dane_SII <- cbind(dane_SII, dummy_data_transformed)

dummy_data <- dummyVars(~ smoking_status, data = dane_SII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_SII))
dane_SII <- cbind(dane_SII, dummy_data_transformed)

dane_SII$gender <- ifelse(dane_SII$gender == "Male", 1, 0)
dane_SII$ever_married <- ifelse(dane_SII$ever_married == "Yes", 1, 0)
dane_SII$Stroke <- dane_SII$stroke
dane_SII <- dane_SII[,-c(6,7,10,11,16,17)]
str(dane_SII$age)
dane_SII$Stroke <- as.factor(dane_SII$Stroke)
# Podzial na zbior uczacy i testowy dla zbioru stroke
set.seed(123)
ind <- sample(2, nrow(dane_SII), replace = TRUE, prob = c(0.7, 0.3))
train_set1 <- dane_SII[ind==1,]
test_set1  <- dane_SII[ind==2,]

# Regresja logistyczna
formula <- Stroke~.
model <- glm(formula , data = train_set1, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set1, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set1$Stroke)


# Zbior Heart
str(dane_HII)
dane_HII$Sex <- ifelse(dane_HII$Sex == "M", 1, 0)
dane_HII$ExerciseAngina <- ifelse(dane_HII$ExerciseAngina == "N",1,0)
dane_HII$Oldpeak <- as.numeric(gsub(",", ".", gsub("\\.", "", dane_HII$Oldpeak)))
dane_HII$HeartDisease <- as.factor(dane_HII$HeartDisease)


dummy_data <- dummyVars(~ ChestPainType, data = dane_HII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_HII))
dane_HII <- cbind(dane_HII, dummy_data_transformed)

dummy_data <- dummyVars(~ RestingECG, data = dane_HII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_HII))
dane_HII <- cbind(dane_HII, dummy_data_transformed)

dummy_data <- dummyVars(~ ST_Slope, data = dane_HII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_HII))
dane_HII <- cbind(dane_HII, dummy_data_transformed)

dane_HII$heartDisease <- dane_HII$HeartDisease
dane_HII <- dane_HII[,-c(3,7,11,12,16,19,22)]
str(dane_HII)

# Podzial na zbior uczacy i testowy heart 
set.seed(123)
ind <- sample(2, nrow(dane_HII), replace = TRUE, prob = c(0.7, 0.3))
train_set2 <- dane_HII[ind==1,]
test_set2  <- dane_HII[ind==2,]

# Model 
formula <- heartDisease~.
model <- glm(formula , data = train_set2, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set2, type = 'response')

#Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set2$heartDisease)

# Zbior Airline 
str(dane_AII)

dane_AII$satisfaction <- as.factor(dane_AII$satisfaction)
dane_AII$satisfaction <- as.integer(dane_AII$satisfaction)-1
dane_AII$satisfaction <- as.factor(dane_AII$satisfaction)

dane_AII$Gender <- ifelse(dane_AII$Gender == "Male",1,0)
dane_AII$Customer.Type <- ifelse(dane_AII$Customer.Type == "Loyal Customer",1,0)
dane_AII$Type.of.Travel <- ifelse(dane_AII$Type.of.Travel == "Personal Travel",1,0)

dummy_data <- dummyVars(~ Class, data = dane_AII)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = dane_AII))
dane_AII <- cbind(dane_AII, dummy_data_transformed)

dane_AII$Satisfaction <- dane_AII$satisfaction 
dane_AII <- dane_AII[,-c(1,6,26)]
str(dane_AII)

# Podzial na zbior uczacy i testowy heart 
set.seed(123)
ind <- sample(2, nrow(dane_AII), replace = TRUE, prob = c(0.7, 0.3))
train_set3 <- dane_AII[ind==1,]
test_set3  <- dane_AII[ind==2,]

# Model 
formula <- Satisfaction~.
model <- glm(formula , data = train_set3, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set3, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set3$Satisfaction)






# Metoda III i budowa modelu---------------------------------------------------------------
install.packages("VIM")
library(VIM)
library(caret)

dane_S<-read.csv("zad2_Stroke.csv", sep = ";")
dane_A<-read.csv("zad2_Airline.csv", sep = ";")
dane_H<-read.csv("zad2_Heart.csv", sep = ";")

dane_S[,2]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_S[,2])))
dane_S[,8]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_S[,8])))
dane_S[,9]<-as.numeric(gsub(",", ".", gsub("\\.", "", dane_S[,9])))
dane_S$age <- as.integer(dane_S$age)
dane_S$stroke <- as.factor(dane_S$stroke)
columns_with_na1 <- colnames(dane_A)[colSums(is.na(dane_A)) > 0]
columns_with_na2 <- colnames(dane_S)[colSums(is.na(dane_S)) > 0]
columns_with_na3 <- colnames(dane_H)[colSums(is.na(dane_H)) > 0]

# Imputacja brakujacych danych za pomoca k-nn
imputed_A <- kNN(dane_A,columns_with_na1, k = 5,imp_var = FALSE)
imputed_S <- kNN(dane_S,columns_with_na2, k = 5,imp_var = FALSE)
imputed_H <- kNN(dane_H,columns_with_na3, k = 5,imp_var = FALSE)

# Modyfikacja danych 
# Zbior stroke 
dummy_data <- dummyVars(~ work_type, data = imputed_S)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_S))
imputed_S <- cbind(imputed_S, dummy_data_transformed)

dummy_data <- dummyVars(~ Residence_type, data = imputed_S)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_S))
imputed_S <- cbind(imputed_S, dummy_data_transformed)

dummy_data <- dummyVars(~ smoking_status, data = imputed_S)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_S))
imputed_S <- cbind(imputed_S, dummy_data_transformed)

imputed_S$gender <- ifelse(imputed_S$gender == "Male", 1, 0)
imputed_S$ever_married <- ifelse(imputed_S$ever_married == "Yes", 1, 0)
imputed_S$Stroke <- imputed_S$stroke
imputed_S <- imputed_S[,-c(6,7,10,11,16,17)]
str(imputed_S$age)

# Podzial na zbior uczacy i testowy dla zbioru stroke
set.seed(123)
ind <- sample(2, nrow(imputed_S), replace = TRUE, prob = c(0.7, 0.3))
train_set1 <- imputed_S[ind==1,]
test_set1  <- imputed_S[ind==2,]

# Regresja logistyczna
formula <- Stroke~.
model <- glm(formula , data = train_set1, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set1, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set1$Stroke)


# Zbior Heart
str(imputed_H)
imputed_H$Sex <- ifelse(imputed_H$Sex == "M", 1, 0)
imputed_H$ExerciseAngina <- ifelse(imputed_H$ExerciseAngina == "N",1,0)
imputed_H$Oldpeak <- as.numeric(gsub(",", ".", gsub("\\.", "", imputed_H$Oldpeak)))
imputed_H$HeartDisease <- as.factor(imputed_H$HeartDisease)


dummy_data <- dummyVars(~ ChestPainType, data = imputed_H)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_H))
imputed_H <- cbind(imputed_H, dummy_data_transformed)

dummy_data <- dummyVars(~ RestingECG, data = imputed_H)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_H))
imputed_H <- cbind(imputed_H, dummy_data_transformed)

dummy_data <- dummyVars(~ ST_Slope, data = imputed_H)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_H))
imputed_H <- cbind(imputed_H, dummy_data_transformed)

imputed_H$heartDisease <- imputed_H$HeartDisease
imputed_H <- imputed_H[,-c(3,7,11,12,16,19,22)]
str(imputed_H)

# Podzial na zbior uczacy i testowy heart 
set.seed(123)
ind <- sample(2, nrow(imputed_H), replace = TRUE, prob = c(0.7, 0.3))
train_set2 <- imputed_H[ind==1,]
test_set2  <- imputed_H[ind==2,]

# Model 
formula <- heartDisease~.
model <- glm(formula , data = train_set2, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set2, type = 'response')

#Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set2$heartDisease)

# Zbior Airline 
str(imputed_A)

imputed_A$satisfaction <- as.factor(imputed_A$satisfaction)
imputed_A$satisfaction <- as.integer(imputed_A$satisfaction)-1
imputed_A$satisfaction <- as.factor(imputed_A$satisfaction)

imputed_A$Gender <- ifelse(imputed_A$Gender == "Male",1,0)
imputed_A$Customer.Type <- ifelse(imputed_A$Customer.Type == "Loyal Customer",1,0)
imputed_A$Type.of.Travel <- ifelse(imputed_A$Type.of.Travel == "Personal Travel",1,0)

dummy_data <- dummyVars(~ Class, data = imputed_A)
dummy_data_transformed <- as.data.frame(predict(dummy_data, newdata = imputed_A))
imputed_A <- cbind(imputed_A, dummy_data_transformed)

imputed_A$Satisfaction <- imputed_A$satisfaction 
imputed_A <- imputed_A[,-c(1,6,26)]
str(imputed_A)

# Podzial na zbior uczacy i testowy heart 
set.seed(123)
ind <- sample(2, nrow(imputed_A), replace = TRUE, prob = c(0.7, 0.3))
train_set3 <- imputed_A[ind==1,]
test_set3  <- imputed_A[ind==2,]

# Model 
formula <- Satisfaction~.
model <- glm(formula , data = train_set3, family = binomial)

# Predykcja na zbiorze testowym
predictions <- predict(model, test_set3, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes <- ifelse(predictions >= 0.5, 1,0)
predicted_classes <- as.factor(predicted_classes)
confusionMatrix(predicted_classes, test_set3$Satisfaction)


#porownanie statystyk 
dane_S<-read.csv("zad2_Stroke.csv", sep = ";")
dane_A<-read.csv("zad2_Airline.csv", sep = ";")
dane_H<-read.csv("zad2_Heart.csv", sep = ";")

braki_w_kolumnachS <- colSums(is.na(dane_S))
braki_w_kolumnachH <- colSums(is.na(dane_H))
braki_w_kolumnachA <- colSums(is.na(dane_A))

#braki w kolumnach Age, customer.type,gate.location, arrival.delay.in.minutes
describe(dane_A)
describe(dane_A1)
describe(dane_AII)
describe(imputed_A)

#braki w kolumnach Age, cholesterol, restingECG, excersiceangina, oldpeak
describe(dane_H)
describe(dane_H1)
describe(dane_HII)
describe(imputed_H)

#braki w kolumnach heart_disease,work_type, avg_glucose_level, bmi 
describe(dane_S)
describe(dane_S1)
describe(dane_SII)
describe(imputed_S)
#zmiana rozkladu zmiennych 
#do oceny zmianu rozkladu zmiennych posluzy nam wykres gestosci, 
#dla przykladu zostanie omowiona jedna zmienna z kazdego zestawu  

plot(density(dane_A1$Age), main = "Wykres gęstości zmiennej x")
plot(density(dane_AII$Age), main = "Wykres gęstości zmiennej x")
plot(density(imputed_A$Age), main = "Wykres gęstości zmiennej x")

plot(density(dane_H1$Age), main = "Wykres gęstości zmiennej x")
plot(density(dane_HII$Age), main = "Wykres gęstości zmiennej x")
plot(density(imputed_H$Age), main = "Wykres gęstości zmiennej x")

plot(density(dane_S1$avg_glucose_level), main = "Wykres gęstości zmiennej x")
plot(density(dane_SII$avg_glucose_level), main = "Wykres gęstości zmiennej x")
plot(density(imputed_S$avg_glucose_level), main = "Wykres gęstości zmiennej x")

