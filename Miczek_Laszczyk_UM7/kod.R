install.packages("DALEX")
install.packages("rms")
library(VIM)
library(caret)
library(psych)
library(mlr)
library(DALEX)
library(rms)
library(ggplot2)

dane <- read.csv("Heart2.csv", sep=";")
#dane[,c(1,3,4,6,8)] <- scale(dane[,c(1,3,4,6,8)])
dane$heartDisease <- as.factor(dane$heartDisease)
dane$FastingBS <- as.factor(dane$FastingBS)
dane$Sex <- as.factor(dane$Sex) 
obserwacja <- dane[200,]
str(dane)
#podział danych 
set.seed(123)
ind <- sample(2, nrow(dane), replace = TRUE, prob = c(0.7, 0.3))
train_set <- dane[ind==1,]
test_set <- dane[ind==2,]

#model 1 regresja logistyczna 
formula <- heartDisease~.
model1 <- glm(formula , data = train_set, family = binomial)
summary(model1)

#predykcja model1
# Predykcja na zbiorze testowym
predictions_test <- predict(model1, test_set, type = 'response')
# Predykcja na zbiorze treningowym
predictions_train <- predict(model1, train_set, type = 'response')

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_train <- ifelse(predictions_train >= 0.5, 1,0)
predicted_classes_train <- as.factor(predicted_classes_train)

# Zamiana prawdopodobienstwa na 0 i 1 
predicted_classes_test <- ifelse(predictions_test >= 0.5, 1,0)
predicted_classes_test <- as.factor(predicted_classes_test)





explain_model1 <- explain(model = model1, 
                          data  = dane[, -16],
                          y     = dane$heartDisease == 1,
                          type = "classification",
                          label = "Logistic Regression")





cp_model1 <- predict_profile(explainer = explain_model1, 
                             new_observation = obserwacja)
cp_model1

#wybrac zmienne ktore maja znaczenie dla predykcji, numeryczne 
plot(cp_model1, variables = c("Oldpeak", "STSlopeFlat","ChestPainTypeASY","Age")) +
  ggtitle("Ceteris-paribus profile", "") + ylim(0, 0.8)




#wybrac zmienne kategoryczne 
plot(cp_model1, variables = c("Sex", "FastingBS"), 
     variable_type = "categorical", categorical_type = "bars") +
  ggtitle("Ceteris-paribus profile", "") 


#metodaPDP
pdp <- model_profile(explainer = explain_model1, variables = "Age", groups="FastingBS")
plot(pdp, geom = "profiles") + 
  ggtitle("PCP and PDP for age")


pdp <- model_profile(explainer = explain_model1, variables = "Sex", variable_type = "categorical")
plot(pdp)

pdp <- model_profile(explainer = explain_model1, variables = "FastingBS", variable_type = "categorical")
plot(pdp)

# Wartość SHAP

obs=train_set[100,]
obs

predict(model1, obs, type="response")

bd1 <- predict_parts(explainer = explain_model1,
                     new_observation = obs,
                     type = "break_down_interactions", 
                     order = c("Sex", "FastingBS", "Age"))
p1 <- plot(bd1)
bd2 <- predict_parts(explainer = explain_model1,
                     new_observation = obs,
                     type = "break_down_interactions", 
                     order =  c("Sex", "FastingBS", "Age"))
p2 <- plot(bd2)
bd3 <- predict_parts(explainer = explain_model1,
                     new_observation = obs,
                     type = "break_down_interactions", 
                     order =  c( "FastingBS", "Age","Sex"))
p3 <- plot(bd3)
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 2)



shap <- predict_parts(explainer = explain_model1, 
                      new_observation = obs, 
                      type = "shap")
p1 <- plot(shap)
p2 <- plot(shap, show_boxplots = FALSE) 
grid.arrange(p1, p2, nrow = 1)
