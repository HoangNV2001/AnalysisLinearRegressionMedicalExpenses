library(ggplot2)
library(superml)
library(tidyverse)
library(reshape2)
library(caret)
library(car)

#Data exploring
a <- read.csv("Data\\insurance.csv")
View(a)
dim(a)
head(a)
length(a)
names(a)
summary(a)

#Scatter plots
plot(a$age, a$charges, main = "Age vs Charges", xlab = "age", ylab = "charges",
     col = alpha('blue',0.5), pch=20, cex =1.6, cex.lab =0.9, cex.main =0.9, cex.axis = 0.9)

plot(a$bmi, a$charges, main = "BMI vs Charges", xlab = "bmi", ylab = "charges",
     col = alpha('dark green',0.5), pch=20, cex =1.6, cex.lab =0.9, cex.main =0.9, cex.axis = 0.9)

plot(a$children, a$charges, main = "Children vs Charges", xlab = "children", ylab = "charges",
     col = alpha('red',0.5), pch=20, cex =1.6, cex.lab =0.9, cex.main =0.9, cex.axis = 0.9)

#Boxplots
boxplot(a$charges ~ a$smoker, xlab = 'smoker', ylab = 'charges', 
        col ='burlywood', main = 'Boxplot grouped by smoker')

boxplot(a$charges ~ a$sex, xlab = 'sex', ylab = 'charges', 
        col ='aquamarine', main = 'Boxplot grouped by sex')

boxplot(a$charges ~ a$region, xlab = 'region', ylab = 'charges', 
        col ='chartreuse', main = 'Boxplot grouped by region')

#Label encoding
label <- LabelEncoder$new()
a$smoker <- label$fit_transform(a$smoker)
a$sex <- label$fit_transform(a$sex)
a$region <- label$fit_transform(a$region)

#Correlation heatmap
cormat <- round(cor(a),2)

cormat[upper.tri(cormat)] <- NA #replace upper triangle of the matrix by NaN
melted_cormat <- melt(cormat, na.rm = TRUE)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.direction = "vertical")

#Model& Hypothesis tests
df <- read.csv("Data\\insurance.csv")

r1 <- lm(charges ~ age + bmi + smoker, data = df)
summary(r1)

r2 <- lm(charges ~ age + bmi + children + region + sex + smoker, data = df)
summary(r2)

r3 <- lm(charges ~ age + bmi + children + region + smoker, data = df)
summary(r3)

View(anova(r1, r3))
View(anova(r2, r3))

standard_res <- rstandard(r3)
sum(standard_res < -2 | standard_res >2)/ length(standard_res)
sum(standard_res < -3 | standard_res >3)/ length(standard_res)

View(vif(r3))

plot(r3)
