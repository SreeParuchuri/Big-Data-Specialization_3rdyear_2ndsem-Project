install.packages('readr')
install.packages('dplyr')
install.packages('psych')
install.packages('rscala')
library(readr)
library(dplyr)
library(psych)
library(rscala)
#180030699
df <- read_csv("diamonds.csv")
str(df)

df <- df %>% select(-"x", -"y", -"z") %>% 
  mutate(cut = factor(cut),
         color = factor(color),
         clarity = factor(clarity))

head(df)
s <- scala()
#Exloratory Data Analysis and Visualisation.
#First of all, let's explore the distribution of our dependent variable pric
summary(df$price)
hist(df$price, main = "Distribution of Price variable", col = "darkorange")

#Because the mean is greater than the median the distribution is right-skewed.
#Some features (cut, color, clarity) are now factors. Let's have a look at how they are distributed.


hist(table(df$cut),  main = "Distribution of Cut", col = "lightblue")
hist(table(df$color), main = "Distribution of Color", col = "darkblue")
hist(table(df$clarity), main = "Distribution of Clarity", col = "purple")
#correlation matrix
pairs.panels(df[c("carat", "cut", "color", "clarity", "depth", "table", "price")],
             scale = TRUE,
             hist.col = "grey",
             main = "Correlation Matrix for the Diamonds Dataset")
tr_model <- lm(price ~ carat + cut + color + clarity + depth + table, data = df)
tr_model

summary(tr_model)
df$pred <- predict(tr_model, df)
cor(df$pred, df$price)
plot(df$pred, df$price)
abline(a=0, b=1, col="red", lwd=3, lty=2)

# First let's define the properties
s+'
def propString(i:Int)={
var z = new Array[String](3);
z(0)="Good";z(1)="F";z(2)="SI1";
z(i)
}
'
s+'
def propInt(i:Int)={
var x=new Array[Int](3);
x(0)=1;x(1)=65;x(2)=70;
x(i)
}
'
#s*'propString(1)'
properties <- data.frame(
  carat = s*'propInt(0)',
  cut = s*'propString(0)',
  color = s*'propString(1)',
  clarity = s*'propString(2)',
  depth = s*'propInt(1)',
  table = s*'propInt(2)'
)

# Now we can predit the price

ans<-predict(tr_model, properties)
s(ans)*"println(ans)"
ans
