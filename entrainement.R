library(ggplot2)
library(dplyr)
library(vroom)
library(here)
here::i_am("Exo-entrainement.Rproj")
white_wine <-vroom(here("Data","winequality-white.csv"))
## Exercise 3
## Question 1 Transform the quality variable of the wine data sets into a ordered factor, 
## using the fact that grades are integers ranging from 1 to 10.

white_wine <- 
white_wine |>
  mutate(quality= factor(quality, ordered= TRUE, levels=1:10))

## Question 2 Using the geom_boxplot and the fill aesthetics, display the distribution 
## of the volatile acidity conditioned on the quality for the white wines.

ggplot(white_wine, aes(x=`volatile acidity`, fill=quality))+
  geom_boxplot()

##Question 3 Using the same data set combination technique 
##as in exercise 1, use faceting to display
##side by side similar box plots as the one used 
##in the previous section for both wine “colors”.

red_wine <-vroom(here("Data","winequality-red.csv"))
white_wine <- white_wine |> mutate(color= "White")
red_wine <- red_wine |> mutate(color= "Red")
wines <- rbind(white_wine, red_wine)
ggplot(wines, aes(y=`volatile acidity`, fill=quality))+
  geom_boxplot()+
  facet_wrap(vars(color))

##Exercise 2:
credit <-vroom(here("Data","default-of-credit-card-clients.csv"))
##Question 1 Using the documentation of the data set, convert the gender/sex, 
##education, marriage and default variables to factors. 
##It is highly recommended to use the labels parameter of the factor function 
##to specify meaningful levels (for instance female/male rather than 1/2 for sex/gender).

credit <- credit |> mutate(SEX= (case_when( SEX == 1 ~ "Male",
                                           SEX == 2 ~"Female")) |> as.factor())
credit <- credit |> mutate(EDUCATION= case_when( EDUCATION == 1 ~ "graduate school",
                                                 EDUCATION == 2 ~"university",
                                                 EDUCATION == 3 ~"high school",
                                                 EDUCATION == 4 ~"others") |> as.factor())
credit <- credit |> mutate(MARRIAGE= case_when( MARRIAGE == 1 ~ "married",
                                                MARRIAGE == 2 ~"single",
                                                MARRIAGE == 3 ~"others") |> as.factor())
credit <- credit |> mutate(`default payment next month`= 
                             case_when( `default payment next month` == 1 ~ "Yes",
                                        `default payment next month` == 0 ~"No") |> as.factor())
##Question 2 Display the distribution of the education variable.
library(tidyr)
credit|>
  drop_na(EDUCATION) |>
ggplot(aes(x=EDUCATION))+
  geom_bar()

##Question 3 Display the dependency between the education variable and the default 
## variable. More precisely, show the distribution of the default variable, conditioned 
##over the education variable.
credit|>
  drop_na(EDUCATION) |>
  ggplot(aes(x=EDUCATION, fill= `default payment next month`))+
  geom_bar(position= "fill")


##Question 4 Show the distribution of the education level conditioned over both the 
##marital status and the sex. To do so, it is recommended to combine multiple bar plots
##using faceting.
credit|>
  drop_na(EDUCATION, MARRIAGE) |>
  ggplot(aes(x=MARRIAGE, fill= SEX))+
  geom_bar(position= "fill")+
facet_wrap(vars(EDUCATION))

## Back to Exercise 3: 
##Question 1 Use a violin plot (with geom_violin) to compare the authorized credit 
##of clients (variable LIMIT_BAL) between education levels.
credit|>
  drop_na(EDUCATION) |>
  ggplot(aes(x=LIMIT_BAL, y=EDUCATION, fill=EDUCATION))+
  geom_violin()

##Question 2 Use faceting to integrate the marital status in the comparison.
credit|>
  drop_na(EDUCATION, MARRIAGE) |>
  ggplot(aes(x=LIMIT_BAL, y=EDUCATION))+
  geom_violin(color="plum")+
  facet_wrap(vars(MARRIAGE))

##Question 3 Using an adapted graphical representation, compare the age distribution 
##between the sexes.

ggplot(credit, aes(x = SEX, y = AGE, fill = SEX)) +
  geom_boxplot() 

##Exercise 1
##Question 1 Draw a scatter plot of the alcohol content of the red wines as 
##a function of their pH, using transparency to limit the effects of 
##superimposition.
ggplot(red_wine, aes(x=alcohol, y=pH))+
  geom_point(alpha=0.2)

##Question 2 Draw a graphical representation of the distribution of the 
##sulphates in white wines.
ggplot(white_wine, aes(x=sulphates))+
  geom_density(color= "plum")

##Question 3 Use a simple data transformation to combine the two data sets 
##and use the resulting data frame to show on the same figure a scatter plot 
##of the citric acid content as a function of the residual sugar.

ggplot(wines, aes(x=`citric acid`, y=`residual sugar`, color=color))+
  geom_point(alpha=0.1)

##Question 4 Using the same data transformation, display side by side the 
##scatter plots of the volatile acidity as a function of the fixed acidity 
##for red wines and white wines (one scatter plot per wine “color”).

ggplot(wines, aes(y=`volatile acidity`, x=`fixed acidity`))+
  geom_point(alpha=0.1)+
  facet_wrap(vars(color))
