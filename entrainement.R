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

ggplot(wines, aes(y = `volatile acidity`, x = `fixed acidity`)) +
  geom_point(alpha = 0.1) +
  facet_wrap(vars(color))

## Ancien graded lab
## Exercice 1
## Question 1 Load the data set using a local file name (preferably using here).

music <-vroom(here("Data","top-spotify-songs-from-20102019-by-year.csv"))

##Question 2 Using distinct (among other functions) compute the number of 
##different songs, artists and musical genre that have been included in the data set.

music |> 
  distinct(artist, `top genre`, title) |>
  summarise(n())

##Question 3 Compute the number of songs per year.

music |>
  summarise(title=n(), .by = year)|>
  knitr::kable()

## Question 4 Find the most popular artist in the data set, i.e. the 
##artist with the largest number of songs in the data set. Make sure to 
##count each song only once

music|>
  distinct(title, .keep_all = TRUE)|>
  group_by(artist)|>
  summarise(title= n()) |>
  arrange(desc(title)) |>
  ungroup()|>
  slice(1)

## Question 5 Compute the minimum, maximum, mean and median bpm as well 
## as the number of songs for each musical genre. Make sure that each song 
## is used only once in the analysis.

music|>
  distinct(title, .keep_all = TRUE) |>
  group_by(`top genre`) |>
  summarise(title=n(),
            min(bpm), 
            max(bpm), 
            mean(bpm)) |>
  ungroup()

## Exercice 2: 
##Question 1 Load the data set using a local file name (preferably using here)
grades <- read.csv(here("C:/Users/marin/Documents/DATA MANAGEMENT M1/Exo-entrainement/Data/grades.csv"))

##Question 2 Compute the minimum, maximum, median and mean grade at the Exam.
grades |>
  summarise(Min=min(Exam,na.rm=TRUE ),
            Max=max(Exam, na.rm=TRUE),
            Mean= mean(Exam, na.rm=TRUE),
            Median=median(Exam, na.rm=TRUE))|>
  knitr:: kable()

##Question 3 Extract the students who missed the Exam.
grades|>
  filter(is.na(Exam))

##Question 4 Compute the number of students in each Group
grades |>
  group_by(Group)|>
  summarise(n())

##Question 5 Compute the number of students who missed the Exam in 
##each Group. Beware that this number can be zero in some groups and 
##this can induce difficulties with group_by. A way to circumvent this 
##problem is to note that the sum of a vector of TRUE/FALSE values is 
##exactly the number of times TRUE values that appear in the vector. 
##For instance

grades|>
  filter(is.na(Exam))|> 
  group_by(Group)|>
  summarise(n())

##Question 6 Create a new data frame built from the grades data set reshapped to a 
##long format. The new data frame should keep the Id and the Group as the orignal 
##variables. The first lines of the data frame should have the following form (the 
## actual values may be different)

grades_2 <-
  grades |>
  pivot_longer(cols = -c(Id, Group)) ##pivot longer tout le tableau sauf les colonnes ID et group.

##Question 7 Using the long format, compute the number of missing grades in total 
##for each student.
grades_2 |>
  filter(is.na(value)) |>
  group_by(Id) |>
  summarise(n())

##Question 8 Using the long format, compute the same information as in question 5.

grades_2|>
  filter(name== "Exam")|>
  summarise(sum(is.na(value)), .by= Group)

##Question 9 Using the long format, compute the number of missing grades for the 
##online tests for each student.
library(stringr)
grades_2|>
  filter(str_detect(name, "Online"))|>
  summarise(Number_of_missing_grades= sum(is.na(value)), .by= Id)|>
  knitr:: kable()

##Question 10 Create a table with two columns: Id to identify students and Missed 
##with value TRUE when the student miss at least one MCQ_xx grade and FALSE when they 
##miss no grade.

## Il fallait utiliser grades 2. 
grades <- read.csv(here("C:/Users/marin/Documents/DATA MANAGEMENT M1/Exo-entrainement/Data/grades.csv"))
grades <-
  grades |>
  mutate(Missing_grades=
           is.na(MCQ_1)|
           is.na(MCQ_2)|
           is.na(MCQ_3)| 
         is.na(MCQ_4)|
         is.na(MCQ_5)|
         is.na(MCQ_6)|
         is.na(MCQ_7)|
         is.na(MCQ_8)|
         is.na(MCQ_9)|
         is.na(MCQ_10))

grades_3 <-
  grades |>
  select(Id, Missing_grades)|>
  arrange()
print(grades_3)

##Question 11 Create a table with two columns: Group to identify groups and P_missed 
##with the percentage of students in each group who missed at least one MCQ_xx grade.


grades <-
  grades|>
  mutate(P_= (sum(Missing_grades==TRUE)/n())*100, .by=Group)
grades_4 <-
  grades |>
  select(Group, P_)
print(grades_4)

##Entrainement tidyr: 

transactions <- data.frame( customer_id = c(1, 1, 2, 2, 3, 3), product = c("A", "B", "A", "C", "B", "C"), amount = c(100, 150, 200, 250, 300, 350) )

##calculer le total amount dépensé par chaque client (customer_id).
transactions |> 
  group_by(customer_id)|>
  summarise(sum(amount))

##transformer le tableau en format large avec une colonne pour chaque produit.

transactions |>
  pivot_wider(names_from = product,
              values_from = amount)

data <- data.frame( year = rep(2019:2021, each = 6), city = rep(c("Paris", "Lyon", "Marseille"), times = 6), product = rep(c("A", "B"), each = 3, times = 3), sales = sample(100:1000, 18, replace = TRUE) )

##calculer les ventes moyennes par city et year.
data |>
  group_by(city,year)|>
  summarise(mean(sales))

##Transforme les données  pour voir les ventes moyennes de chaque produit par 
##année et par ville ne garder que les années et les ventes moyennes.
  
data |>
  group_by(city,year)|>
  mutate(mean_sales=mean(sales))|>
  ungroup()|>
  select(mean_sales,year)|>
  knitr::kable()

set.seed(123)  # Pour reproductibilité
n <- 300  # Nombre de répondants
survey_data <- data.frame(
  respondent_id = rep(1:n, each = 3),  # Identifiant du répondant (1 à 300)
  age = sample(18:60, n, replace = TRUE),  # Âge entre 18 et 60 ans
  gender = sample(c("M", "F"), n, replace = TRUE),  # Sexe M/F
  year = rep(c(2019, 2020, 2021), times = n),  # Années de l'enquête
  q_satisfaction = sample(1:5, n * 3, replace = TRUE),  # Score de satisfaction 1 à 5
  q_frequency = sample(1:5, n * 3, replace = TRUE),  # Fréquence d'utilisation 1 à 5
  q_recommendation = sample(1:5, n * 3, replace = TRUE)  # Probabilité de recommander 1 à 5
)
head(survey_data)

##ne garder que les colonnes respondent_id, year, gender, et 
##les scores (q_satisfaction, q_frequency, q_recommendation).
survey_data <-
  survey_data|>
  select(-(age))

##Convertissez le tableau en format long  de manière à obtenir une colonne 
##question (contenant le nom de chaque question) et une colonne score (valeur associée). 
##Le tableau final doit être organisé pour faciliter une analyse par question, année, 
##et genre.

survey_data2 <-
  survey_data|>
  pivot_longer(cols = c(q_satisfaction, q_frequency, q_recommendation),
               names_to = "Questions")

##calculer la moyenne des scores par question, année, et genre. Cela permettra 
##d’observer les tendances de chaque question en fonction des années et du genre.

survey_data2|>
  group_by(Questions, gender, year)|>
  summarise(mean(value))

##Transformez ensuite le tableau en format large de façon à avoir chaque question 
##(par exemple, q_satisfaction, q_frequency, q_recommendation) en colonne, pour 
##chaque année et chaque genre.

survey_data3 <-
  survey_data2|>
  pivot_wider(names_from = Questions)

