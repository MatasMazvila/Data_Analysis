data <- read.csv("C:/Users/DELL i5/OneDrive/Documents/Age_wage_gap.csv")

#Total age wage gap--------------------

#Creating subsets for total age wage gap for young and senior workers
data_young <- subset(data, Sex == "Total" & AGE == "YOUTH" & Series == "Mean")
data_senior <- subset(data, Sex == "Total" & AGE == "SENIOR" & Series == "Mean")

#Estimating the mean age wage gap for each country throughout the years
mean_Young <- aggregate(Value ~ Country, data = data_young, FUN = mean)
mean_Senior <- aggregate(Value ~ Country, data = data_senior, FUN = mean)

#Getting 5 countries with lowest and highest wage gaps
lowest_y <- mean_Young[order(mean_Young$Value), ][1:5, c("Country", "Value")]
highest_y <- mean_Young[order(mean_Young$Value, decreasing = TRUE), ][1:5, c("Country", "Value")]
lowest_s <- mean_Senior[order(mean_Senior$Value), ][1:5, c("Country", "Value")]
highest_s <- mean_Senior[order(mean_Senior$Value, decreasing = TRUE), ][1:5, c("Country", "Value")]

#Plotting the 5 countries with lowest wage gaps (young)
Sweden <- subset(data_young, Country == "Sweden")
Lithuania <- subset(data_young, Country == "Lithuania")
Latvia <- subset(data_young, Country == "Latvia")
Estonia <- subset(data_young, Country == "Estonia")
Italy <- subset(data_young, Country == "Italy")
young_low <- rbind(Sweden, Latvia, Lithuania, Estonia, Italy)

library(ggplot2)

ggplot(young_low, aes(x = young_low[,9], y = young_low[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with lowest young-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with highest wage gaps (young)
Netherlands <- subset(data_young, Country == "Netherlands")
US <- subset(data_young, Country == "United States")
Chile <- subset(data_young, Country == "Chile")
Greece <- subset(data_young, Country == "Greece")
Ireland <- subset(data_young, Country == "Ireland")
young_high<- rbind(Netherlands, US, Chile, Greece, Ireland)

ggplot(young_high, aes(x = young_high[,9], y = young_high[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with highest young-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with lowest wage gaps (senior)
Turkey <- subset(data_senior, Country == "Turkey")
Greece <- subset(data_senior, Country == "Greece")
Luxembourg <- subset(data_senior, Country == "Luxembourg")
Belgium <- subset(data_senior, Country == "Belgium")
Slovenia <- subset(data_senior, Country == "Slovenia")
senior_low <- rbind(Turkey, Greece, Luxembourg, Belgium, Slovenia)


ggplot(senior_low, aes(x = senior_low[,9], y = senior_low[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with lowest senior-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with highest wage gaps (senior)
Latvia <- subset(data_senior, Country == "Latvia")
Lithuania <- subset(data_senior, Country == "Lithuania")
Estonia <- subset(data_senior, Country == "Estonia")
Sweden <- subset(data_senior, Country == "Sweden")
Italy <- subset(data_senior, Country == "Italy")
senior_high <- rbind(Latvia, Lithuania, Estonia, Sweden, Italy)


ggplot(senior_high, aes(x = senior_high[,9], y = senior_high[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with highest senior-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))


#Women age wage gap-------------------

#Creating subsets for women age wage gap for young and senior workers
data_women_y <- subset(data, Sex == "Women" & AGE == "YOUTH" & Series == "Mean")
data_women_s <- subset(data, Sex == "Women" & AGE == "SENIOR" & Series == "Mean")

#Estimating the mean age wage gap for each country throughout the years
mean_Young_w <- aggregate(Value ~ Country, data = data_women_y, FUN = mean)
mean_Senior_w <- aggregate(Value ~ Country, data = data_women_s, FUN = mean)

#Getting 5 countries with lowest and highest wage gaps
lowest_y_w <- mean_Young_w[order(mean_Young_w$Value), ][1:5, c("Country", "Value")]
highest_y_w <- mean_Young_w[order(mean_Young_w$Value, decreasing = TRUE), ][1:5, c("Country", "Value")]
lowest_s_w <- mean_Senior_w[order(mean_Senior_w$Value), ][1:5, c("Country", "Value")]
highest_s_w <- mean_Senior_w[order(mean_Senior_w$Value, decreasing = TRUE), ][1:5, c("Country", "Value")]

#Plotting the 5 countries with lowest women wage gaps (young)
CR_w <- subset(data_women_y, Country == "Czech Republic")
Lithuania_w <- subset(data_women_y, Country == "Lithuania")
Latvia_w <- subset(data_women_y, Country == "Latvia")
Estonia_w <- subset(data_women_y, Country == "Estonia")
Korea_w <- subset(data_women_y, Country == "Korea")
young_low_w <- rbind(CR_w, Latvia_w, Lithuania_w, Estonia_w, Korea_w)


ggplot(young_low_w, aes(x = young_low_w[,9], y = young_low_w[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with lowest women young-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with highest women wage gaps (young)
Netherlands_w <- subset(data_women_y, Country == "Netherlands")
US_w <- subset(data_women_y, Country == "United States")
Chile_w <- subset(data_women_y, Country == "Chile")
Colombia_w <- subset(data_women_y, Country == "Colombia")
Canada_w <- subset(data_women_y, Country == "Ireland")
young_high_w<- rbind(Netherlands_w, US_w, Chile_w, Colombia_w, Canada_w)

ggplot(young_high_w, aes(x = young_high_w[,9], y = young_high_w[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with highest women young-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with lowest wage gaps (senior)
Turkey_w <- subset(data_women_s, Country == "Turkey")
Greece_w <- subset(data_women_s, Country == "Greece")
Costa_w <- subset(data_women_s, Country == "Costa Rica")
Colombia_w <- subset(data_women_s, Country == "Colombia")
Slovenia_w <- subset(data_women_s, Country == "Slovenia")
senior_low_w <- rbind(Turkey_w, Greece_w, Costa_w, Colombia_w, Slovenia_w)


ggplot(senior_low_w , aes(x = senior_low_w [,9], y = senior_low_w [,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with lowest women senior-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with highest women wage gaps (senior)
Latvia_w <- subset(data_women_s, Country == "Latvia")
Korea_w <- subset(data_women_s, Country == "Korea")
Estonia_w <- subset(data_women_s, Country == "Estonia")
UK_w <- subset(data_women_s, Country == "United Kingdom")
Japan_w <- subset(data_women_s, Country == "Japan")
senior_high_w <- rbind(Latvia_w, Korea_w, Estonia_w, UK_w, Japan_w)


ggplot(senior_high_w, aes(x = senior_high_w[,9], y = senior_high_w[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with highest women senior-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))



#Men age wage gap----------------

#Creating subsets for women age wage gap for young and senior workers
data_men_y <- subset(data, Sex == "Men" & AGE == "YOUTH" & Series == "Mean")
data_men_s <- subset(data, Sex == "Men" & AGE == "SENIOR" & Series == "Mean")

#Estimating the mean age wage gap for each country throughout the years
mean_Young_m <- aggregate(Value ~ Country, data = data_men_y, FUN = mean)
mean_Senior_m <- aggregate(Value ~ Country, data = data_men_s, FUN = mean)

#Getting 5 countries with lowest and highest wage gaps
lowest_y_m <- mean_Young_m[order(mean_Young_m$Value), ][1:5, c("Country", "Value")]
highest_y_m <- mean_Young_m[order(mean_Young_m$Value, decreasing = TRUE), ][1:5, c("Country", "Value")]
lowest_s_m <- mean_Senior_m[order(mean_Senior_w$Value), ][1:5, c("Country", "Value")]
highest_s_m <- mean_Senior_m[order(mean_Senior_w$Value, decreasing = TRUE), ][1:5, c("Country", "Value")]

#Plotting the 5 countries with lowest men wage gaps (young)
Sweden_m <- subset(data_men_y, Country == "Sweden")
Lithuania_m <- subset(data_men_y, Country == "Lithuania")
Latvia_m <- subset(data_men_y, Country == "Latvia")
Estonia_m <- subset(data_men_y, Country == "Estonia")
Italy_m <- subset(data_men_y, Country == "Italy")
young_low_m <- rbind(Sweden_m, Latvia_m, Lithuania_m, Estonia_m, Italy_m)


ggplot(young_low_m, aes(x = young_low_m[,9], y =young_low_m[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with lowest men young-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with highest men wage gaps (young)
Netherlands_m <- subset(data_men_y, Country == "Netherlands")
US_m <- subset(data_men_y, Country == "United States")
Chile_m <- subset(data_men_y, Country == "Chile")
Greece_m <- subset(data_men_y, Country == "Greece")
Korea_m <- subset(data_men_y, Country == "Korea")
young_high_m <- rbind(Netherlands_m, US_m, Chile_m, Greece_m, Korea_m)

ggplot(young_high_m, aes(x = young_high_m [,9], y = young_high_m[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with highest men young-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with lowest men wage gaps (senior)
Spain_m <- subset(data_men_s, Country == "Spain")
Greece_m <- subset(data_men_s, Country == "Greece")
UK_m <- subset(data_men_s, Country == "United Kingdom")
Costa_m <- subset(data_men_s, Country == "Costa Rica")
Colombia_m <- subset(data_men_s, Country == "Colombia")
senior_low_m<- rbind(Spain_m , Greece_m, UK_m, Costa_m, Colombia_m)


ggplot(senior_low_m , aes(x = senior_low_m[,9], y = senior_low_m[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with lowest men senior-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))

#Plotting the 5 countries with highest men wage gaps (senior)
Latvia_m <- subset(data_men_s, Country == "Latvia")
Lithuania_m <- subset(data_men_s, Country == "Lithuania")
Estonia_m <- subset(data_men_s, Country == "Estonia")
US_m <- subset(data_men_s, Country == "United States")
Korea_m <- subset(data_men_s, Country == "Korea")
senior_high_m <- rbind(Latvia_m, Korea_m, Estonia_m, US_m, Lithuania_m)


ggplot(senior_high_m, aes(x = senior_high_m[,9], y = senior_high_m[,11], color = Country)) +
  geom_line(lwd = 0.7) + xlab("Date") + ylab("Difference in means") + 
  ggtitle("5 countries with highest men senior-prime age wage gap") +
  theme(plot.title = element_text(hjust = 0.5))



#Comparison of age wage gap------------------

#Estimating the overall means of age wage gaps
a <- mean(mean_Young[,2])
a1 <- mean(mean_Senior[,2])
b <- mean(mean_Young_w[,2])
b1 <- mean(mean_Senior_w[,2])
c <- mean(mean_Young_m[,2])
c1 <- mean(mean_Senior_m[,2])

Table <- data.frame(a, a1, b, b1, c, c1)

names(Table) <- c("Total young-prime age", "Total senior-prime age",
                  "Women young-prime age", "Women senior-prime age",
                  "Men young-prime age", "Men senior-prime age")

rownames(Table) <- "Mean of wage gap"

#Creating a table for 5 countries with lowest and highest total age wage gap
Table1 <- data.frame(lowest_y, highest_y, lowest_s, highest_s)

names(Table1) <- c("Countries with lowest total young-prime age wage gap", 
                   "Lowest total young-prime age wage gap",
                   "Countries with highest total young-prime age wage gap", 
                   "Highest total young-prime age wage gap",
                   "Countries with lowest total senior-prime age wage gap", 
                   "Lowest total senior-prime age wage gap",
                   "Countries with highest total senior-prime age wage gap", 
                   "Highest total senior-prime age wage gap")

#Creating a table for 5 countries with lowest and highest women age wage gap
Table2 <- data.frame(lowest_y_w, highest_y_w, lowest_s_w, highest_s_w)

names(Table2) <- c("Countries with lowest young-prime age women wage gap", 
                   "Lowest young-prime age women wage gap",
                   "Countries with highest young-prime age women wage gap", 
                   "Highest young-prime age women wage gap",
                   "Countries with lowest senior-prime age women wage gap", 
                   "Lowest senior-prime age women wage gap",
                   "Countries with highest senior-prime age women wage gap", 
                   "Highest senior-prime age women wage gap")

#Creating a table for 5 countries with lowest and highest men age wage gap
Table3 <- data.frame(lowest_y_m, highest_y_m, lowest_s_m, highest_s_m)

names(Table3) <- c("Countries with lowest young-prime age men wage gap", 
                   "Lowest young-prime age men wage gap",
                   "Countries with highest young-prime age men wage gap", 
                   "Highest young-prime age men wage gap",
                   "Countries with lowest senior-prime age men wage gap", 
                   "Lowest senior-prime age men wage gap",
                   "Countries with highest senior-prime age men wage gap", 
                   "Highest senior-prime age men wage gap")


#Social expenditure------------


data1 <- read.csv("C:/Users/kjonu/Downloads/Social_expenditure.csv")

#Spending on employment incentives YOUNG--------------------

#Employment incentives percentage of government spending (lowest total young wage gap)
LTU <- subset(data1, Country == "Lithuania" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
LV <- subset(data1, Country == "Latvia" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
EST <- subset(data1, Country == "Estonia" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
SWE <- subset(data1, Country == "Sweden" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
ITA <- subset(data1, Country == "Italy" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")

#Employment incentives percentage of government spending (highest total young wage gap)
NLD <- subset(data1, Country == "Netherlands" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
USA <- subset(data1, Country == "United States" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
CHL <- subset(data1, Country == "Chile" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
GRE <- subset(data1, Country == "Greece" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
IRL <- subset(data1, Country == "Ireland" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")

#Estimating the mean spending on employment incentives for each country throughout the years
Totale <- subset(data1, Branch == "Active labour market programmes"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "604")
mean_Totale <- aggregate(Value ~ Country, data = Totale, FUN = mean)
mean_Totale <- subset(mean_Totale, Country != "OECD - Total")

#Producing a table for all time means of government spending in employment
#incentives
T1 <- rbind(mean(LV[2:5, 21]), 
            mean(LTU[2:5, 21]),
            mean(EST[2:5, 21]),
            mean(SWE$Value),
            mean(ITA$Value),
            mean(NLD$Value), 
            mean(USA[2:8, 21]),
            mean(CHL$Value),
            mean(GRE$Value),
            mean(IRL$Value))

#Producing a table for all time means of government spending in employment
#incentives
EI <- data.frame(mean(T1[1:5, 1]), mean(T1[6:10, 1]), mean(mean_Totale$Value))

names(EI) <- c("Employment incentives in countries with lowest total wage gap",
               "Employment incentives in countries with highest total wage gap",
               "Employment incentives in all countries")

rownames(EI) <- "Mean of percentage of goverment spending"

#Spending on employment incentives SENIOR--------------------

#Employment incentives percentage of government spending (lowest total senior wage gap)
LUX <- subset(data1, Country == "Luxembourg" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
BEL <- subset(data1, Country == "Belgium" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
SVN <- subset(data1, Country == "Slovenia" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")

#Employment incentives percentage of government spending (highest total senior wage gap)
KOR <- subset(data1, Country == "Korea" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")
SVK <- subset(data1, Country == "Slovak Republic" & Branch == "Active labour market programmes" & TYPROG == "604" & UNIT == "PCT_GOV" & SOURCE == "10")

#Estimating the mean spending on employment incentives for each country throughout the years
Totale <- subset(data1, Branch == "Active labour market programmes"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "604")
mean_Totale <- aggregate(Value ~ Country, data = Totale, FUN = mean)
mean_Totale <- subset(mean_Totale, Country != "OECD - Total")

#Producing a table for all time means of government spending in employment
#incentives SENIOR
T11 <- rbind(mean(ITA$Value, na.rm = TRUE), 
             mean(GRE$Value, na.rm = TRUE),
             mean(LUX$Value, na.rm = TRUE),
             mean(BEL$Value, na.rm = TRUE),
             mean(SVN$Value, na.rm = TRUE),
             mean(EST$Value, na.rm = TRUE), 
             mean(LV$Value, na.rm = TRUE),
             mean(KOR$Value, na.rm = TRUE),
             mean(SVK$Value, na.rm = TRUE),
             mean(LTU$Value, na.rm = TRUE))

#Producing a table for all time means of government spending in employment
#incentives
EI1 <- data.frame(mean(T11[1:5, 1], na.rm = TRUE), mean(T11[6:10, 1]), mean(mean_Totale$Value))

names(EI1) <- c("Employment incentives in countries with lowest senior total wage gap",
                "Employment incentives in countries with highest senior total wage gap",
                "Employment incentives in all countries")

rownames(EI1) <- "Mean of percentage of goverment spending"



#Spending on training YOUNG------------------

#Training percentage of government spending (lowest total young wage gap)
LTU_t <- subset(data1, Country == "Lithuania" & Branch == "Active labour market programmes"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "602")
LV_t <- subset(data1, Country == "Latvia" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
EST_t <- subset(data1, Country == "Estonia" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
SWE_t <- subset(data1, Country == "Sweden" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
ITA_t <- subset(data1, Country == "Italy" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")

#Training percentage of government spending (highest total young wage gap)
NLD_t <- subset(data1, Country == "Netherlands" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
USA_t <- subset(data1, Country == "United States" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
CHL_t <- subset(data1, Country == "Chile" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
GRE_t <- subset(data1, Country == "Greece" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
IRL_t <- subset(data1, Country == "Ireland" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")

#Estimating the mean spending on training for each country throughout the years
Total <- subset(data1, Branch == "Active labour market programmes"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "602")
mean_Total <- aggregate(Value ~ Country, data = Total, FUN = mean)
mean_Total <- subset(mean_Total, Country != "OECD - Total")

#Producing a table for all time means of government spending on training
T2 <- rbind(mean(LV_t[2:5, 21]), 
            mean(LTU_t[2:5, 21]),
            mean(EST_t[2:5, 21]),
            mean(SWE_t$Value),
            mean(ITA_t$Value),
            mean(NLD_t$Value), 
            mean(USA_t[2:8, 21]),
            mean(CHL_t$Value),
            mean(GRE_t$Value),
            mean(IRL_t$Value))


#Producing a table for all time means of government spending on training
Training <- data.frame(mean(T2[1:5, 1]), mean(T2[6:10, 1]), mean(mean_Total$Value))

names(Training) <- c("Training in countries with lowest total wage gap",
                     "Training in countries with highest total wage gap", 
                     "Training in all countries")

rownames(Training) <- "Mean of percentage of goverment spending"


#Spending on training SENIOR------------------

#Training percentage of government spending (lowest total senior wage gap)
LUX_t <- subset(data1, Country == "Luxembourg" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
BEL_t <- subset(data1, Country == "Belgium" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
SVN_t <- subset(data1, Country == "Slovenia" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")

#Training percentage of government spending (highest total senior wage gap)
KOR_t <- subset(data1, Country == "Korea" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")
SVK_t <- subset(data1, Country == "Slovak Republic" & Branch == "Active labour market programmes" & TYPROG == "602" & UNIT == "PCT_GOV" & SOURCE == "10")

#Estimating the mean spending on training for each country throughout the years
Total <- subset(data1, Branch == "Active labour market programmes"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "602")
mean_Total <- aggregate(Value ~ Country, data = Total, FUN = mean)
mean_Total <- subset(mean_Total, Country != "OECD - Total")

#Producing a table for all time means of government spending on training
T22 <- rbind(mean(ITA_t$Value, na.rm=TRUE), 
             mean(GRE_t$Value, na.rm=TRUE),
             mean(LUX_t$Value, na.rm=TRUE),
             mean(BEL_t$Value, na.rm=TRUE),
             mean(SVN_t$Value, na.rm=TRUE),
             mean(EST_t$Value, na.rm=TRUE), 
             mean(LV_t$Value, na.rm=TRUE),
             mean(KOR_t$Value, na.rm=TRUE),
             mean(SVK_t$Value, na.rm=TRUE),
             mean(LTU_t$Value, na.rm=TRUE))


#Producing a table for all time means of government spending on training
Training2 <- data.frame(mean(T22[1:5, 1], na.rm=TRUE), mean(T22[6:10, 1]), mean(mean_Total$Value))

names(Training2) <- c("Training in countries with lowest total senior wage gap",
                      "Training in countries with highest total senior wage gap", 
                      "Training in all countries")

rownames(Training2) <- "Mean of percentage of goverment spending"



#Spending on unemployment compensation YOUNG------------

#Unemployment compensation percentage of government spending (lowest total young wage gap)
LTU_c <- subset(data1, Country == "Lithuania" & Branch == "Unemployment"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "711")
LV_c <- subset(data1, Country == "Latvia" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
EST_c <- subset(data1, Country == "Estonia" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
SWE_c <- subset(data1, Country == "Sweden" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
ITA_c <- subset(data1, Country == "Italy" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")

#Unemployment compensation percentage of government spending (highest total young wage gap)
NLD_c <- subset(data1, Country == "Netherlands" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
USA_c <- subset(data1, Country == "United States" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
CHL_c <- subset(data1, Country == "Chile" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
GRE_c <- subset(data1, Country == "Greece" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
IRL_c <- subset(data1, Country == "Ireland" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")

#Estimating the mean spending on unemployment compensation for each country throughout the years
Totalc <- subset(data1, Branch == "Unemployment"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "711")
mean_Totalc <- aggregate(Value ~ Country, data = Totalc, FUN = mean)
mean_Totalc <- subset(mean_Totalc, Country != "OECD - Total")

#Producing a table for all time means of government spending in unemployment compensation
T3 <- rbind(mean(LV_c$Value, na.rm = TRUE), 
            mean(LTU_c$Value, na.rm = TRUE),
            mean(EST_c$Value, na.rm = TRUE),
            mean(SWE_c$Value, na.rm = TRUE),
            mean(ITA_c$Value, na.rm = TRUE),
            mean(NLD_c$Value, na.rm = TRUE), 
            mean(USA_c$Value, na.rm = TRUE),
            mean(CHL_c$Value, na.rm = TRUE),
            mean(GRE_c$Value, na.rm = TRUE),
            mean(IRL_c$Value, na.rm = TRUE))


#Producing a table for all time means of government spending in employment
#incentives
UC <- data.frame(mean(T3[1:5, 1]), mean(T3[6:10, 1]), mean(mean_Totalc$Value))

names(UC) <- c("Unemployment compensation in countries with lowest total wage gap",
               "Unemployment compensation with highest total wage gap", 
               "Unemployment compensation in all countries")

rownames(UC) <- "Mean of percentage of goverment spending"

#Spending on unemployment compensation SENIOR------------

#Unemployment compensation percentage of government spending (lowest total senior wage gap)
LUX_c <- subset(data1, Country == "Luxembourg" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
BEL_c <- subset(data1, Country == "Belgium" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
SVN_c <- subset(data1, Country == "Slovenia" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")

#Unemployment compensation percentage of government spending (highest total young wage gap)
KOR_c <- subset(data1, Country == "Korea" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")
SVK_c <- subset(data1, Country == "Slovak Republic" & Branch == "Unemployment" & TYPROG == "711" & UNIT == "PCT_GOV" & SOURCE == "10")

#Estimating the mean spending on unemployment compensation for each country throughout the years
Totalc <- subset(data1, Branch == "Unemployment"  & UNIT == "PCT_GOV" & SOURCE == "10" & TYPROG == "711")
mean_Totalc <- aggregate(Value ~ Country, data = Totalc, FUN = mean)
mean_Totalc <- subset(mean_Totalc, Country != "OECD - Total")

#Producing a table for all time means of government spending in unemployment compensation
T33 <- rbind(mean(ITA_c$Value, na.rm = TRUE), 
             mean(GRE_c$Value, na.rm = TRUE),
             mean(LUX_c$Value, na.rm = TRUE),
             mean(BEL_c$Value, na.rm = TRUE),
             mean(SVN_c$Value, na.rm = TRUE),
             mean(EST_c$Value, na.rm = TRUE), 
             mean(LV_c$Value, na.rm = TRUE),
             mean(KOR_c$Value, na.rm = TRUE),
             mean(SVK_c$Value, na.rm = TRUE),
             mean(LTU_c$Value, na.rm = TRUE))


#Producing a table for all time means of government spending in employment
#incentives
UC3 <- data.frame(mean(T33[1:5, 1], na.rm=TRUE), mean(T33[6:10, 1]), mean(mean_Totalc$Value))

names(UC3) <- c("Unemployment compensation in countries with lowest total wage gap",
                "Unemployment compensation with highest total wage gap", 
                "Unemployment compensation in all countries")

rownames(UC3) <- "Mean of percentage of goverment spending"

#Correlation between spending on employment incentives and age wage gap-----------------

#Correlation between spending on employment incentives and young-prime age wage gap
corTotaley <- subset(mean_Totale, Country != "Israel")
coryounge <- subset(mean_Young, Country != "Iceland" & Country != "Türkiye")
correiy <- data.frame(corTotaley, coryounge)
correiyoung <- cor(correiy[,2], correiy[,4])

#Correlation between spending on employment incentives and senior-prime age wage gap
corTotales <- mean_Totale
corseniore <- subset(mean_Senior, Country != "Iceland" & Country != "Türkiye") 
correis <- data.frame(corTotales, corseniore)
correisenior <- cor(correis[,2], correis[,4])

#5 lowest countries young
y <- data.frame(Table1[,2], T1[1:5,1])
y1 <- cor(y[,1], y[,2])
#Strong correlation for countries with lowest young age wage gap

#5 highest countries young
yy <- data.frame(Table1[,4], T1[6:10,1])
yy1 <- cor(yy[,1], yy[,2])
#Moderate correlation for countries with highest young age wage 

#5 lowest countries senior
x <- data.frame(Table1[,6], T11[1:5,1])
x1 <- cor(x[2:5,1], x[2:5,2])
#Stronger correlation for countries with lowest young age wage gap

#5 highest countries senior
xx <- data.frame(Table1[,8], T11[6:10,1])
xx1 <- cor(xx[,1], xx[,2])
#Moderate negative correlation for countries with highest young age wage 

#Producing a table for correlations between spending on employment incentives 
#and age wage gap
Table_ei <- data.frame(correiyoung, correisenior, y1, yy1, x1, xx1)

names(Table_ei) <- c("Total young-prime age wage gap", 
                     "Total senior-prime age wage gap",
                     "Countries with lowest total young-prime age wage gap", 
                     "Countries with highest total young-prime age wage gap", 
                     "Countries with lowest total senior-prime age wage gap", 
                     "Countries with highest total senior-prime age wage gap")

rownames(Table_ei) <- "Employment incentives"

#Correlation between spending on training and age wage gap-----------------

#Correlation between spending on training and young-prime age wage gap
corTotaly <- subset(mean_Total, Country != "Israel")
coryoung <- mean_Young
corry <- data.frame(corTotaly, coryoung)
corryoung <- cor(corry[,2], corry[,4])

#Correlation between spending on training and senior-prime age wage gap
corTotals <- mean_Total
corsenior <- mean_Senior
corrs <- data.frame(corTotals, corsenior)
corrsenior <- cor(corrs[,2], corrs[,4])

#5 lowest countries young
t <- data.frame(Table1[,2], T2[1:5,1])
t1 <- cor(t[,1], t[,2])
#Strong correlation for countries with lowest young age wage gap

#5 highest countries young
th <- data.frame(Table1[,4], T2[6:10,1])
th1 <- cor(th[,1], th[,2])
#Moderate correlation for countries with highest young age wage 

#5 lowest countries senior
ts <- data.frame(Table1[,6], T22[1:5,1])
ts1 <- cor(ts[2:5,1], ts[2:5,2])
#Stronger correlation for countries with lowest young age wage gap

#5 highest countries senior
thc <- data.frame(Table1[,8], T22[6:10,1])
thc1 <- cor(thc[,1], thc[,2])
#Moderate negative correlation for countries with highest young age wage 

#Producing a table for correlations between spending on employment incentives 
#and age wage gap
Table_t <- data.frame(corryoung, corrsenior, t1, th1, ts1, thc1)

names(Table_t) <- c("Total young-prime age wage gap", 
                    "Total senior-prime age wage gap",
                    "Countries with lowest total young-prime age wage gap", 
                    "Countries with highest total young-prime age wage gap", 
                    "Countries with lowest total senior-prime age wage gap", 
                    "Countries with highest total senior-prime age wage gap")

rownames(Table_t) <- "Training"

#Correlation between spending on unemployment compensation and age wage gap-----------------

#Correlation between spending on unemployment compensation and young-prime age wage gap
corTotalucy <- subset(mean_Totalc, Country != "Israel")
coryounguc <- subset(mean_Young, Country != "Mexico")
corryuc <- data.frame(corTotalucy, coryounguc)
corryounguc <- cor(corryuc[,2], corryuc[,4])

#Correlation between spending on unemployment compensation and senior-prime age wage gap
corTotalucs <- mean_Totalc
corsenioruc <- subset(mean_Senior, Country != "Mexico")
corrsuc <- data.frame(corTotalucs, corsenioruc)
corrsenioruc <- cor(corrsuc[,2], corrsuc[,4])

#5 lowest countries young
luc <- data.frame(Table1[,2], T3[1:5,1])
luc1 <- cor(luc[,1], luc[,2])
#Strong correlation for countries with lowest young age wage gap

#5 highest countries young
huc <- data.frame(Table1[,4], T3[6:10,1])
huc1 <- cor(huc[,1], huc[,2])
#Moderate correlation for countries with highest young age wage 

#5 lowest countries senior
suc <- data.frame(Table1[,6], T33[1:5,1])
suc1 <- cor(suc[2:5,1], suc[2:5,2])
#Stronger correlation for countries with lowest young age wage gap

#5 highest countries senior
shuc <- data.frame(Table1[,8], T33[6:10,1])
shuc1 <- cor(shuc[,1], shuc[,2])
#Moderate negative correlation for countries with highest young age wage 

#Producing a table for correlations between spending on unemployment compensation 
#and age wage gap
Table_c <- data.frame(corryounguc, corrsenioruc, luc1, huc1, suc1, shuc1)

names(Table_c) <- c("Total young-prime age wage gap", 
                    "Total senior-prime age wage gap",
                    "Countries with lowest total young-prime age wage gap", 
                    "Countries with highest total young-prime age wage gap", 
                    "Countries with lowest total senior-prime age wage gap", 
                    "Countries with highest total senior-prime age wage gap")

rownames(Table_c) <- "Unemployment compensation"


#Combing the table of all correlations------------

Final_table <- rbind(Table_ei, Table_t, Table_c)