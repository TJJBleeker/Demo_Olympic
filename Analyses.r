### ## ## ## ## ## ## ###
### libraries inladen ###
### ## ## ## ## ## ## ###
libs <- c("ggplot2", "plotly", "tidyverse", "data.table", "gridExtra", "knitr", "stringi")
install.packages(libs)
lapply(libs, require, character.only = TRUE)

### ### ## ### ### ## ### ### 
### ## Dataverzameling ## ### 
### ### ## ### ### ## ### ### 
setwd("G:/Mijn Drive/Stage periode")
Data  <- read.csv("athlete_events.csv", 
                  sep = ",", 
                  header = T)
Regions <- read.csv2("noc_regions.csv",  sep = ",", header = T)

### ### ## ### ### ## ### ### 
### ## Data preparatie ## ###
### ### ## ### ### ## ### ###

str(Data)
## DATA STRUCTUREN
## Data types veranderen
Ch <- c("ID", "Name", "Team", "NOC", "Games", "City", "Sport", "Event")
Int<- c("Age", "Year")
Db <- c("Height", "Weight")
Fac<- c("Sex", "Season", "Medal")

cols_ch = c(grep(paste(Ch, collapse = "|"), names(Data), value=TRUE))
cols_int = c(grep(paste(Int, collapse = "|"), names(Data), value=TRUE))
cols_db = c(grep(paste(Db, collapse = "|"), names(Data), value=TRUE))

Data[,cols_ch] = apply(Data[,cols_ch], 2, function(x) as.character(x))
Data[,cols_int] = apply(Data[,cols_int], 2, function(x) as.numeric(as.character(x)))
Data[,cols_db] = apply(Data[,cols_db], 2, function(x) as.double(as.character(x)))

Data$Sex <- factor(Data$Sex, levels = c("M", "F"))
Data$Season <- factor(Data$Season, levels = c("Summer", "Winter"))
Data$Medal <- factor(Data$Medal, levels = c("Bronze", "Silver", "Gold"))

# Data[is.na(Data)] <- 0
# levels(Data$Medal)[levels(Data$Medal)=="NA's"] <- 0
summary(Data)
# Controle
str(Data)
pairs(Data)
plot(Sex ~ Medal + Age, data = Data)

### ### ## ### ### ## ### ### 
### ## Data exploratie ## ###
### ### ## ### ### ## ### ### 
## MBV beschrijvende statistieken
str(Data)      #laat de structuur van de data zien.
summary(Data)  #geeft een fivenum samenvatting van de data.
sum(Data$Games)      #som van een variabele.
length(Data)   #geeft het aantal observaties.
mean(Data$Age)     #gemiddelde.
## = sum(Data)/length(.)
median(Data)   #de middelste waarneming (robuust, niet gevoelig voor extra extreme waarden).
mode(Data)     #modus, de waarde die het meest voorkomt (library(DescTools)).
sd(Data)       #standaard deviatie, maat om te meten hoe dichtbij de observaties uit de dataset liggen bij het gemiddelde.
sd(Data)/sqrt(length(Data)) #standaard gemiddelde fout.
## describe(Data) #se indiceert de standaard gemiddelde fout (library(psych)).
summary(Data)  #geeft een fivenum samenvatting van de data
sort(Data)     #sorteren variabelen.
quantile(Data, 0.25, 1) #kwantielen, het percentage kan worden gekozen. Type heeft 2 opties: 1(middelt waarden niet), 2 middelt waarden wel.
glimpse(Data)  #een inzichtelijke blik werpen op de data (library(dplyr))
head(Data)     #eerste 5 variabelen bekijken
tail(Data) #laatste 5 variabelen bekijken
fivenum(Data)#Tukey min, 25%, mediaan, 75% en max
sapply(Data, functie, na.rm = True)

## plyr laden voor dplyr ivm ddply functie 
library(plyr)
library(dplyr)

## Distributions 
table(Data$Sex)
table(Data$Age)
table(Data$Medal)
table(Data$Sex, Data$Medal)

prop.table(table(Data$Age))

### ### ## ### ### ## ### ### 
### ## Data splitsing  ## ###
### ### ## ### ### ## ### ### 

smp_size <- floor(0.70 * nrow(Data))
set.seed(123) # om reproduceerbaar te maken
train_ind <- sample(seq_len(nrow(Data)), size = smp_size)
train <- Data[train_ind,]
test <- Data[-train_ind,]

### ### ## ### ## ## ### ## ### ### 
### ## exploratie specifiek  ## ###
### ### ## ### ## ## ### ## ### ### 
# TRENDS
# SEX 
groupMale <- Data %>%
  filter(Sex == "M") %>%
  group_by(Year, Season) %>%
  dplyr::summarise(Number_Of_Men = n())

groupFemale <- Data %>%
  filter(Sex == "F") %>%
  group_by(Year, Season) %>%
  dplyr::summarise(Number_Of_Women = n())

group <- groupMale %>%
  left_join(groupFemale) %>%
  mutate(Sex_Ratio = Number_Of_Men/Number_Of_Women)


Theme1 <-   theme(plot.title = element_text(hjust = 0.5),
                  panel.background = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line.x = element_line(colour="grey80"), 
                  axis.line.y = element_line(colour="grey80"),
                  strip.text.x =element_text(color = "#662506", size = 15, face = "bold"),
                  strip.background = element_blank(),
                  title = element_text(color = "#662506",size = 15, face = "bold"),
                  axis.title.x = element_text(color = "#662506",size = 15, face = "bold"),
                  axis.title.y = element_text(color = "#662506",size = 15, face = "bold"),
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15))
## TREND of sex ratio
pl1 <- group %>%
  ggplot(aes(x = Year, y= Sex_Ratio, group = Season)) +
  geom_line(aes(color = Season)) +
  geom_point(aes(color = Season)) +
  labs(x = "Year", y = "Sex Ratio", title = "Sex Ratio in Olympics through the years") +
  Theme1

pl2 <- group %>%
  filter(Year>1927) %>%
  ggplot(aes(x = Year, y= Sex_Ratio, group = Season)) +
  geom_line(aes(color = Season)) +
  geom_point(aes(color = Season)) +
  labs(x = "Year", y = "Sex Ratio", title = "Sex Ratio in Olympics through the years after 1927") +
  Theme1

PLx1 <- cowplot::plot_grid(pl1,pl2, ncol = 1, 
                           align = 'h', axis = 'l')
ggsave("Man_vrouw verhouding.png", PLx1, width = 18, height = 10, dpi = 300)

## TRENDS 
## AGE 
Data$Age[is.na(Data$Age)] <- median(Data$Age, na.rm = T)
cat("The median age of the athletes in the modern olympics is", median(Data$Age)) 
cat("The median age of the male athletes in the modern olympics is", median(Data$Age[Data$Sex == "M"]))
cat("The median age of the female athletes in the modern olympics is", median(Data$Age[Data$Sex == "F"]))

# Filling the missing ages with median values.

pl3 <- Data %>%
  ggplot(aes(x = Age)) +
  geom_density(color = "black", fill = "tomato") +
  labs(x = "Age", title = "Distribution of Age") +
  Theme1

pl4 <- Data %>%
  ggplot(aes(x=Age, fill=Sex)) +
  geom_density(alpha=0.4) +
  labs(x = "Age", title = "Distribution of Age by Sex") +
  Theme1

PLx2 <- cowplot::plot_grid(pl3,pl4, ncol = 1, 
                           align = 'h', axis = 'l')

ggsave("Verdeling leeftijd.png", PLx2, width = 18, height = 10, dpi = 300)

## Ontwikkeling atleten, per land per event

Atleet <- 
  Data %>% 
  filter(Sport != "Art Competitions") %>%
  group_by(Year, Season) %>%
  dplyr::summarise(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )
summary(Atleet)
str(Atleet)
Atleet$Year = as.numeric(as.character(Atleet$Year))
#Plot
p1 <- ggplot(Atleet, aes(x=Year, y=Athletes, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line()+
  geom_smooth(linetype="dashed",
              color="darkred", fill="lightblue") +
  scale_color_manual(values=c("darkorange","darkblue")) +
  labs(x = "Year", y = "Athletes", title = "Number of athletes participating") +  
  annotate("text", x=c(1932,1956,1976,1980),
           y=c(2000,2750,6800,4700),
           label=c("L.A. 1932","Melbourne 1956","Montreal 1976","Moscow 1980"),
           size=3) +
  annotate("text",x=c(1916,1942),y=c(10000,10000),
           label=c("WWI","WWII"), size=4, color="red") +
  geom_segment(mapping=aes(x=1914,y=8000,xend=1918,yend=8000),color="red", size=2) +
  geom_segment(mapping=aes(x=1939,y=8000,xend=1945,yend=8000),color="red", size=2)+
  Theme1

p2 <- ggplot(Atleet, aes(x=Year, y=Nations, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("darkorange","darkblue")) +
  labs(x = "Year", y = "Nations", title = "Number of nations participating") +  
  annotate("text", x=c(1932,1976,1980),
           y=c(60,105,70),
           label=c("L.A. 1932","Montreal 1976","Moscow 1980"),
           size=3)+
  Theme1
p3 <- ggplot(Atleet, aes(x=Year, y=Events, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  labs(x = "Year", y = "Events", title = "Number of events during the olympics") +
  scale_color_manual(values=c("darkorange","darkblue"))+
  Theme1
PA1 <- grid.arrange(p1, p2, p3, ncol=1)
ggsave("Ontwikkeling_ENA.png", PA1, width = 18, height = 15, dpi = 300)
ggsave("Ontwikkeling_Athletes.png", p1, width = 16, height = 9, dpi = 300)
ggsave("Ontwikkeling_Nations.png", p2, width = 16, height = 9, dpi = 300)
ggsave("Ontwikkeling_Events.png", p3, width = 16, height = 9, dpi = 300)

medal_counts_sport <- 
  Sport %>% 
  filter(!is.na(Medal)) %>%
  group_by(Team, Medal, Event) %>% 
  dplyr::summarise(Count = length(Medal))



Athleet_summary <- summarySE(data=Atleet, measurevar="Athletes", groupvars="Season", 
                             na.rm=FALSE, conf.interval=.95)
Athleet_summary

## test 
ggplot(Atleet, aes(x=Year, y=Athletes, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line()+
  geom_line(Atleet, aes(x = year, y = mav(Atleet$Athletes)), color = "red") +
  geom_line()
scale_color_manual(values=c("darkorange","darkblue")) +
  xlab("") +  
  annotate("text", x=c(1932,1956,1976,1980),
           y=c(2000,2750,6800,4700),
           label=c("L.A. 1932","Melbourne 1956","Montreal 1976","Moscow 1980"),
           size=3) +
  annotate("text",x=c(1916,1942),y=c(10000,10000),
           label=c("WWI","WWII"), size=4, color="red") +
  geom_segment(mapping=aes(x=1914,y=8000,xend=1918,yend=8000),color="red", size=2) +
  geom_segment(mapping=aes(x=1939,y=8000,xend=1945,yend=8000),color="red", size=2)+
  theme(panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 11, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 12, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 11, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11)
  )


#### subset eventen, landen, deelnemers Art competitions 
art <- Data %>% 
  filter(Sport == "Art Competitions") %>%
  select(Name, Sex, Age, Team, NOC, Year, City, Event, Medal)
## Intellen
Counts_art <- art %>%
  filter(Team != "Unknown") %>%
  group_by(Year) %>%
  dplyr::summarise(
    Events = length(unique(Event)),
    Nations= length(unique(Team)),
    Artists = length(unique(Name))
  )

p4 <- ggplot(Counts_art, aes(x = Year, y = Events)) +
  geom_point(size=2)+
  geom_line()+
  labs(x = "Year", y = "Events", title = "Number of art events during the olympics") +
  Theme1
p5 <- ggplot(Counts_art, aes(x = Year, y = Nations)) +
  geom_point(size=2)+
  geom_line()+
  labs(x = "Year", y = "Nations", title = "Number of nations participating") +
  Theme1
p6 <- ggplot(Counts_art, aes(x = Year, y = Artists)) +
  geom_point(size=2)+
  geom_line()+
  labs(x = "Year", y = "Artists", title = "Number of artists participating") + 
  Theme1
grid.arrange(p4,p5,p6, ncol=1)
ggsave("Ontwikkeling_ART_Events.png", p4, width = 16, height = 9, dpi = 300)
ggsave("Ontwikkeling_ART_Nations.png", p5, width = 16, height = 9, dpi = 300)
ggsave("Ontwikkeling_ART_Artists.png", p6, width = 16, height = 9, dpi = 300)

## subset medals art 
medal_counts_art <- 
  art %>% 
  filter(!is.na(Medal)) %>%
  group_by(Team, Medal) %>% 
  dplyr::summarise(Count = length(Medal))

# order team by medal count 
medal_level_art <- medal_counts_art %>%
  group_by(Team)%>%
  dplyr::summarise(Total = sum(Count))%>%
  arrange(Team) %>%
  select(Team)

#factor van maken
medal_counts_art$Team <- factor(medal_counts_art$Team, levels=medal_level_art$Team)

# plot
art1 <- ggplot(medal_counts_art, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  #scale_fill_manual(values=c("gold1","gray70","gold4")) +
  ggtitle("Historical medal counts from Art Competitions") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 15, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 15, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))
ggsave("Medalcounts_Arts.png", art1, width = 16, height = 9, dpi = 300)

## inzoomen op Nazi olympics 
# count number of medals awarded to each Team at Nazi Olympics
medal_counts_art_nazis <- art %>% filter(Year==1936, !is.na(Medal))%>%
  group_by(Team, Medal) %>%
  dplyr::summarise(Count=length(Medal)) 

# order Team by total medal count
levs_art_nazis <- medal_counts_art_nazis %>%
  group_by(Team) %>%
  dplyr::summarise(Total=sum(Count)) %>%
  arrange(Total) %>%
  dplyr::select(Team)
medal_counts_art_nazis$Team <- factor(medal_counts_art_nazis$Team, levels=levs_art_nazis$Team)

# plot
art2 <- ggplot(medal_counts_art_nazis, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  ggtitle("Nazi domination of Art Competitions at the 1936 Olympics") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 15, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 15, face = "bold"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15))

ggsave("Medalcounts_Arts_1936.png", art2, width = 16, height = 9, dpi = 300)


## subset
## subset eventen, landen, deelnemers excl Art competitions 
Sport <- Data %>% 
  filter(Sport != "Art Competitions") %>%
  dplyr::select(Name, Sex, Age, Height, Weight,Team, NOC, Year, City,Event, Sport, Medal)

## subset medals Sport 
medal_counts_sport <- 
  Sport %>% 
  filter(!is.na(Medal)) %>%
  group_by(Team, Medal, Event) %>% 
  dplyr::summarise(Count = length(Medal))

## Change name column

colnames(data_mod$`if_else(is.na(Medal), "Noob", "Hero")`) <- "Winnaar"

# order team by medal count 
medal_level_sport <- medal_counts_sport %>%
  group_by(Team)%>%
  summarise(Total = sum(Count))%>%
  arrange(Team) %>%
  select(Team)

#factor van maken
medal_counts_art$Team <- factor(medal_counts_art$Team, levels=medal_level_art$Team)

## Koppelen aan wereld kaart.
# mbv raster package
library(raster)                                  ##Load the Raster Library
world<-getData('GADM', country='FRA', level=1)  ##Get the Province Shapefile for France
plot(world)                                     ##Plot this shapefile


ggplot(medal_counts_sport, aes(x=Team, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  ggtitle("Historical medal counts from Art Competitions") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 11, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 12, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 11, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

### ### ### ### ### ### 
## Verschil man vrouw # 
### ### ### ### ### ### 
# Exclude art competitions from data 
data <- Data %>% filter(Sport != "Art Competitions")

# Recode year of Winter Games after 1992 to match the next Summer Games
# Thus, "Year" now applies to the Olympiad in which each Olympics occurred 
original <- c(1994,1998,2002,2006,2010,2014)
new <- c(1996,2000,2004,2008,2012,2016)
for (i in 1:length(original)) {
  data$Year <- gsub(original[i], new[i], data$Year)
}
data$Year <- as.integer(data$Year)

# Table counting number of athletes by Year and Sex
counts_sex <- data %>% group_by(Year, Sex) %>%
  summarise(Athletes = length(unique(ID)))
counts_sex$Year <- as.integer(counts_sex$Year)

# Plot number of male/female athletes vs time
mf1 <- ggplot(counts_sex, aes(x=Year, y=Athletes, group=Sex, color=Sex)) +
  geom_point(size=2) +
  geom_line()  +
  scale_color_manual(values=c("darkblue","red")) +
  labs(title = "Number of male and female Olympians over time") +
  Theme1
ggsave("Trend_M_F_athlete.png", mf1, width = 16, height = 9, dpi = 300)
## propotion : M vs. F
prop.s<-data %>%
  group_by(Year, Sex) %>% summarise(n = n()) %>% mutate(prop = prop.table(n))
library(plyr)
df_cumsum <- ddply(prop.s, "Sex", transform, label_ypos = cumsum(prop))

mf2 <- ggplot(df_cumsum, aes(x=Year, y=prop, group=Sex, fill=Sex)) +
  geom_bar(stat='identity', position = position_dodge()) +
  #geom_text(aes(y=label_ypos, label=prop), vjust=1.6, 
  #          color="black", size=2.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 1))+
  labs(title = "Number of male and female Olympians over time") +
  Theme1

ggsave("Ratio_M_F_athlete.png", mf2, width = 16, height = 9, dpi = 300)

ggplot(df_cumsum, aes(x=Year, y=prop, group=Sex, fill=Sex)) +
  geom_bar(stat='identity') +
  #geom_text(aes(y=label_ypos, label=prop), vjust=1.6, 
  #          color="black", size=2.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 1))+
  labs(title = "Proportion of male and female Olympians over time") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 11, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 12, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 11, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))
## plyr uitzetten
prop.Tos<-data %>% filter(!is.na(Medal)) %>% group_by(Year, Sex, Medal) %>% summarise(n = n()) %>% mutate(prop = prop.table(n))
library(plyr)
df_cumsum_s <- ddply(prop.Tos, "Sex", transform, label_ypos = cumsum(prop))

ggplot(df_cumsum_s, aes(x=Year, y=prop, group=Sex, fill=Sex)) +
  geom_bar(stat='identity', position = position_dodge()) +
  #geom_text(aes(y=label_ypos, label=prop), vjust=1.6, 
  #          color="black", size=2.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 1))+
  labs(title = "Proportion of male and female Olympians over time") +
  facet_grid(~Medal)+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 11, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 12, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 11, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))

## plyr uitzetten
prop.Tos<-data %>% filter(!is.na(Medal)) %>% group_by(Year, Sex, Medal) %>% summarise(n = n()) %>% mutate(prop = prop.table(n))
library(plyr)
df_cumsum_s <- ddply(prop.Tos, "Sex", transform, label_ypos = cumsum(prop))

ggplot(df_cumsum_s, aes(x=Year, y=prop, group=Sex, fill=Sex)) +
  geom_bar(stat='identity', position = position_dodge()) +
  #geom_text(aes(y=label_ypos, label=prop), vjust=1.6, 
  #          color="black", size=2.5)+
  scale_fill_brewer(palette="Paired")+
  scale_y_continuous(limits = c(0, 1))+
  labs(title = "Proportion of male and female Olympians over time") +
  facet_grid(~Medal)+
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 11, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 12, face = "bold"),
        axis.title.x = element_text(color = "#662506",size = 11, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))
        
### ### ### ### ### ### ### ### ### ###
## Verhouding man vrouw, over landen verdeeld
# tellen M/F/Totaal per land per olympische spelen
# Enkel land en jaar met minstens 50 deelnemers.(zie dippen in grafiek man vrouw)
# plyr uitzetten, om te kunenn draaien
counts_NOC <- data %>% filter(Year %in% c(1936,1956,1976,1996,2016)) %>%
  group_by(Year, NOC, Sex) %>%
  summarise(Count = length(unique(ID))) %>%
  spread(Sex, Count) %>%
  mutate(Total = sum(M,F,na.rm=T)) %>%
  filter(Total > 49)
names(counts_NOC)[3:4] <- c("Male","Female")
counts_NOC$Male[is.na(counts_NOC$Male)] <- 0
counts_NOC$Female[is.na(counts_NOC$Female)] <- 0
counts_NOC$Year <- as.factor(counts_NOC$Year)

# Plot female vs. male athletes by NOC / Year
mf3 <- ggplot(counts_NOC, aes(x=Male, y=Female, group=Year, color=Year)) +
  geom_point(alpha=0.6) +
  geom_abline(intercept=0, slope=1, linetype="dashed") + #indicator 50/50 verhouding
  geom_smooth(method="lm", se=FALSE) +
  labs(title = "Female vs. Male Olympians from participating NOCs") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color=guide_legend(reverse=TRUE))+
  Theme1
ggsave("Ratio_M_F_athlete_ByNOC.png", mf3, width = 16, height = 9, dpi = 300)
## Verhouding van het aantal atleten/medalles die zijn gewonnen door vrouwen.
## voor selecteerde spelen(NOC/Years)

props <- data %>% filter(Year %in% c(1936,1976,2016)) %>%
  group_by(Year, NOC, Sex) %>%
  summarise(Athletes = length(unique(ID)),
            Medals = sum(!is.na(Medal))) 
props <- dcast(setDT(props), 
               Year + NOC ~ Sex, 
               fun.aggregate = sum, 
               value.var = c("Athletes","Medals"))
props <- props %>% 
  mutate(Prop_F_athletes = Athletes_F/(Athletes_F + Athletes_M),
         Prop_F_medals = Medals_F/(Medals_F + Medals_M)) %>%
  filter(Athletes_F + Athletes_M > 49)
props$Prop_F_medals[props$Medals_M + props$Medals_F == 0] <- NA


# Data 1936
props_1936 <- props %>%
  filter(Year==1936) %>%
  gather(Prop_F_athletes,Prop_F_medals, key = "type", value="value")
levs <- props_1936 %>%
  filter(type == "Prop_F_athletes") %>%
  arrange(value) %>% dplyr::select(NOC)

props_1936$NOC <- factor(props_1936$NOC, levels = c(levs$NOC))

# Plot 1936
ggplot(props_1936, aes(x=value, y=NOC, color=type)) +
  geom_point(na.rm=FALSE, alpha=0.8) +
  scale_color_manual(name="",
                     values=c("black","goldenrod"),
                     labels=c("Athletes","Medals")) +
  labs(title="1936 Olympics (Garmisch-Partenkirchen and Berlin)", x="Proportion female") +
  xlim(0,1)+ 
  Theme1

### ### ### ### ### ### ### ### ###
# Count number of medals awarded to each NOC at 1936 Olympics
counts_1936 <- data %>% filter(Year==1936, !is.na(Medal), Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarise(Count=length(Medal)) 

# Order NOC by total medal count
levs_1936 <- counts_1936 %>%
  group_by(NOC) %>%
  summarise(Total=sum(Count)) %>%
  arrange(Total) %>%
  dplyr::select(NOC)
counts_1936$NOC <- factor(counts_1936$NOC, levels=levs_1936$NOC)

# Plot 1936
ggplot(counts_1936, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  ggtitle("Medal counts for women at the 1936 Olympics") +
  Theme1
## ## ### ### ## ## ## ## ## ## ##

## proportions of woman on olympic teams: 1976
# Data for 1976 only
props_1976 <- props %>% 
  filter(Year == 1976) %>%
  gather(Prop_F_athletes, Prop_F_medals, key="type", value="value")
levs <- props_1976 %>% 
  filter(type == "Prop_F_athletes") %>%
  arrange(value) %>% dplyr::select(NOC)
props_1976$NOC <- factor(props_1976$NOC, levels=c(levs$NOC))

# Plot 1976
ggplot(props_1976, aes(x=value, y=NOC, color=type)) +
  geom_point(na.rm=FALSE, alpha=0.8) +
  scale_color_manual(name="",
                     values=c("black","goldenrod"),
                     labels=c("Athletes","Medals")) +
  labs(title="1976 Olympics (Innsbruck and Montreal)", x="Proportion female") +
  xlim(0,1)+
  Theme1


## #### ### ### ### ### ### ### ### ### ### ### ##
# Count number of medals awarded to each NOC at 1976 Olympics
counts_1976 <- data %>% filter(Year==1976, !is.na(Medal), Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarise(Count=length(Medal)) 

# Order NOC by total medal count
levs_1976 <- counts_1976 %>%
  group_by(NOC) %>%
  summarise(Total=sum(Count)) %>%
  arrange(Total) %>%
  dplyr::select(NOC)
counts_1976$NOC <- factor(counts_1976$NOC, levels=levs_1976$NOC)

# Plot 1976
ggplot(counts_1976, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  ggtitle("Medal counts for women at the 1976 Olympics") +
  Theme1

## proportion of women on Olympic teams 2016
# Data for 2014/2016 only
props_2016 <- props %>% 
  filter(Year == 2016) %>%
  gather(Prop_F_athletes, Prop_F_medals, key="type", value="value")
levs <- props_2016 %>% 
  filter(type == "Prop_F_athletes") %>%
  arrange(value) %>% dplyr::select(NOC)
props_2016$NOC <- factor(props_2016$NOC, levels=c(levs$NOC))

# Plot 2014/2016
ggplot(props_2016, aes(x=value, y=NOC, color=type)) +
  geom_point(na.rm=FALSE, alpha=0.8) +
  scale_color_manual(name="",
                     values=c("black","goldenrod"),
                     labels=c("Athletes","Medals")) +
  labs(title="2014/2016 Olympics (Sochi and Rio)", 
       x="Proportion female") +
  xlim(0,1)+ 
  Theme1
## #### ### ### ### ### ### ### ### ### ### ### ##
# Count number of medals awarded to each NOC at 2014/2016 Olympics
counts_2016 <- data %>% filter(Year==2016, !is.na(Medal), Sex=="F") %>%
  group_by(NOC, Medal) %>%
  summarise(Count=length(Medal)) 

# Order NOC by total medal count
levs_2016 <- counts_2016 %>%
  group_by(NOC) %>%
  summarise(Total=sum(Count)) %>%
  arrange(Total) %>%
  dplyr::select(NOC)
counts_2016$NOC <- factor(counts_2016$NOC, levels=levs_2016$NOC)

# Plot 2014/2016
ggplot(counts_2016, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  ggtitle("Medal counts for women at the 2014/2016 Olympics") +
  Theme1

## #### ### ### ### ### ### ### ### ### ### ### ##
## geographic representation
# Load data file matching NOCs with mao regions (countries)
noc <- read_csv("noc_regions.csv",
                col_types = cols(
                  NOC = col_character(),
                  region = col_character()
                ))

Regions <- read.csv2("noc_regions.csv",  sep = ",", header = T)
# Add regions to data and remove missing points
data_regions <- data %>% 
  left_join(noc,by="NOC") %>%
  filter(!is.na(region))

# Subset to Games of interest and count athletes from each country
amsterdam <- data_regions %>% 
  filter(Games == "1928 Summer") %>%
  group_by(region) %>%
  summarise(Amsterdam = length(unique(ID)))
munich <- data_regions %>% 
  filter(Games == "1972 Summer") %>%
  group_by(region) %>%
  summarise(Munich = length(unique(ID)))
rio <- data_regions %>% 
  filter(Games == "2016 Summer") %>%
  group_by(region) %>%
  summarise(Rio = length(unique(ID)))

# Create data for mapping
world <- map_data("world")
mapdat <- tibble(region=unique(world$region))
mapdat <- mapdat %>% 
  left_join(amsterdam, by="region") %>%
  left_join(munich, by="region") %>%
  left_join(rio, by="region")
mapdat$Amsterdam[is.na(mapdat$Amsterdam)] <- 0 ## NA's opvullen met o
mapdat$Munich[is.na(mapdat$Munich)] <- 0
mapdat$Rio[is.na(mapdat$Rio)] <- 0
world <- left_join(world, mapdat, by="region")

# Plot: Amsterdam 1928
Map1 <- ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Amsterdam)) +
  labs(title = "Amsterdam 1928",
       x = NULL, y=NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "navy"),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 15, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 15, face = "bold")) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient(low="white",high="red")
ggsave("Athlete#_AMS1928.png", Map1, width = 16, height = 9, dpi = 300)

# Plot: Munich 1972
Map2 <- ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Munich)) +
  labs(title = "Munich 1972",
       x = NULL, y=NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "navy"),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 15, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 15, face = "bold")) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient(low="white",high="red")
ggsave("Athlete#_MUN1972.png", Map2, width = 16, height = 9, dpi = 300)

## Rio 2016
Map3 <- ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Rio)) +
  labs(title = "Rio 2016",
       x = NULL, y=NULL) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "navy"),
        axis.line.x = element_line(colour="grey80"), 
        axis.line.y = element_line(colour="grey80"),
        strip.text.x =element_text(color = "#662506", size = 15, face = "bold"),
        strip.background = element_blank(),
        title = element_text(color = "#662506",size = 15, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = "#662506",size = 15, face = "bold"),
        axis.title.y = element_text(color = "#662506",size = 15, face = "bold")) +
  guides(fill=guide_colourbar(title="Athletes")) +
  scale_fill_gradient(low="white",high="red")
ggsave("Athlete#_RIO2016.png", Map3, width = 16, height = 9, dpi = 300)
##
## Aggregations
Count <- data %>%
  group_by(Year, Season, Team) %>%
  summarise(NumberOfAthltes = n())

Gold_Winners <- data %>%
  filter(Medal != "<NA>")%>%
  group_by(Year, Season, Team) %>%
  summarise(NumberOfMedals = n())

Aggregated <- Count %>% left_join(Gold_Winners, by = c("Year", "Season", "Team"))

groupMale <- data %>%
  filter(Sex == "M") %>%
  group_by(Year, Season, Team) %>%
  summarise(Number_Of_Men = n())

groupFemale <- data %>%
  filter(Sex == "F") %>%
  group_by(Year, Season, Team) %>%
  summarise(Number_Of_Women = n())

group <- groupMale %>%
  left_join(groupFemale) %>%
  mutate(Sex_Ratio = Number_Of_Men/Number_Of_Women)

## Joinen
group$Sex_Ratio[is.na(group$Sex_Ratio)] <- 236

Aggregated <- Aggregated %>%
  left_join(group, by = c("Year", "Season", "Team"))

AgeAgg <- data %>%
  group_by(Year, Season, Team) %>%
  summarise(MedianAge = median(Age, na.rm = T))

HeightAgg <- data %>%
  group_by(Year, Season, Team) %>%
  summarise(MedianHeight = median(Height, na.rm = T))

WeightAgg <- data %>%
  group_by(Year, Season, Team) %>%
  summarise(MedianWeight = median(Weight, na.rm = T))

Aggregated <- Aggregated %>%
  left_join(AgeAgg, by = c("Year", "Season", "Team"))
Aggregated <- Aggregated %>%
  left_join(HeightAgg, by = c("Year", "Season", "Team"))
Aggregated <- Aggregated %>%
  left_join(WeightAgg, by = c("Year", "Season", "Team"))

Aggregated$NumberOfMedals[is.na(Aggregated$NumberOfMedals)] <- 0
Aggregated$Sex_Ratio[is.na(Aggregated$Sex_Ratio)] <- 0
## Frequent used theme 
TTheme <- theme(panel.background = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(colour="grey80"), 
                axis.line.y = element_line(colour="grey80"),
                strip.text.x =element_text(color = "#662506", size = 11, face = "bold"),
                strip.background = element_blank(),
                title = element_text(color = "#662506",size = 12, face = "bold"),
                plot.title = element_text(hjust = 0.5),
                axis.title.x = element_text(color = "#662506",size = 11, face = "bold"),
                axis.title.y = element_text(color = "#662506",size = 11, face = "bold"),
                axis.text.x = element_text(size = 11),
                axis.text.y = element_text(size = 11)
)

## Influences on Medals won
# number of athletes 
md1 <- Aggregated %>% 
  ggplot(aes(x=NumberOfAthltes, y=NumberOfMedals)) +
  geom_point(col="navyblue")  + geom_smooth(method = "lm", se=TRUE, color="orangered1", aes(group=1)) +
  labs(x = "Number of Athletes", y = "Number of Medals") +
  Theme1
ggsave("Athlete#_Medals#.png", md1, width = 16, height = 9, dpi = 300)
# sex 
s1 <- Aggregated %>% 
  filter(!is.na(Number_Of_Women)) %>%
  ggplot(aes(x=Number_Of_Women, y=NumberOfMedals)) +
  geom_point(col="hotpink2")  + geom_smooth(method = "lm", se=TRUE, color="orangered1", aes(group=1)) +
  labs(x = "Number of Female Athletes", y = "Number of Medals")+
  TTheme

s2 <- Aggregated %>% 
  filter(!is.na(Number_Of_Men)) %>%
  ggplot(aes(x=Number_Of_Men, y=NumberOfMedals)) +
  geom_point(col="navyblue")  + geom_smooth(method = "lm", se=TRUE, color="orangered1", aes(group=1)) +
  labs(x = "Number of Male Athletes", y = "Number of Medals")+
  TTheme

md2 <- cowplot::plot_grid(s1,s2, ncol = 1, 
                          align = 'h', axis = 'l')
ggsave("Athlete#_Medals#_Sx.png", md2, width = 16, height = 9, dpi = 300)
## Data volledigheid
# Check data availability
hw1 <-data %>% group_by(Year, Sex) %>%
  summarise(Present = length(unique(ID[which(!is.na(Height) & !is.na(Weight))])),
            Total = length(unique(ID))) %>%
  mutate(Proportion = Present/Total) %>%
  ggplot(aes(x=Year, y=Proportion, group=Sex, color=Sex)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values=c("darkblue","red"))  +
  labs(title="Height/Weight data completeness from each Olympiad")+
  Theme1

ggsave("HW_completeness.png", hw1, width = 16, height = 9, dpi = 300)

## There was a dramatic increase in data completeness starting in 
## 1960, reaching 86% for women and 90% for men. For 
## all of the Games after this point, data completeness remained 
## above 85% except for 1992, where completeness dips down 
##to 80% for unclear reasons. limited the 
##remainder of this data exploration to Games from 1960 onward, 
##which includes a total of 15 Olympiads spread over a 56 year period. 

# Remove missing Height/Weight data and limit to years from 1960 onward
data_v <- data %>% filter(!is.na(Height), !is.na(Weight), Year > 1959) 

## trends Heights and weigths of Olympic athletes over time, 
## with grouped data by sex 
hw2 <- data_v %>% ggplot(aes(x=as.factor(Year), y=Height, fill=Sex)) +
  geom_boxplot(alpha=0.75) +
  xlab("Olympiad Year") + ylab("Height (cm)") +
  scale_fill_manual(values=c("blue","red")) +
  Theme1
ggsave("Height_trends.png", hw2, width = 16, height = 9, dpi = 300)
## Athletes weight over time 
hw3 <- data_v %>% ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
  geom_boxplot(alpha=0.75) +
  xlab("Olympiad Year") + ylab("Weight (kg)") +
  scale_fill_manual(values=c("blue","red"))+
  Theme1
ggsave("weight_trends.png", hw3, width = 16, height = 9, dpi = 300)


# To charaterize historical trends in size for different events, I fit separate linear regressions
## for Height ~ Year and Weight ~ Year for athletes in each event, and saved the estimated regression slopes. 
## By plotting the estimated regression slopes for height against the estimated regression slopes for weight 
## across different events, we can identify events in which the size of athletes have changed the most. 
## Importantly, the quadrant of the plot in which the point falls indicates the type of size change for 
## each event:

## - Upper left quadrant: athletes have gotten shorter and heavier
## - Upper right quadrant: athletes have gotten taller and heavier
## - Lower right quadrant: athletes have gotten taller and lighter
## - Lower left quadrant: athletes have gotten shorter and lighter

## Identify events present in all 15 Games (since 1960)
names(data_v$Event)
events <- data_v[data_v$Year==1960, "Event"] %>% unique 
## %>% .["Event"] #177 in 1960
years <- data_v$Year %>% unique %>% sort %>% tail(-1)
for (i in 1:length(years)) {
  nxt <- data_v[data_v$Year==years[i],"Event"] %>% unique 
  events <- intersect(events, nxt)
}

# Subset data to only these events
data_Vs <- data_v %>% filter(Event %in% events)

# Get list of sports matching events
sports_events <- data_Vs %>% dplyr::select(Sport, Event) %>% unique

## Change in height vs change in weight over time across menâs sports
# Eliminate wrestling, weightlifting, and boxing
sports_events <- sports_events %>% 
  filter(!Sport %in% c("Wrestling","Weightlifting","Boxing","Equestrianism")) %>%
  filter(!Event %in% c("Figure Skating Mixed Pairs")) %>%
  arrange(Sport)

# Add column for men/women/mixed
sports_events$Sex <- ifelse(grepl("Women",sports_events$Event),"Women","Men")

# Loop through events and fit regressions
s.height <- s.weight <- c()
for (i in 1:nrow(sports_events)) {
  temp <- data %>% filter(Event == sports_events$Event[i])
  lm.height <- lm(Height ~ Year, data=temp)
  lm.weight <- lm(Weight ~ Year, data=temp)
  s.height[i] <- lm.height$coefficients["Year"]
  s.weight[i] <- lm.weight$coefficients["Year"]
}
slopes <- tibble(Sport = sports_events$Sport, 
                 Event = sports_events$Event,
                 Sex = sports_events$Sex,
                 Height = s.height,
                 Weight = s.weight)

# Multiple slopes by 56 since 56 years passed between 1960 to 2016
slopes$Height <- round(slopes$Height*56,1)
slopes$Weight <- round(slopes$Weight*56,1)

# Plot regression slopes of weight ~ height for men
g2.m <- ggplot(slopes[slopes$Sex=="Men",], aes(x=Height, y=Weight, color=Sport, label=Event)) +
  geom_point(alpha=0.75) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(title="Temporal trends in men's size in different events",
       x="Height (cm)",
       y="Weight (kg)")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")
ggplotly(g2.m)


## Change in height vs change in weight over time across women's sports
# Plot regression slopes of weight ~ height for women
g2.f <- ggplot(slopes[slopes$Sex=="Women",], aes(x=Height, y=Weight, color=Sport, label=Event)) +
  geom_point(alpha=0.75) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(title="Temporal trends in women's size in different events",
       x="Height (cm)",
       y="Weight (kg)")  +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none")
ggplotly(g2.f)


## Visuals Pt 3 
####Major Sports

## Another popular library for visualization is ggplot2 with potly. 
## In this Chart, major sports categories are on the X axis with the 
## number of medals won by each Team. The Chart is interactive and information is displayed by clicking on any point.

sport.2 <- Data%>%filter(!is.na(Medal),Season=='Summer',Year==2016)%>%left_join(Regions,by=c("NOC"="NOC"))
sport.2$notes<-NULL
sport.2 <- sport.2%>%group_by(Sport,region)%>%dplyr::summarise(total=n())

slist =c('Archery','Athletics','Badminton','Baseball','Basketball','Boxing','Canoeing','Football','Gymnastics','Hockey','Judo','Rowing',' Shooting','Swimming ','Table Tennis','Tennis','Triathlon','Weightlifting','Wrestling','Volleyball')

sport.2 <- filter(sport.2,Sport%in%slist,total>=1)
sport.2$region <- as.factor(sport.2$region)

p<-ggplot(sport.2,aes(Sport,total,color=region,fill=region)) +
  geom_bar(position = "stack",  width =.75,stat="identity") +
  coord_flip()+
  # theme_minimal() + 
  scale_x_discrete() +
  xlab("Sport")+ylab("Medals")+
  Theme1 +
  theme(legend.position = "none")+
  ggtitle("Major sports  in 2016 games") 

ggplotly(p)

####By Age, height, weight

## Some male and female medal winners are above the age of 40 years. 
## Most male athletes are taller than female athletes. 


sport_m <- Data%>%filter(!is.na(Age),!is.na(Height),!is.na(Weight),!is.na(Medal),Season=='Summer',Year==2016)

a<-ggplot(sport_m, aes(x=Age,y=Height,z=Weight))+
  geom_point(aes(color=factor(Sport),fill=Name),alpha = 1/1.2)+
  facet_wrap(~Sex)+
  Theme1+
  scale_x_continuous() +
  xlab("Age")+ylab("Height")+
  theme(legend.position = "none")
ggtitle("Age, Height and Weight of Medal winners in 2016 Games") 

ggplotly(a)  

Data_S <- Data%>%filter(!is.na(Age),!is.na(Height),!is.na(Weight),!is.na(Medal))
unique_s <- Data_S %>% group_by(Year, Name) %>% dplyr::summarise(count_events = n())         
###By Age

####Age Distribution 

## For both Female and Male athletes Age distribution is slightly right-skewed.
## Majority of the athletes are in the age group of 20 to 30. Few of the athletes
## are above the age of 40 resulting in a slightly right-skewed distribution.

age= Data%>%filter(Age<60,Season=='Summer')

ggplot(age,aes(x=Age))+
  geom_histogram(binwidth = 1,aes(fill = ..count..))+
  facet_wrap(~Sex)+
  theme(axis.line = element_line(color = "orange",size=1))+
  theme(panel.background=element_blank())+ 
  xlab("Age")+ylab("Number of Athletes")+
  theme(legend.position = "none",
        axis.text = element_text(size = 8,face="bold"),
        plot.title = element_text(size=16,face = "bold")) + 
  ggtitle("Age Distribution of Athletes",subtitle = "Summer Olympics 1896 to 2016 ") 


####Average Age  in Major sports

## Average Age various by each sport cateogry. A look at five major sport 
## categories highlights these variances by sport category as well as 
## with progress of the games over the years.

## mbv Alternative plotting package 
install.packages("highcharter")
library(highcharter)

age_sps <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Swimming")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Age,na.rm = TRUE),1))
age_spa <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Athletics")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Age,na.rm = TRUE),1))
age_spg <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Gymnastics")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Age,na.rm = TRUE),1))
age_spf <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Fencing")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Age,na.rm = TRUE),1))
age_spt <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Shooting")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Age,na.rm = TRUE),1))

age_y = Data%>%filter(Season=='Summer')%>%group_by(Year)%>%dplyr::summarise(Y=unique(Year))
age_spt <- age_y%>%left_join(age_spt, by=c("Y"="Year"))

highchart(height = "700px") %>% 
  hc_title(text = "Average Age of male Athletes in 5 major sports") %>%
  hc_subtitle(text = "Summer Olympics 1896 to 2016") %>%
  hc_credits(enabled = TRUE, text = "120 years of Olympic history: athletes and results", 
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_sandsignika()) %>%
  hc_xAxis(categories = age_y$Y,title = list(text = "Year")) %>% 
  hc_add_series(name = "Swimming", data = age_sps$avg)%>% 
  hc_add_series(name = "Athletics", data = age_spa$avg) %>% 
  hc_add_series(name = "Gymnastics",data = age_spg$avg)%>%
  hc_add_series(name = "Fencing",data = age_spf$avg)%>%
  hc_add_series(name = "Shooting",data = age_spt$avg)%>%
  
  hc_yAxis(title = list(text = "Average Age"),
           labels = list(format = "{value}"), max = 45) %>%
  hc_legend(enabled = T, align= "left", verticalAlign = "bottom") 


####Oldest medal winners 

## The oldest ever Olympian is Oscar Swahn of Sweden. He was 72 years, 
## 281 days old when he competed at the 1920 Olympics in shooting. 
## He also qualified for the 1924 Olympics but withdrew without competing.

## Letitia Marion Hamilton won a bronze medal at the art competitions at 
## the 1948 London Olympic Games at the age of 69 years.


age_mt <- Data%>%filter(!is.na(Medal),Season=='Summer')
age_mt <-age_mt%>%group_by(Sex,Sport)%>%dplyr::summarise(Age=max(Age,na.rm = TRUE))
age_max <- Data%>%filter(!is.na(Medal),Season=="Summer")%>%right_join(age_mt,by=c("Sex","Sport","Age"))


c <-ggplot(age_max,aes(Sport,Age, color=Sport,fill=Name)) +
  geom_bar(position = "dodge",  width =.5,stat="identity") +
  coord_flip()+
  facet_wrap(~Sex)+
  theme_grey() + 
  scale_x_discrete() +
  xlab("Sport")+ylab("Age")+
  theme(legend.position = "none",
        axis.text = element_text(size = 8,face="bold"),
        plot.title = element_text(size=16,face = "bold")) + 
  ggtitle("Oldest Medal Winners in  all summer Games") 

ggplotly(c)

####Weight Distribution 

## Average weight of Female and Male athletes have slightly increased 
## over the course of the games history. This is highlighted by the linear 
## line which has a slight upward trend.
library(viridis)
weight= Data%>%filter(Weight<150, Season=='Summer')

ggplot(weight, aes(x=Year, y=Weight,color=Weight))+
  geom_point()+
  scale_color_viridis(option = "B")+
  geom_smooth(method = "lm")+
  facet_wrap(~Sex)+
  theme(axis.line = element_line(color = "orange",size=1))+
  scale_x_continuous(breaks = seq(1896, 2016, by = 15))+
  scale_y_continuous()+
  xlab("year")+ylab("Weight")+
  theme(panel.background=element_blank())+
  theme(legend.position = "bottom",
        axis.text = element_text(size = 8,face="bold"),
        plot.title = element_text(size=16,face = "bold")) + 
  ggtitle("Weight of Athletes",subtitle = "Summer Olympics 1896 to 2016 ") 

####Average Weight in 5 Major Sports

## Average weight in each sport cateogry varies. In the chart there is almost 
## 50% increase in weightlifting comapred to Diving which is quiet apparent.

weight_spw <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Weightlifting")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Weight,na.rm = TRUE),1))
weight_spb <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Boxing")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Weight,na.rm = TRUE),1))

weight_spd <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Diving")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Weight,na.rm = TRUE),1))

weight_sph <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Hockey")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Weight,na.rm = TRUE),1))

weight_swr <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Wrestling")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Weight,na.rm = TRUE),1))

weight_spw <- age_y%>%left_join(weight_spw, by=c("Y"="Year"))
weight_spb <- age_y%>%left_join(weight_spb, by=c("Y"="Year"))
weight_spd <- age_y%>%left_join(weight_spd, by=c("Y"="Year"))
weight_sph <- age_y%>%left_join(weight_sph, by=c("Y"="Year"))
weight_swr <- age_y%>%left_join(weight_swr, by=c("Y"="Year"))

highchart(height = "700px") %>% 
  hc_title(text = "Average weight of male athletes in  5 major sports") %>%
  hc_subtitle(text = "Summer Olympics 1896 to 2016") %>%
  hc_credits(enabled = TRUE, text = "120 years of Olympic history: athletes and results", 
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_flat()) %>%
  hc_xAxis(categories = age_y$Y,title = list(text = "Year")) %>% 
  hc_add_series(name = "Weightlifting", data = weight_spw$avg)%>% 
  hc_add_series(name = "Boxing", data = weight_spb$avg) %>% 
  hc_add_series(name = "Diving",data = weight_spd$avg)%>%
  hc_add_series(name = "Hockey",data = weight_sph$avg)%>%
  hc_add_series(name = "Wrestling",data = weight_swr$avg)%>%
  
  hc_yAxis(title = list(text = "Average Weight"),
           labels = list(format = "{value}"), max = 110) %>%
  hc_legend(enabled = T, align= "left", verticalAlign = "bottom") 

####Heaviest medal winners 
install.packages("ggthemes")
library(ggthemes)
weight_mt <- Data%>%filter(!is.na(Medal),Season=='Summer')%>%group_by(Sex,Sport)%>%dplyr::summarise(Weight=max(Weight,na.rm = TRUE))%>%arrange(desc(Weight))

weight_mt<- weight_mt[-c(76:87),]

weight_max <- Data%>%filter(!is.na(Medal),Season=="Summer")%>%right_join(weight_mt,by=c("Sex","Sport","Weight"))


c <-ggplot(weight_max,aes(Sport,Weight, color=Name,fill=Sport)) +
  geom_bar(position = "dodge",  width =.7,stat="identity") +
  coord_flip()+
  facet_wrap(~Sex)+
  theme_minimal() + scale_colour_tableau()+
  scale_x_discrete() +
  xlab("Sport")+ylab("Weight")+
  theme(legend.position = "none",
        axis.text = element_text(size = 8,face="bold"),
        plot.title = element_text(size=16,face = "bold")) + 
  ggtitle("Heaviest Medal Winners in  all summer Games") 

ggplotly(c)

###By Height

####Height Denisty 

## Medal winning athletes tend to be taller compared to the other athletes. 
## Density curve of all Gold, Silver and Bronze medal winners is slightly higher on the X axis.

height= Data%>%filter(Season=='Summer')
height$Medal=ifelse(is.na(height$Medal),"Others",ifelse(height$Medal=="Gold","Gold",ifelse(height$Medal=="Silver","Silver","Bronze")))

ggplot(height,aes(x=Height))+
  geom_density(aes(group=Medal,colour=Medal,fill=Medal),alpha=0.01)+
  facet_wrap(~Sex)+
  scale_y_continuous(name =  "Density")+
  theme(panel.background=element_blank())+
  theme(axis.line = element_line(color = "orange",size=1))+
  theme(legend.position = "bottom",
        axis.text = element_text(size = 8,face = "bold"),
        plot.title = element_text(size=12,face = "bold")) + 
  ggtitle("Height of all Athletes and Medal winners", subtitle = "Summer Olympics 1896 to 2016")

####Average Height in 5 major sports

## Height of Basketball, volleyball and Rowing athletes are showing steep increase in average height. 
## Average height of athletics cateogry is 179.6 and for basketball athletes it is 199.50. 
## Average  height for athletics category is almost the same since 1896 games.


height_spw <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Volleyball")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Height,na.rm = TRUE),1))
height_spb <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Basketball")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Height,na.rm = TRUE),1))

height_spd <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Football")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Height,na.rm = TRUE),1))

height_sph <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Rowing")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Height,na.rm = TRUE),1))

height_swr <- Data%>%filter(Sex=="M",Season=='Summer',Sport=="Athletics")%>%group_by(Year)%>%dplyr::summarise(avg=round(mean(Height,na.rm = TRUE),1))

height_spw <- age_y%>%left_join(height_spw, by=c("Y"="Year"))
height_spb <- age_y%>%left_join(height_spb, by=c("Y"="Year"))
height_spd <- age_y%>%left_join(height_spd, by=c("Y"="Year"))
height_sph <- age_y%>%left_join(height_sph, by=c("Y"="Year"))
height_swr <- age_y%>%left_join(height_swr, by=c("Y"="Year"))

highchart(height = "700px") %>% 
  hc_title(text = "Average Height of Male athletes of 5 major sports") %>%
  hc_subtitle(text = "Summer Olympics 1896 to 2016") %>%
  hc_credits(enabled = TRUE, text = "120 years of Olympic history: athletes and results", 
             style = list(fontSize = "10px")) %>%
  hc_add_theme(hc_theme_db()) %>%
  hc_xAxis(categories = age_y$Y,title = list(text = "Year")) %>% 
  hc_add_series(name = "Volleyball", data = height_spw$avg)%>% 
  hc_add_series(name = "Basketball", data = height_spb$avg) %>% 
  hc_add_series(name = "Football",data = height_spd$avg)%>%
  hc_add_series(name = "Rowing",data = height_sph$avg)%>%
  hc_add_series(name = "Athletics",data = height_swr$avg)%>%
  
  hc_yAxis(title = list(text = "Average Height"),
           labels = list(format = "{value}"), max = 200) %>%
  hc_legend(enabled = T, align= "left", verticalAlign = "bottom") 

####Max Height 

## Male basketball and volleyball athletes are the tallest 
## among all medal winners. Even among female medal winners 
## the tallest are from the basketball and volleyball teams.

height_ht <- Data%>%filter(!is.na(Medal),Season=='Summer')%>%group_by(Sex,Sport)%>%dplyr::summarise(Height=round(max(Height,na.rm = TRUE),0))%>%arrange(desc(Height))
height_ht <- height_ht[-c(78:87),]

height_max <- Data%>%filter(!is.na(Medal),Season=="Summer")%>%right_join(height_ht,by=c("Sex","Sport","Height"))

h<-ggplot(height_max, aes(x=Age,y=Height,z=Weight))+
  geom_point(aes(color=factor(Sport),fill=Name),alpha = 1/.9)+
  facet_wrap(~Sex)+
  theme_dark() +
  scale_x_continuous() +
  xlab("Age")+ylab("Height")+
  theme(legend.position = "none",
        axis.text = element_text(size = 8,face="bold"),
        plot.title = element_text(size=16,face = "bold")) + 
  ggtitle("Max Height of Medal winners in all Games") 

ggplotly(h)  
