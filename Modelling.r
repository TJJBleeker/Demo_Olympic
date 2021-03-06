### ### ### ### ### ### ### ### ### ### ### ###
## Medal winnen, verklaren door:
## Logitische regressie
## afhankelijk van type event. ==> Per event opstellen? 
##  * Geslacht
##  * Leeftijd
##  * Nationaliteit
##  * Deelnames
##  * Lengte
##  * gewicht

## libraries inladen
libs <- c("ggplot2", "plotly", "tidyverse", "data.table", "gridExtra", "knitr", "stringi")
install.packages(libs)
lapply(libs, require, character.only = TRUE)

## data inladen
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

# Exclude art competitions from data 
data <- Data %>% filter(Sport != "Art Competitions")

# Remove missing Height/Weight data and limit to years from 1960 onward
data_v <- data %>% filter(!is.na(Height), !is.na(Weight), Year > 1959) 

## ## ### ### ## 

ggplot(data_mod)+
  geom_histogram(aes(count))

D11 <- ggplot(data = data_mod_f, aes(Height)) + 
  geom_density(aes(fill = Winnaar), alpha = 0.55)+
  scale_fill_manual(values = c("FALSE" = "red1", "TRUE" = "royalblue"))+
  Theme1

D12 <- ggplot(data = data_mod_f, aes(Weight)) + 
  geom_density(aes(fill = Winnaar), alpha = 0.55)+
  scale_fill_manual(values = c("FALSE" = "red1", "TRUE" = "royalblue"))+
  Theme1

D13 <- ggplot(data = data_mod_f, aes(Age)) + 
  geom_density(aes(fill = Winnaar), alpha = 0.55)+
  scale_fill_manual(values = c("FALSE" = "red1", "TRUE" = "royalblue"))+
  Theme1
DZ1 <- grid.arrange(D11, D12, D13, ncol=1)
ggsave("Density_Winnaars.png", DZ1, width = 16, height = 12, dpi = 300)

D1 <- ggplot(data_mod_f) + 
  geom_histogram(bins=35,aes(x=Height,y=..density..), position="identity", fill = "Forestgreen") + 
  geom_density(aes(x=Height,y=..density..), lwd = 0.75, col = "Black") +
  Theme1
D2 <- ggplot(data_mod_f) + 
  geom_histogram(bins=35,aes(x=Weight,y=..density..), position="identity", fill = "Lightblue") + 
  geom_density(aes(x=Weight,y=..density..), lwd = 0.75, col = "Black") +
  Theme1
D3 <- ggplot(data_mod_f) + 
  geom_histogram(bins=35,aes(x=Age,y=..density..), position="identity", fill = "tomato") + 
  geom_density(aes(x=Age,y=..density..), lwd = 0.75, col = "Black") +
  Theme1
DZ <- grid.arrange(D1, D2, D3, ncol=1)
ggsave("Density.png", DZ, width = 16, height = 12, dpi = 300)

## medalists
medalist <- Data %>% 
  filter(!is.na(Medal)) 

medalist %>% 
  group_by(Medal) %>% 
  summarise(
    mean = mean(Age, na.rm = TRUE)
  )
medalist %>%
  group_by(Season, Sex) %>% 
  summarise(
    mean = mean(Age, na.rm = TRUE)
  )

## Summer olympics 
summer_medalist <-
  medalist %>% 
  group_by(Year, Season, Medal) %>% 
  filter(Season == "Summer") %>% 
  summarise(
    mean = mean(Age, na.rm = TRUE)
  )


ggplot(data = summer_medalist, 
       mapping = aes(x = Year, y= mean, group = Medal, color = Medal)) +
  scale_color_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  geom_point(size=2) +
  geom_line(lwd = 0.75) +
  ggtitle("summer olympic medalist's average of age") + 
  Theme1
# n between WW1 and WW2, the average age of medalists is decreasing. After WW2, 
# the average age temporarily rose. But since that time it kept going down until 1980.
# It is rising again with bottom of 1980.

summer_medalist_sex <-
  medalist %>%
  filter(Season == "Summer") %>% 
  group_by(Year, Sex) %>% 
  summarise(
    mean = mean(Age, na.rm = TRUE)
  )

ggplot(data = summer_medalist_sex, 
       mapping = aes(x = Year, y= mean, group = Sex, color = Sex)) +
  geom_point(size=2) +
  geom_line() +
  ggtitle("summer olympic medalist's average of age") +
  ylim(20,35) + 
  Theme1
## By sex, men get medals at older age than women, but in recent years the differences are shrinking. 

## Winter Olympics 
medalist_winter <-
  medalist %>% 
  group_by(Year, Season, Medal) %>% 
  filter(Season == "Winter") %>% 
  summarise(
    mean = mean(Age, na.rm = TRUE)
  )

ggplot(data = medalist_winter, mapping = aes(x= Year, y= mean, group = Medal, color = Medal)) +
  scale_color_manual(values = c("Bronze" = "sienna4", "Silver" = "gray70", "Gold"="gold4"))+
  geom_point(size=2) +
  geom_line(lwd = 0.75) +
  Theme1

## Chronological look 
season_medalist <-
  medalist %>% 
  group_by(Season, Year) %>% 
  summarise(
    mean = mean(Age, na.rm = TRUE)
  )

ggplot( data = season_medalist, 
        mapping = aes(x= Year, y= mean, group = Season, color = Season)) +
  geom_point(size=2) +
  geom_line() +
  Theme1 

### ## ### ### ## ### 
### Modelleringen ###
### ## ### ### ## ### 

data_mod =
  Sport %>%
  filter(Year > 1959) %>%
  # mutate(Winnaar = if_else(is.na(Medal), "Noob", "Hero")) %>%
  group_by(Year, Name) %>%
  dplyr::summarise(count = length(Event),
                   Winnaar = any(!is.na(Medal)), 
                   medal_count = sum(!is.na(Medal)),
                   Medal_change = medal_count/count,
                   Height = first(Height),
                   Weight = first(Weight),
                   Age = first(Age),
                   Sex = first(Sex),
                   NOC = first(NOC))
View(data_mod)
summary(data_mod)
data_mod$NOC <- as.chracter( )

dx <- data_mod %>% 
  group_by(Height) %>%
  dplyr::summarise(Medal_change = mean(Medal_change))

ggplot()+
  geom_point(data = dx, aes(x=Height, y = Medal_change))+
  geom_line(data = train, aes(x=Height, y=predicted), col = "red")+
  Theme1

## Dealing with NA's 
row.na <- apply(data_mod, 1, function(x){any(is.na(x))})
sum(row.na) # 70
data_mod_f<- data_mod[!row.na,]


smp_size <- floor(0.30 * nrow(data_mod_f))
set.seed(123) # om reproduceerbaar te maken
train_ind <- sample(seq_len(nrow(data_mod_f)), size = smp_size)
train <- data_mod_f[train_ind,]
test  <- data_mod_f[-train_ind,]

## Correltaties 
libs2 <- c("tidyverse", "corrr", "igraph", "ggraph")
install.packages(libs2)
lapply(libs2, require, character.only = TRUE)


tidy_cors <- 
  data_mod_f[,c("Year", "count", "medal_count", "Height", "Weight", "Age")] %>%
  correlate() %>%
  stretch()

# Convert correlations stronger than some value
# to an undirected graph object
graph_cors <- tidy_cors %>% 
  filter(abs(r) > .3) %>% 
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2")) +
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between Olympic atlete variables")

### ### ## ### ### ## ### ### 
### ##  Modelleringen  ## ###
### ### ## ### ### ## ### ### 

# Meervoudige lineaire regressive voorbeeld
fit <- glm(Winnaar ~ Height + Weight + Age + Sex, data=train)
summary(fit) # Laat resultaten zien

# Andere nuttige functies
coefficients(fit) # model co?ffici?nten
confint(fit, level=0.95) # betrouwbaarheidsintervallen voor de parameters
fitted(fit) # voorspelde waardes
residuals(fit) # residuen
anova(fit) # anova tabel

#### Selectie procedure ##
## standaard lineair model
nothing <- glm(data=train, Winnaar ~ 1, family = binomial)
## achterwaartse selectie 
backward <- step(fit, trace = 0) #Niet efficient met de hoeveelheid variabelen die het model nu kent
## backward <- step(m2015, trace = 0) ; Laat stap voor stap selectie achterwege
formula(backward)
summary(backward)
backward$deviance
glm3$deviance
## voorwaartse selectie
forward <- step(nothing, 
                scope = list(lower=formula(nothing),
                             upper = formula(fit)),
                direction = "forward")

formula(fit)
formula(backward)
formula(forward)
summary(forward)
## tweezijdige
bothways <- step(nothing, 
                 scope = list(lower=formula(nothing),
                              upper = formula(fit)),
                 direction = "both") ## , trace = 0

formula(bothways)
summary(bothways)
exp_cov <- exp(coef(fit)) # exponentiated coefficients
exp_cov_int <- exp(confint(fit)) # 95% CI for exponentiated coefficients
write.table(exp_cov, 'clipboard', sep='\t')
write.table(exp_cov_int, 'clipboard', sep='\t')

predict(fit, type="response") # predicted values
residuals(fit, type="deviance") # residuals 
fitted(fit)

## analyseren 
anova_fit <- anova(fit,test="Chisq")
anova_fit <- anova(bothways,test="Chisq")
anova_fit <- anova(forward,test="Chisq")
anova_fit <- anova(backward,test="Chisq")
coef(fit)
summary(bothways)
train$predicted = predict(fit)    # save predicted values
train$residuals = residuals(fit)   # save residual values
# controle 
train %>% dplyr::select(Name, Winnaar, Age, Height, Weight, predicted, residuals)
train$Winnaar <- as.integer(as.logical(train$Winnaar))
coef(bothways)
#Aggregaties 

LX1 <- train %>% 
  group_by(Weight) %>% 
  dplyr::summarise(win_avg = mean(Winnaar),
                   pred_avg = mean(predicted),
                   res_avg = mean(residuals))

LX2 <- train %>% 
  group_by(Height) %>% 
  dplyr::summarise(win_avg = mean(Winnaar),
                   pred_avg = mean(predicted),
                   res_avg = mean(residuals))

LX3 <- train %>% 
  group_by(Age) %>% 
  dplyr::summarise(win_avg = mean(Winnaar),
                   pred_avg = mean(predicted),
                   res_avg = mean(residuals))
### TEST
TX <- train %>% 
  mutate(Weight_bin  = ifelse(Weight >= 180, "> 180", 
                              ifelse(Weight >= 160 & Weight < 180, "160 <- 179",
                                     ifelse(Weight >= 140 & Weight < 160, "140 <- 159",
                                            ifelse(Weight >= 120 & Weight < 140, "120 <- 139",
                                                   ifelse(Weight >= 100 & Weight < 120, "100 <- 119",
                                                          ifelse(Weight >= 80 & Weight < 100, "80 <- 99",
                                                                 ifelse(Weight >= 60 & Weight < 80, "60 <- 79",
                                                                        ifelse(Weight >= 40 & Weight < 60, "40 <- 59",
                                                                               ifelse(Weight >= 20 & Weight < 20, "20 <- 39",
                                                                                      "< 20"))))))))))  %>% 
  group_by(Weight_bin) %>% 
  dplyr::summarise(win_avg = mean(Winnaar),
                   pred_avg = mean(predicted),
                   res_avg = mean(residuals))

LD1 <- 
  ggplot(data = LX1, aes(x=Weight, y = win_avg))+
  geom_point()+
  geom_segment(aes(xend = Weight, yend = pred_avg), alpha = .2) +  # alpha to fade lines
  geom_smooth(aes(x=Weight, y=pred_avg), color="red", fill="lightblue", method = "loess")+
  geom_point(aes(color = abs(res_avg), size = abs(res_avg))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE)+
  Theme1

LD2 <- 
  ggplot(data = LX2, aes(x=Height, y = win_avg))+
  geom_point()+
  geom_segment(aes(xend = Height, yend = pred_avg), alpha = .2) +  # alpha to fade lines
  geom_smooth(aes(x=Height, y=pred_avg), color="red", fill="lightblue", method = "loess")+
  geom_point(aes(color = abs(res_avg), size = abs(res_avg))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE)+
  Theme1

LD3 <- 
  ggplot(data = LX3[LX3$win_avg != 0 & LX3$win_avg != 1,], aes(x=Age, y = win_avg))+
  geom_point()+
  geom_segment(aes(xend = Age, yend = pred_avg), alpha = .2) +  # alpha to fade lines
  geom_smooth(aes(x=Age, y=pred_avg), color="red", fill="lightblue", method = "loess")+
  geom_point(aes(color = abs(res_avg), size = abs(res_avg))) + # size also mapped
  scale_color_continuous(low = "black", high = "red") +
  guides(color = FALSE, size = FALSE)+
  Theme1

ggsave("Predict_Height.png", LD1, width = 16, height = 9, dpi = 300)
ggsave("Predict_Weight.png", LD2, width = 16, height = 9, dpi = 300)
ggsave("Predict_Age.png", LD3, width = 16, height = 9, dpi = 300)

ggplot(data = LX)+
  geom_bar(aes(x=Weight, y = win_avg), stat = "identity")+
  geom_line(aes(x=Weight, y=pred_avg), col = "red")+
  #geom_smooth(aes(x=Weight_bin, y=pred_avg), color="red", fill="lightblue")+
  Theme1

ggplot(data = LX, aes(x=Weight))+
  geom_histogram(bins=10,aes(y =..density.. ), position="identity", fill = "Forestgreen") +
  # geom_line(aes(x=Weight, y=pred_avg), col = "red")+
  geom_smooth(aes(x=Weight, y=pred_avg), color="red", fill="lightblue")+
  Theme1



geom_histogram(bins=35,aes(x=Height,y=..density..), position="identity", fill = "Forestgreen") + 
  geom_density(aes(x=Height,y=..density..), lwd = 0.75, col = "Black") +
  
  # Create plot
  train %>% 
  gather(key = "iv", value = "x", -Winnaar, -predicted, -residuals) %>%
  ggplot(aes(x = x, y = Winnaar)) +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ iv, scales = "free") +
  theme_bw()

## VALIDATIE 
library(ROCR)

ts <- c("Height", "Weight", "Age", "Sex")
# Voorspelling 
p <- predict(fit, newdata=subset(test,select=ts), type="response")
pr <- prediction(p, test$Winnaar)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
sens <- performance(pr, "sens", "spec")
err <- performance(pr, "err")
SS <- data.frame(SENS=as.data.frame(sens@y.values)[,1],SPEC=as.data.frame(sens@x.values)[,1])
MSE.glm <- sum((p- test)^2)/nrow(test)

plot.ecdf(prf,  xlab="x_i", ylab="Prob(x_i<=y)",  xaxs="i",yaxs="i", xlim=c(0,1000))
## ROC-curve
## Baseplot 
plot(prf)
grid()
abline(0,1, col="red", lty=2)

## GGplot
ggplot(prf,aes(as.numeric(fpr),as.numeric(tpr),color=GeneSet))+geom_line(size = 2, alpha = 0.7)+
  labs(title= "ROC curve", 
       x = "False Positive Rate (1-Specificity)", 
       y = "True Positive Rate (Sensitivity)")+
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
        axis.text.y = element_text(size = 11),
        legend.position = "none")


auc <- performance(pr, measure = "auc")
auc@y.values[[1]] ## 0.6839154
## COR ##
pquan <- quantile(p, seq(0,1,length.out=21)) ## quantificering 
pbin <- cut(p, pquan, include.lowest = TRUE)
ptab <- table(pbin, test$Winnaar)
ptab/rowSums(ptab)

pr2 <- prediction(p, test$Winnaar)
prf2 <- performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2); abline(0,1, col=2)
grid()

auc2 <- performance(pr2, measure = "auc")
auc2@y.values[[1]]

install.packages("caret")
library(caret)
confusionMatrix(table(data = ifelse(p> 0.5,1,0),
                      reference = test$Winnaar))
