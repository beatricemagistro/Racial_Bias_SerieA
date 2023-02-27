library(marginaleffects)
library(texreg)
library(sandwich)
library(lmtest)
library(ggplot2)
library(ggpubr)
library(gtools)
library(countrycode)
library(readxl)
library(plyr)
library(dplyr)
library(estimatr)
library(ggeffects)
library(ape)
library(plm)
library(clubSandwich)
library(margins)
library(tidyverse)
library(tile)
library(stargazer)
#load the final dataset
Total_Data <- read_csv("Final_Merged.csv")

# create some new variables

Total_Data$COVID <- ifelse(Total_Data$Season.x=="2020-21", 1,0) # dummy for COVID
Total_Data_subset<- subset(Total_Data, Position != "GK") #exclude GKs because no fouls and no tackles

Total_Data_subset$Position <- as.factor(Total_Data_subset$Position) #transform to factor
Total_Data_subset$Season.x <- as.factor(Total_Data_subset$Season.x) #transform to factor
Total_Data_subset$Squad.x <- as.factor(Total_Data_subset$Squad.x) #transform to factor

#COVID & VAR
Total_Data_subset_postcovid <- dplyr::filter(Total_Data_subset, COVID==1) #create subset postcovid
Total_Data_subset_precovid <- dplyr::filter(Total_Data_subset, COVID==0 & Season.x != "2019-20")# create subset precovid exclude 2019-20
Total_Data_subset_postvar <- dplyr::filter(Total_Data_subset, VAR==1 & Season.x != "2019-20" & Season.x != "2020-21") #create subset postvar and precovid

### histograms

#histogram of 
tone
ggplot(data=Total_Data_subset, aes(x=
                                     )) +
  geom_bar() +
  labs (x="
        Tone", y = "Count") +
  theme_bw() + theme(text = element_text(size = 25))

#histogram of fouls
plot1 <- ggplot(data=Total_Data_subset, aes(x=Fouls)) +
  geom_histogram() +
  labs (x="Fouls", y = "Count") +
  theme_bw() + theme(text = element_text(size = 25))

#histogram yellow cards
plot2 <- ggplot(data=Total_Data_subset, aes(x=yellow_cards_overall)) +
  geom_bar() +
  labs (x="Yellow Cards", y = "Count") +
  theme_bw() + theme(text = element_text(size = 25))

#histogram red cards
plot3 <- ggplot(data=Total_Data_subset, aes(x=red_cards_overall)) +
  geom_bar() +
  labs (x="Red Cards", y = "Count") +
  theme_bw() + theme(text = element_text(size = 25))

# histogram tackles
plot4 <- ggplot(data=Total_Data_subset, aes(x=TotalAttemptedTackles)) +
  geom_histogram() +
  labs (x="Attempted Tackles", y = "Count") +
  theme_bw() + theme(text = element_text(size = 25)) +
  scale_x_continuous(breaks=c(0,50,100,150,200))

sub_plot <- ggarrange(plot1,plot2,plot3,ncol=2, nrow=2,labels = c("a)","b)","c)"))

top_row = ggarrange(plot1, plot2, ncol = 2, labels = c("a)", "b)"))
bottom_row = ggarrange(NULL, plot3, NULL, ncol = 3, labels = c("", "c)", ""), widths = c(1,2,1))
final_plot = ggarrange(top_row, bottom_row, ncol = 1)


### models

# DV fouls
lm_fouls1 <- lm(Fouls~Skin + Mins + Position +TotalAttemptedTackles + 
                  Season.x + Squad.x + Nation, data=Total_Data_subset)

summary(lm_fouls1)
#cluster robust standard errors
lm_fouls1_rob <- coeftest(lm_fouls1, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)
lm_fouls1_rob

#predicted values
m1 <- plot(ggeffect(lm_fouls1, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Fouls", title="") + theme(text = element_text(size = 25))

## DV yellows

lm_y_cards2 <- lm(yellow_cards_overall~ Skin
                    + Mins + Position +TotalAttemptedTackles + 
                    Season.x + Squad.x + Nation,data=Total_Data_subset)

summary(lm_y_cards2)

#cluster robust
lm_y_cards2_rob <-  coeftest(lm_y_cards2, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)
lm_y_cards2_rob 

#predicted values
m2 <- plot(ggeffect(lm_y_cards2, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Yellow Cards", title="") + theme(text = element_text(size = 25))


# DV red

lm_r_cards2 <- lm(red_cards_overall ~Skin
                    + Mins + Position +TotalAttemptedTackles + 
                    Season.x + Squad.x + Nation,data=Total_Data_subset)

summary(lm_r_cards2)

#cluster robust
lm_r_cards2_rob <-coeftest(lm_r_cards2, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)
lm_r_cards2_rob
#predicted values
m3 <- plot(ggeffect(lm_r_cards2, terms = c("Skin "),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Red Cards", title="") + theme(text = element_text(size = 25))

# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for fouls and cards
comp <- comparisons(lm_fouls1, newdata = datagrid("Skin"),variables = list(Skin= c(3,17)), vcov = ~Player.x)
comp2 <- comparisons(lm_y_cards2, newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp3 <- comparisons(lm_r_cards2, newdata = datagrid("Skin"),variables = list(Skin= c(3,17)), vcov = ~Player.x)

## combined plots of predicted values

top_row = ggarrange(m1, m2, ncol = 2, labels = c("a)", "b)"))
bottom_row = ggarrange(NULL, m3, NULL, ncol = 3, labels = c("", "c)", ""), widths = c(1,2,1))
final_plot = ggarrange(top_row, bottom_row, ncol = 1)


#### PRE-POST COVID

## precovid

#DV fouls

lm_fouls_precovid1 <- lm(Fouls~Skin + Mins + Position +TotalAttemptedTackles + 
                           Season.x + Squad.x + Nation,data=Total_Data_subset_precovid)
#cluster robust se
lm_fouls_precovid1_rob <- coeftest(lm_fouls_precovid1 , vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
lm_fouls_precovid1_rob

#DV yellow
lm_y_cards_precovid2 <- lm(yellow_cards_overall~ Skin
                             + Mins + Position +TotalAttemptedTackles + 
                             Season.x + Squad.x + Nation,data=Total_Data_subset_precovid)
summary(lm_y_cards_precovid2 )

#cluster robust se
lm_y_cards_precovid2_rob <- coeftest(lm_y_cards_precovid2, vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
lm_y_cards_precovid2_rob

#DV red

lm_r_cards_precovid2 <- lm(red_cards_overall~Skin
                             + Mins + Position +TotalAttemptedTackles + 
                             Season.x + Squad.x + Nation,data=Total_Data_subset_precovid)
#cluster robust se
lm_r_cards_precovid2_rob <- coeftest(lm_r_cards_precovid2, vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
lm_r_cards_precovid2_rob

# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for fouls and cards pre COVID 95%CI 

comp_pre <- comparisons(lm_fouls_precovid1, newdata = datagrid("Skin"),variables = list(Skin= c(3,17)), vcov = ~Player.x)
comp2_pre <- comparisons(lm_y_cards_precovid2 , newdata = datagrid("Skin"),variables = list(Skin= c(3,17)), vcov = ~Player.x)
comp3_pre <- comparisons(lm_r_cards_precovid2 , newdata = datagrid("Skin"),variables = list(Skin= c(3,17)), vcov = ~Player.x)


# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for fouls and cards pre COVID 90%CI 

comp_pre_90 <- comparisons(lm_fouls_precovid1, newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp2_pre_90 <- comparisons(lm_y_cards_precovid2 , newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp3_pre_90 <- comparisons(lm_r_cards_precovid2 , newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = ~Player.x)

##post COVID

# DV fouls
lm_fouls_postcovid1 <- lm(Fouls~ Skin
                            + Mins + Position +TotalAttemptedTackles + 
                            Squad.x + Nation,data=Total_Data_subset_postcovid)

#robust se
lm_fouls_postcovid1_rob  <- coeftest(lm_fouls_postcovid1, vcov = vcovHC(lm_fouls_postcovid1, type = "HC1"))

# DV yellow
lm_y_cards_postcovid2<- lm(yellow_cards_overall~ Skin
                             + Mins + Position +TotalAttemptedTackles +
                                         Squad.x + Nation,data=Total_Data_subset_postcovid)
#robust se
lm_y_cards_postcovid2_rob  <- coeftest(lm_y_cards_postcovid2 , vcov = vcovHC(lm_y_cards_postcovid2, type = "HC1"))

summary(lm_y_cards_postcovid2_rob)


## DV red 
lm_r_cards_postcovid2 <- lm(red_cards_overall~ Skin
                              + Mins + Position +TotalAttemptedTackles + 
                                         Squad.x + Nation,data=Total_Data_subset_postcovid)
#robust se

lm_r_cards_postcovid2_rob  <- coeftest(lm_r_cards_postcovid2 , vcov = vcovHC(lm_r_cards_postcovid2, type = "HC1"))


# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for fouls and cards post COVID 95%CI 

comp_post <- comparisons(lm_fouls_postcovid1, newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = "HC1")
comp2_post <- comparisons(lm_y_cards_postcovid2 , newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = "HC1")
comp3_post <- comparisons(lm_r_cards_postcovid2 , newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = "HC1")

# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for fouls and cards post COVID 90%CI 

comp_post_90 <- comparisons(lm_fouls_postcovid1, newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = "HC1")
comp2_post_90 <- comparisons(lm_y_cards_postcovid2 , newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = "HC1")
comp3_post_90 <- comparisons(lm_r_cards_postcovid2 , newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = "HC1")

## Figures
## fouls pre post covid 


comp_fouls <- rbind.fill(comp_pre, comp_post)

traceFD <- ropeladder(x=comp_fouls$comparison,
                      lower=comp_fouls$conf.low,
                      upper=comp_fouls$conf.high,
                      labels=c("Fouls with Fans",
                               "Fouls without Fans"),
                      size=0.65,
                      entryheight=0.30,
                      subentryheight=1,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

FDsigMark1 <- comp_fouls$comparison
is.na(FDsigMark1) <- (comp_fouls$conf.low>0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-1, 0, 1, 2, 3,4,5,6)
xlab <- c("-1","0","1","2","3","4", "5","6")

# Make plot with tile
file <- "FD_fouls_COVID"
tile(traceFD, FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker 
     (17) compared to lighter 
                    (3)"),
     xaxistitle=list(labels="difference in referee calls"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))

## with both 90 and 95 confidence intervals

comp_fouls_90 <- rbind.fill(comp_pre_90, comp_post_90)

traceFD <- ropeladder(x=comp_fouls$comparison,
                      lower=comp_fouls$conf.low,
                      upper=comp_fouls$conf.high,
                      labels=c("Fouls with Fans",
                               "Fouls without Fans"),
                      col="darkgrey",
                      entryheight=0,
                      subentryheight=0,
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

traceFD_90 <- ropeladder(x=comp_fouls_90$comparison,
                         lower=comp_fouls_90$conf.low,
                         upper=comp_fouls_90$conf.high,
                         labels=c("Fouls with Fans",
                                  "Fouls without Fans"),
                         entryheight=0,
                         subentryheight=0,
                         size=0.65,
                         lex=1.75,
                         lineend="square",
                         plot=1
)

FDsigMark1 <- comp_fouls$comparison
is.na(FDsigMark1) <- (comp_fouls$conf.low>0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-1, 0, 1, 2, 3,4,5,6)
xlab <- c("-1","0","1","2","3","4", "5","6")

# Make plot with tile
file <- "FD_fouls_COVID2"
tile(traceFD,traceFD_90, FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker 
     (17) compared to lighter 
                    (3)"),
     xaxistitle=list(labels="difference in referee calls"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))



## yellows pre post covid

comp_yellow <- rbind.fill(comp2_pre, comp2_post)

traceFD <- ropeladder(x=comp_yellow$comparison,
                      lower=comp_yellow$conf.low,
                      upper=comp_yellow$conf.high,
                      labels=c("Yellow Cards with Fans",
                               "Yellow Cards without Fans"),
                      size=0.65,
                      entryheight=0.30,
                      subentryheight=1,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

FDsigMark1 <- comp_yellow$comparison
is.na(FDsigMark1) <- (comp_yellow$conf.low>0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-1,-0.75,-0.5,-0.25, 0, 0.25, 0.5, 0.75, 1)
xlab <- c("-1","-0.75","-0.5","-0.25","0","0.25","0.5","0.75","1")

# Make plot with tile
file <- "FD_yellows_COVID"
tile(traceFD, FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker (17) compared to lighter (3)"),
     xaxistitle=list(labels="difference in referee calls"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))

## 90 and 95 CI


comp_yellow_90 <- rbind.fill(comp2_pre_90, comp2_post_90)

traceFD <- ropeladder(x=comp_yellow$comparison,
                      lower=comp_yellow$conf.low,
                      upper=comp_yellow$conf.high,
                      labels=c("Yellow Cards with Fans",
                               "Yellow Cards without Fans"),
                      col="darkgrey",
                      entryheight=0,
                      subentryheight=0,
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

traceFD_90 <- ropeladder(x=comp_yellow_90$comparison,
                         lower=comp_yellow_90$conf.low,
                         upper=comp_yellow_90$conf.high,
                         labels=c("Yellow Cards with Fans",
                                  "Yellow Cards without Fans"),
                         entryheight=0,
                         subentryheight=0,
                         size=0.65,
                         lex=1.75,
                         lineend="square",
                         plot=1
)

FDsigMark1 <- comp_yellow$comparison
is.na(FDsigMark1) <- (comp_yellow$conf.low>0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-1,-0.5, 0, 0.5,  1)
xlab <- c("-1","-0.5","0","0.5","1")

# Make plot with tile
file <- "FD_yellows_COVID2"
tile(traceFD,traceFD_90, FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker (17) compared to lighter (3)"),
     xaxistitle=list(labels="difference in referee calls"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))



## red pre post covid

comp_red <- rbind.fill(comp3_pre, comp3_post)

traceFD <- ropeladder(x=comp_red$comparison,
                      lower=comp_red$conf.low,
                      upper=comp_red$conf.high,
                      labels=c("Red Cards with Fans",
                               "Red Cards without Fans"),
                      size=0.65,
                      entryheight=0.30,
                      subentryheight=1,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

FDsigMark1 <- comp_red$comparison
is.na(FDsigMark1) <- (comp_red$conf.low>0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.10, 0, 0.10, 0.20)
xlab <- c("-0.10","0","0.10","0.20")

# Make plot with tile
file <- "FD_reds_COVID"
tile(traceFD, FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker 
     (17) compared to lighter 
                    (3)"),
     xaxistitle=list(labels="difference in referee calls"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))

## 90 and 95 CI

comp_red_90 <- rbind.fill(comp3_pre_90, comp3_post_90)

traceFD <-ropeladder(x=comp_red$comparison,
                     lower=comp_red$conf.low,
                     upper=comp_red$conf.high,
                     labels=c("Red Cards with Fans",
                              "Red Cards without Fans"),
                     col="darkgrey",
                     entryheight=0,
                     subentryheight=0,
                     size=0.65,
                     lex=1.75,
                     lineend="square",
                     plot=1
)

traceFD_90 <- ropeladder(x=comp_red_90$comparison,
                         lower=comp_red_90$conf.low,
                         upper=comp_red_90$conf.high,
                         labels=c("Red Cards with Fans",
                                  "Red Cards without Fans"),
                         entryheight=0,
                         subentryheight=0,
                         size=0.65,
                         lex=1.75,
                         lineend="square",
                         plot=1
)

FDsigMark1 <- comp_red$comparison
is.na(FDsigMark1) <- (comp_red$conf.low>0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-0.10, 0, 0.10, 0.20)
xlab <- c("-0.10","0","0.10","0.20")

# Make plot with tile
file <- "FD_reds_COVID2"
tile(traceFD, traceFD_90,FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker (17) compared to lighter (3)"),
     xaxistitle=list(labels="difference in referee calls"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))


## tables
#latex
stargazer(lm_fouls1_rob,lm_y_cards2_rob ,lm_r_cards2_rob ,
          type = "latex", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("
                             Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls1$model[,1]), 
                             length(lm_y_cards2$model[,1]), length(lm_r_cards2$model[,1])),
                           c("Adjusted $R^2$", round(summary(lm_fouls1)$adj.r.squared, 3),
                             round(summary(lm_y_cards2)$adj.r.squared,3),round(summary(lm_r_cards2)$adj.r.squared,3))))

#html

stargazer(lm_fouls1_rob,lm_y_cards2_rob ,lm_r_cards2_rob ,
          type = "html",out="table2.html", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("
                             Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls1$model[,1]), 
                             length(lm_y_cards2$model[,1]), length(lm_r_cards2$model[,1])),
                           c("Adjusted R2", round(summary(lm_fouls1)$adj.r.squared, 3),
                             round(summary(lm_y_cards2)$adj.r.squared,3),round(summary(lm_r_cards2)$adj.r.squared,3))))


stargazer(lm_fouls_precovid1_rob,lm_y_cards_precovid2_rob,lm_r_cards_precovid2_rob,
          type = "latex", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("
                             Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_precovid1$model[,1]), 
                             length(lm_y_cards_precovid2$model[,1]), length(lm_r_cards_precovid2$model[,1])),
                           c("Adjusted $R^2$", round(summary(lm_fouls_precovid1)$adj.r.squared, 3),
                             round(summary(lm_y_cards_precovid2)$adj.r.squared,3),round(summary(lm_r_cards_precovid2)$adj.r.squared,3))))

stargazer(lm_fouls_precovid1_rob,lm_y_cards_precovid2_rob,lm_r_cards_precovid2_rob,
          type = "html",out="table3.html", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("
                             Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_precovid1$model[,1]), 
                             length(lm_y_cards_precovid2$model[,1]), length(lm_r_cards_precovid2$model[,1])),
                           c("Adjusted R2", round(summary(lm_fouls_precovid1)$adj.r.squared, 3),
                             round(summary(lm_y_cards_precovid2)$adj.r.squared,3),round(summary(lm_r_cards_precovid2)$adj.r.squared,3))))

stargazer(lm_fouls_postcovid1_rob ,lm_y_cards_postcovid2_rob,lm_r_cards_postcovid2_rob,
          type = "latex", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("
                             Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_postcovid1$model[,1]), 
                             length(lm_y_cards_postcovid2$model[,1]), length(lm_r_cards_postcovid2$model[,1])),
                           c("Adjusted $R^2$", round(summary(lm_fouls_postcovid1)$adj.r.squared, 3),
                             round(summary(lm_y_cards_postcovid2)$adj.r.squared,3),round(summary(lm_r_cards_postcovid2)$adj.r.squared,3))))

stargazer(lm_fouls_postcovid1_rob ,lm_y_cards_postcovid2_rob,lm_r_cards_postcovid2_rob,
          type = "html",out="table4.html", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("
                             Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_postcovid1$model[,1]), 
                             length(lm_y_cards_postcovid2$model[,1]), length(lm_r_cards_postcovid2$model[,1])),
                           c("Adjusted R2", round(summary(lm_fouls_postcovid1)$adj.r.squared, 3),
                             round(summary(lm_y_cards_postcovid2)$adj.r.squared,3),round(summary(lm_r_cards_postcovid2)$adj.r.squared,3))))





