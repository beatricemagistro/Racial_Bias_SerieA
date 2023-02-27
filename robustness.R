### ROBUSTNESS CHECKS APPENDIX

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

## models
#OLS DV tackles

lm_tackles <- lm(TotalAttemptedTackles ~Skin + Mins + Position  + 
                   Season.x + Squad.x + Nation,data=Total_Data_subset)

summary(lm_tackles)
#cluster robust standard errors
lm_tackles_rob <-coeftest(lm_tackles, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)
lm_tackles_rob
#predicted values
m4 <- plot(ggeffect(lm_tackles, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Attempted Tackles", title="") + theme(text = element_text(size = 25))

###### tackles pre post COVID to show it's not aggressiveness
# DV tackles pre covid
tkls_pre <- lm(TotalAttemptedTackles~ Skin + Mins + Position + 
                 +Season.x + Squad.x + Nation, data=Total_Data_subset_precovid)
#cluster robust standard errors
tkls_pre_rob <- coeftest(tkls_pre, vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
tkls_pre_rob 
#plot predicted values
plot(ggeffect(tkls_pre, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset_precovid$Player.x)) +ylim(20,45)
pre <- plot(ggeffect(tkls_pre, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset_precovid$Player.x)) +
  ylim(20,45) + labs(title="Pre-COVID", y="Total Attempted Tackles")
# DV tackles post covid
tkls_post <- lm(TotalAttemptedTackles~ Skin + Mins + Position + 
                  Squad.x + Nation, data=Total_Data_subset_postcovid)
#cluster robust standard errors
tkls_post_rob <- coeftest(tkls_post, vcov = vcovHC(tkls_post, type = "HC1"))
tkls_post_rob

#plot predicted values
plot(ggeffect(tkls_post, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset_postcovid$Player.x))+ylim(20,45)
post <- plot(ggeffect(tkls_post, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset_postcovid$Player.x))+
  ylim(20,45)+ labs(title="Post-COVID", y="Total Attempted Tackles")

#merge two plots of predicted values
grid.arrange(pre,post, ncol=2)

# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for tackles pre- and post-covid 95 and 90%CI 
comp_tac_pre <- comparisons(tkls_pre, newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp2_tac_post <- comparisons(tkls_post , newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = "HC1")
comp_tac_pre_90 <- comparisons(tkls_pre, newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp2_tac_post_90 <- comparisons(tkls_post , newdata = datagrid("Skin"),conf_level = 0.90,variables = list(Skin = c(3,17)), vcov = "HC1")

## plot tackles pre post covid differences

comp_tackles <- rbind.fill(comp_tac_pre, comp2_tac_post)

comp_tackles_90 <- rbind.fill(comp_tac_pre_90, comp2_tac_post_90)

traceFD <- ropeladder(x=comp_tackles$comparison,
                      lower=comp_tackles$conf.low,
                      upper=comp_tackles$conf.high,
                      labels=c("Tackles with Fans",
                               "Tackles without Fans"),
                      size=0.65,
                      entryheight=0.30,
                      subentryheight=1,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

FDsigMark1 <- comp_tackles$comparison
is.na(FDsigMark1) <- (comp_tackles$conf.high<0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-10,-8,-6,-4,-2, 0, 2)
xlab <- c("-10","-8","-6","-4","-2","0","2")

# Make plot with tile
file <- "FD_tackles_COVID"
tile(traceFD, FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker skin (17) compared to lighter skin (3)"),
     xaxistitle=list(labels="difference in aggressiveness"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))

## 90 and 95 CI

traceFD <- ropeladder(x=comp_tackles$comparison,
                      lower=comp_tackles$conf.low,
                      upper=comp_tackles$conf.high,
                      labels=c("Tackles with Fans",
                               "Tackles without Fans"),
                      col="darkgrey",
                      entryheight=0,
                      subentryheight=0,
                      size=0.65,
                      lex=1.75,
                      lineend="square",
                      plot=1
)

traceFD_90 <- ropeladder(x=comp_tackles_90$comparison,
                         lower=comp_tackles_90$conf.low,
                         upper=comp_tackles_90$conf.high,
                         labels=c("Tackles with Fans",
                                  "Tackles without Fans"),
                         entryheight=0,
                         subentryheight=0,
                         size=0.65,
                         lex=1.75,
                         lineend="square",
                         plot=1
)

FDsigMark1 <- comp_tackles$comparison
is.na(FDsigMark1) <- (comp_tackles$conf.high<0)
FDtraceSig1 <- ropeladder(x=FDsigMark1,
                          col="white",
                          group=1,
                          plot=1)

vertmarkFD <- linesTile(x=c(0,0), y=c(0,1), plot=1)

xat <- c(-10,-8,-6,-4,-2, 0, 2)
xlab <- c("-10","-8","-6","-4","-2","0","2")

# Make plot with tile
file <- "FD_tackles_COVID2"
tile(traceFD, traceFD_90,FDtraceSig1, vertmarkFD,
     xaxis=list(at=xat, labels=xlab),
     topaxis=list(add=TRUE, at=xat, labels=xlab),
     plottitle=list(labels="Darker skin (17) compared to lighter skin (3)"),
     xaxistitle=list(labels="difference in aggressiveness"),
     width=list(null=1),
     height=list(xaxistitle=3, plottitle=4),
     gridlines=list(type="xt"),
     output=list(outfile=file, width=6.75))

## Check that it is not about VAR. just keep post VAR years (before COVID)
###postvar
# DV Fouls
lm_fouls_postvar <- lm(Fouls~Skin + Mins + Position +TotalAttemptedTackles + 
                         Season.x + Squad.x + Nation,data=Total_Data_subset_postvar)
#cluster robust standard errors
lm_fouls_postvar_rob <- coeftest(lm_fouls_postvar, vcov = vcovCL, cluster = ~ Total_Data_subset_postvar$Player.x)
lm_fouls_postvar_rob
#DV Yellow cards
lm_y_cards_postvar <- lm(yellow_cards_overall~Skin + Mins + Position +TotalAttemptedTackles +
                           Season.x + Squad.x + Nation,data=Total_Data_subset_postvar)
#cluster robust standard errors
lm_y_cards_postvar_rob <- coeftest(lm_y_cards_postvar, vcov = vcovCL, cluster = ~ Total_Data_subset_postvar$Player.x)
lm_y_cards_postvar_rob

#DV red cards
lm_r_cards_postvar <- lm(red_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                           Season.x + Squad.x + Nation,data=Total_Data_subset_postvar)
#cluster robust standard errors
lm_r_cards_postvar_rob <- coeftest(lm_r_cards_postvar , vcov = vcovCL, cluster = ~ Total_Data_subset_postvar$Player.x)
lm_r_cards_postvar_rob

# differences between dark-skinned and light-skinned players (85th vs 15th percentiles) for post-var years 

comp_var <- comparisons(lm_fouls_postvar, newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp2_var <- comparisons(lm_y_cards_postvar , newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = ~Player.x)
comp3_var <- comparisons(lm_r_cards_postvar , newdata = datagrid("Skin"),variables = list(Skin = c(3,17)), vcov = ~Player.x)

### alternative models

## poisson for yellow cards
summary(m_y3 <- glm(yellow_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                      Season.x + Squad.x + Nation, data = Total_Data_subset, family="poisson"))
#cluster robust standard errors
m_y3_rob <- coeftest(m_y3, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)
m_y3_rob

##poisson for reds
summary(m_r3 <- glm(red_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                      Season.x + Squad.x + Nation, data = Total_Data_subset, family="poisson"))
#cluster robust standard errors
m_r3_rob <- coeftest(m_r3, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

m_r3_rob

# ### poisson regressions pre post covid
# yellow pre
summary(m_y_pre <- glm(yellow_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                         Season.x + Squad.x + Nation, data = Total_Data_subset_precovid, family="poisson"))
#cluster robust standard errors
m_y_pre_rob <-coeftest(m_y_pre, vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
m_y_pre_rob
#red pre
summary(m_r_pre <- glm(red_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                         Season.x + Squad.x + Nation, data = Total_Data_subset_precovid, family="poisson"))
#cluster robust standard errors
m_r_pre_rob <- coeftest(m_r_pre, vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
m_r_pre_rob

#yellow post
summary(m_y_post <- glm(yellow_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                          Squad.x + Nation, data = Total_Data_subset_postcovid, family="poisson"))
#robust
m_y_post_rob <- coeftest(m_y_post, vcov = vcovHC(m_y_post, type = "HC1"))
m_y_post_rob 
#red post
summary(m_r_post <- glm(red_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                          Squad.x + Nation, data = Total_Data_subset_postcovid, family="poisson"))
#robust
m_r_post_rob <- coeftest(m_r_post, vcov = vcovHC(m_r_post, type = "HC1"))
m_r_post_rob 

## post var yellow and red poisson regression
#yellow post var
m_y_postvar <- glm(yellow_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data = Total_Data_subset_postvar, family="poisson")
#cluster robust se
m_y_postvar_rob <- coeftest(m_y_postvar, vcov = vcovCL, cluster = ~ Total_Data_subset_postvar$Player.x)
m_y_postvar_rob
# red post var
m_r_postvar <- glm(red_cards_overall~Skin + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data = Total_Data_subset_postvar, family="poisson")
#cluster robust standard errors
m_r_postvar_rob <- coeftest(m_r_postvar, vcov = vcovCL, cluster = ~ Total_Data_subset_postvar$Player.x)
m_r_postvar_rob

######## alternative models for Fouls with Fls variable (FBREF)


# fouls
lm_fouls_fbref <- lm(Fls~Skin + Mins + Position +TotalAttemptedTackles + 
                       Season.x + Squad.x + Nation,data=Total_Data_subset)

summary(lm_fouls_fbref)
#cluster robust standard errors
lm_fouls_fbref_rob <- coeftest(lm_fouls_fbref, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

## precovid fouls

lm_fouls_precovid1_fbref <- lm(Fls~Skin + Mins + Position +TotalAttemptedTackles + 
                                 Season.x + Squad.x + Nation,data=Total_Data_subset_precovid)
summary(lm_fouls_precovid1_fbref)
#cluster robust standard errors
lm_fouls_precovid1_fbref_rob <- coeftest(lm_fouls_precovid1_fbref, vcov = vcovCL, cluster = ~ Total_Data_subset_precovid$Player.x)
lm_fouls_precovid1_fbref_rob
## postcovid fouls

lm_fouls_postcovid1_fbref <- lm(Fls~Skin + Mins + Position +TotalAttemptedTackles + 
                                  Squad.x + Nation,data=Total_Data_subset_postcovid)
summary(lm_fouls_postcovid1_fbref)
#robust
lm_fouls_postcovid1_fbref_rob <- coeftest(lm_fouls_postcovid1_fbref, vcov = vcovHC(lm_fouls_postcovid1_fbref, type = "HC1"))
lm_fouls_postcovid1_fbref_rob
## postvar fouls

lm_fouls_postvar_fbref <- lm(Fls~Skin + Mins + Position +TotalAttemptedTackles + 
                               Season.x + Squad.x + Nation,data=Total_Data_subset_postvar)
#cluster robust standard errors
lm_fouls_postvar_fbref_rob <- coeftest(lm_fouls_postvar_fbref, vcov = vcovCL, cluster = ~ Total_Data_subset_postvar$Player.x)
lm_fouls_postvar_fbref_rob

##########################
#### nonlinear models ####
#########################

#quadratic fouls
mod_non_lin1 <- lm(Fouls~Skin + I(Skin^2) +  Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin1 <-coeftest(mod_non_lin1, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#quadratic yellow cards
mod_non_lin2 <- lm(yellow_cards_overall~Skin + I(Skin^2) + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin2 <-coeftest(mod_non_lin2, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#quadratic red cards
mod_non_lin3 <- lm(red_cards_overall~Skin + I(Skin^2) + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin3 <- coeftest(mod_non_lin3, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#cubic fouls
mod_non_lin4 <- lm(Fouls~Skin + I(Skin^2) + I(Skin^3) + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin4 <- coeftest(mod_non_lin4, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#cubic yellow
mod_non_lin5 <- lm(yellow_cards_overall~Skin + I(Skin^2) + I(Skin^3) + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin5 <- coeftest(mod_non_lin5, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#cubic red 
mod_non_lin6 <- lm(red_cards_overall~Skin + I(Skin^2) + I(Skin^3) + Mins + Position +TotalAttemptedTackles + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin6 <- coeftest(mod_non_lin6, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#quadratic tackles
mod_non_lin7 <- lm(TotalAttemptedTackles~Skin + I(Skin^2) + Mins + Position + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin7 <- coeftest(mod_non_lin7, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)

#cubic tackles
mod_non_lin8 <- lm(TotalAttemptedTackles~Skin + I(Skin^2) + I(Skin^3) + Mins + Position  + 
                     Season.x + Squad.x + Nation, data=Total_Data_subset)
#cluster robust standard errors
rob_non_lin8 <- coeftest(mod_non_lin8, vcov = vcovCL, cluster = ~ Total_Data_subset$Player.x)



### plots of nonlinear models

p1 <- plot(ggeffect(mod_non_lin1, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Fouls", title="") + theme(text = element_text(size = 25))
p2 <-plot(ggeffect(mod_non_lin2, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Yellow Cards", title="") + theme(text = element_text(size = 25))
p3 <-plot(ggeffect(mod_non_lin3, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Red Cards", title="") + theme(text = element_text(size = 25))
p4 <-plot(ggeffect(mod_non_lin7, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Attempted Tackles", title="") + theme(text = element_text(size = 25))

sub_plot <- ggarrange(p1, p2, p3, p4,ncol=2, nrow=2,labels = c("a)","b)","c)","d)"))

p5 <-plot(ggeffect(mod_non_lin4, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Fouls", title="") + theme(text = element_text(size = 25))
p6 <-plot(ggeffect(mod_non_lin5, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Yellow Cards", title="") + theme(text = element_text(size = 25))
p7 <-plot(ggeffect(mod_non_lin6, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Red Cards", title="") + theme(text = element_text(size = 25))
p8 <-plot(ggeffect(mod_non_lin8, terms = c("Skin"),vcov.fun = "vcovCL",cluster = ~ Total_Data_subset$Player.x))+
  labs(y="Attempted Tackles", title="") + theme(text = element_text(size = 25))

sub_plot <- ggarrange(p5, p6, p7, p8,ncol=2, nrow=2,labels = c("a)","b)","c)","d)"))




## regression tables 

stargazer(lm_fouls_postvar_rob ,lm_y_cards_postvar_rob,lm_r_cards_postvar_rob,
          type = "latex", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_postvar$model[,1]), 
                             length(lm_y_cards_postvar$model[,1]), length(lm_r_cards_postvar$model[,1])),
                           c("Adjusted $R^2$", round(summary(lm_fouls_postvar)$adj.r.squared, 3),
                             round(summary(lm_y_cards_postvar)$adj.r.squared,3),round(summary(lm_r_cards_postvar)$adj.r.squared,3))))

stargazer(lm_fouls_postvar_rob ,lm_y_cards_postvar_rob,lm_r_cards_postvar_rob,
          type = "html",out="table5.html", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_postvar$model[,1]), 
                             length(lm_y_cards_postvar$model[,1]), length(lm_r_cards_postvar$model[,1])),
                           c("Adjusted R2", round(summary(lm_fouls_postvar)$adj.r.squared, 3),
                             round(summary(lm_y_cards_postvar)$adj.r.squared,3),round(summary(lm_r_cards_postvar)$adj.r.squared,3))))


stargazer(m_y3_rob,m_r3_rob,
          type = "latex",column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y3), 
                             nobs(m_r3)),
                           c("Log Likelihood", round(logLik(m_y3), 3),
                             round(logLik(m_r3),3)),
                           c("AIC", round(AIC(m_y3), 3),
                             round(AIC(m_r3),3))))

stargazer(m_y3_rob,m_r3_rob,
          type = "html",out="table6.html", column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y3), 
                             nobs(m_r3)),
                           c("Log Likelihood", round(logLik(m_y3), 3),
                             round(logLik(m_r3),3)),
                           c("AIC", round(AIC(m_y3), 3),
                             round(AIC(m_r3),3))))


stargazer(m_y_pre_rob,m_r_pre_rob,
          type = "latex",column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y_pre), 
                             nobs(m_r_pre)),
                           c("Log Likelihood", round(logLik(m_y_pre), 3),
                             round(logLik(m_r_pre),3)),
                           c("AIC", round(AIC(m_y_pre), 3),
                             round(AIC(m_r_pre),3))))

stargazer(m_y_pre_rob,m_r_pre_rob,
          type = "html",out="table7.html",column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y_pre), 
                             nobs(m_r_pre)),
                           c("Log Likelihood", round(logLik(m_y_pre), 3),
                             round(logLik(m_r_pre),3)),
                           c("AIC", round(AIC(m_y_pre), 3),
                             round(AIC(m_r_pre),3))))


stargazer(m_y_post_rob,m_r_post_rob,
          type = "latex",column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y_post), 
                             nobs(m_r_post)),
                           c("Log Likelihood", round(logLik(m_y_post), 3),
                             round(logLik(m_r_post),3)),
                           c("AIC", round(AIC(m_y_post), 3),
                             round(AIC(m_r_post),3))))

stargazer(m_y_post_rob,m_r_post_rob,
          type = "html",out="table8.html", column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y_post), 
                             nobs(m_r_post)),
                           c("Log Likelihood", round(logLik(m_y_post), 3),
                             round(logLik(m_r_post),3)),
                           c("AIC", round(AIC(m_y_post), 3),
                             round(AIC(m_r_post),3))))

stargazer(m_y_postvar_rob,m_r_postvar_rob,
          type = "latex",column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y_postvar), 
                             nobs(m_r_postvar)),
                           c("Log Likelihood", round(logLik(m_y_postvar), 3),
                             round(logLik(m_r_postvar),3)),
                           c("AIC", round(AIC(m_y_postvar), 3),
                             round(AIC(m_r_postvar),3))))

stargazer(m_y_postvar_rob,m_r_postvar_rob,
          type = "html",out="table9.html", column.labels=c("Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x","Season.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes",  "Yes"),
                           c("Observations", nobs(m_y_postvar), 
                             nobs(m_r_postvar)),
                           c("Log Likelihood", round(logLik(m_y_postvar), 3),
                             round(logLik(m_r_postvar),3)),
                           c("AIC", round(AIC(m_y_postvar), 3),
                             round(AIC(m_r_postvar),3))))



stargazer(lm_fouls_fbref_rob,lm_fouls_precovid1_fbref_rob,lm_fouls_postcovid1_fbref_rob,lm_fouls_postvar_fbref_rob,
          type = "latex", column.labels=c("Fouls Total","Fouls Pre-COVID","Fouls Post-COVID", "Fouls Post-VAR"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_fbref$model[,1]), 
                             length(lm_fouls_precovid1$model[,1]), length(lm_fouls_postcovid1$model[,1]),
                             length(lm_fouls_postvar$model[,1])),
                           c("Adjusted $R^2$", round(summary(lm_fouls_fbref)$adj.r.squared, 3),
                             round(summary(lm_fouls_precovid1)$adj.r.squared,3),round(summary(lm_fouls_postcovid1)$adj.r.squared,3),
                             round(summary(lm_fouls_postvar_fbref)$adj.r.squared,3))))

stargazer(lm_fouls_fbref_rob,lm_fouls_precovid1_fbref_rob,lm_fouls_postcovid1_fbref_rob,lm_fouls_postvar_fbref_rob,
          type = "html",out="table10.html",  column.labels=c("Fouls Total","Fouls Pre-COVID","Fouls Post-COVID", "Fouls Post-VAR"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes", "Yes"),
                           c("Observations", length(lm_fouls_fbref$model[,1]), 
                             length(lm_fouls_precovid1$model[,1]), length(lm_fouls_postcovid1$model[,1]),
                             length(lm_fouls_postvar$model[,1])),
                           c("Adjusted R2", round(summary(lm_fouls_fbref)$adj.r.squared, 3),
                             round(summary(lm_fouls_precovid1)$adj.r.squared,3),round(summary(lm_fouls_postcovid1)$adj.r.squared,3),
                             round(summary(lm_fouls_postvar_fbref)$adj.r.squared,3))))

stargazer(lm_tackles_rob, tkls_pre_rob,tkls_post_rob,
          type = "latex", column.labels=c("Tackles","Tackles Pre-COVID","Tackles Post-COVID"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes","Yes"),
                           c("Observations",length(lm_tackles$model[,1]), length(tkls_pre$model[,1]), 
                             length(tkls_post$model[,1])),
                           c("Adjusted R2", round(summary(lm_tackles)$adj.r.squared, 3),
                             round(summary(tkls_pre)$adj.r.squared, 3),
                             round(summary(tkls_post)$adj.r.squared,3))))

stargazer(lm_tackles_rob, tkls_pre_rob,tkls_post_rob,
          type = "html",out="table11.html",  column.labels=c("Tackles","Tackles Pre-COVID","Tackles Post-COVID"),
          covariate.labels=c("Skin Tone","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Squad.x", "Nation", "Season.x"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),notes.append = FALSE,
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes","Yes"),
                           c("Observations",length(lm_tackles$model[,1]), length(tkls_pre$model[,1]), 
                             length(tkls_post$model[,1])),
                           c("Adjusted R2", round(summary(lm_tackles)$adj.r.squared, 3),
                             round(summary(tkls_pre)$adj.r.squared, 3),
                             round(summary(tkls_post)$adj.r.squared,3))))

stargazer(rob_non_lin1, rob_non_lin2,rob_non_lin3,rob_non_lin7,
          type = "latex", column.labels=c("Fouls","Yellow Cards","Red Cards", "Tackles"),
          covariate.labels=c("Skin Tone", "Skin Tone Squared","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Season.x", "Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes", "Yes"),
                           c("Observations", length(mod_non_lin1$model[,1]), 
                             length(mod_non_lin2$model[,1]), length(mod_non_lin3$model[,1]),
                             length(mod_non_lin7$model[,1])),
                           c("Adjusted $R2$", round(summary(mod_non_lin1)$adj.r.squared, 3),
                             round(summary(mod_non_lin2)$adj.r.squared,3),
                             round(summary(mod_non_lin3)$adj.r.squared,3),
                             round(summary(mod_non_lin7)$adj.r.squared,3))))

stargazer(rob_non_lin1, rob_non_lin2,rob_non_lin3,rob_non_lin7,
          type = "html", column.labels=c("Fouls","Yellow Cards","Red Cards", "Tackles"),
          covariate.labels=c("Skin Tone", "Skin Tone Squared","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Season.x", "Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes", "Yes"),
                           c("Observations", length(mod_non_lin1$model[,1]), 
                             length(mod_non_lin2$model[,1]), length(mod_non_lin3$model[,1]),
                             length(mod_non_lin7$model[,1])),
                           c("Adjusted $R2$", round(summary(mod_non_lin1)$adj.r.squared, 3),
                             round(summary(mod_non_lin2)$adj.r.squared,3),
                             round(summary(mod_non_lin3)$adj.r.squared,3),
                             round(summary(mod_non_lin7)$adj.r.squared,3))),
          out="quadratic.html")

stargazer(rob_non_lin4, rob_non_lin5,rob_non_lin6,rob_non_lin8,
          type = "latex", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone", "Skin Tone Squared","Skin Tone Cubic","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Season.x", "Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes", "Yes"),
                           c("Observations", length(mod_non_lin4$model[,1]), 
                             length(mod_non_lin5$model[,1]), length(mod_non_lin6$model[,1]),
                             length(mod_non_lin8$model[,1])),
                           c("Adjusted $R2$", round(summary(mod_non_lin4)$adj.r.squared, 3),
                             round(summary(mod_non_lin5)$adj.r.squared,3),
                             round(summary(mod_non_lin6)$adj.r.squared,3),
                             round(summary(mod_non_lin8)$adj.r.squared,3))))

stargazer(rob_non_lin4, rob_non_lin5,rob_non_lin6,rob_non_lin8,
          type = "html", column.labels=c("Fouls","Yellow Cards","Red Cards"),
          covariate.labels=c("Skin Tone", "Skin Tone Squared","Skin Tone Cubic","Minutes Played", "Position Forward (Ref. Defender)", "Position Midfielder",
                             "Tackles","Constant"), star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          omit = c("Season.x", "Squad.x", "Nation"),
          star.char=c("+", "*", "**", "***"),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"),
          add.lines = list(c("Fixed Effects (Team, Season, Country)", "Yes", "Yes", "Yes", "Yes"),
                           c("Observations", length(mod_non_lin4$model[,1]), 
                             length(mod_non_lin5$model[,1]), length(mod_non_lin6$model[,1]),
                             length(mod_non_lin8$model[,1])),
                           c("Adjusted $R2$", round(summary(mod_non_lin4)$adj.r.squared, 3),
                             round(summary(mod_non_lin5)$adj.r.squared,3),
                             round(summary(mod_non_lin6)$adj.r.squared,3),
                             round(summary(mod_non_lin8)$adj.r.squared,3))),
          out="cubic.html")

