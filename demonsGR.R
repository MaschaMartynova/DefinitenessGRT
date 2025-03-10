library(tidyverse)
library(lme4)
library(readxl)
library(viridis)

norm <- read.csv('nounsGR.csv', na="",sep="\t", fileEncoding = "UTF-8")
norm %>%
  rename(
    doc = "Document"
  )%>%
  mutate(country = case_when(
    startsWith(doc, "DE") ~ "Germany",
    startsWith(doc, "De") ~ "Germany",
    startsWith(doc, "US") ~ "USA",
    startsWith(doc, "Us") ~ "USA",
    startsWith(doc, "GR") ~ "Greece",
    startsWith(doc, "Gr") ~ "Greece",
  ))%>%
  mutate(register = case_when(
    grepl("_f", doc) ~ "formal",
    grepl("_i", doc) ~ "informal",
  ))%>%
  mutate(mode = case_when(
    grepl("_fs", doc) ~ "spoken",
    grepl("_is", doc) ~ "spoken",
    grepl("_fw", doc) ~ "written",
    grepl("_iw", doc) ~ "written",
  )) -> norm
NPnorm <- aggregate(norm$count, by=list(country=norm$country), FUN=sum) # country is per country
Pnorm <- aggregate(norm$count, by=list(participant=norm$participant), FUN=sum) # per participants
Dnorm <- aggregate(norm$count, by=list(doc=norm$doc), FUN=sum) # per document

###
### demonstratives
dfDEMPRON <- read.csv('DEMON_GR.csv', na="", fileEncoding = "UTF-8") %>%
  rename(
    doc = "metadoc"
  ) %>%# country / country
  mutate(doc = as.character(doc))%>%
  mutate(country = case_when(
    startsWith(doc, "DE") ~ "Germany",
    startsWith(doc, "De") ~ "Germany",
    startsWith(doc, "US") ~ "USA",
    startsWith(doc, "Us") ~ "USA",
    startsWith(doc, "GR") ~ "Greece",
    startsWith(doc, "Gr") ~ "Greece",
  ))%>%
  mutate(register = case_when(
    grepl("_f", doc) ~ "formal",
    grepl("_i", doc) ~ "informal",
  ))%>%
  mutate(mode = case_when(
    grepl("_fs", doc) ~ "spoken",
    grepl("_is", doc) ~ "spoken",
    grepl("_fw", doc) ~ "written",
    grepl("_iw", doc) ~ "written",
  ))


dfDEMPRONplot <- dfDEMPRON %>% count(country) #norm in calc
dfDEMPRONplot$normN <- dfDEMPRONplot$n / NPnorm$x * 100   
ggplot(dfDEMPRONplot, aes(x= country, y = normN, fill = country)) +
  geom_bar(stat='identity', position = "dodge") +
  scale_fill_viridis(discrete = T, 
                     option = "G", 
                     begin = 0.2, 
                     end = 0.9)+
  xlab("Country of elicitation")+
  ylab("% of NPs preceeded by demonstratives")+
  guides(fill = FALSE)+
  theme(axis.text=element_text(size=12))
ggsave("DEMPRON.png", width = 7, height = 4)

variationDEMPRON <- dfDEMPRON %>% count(participant, country)
variationDEMPRONplot <- Pnorm %>% right_join(variationDEMPRON, by=c("participant"))
variationDEMPRONplot$normN <- variationDEMPRONplot$n / variationDEMPRONplot$x * 100 
ggplot(variationDEMPRONplot, aes(country, normN))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 1)+
  geom_jitter(width = 0.2)  +
  stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="red", fill="red")+
  xlab("Country of elicitation") +
  ylab("% of NPs preceeded by demonstratives")
ggsave("DEMPRONvar.png", width = 7, height = 4)

dfDEMPRONplot <- dfDEMPRON %>% count(country, register) #norm in calc
dfDEMPRONplot$normN <- dfDEMPRONplot$n / NPnorm$x * 100    
ggplot(dfDEMPRONplot, x = country, aes(x= country, y = normN, fill = register)) +
  geom_bar(stat='identity', position = "dodge") +
  scale_fill_viridis(discrete = T, 
                     option = "G", 
                     begin = 0.2, 
                     end = 0.9)+
  xlab("Country of elicitation")+
  ylab("% of NPs preceeded by demonstratives")+
  theme(axis.text=element_text(size=12))
ggsave("DEMPRONreg.png", width = 7, height = 4)

dfDEMPRONplot <- dfDEMPRON %>% count(country, mode) #norm in calc
dfDEMPRONplot$normN <- dfDEMPRONplot$n / NPnorm$x * 100   
ggplot(dfDEMPRONplot, x = country, aes(x= country, y = normN, fill = mode)) +
  geom_bar(stat='identity', position = "dodge") +
  scale_fill_viridis(discrete = T, 
                     option = "G", 
                     begin = 0.2, 
                     end = 0.9)+
  xlab("Country of elicitation")+
  ylab("% of NPs preceeded by demonstratives")+
  theme(axis.text=element_text(size=12))
ggsave("DEMPRONmode.png", width = 7, height = 4)

#### preparing for analysis
### aim: extract the number of NPs per doc that are not marked by demonstratives

Dnorm$doc <- as.character(Dnorm$doc)
dfDEMPRON$doc <- as.character(dfDEMPRON$doc)
Dnorm$doc = substr(Dnorm$doc,0,nchar(Dnorm$doc)-1)

dfDEMPRON %>%
  select(-"meta.setting",
         -"meta.elicitation.country",
         -"meta.speaker.age.group",
           -"meta.speaker.age",
           -"meta.speaker.AoO",
           -"meta.speraker.bilingual")%>% 
  rename(DEMON = "count"
        ) %>%
  left_join(Dnorm, by="doc")%>% 
  rename(NP = "x" # n of DEMONSTRATIVES
  ) %>%
  drop_na(NP) -> dfGR


dfGR$NPo <- dfGR$NP - dfGR$DEMON # NP ohne / without DEMONSTRATIVES
dfGR %>% uncount(NPo) -> dfGRNPo # creating a table for the binomial distribution
dfGR %>% uncount(DEMON) ->dfGRDEMON # creating a table for the binomial distribution
dfGRNPo$coding <- 0
dfGRDEMON$coding <- 1
dfModel <- bind_rows(dfGRNPo, dfGRDEMON)
dfModel %>%  select(-"DEMON", -"NPo") -> dfModel


### modelling
dfModel %>% mutate(country = as.factor(country),
                   register = as.factor(register),
                   mode = as.factor(mode),
                   participant = as.factor(participant),
                   coding = as.factor(coding)
) -> dfModel

contrasts(dfModel$country) = contr.sum(3)
contrasts(dfModel$register) = contr.sum(2)
contrasts(dfModel$mode) = contr.sum(2)

summary(DEMONModelGR <- glmer(coding ~ country + mode + register + (1|participant), data=dfModel, family=binomial, control = glmerControl(calc.derivs=FALSE)))
# significant effect of mode

#Post-hoc tests
summary(multcomp::glht(DEMONModel,	linfct	=	multcomp::mcp(country	=	"Tukey"))) 

#### or try other distribution...
dfGR %>% mutate(country = as.factor(country),
                register = as.factor(register),
                mode = as.factor(mode),
                participant = as.factor(participant),
                DEMON = as.numeric(DEMON))-> dfGR

summary(DEMONModel2 <- lmer(DEMON ~ country + register + mode + (1|participant), data=dfGR)) # no significant results

summary(glht(DEMONModel2,	linfct	=	mcp(country	=	"Tukey"))) 


#####################
# plotting
####################
dfDEMPRON <- dfDEMPRON %>% mutate(doc=as.character(doc))
Dnorm <- Dnorm %>% mutate(doc=as.character(doc))

# merge tables with dems and nouns
dfDEMPRONrate <- dfDEMPRON %>%
  left_join(Dnorm, by="doc")%>%
  na.omit(x)

# add proportions of det counts per nouns

dfDEMPRONrate <- dfDEMPRONrate %>%
  mutate(rate = ((count/x) *100))  #(demons/nouns)*100 please check!
# we need either to refill or to leave the 8 NAs here, otherwise ddply_p cannot be counted

library(plyr) # causes probs with tidyverse rename 

#ddply for mean, by country, formality, mode
ddply_p <-ddply(dfDEMPRONrate, .(country, register, mode), summarise, 
                M=mean(rate),
                SE = (sd(rate)/sqrt(length(rate))))

ddply_p # NAs, s. above

p1 <- ddply_p %>% 
  ggplot(mapping = aes(x = country,
                       y = M,
                       ymax=(M + SE),
                       ymin=(M - SE),
                       color = country, shape = country)) +
  geom_point(position = position_dodge(width = 0.5),
             size = 2.0) + 
  geom_errorbar(width = 0.15,
                position = position_dodge(width = 0.5)) +
  scale_x_discrete(name = "Country of elicitation") +
  scale_y_continuous(name = "% of NPs preceeded by demonstratives") +
  #scale_color_discrete(name = "Country of elicitation") +
  #scale_shape_discrete(name = "Country of elicitation") +
  facet_grid(mode~register) +
  guides(fill = FALSE) 
ggsave("GR-DEM-t-plot.png", width = 7, height = 4)

p1

### PLOT for all DEMONs - cross-ling
# For publication
library(jtools)
#compare
plot_summs(DEMONModelGR, DEMONModelRU, DEMONModelTR, scale = TRUE, inner_ci_level = .9)
library("huxtable")
export_summs(DEMONModelGR, DEMONModelRU, DEMONModelTR, scale = TRUE)

