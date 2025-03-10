library(tidyverse)
library(lme4)
library(readxl)
library(viridis)
norm <- read.csv('allNPs.csv', na="", sep="\t", fileEncoding = "UTF-8")%>%
  rename(
    doc = "meta.annis.doc"
  )%>%
  mutate(country = case_when(
    startsWith(doc, "DE") ~ "Germany",
    startsWith(doc, "De") ~ "Germany",
    startsWith(doc, "US") ~ "USA",
    startsWith(doc, "Us") ~ "USA",
    startsWith(doc, "TU") ~ "Turkey",
    startsWith(doc, "Tu") ~ "Turkey",
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
  ))%>%
  mutate(count = as.numeric(count))
NPnorm <- aggregate(norm$count, by=list(country=norm$country), FUN=sum) # country is per country
Pnorm <- aggregate(norm$count, by=list(participant=norm$participant), FUN=sum) # per participants
Dnorm <- aggregate(norm$count, by=list(doc=norm$doc), FUN=sum) # per document 


###
### demonstratives
dfDEMPRON <- read.csv('dfDEMPRON.csv', na="", sep="\t") %>%
  rename(
    doc = "metadoc"
  ) %>%# country / country
  mutate(doc = as.character(doc))%>%
  mutate(country = case_when(
    startsWith(doc, "DE") ~ "Germany",
    startsWith(doc, "De") ~ "Germany",
    startsWith(doc, "US") ~ "USA",
    startsWith(doc, "Us") ~ "USA",
    startsWith(doc, "TU") ~ "Turkey",
    startsWith(doc, "Tu") ~ "Turkey",
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
dfDEMPRON %>% right_join(Dnorm, dfDEMPRON, by = "doc")%>%
  rename(DEMON = "count", # n of DEMONSTRATIVES
         NP = "x")%>% # total NPs
  mutate(DEMON = as.numeric(DEMON),
         NP = as.numeric(NP))%>%
  filter(DEMON != "NA")%>%
  select(-"X.1.pos_lang",
         -"meta.setting",
         -"meta.elicitation.country",
         -"meta.speaker.AoO",
         -"meta.speaker.age",
         -"meta.speaker.age.group",
         -"meta.speaker.bilingual")-> dfTR
dfTR$NPo <- dfTR$NP - dfTR$DEMON # NP ohne / without DEMONSTRATIVES
dfTR %>% uncount(NPo) -> dfTRNPo # creating a table for the binomial distribution
dfTR %>% uncount(DEMON) ->dfTRDEMON # creating a table for the binomial distribution
dfTRNPo$coding <- 0
dfTRDEMON$coding <- 1
dfModel <- bind_rows(dfTRNPo, dfTRDEMON)
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

summary(DEMONModelTR <- glmer(coding ~ country + mode + register + (1|participant), data=dfModel, family=binomial, control = glmerControl(calc.derivs=FALSE)))
# significant effect of mode
library(stargazer)
stargazer(DEMONModelTR)
#Post-hoc tests
library(multcomp)
summary(glht(DEMONModel,	linfct	=	mcp(country	=	"Tukey"))) 

# 
# #### or try other distribution...
# dfTR %>% mutate(country = as.factor(country),
#                    register = as.factor(register),
#                    mode = as.factor(mode),
#                    participant = as.factor(participant),
#                 DEMON = as.numeric(DEMON))-> dfTR
# 
# summary(DEMONModel2 <- lmer(DEMON ~ country + register * mode + (1|participant), data=dfTR)) # no significant results
# 
# summary(glht(DEMONModel2,	linfct	=	mcp(country	=	"Tukey"))) 
# 
# 
#####################
# plotting
####################

# merge tables with dems and nouns
dfDEMPRONrate <- dfDEMPRON %>%
  left_join(Dnorm, by="doc")%>%
  na.omit(x) # omit 6 missing datapoints based on missing doc values 

# add proportions of det counts per nouns

dfDEMPRONrate <- dfDEMPRONrate %>%
  mutate(rate = ((count/x)*100))  #(demons/nouns)*100 please check!
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
  theme_bw()+
  geom_errorbar(width = 0.15,
                position = position_dodge(width = 0.5)) +
  scale_x_discrete(name = "Country of elicitation") +
  scale_y_continuous(name = "% of NPs preceeded by demonstratives") +
  #scale_color_discrete(name = "Country of elicitation") +
  #scale_shape_discrete(name = "Country of elicitation") +
  facet_grid(mode~register) +
  guides(fill = FALSE) 
ggsave("TR-DEM-t-plot.png", width = 7, height = 4)

p1

### investigate phonological effects to to higher frequency of spoken vs written

dfPhonoDemons <- read.csv('phonological-demons.csv', na="", sep=",", encoding="UTF-8") %>%
  # country / country
  mutate(doc = as.character(doc))%>%
  mutate(country = case_when(
    startsWith(doc, "DE") ~ "Germany",
    startsWith(doc, "De") ~ "Germany",
    startsWith(doc, "US") ~ "USA",
    startsWith(doc, "Us") ~ "USA",
    startsWith(doc, "TU") ~ "Turkey",
    startsWith(doc, "Tu") ~ "Turkey",
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
  )) %>% #filter demons that don't directly border other words
  drop_na(left, right)

#only keep last letter of left context, and first letter of right context
  n_last <- 1
  substr(dfPhonoDemons$left, nchar(dfPhonoDemons$left) - n_last + 1, nchar(dfPhonoDemons$left)) -> dfPhonoDemons$leftlast
#merge contexts to analyze consonant clusters  
  paste(dfPhonoDemons$leftlast, dfPhonoDemons$rightfirst) -> dfPhonoDemons$CCcluster
  
  dfPhonoDemons %>% mutate(
    cluster = if_else(
      grepl("a|e|i|i|o|ö|u|ü", CCcluster), 0, 1
               )) -> PhonoDemons
  ### PLOT rate of 1 and 0 per mode!!!
  PhonoDemonsPlot <- aggregate(PhonoDemons$cluster, by=list(country=PhonoDemons$country, mode=PhonoDemons$mode), FUN=sum)
  PhonoDemonsMode <- PhonoDemons %>% group_by(country, mode)
  dfDEMPRONplot %>%  right_join(PhonoDemonsPlot, by=c("country","mode")) %>% 
    mutate(n = as.numeric(n)) %>% mutate(x = as.numeric(x))-> ClusterCheck
  

  ggplot(ClusterCheck, aes(x= country, y = x, fill=mode)) +
    geom_bar(stat='identity', position = "dodge") +
    scale_fill_viridis(discrete = T, 
                       option = "G", 
                       begin = 0.2, 
                       end = 0.9)+
    xlab("Country of elicitation")+
    ylab("demonstratives in consonant clusters")+
    theme(axis.text=element_text(size=12))
  ggsave("Cluster.png", width = 7, height = 4)
  
  ### demons without clusters
  PhonoDemonsPlot <- aggregate(PhonoDemons$cluster, by=list(country=PhonoDemons$country, mode=PhonoDemons$mode), FUN=sum)
  PhonoDemonsMode <- PhonoDemons %>% group_by(country, mode)
  dfDEMPRONplot %>%  right_join(PhonoDemonsPlot, by=c("country","mode")) %>% 
    mutate(n = as.numeric(n)) %>% mutate(x = as.numeric(x))-> ClusterCheck
  
  
  ggplot(ClusterCheck, aes(x= country, y = x, fill=mode)) +
    geom_bar(stat='identity', position = "dodge") +
    scale_fill_viridis(discrete = T, 
                       option = "G", 
                       begin = 0.2, 
                       end = 0.9)+
    xlab("Country of elicitation")+
    ylab("demonstratives in consonant clusters")+
    theme(axis.text=element_text(size=12))
  ggsave("Cluster.png", width = 7, height = 4)
  
  
  #
  PhonoDemons %>% filter(mode=="spoken") -> dfPhonoSpoken
  dfPhonoSpoken$coding <- 1
  PhonoDemons %>% filter(mode=="written") -> dfPhonoWritten
  dfPhonoWritten$coding <- 0
  dfModelPhono <- bind_rows(dfPhonoSpoken, dfPhonoWritten)
  #dfModelPhono %>%  select(-"NPo") -> dfModelPhono
  
  dfModelPhono %>% mutate(country = as.factor(country),
                     register = as.factor(register),
                     mode = as.factor(mode),
                     doc = as.factor(doc),
                     coding = as.factor(coding),
                     cluster = as.factor(cluster)
  ) -> dfModelPhono
  
  contrasts(dfModelPhono$country) = contr.sum(3)
  contrasts(dfModelPhono$register) = contr.sum(2)
  contrasts(dfModelPhono$mode) = contr.sum(2)
  
  
  summary(DEMONModelPhono <- glmer(coding ~ cluster + (1|doc), data=dfModelPhono, family=binomial, control = glmerControl(calc.derivs=FALSE)))
  
  