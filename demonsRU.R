library(readxl)
library(tidyverse)
library(lme4)
library(viridis)

#########################
# data
#########################

# query for metadata and demonstratives followed by nouns
# https://korpling.german.hu-berlin.de/annis/#_q=bGVtbWE9Ly4q0YLQvtGCLyAuIHBvcz0iTk9VTiIgJiBtZXRhOjplbGljaXRhdGlvbi1jb3VudHJ5PS9SdXNzaWF8VVNBfEdlcm1hbnkvICYgbWV0YTo6c2V0dGluZz0vZm9ybWFsfGluZm9ybWFsLyAmIG1ldGE6Om1vZGU9L3Nwb2tlbnx3cml0dGVuLyAmCm1ldGE6OnNwZWFrZXItaWQ9Ly4qLyAmCm1ldGE6OnNwZWFrZXItQW9PPS8uKi8gJiAKbWV0YTo6c3BlYWtlci1hZ2U9Ly4qLyAmIAptZXRhOjpzcGVha2VyLWJpbGluZ3VhbD0vLiovCg&ql=aql_quirks_v3&_c=UlVFRy1SVV8xLjAtU05BUFNIT1Q&cl=5&cr=5&s=0&l=10&_seg=ZGlwbA
# the content of the txt file derived from the corpus is transmitted to an excel sheet

dems <- read_excel("dems.xlsx", 
                   col_types = c("skip", "text", "skip", "text", "text", "text", "text", 
                    "text", "skip", "skip", "numeric"))

dems %>% rename(
  bilingual="#10|speaker-bilingual",
  country="#3|elicitation-country",
  doc="#4|doc",
  register="#5|setting",
  mode="#6|mode",
  participant="#7|speaker-id",
  dems="count"
) -> dems

# query for nouns per doc
# https://korpling.german.hu-berlin.de/annis/#_q=cG9zPSJOT1VOIiAmIG1ldGE6OmRvYz0vLiovCg&ql=aql_quirks_v3&_c=UlVFRy1SVV8xLjAtU05BUFNIT1Q&cl=5&cr=5&s=0&l=10&_seg=ZGlwbA&
# the content of the txt file derived from the corpus is transmitted to an excel sheet

nouns <- read_excel("nouns.xlsx", 
                    col_types = c("skip", "skip", "text", "numeric"))

nouns %>% rename(
  doc="#3|doc",
  nouns="count"
) -> nouns

# merge tables with dets and nouns
demonsRU <- dems %>%
  left_join(nouns, by="doc")

# save as factors
demonsRU$participant <- as_factor(demonsRU$participant)
demonsRU$country <- as_factor(demonsRU$country)
demonsRU$doc <- as_factor(demonsRU$doc)
demonsRU$register <- as_factor(demonsRU$register)
demonsRU$mode <- as_factor(demonsRU$mode)
demonsRU$bilingual <- as_factor(demonsRU$bilingual)

# add proportions of det counts per nouns
demonsRU <- demonsRU %>%
  mutate(rate = ((dems/nouns)*100)) 

# aggregate numbers per groups
DETcountry <- aggregate(demonsRU$dems, by=list(demonsRU$country), FUN=sum) # per country
DETparticipant <- aggregate(demonsRU$dems, by=list(participant=demonsRU$participant), FUN=sum) # per participants
DETdocument <- aggregate(demonsRU$dems, by=list(doc=demonsRU$doc), FUN=sum) # per document

#######################
# modelling
#######################

#### preparing for analysis
### aim: extract the number of NPs per doc that are not marked by demonstratives 
demonsRU$NPo <- demonsRU$nouns - demonsRU$dems # NP ohne / without DEMONSTRATIVES
demonsRU %>% uncount(NPo) -> dfRUNPo # creating a table for the binomial distribution
demonsRU %>% uncount(dems) ->dfRUDEMON # creating a table for the binomial distribution

dfRUNPo$coding <- 0
dfRUDEMON$coding <- 1

dfModel <- bind_rows(dfRUNPo, dfRUDEMON)
dfModel %>%  select(-"dems", -"NPo") -> dfModel

dfModel %>% mutate(coding = as.factor(coding)
) -> dfModel

contrasts(dfModel$country) = contr.sum(3)
contrasts(dfModel$register) = contr.sum(2)
contrasts(dfModel$mode) = contr.sum(2)

summary(DEMONModelRU <- glmer(coding ~ country + mode + register + (1|participant), 
                            data=dfModel, family=binomial, control = glmerControl(calc.derivs=FALSE)))
# significant effect of mode and register, no ia of mode and register

summary(DEMONModel.biling <- glmer(coding ~ bilingual + mode + register + (1|participant), 
                            data=dfModel, family=binomial, control = glmerControl(calc.derivs=FALSE)))
# no effect of bilingualism

# post-hoc test
library(multcomp)  
summary(multcomp::glht(DEMONModel,	linfct	=	multcomp::mcp(country	=	"Tukey")))

#####################
# plotting
####################

library(plyr) # causes probs with tidyverse rename 

#ddply for mean, by country, formality, mode
ddply_p <-ddply(demonsRU, .(country, register, mode), summarise, 
                M=mean(rate),
                SE = (sd(rate)/sqrt(length(rate))))

ddply_p

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
ggsave("RU-DEM-t-plot.png", width = 7, height = 4)

p1
