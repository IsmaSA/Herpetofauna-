
df<- read_excel("Reptilia.xlsx", sheet = "Reptilia")#235 entries
any(is.na(df$Cost_ID))
df$Cost_ID<- as.numeric(df$Cost_ID)
expanded <- expandYearlyCosts(df, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                 endcolumn = "Probable_ending_year_adjusted")
expanded<-expanded %>% filter(Impact_year <= "2020")
expanded<-expanded %>% filter(Impact_year >= "1986")             
expanded$cost <- as.numeric(gsub(",", "", expanded$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded <- expanded[!is.na(expanded$cost),]
expanded$cost_bil <- (expanded$cost/1000000000)
sum(expanded$cost_bil) # 16.98
view(df)
nrow(expanded)
#Subsetting
expanded <- expanded[expanded$Implementation %in% c("Observed"),]
sum(expanded$cost_bil) #3.57
sort(expanded_r$Probable_starting_year_adjusted, decreasing = F)
global.raw.all <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1986,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1986,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))

res <- computeAvgTotCost(expanded_r,
                         min.year = 1986,
                         max.year = 2020) # Excluding data after 2020 (e.g. planned budgets)
res

expanded_r%>% group_by(Geographic_region) %>% summarise(costs=sum(Implementation))
expanded_a%>% group_by(Geographic_region) %>% summarise(costs=n())

###########
a <- read.csv2("Amphibian.csv", header = T)

expanded_r$Implementation <- as.numeric(expanded_r$Implementation)
a$Geographic_region[a$Geographic_region== "Pacific Islands"] <- "Oceania"
a$Geographic_region[a$Geographic_region== "Oceania/Pacific Islands"] <- "Oceania"
a$Geographic_region[a$Geographic_region== "Central America"] <- "Central and south america"
a$Geographic_region[a$Geographic_region== "South America"] <- "Central and south america"

{a$Species[a$Species == "Trachemys scripta/Gambusia holbrooki/Procambarus clarkii/Alburnus alburnus/Lepomis gibbosus/Cyprinus carpio/Carassius auratus/Esox lucius/Micropterus salmoides/Graptemys pseudogeographica/Graptemys ouachitensis/Graptemys sp./Mauremys sinensis/Pelodiscus sinensis/Pseudemys concinna/Pseudemys nelsoni/Pseudemys sp./Trachemys emolli/Trachemys gaigeae"] <- "Variety"
  a$Species[a$Species == "Trachemys scripta/Mauremys reevesii/Chrysemys picta/Graptemys pseudogeographica/Graptemys ouachitensis/Graptemys sp./Mauremys sinensis/Pelodiscus sinensis/Pseudemys concinna/Pseudemys nelsoni/Pseudemys sp./Trachemys emolli/Trachemys gaigeae/Gambusia holbrooki/Procambarus clarkii/Alburnus alburnus/Lepomis gibbosus/Cyprinus carpio/Carassius auratus/Esox lucius/Micropterus salmoides"] <- "Variety"
  a$Species[a$Species == "Diverse/Unspecified"] <- "Variety"
  a$Species[a$Species == "Rhinella marina/Polypedates leucomystax"] <- "Variety"
  a$Species[a$Species == "Anolis carolinensis/Polypedates leucomystax"] <- "Variety"
  a$Species[a$Species == "Hemorrhois hippocrepis/Zamenis scalaris"] <- "Variety"
  a$Species[a$Species == "Trachemys scripta/Graptemys pseudogeographica/Pseudemys nelsoni"] <- "Variety"
  a$Species[a$Species == "Hemorrhois hippocrepis/Zamenis scalaris/Malpolon monspessulanus"] <- "Variety"
  a$Class[a$Class == "Reptilia/Amphibia"] <- "Diverse"
  a$Class[a$Class == "Amphibia/Reptilia"] <- "Diverse"}
nrow(a)
table(da$Species)

expanded_a <- expandYearlyCosts(a, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")
expanded_a<-expanded_a %>% filter(Impact_year <= "2020")
expanded_a<-expanded_a %>% filter(Impact_year >= "1986")
expanded_a$cost <- as.numeric(gsub(",", "", expanded_a$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded_a <- expanded_a[!is.na(expanded_a$cost),]
expanded_a$cost_bil <- (expanded_a$cost/1000000000)
sum(expanded_a$cost_bil) # 6.27

#Subsetting
expanded_a_obs <- expanded_a[expanded_a$Implementation %in% c("Observed"),]
sum(expanded_a_obs$cost_bil) #0.07

global.raw.all <- summarizeCosts(expanded_a,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1986,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded_a_obs,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1986,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))

res <- computeAvgTotCost(expanded_r_a,
                         min.year = 1986,
                         max.year = 2020) # Excluding data after 2020 (e.g. planned budgets)
res


######################Reptilia
r <- read.csv2("Reptilia.csv", header = T)
r$Geographic_region[r$Geographic_region== "Pacific Islands"] <- "Oceania"
r$Geographic_region[r$Geographic_region== "Oceania/Pacific Islands"] <- "Oceania"
r$Geographic_region[r$Geographic_region== "Central America"] <- "Central and south america"
r$Geographic_region[r$Geographic_region== "South America"] <- "Central and south america"


{r$Species[r$Species == "Trachemys scripta/Gambusia holbrooki/Procambarus clarkii/Alburnus alburnus/Lepomis gibbosus/Cyprinus carpio/Carassius auratus/Esox lucius/Micropterus salmoides/Graptemys pseudogeographica/Graptemys ouachitensis/Graptemys sp./Mauremys sinensis/Pelodiscus sinensis/Pseudemys concinna/Pseudemys nelsoni/Pseudemys sp./Trachemys emolli/Trachemys gaigeae"] <- "Variety"
  r$Species[r$Species == "Trachemys scripta/Mauremys reevesii/Chrysemys picta/Graptemys pseudogeographica/Graptemys ouachitensis/Graptemys sp./Mauremys sinensis/Pelodiscus sinensis/Pseudemys concinna/Pseudemys nelsoni/Pseudemys sp./Trachemys emolli/Trachemys gaigeae/Gambusia holbrooki/Procambarus clarkii/Alburnus alburnus/Lepomis gibbosus/Cyprinus carpio/Carassius auratus/Esox lucius/Micropterus salmoides"] <- "Variety"
  r$Species[r$Species == "Diverse/Unspecified"] <- "Variety"
  r$Species[r$Species == "Rhinella marina/Polypedates leucomystax"] <- "Variety"
  r$Species[r$Species == "Anolis carolinensis/Polypedates leucomystax"] <- "Variety"
  r$Species[r$Species == "Hemorrhois hippocrepis/Zamenis scalaris"] <- "Variety"
  r$Species[r$Species == "Trachemys scripta/Graptemys pseudogeographica/Pseudemys nelsoni"] <- "Variety"
  r$Species[r$Species == "Hemorrhois hippocrepis/Zamenis scalaris/Malpolon monspessulanus"] <- "Variety"
  r$Class[r$Class == "Reptilia/Amphibia"] <- "Diverse"
  r$Class[r$Class == "Amphibia/Reptilia"] <- "Diverse"}
unique(r$Species)
nrow(r)
expanded_r <- expandYearlyCosts(df, #your help file
                              startcolumn = "Probable_starting_year_adjusted",
                              endcolumn = "Probable_ending_year_adjusted")
expanded_r<-expanded_r %>% filter(Impact_year <= "2020")
expanded_r<-expanded_r %>% filter(Impact_year >= "1986")
expanded_r$cost <- as.numeric(gsub(",", "", expanded_r$Cost_estimate_per_year_2017_USD_exchange_rate))
expanded_r <- expanded_r[!is.na(expanded_r$cost),]
expanded_r$cost_bil <- (expanded_r$cost/1000000000)
sum(expanded_r$cost_bil) # 10.37

expanded%>% group_by(Geographic_region) %>% summarise(costs=sum(cost_bil))

#Subsetting
expanded_r_obs <- expanded_r[expanded_r$Implementation %in% c("Observed"),]
(expanded_r_obs$cost_bil) #3.57

global.raw.all <- summarizeCosts(expanded_r,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1986,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded_r_obs,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1986,
                                 maximum.year = 2020,
                                 year.breaks = seq(1986, 2020, by = 5))

res <- computeAvgTotCost(expanded_r,
                         min.year = 1986,
                         max.year = 2020) # Excluding data after 2020 (e.g. planned budgets)
res


#Diverse
diverse <- read.csv2("Diverse.csv")

{diverse$Geographic_region[diverse$Geographic_region== "Pacific Islands"] <- "Oceania"
  diverse$Geographic_region[diverse$Geographic_region== "Oceania/Pacific Islands"] <- "Oceania"
  diverse$Geographic_region[diverse$Geographic_region== "Central America"] <- "Central and south america"
  diverse$Geographic_region[diverse$Geographic_region== "South America"] <- "Central and south america"
  diverse$Species[diverse$Species == "Trachemys scripta/Gambusia holbrooki/Procambarus clarkii/Alburnus alburnus/Lepomis gibbosus/Cyprinus carpio/Carassius auratus/Esox lucius/Micropterus salmoides/Graptemys pseudogeographica/Graptemys ouachitensis/Graptemys sp./Mauremys sinensis/Pelodiscus sinensis/Pseudemys concinna/Pseudemys nelsoni/Pseudemys sp./Trachemys emolli/Trachemys gaigeae"] <- "Variety"
  diverse$Species[diverse$Species == "Trachemys scripta/Mauremys reevesii/Chrysemys picta/Graptemys pseudogeographica/Graptemys ouachitensis/Graptemys sp./Mauremys sinensis/Pelodiscus sinensis/Pseudemys concinna/Pseudemys nelsoni/Pseudemys sp./Trachemys emolli/Trachemys gaigeae/Gambusia holbrooki/Procambarus clarkii/Alburnus alburnus/Lepomis gibbosus/Cyprinus carpio/Carassius auratus/Esox lucius/Micropterus salmoides"] <- "Variety"
  diverse$Species[diverse$Species == "Diverse/Unspecified"] <- "Variety"
  diverse$Species[diverse$Species == "Rhinella marina/Polypedates leucomystax"] <- "Variety"
  diverse$Species[diverse$Species == "Anolis carolinensis/Polypedates leucomystax"] <- "Variety"
  diverse$Species[diverse$Species == "Hemorrhois hippocrepis/Zamenis scalaris"] <- "Variety"
  diverse$Species[diverse$Species == "Trachemys scripta/Graptemys pseudogeographica/Pseudemys nelsoni"] <- "Variety"
  diverse$Species[diverse$Species == "Hemorrhois hippocrepis/Zamenis scalaris/Malpolon monspessulanus"] <- "Variety"
  diverse$Class[diverse$Class == "Reptilia/Amphibia"] <- "Diverse"
  diverse$Class[diverse$Class == "Amphibia/Reptilia"] <- "Diverse"}

unique(diverse$Class)
unique(diverse$Geographic_region)
unique(diverse$Species)

expanded_d <- expandYearlyCosts(diverse, #your help file
                                startcolumn = "Probable_starting_year_adjusted",
                                endcolumn = "Probable_ending_year_adjusted")
expanded_d<-expanded_d %>% filter(Impact_year <= "2020")
expanded_d<-expanded_d %>% filter(Impact_year >= "1986")

res_d <- computeAvgTotCost(expanded_d,
                           min.year = 1986,
                           max.year = 2020)
res_d #334.209 million




s <- read.csv2("df.csv")
s <-s[!duplicated(s$Species),]

head(s)
s <- filter(s, s$Group=="Reptilia")
table(s$Order)
unique(s$Order)

########### cost type and
unique(expanded_a$Impacted_sector)
unique(expanded_a$Type_of_cost)
{expanded_a$Impacted_sector[expanded_a$Impacted_sector=="Environment/Fishery"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Control/Detection"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Monitoring/Research"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Prevention/Research"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Control/Damage repair/Eradication/Management/Monitoring/Surveillance"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Communication/Eradication/Research"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Early detection/Eradication"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Prevention/Surveillance"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Control/Research"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Control/Education"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Control/Surveillance/Unspecified"] <- "Diverse"
  expanded_a$Type_of_cost[expanded_a$Type_of_cost=="Control/Eradication"] <- "Diverse"
}



colnames(expanded_a)
j <- expanded_a %>% group_by(Impacted_sector) %>%
  summarise(cost= sum(Cost_estimate_per_year_2017_USD_exchange_rate))
j
sum(j$cost)
j$percentage <- (j$cost*100)/6275231961
j
table(expanded_a$Impacted_sector)


sum(j$cost)
j <- expanded_a %>% group_by(Type_of_cost_merged) %>%
  summarise(cost= sum(Cost_estimate_per_year_2017_USD_exchange_rate))
j
j$percentage <- (j$cost*100)/6275231961
j
table(expanded_a$Type_of_cost_merged)
### Reptilia

unique(expanded_r$Impacted_sector)
unique(expanded_r$Type_of_cost)
{expanded_r$Impacted_sector[expanded_r$Impacted_sector=="Environment/Public and social welfare"] <- "Diverse"
expanded_r$Impacted_sector[expanded_r$Impacted_sector=="Environment/Health/Public and social welfare"] <- "Diverse"
expanded_r$Impacted_sector[expanded_r$Impacted_sector=="Agriculture/Environment/Health/Public and social welfare"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Damage-Loss"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Prevention"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Research"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Research/Information"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Detection/Information"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Eradication/Research"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Detection/Eradication"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Eradication"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Research"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Administration/Control/Education/Research"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Communication/Control/Damage repair/Eradication/Monitoring/Research/Surveillance"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Monitoring/Surveillance"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Eradication/Monitoring/Surveillance"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Early detection/Eradication"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Monitoring/Research"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Monitoring"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Control/Early detection"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Damage-Loss/Damage-repair"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Management (Unspecified)/Control/Research"] <- "Diverse"
expanded_r$Type_of_cost[expanded_r$Type_of_cost=="Funding/Management (Unspecified)/Education"] <- "Diverse"
}



j <- expanded_r %>% group_by(Impacted_sector) %>%
  summarise(cost= sum(Cost_estimate_per_year_2017_USD_exchange_rate))
j$cost <- sort(j$cost, decreasing = T)
j
sum(h$cost)
j$percentage <- (j$cost*100)/10373077830
j
table(expanded_r$Impacted_sector)

h <- expanded_r %>% group_by(Type_of_cost_merged) %>%
  summarise(cost= sum(Cost_estimate_per_year_2017_USD_exchange_rate))
h$cost <- sort(h$cost, decreasing = T)
h
h$percentage <- (h$cost*100)/10373077830
h
view(h)
table(expanded_r$Type_of_cost_merged)


sum(3396915943,99035617,47808)

(47808*100)/3495999368



####################################

#Subsetting
expanded_obs <- expanded[expanded$Implementation %in% c("Observed"),]
sum(expanded_obs$cost_bil)

global.raw.all <- summarizeCosts(expanded,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1960,
                                 maximum.year = 2020,
                                 year.breaks = seq(1960, 2020, by = 10))    #you may have to use other intervals, but 10 is like the common basis
global.raw.obs <- summarizeCosts(expanded_obs,
                                 cost.column = "cost",
                                 in.millions = FALSE,
                                 minimum.year = 1960,
                                 maximum.year = 2020,
                                 year.breaks = seq(1960, 2020, by = 10))



global.raw.obs$average.cost.per.period$middle.years <- global.raw.obs$average.cost.per.period$initial_year +
  (global.raw.obs$average.cost.per.period$final_year -
     global.raw.obs$average.cost.per.period$initial_year) / 2
global.raw.all$average.cost.per.period$middle.years <- global.raw.all$average.cost.per.period$initial_year +
  (global.raw.all$average.cost.per.period$final_year -
     global.raw.all$average.cost.per.period$initial_year) / 2
all.costs <- rbind(data.frame(global.raw.obs$average.cost.per.period,
                              cost.type = "Observed"),
                   data.frame(global.raw.all$average.cost.per.period,
                              cost.type = "All"))
all.costs.per.year <- rbind(data.frame(global.raw.obs$cost.per.year,
                                       cost.type = "Observed"),
                            data.frame(global.raw.all$cost.per.year,
                                       cost.type = "All"))
svg("fish_cum_all_vs_obs.svg",width=15,height=8)
p2<- ggplot(all.costs) +
  ylab(paste0("Annual cost in US$")) +
  # Points
  geom_point(aes_string(x = "middle.years",
                        y = "annual_cost",
                        col = "cost.type"),
             shape = 15) +
  # Lines between points
  geom_line(aes_string(x = "middle.years",
                       y = "annual_cost",
                       col = "cost.type"),
            linetype = 2) +
  # Horizontal bars (year span)
  geom_segment(aes_string(x = "initial_year",
                          xend = "final_year",
                          y = "annual_cost",
                          yend = "annual_cost",
                          col = "cost.type")) +
  geom_point(data = all.costs.per.year,
             aes(x = year, y = cost,
                 size = number_estimates,
                 col = cost.type),
             alpha = .6) +
  xlab("Year") +
  scale_x_continuous(breaks = global.raw.obs$year.breaks) +
  scale_size_continuous(name = "Number of estimates\nper year",
                        breaks = c(1, 5, 20, 50)) +
  scale_color_brewer(name = "Cost estimations",
                     palette = "Dark2") + # Minimal theme
  scale_y_log10(breaks = 10^(-15:15), # y axis in log 10 with pretty labels
                labels = scales::comma) +
  annotation_logticks(sides = "l")+
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))
p2


p1+p2
