library(dplyr)
library(tidyverse)
library(magrittr)
library(reshape2)
library(ggplot2)
library(DataCombine)
library(ggpubr)
library(lubridate)

list.files()
c02_source <- read.csv("CO2-by-source.csv")
c02_sector <- read.csv("carbon-dioxide-co2-emissions-by-sector-or-source.csv")
hdi <- read.csv("HDI.csv",skip = 1)

column_name <- names(hdi)
print(column_name)
hdi <- select(hdi,c("HDI.Rank..2017.","Country" ,"X1990","X1991","X1992","X1993","X1994","X1995","X1996","X1997","X1998",
                        "X1999","X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008","X2009","X2010","X2011",
                        "X2012","X2013","X2014","X2015","X2016", "X2017")) 

names(hdi)[c(-1,-2)]<- c(1990:2017)

upper <- c("Australia","Belgium","Canada","Germany","Iceland","Japan","Netherlands","New Zealand","Norway","Sweden","Switzerland","United States")

middle <- c("Argentina", "Austria", "Bahrain", "Barbados", "Brunei Darussalam", "Bulgaria", "Chile", "Croatia", "Cyprus", "Czechia", "Denmark",
            "Estonia", "Finland", "France", "Greece", "Hong Kong, China (SAR)", "Ireland", "Israel", "Italy", "Korea (Republic of)", "Kuwait",
            "Latvia", "Lithuania", "Luxembourg", "Malaysia", "Malta", "Poland", "Portugal", "Qatar", "Romania", 
            "Russian Federation", "Saudi Arabia", "Singapore", "Slovakia", "Slovenia", "Spain", "United Arab Emirates", "United Kingdom", "Uruguay")
bottom <- c("Albania", "Algeria", "Armenia", "Bangladesh", "Belize", "Benin", "Bolivia (Plurinational State of)", "Botswana",
            "Brazil", "Burundi", "Cambodia", "Cameroon", "Central African Republic", "China", "Colombia", "Congo", "Costa Rica",
            "Cuba", "C?te d'Ivoire", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Eswatini (Kingdom of)", "Fiji", "Gabon",
            "Gambia", "Ghana", "Guatemala", "Guinea", "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran (Islamic Republic of)",
            "Iraq", "Jamaica", "Jordan", "Kenya", "Kyrgyzstan", "Lao People's Democratic Republic", "Lesotho", "Libya", "Malawi", "Mali",
            "Mauritania", "Mauritius", "Mexico", "Moldova (Republic of)", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal",
            "Nicaragua", "Niger", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Rwanda", "Samoa", "Sao Tome and Principe",
            "Senegal", "Serbia", "Sierra Leone", "South Africa", "Sri Lanka", "Sudan", "Tajikistan", "Tanzania (United Republic of)", "Thailand", "Togo",
            "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Uganda", "Ukraine",
            "Venezuela (Bolivarian Republic of)", "Viet Nam", "Yemen", "Zambia", "Zimbabwe")
# 
upper_3rd <- filter(hdi,hdi$Country %in% c(upper))
middle_3rd <- filter(hdi,hdi$Country %in% c(middle))
bottom_3rd <- filter(hdi,hdi$Country %in% c(bottom))

#################### Upper 3rd ########################
upper_3rd <- upper_3rd[,-1]
upper_3rd <- upper_3rd %>% remove_rownames %>% column_to_rownames(var = "Country")
upper_3rd_copy <- upper_3rd
#change column class from factor to numeric
upper_3rd_copy <- upper_3rd_copy %<>% mutate_if(is.factor,as.character) %>% mutate_if(is.character, as.numeric)
#adding country as rownames
rownames(upper_3rd_copy)<- rownames(upper_3rd)
#calculating the means of all columns
upper_3rd_copy[nrow(upper_3rd_copy)+1,]<- colMeans(upper_3rd_copy)
#renaming thelast row appropriately
rownames(upper_3rd_copy)[nrow(upper_3rd_copy)]<- "Average HDI Upper third"
upper_3rd <- upper_3rd_copy


#################Middle 3rd#####################
middle_3rd <- middle_3rd[,-1]
middle_3rd <- middle_3rd %>% remove_rownames %>% column_to_rownames(var = "Country")
middle_3rd_copy <- middle_3rd
middle_3rd_copy <- middle_3rd_copy %<>% mutate_if(is.factor,as.character) %>% mutate_if(is.character, as.numeric)
rownames(middle_3rd_copy)<- rownames(middle_3rd)
middle_3rd_copy[nrow(middle_3rd_copy)+1,]<- colMeans(middle_3rd_copy)
rownames(middle_3rd_copy)[nrow(middle_3rd_copy)]<- "Average HDI Middle third"
middle_3rd <- middle_3rd_copy




################## Bottom 3rd ############
bottom_3rd <- bottom_3rd[,-1]
bottom_3rd <- bottom_3rd %>% remove_rownames %>% column_to_rownames(var = "Country")
bottom_3rd_copy <- bottom_3rd
bottom_3rd_copy <- bottom_3rd_copy %<>% mutate_if(is.factor,as.character) %>% mutate_if(is.character, as.numeric)
rownames(bottom_3rd_copy)<- rownames(bottom_3rd)
bottom_3rd_copy[nrow(bottom_3rd_copy)+1,]<- colMeans(bottom_3rd_copy)
rownames(bottom_3rd_copy)[nrow(bottom_3rd_copy)]<- "Average HDI Bottom third"
bottom_3rd <- bottom_3rd_copy



#######################ploting comparisons ###########################

combined_df <- rbind(upper_3rd[nrow(upper_3rd_copy),], middle_3rd[nrow(middle_3rd),], bottom_3rd[nrow(bottom_3rd),])
combined_df <- combined_df %>% rownames_to_column("Category")
combined_df_melted<- melt(combined_df, id.vars = "Category")
str(combined_df_melted)

combined_df_melted$variable <- as.Date(as.character(combined_df_melted$variable), format = "%Y")
combined_df_melted$variable <- year(combined_df_melted$variable)

ggplot(data = combined_df_melted,
       aes(x = variable, y = value, group = Category, color = Category)) +
  geom_line(size=1)+
  scale_y_continuous(limits = c(0,1))+ xlab("Years") + ylab("Human Development Index")



################## C02 per capita ####################

#Renaming HDI dataframe to match names of Co2
new_hdi <- rbind(upper_3rd[-nrow(upper_3rd),],middle_3rd[-nrow(middle_3rd),],bottom_3rd[-nrow(bottom_3rd),])
new_hdi <- rownames_to_column(new_hdi,var = "countries")
new_hdi$countries

replace <- data.frame (from =c("Brunei Darussalam","Czechia","Hong Kong, China (SAR)","Korea (Republic of)","Russian Federation",
                               "Bolivia (Plurinational State of)",
  "C?te d'Ivoire","Eswatini (Kingdom of)","Iran (Islamic Republic of)","Lao People's Democratic Republic","Moldova (Republic of)",
"Tanzania (United Republic of)","Venezuela (Bolivarian Republic of)", "Viet Nam"),
To =c("Brunei","Czech Republic","Hong Kong","South Korea","Russia","Bolivia","Cote d'Ivoire",
            "Swaziland","Iran","Laos","Moldova","Tanzania","Venezuela","Vietnam"))
new_hdi <- FindReplace(data = new_hdi, Var = "countries",replaceData = replace, from = "from", to = "To", exact = F)

new_hdi$countries[new_hdi$countries == "Hong Kong, China (SAR)"] <- "Hong Kong"
new_hdi$countries[new_hdi$countries == "Korea (Republic of)"] <- "South Korea"
new_hdi$countries[new_hdi$countries == "Bolivia (Plurinational State of)"] <- "Bolivia"
new_hdi$countries[new_hdi$countries == "Eswatini (Kingdom of)"] <- "Swaziland"
new_hdi$countries[new_hdi$countries == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
new_hdi$countries[new_hdi$countries == "Iran (Islamic Republic of)"] <- "Iran"
new_hdi$countries[new_hdi$countries == "Moldova (Republic of)"] <- "Moldova"
new_hdi$countries[new_hdi$countries == "Tanzania (United Republic of)"] <- "Tanzania"


c02_per_capita <- read.csv("co-emissions-per-capita.csv")
str(c02_per_capita)
c02_per_capita$Entity <- as.character(c02_per_capita$Entity)
names(c02_per_capita)[names(c02_per_capita)=="Entity"] <- "countries"
length(unique(c02_per_capita$countries))
co2_countries <- unique(c02_per_capita$countries)
hdi_countries <- new_hdi$countries

setdiff(hdi_countries,co2_countries)#all countries in HDI are in c02

names(c02_per_capita)
c02_per_capita <- select(c02_per_capita, -c(Code))
unique(c02_per_capita$Year)
c02_per_capita <- filter(c02_per_capita, c02_per_capita$Year %in% c(1990:2017))
# names(new_hdi)[-1]<- c(1990:2017)
new_hdi <- melt(new_hdi,id.vars = c("countries"),variable.name = "Year",value.name  = "HDI")
c02_per_capita2 <- filter(c02_per_capita, c02_per_capita$countries%in% c(new_hdi$countries))
hdi_c02_per_capita <- merge(new_hdi,c02_per_capita2,by = c("countries","Year"))

# hdi_c02_per_capita$developed <- ifelse(hdi_c02_per_capita$HDI>=0.8,"developed","not developed")
# hdi_c02_per_capita$developed <- as.factor(hdi_c02_per_capita$developed)
# 
str(hdi_c02_per_capita)

hist(hdi_c02_per_capita$HDI)
class(hdi_c02_per_capita$C02_emmsion_per_capita_tonnes)


shapiro.test(hdi_c02_per_capita$HDI)
shapiro.test(hdi_c02_per_capita$C02_emmsion_per_capita_tonnes)
ggqqplot(hdi_c02_per_capita$HDI )
ggqqplot(hdi_c02_per_capita$C02_emmsion_per_capita_tonnes)


cor.test(hdi_c02_per_capita$HDI,hdi_c02_per_capita$C02_emmsion_per_capita_tonnes,method = "spearman", exact = F)
#correlation shows strong postive relationship between c02 and HDI

hdi_c02_per_capita_combined<- hdi_c02_per_capita %>%
  group_by(countries)%>%
  select(HDI, C02_emmsion_per_capita_tonnes)%>%
  summarise(average_HDI = mean(HDI),average_co2_per_cap = mean(C02_emmsion_per_capita_tonnes))

# hdi_c02_per_capita_combined$developed <- ifelse(hdi_c02_per_capita_combined$average_HDI>=0.8,"developed","not developed")


cor.test(hdi_c02_per_capita_combined$average_HDI,hdi_c02_per_capita_combined$average_co2_per_cap,method = "spearman", exact = F)
#correlation shows strong postive relationship between c02 and HDI

################################# categorising based on HDI#################################################################
upper <- c("Australia","Belgium","Canada","Germany","Iceland","Japan","Netherlands","New Zealand","Norway","Sweden","Switzerland","United States")

middle <- c("Argentina", "Austria", "Bahrain", "Barbados", "Brunei", "Bulgaria", "Chile", "Croatia", "Cyprus", "Czech Republic", "Denmark",
            "Estonia", "Finland", "France", "Greece", "Hong Kong", "Ireland", "Israel", "Italy", "South Korea", "Kuwait",
            "Latvia", "Lithuania", "Luxembourg", "Malaysia", "Malta", "Poland", "Portugal", "Qatar", "Romania", 
            "Russia", "Saudi Arabia", "Singapore", "Slovakia", "Slovenia", "Spain", "United Arab Emirates", "United Kingdom", "Uruguay")

bottom <- c("Albania", "Algeria", "Armenia", "Bangladesh", "Belize", "Benin", "Bolivia", "Botswana",
            "Brazil", "Burundi", "Cambodia", "Cameroon", "Central African Republic", "China", "Colombia", "Congo", "Costa Rica",
            "Cuba", "Cote d'Ivoire", "Dominican Republic", "Ecuador", "Egypt", "El Salvador", "Swaziland", "Fiji", "Gabon",
            "Gambia", "Ghana", "Guatemala", "Guinea", "Guyana", "Haiti", "Honduras", "India", "Indonesia", "Iran",
            "Iraq", "Jamaica", "Jordan", "Kenya", "Kyrgyzstan", "Laos", "Lesotho", "Libya", "Malawi", "Mali",
            "Mauritania", "Mauritius", "Mexico", "Moldova", "Mongolia", "Morocco", "Mozambique", "Myanmar", "Namibia", "Nepal",
            "Nicaragua", "Niger", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "Peru", "Philippines", "Rwanda", "Samoa", "Sao Tome and Principe",
            "Senegal", "Serbia", "Sierra Leone", "South Africa", "Sri Lanka", "Sudan", "Tajikistan", "Tanzania", "Thailand", "Togo",
            "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Uganda", "Ukraine",
            "Venezuela", "Vietnam", "Yemen", "Zambia", "Zimbabwe")


hdi_c02_per_capita_combined$category <- ifelse(hdi_c02_per_capita_combined$countries %in% upper,"upper",
                                      ifelse(hdi_c02_per_capita_combined$countries %in% middle,"middle","lower" ))

str(hdi_c02_per_capita)


#######################plotting HDI and CO2 by HDI categories ##############################################################
unique(hdi_c02_per_capita$category)

hdi_c02_per_capita$category <- ifelse(hdi_c02_per_capita$countries %in% upper,"upper",
                                               ifelse(hdi_c02_per_capita$countries %in% middle,"middle","lower" ))
plot_df <- hdi_c02_per_capita %>% 
  select(category,Year,HDI,C02_emmsion_per_capita_tonnes)%>%
  group_by(category, Year)%>%
  summarise(average_hdi= mean(HDI), average_co2_per_capita= mean(C02_emmsion_per_capita_tonnes))%>% data.frame()

#converting year to year format
plot_df$Year <- as.Date(as.character(plot_df$Year), format = "%Y")
plot_df$Year <- year(plot_df$Year)

#HDI and Average CO2 emmsions for Developed countries
p_upper <- ggplot(plot_df[plot_df$category=="upper",], aes(x= Year))
p_upper <- p_upper + geom_line(aes(y=average_hdi, color= "Average HDI"), size= 1)
p_upper <- p_upper + geom_line(aes(y= average_co2_per_capita/10, color= "Average CO2 Per Capita"),size= 1)
p_upper <- p_upper + scale_y_continuous(sec.axis = sec_axis(~.*10, name= "Average CO2 Cer Capita"))
p_upper <- p_upper + scale_color_manual(values = c("blue","red"))
p_upper <- p_upper + labs(y= "Average HDI",x= "Year",colour= "Paremeter", title = "Relationship between Average HDI and Average CO2 emission Per Capita(UPPER) ")
p_upper <- p_upper+ theme(legend.position = c(0.9,0.9))

#HDI and Average CO2 emmsions for developing countries
p_middle <- ggplot(plot_df[plot_df$category=="middle",], aes(x= Year))
p_middle <- p_middle + geom_line(aes(y=average_hdi, color= "Average HDI"), size= 1)
p_middle <- p_middle + geom_line(aes(y= average_co2_per_capita/10, color= "Average CO2 Per Capita"),size= 1)
p_middle <- p_middle + scale_y_continuous(sec.axis = sec_axis(~.*10, name= "Average CO2 Cer Capita"))
p_middle <- p_middle + scale_color_manual(values = c("blue","red"))
p_middle <- p_middle + labs(y= "Average HDI",x= "Year",colour= "Paremeter", title = "Relationship between Average HDI and Average CO2 emission Per Capita(MIDDLE) ")
p_middle <- p_middle+ theme(legend.position = c(0.9,0.9))


#HDI and Average CO2 emmsions for Underdeveloped countries
p_lower<- ggplot(plot_df[plot_df$category=="lower",], aes(x= Year))
p_lower <- p_lower + geom_line(aes(y=average_hdi, color= "Average HDI"), size= 1)
p_lower <- p_lower + geom_line(aes(y= average_co2_per_capita, color= "Average CO2 Per Capita"),size= 1)
p_lower <- p_lower + scale_y_continuous(sec.axis = sec_axis(~.*1, name= "Average CO2 Cer Capita"))
p_lower <- p_lower + scale_color_manual(values = c("blue","red"))
p_lower <- p_lower + labs(y= "Average HDI",x= "Year",colour= "Paremeter",title = "Relationship between Average HDI and Average CO2 emission Per Capita(LOWER) ")
p_lower <- p_lower+ theme(legend.position = c(0.9,0.6))


ggarrange(p_upper, p_lower + rremove("x.text"), 
          ncol = 1, nrow = 2)

#finding correlation based on diferent categories (middle, lower, upper) of c02 and HDI
cor.test(hdi_c02_per_capita[hdi_c02_per_capita$category=="lower",]$HDI,
         hdi_c02_per_capita[hdi_c02_per_capita$category=="lower",]$C02_emmsion_per_capita_tonnes,method = "spearman",exact = F)

#conclusion from correlation:as HDI increases, co2 increases for lower but the average co2 is low compared to the upper and middle

cor.test(hdi_c02_per_capita[hdi_c02_per_capita$category=="middle",]$HDI,
         hdi_c02_per_capita[hdi_c02_per_capita$category=="middle",]$C02_emmsion_per_capita_tonnes,method = "spearman",exact = F)
#in middle, the correlation is weak(pos)

cor.test(hdi_c02_per_capita[hdi_c02_per_capita$category=="upper",]$HDI,
         hdi_c02_per_capita[hdi_c02_per_capita$category=="upper",]$C02_emmsion_per_capita_tonnes,method = "spearman",exact = F)
#the upper HDI and CO2 are weak and negatively correlated

#stongest correlation between HDI and Average CO2 is found in the lower categories of countries
#the plot shows increase in the HDI and Average CO2 emmsion over time for lower category  countries,
#the tonnes of co2 emmited though islow when compared to the average emmsion of the middle and upper categories of nations
#combined the 


####################regression#########################################################

#Based on year
hdi_c02_per_capita$category <- as.factor(hdi_c02_per_capita$category)
baseline <- filter(hdi_c02_per_capita, Year==1990)
baseline_model <-lm(C02_emmsion_per_capita_tonnes ~HDI+category, data = baseline)
summary(baseline_model)

y2000 <- filter(hdi_c02_per_capita,Year==2000)
y2000_model <-lm(C02_emmsion_per_capita_tonnes ~HDI+category, data = y2000)
summary(y2000_model)

y2010 <- filter(hdi_c02_per_capita,Year==2010)
y2010_model <-lm(C02_emmsion_per_capita_tonnes ~HDI+category, data = y2010)
summary(y2010_model)

y2017 <- filter(hdi_c02_per_capita,Year==2017)
y2017_model <-lm(C02_emmsion_per_capita_tonnes ~HDI+category, data = y2017)
summary(y2017_model)

##################Plot C02 per capita by HDI####################
plot(C02_emmsion_per_capita_tonnes ~ category, data=hdi_c02_per_capita, main="Average CO2 per capita emmsion grouped by HDI categories from 1990-2017")

par(mfrow=c(2,2))
plot(C02_emmsion_per_capita_tonnes ~ category, data=baseline, main="CO2 per capita by HDI categories in 1990")
plot(C02_emmsion_per_capita_tonnes ~ category, data=y2000, main="CO2 per capita by HDI categories in 2000")
plot(C02_emmsion_per_capita_tonnes ~ category, data=y2010, main="CO2 per capita by HDI categories in 2010")
plot(C02_emmsion_per_capita_tonnes ~ category, data=y2017, main="CO2 per capita by HDI categories in 2017")


#ploting average hDI and average CO2 with countries label (combined years)
ggscatter(hdi_c02_per_capita_combined, x = "average_HDI", y = "average_co2_per_cap",
          color = "category", palette = "jco",
          size = "average_co2_per_cap", 
          label = "countries", repel = TRUE,
          label.select = list(criteria = "`y` > 15"))+
  labs(x= 'Average HDI', y = "Average C02 Per")





####################################### C02 emmision Rate sector by sector ############################################ 

names(c02_sector)<- c("countries","code","Year","Transport","Electricity_and_Heat","Manufacturing_construction",
                      "other_sectors","residential_buildings_commercial_public_services")
c02_sector$countries <- as.character(c02_sector$countries)

###############################################Upper

c02_sector_upper <- filter(c02_sector, c02_sector$countries %in% c(upper))
length(unique(c02_sector_upper$countries))

c02_sector_upper1 <- c02_sector_upper %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_transport= mean(Transport,na.rm = T),avg_Elect_Heat =mean(Electricity_and_Heat,na.rm = T),
            avg_Manu_cons=mean(Manufacturing_construction,na.rm = T),avg_other_sect= mean(other_sectors,na.rm = T),
            avg_resBuild_comm_pubSer= mean(residential_buildings_commercial_public_services,na.rm = T))
c02_sector_upper2<- gather(c02_sector_upper1,"sector","c02_values",-countries)%>% select(-countries)
c02_sector_upper2$sector <-as.factor(c02_sector_upper2$sector)

group_by(c02_sector_upper2, sector) %>%
  summarise(
    mean = mean(c02_values, na.rm = TRUE),
    sd = sd(c02_values, na.rm = TRUE)
  )


ggboxplot(c02_sector_upper2, x = "sector", y = "c02_values", 
          color = "sector", palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          ylab = "Weight", xlab = "Treatment")

AN_c02_sector_upper <- aov(c02_values ~ sector, data = c02_sector_upper2)
summary(AN_c02_sector_upper)
TukeyHSD(AN_c02_sector_upper)

############################################## Middle

c02_sector_middle <- filter(c02_sector, c02_sector$countries %in% c(middle))
length(unique(c02_sector_middle$countries))

c02_sector_middle1 <- c02_sector_middle %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_transport= mean(Transport,na.rm = T),avg_Elect_Heat =mean(Electricity_and_Heat,na.rm = T),
            avg_Manu_cons=mean(Manufacturing_construction,na.rm = T),avg_other_sect= mean(other_sectors,na.rm = T),
            avg_resBuild_comm_pubSer= mean(residential_buildings_commercial_public_services,na.rm = T))

c02_sector_middle2<- gather(c02_sector_middle1,"sector","c02_values",-countries)%>% select(-countries)
c02_sector_middle2$sector <-as.factor(c02_sector_middle2$sector)

group_by(c02_sector_middle2, sector) %>%
  summarise(
    mean = mean(c02_values, na.rm = TRUE),
    sd = sd(c02_values, na.rm = TRUE)
  )


ggboxplot(c02_sector_middle2, x = "sector", y = "c02_values", 
          color = "sector", palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          ylab = "Weight", xlab = "Treatment")

AN_c02_sector_middle <- aov(c02_values ~ sector, data = c02_sector_middle2)
summary(AN_c02_sector_middle)
TukeyHSD(AN_c02_sector_middle)

#############################bottom

c02_sector_lower <- filter(c02_sector, c02_sector$countries %in% c(bottom))
length(unique(c02_sector_lower$countries))

c02_sector_lower1 <- c02_sector_lower %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_transport= mean(Transport,na.rm = T),avg_Elect_Heat =mean(Electricity_and_Heat,na.rm = T),
            avg_Manu_cons=mean(Manufacturing_construction,na.rm = T),avg_other_sect= mean(other_sectors,na.rm = T),
            avg_resBuild_comm_pubSer= mean(residential_buildings_commercial_public_services,na.rm = T))

c02_sector_lower2<- gather(c02_sector_lower1,"sector","c02_values",-countries)%>% select(-countries)
c02_sector_lower2$sector <-as.factor(c02_sector_lower2$sector)

group_by(c02_sector_lower2, sector) %>%
  summarise(
    mean = mean(c02_values, na.rm = TRUE),
    sd = sd(c02_values, na.rm = TRUE)
  )


ggboxplot(c02_sector_lower2, x = "sector", y = "c02_values", 
          color = "sector", palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          ylab = "Weight", xlab = "Treatment")

AN_c02_sector_lower<- aov(c02_values ~ sector, data = c02_sector_lower2)
summary(AN_c02_sector_lower)
TukeyHSD(AN_c02_sector_lower)


######################c02 by source (in TOnnes)##########################################

######################### Upper
names(c02_source)<- c("countries","code","Year","Cement","Flaring","Oil",
                      "coal","Gas")
c02_source$countries <- as.character(c02_source$countries)
c02_source_upper1<- filter(c02_source, c02_source$countries %in% c(upper))
length(unique(c02_source_upper1$countries))

c02_source_upper2 <- c02_source_upper1 %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_Cement= mean(Cement,na.rm = T),avg_Flaring =mean(Flaring,na.rm = T),avg_Oil=mean(Oil,na.rm = T),
            avg_coal= mean(coal,na.rm = T), avg_Gas= mean(Gas,na.rm = T))

c02_source_upper3<- gather(c02_source_upper2,"sector","c02_values",-countries)%>% select(-countries)
c02_source_upper3$sector <-as.factor(c02_source_upper3$sector)

group_by(c02_source_upper3, sector) %>%
  summarise(
    mean = mean(c02_values, na.rm = TRUE),
    sd = sd(c02_values, na.rm = TRUE)
  )


ggboxplot(c02_source_upper3, x = "sector", y = "c02_values", 
          color = "sector", palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          ylab = "Weight", xlab = "Treatment")+
  scale_y_log10()

KW_c02_source_upper3 <- kruskal.test(c02_values ~ sector, data = c02_source_upper3)
pairwise.wilcox.test(c02_source_upper3$c02_values, c02_source_upper3$sector,
                     p.adjust.method = "bonferroni")

###################################################MIddle 

c02_source_middle1<- filter(c02_source, c02_source$countries %in% c(middle))
length(unique(c02_source_middle1$countries))

c02_source_middle2 <- c02_source_middle1 %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_Cement= mean(Cement,na.rm = T),avg_Flaring =mean(Flaring,na.rm = T),avg_Oil=mean(Oil,na.rm = T),
            avg_coal= mean(coal,na.rm = T), avg_Gas= mean(Gas,na.rm = T))

c02_source_middle3<- gather(c02_source_middle2,"sector","c02_values",-countries)%>% select(-countries)
c02_source_middle3$sector <-as.factor(c02_source_middle3$sector)

group_by(c02_source_middle3, sector) %>%
  summarise(
    mean = mean(c02_values, na.rm = TRUE),
    sd = sd(c02_values, na.rm = TRUE)
  )


ggboxplot(c02_source_middle3, x = "sector", y = "c02_values", 
          color = "sector", palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          ylab = "Weight", xlab = "Treatment")+
  scale_y_log10()

KW_c02_source_middle3 <- kruskal.test(c02_values ~ sector, data = c02_source_middle3)
pairwise.wilcox.test(c02_source_middle3$c02_values, c02_source_middle3$sector,
                     p.adjust.method = "bonferroni")

##########################################Lower

c02_source_lower1<- filter(c02_source, c02_source$countries %in% c(bottom))
length(unique(c02_source_lower1$countries))

c02_source_lower2 <- c02_source_lower1 %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_Cement= mean(Cement,na.rm = T),avg_Flaring =mean(Flaring,na.rm = T),avg_Oil=mean(Oil,na.rm = T),
            avg_coal= mean(coal,na.rm = T), avg_Gas= mean(Gas,na.rm = T))

c02_source_lower3<- gather(c02_source_lower2,"sector","c02_values",-countries)%>% select(-countries)
c02_source_lower3$sector <-as.factor(c02_source_lower3$sector)

group_by(c02_source_lower3, sector) %>%
  summarise(
    mean = mean(c02_values, na.rm = TRUE),
    sd = sd(c02_values, na.rm = TRUE)
  )


ggboxplot(c02_source_lower3, x = "sector", y = "c02_values", 
          color = "sector", palette = c("#00AFBB", "#E7B800", "#FC4E07","red","blue"),
          ylab = "Weight", xlab = "Treatment")+
  scale_y_log10()

KW_c02_source_lower3 <- kruskal.test(c02_values ~ sector, data = c02_source_lower3)
pairwise.wilcox.test(c02_source_lower3$c02_values, c02_source_lower3$sector,
                     p.adjust.method = "bonferroni") #The false discovery rate





################################################# Regression model##############################################

#checking total tonnes by source

total_c02<- read.csv("annual-co2-emissions-per-country.csv")
str(total_c02)
names(total_c02)<- c("countries","code","Year","total_C02_tonnes")
total_c02<-filter(total_c02, countries %in% c(upper,middle,bottom))


total_c02 <- total_c02%>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(Avg_total_C02_tonnes=mean(total_C02_tonnes))

c02_source3<- c02_source %>%
  select(-c(code,Year))%>%
  group_by(countries)%>%
  summarise(avg_Cement= mean(Cement,na.rm = T),avg_Flaring =mean(Flaring,na.rm = T),avg_Oil=mean(Oil,na.rm = T),
            avg_coal= mean(coal,na.rm = T), avg_Gas= mean(Gas,na.rm = T))
avg_source <- merge(total_c02,c02_source3, by = "countries")

lm_model2 <- lm(Avg_total_C02_tonnes ~ avg_Cement+ avg_Flaring+ avg_Oil+ avg_coal+ avg_Gas,
                data =avg_source)
summary(lm_model2)


