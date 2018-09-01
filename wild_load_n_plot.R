# This is an R script written by Matthew morriss on June 29, 2018.  The question is whether or not
# the rate of wilderness area designation has changed over time, specifically if it has slowed.

#################################### Set dir and import libraries ####################################
library(tidyverse)
library(ggplot2)
# library(xlsx)
library(ggalt)
library(scales)
library(gridExtra)
library(rvest)
library(plyr)
library(stringr)
library(textclean)
library(XML)
library(lubridate)
library(htmltab)
library(viridis)
library(formattable)
library(htmltools)
library(webshot)

setwd('/Users/matthew/Documents/GitHub/wilderness')

#################################### Scrape wikipedia page ####################################

webpage <- "https://en.wikipedia.org/wiki/List_of_U.S._Wilderness_Areas"

tbls <-  htmltab("http://en.wikipedia.org/wiki/List_of_U.S._Wilderness_Areas",1)


plURl <- "https://ballotpedia.org/Federal_land_ownership_by_state#Federal_land_by_state"
pl_tbl <- htmltab('https://ballotpedia.org/Federal_land_ownership_by_state#Federal_land_by_state',2)

rm(plURl)
rm(webpage)

#################################### Clean Table ####################################
# Remove commas from area
tbls$`Size >> acres` <- (gsub(",","",tbls$`Size >> acres`))
tbls$`Size >> ha` <- (gsub(",","",tbls$`Size >> ha`))

tbls$`Size >> acres` <- as.numeric(tbls$`Size >> acres`)
tbls$`Size >> ha` <-  as.numeric(tbls$`Size >> ha`)

pl_tbl$`Federal land ownership by state (as of 2013) >> Percentage of federal land` <- (gsub("%","",pl_tbl$`Federal land ownership by state (as of 2013) >> Percentage of federal land`))
pl_tbl$`Federal land ownership by state (as of 2013) >> Federal land acreage` <- (gsub(",","",pl_tbl$`Federal land ownership by state (as of 2013) >> Federal land acreage`))
pl_tbl$`Federal land ownership by state (as of 2013) >> Total state acreage` <- (gsub(",","",pl_tbl$`Federal land ownership by state (as of 2013) >> Total state acreage`))
pl_tbl$`Federal land ownership by state (as of 2013) >> Federal land acreage` <- as.numeric(pl_tbl$`Federal land ownership by state (as of 2013) >> Federal land acreage`)
pl_tbl$`Federal land ownership by state (as of 2013) >> Total state acreage` <- as.numeric(pl_tbl$`Federal land ownership by state (as of 2013) >> Total state acreage`)
pl_tbl$`Federal land ownership by state (as of 2013) >> Percentage of federal land` <- as.numeric(pl_tbl$`Federal land ownership by state (as of 2013) >> Percentage of federal land`)
pl_tbl <- pl_tbl[-nrow(pl_tbl),] #remove the last row and the total US row
pl_tbl <- pl_tbl[-nrow(pl_tbl),]

##################################### Munging ####################################
#convert dates
tbls$Designated <- mdy(tbls$Designated)
year = year(tbls$Designated)

tbls <- cbind(tbls, year)


lwr48 <- filter(tbls,tbls$State != "AK")
wild_rate <-  data.frame(count(tbls,c('year')))


area_rate <- aggregate(tbls$`Size >> acres`,by = list(tbls$year), sum)
colnames(area_rate) <- c('Year','Area (acre)')

area_rt_lwr48 <- aggregate(lwr48$`Size >> acres`,by = list(lwr48$year), sum)
colnames(area_rt_lwr48) <- c('Year','Area (acre)')

stateArea <- aggregate(lwr48$`Size >> acres`,by = list(lwr48$State),sum)
colnames(stateArea) <- c('State', 'Area (acre)')

stateCount <- data.frame(count(lwr48,c('State')))
colnames(stateCount) <- c('State','Count')
stateCount <-  stateCount[order(-stateCount$Count),]

stateData <- merge(stateArea, stateCount, by = "State")
stateData <-  stateData[order(-stateData$Count),]

colnames(pl_tbl) <- c('State','Federal P.L. (Ac)','Total State Area (Ac)','Percent public')
pl_lwr48 <- filter(pl_tbl, pl_tbl$State != 'Alaska')
pl_lwr48$State <- state.abb[match(pl_lwr48$State,state.name)]
#################################### Figure 1; Format table ########################################
#Write a table that shows the states with the most wilderness areas and 
# the largest area designated as wilderness

widget_formattable <- formattable(stateData,
                 list(Count = color_tile('white','orange'),
                      `Area (acre)` = color_tile('white','green')))
widget_formattable



html_table <- format_table(stateData,
                          list(Count = color_tile('white','orange'),
                               `Area (acre)` = color_tile('white','green')))
html_table

write(paste(html_header, html_table, sep=""), "./Table_1_Wilderness_and_Area.html")

#################################### Export Table ####################################
# # THIS SECTION WORKS!@!!@ WOOHOO!!!


#################################### Export Table 1 ####################################
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- as.htmlwidget(f, width = width, height = height)
  path <- html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}


export_formattable(widget_formattable,"./Table1.png")





stateData <- stateData[order(-stateData$`Area (acre)`),]
widget_formattable <- formattable(stateData,
                                  list(Count = color_tile('white','orange'),
                                       `Area (acre)` = color_tile('white','green')))

widget_formattable
export_formattable(widget_formattable,"./Table1.5.png")
################################ Figure 2: area by year ####################################
fit1 <- glm(lwr48$`Size >> acres` ~ (lwr48$year), family = "poisson")


ggplot(lwr48,aes(x = Designated,
            y = log10(lwr48$`Size >> acres`)))+

    
  geom_point(alpha = 0.25,color = 'blue',size = log10(lwr48$`Size >> acres`))+
  # geom_smooth(method = "glm", col = "red",
  #             fill = 'blue',
  #             method.args = list(family = "poisson"))+
  theme_classic()+
  labs(x = "Year",y = "Size log10(Acres)", title = "Wilderness Designation sizes")+
  theme(plot.title = element_text(hjust = 0.5))


#################################### Figure 3: number per year ####################################
#create a sum of wilderness designations per year
fit1 <- glm(wild_rate$freq ~ (wild_rate$year),family = 'poisson')
  
  ggplot(wild_rate, aes(x = year,
                        y = freq))+
    geom_point()+
    # geom_smooth(method = "lm")+
    geom_smooth(method = "glm", col = "red",
                fill = 'blue',
                method.args = list(family = "poisson"))+
    
    theme_classic()+
    labs(x = 'Year',y = 'Number of Wilderness areas',title = 'Wilderness creation by year')+
    labs(subtitle = paste("Intercept =",signif(fit1$coef[[1]],5 ),
                       " Slope =",signif(fit1$coef[[2]], 5),
                       " P =",signif(summary(fit1)$coef[2,4], 5)))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.subtitle = element_text(hjust = 0.5))
  
  
  
#################################### SECTION TITLE ####################################
  ggplotRegression <- function (fit) {
    
    require(ggplot2)
    
    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point(alpha= 0.5) +
      # geom_hex(bins = 30)+
      # scale_fill_viridis()+
      # scale_fill_brewer(palette="Dark2")+
      # scale_fill_gradient2('Density',high = 'gray37')+
      stat_smooth(method = "glm", col = "red",
                  fill = 'blue',
                  method.args = list(family = "poisson")) +
      labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                         "Intercept =",signif(fit$coef[[1]],5 ),
                         " Slope =",signif(fit$coef[[2]], 5),
                         " P =",signif(summary(fit)$coef[2,4], 5)))+
      labs(x = 'Year',
           y = 'Number of Wildernss Designations')+
      
      theme_classic()
    
  }
  
  fit1 <- glm(wild_rate$freq ~wild_rate$year, family = "poisson")
  
  ggplotRegression(fit1)
  
  
#################################### Area Yearly Rate (Lower 48) ####################################
  fit <- glm(area_rt_lwr48$Year ~ (area_rt_lwr48$`Area (acre)`), family = "poisson")
  
  g1 <- ggplot(area_rt_lwr48, aes(x = area_rt_lwr48$Year,
                                  y = area_rt_lwr48$`Area (acre)`))+
    geom_point()+
    geom_smooth(method = "glm", col = "red",
                fill = 'blue',
                method.args = list(family = "poisson"))+
    theme_classic()+
    labs(x = "Year", y = 'Wilderness area (acres)',title = 'Area of Wilderness Designated')+
    labs(subtitle = paste("Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.subtitle = element_text(hjust = 0.5))
               
  
  g1
  rm(fit, fit1, g1, g2, stateCount, stateArea)
#################################### Cumulative total protected, out of total area ####################################
  #In this section I would like to calculate a summed total of area protected vs remaining federal
  #lands
  
  #combine databases (state wilderness and state federal lands)
  
  x <- anti_join(pl_lwr48, stateData, by = "State")
  y <- anti_join(stateData, pl_lwr48, by = "State")
  
  pl_lwr48 <- pl_lwr48[!(pl_lwr48$State %in% x$State),]
  stateData <- stateData[!(stateData$State %in% y$State),]
  rm(x,y)
  
  State_database <- merge(pl_lwr48,stateData,by = "State")
  
  colnames(State_database)[5] <- "Acre of Wilderness"
  colnames(State_database)[6] <- "No of Wildnerness Areas"
  
  #Now I have the table that I wanted
  #Calculate a few percentages
  # 1) what percentage of public land in each state is wilderness
  # 2) what percentage of state's land is wilderness
  
  State_database <- State_database %>% mutate('Percent of P.L. Wilderness' = round(State_database$`Acre of Wilderness`/State_database$`Federal P.L. (Ac)`,digits = 3))
  
  State_database <- State_database %>% mutate('Percent of State Area t.i. Wilderness' = round(State_database$`Acre of Wilderness`/State_database$`Total State Area (Ac)`,digits = 5))
  
  
#################################### Table 2: Normalized Proportion table ####################################
  State_database <- State_database[order(-State_database$`Acre of Wilderness`),]
State_database$`Percent of P.L. Wilderness` <- percent(State_database$`Percent of P.L. Wilderness`)
State_database$`Percent of State Area t.i. Wilderness` <- percent(State_database$`Percent of State Area t.i. Wilderness`)  


widget_formattable2 <- formattable(State_database,
                                    list(`Acre of Wilderness` = color_tile('white','green'),
                                         `Percent public` = color_tile('white','orange'),
                                          `Percent of P.L. Wilderness`= color_bar('lightgreen'),
                                           `Percent of State Area t.i. Wilderness` = color_bar('lightblue')))
  widget_formattable2
  
  export_formattable(widget_formattable2,"./Table2.png")
  
#################################### Table 3: Sorted % Wilderness ####################################
  State_database <- State_database[order(-State_database$`Percent of P.L. Wilderness`),]
 
  widget_formattable3 <- formattable(State_database,
                                     list(`Acre of Wilderness` = color_tile('white','green'),
                                          `Percent public` = color_tile('white','orange'),
                                          `Percent of P.L. Wilderness`= color_bar('lightgreen'),
                                          `Percent of State Area t.i. Wilderness` = color_bar('lightblue')))
  widget_formattable3
  export_formattable(widget_formattable3,"./Table3.png")
  #################################### Western States ####################################
  
Wstrn_states <- filter(State_database, State_database$State == 'AZ' |
                             State_database$State == 'CA'|
                           State_database$State == 'CO'|
                           State_database$State == 'ID'|
                           State_database$State == 'MT'|
                           State_database$State == 'NV'|
                           State_database$State == 'NM'|
                           State_database$State == 'OR'|
                           State_database$State == 'UT'|
                           State_database$State == 'WA'|
                           State_database$State == 'WY')
  
  stateMedian = median(Wstrn_states$`Acre of Wilderness`)
  stateMean = mean(Wstrn_states$`Acre of Wilderness`)
  
ggplot(Wstrn_states, aes(x = Wstrn_states$State,
                           y = (Wstrn_states$`Acre of Wilderness`)))+
    geom_col(aes(fill = (Wstrn_states$`Acre of Wilderness`)))+
    geom_hline(aes(yintercept = stateMean, 
               
               linetype = 'Mean Area'),
               color = 'blue')+
    geom_hline(aes(yintercept = stateMedian,
               linetype = 'Median Area'),
               color = 'red')+
    scale_linetype_manual(name = "", values = c(2, 2), 
                          guide = guide_legend(override.aes = list(color = c("blue", "red"))))+
    labs(x = 'State',y = 'Wilderness Area (Acre)',
         title = 'Wilderness area in the west by state',
         fill = 'Area (Acre)')
#################################### West no cali ####################################

Wstrn_states_noc <- filter(State_database, State_database$State == 'AZ' |
                         
                         State_database$State == 'CO'|
                         State_database$State == 'ID'|
                         State_database$State == 'MT'|
                         State_database$State == 'NV'|
                         State_database$State == 'NM'|
                         State_database$State == 'OR'|
                         State_database$State == 'UT'|
                         State_database$State == 'WA'|
                         State_database$State == 'WY')

stateMedian = median(Wstrn_states_noc$`Acre of Wilderness`)
stateMean = mean(Wstrn_states_noc$`Acre of Wilderness`)
stateStd = (sd(Wstrn_states_noc$`Acre of Wilderness`))

ggplot(Wstrn_states_noc, aes(x = Wstrn_states_noc$State,
                         y = (Wstrn_states_noc$`Acre of Wilderness`)))+
  geom_col(aes(fill = (Wstrn_states_noc$`Acre of Wilderness`)))+
  geom_hline(aes(yintercept = stateMean, 
                 
                 linetype = 'Mean Area'),
             color = 'blue')+
  geom_hline(aes(yintercept = stateMedian,
                 linetype = 'Median Area'),
             color = 'red')+
  geom_hline(aes(yintercept = (stateMedian + stateStd),
                 linetype = 'Std'),
             color = 'black')+
  geom_hline(aes(yintercept = (stateMedian - stateStd),
                 linetype = 'Std'),
             color = 'black')+
  scale_linetype_manual(name = "", values = c(2, 2,2),
                        guide = guide_legend(override.aes = list(color = c("blue", "red",'black'))))+
  labs(x = 'State',y = 'Wilderness Area (Acre)',
       title = 'Wilderness area in the west by state',
       fill = 'Area (Acre)')

#################################### Western State, prop wildernss ####################################

ggplot(Wstrn_states, aes(x = State,
                         y = 100* Wstrn_states$`Percent of P.L. Wilderness`))+

    geom_col(aes(fill = (100*Wstrn_states$`Percent of P.L. Wilderness`)))+
    labs(x = "State",y = 'Percent of Public land Wilderness (acre)',
         title = 'Western States, % of P.L. that is wilderness',
         fill = '% of P.L. Wilderness')+
  theme_classic()+
  theme(legend.key.height=unit(2, "cm"))

#################################### Ordered plot ####################################
ggplot(Wstrn_states, aes(x = reorder(State, - `Percent of P.L. Wilderness`),
                         y = 100* Wstrn_states$`Percent of P.L. Wilderness`))+
  
  geom_col(aes(fill = (100*Wstrn_states$`Percent of P.L. Wilderness`)))+
  labs(x = "State",y = 'Percent of Public land Wilderness (acre)',
       title = 'Western States, % of P.L. that is wilderness',
       fill = '% of P.L. Wilderness')+
  theme_classic()+
  theme(legend.key.height=unit(2, "cm"))