library(dplyr)
library(ggplot2)
#opens the files and puts them into a data fram
od <- read.csv("~/Side Project/VSRR_Provisional_Drug_Overdose_Death_Counts.csv")

# selects 4 colums and filters out number of deaths that are not accoiated with overdose
# Gets the total OD deaths and the drug that caused in for the whole countery for every year 

df <- od %>%
  select('State','Year','Month','Indicator', Death ="Data.Value") %>%
  filter(Indicator != "Number of Deaths" & Indicator != "Number of Drug Overdose Deaths") %>%
  filter(State != 'US')%>% # removes states that contain US, these numbers are sum of over does 
  group_by(Year,Indicator,State) %>%
  summarise(totalDeath = sum(Death))


# puts all data from DMV area into a dataframe 
DMV <- filter(df, State == "MD" | State == "DC" | State == 'VA' )


# filters so only data from MD will show in the varibale mdSum 
mdSum <- df %>% 
        filter(State == 'MD')

ggplot(mdSum, aes(x = Year, y = totalDeath)) +
  geom_point() +
  scale_x_log10() + 
  facet_wrap(~ Indicator) 


write.csv(df,"~/Side Project\\ODSum.csv", row.names = FALSE )
