library(tidyverse)
library(XML)

# ------------------
# Get Players
# ------------------
#Players, used to get gender of match
player_url <- 'http://www.fivb.org/vis2009/XmlRequest.asmx?Request=%3CRequests%3E%3CRequest%20Type=%27GetPlayerList%27%20No=%27100002%27%20Fields=%27FederationCode%20Gender%20Birthdate%20FirstName%20Gender%20LastName%20Nationality%20PlaysBeach%20PlaysVolley%20ActiveBeach%20ActiveVolley%20Height%20BeachPosition%20PlayerStatus%20PopularName%20TeamName%27%20/%3E%3C/Requests%3E'
player_xml <- XML::xmlParse(player_url)
player_df <- XML:::xmlAttrsToDataFrame(XML::getNodeSet(player_xml, path ='//Player'))
#Clean player data
ply_df <- player_df %>% 
  mutate(Gender = ifelse(Gender == '1', 'W', 'M')) %>% 
  select(No, Gender)

# ------------------
# Get Matches
# ------------------
#Matches
match_url <- "http://www.fivb.org/vis2009/XmlRequest.asmx?Request=<Requests> <Request Type= 'GetBeachMatchList' Fields='NoPlayerA1 NoTournament TeamAFederationCode TeamBFederationCode LocalDate LocalTime TeamAName TeamBName Court Referee1FederationCode NoReferee1  Referee1Name Referee2FederationCode NoReferee2 Referee2Name RefereeChallengeFederationCode NoRefereeChallenge RefereeChallengeName MatchPointsA MatchPointsB DurationSet1 DurationSet2 DurationSet3 RoundName TournamentCode TournamentTitle TournamentType'></Request></Requests>"
match_xml <- XML::xmlParse(match_url, encoding="UTF-8")
match_dfs <- XML:::xmlAttrsToDataFrame(XML::getNodeSet(match_xml, path ='//BeachMatch'))
#Clean the match data
match_df <- match_dfs %>% 
  filter(NoReferee1 != '') %>% 
  mutate(LocalDate = as.Date(LocalDate),
         Court = as.integer(Court),
         Hour = as.integer(substr(LocalTime, 1, 2)),
         Year = as.integer(substr(LocalDate,1,4)),
         NoReferee1 = as.integer(NoReferee1),
         NoReferee2 = as.integer(NoReferee2),
         NoRefereeChallenge = as.integer(NoRefereeChallenge),
         DurationSet1 = as.integer(DurationSet1),
         DurationSet2 = as.integer(DurationSet2),
         DurationSet3 = as.integer(DurationSet3)) %>% 
  pivot_longer(c('NoReferee1', 'NoReferee2', 'NoRefereeChallenge'), names_to = 'RefereePosition', values_to = 'RefereeNo') %>% 
  select(NoPlayerA1,NoTournament, LocalDate, Court, RoundName, TournamentCode, TournamentTitle, TournamentType, No, Hour:RefereeNo) %>% 
  na.omit()

# ------------------
# Get Refs
# ------------------
#Refs
ref_url <- "http://www.fivb.org/vis2009/XmlRequest.asmx?Request=<Requests> <Request Type= 'GetRefereeList' Fields='RefsBeach Volley No Gender FirstName LastName Birthdate FederationCode Nationality ConfederationCode CountryCode '></Request></Requests>"
ref_xml <- XML::xmlParse(ref_url, encoding="UTF-8")
ref_df <- XML:::xmlAttrsToDataFrame(XML::getNodeSet(ref_xml, path ='//Referee'))
#Clean the ref data
ref_df <- ref_df %>% 
  mutate(RefGender = ifelse(Gender == '0', 'M', 'W'),
         Birthdate = as.Date(Birthdate),
         No = as.integer(No)) %>% 
  select(-Version, -Gender)


# ------------------
# Join Data
# ------------------
#To change the tournament type name
tourn_type <- data.frame(TournamentType = c('0','1','2','3','4','5','6','7','8','9','10','11','12','13','14','15','16','17','18','19','20','21',
                                            '22','23','24','25','26','27','28','29','30','31','32','33','34','35','36','37','38','39','40','41',
                                            '42','43','44','45','46','47','48','49','50'),
                         Tournament_Type = c('GrandSlam','Open','Challenger','WorldSeries','WorldChamp','OlympicGames','Satellite','ContinentalChamp',
                                             'OtherContinental','Other','Masters','ContinentalCup','ContinentalTour','JuniorWorldChamp','YouthWorldChamp',
                                             'NationalTour','NationalTourU23','NationalTourU21','NationalTourU19','NationalTourU20','NationalTourU17','NationalTourU15',
                                             'ContinentalChampU22','ContinentalChampU20','ContinentalChampU18','WorldChampU23','WorldChampU21','WorldChampU19',
                                             'NationalTourU14','NationalTourU16','NationalTourU18','WorldChampU17','MajorSeries','WorldTourFinals','ZonalTour','Test',
                                             'SnowVolleyball','ContinentalCupFinal','WorldTour5Star','WorldTour4Star','WorldTour3Star','WorldTour2Star','WorldTour1Star',
                                             'YouthOlympicGames','MultiSports','NationalSnow','NationalTourU22','ContinentalChampU21','ContinentalChampU19','OlympicGamesQualification',
                                             'KingOfTheCourt'))

df <- match_df %>% 
  left_join(tourn_type) %>% 
  left_join(ply_df, by = c('NoPlayerA1' = 'No')) %>% 
  left_join(ref_df, by = c('RefereeNo' = 'No')) %>% 
  mutate(RefereeAge = (as.numeric(as.Date(LocalDate)) - as.numeric(as.Date(Birthdate))) / 365,
         RefereeAge = as.integer(RefereeAge)) %>% 
  select(-TournamentType) %>% 
  filter(!is.na(RefereeAge), Year != 2012)

# ------------------
# Intro EDA
# ------------------
#Number of registered refs per year
one <- df %>% 
  filter(Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries','WorldTourFinals',
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'), Gender == 'M') %>% 
  distinct(RefereeNo, RefGender, Year) %>% 
  count(RefGender, Year) %>% 
  rename(NumberOfRegisteredRefs = n) %>% 
  ggplot(aes(Year, NumberOfRegisteredRefs)) + 
  geom_line(aes(colour = RefGender), size = 2) +
  theme_bw()+
  labs(title = "Registered Refs for Men's Matches 2014-2020")

# Number of new registered refs per year
two <- df %>% 
  filter(Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries','WorldTourFinals',
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'), Gender == 'M') %>% 
  group_by(RefGender, RefereeNo) %>% 
  summarise(first_year = min(Year)) %>% 
  ungroup() %>% 
  left_join(df, by = 'RefereeNo') %>% 
  distinct(RefereeNo, RefGender.x, first_year) %>% 
  count(first_year, RefGender.x) %>% 
  rename(Year = first_year, NumberOfNewRegisteredRefs = n, RefGender = RefGender.x) %>% 
  ggplot(aes(Year, NumberOfNewRegisteredRefs)) + 
  geom_line(aes(colour = RefGender), size = 2) +
  theme_bw() +
  labs(title = "Number of new registered Refs for Men's Matches 2014-2020",
  caption = "Events Considered:\n Open, Grand Slam, Major Series, World Champ, World Tour Finals, World Tour 4 Star, World Tour 5 Star 2014-2020") +
  theme(plot.caption = element_text(face = 'italic'))

grid.arrange(one, two)

# ------------------
# All time refs 
# ------------------
# All
df %>% 
  filter(Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries','WorldTourFinals',
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'), Gender == 'M') %>% 
  mutate(id = row_number()) %>% 
  distinct(RefGender, id) %>% 
  count(RefGender) %>% 
  rename(`Number of Reffed Matches` = n) %>% 
  ggplot(aes(RefGender, `Number of Reffed Matches`)) + 
  geom_col(aes(fill = RefGender),position = 'dodge') + 
  geom_label(aes(label = `Number of Reffed Matches`)) +
  labs(title = "Number of top level men's matches Refereed by Gender 2014-2020",
       caption = "Events Considered:\n Open, Grand Slam, Major Series, World Champ, World Tour Finals, World Tour 4 Star, World Tour 5 Star 2014-2020") +
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(face = 'italic'))

# All time refs past pool play
df %>% 
  filter(Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries','WorldTourFinals',
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'), Gender == 'M',
         !grepl('quota', RoundName), !grepl('Pool', RoundName), !grepl('Qualification', RoundName)) %>% 
  mutate(id = row_number()) %>% 
  distinct(RefGender, id) %>% 
  count(RefGender) %>% 
  rename(`Number of Reffed Matches` = n) %>% 
  ggplot(aes(RefGender, `Number of Reffed Matches`)) + 
  geom_col(aes(fill = RefGender),position = 'dodge') + 
  geom_label(aes(label = `Number of Reffed Matches`)) +
  labs(title = "Number of top level men's matches post pool play Refereed by Gender 2014-2020",
       caption = "Events Considered:\n Open, Grand Slam, Major Series, World Champ, World Tour Finals, World Tour 4 Star, World Tour 5 Star 2014-2020") +
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(face = 'italic'))

# ------------------
# Referee Position
# ------------------
# Ref position by year All Matches
df %>% 
  filter(Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries','WorldTourFinals',
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'), Gender == 'M') %>% 
  group_by(RefGender,RefereePosition, Year) %>% 
  count() %>% 
  rename(`Number of Reffed Matches` = n) %>% 
  ggplot(aes(RefereePosition, `Number of Reffed Matches`)) + 
  geom_col(aes(fill = RefGender), position = 'dodge') + 
  facet_grid(~Year) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggrepel::geom_text_repel(aes(label = `Number of Reffed Matches`))+
  labs(title = "Number of top level men's matches by Referee Position 2014-2020",
       caption = "Events Considered:\n Open, Grand Slam, Major Series, World Champ, World Tour Finals, World Tour 4 Star, World Tour 5 Star 2014-2020") +
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(face = 'italic'))


# Past pool play
df %>% 
  filter(Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries','WorldTourFinals',
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'), Gender == 'M',
         !grepl('quota', RoundName), !grepl('Pool', RoundName), !grepl('Qualification', RoundName)) %>% 
  group_by(RefGender,RefereePosition, Year) %>% 
  count() %>% 
  rename(`Number of Reffed Matches` = n) %>% 
  ggplot(aes(RefereePosition, `Number of Reffed Matches`)) + 
  geom_col(aes(fill = RefGender), position = 'dodge') + 
  facet_grid(~Year) + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggrepel::geom_text_repel(aes(label = `Number of Reffed Matches`))+
  labs(title = "Number of top level men's matches post pool play by Referee Position 2014-2020",
       caption = "Events Considered:\n Open, Grand Slam, Major Series, World Champ, World Tour Finals, World Tour 4 Star, World Tour 5 Star 2014-2020") +
  theme(axis.title.x = element_blank(),
        plot.caption = element_text(face = 'italic'))

# ------------------
# Women's No1 ref by all types of tournaments for mens
# ------------------
# By tournament
ggplot(aes(n, reorder(Tournament_Type, -n)))
df %>% 
  group_by(Gender, RefGender, Tournament_Type) %>% 
  count() %>% 
  filter(Gender == 'M' & RefGender == 'W') %>% 
  ggplot(aes(n, reorder(Tournament_Type, -n), fill = Tournament_Type)) + 
  ggrepel::geom_text_repel(aes(label = n), direction = 'x') +
  geom_bar(stat = 'identity', show.legend = F, alpha = 0.5) + 
  theme_bw() +
  labs(title = "Women Referees working Male matches",
       caption = "2014-2020")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(face = 'italic'))


# In total
df %>% 
  filter(Gender == 'M' & RefGender == 'W') %>% 
  count(Year) %>% 
  ggplot(aes(Year, n)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title = "Women Referees working Male matches by year",
       caption = "Data from: https://www.fivb.org/VisSDK \n2014-2020")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(face = 'italic'))


# The highest level of men's
df %>% 
  filter(Gender == 'M' & RefGender == 'W', 
         Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries', 'WorldTourFinals', 
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp')) %>% 
  group_by(Year, Tournament_Type) %>% 
  count() %>% 
  ggplot(aes(Year, n))+
  geom_bar(aes(fill = Tournament_Type), stat = 'identity') + 
  theme_bw() +
  labs(title = "Women Referees working Male matches \nby year and Tournament Type",
       caption = "Data from: https://www.fivb.org/VisSDK \n2014-2020")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(face = 'italic'))



# ------------------
# Tournament Type friendly for women refs for top level mens play
# ------------------
df %>% 
  filter(Gender == 'M' & RefGender == 'W', 
         Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries', 'WorldTourFinals', 
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp')) %>% 
  group_by(Year, Tournament_Type) %>% 
  count() %>% 
  ggplot(aes(Year, n)) + 
  geom_col(aes(fill = Tournament_Type), position = 'dodge') + 
  theme_bw() +
  labs(title = "Women Referees working Male matches \nby year and Tournament Type",
       caption = "Data from: https://www.fivb.org/VisSDK \n2014-2020")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(face = 'italic'))


df %>% 
  filter(Gender == 'M' & RefGender == 'W', 
         Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries', 'WorldTourFinals', 
                                'WorldTour5Star', 'WorldTour4Star', 'WorldChamp'),
         LastName != 'GÃ¼Ã§lÃ¼', LastName != 'PistÃ³n Braggio', LastName != 'RÃ¼egg') %>% 
  group_by(Year, Tournament_Type, FirstName, LastName) %>% 
  count() %>% 
  ggplot(aes(Year, n)) + 
  geom_col(aes(fill = LastName), show.legend = F, alpha = 0.5) + 
  facet_wrap(~LastName, scales = 'free_y') + 
  geom_smooth(se = F, method = 'glm', aes(color = LastName), show.legend = F) +
  labs(title = "Women's Individual reffing assignments for men's matches by year",
       caption = "Data from: https://www.fivb.org/VisSDK \n2014-2020")+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_text(face = 'italic'))



df %>% 
  filter(RefereePosition == 'NoReferee1' & Gender == 'M' & RefGender == 'W', 
         RoundName %in% c('Semi-finals', 'Final 3rd Place', 'Final 1st Place', 'Semifinals'),
         Tournament_Type %in% c('GrandSlam', 'Open', 'MajorSeries', 'WorldTourFinals', 'WorldTour5Star', 'WorldTour4Star', 'WorldChamp')) %>% 
  group_by(Year,RoundName,Tournament_Type) %>% 
  count() %>% 
  rename(Count = n) %>% 
  DT::datatable(
    rownames = F,
    options = list(
      dom = 't', 
      pageLength = 20),
    caption = "Number of women's referees as NoReferee1 in men's matches for Semifinals, Final 3rd place, Final 1st Place")
