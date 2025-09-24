#Parsa Vetry
#2016 U.S. Election-Poll Analysis

#setup
library("tidyverse")

polls <- read_csv("C:\\Users\\legom\\Downloads\\polls.csv")

#view data
polls

#view item count for each column
apply(X = polls, MARGIN = 2, FUN = unique) %>% 
  lapply(FUN = length) %>% 
  str()

#view value counts for the type column
table(polls$type)

#compare the type column with the poll data columns
polls %>% 
  select(type, state, enddate, rawpoll_clinton, adjpoll_clinton) %>% 
  arrange(state, enddate, type)

#view the unique values for the state column
unique(polls$state)

#use a plot to examine the population column
ggplot(polls, aes(x = population, fill = population)) + 
  geom_bar()

#scatter Plot to display poll_wt data
ggplot(polls, aes(x = poll_wt, y = samplesize, color = grade)) + 
  geom_point(size = 3)

#select the columns for use, rename, and sort
polls <- polls %>% select(type, state, enddate, population, rawpoll_clinton, rawpoll_trump)

polls <- polls %>% rename(Type = type, State = state, EndDate = enddate, Population = population,
                          Clinton = rawpoll_clinton, Trump = rawpoll_trump)

polls <- polls %>% arrange(State, EndDate)

polls

#select only "now-cast" rows and also drop the Type column
polls <- polls %>% 
  filter(Type == "now-cast") %>%
  select(-Type)

#select only rows with "lv" or "rv" in the population column
polls <- polls %>% filter(Population %in% c("lv", "rv"))

#remove rows for congressional districts
polls <- polls %>% filter(!str_detect(State, "CD-"))

#check number of states 
length(unique(polls$State))

#rename the population column and improve its values
polls <- polls %>% 
  mutate(Population = str_replace(Population,"lv", "Likely"), 
                          Population = str_replace(Population, "rv", "Registered")) %>% 
  rename(VoterType = Population)

#convert the EndDate column to the date type
polls <- polls %>%
  mutate(EndDate = as.Date(EndDate, format = "%m/%d/%Y"))

polls

#save the data
saveRDS(polls, file = "C:\\Users\\legom\\Downloads\\polls_clean.rds")

#read the clean data
polls <- readRDS("C:\\Users\\legom\\Downloads\\polls_clean.rds")

#create two gap columns
polls <- polls %>% mutate(Gap = Clinton - Trump)

polls <- polls %>% group_by(State) %>%
  mutate(StateGap = mean(Gap)) %>%
  ungroup()

#create a column for swing states
polls <- polls %>% mutate(
  Swing = ifelse(State != "U.S." & (abs(StateGap) < 7),
                 TRUE, FALSE))

#drop StateGap column
polls <- select(polls, -StateGap)

#long version for data
polls <- pivot_longer(polls, cols = c("Clinton", "Trump"), 
                      names_to = "Candidate", values_to = "Percent")

polls

#save the data
saveRDS(polls, file = "C:\\Users\\legom\\Downloads\\polls_prepared.rds")

#read the prepared data
polls <- readRDS("C:\\Users\\legom\\Downloads\\polls_prepared.rds")

#national polls with a line plot
ggplot(filter(polls, State == "U.S."), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_line() + 
  scale_color_manual(values = c("blue", "red")) + 
  labs(title = "Polls for the U.S.", x = "Date", y = "") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

#with scatterplot and smooth line
ggplot(filter(polls, State == "U.S."), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  scale_color_manual(values = c("blue", "red")) + 
  labs(title = "Polls for the U.S.", x = "Date", y = "") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

#with smooth line
ggplot(filter(polls, State == "U.S."), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("blue", "red")) + 
  labs(title = "Polls for the U.S.", x = "Date", y = "") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

#swing state polls with smooth line
ggplot(polls %>% filter(Swing == TRUE), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("blue", "red")) + 
  labs(title = "Polls for the Swing States", x = "Date", y = "") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")


#likely voters in swing states 3 months prior to election
ggplot(polls %>% filter(Swing == TRUE &
                          VoterType == "Likely" &
                          EndDate > "2016-08-01"), 
       aes(x = EndDate, y = Percent, color = Candidate)) +
  geom_smooth(se = FALSE) +
  scale_color_manual(values = c("blue", "red")) + 
  labs(title = "Likely voters in Swing States", x = "Date", y = "") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom")

#two swing states
states <- c("Arizona", "Wisconsin")

ggplot(polls %>% filter(State %in% states & EndDate > "2016-08-01"), 
       aes(x = EndDate, y = Percent, color = Candidate)) + 
  geom_smooth(se = FALSE) + 
  scale_color_manual(values = c("blue", "red")) + 
  facet_grid(vars(State)) + 
  theme(legend.position = "bottom")

#plot gap data for selected states
start_date <- as.Date("2016-09-01") 

states <- c("Florida", "Michigan", "Ohio") 

ggplot(polls %>% filter(State %in% states & EndDate > start_date), 
       aes(x = EndDate, y = Gap)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  geom_hline(yintercept = 0, size = 1) + 
  facet_grid(vars(State))

#summarize data by voter type and candidate
voter_types <- polls %>% 
  filter(Swing == TRUE) %>% 
  group_by(VoterType, Candidate) %>% 
  summarize(MeanPercent = mean(Percent), SD = sd(Percent))

#plot summarized data
ggplot(voter_types, 
       aes(x = Candidate, y = MeanPercent, fill = VoterType)) + 
  geom_col(position = "dodge") + 
  geom_errorbar(aes(ymin = MeanPercent - SD, ymax = MeanPercent + SD), 
                width = 0.1, position = position_dodge(0.9)) + 
  labs(y = "", x = "")


#mean gap for selected swing states 
#date changed from "2016-11-01" to "2016-10-25" as "2016-11-01" did not give any useful data
#(discussed with fellow students)
states <- c("Arizona", "Florida", "Iowa", "Nevada", "North Carolina", "Ohio", "Pennsylvania", "Wisconsin") 

polls_nov <- polls %>% 
  filter(State %in% states & EndDate > as.Date("2016-10-25")) %>% 
  group_by(State) %>% 
  summarize(MeanStateGap = mean(Gap))

#add advantage column and get absolute value for mean gap
polls_nov <- polls_nov %>% mutate(
  Advantage = ifelse(MeanStateGap >= 0, "Clinton", "Trump"), 
  MeanStateGap = abs(round(MeanStateGap, 3)))

polls_nov

#plot prepared data
ggplot(polls_nov) + 
  geom_col(aes(x = State, y = MeanStateGap, fill = Advantage)) + 
  scale_fill_manual(labels = c("Clinton", "Trump"), 
                    values = c("blue","red")) + 
  labs(title = "Results for Final Week of Election", 
       x = "", y = "Mean Percent") + 
  theme(plot.title = element_text(hjust = 0.5))


#select only rows and columns needed
polls_weekly <- polls %>% filter(Swing == TRUE) %>% 
  select(State, EndDate, Gap) %>% 
  unique()


#create function to put each end date into weekly bin
get_next_sunday <- function(row) { 
  date <- as.Date(row[2]) 
  day_of_week <- as.integer(format(date, "%w")) 
  next_sunday <- ""
  if(day_of_week == 0) { 
    next_sunday <- date 
  } else { 
    next_sunday <- date + (7 - day_of_week) 
  } 
  return(next_sunday) 
}

#use the function to add a column to hold the date bins
polls_weekly <- polls_weekly %>% 
  mutate(Week = apply(polls_weekly, MARGIN = 1, FUN = get_next_sunday), 
         Week = as.Date(Week, origin = "1970-01-01"))

polls_weekly

#mean poll date for each date bin
polls_weekly <- polls_weekly %>% 
  filter(Week > as.Date("2016-09-01")) %>% 
  group_by(State, Week) %>% 
  summarize(MeanGap = mean(Gap)) %>% 
  ungroup()

#add advantage column and get absolute value for the mean gap
polls_weekly <- polls_weekly %>% 
  mutate(Advantage = ifelse(MeanGap >= 0, "Clinton", "Trump"), 
         MeanGap = abs(round(MeanGap, 1)))

polls_weekly

#plot the weekly gap data of the swing states
ggplot(polls_weekly, aes(x = Week, y = State)) + 
  geom_tile(aes(fill = Advantage, color = "", alpha = MeanGap)) + 
  geom_text(aes(label = MeanGap, color = "")) + 
  scale_fill_manual(values = c("blue", "red")) + 
  scale_color_manual(values = "white", guide = "none") + 
  labs(title = "Weekly Gap for the Swing States") + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") + 
  scale_alpha(range = c(0.25,0.95)) + 
  guides(alpha = "none")