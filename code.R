#import all packages
library(tidyverse)
library(readxl)
library(janitor)
library(clipr)
library(roll)
library(stringi)
library(rvest)
library(stargazer)

############################IMPORT DATA#############################

############IMPORT GATE REVENUES############

setwd("/Users/jackdolitsky/Documents/Replication Project/statista_excel_data")

file_names <- list.files()

gate_revenue <- lapply(file_names, read_xlsx, sheet = 2, range = "B6:C9",
                       col_names = c("year","revenue"))

team_names <- str_remove_all(string = file_names, pattern = "statistic_id......_gate-receipts-of-the-")
team_names <- str_remove_all(string = team_names, "--nba--2019-20.xlsx")
team_names <- str_replace_all(string = team_names, pattern = "-", " ")
team_names <- str_to_title(string = team_names)

df_gate_revenues <- data.frame()
for (i in 1:length(team_names)) {
  team <- as.character(rep.int(team_names[[i]], nrow(gate_revenue[[i]])))
  gate_revenue[[i]] <- cbind(gate_revenue[[i]], team) #adding column with name of team
  df_gate_revenues <- rbind(df_gate_revenues, gate_revenue[[i]])
}

df_gate_revenues = df_gate_revenues %>% 
  mutate(year = ifelse(
    year == "10/11", 2011,
    ifelse(
      year == "11/12", 2012,
      ifelse(
        year == "12/13", 2013,2014
      )
    )
  ), revenue = revenue * 1000000,
  team = as.character(team)
  
  )

############IMPORT TEAM DATA############

setwd("/Users/jackdolitsky/Documents/Replication Project/team_excel_data")

team_wins_2010 <- read_xlsx("team_data_2010.xlsx", skip = 2) %>% 
  clean_names() %>% 
  select(tm, g_7, w_8, w_19) %>% 
  replace_na(list(w_8 = 0, w_19 = 0)) %>% 
  group_by(tm) %>% 
  summarise(reg_season_g = sum(g_7), reg_season_w = sum(w_8), playoff_w = sum(w_19)) %>% 
  ungroup() %>% 
  mutate(year = 2010)


team_wins_2011 <- read_xlsx("team_data_2011.xlsx", skip = 2) %>% 
  clean_names() %>% 
  select(tm, g_7, w_8, w_19) %>% 
  replace_na(list(w_8 = 0, w_19 = 0)) %>% 
  group_by(tm) %>% 
  summarise(reg_season_g = sum(g_7), reg_season_w = sum(w_8), playoff_w = sum(w_19)) %>% 
  ungroup() %>% 
  mutate(year = 2011)

team_wins_2012 <- read_xlsx("team_data_2012.xlsx", skip = 2) %>% 
  clean_names() %>% 
  select(tm, g_7, w_8, w_19) %>% 
  replace_na(list(w_8 = 0, w_19 = 0)) %>% 
  group_by(tm) %>% 
  summarise(reg_season_g = sum(g_7), reg_season_w = sum(w_8), playoff_w = sum(w_19)) %>% 
  ungroup() %>% 
  mutate(year = 2012)


team_wins_2013 <- read_xlsx("team_data_2013.xlsx", skip = 2) %>% 
  clean_names() %>% 
  select(tm, g_7, w_8, w_19) %>% 
  replace_na(list(w_8 = 0, w_19 = 0)) %>% 
  group_by(tm) %>% 
  summarise(reg_season_g = sum(g_7), reg_season_w = sum(w_8), playoff_w = sum(w_19)) %>% 
  ungroup() %>% 
  mutate(year = 2013)

team_wins_2014 <- read_xlsx("team_data_2014.xlsx", skip = 2) %>% 
  clean_names() %>% 
  select(tm, g_7, w_8, w_19) %>% 
  replace_na(list(w_8 = 0, w_19 = 0)) %>% 
  group_by(tm) %>% 
  summarise(reg_season_g = sum(g_7), reg_season_w = sum(w_8), playoff_w = sum(w_19)) %>% 
  ungroup() %>% 
  mutate(year = 2014)

df_team_wins <- rbind(team_wins_2010, team_wins_2011, team_wins_2012, team_wins_2013, team_wins_2014)


##########IMPORT ATTENDANCE DATA############

setwd("/Users/jackdolitsky/Documents/Replication Project/attendance_excel_data")

attendance_data_2011 <- read_xlsx("attendance_data_2011.xlsx") %>% 
  clean_names() %>% 
  mutate(year = 2011, stadium_cap = round((avg * 100)/pct))

attendance_data_2012 <- read_xlsx("attendance_data_2012.xlsx") %>% 
  clean_names() %>% 
  mutate(year = 2012, stadium_cap = round((avg * 100)/pct))

attendance_data_2013 <- read_xlsx("attendance_data_2013.xlsx") %>% 
  clean_names() %>% 
  mutate(year = 2013, stadium_cap = round((avg * 100)/pct))

attendance_data_2014 <- read_xlsx("attendance_data_2014.xlsx") %>% 
  clean_names() %>% 
  mutate(year = 2014, stadium_cap = round((avg * 100)/pct))

attendance_df = rbind(attendance_data_2011, attendance_data_2012,
                      attendance_data_2013, attendance_data_2014)

##########IMPORT CHAMP DATA############

setwd("/Users/jackdolitsky/Documents/Replication Project/")

winner_data <- read_csv("csv_championships.csv", skip = 1) %>% 
  clean_names() %>% 
  select(year, champion, runner_up) %>% 
  filter(year > 1990, year < 2015) %>% 
  mutate(points_2014 = year - 1993, points_2013 = year - 1992, 
         points_2012 = year - 1991, points_2011 = year - 1990)
champ_data = winner_data %>% group_by(champion) %>% 
  summarise(`2014` = sum(points_2014[year < 2014 & year > 1993]),
            `2013` = sum(points_2013[year < 2013 & year > 1992]),
            `2012` = sum(points_2012[year < 2012 & year > 1991]),
            `2011` = sum(points_2011[year < 2011 & year > 1990])) %>% 
  pivot_longer(`2014`:`2011`, names_to = "year", values_to = "WCHM20")

runner_up_data = winner_data %>% group_by(runner_up) %>% 
  summarise(`2014` = sum(points_2014[year < 2014 & year > 1993]),
            `2013` = sum(points_2013[year < 2013 & year > 1992]),
            `2012` = sum(points_2012[year < 2012 & year > 1991]),
            `2011` = sum(points_2011[year < 2011 & year > 1990])) %>% 
  pivot_longer(`2014`:`2011`, names_to = "year", values_to = "RCHM20")

##########IMPORT ROSTER DATA############

# setwd("/Users/jackdolitsky/Documents/Replication Project")
# abbreviations <- read_xlsx("abbreviations.xlsx")
# abbreviations <- mutate(abbreviations, `2010` = `2011`)
# 
# 
# all_tables <- read_html("https://www.basketball-reference.com/teams/BOS/2011.html") %>%
#   html_nodes("table") %>%
#   html_table()
# advanced_table <- all_tables[[3]] %>%
#   clean_names() %>%
#   select(name = x, g, mp, ws, bpm, vorp)
# 
# all_df <- list()
# 
# count = 0
# for (i in 2010:2014) {
#   for(j in ifelse(rep.int(i, nrow(abbreviations)) == 2010, abbreviations$`2010`,
#       ifelse(rep.int(i, nrow(abbreviations)) == 2011, abbreviations$`2011`,
#                   ifelse(rep.int(i, nrow(abbreviations)) == 2012, abbreviations$`2012`,
#                          ifelse(rep.int(i, nrow(abbreviations)) == 2013, abbreviations$`2013`,
#                                abbreviations$`2014`))))){
#     url <- paste("https://www.basketball-reference.com/teams/",j,"/",i,".html", sep = "")
#     count = count + 1
#     print(paste(i,j,count))
#     all_tables <- read_html(url) %>%
#       html_nodes("table") %>%
#       html_table()
#     all_df[[count]] <- all_tables[[3]] %>%
#      clean_names() %>%
#       select(name = x, g, mp, ws, bpm, vorp) %>%
#       mutate(season = i, team = j)
#   }
# }
# 
# for (i in 1:120){
#     all_df[[i+30]] <- all_df[[i+30]] %>% 
#       mutate(returning = name %in% all_df[[i]]$name) 
#     all_df[[i]] <- all_df[[i]] %>% 
#       mutate(staying = name %in% all_df[[i+30]]$name)
# }
# 
# for (i in 1:30){
#   all_df[[i]] <- all_df[[i]] %>% 
#     mutate(returning = NA)
#   all_df[[i+120]] <- all_df[[i+120]] %>% 
#     mutate(staying = NA)
# }  
# 
# 
# roster_df <- data.frame()
# for (i in 1:length(all_df)){
#   roster_df <- rbind(roster_df, all_df[[i]])
# }
# 
# save(roster_df, file = "roster_df.RData")
load("roster_df.RData")

############IMPORT ALL STAR VOTES##############

setwd("/Users/jackdolitsky/Documents/Replication\ Project/all_star_excel_data")

file_names <- list.files()


all_star_votes <- lapply(file_names, read_xlsx)

all_star_votes_df = data.frame()

for (i in 1:length(all_star_votes)){
  all_star_votes[[i]] = all_star_votes[[i]] %>% 
    mutate(season = 2010 + i) 
  all_star_votes_df = rbind(all_star_votes_df, all_star_votes[[i]])
}


roster_2011 = roster_df %>% 
  filter(season == 2011) %>% 
  group_by(name) %>% 
  summarise(count = n(), team = team) %>% 
  filter(count < 2) %>% 
  select(-count) 
right_join(x = roster_2011, y = all_star_votes[[1]], by = c("name")) %>% 
  write_clip()

roster_2012 = roster_df %>% 
  filter(season == 2012) %>% 
  group_by(name) %>% 
  summarise(count = n(), team = team) %>% 
  filter(count < 2) %>% 
  select(-count)
right_join(x = roster_2012, y = all_star_votes[[2]], by = c("name")) %>% 
  write_clip()

roster_2013 = roster_df %>% 
  filter(season == 2013) %>% 
  group_by(name) %>% 
  summarise(count = n(), team = team) %>% 
  filter(count < 2) %>% 
  select(-count)
right_join(x = roster_2013, y = all_star_votes[[3]], by = c("name")) %>% 
  write_clip()

roster_2014 = roster_df %>% 
  filter(season == 2014) %>% 
  group_by(name) %>% 
  summarise(count = n(), team = team) %>% 
  filter(count < 2) %>% 
  select(-count)
right_join(x = roster_2014, y = all_star_votes[[4]], by = c("name")) %>% 
  write_clip()


#########################    MERGE     #############################


setwd("/Users/jackdolitsky/Documents/Replication Project")

all_star_votes_df_grouped = all_star_votes_df %>%
  group_by(team, season) %>%
  summarise(votes = sum(votes))

directory <- read_xlsx("team_name_directory.xlsx", skip = 2,
                       col_names = c("name_abb", "name_first",
                                     "name_full", "name_population",
                                     "empty", "conference")) %>%
  select(-empty)

directory = directory %>%
  mutate(name_abb = ifelse(name_abb == "BKN", "BRK", name_abb)) %>%
  filter(name_abb != "NJN" & name_abb != "NOH")


# df_gate_revenues[[17,3]] == directory[[5,3]]
# stri_compare(df_gate_revenues[[9,3]], directory[[4,3]])

df_gate_revenues = df_gate_revenues %>%
  arrange(team)

directory = directory %>% arrange(name_full)
directory$name_full = unique(df_gate_revenues$team)

#run before 33
final_df = inner_join(x = df_gate_revenues, y = directory,
                      by = c("team" = "name_full"))

final_df = left_join(x = final_df, y = all_star_votes_df_grouped, by = c("name_abb" = "team", "year" = "season")) %>%
  mutate(votes = ifelse(is.na(votes), 0, votes))

income_data = read_xlsx("income_data.xlsx")


income_data[income_data == 0] <- NA

income_data = income_data %>% pivot_longer(cols = -City, names_to = "season",
                                           values_to = "income")


final_df = final_df %>%  mutate(year = as.character(year))
final_df = left_join(x = final_df, y = income_data, by = c("name_population" = "City",
                                                           "year" = "season"))

population_data = read_xlsx("population_data.xlsx", skip = 2)
population_data = population_data %>% pivot_longer(cols = -City, names_to = "season",
                                                   values_to = "population")

final_df = left_join(x = final_df, y = population_data, by = c("name_population" = "City",
                                                               "year" = "season"))

lagged_wins = df_team_wins %>% mutate(year = as.character(as.numeric(year)+1)) %>%
  select(lagged_playoff_wins = playoff_w, tm, year) %>%
  filter(year < 2015)

final_df = final_df %>% rename(franchise_abb = name_abb)
final_df$name_abb = final_df$franchise_abb
final_df[which(final_df$year %in% c(2011,2012) & (final_df$franchise_abb == "BRK")),]$name_abb <- "NJN"

final_df[which(final_df$year %in% c(2011,2012, 2013) & (final_df$franchise_abb == "NOP")),]$name_abb <- "NOH"

lagged_wins = lagged_wins %>%  mutate(year = as.character(year))
lagged_wins[which(lagged_wins$year == "2013" & lagged_wins$tm == "NJN"),]$tm <- "BRK"
lagged_wins[which(lagged_wins$year == "2014" & lagged_wins$tm == "NOH"),]$tm <- "NOP"
final_df = inner_join(x = final_df, y = lagged_wins, by = c("name_abb" = "tm",
                                                            "year"))

table(final_df$name_abb, final_df$year)

save(final_df, file = "pre_champ_df.RData")
load("pre_champ_df.RData")

champ_data = champ_data %>% mutate(champion = ifelse(champion == "BKN", "BRK", champion))


df_team_wins = df_team_wins %>% mutate(year = as.character(year)) %>%
  mutate(reg_season_wp = (reg_season_w/reg_season_g)*100)

final_df = inner_join(x = final_df, y = df_team_wins, by = c("name_abb" = "tm",
                                                             "year" = "year"))

attendance_df = attendance_df %>% mutate(year = as.character(year)) %>%
  arrange(team)

final_df = final_df %>% arrange(name_first)
attendance_df$team <- final_df$name_first


final_df = inner_join(x = final_df, y = attendance_df, by = c("name_first" = "team",
                                                              "year"))

top_bpm_df = roster_df %>%
  filter(mp >= 1000) %>%
  group_by(team, season) %>%
  mutate(bpm_rank = rank(desc(bpm), ties.method = "random")) %>%
  ungroup() %>%
  filter(bpm_rank < 2.0)

top_bpm_df = top_bpm_df %>% mutate(season = as.character(season))

final_df = inner_join(x = final_df, y = top_bpm_df, by = c("name_abb" = "team",
                                                           "year" = "season"))
save(final_df, file = "pre_cb_df.RData")
load("pre_cb_df.RData")

#cb calculaton
final_df = final_df %>% 
  group_by(conference, year) %>% 
  mutate(sd_reg_season_wp = sd(reg_season_wp),
         mean_reg_season_wp = mean(reg_season_wp), 
         sq_reg_season_g = sqrt((sum(reg_season_g)/2))) %>% 
  ungroup()

final_df = final_df %>% 
  mutate(comp_balanace = sd_reg_season_wp/(mean_reg_season_wp/sq_reg_season_g))

#roster stability

roster_df_2 = left_join(x= roster_df, y = directory, by = c("team" = "name_abb"))


franchise = final_df %>% group_by(franchise_abb, name_abb) %>% 
  summarise(a = NA) %>% 
  select(-a)

roster_df_2 <- left_join(x= roster_df_2, franchise, by = c("team" = "name_abb"))

roster_df_2 <- roster_df_2 %>% 
  group_by(franchise_abb, season) %>% 
  summarise(returning_mp = sum(mp[returning]), staying_mp = sum(mp[staying]),
            total_mp = sum(mp)) %>% 
  ungroup() %>% 
  mutate(returning_prop = returning_mp/total_mp, 
         staying_prop = staying_mp/total_mp)

roster_df_2 = roster_df_2 %>% mutate(prior_staying_prop = lag(staying_prop)) %>% 
  mutate(roster_stability = (prior_staying_prop + returning_prop)/ 2) %>% 
  filter(season != 2010) %>%
  select(franchise_abb, season, roster_stability) %>% 
  mutate(season = as.character(season)) 


final_df = inner_join(final_df, roster_df_2, by = c("franchise_abb" = "franchise_abb",
                                                    "year"= "season"))
final_df = left_join(final_df, champ_data, c("team" = "champion",
                                             "year" = "year"))
final_df = left_join(final_df, runner_up_data, c("team" = "runner_up",
                                                 "year" = "year")) 
final_df = final_df %>% 
  mutate(WCHM20 = replace_na(WCHM20, 0),
         RCHM20 = replace_na(RCHM20, 0))

final_df = final_df %>% 
  select(D = year, GATE = revenue, WINS = reg_season_w, WPLAY = playoff_w,
         `WPLAY(-1)` = lagged_playoff_wins, WCHM20, RCHM20, STARVOT = votes,
         SCAP = stadium_cap, RSTAB = roster_stability, ATTEND = total, CB = comp_balanace,
         RYCAP = income, POP = population, TBPM = bpm)

############################MODELS#############################


adjusted_final_df = final_df %>% 
  mutate(WPLAY = WPLAY + 1,`WPLAY(-1)` = `WPLAY(-1)`+ 1, 
         WCHM20 = WCHM20 + 1, RCHM20 = RCHM20 + 1, STARVOT = STARVOT + 1, TBPM = TBPM + 1)

model_1 = glm(log(GATE) ~ log(WINS) + log(WPLAY) +
                log(`WPLAY(-1)`) + log(WCHM20) + log(RCHM20) +
                log(STARVOT) + log(TBPM) + log(SCAP) + log(RSTAB) +
                log(POP) + log(RYCAP) + log(CB) + D,
              data = adjusted_final_df)

model_1_before = glm(log(GATE) ~ log(WINS) + log(WPLAY) +
                log(`WPLAY(-1)`) + log(WCHM20) +
                log(STARVOT) + log(SCAP) + log(RSTAB) +
                log(POP) + log(RYCAP) + log(CB) + D,
              data = adjusted_final_df)


model_2 = glm(GATE ~ ., -RCHM20, data = adjusted_final_df)

model_2_before = glm(GATE ~ WINS + WPLAY +
                                 `WPLAY(-1)`+ WCHM20 +
                                 STARVOT + SCAP + RSTAB +
                                 POP + RYCAP + CB + D, , data = adjusted_final_df)

(mean(adjusted_final_df$GATE)/mean(adjusted_final_df$STARVOT))*(0.010759/5.517484)


M <- c(mean(adjusted_final_df$WINS, na.rm = TRUE), mean(adjusted_final_df$`WPLAY(-1)`, na.rm = TRUE), 
       mean(adjusted_final_df$WCHM20, na.rm = TRUE),
       mean(adjusted_final_df$RCHM20, na.rm = TRUE), mean(adjusted_final_df$RYCAP, na.rm = TRUE), 
       mean(adjusted_final_df$CB, na.rm = TRUE))

table_4 = model_1 %>% 
  summary() %>% coef() %>% 
  as.data.frame() %>% 
  select(`Coeffiecient or Elasticity` = Estimate, p = `Pr(>|t|)`) %>% 
  filter (p<0.05) %>% 
  cbind(M) %>% 
  mutate(`$ Average Marginal Revenue` = (mean(adjusted_final_df$GATE, na.rm = TRUE)/M)*
           (`Coeffiecient or Elasticity`/coef(model_1)[1]))

table_4_before = model_1_before %>% 
  summary() %>% coef() %>% 
  as.data.frame() %>% 
  select(`Coeffiecient or Elasticity` = Estimate, p = `Pr(>|t|)`) %>% 
  filter (p<0.05) %>% 
  cbind(M) %>% 
  mutate(`$ Average Marginal Revenue` = (mean(adjusted_final_df$GATE, na.rm = TRUE)/M)*
           (`Coeffiecient or Elasticity`/coef(model_1)[1]))

######### STARGAZER ############

stargazer(model_1, type="html") %>% 
  write_clip()










