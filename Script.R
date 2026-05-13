library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(tidyr)
 
# 🟪 Todo
# 1️⃣ making the graphing function plot one habit and the goal and how it changed over time
# 2️⃣ updating the cleaning to include months, and then updating my goal sheet to include months,
#     and then merging on habit, year, and month. Eg. Non-math time goal used to be 5 hours, now is 3 for May
# 3️⃣ Making the minimize group of goals flip the distance measure so they are greener when closer to -1, rather than 0?
# 4️⃣Making the coloring of the heat map more of a gradient instead of blocks! redo visual 1 or 2!
# 5️⃣can also make a graph that does a day by day analysisusing the pre-cleaned data of time spent, and can look at when I reached the 5 hour goal,
# and can use the groupings to filter out what counts as time I'm tracking!! 


# Coding Outline

# Take in data about my time each week
# # Category --- Time --- Goal Time --- Date --- Unit

# ✅ Start with sample data, then decide what I want to do with it

df_real <- read.csv("real_data.csv")

cleaning_real <- function(df){
  df <- df %>% 
    rename_with(tolower) %>%
    mutate(date = ymd(date),
           week = isoweek(date),
           month = month(date),
           year = year(date)) %>%  
    group_by(week,habit,month, year) %>% 
    summarize(total = sum(value)) 
    # %>% 
    # mutate(goal = case_when(habit == "Eating Meals" ~ 4,
    #                         habit == "Morning and Night Routine" ~ 3) #see if there is a way to merge data here, cause I want the goals to change based on the week for my exercise habit
    #       )
  
  ## 🟪 Left to do 🟪
  # Data only has categories for non-zero values, will need to merge and set these to zero
  # Adding goals for each week
  # cleaning the names cause they change kinda
  # using R to build out my goal data --> can use append to add the df to itself and make changes per week as I need
  
  
    
}

df_r_clean <-  cleaning_real(df_real)

# Building out goals
df_goals <- read.csv("Time Tracker Goals - Sheet1.csv")
df_goals_test <- read.csv("test-week-Time Tracker Goals - Sheet1.csv")


# Merge the data
merge_data <- function(df,goal){
# Create all combinations
all_combos <- expand_grid(
  week = unique(df$week),
  habit = goal$habit,
  year = unique(df$year)
)


# Merge onto original data
out <- all_combos %>%
  left_join(df, by = c("week", "habit","year")) %>%
  left_join(goal, by = c( "habit","year")) %>%
  mutate(total = replace_na(total, 0),
         habit = new_name) %>% 
  select(-new_name)

}

df_merge <- merge_data(df_r_clean,df_goals_test)

#' @title Met Goal
#' @param df A cleaned dataset of weekly time tracking
#' @param visual An indicator, 1 if want to output graph, 0 otherwise
#' @return A new dataset with added met_goal column and graph to visualize 
#' all the weeks of data and all the catergory
met_goal <- function(df,visual,group_list = "all"){
  df <- df %>% mutate(met_goal = ifelse(total >= goal,1,0),
                      goal_dist = (total - goal)/goal,
                      goal_group = case_when(
                        goal_dist >= 0.5 ~ "v_over",
                        goal_dist >= 0.1 ~ "over",
                        goal_dist >= -0.1 ~ "met",
                        goal_dist >= -0.7 ~ "middle",
                        TRUE ~ "bottom"
                      )) 
  if(!("all" %in% tolower(group_list))){
    df <- df %>% filter(group %in% group_list)
  }
  default <- ggplot(data = df, aes(week, habit)) +
    scale_x_continuous(breaks = unique(df$week)) +
    geom_text(aes(label = round(goal_dist, 2)), color = "black", size = 3)
  
  if(visual == 1){
    bin_plot <-  default +
      geom_tile(aes(fill = met_goal)) +
      scale_fill_gradient(low = "lightgrey",high = "lightgreen")
    print(bin_plot)
  } else if(visual ==2) {
    dist_plot <-  default +
      geom_tile(aes(fill = goal_dist)) +
      scale_fill_gradient2(low = "#F54927",mid = "#52E041", high = "#11E7EE")
    print(dist_plot)
  }
  else if(visual == 3){
    group_plot <-  default +
      geom_tile(aes(fill = goal_group)) +
      scale_fill_manual(values = c("v_over" = "#FFC080", 
                        "over" = "#FFFF80",
                        "met" = "#C0FF80",
                        "middle" = "#80C0FF",
                        "bottom" = "#C080FF")) +
    geom_text(aes(label = round(goal_dist, 2)), color = "black", size = 3) 
    print(group_plot)
  }
  else{
    print("No visualization")
  }
  return(df)
}

df_met <- met_goal(df_merge,3,"Work")
df_met <- met_goal(df_merge,3,c("Health","Family"))
met_goal(df_merge,3,c("Social"))
met_goal(df_merge,3,"Minimize")


#' @title Get habit by group
#' @return a print of all habits, sorted by the groups they are in
get_habit_by_group <- function(df){
  list = unique(df$group)
  for(i in list){
  print(paste0(c("Group: ",i)))
    unq = df %>% 
    filter(group == i) %>% 
    pull(habit) %>% 
    unique()
    print(unq)
  print("-----------------------------")
  }
}

get_habit_by_group(df_merge)


# # Graph
# You input category or categories you want, and then it will plot them with a 
# vertial line for the goal that week, keeping in mind that the goal can change
# each week as well




