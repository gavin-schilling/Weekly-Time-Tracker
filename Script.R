library(dplyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(tidyr)
library(usethis) 

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
           year = year(date)) %>%  
    group_by(week,habit, year) %>% 
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
df_goal_f <- read.csv("w20-w7 goals.csv")

#expanding goal testing data to go back to week 7
# on_merge_goal <- expand.grid(
#   week = unique(df_r_clean$week),
#   habit = unique(df_goals_test$habit),
#   year = unique(df_goals_test$year)
# )
# df_goal_same <- df_goals_test %>% 
#   filter(week == 19)
# 
# goal_expand <- on_merge_goal %>% 
#   left_join(df_goal_same,by = c("habit","year") ) %>% 
#   select(-"week.y") %>% 
#   rename(week = week.x) %>% 
#   arrange(desc(week))
# 
# write.csv(goal_expand, "w20-w7 goals.csv",row.names = T)
# 
# df_goal
# 
# goal_exp_f <- goal_expand %>% 
#   mutate(goal = case_when(habit == ))
#   


# Merge the data
#' @title Merge data
#' @param df cleaned goal data
#' @param df_goal goal data with weeks
#' @return merged data with all habit and week combos filled,
merge_data <- function(df,df_goal){
  # Create all combinations
  all_combos <- expand_grid(
    week = unique(df$week),
    habit = unique(df_goal$habit),
    year = unique(df$year)
  )
  
  # Merge onto original data
  out <- all_combos %>%
    left_join(df, by = c("week","habit","year")) %>%
    left_join(df_goal, by = c("week","habit","year")) %>%
    mutate(total = replace_na(total, 0),
           habit = new_name) %>% 
    select(-new_name)
  
  #adding whether met goals or not
  out <- out %>% mutate(met_goal = ifelse(total >= goal,1,0),
                      goal_dist = (total - goal)/goal,
                      goal_group = case_when(
                        goal_dist >= 0.5 ~ "v_over",
                        goal_dist >= 0.1 ~ "over",
                        goal_dist >= -0.1 ~ "met",
                        goal_dist >= -0.7 ~ "middle",
                        TRUE ~ "bottom"
                      )) 
  
}

df_merge <- merge_data(df_r_clean,df_goal_f)

#' @title Met Goal
#' @param df A cleaned dataset of weekly time tracking
#' @param visual An indicator, 1 if want to output graph, 0 otherwise
#' @return A new dataset with added met_goal column and graph to visualize 
#' all the weeks of data and all the catergory
met_goal_viz <- function(df,visual,group_list = "all"){
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
  
}

met_goal_viz(df_merge,3)
met_goal_viz(df_merge,3,c("Health"))



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

get_habit_by_group(df_met)



# # Graph
# You input category or categories you want, and then it will plot them with a 
# vertial line for the goal that week, keeping in mind that the goal can change
# each week as well
#'@title Graph month
#'@param target The habit you want to graph
#'
graph_month_single <- function(df,target){
  df <- df %>% filter(habit == target)
  
  
  out_plot <- ggplot(df, aes(x = week)) +
    
    # Total bars
    geom_col(aes(y = total,fill = goal_group),
             width = 0.8) +
             scale_fill_manual(values = c("v_over" = "#FFC080", 
                                     "over" = "#FFFF80",
                                     "met" = "#C0FF80",
                                     "middle" = "#80C0FF",
                                     "bottom" = "#C080FF")) +
    
    # Goal stair-step line
    geom_step(aes(y = goal),
             
              linewidth = 1.2,
              color = "green") +
    
    scale_x_continuous(breaks = unique(df$week)) +
    
    labs(
      title = target,
      x = "Week",
      y = "Total"
    )
  
  print(out_plot)
}

graph_month_single(df_merge,"Active Zone Minutes")
graph_month_single(df_merge,"Meal Prep")
get_habit_by_group(df_merge)

#'@title Graph Group
#'@return Same graph as graph_month_single, but plots all from a habit group, thank you gpt
graph_group <- function(df, target_group){
  
  df <- df %>% 
    filter(group == target_group)
  
  out_plot <- ggplot(df, aes(x = week)) +
    
    # Bars
    geom_col(
      aes(y = total, fill = goal_group),
      width = 0.8
    ) +
    
    # Colors
    scale_fill_manual(
      values = c(
        "v_over" = "#FFC080", 
        "over"   = "#FFFF80",
        "met"    = "#C0FF80",
        "middle" = "#80C0FF",
        "bottom" = "#C080FF"
      )
    ) +
    
    # Goal line
    geom_step(
      aes(y = goal),
      linewidth = 1.1,
      color = "green"
    ) +
    
    # One graph per habit
    facet_wrap(~habit, scales = "free_y") +
    
    scale_x_continuous(
      breaks = unique(df$week)
    ) +
    
    labs(
      title = paste("Goal Group:", target_group),
      x = "Week",
      y = "Total"
    ) +
    
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12)
    )
  
  print(out_plot)
}
graph_group(df_merge, "Health")
graph_group(df_merge, "Work")
graph_group(df_merge, "Fulfill")
graph_group(df_merge, "Minimize")
get_habit_by_group(df_merge)




