library(tidyverse)

setwd() # set the working directiory 
df_raw <- read_csv("chsi_dataset_DEMOGRAPHICS.csv")

df_clean <- df_raw %>% 
  #select relevant columns
  select(County_FIPS_Code,CHSI_County_Name,
          State_FIPS_Code, CHSI_State_Name, 
          Population_Size, 
          Age_19_Under,Age_19_64,Age_65_84,Age_85_and_Over, 
          White, Black, Native_American,Asian,Hispanic) %>% 
  rename(county_name=CHSI_County_Name, county_code=County_FIPS_Code,
         state_name=CHSI_State_Name, state_code=State_FIPS_Code,
         population_size=Population_Size)

glimpse(df_clean)

# pivot wide to long
df_long <- df_clean %>% 
  pivot_longer(cols=starts_with("Age"),names_to="age_range",values_to="age_percent") %>% 
  pivot_longer(cols=c("White","Black","Native_American","Asian","Hispanic"),names_to="race_type",values_to="race_percent")  

df_result <- df_long %>% 
  mutate(county_group_size=population_size*age_percent*race_percent/10000) %>% 
  group_by(state_code,state_name,age_range,race_type) %>% 
  summarize(state_group_size=sum(county_group_size)) 

df_result2 <- df_result %>% 
  group_by(state_code, state_name) %>% 
  mutate(state_population_size=sum(state_group_size)) %>% 
  ungroup() %>% 
  mutate(state_group_percent=state_group_size/state_population_size*100)

  
#check the result
pop_by_state_a <- df_clean %>% 
  group_by(state_code, state_name) %>% 
  summarize(size=sum(population_size))
pop_by_state_a

pop_by_state_b <- df_result %>% 
  group_by(state_code, state_name) %>% 
  summarize(size=sum(state_group_size))
pop_by_state_b
  
View(df_result2)
write_csv(df_result2, "demographic_us_clean.csv")
