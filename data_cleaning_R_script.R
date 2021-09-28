# Required libraries
install.packages("dplyr")
install.packages("EnvStats")
library(dplyr)
library(EnvStats)


# IMPORTANT: Change this line to whatever the working directory is meant to be. 
setwd("~/Goldsmith's  DS Coursework and Cheatsheets/Semester 3/DSM110 - R for data science/First Assignment/Submission Folder")
  
  
# CSV's files are assumed to be in the same directory as this R script, 
# Which is also the working directory.
# This will be the primary/main dataframe we will work on. 
male_height_df <- read.csv("change-in-male-height-slope.csv")
life_expectancy_df <- read.csv("Life Expectancy Data.csv")

# Splitting the height dataframe. countries_region_df contains the region 
# for each country,which is only on rows where year = 2015 for some reason.
# male_height_df contains the average male height in the year 2000 to 2015. 
# (after we add 19 to get the  adult height, as by default the year in
#  male_height_df is the birth year). 
countries_region_df <- filter(male_height_df, Year == 2015)
male_height_df$Year <- male_height_df$Year + 19 
male_height_df <- filter(male_height_df, Year>= 1996) # Only the years we want. 



# Filtering columns for both. 
countries_region_df <- select(countries_region_df, Entity, Code, Continent)
male_height_df <- select(male_height_df, Year, Entity, Mean.male.height..cm.)


# Examining differences in country names between the countries listed in the 
# life expectancy csv and the male height csv. 
country_names_only_in_primary <- setdiff(life_expectancy_df$Country , 
                                         male_height_df$Entity)

country_names_only_in_secondary <- setdiff(male_height_df$Entity , 
                                           life_expectancy_df$Country)

# print(country_names_only_in_primary)
# print(country_names_only_in_secondary)


dirty_names_vec <- c("Bolivia (Plurinational State of)",              
                     "Brunei Darussalam", 
                     "Côte d'Ivoire",                                     
                     "Democratic People's Republic of Korea",
                     "Democratic Republic of the Congo",                   
                     "Iran (Islamic Republic of)",
                     "Lao People's Democratic Republic",                     
                     "Micronesia (Federated States of)",
                     "Republic of Korea",                                    
                     "Republic of Moldova",
                     "Russian Federation",                                  
                     "Swaziland",
                     "Syrian Arab Republic",                                 
                     "The former Yugoslav republic of Macedonia",
                     "Timor-Leste",                                          
                     "United Kingdom of Great Britain and Northern Ireland",
                     "United Republic of Tanzania",                          
                     "United States of America",
                     "Venezuela (Bolivarian Republic of)",                   
                     "Viet Nam")  


desired_names_vec <- c("Bolivia", 
                       "Brunei", 
                       "Cote d'Ivoire", 
                       "North Korea", 
                       "Democratic Republic of Congo",
                       "Iran", 
                       "Laos", 
                       "Micronesia (country)",
                       "South Korea",
                       "Moldova",
                       "Russia",
                       "Eswatini",
                       "Syria",
                       "North Macedonia", 
                       "Timor",
                       "United Kingdom", 
                       "Tanzania", 
                       "United States", 
                       "Venezuela", 
                       "Vietnam")

# Changing the country names in life_expectancy_df. This will will allow us to
# join the dfs properly. 
for (i in 1:length(dirty_names_vec)){ 
  life_expectancy_df$Country = ifelse(life_expectancy_df$Country == dirty_names_vec[i],
                                      desired_names_vec[i], 
                                      life_expectancy_df$Country)
}

# No longer needed
rm(dirty_names_vec)
rm(desired_names_vec)


# Adding desired data to the main dataframe, life_expectancy. 
life_expectancy_df <- left_join(life_expectancy_df, 
                                 countries_region_df, 
                                    by = c("Country" = "Entity"))


life_expectancy_df <- left_join(life_expectancy_df, 
                                male_height_df, 
                                by = c("Country" = "Entity", 
                                       "Year" = "Year"))



# No longer needed. 
rm(countries_region_df)
rm(male_height_df)


# Renaming the Columns. First gsub replaces the dot with underscore. 
# Second gsub Capitalizes first letter of each word separated by an 
# underscore. 
names(life_expectancy_df) <- gsub(x = names(life_expectancy_df), 
                                  pattern = "\\.", 
                                  replacement = "_")  

names(life_expectancy_df) <- gsub("(?<=^|_)([a-z])",
                                  "\\U\\1",
                                  names(life_expectancy_df), 
                                  perl=TRUE)



# Manually renaming some columns. Left arg = new name. Right arg = old name. 
life_expectancy_df <- rename(life_expectancy_df, Country_Code = Code)
life_expectancy_df <- rename(life_expectancy_df, Average_Years_of_schooling = Schooling)
life_expectancy_df <- rename(life_expectancy_df, Region = Continent)
life_expectancy_df <- rename(life_expectancy_df, Alcohol_Consumption = Alcohol)
life_expectancy_df <- rename(life_expectancy_df, Development_Status = Status) 
life_expectancy_df <- rename(life_expectancy_df, Mean_Male_Height_cm = Mean_Male_Height__Cm_) 
life_expectancy_df <- rename(life_expectancy_df, Thinness_1_19_Years = Thinness__1_19_Years) 
life_expectancy_df <- rename(life_expectancy_df, GDP_Per_Capita = GDP) 



# Filtering. We are removing all rows with Na's in any of the following columns.
# This will result in us dropping 66 rows, an acceptable loss. 
life_expectancy_df <- filter(life_expectancy_df, !is.na(Life_Expectancy), 
                                  !is.na(Adult_Mortality), 
                                  !is.na(Polio),
                                  !is.na(Diphtheria), 
                                  !is.na(Thinness_1_19_Years), 
                                  !is.na(Thinness_5_9_Years), 
                                  !is.na(Country_Code), 
                                  !is.na(Region),
                                  !is.na(Mean_Male_Height_cm))


# Dropping These columns as many of the values are extremely anomalous and 
# impossible to clean. 
life_expectancy_df$BMI <- NULL
life_expectancy_df$Population <- NULL
life_expectancy_df$Percentage_Expenditure <- NULL
life_expectancy_df$Infant_Deaths <- NULL
life_expectancy_df$Under_Five_Deaths <- NULL
life_expectancy_df$Measles <- NULL





# Stochastic Infill. Basically, we are going to replace NA values in several 
# rows by sampling from a normal distribution for the data we DO have. 
vec_of_countries <- unique(life_expectancy_df$Country)

# Columns for Stochastic Infill. These columns all contain significant number 
# of NA values (too many to just drop the rows) or weirdly small values that are 
# obvious outliers. To generate plausible values to replace these, we will 
# sample from a normal distribution based on the country. 
# Bit clunky, but it works.
vec_of_columns <- c("Hepatitis_B", 
                    "Alcohol_Consumption", 
                    "Total_Expenditure", 
                    "Adult_Mortality")

# setting the seed for consistency.  
set.seed(19)

# For every country/vec_of_columns combination. 
for (i in 1:length(vec_of_countries)){ 
  for (j in 1: length(vec_of_columns)){
    
    
    current_country <- vec_of_countries[i]
    current_column <- vec_of_columns[j]
    
    # Filtered df is the life expectancy df for a given country. A convenience, 
    # variable to make the code more readable. 
    filtered_df <- filter(life_expectancy_df, Country == current_country)
    
    # Some useful statistics. Will be fed into rnorm(). Based off of the 
    # filtered df. 
    current_mean = mean(filtered_df[, current_column], na.rm = TRUE)
    
    current_median = median(filtered_df[, current_column], na.rm = TRUE)
    
    current_sd = sd(filtered_df[, current_column], na.rm = TRUE)
    
    # a reduced form of sd meant to be conservative. 
    current_scaled_sd = sd(c(current_mean, current_median))
    
    # Don't infill NA with NA! If we can't get normal estimate for a given country, 
    # just go onto the next country/column combination. 
    if (is.na(current_mean)){ 
        next
    }
    
    
    # Doing infill for outliers. Outliers are defined as any values less than
    # one-fifth the size of the median. ------------------------------------------------
    
    # Indices of the df we want to alter. 
    # Criteria: 
    # 1) Current Country
    # 2) Point on the current column is an outlier
    row_indices = intersect(which(life_expectancy_df$Country == current_country),
                            which(life_expectancy_df[, current_column] < 0.2 * current_median))
    
    values <- rnorm(length(row_indices), current_median, current_scaled_sd)
    
    life_expectancy_df[row_indices, current_column] <- values


    # Doing infill for NA values. -----------------------------------------------------
    
    # Indices of the df we want to alter. 
    # Criteria: 
    # 1) Current Country
    # 2) Point on the current column is NA
    row_indices = intersect(which(life_expectancy_df$Country == current_country),
                            which(is.na(life_expectancy_df[, current_column])))
    
    values <- rnorm(length(row_indices), current_mean, current_sd)
    
    life_expectancy_df[row_indices, current_column] <- values    
    
  }
}


# GDP has significant outlier thats distory the data and make infill very 
# difficult. As a compromise, we replace all the differnt gdp values with the 
# geometric mean of GDP for a given country. 
grouped_country <- group_by(life_expectancy_df, Country)

grouped_country <- summarise(grouped_country, 
                             geo_GDP = geoMean(GDP_Per_Capita, na.rm = TRUE))             

life_expectancy_df <- left_join(life_expectancy_df, 
                                grouped_country)

life_expectancy_df$GDP_Per_Capita <- life_expectancy_df$geo_GDP

life_expectancy_df$geo_GDP <- NULL


# After the stochastic infill, we need to floor/ceiling the data to get rid of 
# impossible values (such as negative alcohol consumption or inoculation rates
# greater than 100%)
life_expectancy_df$Hepatitis_B <- sapply(life_expectancy_df$Hepatitis_B, 
                                         function(x) ifelse(x<0, 0, x))
life_expectancy_df$Hepatitis_B <- sapply(life_expectancy_df$Hepatitis_B, 
                                         function(x) ifelse(x>100, 100, x))
life_expectancy_df$Alcohol_Consumption <- sapply(life_expectancy_df$Alcohol_Consumption, 
                                                 function(x) ifelse(x<0, 0, x))
life_expectancy_df$Total_Expenditure <- sapply(life_expectancy_df$Total_Expenditure, 
                                               function(x) ifelse(x<0, 0,  x))



# Writing cleaned df to the working directory. 
write.csv(life_expectancy_df, "cleaned_life_expectancy_data.csv", row.names = FALSE) 


# Exploratory Analysis of the cleaned data -----------------------------------------------------------------------

# Seeing some summary statistics.
grouped_country <- group_by(life_expectancy_df, Region, Development_Status)
grouped_country <- summarise(grouped_country, 
                             avg_life_expectancy = mean(Life_Expectancy), 
                             num_countires = length(unique(Country))
)

# Some correlations between life_expectancy and some (potentially) 
# useful predictor variables.  
print(cor(x = life_expectancy_df$Year, 
          y = life_expectancy_df$Life_Expectancy, 
          use = "complete.obs"))
print(cor(x = life_expectancy_df$GDP_Per_Capita, 
          y = life_expectancy_df$Life_Expectancy, 
          use = "complete.obs"))
print(cor(x = life_expectancy_df$Income_Composition_Of_Resources, 
          y = life_expectancy_df$Life_Expectancy, 
          use = "complete.obs"))
print(cor(x = life_expectancy_df$Alcohol_Consumption, 
          y = life_expectancy_df$Life_Expectancy, 
          use = "complete.obs"))



# Making a histogram of life expectancy for the entire dataset. 
hist(life_expectancy_df$Life_Expectancy, 
      main = "Overall Life Expectancies", 
      xlab = "Life Expectancy", 
      ylab = "Frequency", 
      col = "blue")

# Schooling histogram 
hist(life_expectancy_df$Average_Years_of_schooling, 
     main = "Years of Schooling Worldwide", 
     xlab = "Average Years of Schooling", 
     ylab = "Frequency", 
     col = "green")


# Number of NA's per column.
n <- apply(life_expectancy_df, MARGIN=2, function(x) {sum(is.na(x))}) 

# Examining the cleaned df. 
View(life_expectancy_df)


# General summary df. 
summary_df <- as.data.frame(summary(life_expectancy_df))
