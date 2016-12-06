# DMV_Capstone
Collaborative work for 2016-2017 UVA DSI Capstone Project.

- DataCleaning.R:
  Merge data with same attributes but in different years into a single data frame. Variable name and type checking have been done before merging raw text files.
  Nine data frames will be created including:
  > 1. crash location
  > 2. vehicle 
  > 3. driver
  > 4. passenger
  > 5. pedestrain
  > 6. license
  > 7. property damage
  > 8. vehicle commercial
  > 9. uva-vt
  
- merge_fatal&injury_only.R
  Merge all the text files into a single data frame. Here the data contains only fatal and injury crashes caused by unrestraint fatalities, which is a subset of the old data. Data created will be used to analyze any correlation between unrestraint fatalities and serious car crashes.
  
- mergeOldData.R  
  Merge all the nine data frames (from DataCleaning.R) together into a huge data frame. Any observations that are mistyped will be removed. After this, we will obtain the cleaned data for analysis.
  
- plots.R
  Created different plots to help us understand the problems. 
  There are two types of plots: 
  > 1. plot of number of car crashes by month for each year from 2010 to 2015 (different plots for fatal, injury and property damage crashes)
  > 2. plot of car crash by location for each year from 2010 to 2015 (different plots for fatal, injury and property damage crashes)
