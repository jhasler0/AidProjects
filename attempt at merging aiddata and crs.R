##Messing around at the subnational level.

##OK, so there are many problems with this and I should probably give up, but I'm not going to!!!
##Problems: No common codings. AIDdata has disbursements over time by project and CRS has over projects by year. 
####This means we cannot merge on that either. Both have project titles, BUT they are in different langauges.

##The goal of this project is thus to:
#1. Import and Merge CRS files for Colombia
#2. Import AIDdata files.
#3. translate the respective title columns, drop flags for weird text, and tries to merge by both languages

##Note: The goal is to wind up with project-year data. (I think)

###All the libraries!
library(tidyverse)
library(readr)
library(translateR)
library(stringr)

crs_country_select <- function(crs_data_by_year,country){
  tmp <- crs_data_by_year %>%
    filter(RecipientName == country)
}

#vector of the file name years. Early ones are grouped together. 1973-94 is omitted as it serves as the base case below.
years <- c('2000-01','2002-03','2004-05','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')


x<- read_delim('./CRS 1995-99 data.txt', "|", escape_double = FALSE, trim_ws = TRUE)
crs_colombia <- crs_country_select(x,'Colombia')

#importing, reshaping and appending each of the CRS data files.
for (i in years){
  filepath = paste0('./CRS ',i,' data.txt')
  crs_tmp <- read_delim(filepath,"|", escape_double = FALSE, trim_ws = TRUE)
  tmp_df <- crs_country_select(crs_tmp, 'Colombia')
  crs_colombia <- rbind(crs_colombia, tmp_df)
}

crs_colombia$NewProjectTitle <- str_replace_all(crs_colombia$ProjectTitle, "[[:punct:]]", " ")

crs_colombia2 <- crs_colombia %>%
  filter(!is.na(ProjectTitle)) %>%
  mutate(NewProjectTitle = tolower(NewProjectTitle))


level_1a <- read_csv("C:/Users/Jack/Google Drive/Active Papers/AidProjects/Columbia/level_1a.csv")
level_1a$project_title <- str_replace_all(level_1a$project_title, "[[:punct:]]", " ")
level_1a <- level_1a %>%
  filter(!is.na(project_title)) %>%
  mutate(project_title = tolower(project_title)) %>%
  select(precision_code, place_name, latitude, longitude,location_type_code, project_title, start_actual_isodate,
         end_actual_isodate,donors) %>%
  unique()


##Attempted
georeferenced_crs <- merge(crs_colombia2, level_1a, by.x = 'NewProjectTitle', by.y = 'project_title', all = TRUE)
g_crs <- georeferenced_crs %>%
  filter(!is.na(latitude)&(!is.na(DonorName)))
##And failed

crs_project_titles <- crs_colombia2 %>%
  select(NewProjectTitle) %>%
  unique() %>%
  arrange(NewProjectTitle)

aiddata_project_titles <- level_1a %>%
  select(project_title) %>%
  unique() %>%
  arrange(project_title)



#Starting with 2010 because why not.
x<- read_delim('./CRS 2010 data.txt', "|", escape_double = FALSE, trim_ws = TRUE)
colombia_2010 <- x %>%
  filter(RecipientName == 'Colombia') %>%
  arrange(DonorName) %>%
  filter(DonorName == 'Switzerland')

colombia_2010$usd_commitment <- colombia_2010$usd_commitment*1000000





first_attempt <- merge(colombia_2010, level_1a, by.x = c('DonorName','usd_commitment'), by.y = c('donors','total_commitments'))


col_subnational_2010 <- level_1a %>%
  filter(start_actual_isodate > )

