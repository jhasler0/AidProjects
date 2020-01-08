##Jack Hasler
##8/1/2018
##R Script for transforming CRS data


##All required packages should be loaded here in this order.
library(readr)
library(tidyverse)
library(countrycode)
library(car)
library(plm)
library(psData)
library(readxl)
library(wbstats)
library(stargazer)

##This file is for constructing country-year aid data, regardless of donor.

##First grabbing the channel codes. These were last updated on 7/25/2018.
modified_crs_channel_codes <- read_delim("new_channel_codes.csv",",", escape_double = FALSE, trim_ws = TRUE)

channel_codes <- modified_crs_channel_codes %>%
  select(`Channel ID`, ch_type,ch_identity) %>%
  rename(code = `Channel ID`)
rm(modified_crs_channel_codes)
channel_codes$ch_type <- as.factor(channel_codes$ch_type)
channel_codes$ch_identity <- as.factor(channel_codes$ch_identity)

#Importing dem purpose code dict
dempurposecodes <- read_csv("dempurposecodes2.csv")
dempurposecodes <- dempurposecodes %>%
  filter(dem_purpose==1)%>%
  select(PurposeCode)


#The below function reformats it for use in this study.
crs_reshape_cy <- function(crs_data, ch_codes, dempurposecodes){
  
  #This chunk selects necessary variables (donor,recipient, channel, purpose and disbursement) and filters
  #out unwanted donors, recipients and obs with missing channel codes.
  crs_data <- crs_data %>%
    select(Year,DonorCode,DonorName,RecipientCode,RecipientName,
           usd_disbursement,ChannelCode,PurposeCode,PurposeName) %>%
    filter(!(RecipientCode %in% c(88,189,289,489,689,389,380,298,798,889,789,679,498,9998,589))) %>%
    filter(PurposeCode %in% dempurposecodes$PurposeCode) %>%
    #I have commented out the donor stuff since this is recipient country-year.
    #filter(DonorCode %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,20,21,22,40,50,61,68,69,75,76,301,302,701,742,801,820,
    #                        30,45,55,62.70,72,77,82,83,84,87,130,133,358,543,546,552,561,566,576,611,613,732,764,765)) %>%
    filter(!is.na(ChannelCode))
  #merge with channelcodes
  crs_data <- merge(crs_data, channel_codes, by.x = 'ChannelCode', by.y = 'code', all.x = TRUE)
  #counts and aid totals for all types
  crs_data <- crs_data %>%
    group_by(Year,RecipientCode) %>%
    add_count(sort = TRUE) %>%
    mutate(total_disbursement = sum(usd_disbursement, na.rm = TRUE)) %>%
    ungroup()
  
  #This giant block splits it up by type and subtype-identity. 
  crs_data2 <- crs_data %>%
    group_by(Year,RecipientCode,ch_type) %>%
    mutate(
      gov_d_total = ifelse(ch_type == 'government'&ch_identity=='donor',sum(usd_disbursement,na.rm = TRUE),0),
      gov_r_total = ifelse(ch_type == 'government'&ch_identity=='recipient',sum(usd_disbursement,na.rm = TRUE),0),
      gov_o_total = ifelse(ch_type == 'government'&ch_identity=='other',sum(usd_disbursement,na.rm = TRUE),0),
      gov_3_total = ifelse(ch_type == 'government'&ch_identity=='thirdparty',sum(usd_disbursement,na.rm = TRUE),0),
      corp_total = ifelse(ch_type == 'corporation'|ch_type=='publicprivate',sum(usd_disbursement,na.rm = TRUE),0),
      ngo_total = ifelse(ch_type == 'ngo'|ch_type=='network'|ch_type=='edu',sum(usd_disbursement,na.rm = TRUE),0),
      igo_total = ifelse(ch_type == 'igo',sum(usd_disbursement,na.rm = TRUE),0),
      other_total = ifelse(ch_type == 'government'&ch_identity=='donor',sum(usd_disbursement,na.rm = TRUE),0)
    ) %>%
    ungroup() %>%
    group_by(Year,RecipientCode) %>%
    select(Year,RecipientCode,RecipientName,n,total_disbursement,gov_d_total,gov_r_total,
           gov_3_total,gov_o_total,corp_total,ngo_total,igo_total,other_total) %>%
    mutate(
      gov_d_total = max(round(gov_d_total,6), na.rm = TRUE),
      gov_r_total = max(round(gov_r_total,6), na.rm = TRUE),
      gov_3_total = max(round(gov_3_total,6), na.rm = TRUE),
      gov_o_total = max(round(gov_o_total,6), na.rm = TRUE),
      corp_total = max(round(corp_total,6), na.rm = TRUE),
      ngo_total = max(round(ngo_total,6), na.rm = TRUE),
      igo_total = max(round(igo_total,6), na.rm = TRUE),
      other_total = max(round(other_total,6), na.rm = TRUE)
    ) %>%
    unique() %>%
    ungroup() %>%
    mutate(
      gov_d_percent = round(gov_d_total / total_disbursement,3),
      gov_r_percent = round(gov_r_total / total_disbursement,3),
      gov_o_percent = round(gov_o_total / total_disbursement,3),
      gov_3_percent = round(gov_3_total / total_disbursement,3),
      corp_percent = round(corp_total / total_disbursement,3),
      ngo_percent = round(ngo_total / total_disbursement,3),
      igo_percent = round(igo_total / total_disbursement,3),
      other_percent = round(other_total / total_disbursement,3)
    )
  #Return the data frame.
  crs_data2
}

#vector of the file name years. Early ones are grouped together. 1973-94 is omitted as it serves as the base case below.
years <- c('1995-99','2000-01','2002-03','2004-05','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')

#importing and running crs_by_types on the base case
x<- read_delim('./CRS 1973-94 data.txt', "|", escape_double = FALSE, trim_ws = TRUE)
crs_by_types <- crs_reshape_cy(x,channel_codes,dempurposecodes)

#importing, reshaping and appending each of the CRS data files.
for (i in years){
  filepath = paste0('./CRS ',i,' data.txt')
  crs_tmp <- read_delim(filepath,"|", escape_double = FALSE, trim_ws = TRUE)
  tmp_df <- crs_reshape_cy(crs_tmp, channel_codes,dempurposecodes)
  crs_by_types <- rbind(crs_by_types, tmp_df)
}


crs_by_types$new.code <- countrycode(crs_by_types$RecipientName, origin = 'country.name', destination = 'iso3n')
load("C:/Users/Jack/Google Drive/Active Papers/AidProjects/CRS 2016 data/Freedom House.RData")
x$new.code <- countrycode(x$country, origin = 'country.name', destination = 'iso3n')

working <- merge(crs_by_types, x, by.x = c('Year','new.code'), by.y = c('year','new.code'))
working <- working %>%
  filter(!is.na(Year)&!is.na(new.code))

p4v2017d <- read_excel("p4v2017.xls")
polity <- p4v2017d %>%
  filter(year >2004) %>%
  select(country, year, polity,polity2)

polity$new.code <- countrycode(polity$country, origin = 'country.name', destination = 'iso3n')
working <- merge(working, polity, by.x = c('Year','new.code'), by.y = c('year','new.code'), all.x = TRUE)


#Use this code to import and reformat vdem variables from scratch.
#vdem <- read_csv("Country_Year_V-Dem_Extended_CSV_v8/Country_Year_V-Dem_Extended_CSV_v8/V-Dem-CY+Others-v8.csv")
#vdem <- vdem %>%
#  select(country_name,year,COWcode,v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem,v2x_egaldem,
#         e_fh_cl,e_fh_pair,e_fh_pr,e_fh_rol,e_fh_status) %>%
#  filter(year>1993)
#vdem$new.code <- countrycode(vdem$COWcode, origin = 'cown', destination = 'iso3n')

load('./vdem.RData')


working <- merge(working, vdem, by.x = c('Year','new.code'),by.y = c('year','new.code'))

##Pulling in some basic controls from the World Bank
gdp <- wb(country = 'all', indicator = 'NY.GDP.PCAP.CD', startdate = '2003', enddate = '2018') %>%
  rename(gdp = value, year = date) %>%
  select(iso3c, year, gdp)
urban <- wb(country = 'all', indicator = 'SP.URB.TOTL.IN.ZS', startdate = '2003', enddate = '2018') %>%
  rename(urban = value, year = date) %>%
  select(iso3c,year,urban)
pop <- wb(country = 'all', indicator = 'SP.POP.TOTL', startdate = '2003', enddate = '2018') %>%
  rename(pop = value, year = date) %>%
  select(iso3c,year,pop)
controls <- merge(gdp,merge(urban, pop, by = c('iso3c','year')), by = c('iso3c','year'))

controls$new.code <- countrycode(controls$iso3c, origin = 'iso3c', destination = 'iso3n')

controls <- controls %>%
  select(new.code,year,gdp,urban,pop)
working <- merge(working, controls, by.x = c('Year','new.code'), by.y = c('year','new.code'), all.x = TRUE)

attach(working)
working$gov_d_indicator <- ifelse(gov_d_percent == 0,0,1)
working$gov_r_indicator <- ifelse(gov_r_percent == 0,0,1)
working$gov_3_indicator <- ifelse(gov_3_percent == 0,0,1)
working$gov_o_indicator <- ifelse(gov_o_percent == 0,0,1)
working$corp_indicator <- ifelse(corp_percent == 0,0,1)
working$ngo_indicator <- ifelse(ngo_percent == 0,0,1)
working$igo_indicator <- ifelse(igo_percent == 0,0,1)
working$other_indicator <- ifelse(other_percent == 0,0,1)
detach()


dep.vars <- c('v2x_polyarchy','v2x_libdem','v2x_partipdem','v2x_delibdem','v2x_egaldem','polity2','e_fh_cl',
              'e_fh_pair','e_fh_pr','e_fh_rol','e_fh_status')

ind.vars_gov_together <- c('I(gov_d_percent+gov_r_percent+gov_3_percent+gov_o_percent)', 'ngo_percent', 'igo_percent',
              'other_percent', 'log(total_disbursement)', 'log(gdp)','urban','log(pop)')

ind.vars_all <- c('gov_d_percent','gov_r_percent','gov_3_percent','gov_o_percent', 'ngo_percent', 'igo_percent',
                  'corp_percent', 'log(total_disbursement)')

ind.vars_all_controls <- c('gov_d_percent','gov_r_percent','gov_3_percent','gov_o_percent', 'ngo_percent', 'igo_percent',
                           'other_percent', 'log(total_disbursement)', 'log(gdp)','urban','log(pop)')

ind.vars_indicators <- c('gov_d_percent','gov_r_percent','gov_3_percent','gov_o_percent', 'ngo_percent', 'igo_percent',
                         'corp_percent', 'log(total_disbursement)','gov_d_indicator','gov_r_indicator',
                         'gov_3_indicator','gov_o_indicator','corp_indicator','ngo_indicator','igo_indicator',
                         'other_indicator')


for (i in dep.vars){
  form <- as.formula(paste(i, paste(c(ind.vars_gov_together),collapse = '+'),sep = '~'))
  model <- plm(form, model = 'within', data = working, index = c('RecipientCode'))
  assign(x = paste0('mod_',i), value = model)
}
#with lags
for (i in dep.vars){
  form <- as.formula(paste(i, paste(c(ind.vars_gov_together,paste0('lag(',i,')')),collapse = '+'),sep = '~'))
  model <- plm(form, model = 'within', data = working, index = c('RecipientCode'))
  assign(x = paste0('mod_',i), value = model)
}

stargazer(mod_polity2,mod_v2x_polyarchy,mod_v2x_libdem,mod_v2x_partipdem,mod_v2x_delibdem,mod_v2x_egaldem,
          mod_e_fh_cl,mod_e_fh_pair,mod_e_fh_pr,mod_e_fh_rol,mod_e_fh_status, type = 'text', out = './all_ivs_controls_gov_together.txt')

tmp <- crs_data %>%
  select(RecipientCode,RecipientName) %>%
  unique()
