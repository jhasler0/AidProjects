##Jack Hasler
##7/25/2018
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

##First grabbing the channel codes. These were last updated on 7/25/2018.
modified_crs_channel_codes <- read_delim("new_channel_codes.csv",",", escape_double = FALSE, trim_ws = TRUE)

channel_codes <- modified_crs_channel_codes %>%
  select(`Channel ID`, ch_type,ch_identity) %>%
  rename(code = `Channel ID`)
rm(modified_crs_channel_codes)
channel_codes$ch_type <- as.factor(channel_codes$ch_type)
channel_codes$ch_identity <- as.factor(channel_codes$ch_identity)

#Importing dem purpose code dict
dempurposecodes <- read_csv("dempurposecodes.csv")
dempurposecodes <- dempurposecodes %>%
  filter(dem_purpose==1)%>%
  select(PurposeCode)


#These datasets come with 86 varaibles and most years are over 150 MB in size. R is going to start choking 
#aroung 500ish MB, so I'm doing the reshaping by dataset and then merging. This particular function
#is made to select out certain types, but could be easily modified.


crs_reshape <- function(crs_data, ch_codes,dempurposecodes){
  require(tidyverse)
  
  #selects desired variables and filters out nebulous recipients
  crs_data <- crs_data %>%
    select(Year,DonorCode,DonorName,RecipientCode,RecipientName,RegionName,
           usd_commitment,usd_disbursement,usd_received,ChannelCode,PurposeCode,PurposeName) %>%
    filter(!(RecipientCode %in% c(88,189,489,689,389,380,298,798,889,789,679,498,9998,589))) %>%
    filter((PurposeCode %in% dempurposecodes$PurposeCode)) %>%
    filter(DonorCode %in% c(1,2,3,4,5,6,7,8,9,10,11,12,13,18,20,21,22,40,50,61,68,69,75,76,301,302,701,742,801,820,
                            30,45,55,62.70,72,77,82,83,84,87,130,133,358,543,546,552,561,566,576,611,613,732,764,765)) %>%
    filter(!is.na(ChannelCode))
  #merge with channelcodes
  crs_data <- merge(crs_data, channel_codes, by.x = 'ChannelCode', by.y = 'code', all.x = TRUE)
  #counts and aid totals for all types
  crs_data <- crs_data %>%
    group_by(Year,DonorCode,RecipientCode) %>%
    add_count(sort = TRUE) %>%
    mutate(total_commitment = sum(usd_commitment, na.rm = TRUE), total_disbursement = sum(usd_disbursement, na.rm = TRUE), total_received = sum(usd_received, na.rm = TRUE)) %>%
    ungroup()
  #consolidates som of the type codes. Might need to be rewritten later.
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'corporation','corporation',NA)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'publicprivate','corporation',crs_data$ch_type2)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'edu','ngo',crs_data$ch_type2)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'ngo','ngo',crs_data$ch_type2)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'igo','igo',crs_data$ch_type2)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'network','ngo',crs_data$ch_type2)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'government',
                              ifelse(crs_data$ch_identity == 'donor', 'gov_donor',
                                     ifelse(crs_data$ch_identity == 'recipient','gov_recip',ifelse(crs_data$ch_identity == 'thirdparty','gov_3','gov_other'))),
                              crs_data$ch_type2)
  crs_data$ch_type2 <- ifelse(crs_data$ch_type == 'other','other',crs_data$ch_type2)
  crs_data$ch_type2 <- as.factor(crs_data$ch_type2)
  #Does the same for identity, but note that this is not used in the output
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'devbank','international',NA)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'developing','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'donor','donor',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'eu','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'imf','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'international','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'other','other',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'recipient','recipient',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'thirdparty','thirdparty',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'un','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'wb','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- ifelse(crs_data$ch_identity == 'wto','international',crs_data$ch_identity2)
  crs_data$ch_identity2 <- as.factor(crs_data$ch_identity2)
  
  
  #Groups by the consolidated types and sums commitment, received and disbursement. Adds a count.
  crs_data <- crs_data %>%
    group_by(Year,DonorCode,RecipientCode,ch_type2)%>%
    add_count(sort = TRUE) %>%
    mutate(type2_commitment = sum(usd_commitment, na.rm = TRUE), type2_disbursement = sum(usd_disbursement, na.rm = TRUE), type2_received = sum(usd_received, na.rm = TRUE)) %>%
    ungroup()
  #Same for identity
  crs_data <- crs_data %>%
    group_by(Year,DonorCode,RecipientCode,ch_identity2)%>%
    add_count(sort = TRUE) %>%
    mutate(id2_commitment = sum(usd_commitment, na.rm = TRUE), id2_disbursement = sum(usd_disbursement, na.rm = TRUE), id2_received = sum(usd_received, na.rm = TRUE)) %>%
    rename(n_ccy = n,n_type = nn,n_identity =nnn) %>%
    ungroup()
  #Selects out desired variables. NOTE: This is what needs to be changed to get identities instead of types.
  crs_data_type <- crs_data %>%
    select(Year,DonorCode,DonorName,RecipientCode,RecipientName,RegionName,n_ccy,total_disbursement,total_received,
           total_commitment,ch_type2,n_type,type2_commitment,type2_disbursement, type2_received) %>%
    unique() %>%
    arrange(DonorCode,RecipientCode) %>%
    filter(!is.na(ch_type2))
  #return the data frame
  crs_data_type
}

#vector of the file name years. Early ones are grouped together. 1973-94 is omitted as it serves as the base case below.
years <- c('1995-99','2000-01','2002-03','2004-05','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')

#importing and running crs_by_types on the base case
x<- read_delim('./CRS 1973-94 data.txt', "|", escape_double = FALSE, trim_ws = TRUE)
crs_by_types <- crs_reshape(x,channel_codes,dempurposecodes)

#importing, reshaping and appending each of the CRS data files.
for (i in years){
  filepath = paste0('./CRS ',i,' data.txt')
  crs_tmp <- read_delim(filepath,"|", escape_double = FALSE, trim_ws = TRUE)
  tmp_df <- crs_reshape(crs_tmp, channel_codes,dempurposecodes)
  crs_by_types <- rbind(crs_by_types, tmp_df)
}

save(crs_by_types, file='./crs_by_types.RData')


crs_by_types$total_received_mod <- ifelse(crs_by_types$total_received>0, crs_by_types$total_received, NA)
crs_by_types$total_disbursed_mod <- ifelse(crs_by_types$total_disbursement>0, crs_by_types$total_disbursement, NA)
crs_by_types$type_disbursed_mod <- ifelse(crs_by_types$type2_disbursement>0, crs_by_types$type2_disbursement, NA)

crs_by_types$disbursement_type_percent <- crs_by_types$type_disbursed_mod / crs_by_types$total_disbursed_mod

crs_by_types <- crs_by_types %>%
  filter(disbursement_type_percent<=1)

hist(crs_by_types$disbursement_type_percent)

crs_by_types_gd <- crs_by_types %>%
  filter(ch_type2=='gov_donor') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(govd_percent = disbursement_type_percent, govd_count = n_type, govd_total = type2_disbursement)

crs_by_types_gr <- crs_by_types %>%
  filter(ch_type2=='gov_recip') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(govr_percent = disbursement_type_percent, govr_count = n_type, govr_total = type2_disbursement)

crs_by_types_go <- crs_by_types %>%
  filter(ch_type2=='gov_other') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(govo_percent = disbursement_type_percent, govo_count = n_type, govo_total = type2_disbursement)

crs_by_types_g3 <- crs_by_types %>%
  filter(ch_type2=='gov_3') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(gov3_percent = disbursement_type_percent, gov3_count = n_type, gov3_total = type2_disbursement)


crs_by_types_n <- crs_by_types %>%
  filter(ch_type2=='ngo') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(ngo_percent = disbursement_type_percent, ngo_count = n_type, ngo_total = type2_disbursement)

crs_by_types_i <- crs_by_types %>%
  filter(ch_type2=='igo') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(igo_percent = disbursement_type_percent, igo_count = n_type, igo_total = type2_disbursement)


crs_by_types_c <- crs_by_types %>%
  filter(ch_type2=='corporation') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(corp_percent = disbursement_type_percent, corp_count = n_type, corp_total = type2_disbursement)

crs_by_types_o <- crs_by_types %>%
  filter(ch_type2=='other') %>%
  select(Year,DonorCode,RecipientCode,type2_disbursement,disbursement_type_percent, n_type) %>%
  rename(other_percent = disbursement_type_percent, other_count = n_type, other_total = type2_disbursement)

crs_by_types2 <- crs_by_types %>%
  select(Year,DonorCode,DonorName,RecipientCode,RecipientName,RegionName,n_ccy,total_disbursement) %>%
  unique()

crs_by_types2 <- merge(crs_by_types2, crs_by_types_c, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_gd, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_gr, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_go, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_g3, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_o, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_n, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)
crs_by_types2 <- merge(crs_by_types2, crs_by_types_i, by = c('Year','DonorCode','RecipientCode'), all.x = TRUE)


crs_by_types2$corp_percent <- ifelse(is.na(crs_by_types2$corp_percent),0,crs_by_types2$corp_percent)
crs_by_types2$corp_count <- ifelse(is.na(crs_by_types2$corp_count),0,crs_by_types2$corp_count)

crs_by_types2$igo_percent <- ifelse(is.na(crs_by_types2$igo_percent),0,crs_by_types2$igo_percent)
crs_by_types2$igo_count <- ifelse(is.na(crs_by_types2$igo_count),0,crs_by_types2$igo_count)

crs_by_types2$ngo_percent <- ifelse(is.na(crs_by_types2$ngo_percent),0,crs_by_types2$ngo_percent)
crs_by_types2$ngo_count <- ifelse(is.na(crs_by_types2$ngo_count),0,crs_by_types2$ngo_count)

crs_by_types2$govd_percent <- ifelse(is.na(crs_by_types2$govd_percent),0,crs_by_types2$govd_percent)
crs_by_types2$govd_count <- ifelse(is.na(crs_by_types2$govd_count),0,crs_by_types2$govd_count)
crs_by_types2$govr_percent <- ifelse(is.na(crs_by_types2$govr_percent),0,crs_by_types2$govr_percent)
crs_by_types2$govr_count <- ifelse(is.na(crs_by_types2$govr_count),0,crs_by_types2$govr_count)
crs_by_types2$govo_percent <- ifelse(is.na(crs_by_types2$govo_percent),0,crs_by_types2$govo_percent)
crs_by_types2$govo_count <- ifelse(is.na(crs_by_types2$govo_count),0,crs_by_types2$govo_count)
crs_by_types2$gov3_percent <- ifelse(is.na(crs_by_types2$gov3_percent),0,crs_by_types2$gov3_percent)
crs_by_types2$gov3_count <- ifelse(is.na(crs_by_types2$gov3_count),0,crs_by_types2$gov3_count)


crs_by_types2$other_percent <- ifelse(is.na(crs_by_types2$other_percent),0,crs_by_types2$other_percent)
crs_by_types2$other_count <- ifelse(is.na(crs_by_types2$other_count),0,crs_by_types2$other_count)

crs_by_types2$corp_total <- ifelse(is.na(crs_by_types2$corp_total),0,crs_by_types2$corp_total)
crs_by_types2$igo_total <- ifelse(is.na(crs_by_types2$igo_total),0,crs_by_types2$igo_total)
crs_by_types2$ngo_total <- ifelse(is.na(crs_by_types2$ngo_total),0,crs_by_types2$ngo_total)
crs_by_types2$govd_total <- ifelse(is.na(crs_by_types2$govd_total),0,crs_by_types2$govd_total)
crs_by_types2$govr_total <- ifelse(is.na(crs_by_types2$govr_total),0,crs_by_types2$govr_total)
crs_by_types2$govo_total <- ifelse(is.na(crs_by_types2$govo_total),0,crs_by_types2$govo_total)
crs_by_types2$gov3_total <- ifelse(is.na(crs_by_types2$gov3_total),0,crs_by_types2$gov3_total)
crs_by_types2$other_total <- ifelse(is.na(crs_by_types2$other_total),0,crs_by_types2$other_total)


save(crs_by_types2, file = './typed_disbursement.RData')

crs_by_types2$new.code <- countrycode(crs_by_types2$RecipientName, origin = 'country.name', destination = 'iso3n')
load("C:/Users/Jack/Google Drive/Active Papers/AidProjects/CRS 2016 data/Freedom House.RData")
x$new.code <- countrycode(x$country, origin = 'country.name', destination = 'iso3n')

working <- merge(crs_by_types2, x, by.x = c('Year','new.code'), by.y = c('year','new.code'))
working2 <- working %>%
  filter(!is.na(Year)&!is.na(new.code))
working2$d.code <- countrycode(working2$DonorName, origin = 'country.name', destination = 'iso3n')


p4v2017d <- read_excel("p4v2017.xls")
polity <- p4v2017d %>%
  filter(year >2004) %>%
  select(country, year, polity,polity2)

polity$new.code <- countrycode(polity$country, origin = 'country.name', destination = 'iso3n')
working3 <- merge(working2, polity,  by.x = c('Year','new.code'), by.y = c('year','new.code'), all.x = TRUE)
vdem <- read_csv("Country_Year_V-Dem_Extended_CSV_v8/Country_Year_V-Dem_Extended_CSV_v8/V-Dem-CY+Others-v8.csv")
vdem <- vdem %>%
  select(country_name,year,COWcode,v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem,v2x_egaldem,
         e_fh_cl,e_fh_pair,e_fh_pr,e_fh_rol,e_fh_status) %>%
  filter(year>1993)
vdem$new.code <- countrycode(vdem$COWcode, origin = 'cown', destination = 'iso3n')
working3 <- merge(working3, vdem, by.x = c('Year','new.code'),by.y = c('year','new.code'))

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
working3 <- merge(working3, controls, by.x = c('Year','new.code'), by.y = c('year','new.code'), all.x = TRUE)


#formula <- c('log(govd_total)', 'log(govr_total)', 'log(gov3_total)', 'log(govo_total)', 'log(ngo_total)', 'log(igo_total)', 'log(corp_total)', 'log(total_disbursement)', 'log(pop)', 'urban', 'log(gdp)')


  #
formula <- c('govd_percent', 'govr_percent', 'gov3_percent', 'govo_percent', 'ngo_percent', 'igo_percent', 'corp_percent', 'log(total_disbursement)')


mod_vdem_polyarchy <- plm(as.formula(paste('v2x_polyarchy ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_vdem_libdem <- plm(as.formula(paste('v2x_libdem ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_vdem_partipdem <- plm(as.formula(paste('v2x_partipdem ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_vdem_delibdem <- plm(as.formula(paste('v2x_delibdem ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_vdem_egaldem <- plm(as.formula(paste('v2x_egaldem ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_polity <- plm(as.formula(paste('polity2 ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_fh_civil <- plm(as.formula(paste('e_fh_cl ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_fh_pair <- plm(as.formula(paste('e_fh_pair ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_fh_pr <- plm(as.formula(paste('e_fh_pr ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_fh_rol <- plm(as.formula(paste('e_fh_rol ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
mod_fh_status <- plm(as.formula(paste('e_fh_status ~ ',paste(formula,collapse = '+'))), data = working3, model = 'within', index = c('new.code'))
  

stargazer(mod_vdem_polyarchy,mod_vdem_partipdem,mod_vdem_libdem,mod_vdem_egaldem,mod_vdem_delibdem,mod_polity,mod_fh_civil,mod_fh_pair,mod_fh_pr,mod_fh_rol,mod_fh_status, type = 'text')

summary(mod1)

mod2 <- lm(polity2 ~ govd_percent + govr_percent + gov3_percent + govo_percent + ngo_percent + corp_percent,data = working3)

crs_by_types_all <- crs_by_types2

###GOTTA FIX NEGATIVE DISBURSEMENTS