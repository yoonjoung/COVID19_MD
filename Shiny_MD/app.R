library(shiny)

library(dplyr)
library(tidyverse)
library(readxl)

library(Matrix)
library(lubridate)
library(stringr)
library(stringi)

library(jsonlite)
library(httr)
library(rlist)
library(zoo)

library(plotly)
library(RColorBrewer) 

#library(conflicted)
#conflict_prefer("filter", "dplyr")
#conflict_prefer("rename", "dplyr")
#conflict_prefer("mutate", "dplyr")
#conflict_prefer("arrange", "dplyr")

date<-as.Date(Sys.time(	), format='%d%b%Y')

# This creates shiny app to display Maryland Covid data  
# There are four parts in this document:
# 0. Database update 
# 1. USER INTERFACE 
# 2. SERVER
# 3. CREATE APP     

#******************************
# 0. Database update 
#******************************

#setwd("~/Dropbox/0 Project/COVID19_US/Shiny_MD/")

# 0.1 Get population data ----
fips<-read_excel("MD_county_fips.xlsx")%>%
    select(fips, county)%>%
    mutate(fips=as.character(fips))

dtapopcounty<-read.csv("co-est2019-alldata.csv")
names(dtapopcounty)<- tolower(names(dtapopcounty))   
dtapopcounty<-dtapopcounty%>%select(stname, ctyname, popestimate2019)%>%
    filter(stname=="Maryland")%>%
    mutate(pop=popestimate2019, 
           state=as.character(stname), 
           county=str_remove(as.character(ctyname), " County")
    )%>%
    select(state, county, pop)%>%
    filter(state=="Maryland")%>%
    select(county, pop)

dtapopbyage<-read_excel("CNTY-PopEst-2018.xlsx")%>%
    select(agegroup, pop10)%>%
    filter(is.na(pop10)==F)%>%
    filter(agegroup!="Total")%>%
    mutate(    
        age=as.numeric(sapply(strsplit(agegroup,"-"), `[`, 1)), 
        pop=pop10
    )%>%
    select(age, pop)

popMD<-dtapopcounty%>%filter(county=="Maryland")%>%select(pop)
popMD<-as.numeric(mean(popMD$pop))

dtapopbyrace<-read_excel("table1a.xlsx")%>%
    filter(is.na(pop)==F)%>%
    mutate(
        popMD=as.numeric(popMD),
        pop=round(popMD*pctpop, 0)
    )%>%select(race, pop)%>%
    group_by(race)%>%summarize_all(funs(sum))%>%filter(is.na(pop)==F)

# 0.2 Get API COVID data for MD from MDH ---- 

url<-("https://opendata.arcgis.com/datasets/18582de727934249b92c52542395a3bf_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

cases<-dta%>%
    mutate(date=as.Date(DATE))%>%
    select(-OBJECTID, -DATE)%>%
    rename(cases = Count_)

url<-("https://opendata.arcgis.com/datasets/096cca5f77404a06babb9367530136b9_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

deaths<-dta%>%
    mutate(date=as.Date(DATE))%>%
    select(-OBJECTID, -DATE)%>%
    rename(deaths=Count_)

url<-("https://opendata.arcgis.com/datasets/e5d044bda3a44bfa8ba224145617cda0_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

newtests<-dta%>%
    
    #beginning of edit 9/30/2022
    #hourly data in each row? keep only one row per day
    arrange(date)%>%
    filter(date!=lag(date))%>%
    #end of edit 9/30/2022
    
    mutate(date=as.Date(date))%>%
    select(-OBJECTID)%>%
    rename(newtests=number_of_tests,
           newpositivetests=number_of_positives)%>%
    select(-rolling_avg)

url<-("https://opendata.arcgis.com/datasets/67c49f40064c45f9aadfcc9298cba9e6_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

negativetests<-dta%>%
    mutate(date=as.Date(ReportDate))%>%
    select(-OBJECTID, -ReportDate)%>%
    rename(negativetests=NegativeTests)

url<-("https://opendata.arcgis.com/datasets/bf3f201b056b4c488b5dac3441b7ac20_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]
names(dta)<- tolower(names(dta))   
hospital<-dta%>%
    mutate(date=as.Date(date))%>%
    select(-objectid)%>%
    rename(
        hospitalizations_current=total)

url<-("https://data.cdc.gov/resource/unsk-b7fc.json?location=MD&$limit=100000")
jsondata<-fromJSON(url) 
covax<-jsondata%>%
    filter(location=="MD")%>%
    mutate(
        date=as.Date(date),
        #date=as.Date(date, format = "%Y/%m/%d"),
        covaxcomp_cumpct=as.numeric(series_complete_pop_pct),
        covaxone_cumpct=as.numeric(administered_dose1_pop_pct), 
        covaxbooster_cumpct=as.numeric(additional_doses_vax_pct)
    )%>%
    select(date, location, starts_with("covax"))        

# 0.3 Append API COVID data for MD from MDH ---- 

dtaMD<-left_join(cases, deaths, by = c("date"))
dim(dtaMD)
dtaMD<-left_join(dtaMD, newtests, by = c("date"))
dim(dtaMD)   
#dtaMD<-left_join(dtaMD, negativetests, by = c("date"))
#dim(dtaMD)   
dtaMD<-left_join(dtaMD, hospital, by = c("date"))
dim(dtaMD)   
dtaMD<-left_join(dtaMD, covax, by = c("date"))
dim(dtaMD)           

dtaMD<-dtaMD%>%
    mutate(    
        #totaltests=cases+negativetests
        totaltests=newtests #edit on 9/30/2022
    )%>%
    arrange(date)%>%
    
    mutate( #deaths data released only on business days
        deaths=ifelse(is.na(deaths)==TRUE, lag(deaths), deaths),
        deaths=ifelse(is.na(deaths)==TRUE, lag(deaths), deaths),
        deaths=ifelse(is.na(deaths)==TRUE, lag(deaths), deaths)
    )%>%    
    mutate(
        newcases=lead(cases)-cases,
        newdeaths=lead(deaths)-deaths,
        newtests_calculated=lead(totaltests)-totaltests, 
        
        positiverate=percent_positive, 
        
        admission=hospitalizations_current,
        admission=as.numeric(admission),
        icu=as.numeric(icu),
        
        positiveratesmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                    rollmean(positiverate, 14)), 1), 
        positiveratesmooth =ifelse(positiveratesmooth<0,  0, positiveratesmooth), 
        admissionsmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                 rollmean(admission, 14)), 1), 
        admissionsmooth =ifelse(admissionsmooth<0,  0, admissionsmooth), 
        icusmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                           rollmean(icu, 14)), 1), 
        icusmooth =ifelse(icusmooth<0,  0, icusmooth), 
        newcasessmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                rollmean(newcases, 14)), 1),
        newcasessmooth =ifelse(newcasessmooth<0,  0, newcasessmooth), 
        newdeathssmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                 rollmean(newdeaths, 14)), 7),
        newdeathssmooth =ifelse(newdeathssmooth<0,  0, newdeathssmooth), 
        
        newcasessmoothpp= round(100000*newcasessmooth/popMD, 1),   
        newdeathssmoothpp=round(100000*newdeathssmooth/popMD, 5),
        
        #color for the recent time
        last2weeks  = as.Date(Sys.time(	), format='%d%b%Y')-date<=14,
        last2months = as.Date(Sys.time(	), format='%d%b%Y')-date<=60,
        
        newcasessmooth_2m=newcasessmooth,
        newcasessmooth_2m=ifelse(last2months==FALSE, NA, newcasessmooth_2m),        
        newcasessmooth_2w=newcasessmooth,
        newcasessmooth_2w=ifelse(last2weeks==FALSE, NA, newcasessmooth_2w), 
        
        admissionsmooth_2m=admissionsmooth,
        admissionsmooth_2m=ifelse(last2months==FALSE, NA, admissionsmooth_2m),        
        admissionsmooth_2w=admissionsmooth,
        admissionsmooth_2w=ifelse(last2weeks==FALSE, NA, admissionsmooth_2w), 
        
        icusmooth_2m=icusmooth,
        icusmooth_2m=ifelse(last2months==FALSE, NA, icusmooth_2m),        
        icusmooth_2w=icusmooth,
        icusmooth_2w=ifelse(last2weeks==FALSE, NA, icusmooth_2w), 
        
        covaxcomp_cumpct_2m=covaxcomp_cumpct,
        covaxcomp_cumpct_2m=ifelse(last2months==FALSE, NA, covaxcomp_cumpct_2m),        
        covaxcomp_cumpct_2w=covaxcomp_cumpct,
        covaxcomp_cumpct_2w=ifelse(last2weeks==FALSE, NA, covaxcomp_cumpct_2w) 

    )

dtaMDsummary<-dtaMD%>%
    rename(
        newcasessmoothppMD = newcasessmoothpp,
        newdeathssmoothppMD = newdeathssmoothpp,
        positiveratesmoothMD = positiveratesmooth, 
        covaxcomp_cumpctMD = covaxcomp_cumpct
           )%>%
    select(date, ends_with("MD"))

# 0.4 Get API COVID data for MD by county from MDH  ----

url<-("https://opendata.arcgis.com/datasets/0573e90adab5434f97b082590c503bc1_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]
countylist<-as.vector(colnames(dta))[-c(1:2)]

casesbycounty<-dta%>%
    mutate(date=as.Date(DATE))%>%
    select(-OBJECTID, -DATE)%>%
    gather(county, cases, countylist)%>%
    mutate(
        county=ifelse(county=="Anne_Arundel", "Anne Arundel", county), 
        county=ifelse(county=="AnneArundel", "Anne Arundel", county), 
        county=ifelse(county=="Baltimore_City", "Baltimore city", county), 
        county=ifelse(county=="Baltimore_city", "Baltimore city", county), 
        county=ifelse(county=="Baltimore City", "Baltimore city", county), 
        county=ifelse(county=="BaltimoreCity", "Baltimore city", county), 
        county=ifelse(county=="Prince_Georges", "Prince George's", county), 
        county=ifelse(county=="Queen_Annes", "Queen Anne's", county), 
        county=ifelse(county=="St_Marys", "St. Mary's", county)       
    )    

url<-("https://opendata.arcgis.com/datasets/3dbd3e633b344c7c9a0d166b1d6a2b03_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]
countylist<-as.vector(colnames(dta))[-c(1:2)]

deathsbycounty<-dta%>%
    mutate(date=as.Date(DATE))%>%
    select(-OBJECTID, -DATE)%>%
    gather(county, deaths, countylist)%>%
    mutate(
        county=ifelse(county=="Anne_Arundel", "Anne Arundel", county), 
        county=ifelse(county=="AnneArundel", "Anne Arundel", county), 
        county=ifelse(county=="Baltimore_City", "Baltimore city", county), 
        county=ifelse(county=="Baltimore_city", "Baltimore city", county), 
        county=ifelse(county=="Baltimore City", "Baltimore city", county), 
        county=ifelse(county=="BaltimoreCity", "Baltimore city", county), 
        county=ifelse(county=="Prince_Georges", "Prince George's", county), 
        county=ifelse(county=="Queen_Annes", "Queen Anne's", county), 
        county=ifelse(county=="St_Marys", "St. Mary's", county)     
    )    

url<-("https://opendata.arcgis.com/datasets/853593daf977435aaa909b334445542c_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]    
datelist<-as.vector(colnames(dta))[-c(1:2)]

testsbycounty<-dta%>%
    select(-OBJECTID)%>%
    gather(date, tests, datelist)%>%
    rename(county=County)%>%
    mutate(
        date=substring(date, 3),
        date=gsub("\\_", "/", date),
        date=as.Date(date, format = "%m/%d/%Y")
    )%>%
    mutate(
        county=ifelse(county=="Anne_Arundel", "Anne Arundel", county), 
        county=ifelse(county=="AnneArundel", "Anne Arundel", county), 
        county=ifelse(county=="Baltimore_City", "Baltimore city", county), 
        county=ifelse(county=="Baltimore_city", "Baltimore city", county), 
        county=ifelse(county=="Baltimore City", "Baltimore city", county), 
        county=ifelse(county=="BaltimoreCity", "Baltimore city", county), 
        county=ifelse(county=="Prince_Georges", "Prince George's", county), 
        county=ifelse(county=="Queen_Annes", "Queen Anne's", county), 
        county=ifelse(county=="St_Marys", "St. Mary's", county)    
    )    

url<-("https://opendata.arcgis.com/datasets/54d028e340f44e88945e26579eecd447_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]    

dta<-dta%>%
    select(-ends_with("Percent_Positive"))%>% #two indicators. Keep only the rolling avearage positivity rate
    rename(date=ReportDate)%>%
    mutate(
        date=substring(date, 1, 10),
        date=as.Date(date, format = "%Y-%m-%d")
    )

countylist<-as.vector(colnames(dta))[-c(1:2)]

prbycounty<-dta%>%
    select(-OBJECTID)%>%
    gather(county, positiverate, countylist)%>%
    mutate(
        county=gsub( "_Rolling_Avg", "", county)
    )%>%
    mutate(
        county=ifelse(county=="PrinceGeorges", "Prince George's", county), 
        county=ifelse(county=="QueenAnnes", "Queen Anne's", county), 
        county=ifelse(county=="StMarys", "St. Mary's", county),      
        
        county=ifelse(county=="Anne_Arundel", "Anne Arundel", county), 
        county=ifelse(county=="AnneArundel", "Anne Arundel", county), 
        county=ifelse(county=="Baltimore_City", "Baltimore city", county), 
        county=ifelse(county=="Baltimore_city", "Baltimore city", county), 
        county=ifelse(county=="Baltimore City", "Baltimore city", county), 
        county=ifelse(county=="BaltimoreCity", "Baltimore city", county), 
        county=ifelse(county=="Prince_Georges", "Prince George's", county), 
        county=ifelse(county=="Queen_Annes", "Queen Anne's", county), 
        county=ifelse(county=="St_Marys", "St. Mary's", county)      
    )%>%
    mutate(positiverate = as.numeric(positiverate))

url<-("https://data.cdc.gov/resource/8xkx-amqh.json?recip_state=MD&$limit=100000")

jsondata<-fromJSON(url) 
#str(jsondata)
dim(jsondata)
colnames(jsondata[, 1:6])
table(jsondata$recip_state)

covaxbycounty<-jsondata%>%
    filter(recip_state=="MD")%>%
    select(date, recip_county, completeness_pct)%>%
    rename(county = recip_county)%>%
    rename(covaxcomp_cumpct = completeness_pct)%>%
    mutate(
        county=gsub( " County", "", county), 
        date=as.Date(date)
    )

# 0.5 Append API COVID data for MD by county from MDH  ----

dtabycounty<-left_join(casesbycounty, deathsbycounty, 
                       by = c("county", "date"))
dtabycounty<-left_join(dtabycounty, testsbycounty, 
                       by = c("county", "date"))
dtabycounty<-left_join(dtabycounty, prbycounty, 
                       by = c("county", "date"))
dtabycounty<-left_join(dtabycounty, covaxbycounty, 
                       by = c("county", "date"))
#   BUT covax data are still problematic.....

dtabycounty<-left_join(dtabycounty, dtapopcounty, 
                       by = "county")
dtabycounty<-left_join(dtabycounty, fips, 
                       by = "county")

dtabycounty<-dtabycounty%>%
    arrange(county, date)%>%
    filter(county!="Maryland" & county!="Unknown")%>%
    mutate(group=county)%>%
    
    group_by(group)%>%
    mutate(
        day = row_number(),
        maxday = max(day),
        retroday = maxday - day #retrospective day
    )%>%
    
    #Argh no deaths data on the weekend starting from April
    #Makes sense but what a pain to adjust code...!!! 
    arrange(group, desc(date))%>%
    mutate( #deaths data released only on business days
        deaths=ifelse(is.na(deaths)==TRUE, lag(deaths), deaths),
        deaths=ifelse(is.na(deaths)==TRUE, lag(deaths), deaths),
        deaths=ifelse(is.na(deaths)==TRUE, lag(deaths), deaths)
        )%>%
    
    arrange(group, date)%>%    
    mutate(
        #Tests
        newtests=tests-lag(tests),
        newtests=ifelse(group!=lag(group), NA, newtests), 
        newtests=ifelse(newtests<0,    0, newtests),
        
        newtestssmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                               rollmean(newtests, 14)), 1), 
        newtestssmooth=ifelse(group!=lag(group), NA, newtestssmooth),
        
        newtestspp=      round(100000*newtests/pop, 1),
        newtestssmoothpp=round(100000*newtestssmooth/pop, 5),
        
        #Cases
        newcases=cases-lag(cases),
        newcases=ifelse(group!=lag(group), NA, newcases), 
        newcases=ifelse(newcases<0,    0, newcases),
        
        newcasessmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                rollmean(newcases, 14)), 1), 
        newcasessmooth=ifelse(group!=lag(group), NA, newcasessmooth),
        
        newcasespp=      round(100000*newcases/pop, 1),
        newcasessmoothpp=round(100000*newcasessmooth/pop, 5), 
        
        #Deaths
        newdeaths=deaths-lag(deaths),
        newdeaths=ifelse(group!=lag(group), NA, newdeaths), 
        newdeaths=ifelse(newdeaths<0,    0, newdeaths),
        
        newdeathssmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                 rollmean(newdeaths, 14)), 9), 
        newdeathssmooth=ifelse(group!=lag(group), NA, newdeathssmooth),
        
        newdeathspp=      round(100000*newdeaths/pop, 5),
        newdeathssmoothpp=round(100000*newdeathssmooth/pop, 5), 
        
        #Positivity rate
        positiveratesmooth =round(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                                      rollmean(positiverate, 14)), 1), 
        positiveratesmooth=ifelse(group!=lag(group), NA, positiveratesmooth),
        
        #color for the recent time
        last2weeks  = as.Date(Sys.time(	), format='%d%b%Y')-date<=14,
        last2months = as.Date(Sys.time(	), format='%d%b%Y')-date<=60,
        
        newcasessmoothpp_2m=newcasessmoothpp,
        newcasessmoothpp_2m=ifelse(last2months==FALSE, NA, newcasessmoothpp_2m),        
        newcasessmoothpp_2w=newcasessmoothpp,
        newcasessmoothpp_2w=ifelse(last2weeks==FALSE, NA, newcasessmoothpp_2w), 
        
        newdeathssmoothpp_2m=newdeathssmoothpp,
        newdeathssmoothpp_2m=ifelse(last2months==FALSE, NA, newdeathssmoothpp_2m),        
        newdeathssmoothpp_2w=newdeathssmoothpp,
        newdeathssmoothpp_2w=ifelse(last2weeks==FALSE, NA, newdeathssmoothpp_2w), 
        
        positiveratesmooth_2m=positiveratesmooth,
        positiveratesmooth_2m=ifelse(last2months==FALSE, NA, positiveratesmooth_2m),        
        positiveratesmooth_2w=positiveratesmooth,
        positiveratesmooth_2w=ifelse(last2weeks==FALSE, NA, positiveratesmooth_2w),
        
        covaxcomp_cumpct_2m=covaxcomp_cumpct,
        covaxcomp_cumpct_2m=ifelse(last2months==FALSE, NA, covaxcomp_cumpct_2m),        
        covaxcomp_cumpct_2w=covaxcomp_cumpct,
        covaxcomp_cumpct_2w=ifelse(last2weeks==FALSE, NA, covaxcomp_cumpct_2w) 
    )%>%
    group_by(group)%>%
    mutate(
        peakcasespp=round(max(newcasessmoothpp, na.rm = TRUE), 5), 
        peakdate=as.character(date),
        peakdate=ifelse(peakcasespp!=newcasessmoothpp, "", peakdate),
        peakdate=ymd(substring(peakdate, 1)),      
        
        total=max(cases, na.rm = TRUE),   
        bottom=min(newcasessmooth, na.rm = TRUE),
        current=newcasessmooth, 
        changeratio=round(current/bottom, 2),
        changediff =current-bottom)%>%
    ungroup()

# 0.6 Get MD data by age ----
url<-("https://opendata.arcgis.com/datasets/68fbe34617cd450aa423e27692f503b0_0.geojson")
jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

    agelist<-c("Age_0_to_9", "Age_10_to_19", "Age_20_to_29", 
               "Age_30_to_39", "Age_40_to_49", "Age_50_to_59", 
               "Age_60_to_69", "Age_70_to_79", "Age_80plus", "Age_Unknown") 

    casesbyage<-dta%>%
        mutate(date=as.Date(DATE))%>%
        select(-OBJECTID, -DATE)%>%
        gather(agegroup, cases, agelist)    

url<-("https://opendata.arcgis.com/datasets/b51451a222cd4c3d89a58c3393212cc7_0.geojson")
jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

    deathsbyage<-dta%>%
        mutate(date=as.Date(DATE))%>%
        select(-OBJECTID, -DATE)%>%
        gather(agegroup, deaths, agelist)

dtabyage<-left_join(casesbyage, deathsbyage, 
                        by = c("agegroup", "date"))%>%
        mutate(
            agegroup=substring(agegroup, 5),
            agegroup=gsub("\\_to_", "-", agegroup),
            agegroup=ifelse(agegroup=="80plus", "80+", agegroup),
            age=as.numeric(sapply(strsplit(agegroup,"-"), `[`, 1)), 
            age=ifelse(agegroup=="80+", 80, age)
        )%>%
        arrange(agegroup, date)%>%
        mutate( #MDH started publishing deaths data only on week days
            deaths=ifelse(agegroup==lag(agegroup) & is.na(deaths)==TRUE, lag(deaths), deaths),
            deaths=ifelse(agegroup==lag(agegroup) & is.na(deaths)==TRUE, lag(deaths), deaths),
            deaths=ifelse(agegroup==lag(agegroup) & is.na(deaths)==TRUE, lag(deaths), deaths)
        )

dtabyage<-left_join(dtabyage, dtapopbyage, by = "age")%>%
        mutate(
            incidence=round(cases*100000/pop, 1), 
            cfr=round(100*deaths/cases, 3), 
            mortality=round(100000*(deaths)/pop, 3),
            temp=max(date),
            latest=date==temp
        )%>%select(-temp)

dtabyagemonthly<-dtabyage%>%
    filter(is.na(age)==FALSE)%>%
    mutate(group=agegroup)%>%
    arrange(group, date)%>%
    mutate(
        newcases=cases-lag(cases),
        newcases=ifelse(group!=lag(group), NA, newcases), 
        newcases=ifelse(newcases<0,    0, newcases),
        
        newdeaths=deaths-lag(deaths),
        newdeaths=ifelse(group!=lag(group), NA, newdeaths), 
        newdeaths=ifelse(newdeaths<0,    0, newdeaths),        
        
        #week=as.numeric(strftime(date, format = "%V")),
        #totalnewcasesweekly=sum(newcases),
        latest  =max(date, na.rm = TRUE),
        retromonth =  floor((latest-date)/30),
        totalnewcasesmonthly=sum(newcases),
        numberofdays=1
    )%>%
    group_by(retromonth)%>%
    mutate(
        retromonthdate=max(date, na.rm = TRUE))%>%
    select(group, retromonthdate, pop, newcases, newdeaths, numberofdays)%>%
    group_by(group, retromonthdate)%>%
    summarize_all(sum)%>%
    ungroup()%>%
    group_by(retromonthdate)%>%
    mutate(
        
        totalnewcases=sum(newcases, na.rm = TRUE),
        totalnewdeaths=sum(newdeaths, na.rm = TRUE),
        
        pctnewcases=round(100*newcases/totalnewcases,1), 
        pctnewdeaths=round(100*newdeaths/totalnewdeaths,1))%>%
    ungroup()%>%
    arrange(retromonthdate)

# 0.7 Get MD data by race---- 
url<-("https://opendata.arcgis.com/datasets/2362c46380aa48f9a18f88571d2090bb_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

    racelist<-c("African_American", "White", "Hispanic", "Asian", "Other", "Not_Available")

    casesbyrace<-dta%>%
        mutate(date=as.Date(DATE))%>%
        select(-OBJECTID, -DATE)%>%
        gather(race, cases, racelist)    

url<-("https://opendata.arcgis.com/datasets/312715a843064ef18879eb726f64c63a_0.geojson")

jsondata<-fromJSON(url) 
jsondta<-jsondata[[4]]
dta<-jsondta[[2]]

    deathsbyrace<-dta%>%
        mutate(date=as.Date(DATE))%>%
        select(-OBJECTID, -DATE)%>%
        gather(race, deaths, racelist)

dtabyrace<-left_join(casesbyrace, deathsbyrace, by = c("race", "date"))%>%
    mutate(race=ifelse(race=="African_American", "African-American", race),
           race=ifelse(race=="Not_Available", "Unknown", race))

dtabyrace<-left_join(dtabyrace, dtapopbyrace, by = "race")%>%
    mutate(
        incidence=round(cases*100000/pop, 1), 
        cfr=round(100*deaths/cases, 3), 
        mortality=round(100000*(deaths)/pop, 3),
        temp=max(date),
        latest=date==temp
    )%>%select(-temp)

# 0.8 Get county shape file data----
library(rjson)
url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)

# 0.9 Define county and other input list -----
countylist<-as.vector(unique(dtabycounty$county))
retrodaylist<-c("All", "Last 6 months")

#******************************####
# 1. USER INTERFACE 
#******************************

ui<-fluidPage(
    
    # Header panel 
    headerPanel("COVID-19 in Maryland, US: Daily Updates and Insights with Granular Local Data"),
    
    # Title panel 
    titlePanel(" "),
    
    # Side panel: define input and output   
    #sidebarLayout(
    #    fluid = TRUE,
    #    # Side panel for inputs: only ONE in this case
    #    sidebarPanel(
    #        style = "position:fixed;width:inherit;", 
    #        width = 3,
    #        h4(strong("Select a county for Part 3: Trends by county")),
    #        br(),
    #        selectInput("county", 
    #                    "Select a county",
    #                    choices = countylist, 
    #                    selected = "Anne Arundel")
    #    ),
        
    # Main page for output display 
    mainPanel(
            width = 10,
            
            tabsetPanel(type = "tabs",

                        tabPanel("Part 1: Overall trends in Maryland",                    

                                 h4("New daily hospital admission cases and new daily confirmed cases, 14-day average"),    
                                 plotlyOutput("plot_MD_trends"), 
                                 
                                 h6(span(style="color: #B41F77", "New daily number of ICU cases (14-day average) - see left axis."),
                                    "Darker segment of the line at the right end is last two weeks"),
                                 h6(span(style="color: #D98DB9", "New daily number of ICU + hospitalization cases (14-day average) - see left axis."), 
                                    "Darker segment of the line at the right end is last two weeks"),
                                 h6(span(style="color: #1F77B4", "New daily number of confirmed cases (14-day average) - see right axis."), 
                                    "Darker segment of the line at the right end is last two weeks"),
                                 
                                 h4("Age composition among monthly new cases and new deaths over time"), 
                                 plotlyOutput("plot_MD_trends_age"), 
                                  
                                 h4("Cumulative incidence and mortality rate by race over time"),
                                 plotlyOutput("plot_MD_trends_race"), 
                                 
                                 hr(), 
                                 h6("Data source:", 
                                    a("Maryland Department of Health", href="https://coronavirus.maryland.gov/"),
                                    "as of", date, ".",
                                    strong("See Annex"), "for details."),
                                 h6("See", 
                                    a("GitHub",href="https://github.com/yoonjoung/PMA_Access"),
                                    "for more information."),
                                 h6("Application last updated on March 1, 2022."),
                                 h6("For typos, errors, and questions:", 
                                    a("contact YJ Choi at www.iSquared.global", href="https://www.isquared.global/YJ")), 
                                 #h4(strong("See Annex"), em("availability"), "and", em("methods")),
                                 
                                 hr()
                        ),            
                        
                        tabPanel("Part 2: Latest snapshot by county",                    

                                 h4("Test positivity rate (%), in the past 14 days"),    
                                 plotlyOutput("plot_county_cases_map"), 
                                                                  
                                 h4("New daily cases (14-day average) and cumulative cases per 100,000 population across the 24 counties"),    
                                 plotlyOutput("plot_county_cases"), 
                                 
                                 hr(), 
                                 h4("New daily deaths (14-day average) and cumulative deaths per 100,000 population across the 24 counties"),    
                                 plotlyOutput("plot_county_deaths"), 

                                 hr()
                        ),
                        
                        tabPanel("Part 3: Trends by county",         
                                 
                                 selectInput("county", 
                                            "Select a county",
                                            choices = countylist, 
                                            selected = "Anne Arundel"), 
                                 
                                 selectInput("retroday", 
                                             "Select a period",
                                             choices = retrodaylist, 
                                             selected = "Last 6 months"),   
                                 #h4("The following output is trends of new confirmed cases and test positivity rate in:"),
                                 #verbatimTextOutput("text_county"),

                                 hr(),
                                 h4("New daily cases per 100,000 population, 14-day average"),    
                                 plotlyOutput("plot_county_trend_cases"), 

                                 h6(
                                    span(style="color: #e66101", "The orange segment at the right end, last 14 days."), 
                                    span(style="color: #5e3c99", "The puple segment of the line is the last 60 days."),
                                    span(style="color: #ABABAB", "The light gray line is the Maryland state-level data.")), 
                                 
                                 #hr(),
                                 #h4("New daily deaths per 100,000 population, 14-day average"),    
                                 #plotlyOutput("plot_county_trend_deaths"), 
                                 
                                 hr(),
                                 h4("Positivity rate (%), 14-day average"),    
                                 plotlyOutput("plot_county_trend_positivity"), 
                                
                                 h6(
                                     span(style="color: #e66101", "The orange segment at the right end, last 14 days."), 
                                     span(style="color: #5e3c99", "The puple segment of the line is the last 60 days."),
                                     span(style="color: #ABABAB", "The light gray line is the Maryland state-level data.")), 
                                 
                                 #hr(),
                                 #h4("Percentage of population who have been fully vaccinated, cumulative"),    
                                 #plotlyOutput("plot_county_trend_covax"), 
                                 
                                 hr()
                                 
                        ),
                        
                        tabPanel("Annex",           
                                 
                                 h4("For further data, including age pattern and disparity by race and ethnicity,",
                                    a("visit this site.", 
                                      href="https://rpubs.com/YJ_Choi/COVID19_Maryland_V2")),
                                 h4(strong("Data sources")),
                                 h5("1. All COVID-19 data on testing, confirmed cases, and hospitalization come from Maryland Department of Health, via",
                                    a("Open Data Portal.", 
                                      href="https://opendata.maryland.gov/"), 
                                    "Maryland COVID-19 Data Dashboard presents these data available at Open Data Portal as well as some not publicaly accessible.", 
                                    "Hospitalization data at the county level are not publicaly accessible."),
                                 h5("2. All vaccination data by county come from", 
                                    a("US Centers for Disease Control and Prevention.", 
                                      href="https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh")),                                 
                                 h5("3. All data on Maryland county population come from US Census Bureau’s County Population Totals: 2010-2019. Accessed on March 29, 2020."),
                                 h5("4. Data on Maryland population by age and sex come from Maryland Department of Planning’s Population Estimates by Race and Hispanic Origin for July 1, 2018. Accessed on April 1, 2020."),
                                 h5("5. For further information on county population size and characteristics,",
                                    a("Please see this.", 
                                      href=""),),
                                 hr()
                        )
            )
        )
    )



#******************************####
# 2. SERVER
#******************************

server<-function(input, output) {
    
    ##### general text output of inputs #####
    output$text_county <- renderText({
        paste(input$county, "county") 
    })    

    ##### output: Tab 1 #####
    output$plot_MD_cases <- renderPlotly({
        
        dtaMD%>%
            plot_ly(x=~date, 
                    type='scatter', mode='line',  
                    y=~positiveratesmooth, name="Positive rate", 
                    marker=list(color = "black", size=1),
                    line=list(color = "black"))%>%
            add_trace(    
                y=~newcasessmooth, name="Cases", 
                marker=list(color = "#1F77B4", size=1),
                line=list(color = "#1F77B4"), 
                yaxis='y2')%>%
            add_segments(x = min(dtaMD$date), xend = max(dtaMD$date), 
                         y = 5, yend = 5, yaxis='y', 
                         marker = list(color = "gray",size = 2),
                         line= list(color = "gray",  dash = 'dot'),
                         showlegend=FALSE)%>%
            layout(
                title = c("Trend of daily new cases and positive test rate, 14-day average"),
                xaxis = list(title = "", tickfont = list(size=10), showgrid = FALSE), 
                yaxis = list(title = "Positive rate (%)", 
                             #range=c(0, max(dtaMD$positiveratesmooth)),
                             range=c(0, 100),
                             zeroline = TRUE,
                             side="left", showgrid = FALSE),
                yaxis2 = list(title = "Number of daily new cases", 
                              #range=c(0, max(dtaMD$newcases)),
                              range=c(0, 20000),
                              zeroline = TRUE,
                              overlaying='y',  
                              side="right", showgrid = FALSE ),
                margin = list(r=100), 
                legend=list(orientation="v", xanchor = "left", yanchor = "center", 
                            x = 0.05, y = 0.95) 
            ) 
        
    })    
    
    output$plot_MD_hospital <- renderPlotly({
        
        dtaMD%>%
            plot_ly(x=~date, 
                    type='scatter', mode='line',  
                    y=~icusmooth, name="ICU for adults", 
                    marker=list(color = "#B41F77", size=1),
                    line=list(color = "#B41F77"))%>%
            add_trace(    
                y=~admissionsmooth, name="ICU + Acute beds for adults", 
                marker=list(color = "#D98DB9", size=1),
                line=list(color = "#D98DB9"))%>%  
            layout(
                title = c("Trend of daily new cases and admission/ICU cases"),
                xaxis = list(title = "", tickfont = list(size=10), showgrid = FALSE), 
                yaxis = list(title = "Number of ICU and hospitalization cases", 
                             #range=c(0, max(dtafig$positiveratesmooth)),
                             range=c(0, 4000),
                             zeroline = TRUE,
                             side="left", showgrid = FALSE),
                legend=list(orientation="v", xanchor = "left", yanchor = "center", 
                            x = 0.05, y = 0.95) 
                
            ) 
    })        

    output$plot_MD_trends <- renderPlotly({    
        
        dtaMD%>%
            plot_ly(x=~date, 
                    type='scatter', mode='line',  
                    y=~icusmooth, name="ICU", 
                    marker=list(color = "#B41F77", size=1),
                    line=list(color = "#B41F77"))%>%
            add_trace(    
                y=~admissionsmooth, name="Hospitalization, including ICU", 
                marker=list(color = "#D98DB9", size=1),
                line=list(color = "#D98DB9"))%>%  
            add_trace(    
                y=~newcasessmooth, name="Confirmed COVID-19 cases", 
                marker=list(color = "#1F77B4", size=1),
                line=list(color = "#1F77B4"), 
                yaxis='y2')%>%
            
            add_trace(    
                    y=~icusmooth_2w, name="", 
                    marker=list(color = "#5C103D", size=1),
                    line=list(color = "#5C103D", width=3))%>%            
            add_trace(    
                y=~admissionsmooth_2w, name="", 
                marker=list(color = "#6F485E", size=1),
                line=list(color = "#6F485E", width=3))%>%  
            add_trace(    
                y=~newcasessmooth_2w, name="", 
                marker=list(color = "#103D5C", size=1),
                line=list(color = "#103D5C", width=3), 
                yaxis='y2')%>%            
            layout(
                #title = c("Trend of daily new cases and admission/ICU cases"),
                xaxis = list(title = "", tickfont = list(size=10), showgrid = FALSE), 
                yaxis = list(title = "Number of ICU and hospitalization cases", 
                             #range=c(0, max(dtafig$positiveratesmooth)),
                             range=c(0, 4000),
                             zeroline = TRUE,
                             side="left", showgrid = FALSE),
                yaxis2 = list(title = "Number of daily new confirmed cases", 
                              #range=c(0, max(dtafig$newcases)),
                              range=c(0, 20000),
                              zeroline = TRUE,
                              overlaying='y',  
                              side="right", showgrid = FALSE ),
                margin = list(r=100), 
                showlegend=FALSE 
                #legend=list(orientation="v", xanchor = "left", yanchor = "center", 
                #            x = 0.05, y = 0.95) 
                
            )    
    }) 
    
    output$plot_MD_trends_age <- renderPlotly({

        fig1<-dtabyagemonthly %>% group_by(group) %>% 
            plot_ly( x = ~retromonthdate, y = ~pctnewcases, type = "bar",
                     color= ~group, 
                     colors = brewer.pal(length(unique(dtabyagemonthly$group)),
                                         "Spectral")
            ) %>%
            layout(
                yaxis = list(title = "Percent of monthly new cases",
                             titlefont=list(size=12)), 
                legend = list(font=list(size=12)),
                barmode = 'stack'
            ) 
        
        fig2<-dtabyagemonthly %>% group_by(group) %>% 
            plot_ly( x = ~retromonthdate, y = ~pctnewdeaths, type = "bar", showlegend=F,
                     color= ~group, 
                     colors = brewer.pal(length(unique(dtabyagemonthly$group)),
                                         "Spectral")
            ) %>%
            layout(
                yaxis = list(title = "Percent of monthly new deaths",
                             titlefont=list(size=12)), 
                legend = list(font=list(size=12)),
                barmode = 'stack'
            ) 
        
        subplot(fig1, fig2, 
                nrows=1, margin=0.05, shareY = FALSE, titleY = TRUE) 
        
    })
    
    output$plot_MD_trends_race <- renderPlotly({

        dtabyracewide<-dtabyrace%>%select(date, race, incidence)%>%
            mutate(
                race=ifelse(race=="African-American", "AfricanAmerican", race), 
                race=paste0("incidence",race)
            )%>%
            arrange(date, race)%>% 
            spread(race, incidence, fill = NA, convert = FALSE) 
        
        maxy<-max(dtabyracewide$incidence)
        
        fig1<-plot_ly(dtabyracewide, x=~date, y=~incidenceAfricanAmerican, name = "African-American", 
                                       type = 'scatter', mode='lines',
                                       line = list(color = c( "#d62728"))) %>% 
            add_trace(y = ~incidenceAsian, name = "Asian", line = list(color = c( "#ff7f0e"))) %>% 
            add_trace(y = ~incidenceHispanic, name = "Hispanic", line = list(color = c( "#2ca02c"))) %>% 
            add_trace(y = ~incidenceWhite, name = "White", line = list(color = c( "#1f77b4"))) %>%  
            layout(
                xaxis = list(title = ""), 
                yaxis = list(title = "Total number of confirmed cases (per 100,000 population)",
                             titlefont=list(size=10),
                             tickfont=list(size=10),
                             range=c(0, maxy) ), 
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "center",  
                              x=0.5, y=-0.1)               
            )
        
        dtabyracewide<-dtabyrace%>%select(date, race, mortality)%>%
            mutate(
                race=ifelse(race=="African-American", "AfricanAmerican", race), 
                race=paste0("mortality",race)
            )%>%
            arrange(date, race)%>% 
            spread(race, mortality, fill = NA, convert = FALSE) 
        
        maxy<-max(dtabyracewide$mortality)
        
        fig2<-plot_ly(dtabyracewide, x=~date, y=~mortalityAfricanAmerican, name = "African-American", 
                      type = 'scatter', mode='lines',
                      line = list(color = c( "#d62728"))) %>% 
            add_trace(y = ~mortalityAsian, name = "Asian", line = list(color = c( "#ff7f0e"))) %>% 
            add_trace(y = ~mortalityHispanic, name = "Hispanic", line = list(color = c( "#2ca02c"))) %>% 
            add_trace(y = ~mortalityWhite, name = "White", line = list(color = c( "#1f77b4"))) %>%  
            layout(
                xaxis = list(title = ""), 
                yaxis = list(title = "Total number of confirmed deaths (per 100,000 population)",
                             titlefont=list(size=10),
                             tickfont=list(size=10),
                             range=c(0, maxy) ), 
                legend = list(font=list(size=10), 
                              orientation = "h", xanchor = "center",  
                              x=0.5, y=-0.1)               
            ) 
        
        subplot(fig1, fig2, 
                nrows=1, margin=0.05, shareY = FALSE, titleY = TRUE) 
            
    })
    
    ##### output: Tab 2 #####

    output$plot_county_cases_map <- renderPlotly({

        withProgress(message = 'Making plot', value = 0, {
            # Number of times we'll go through the loop
            n <- 50
            
            for (i in 1:n) {
                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.
                
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/n, detail = paste("Doing part", i))
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
            }
        })        
        
        dtabycounty%>%
                filter(date==max(date)-1)%>%
                plot_ly()%>%
                add_trace(
                    type ="choroplethmapbox",
                    geojson = counties,
                    locations = ~fips,
                    z= ~positiveratesmooth,
                    color = ~positiveratesmooth,
                    colors = 'Reds', 
                    text = ~county, 
                    #zmin=0,
                    #zmax=50,
                    marker=list(
                        line=list(width=0),
                        opacity=0.5)        
                )%>%
                colorbar(title = "Positivity rate (%)")%>%  
                layout(
                    mapbox=list(
                        style="carto-positron",  
                        zoom = 6,
                        center=list(lon= -77.04, lat=38.90))
                )
    })    
    
    output$plot_county_cases <- renderPlotly({
        
        dtafig<-dtabycounty%>%
            filter(county!="Unknown")%>%
            arrange(county, date)%>%
            filter(date==max(date))%>%
            select(date, county, cases, newcasessmoothpp, pop)%>%
            mutate(cumincidence=round(cases*100000/pop), 0)
        
        dtafig$county<-factor(dtafig$county, 
                              levels = unique(dtafig$county) 
                              [order(dtafig$cases, decreasing = FALSE)])
        
        figcases<-  plot_ly(dtafig, y = ~county, x = ~cases, 
                            name="total confirmed cases", 
                            type="bar", orientation="h" , 
                            marker = list(color = c( "gray"))) %>%
            layout(
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=9)
                ),
                xaxis = list(title = "Total cumulative number of confirmed cases")
            )
        
        dtafig$county<-factor(dtafig$county, 
                              levels = unique(dtafig$county) 
                              [order(dtafig$cumincidence, decreasing = FALSE)])
        
        figincidence<-  plot_ly(dtafig, y = ~county, x = ~cumincidence, 
                                name="incidence, cumulative (per 100,000)",
                                type="bar", orientation="h",
                                marker = list(color = c( "#C5DBEC"))) %>%
            layout(
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=9)
                ),
                xaxis = list(title = "Number of cumulative confirmed cases per 100,000 population")
            )
        
        dtafig$county<-factor(dtafig$county, 
                              levels = unique(dtafig$county) 
                              [order(dtafig$newcasessmoothpp, decreasing = FALSE)])
        
        fignew7day<-plot_ly(dtafig, y = ~county, x = ~newcasessmoothpp, 
                            name="incidence, latest 14-day average (per 100,000)",
                            type="bar", orientation="h",
                            marker = list(color = c( "#8DB9D9"))) %>%
            layout(
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=9)
                ),
                xaxis = list(title = "Number of confirmed cases (daily, latest 14-day average) per 100,000 population")
            )
        
        #subplot(figcases, figincidence, fignew7day, 
        subplot(fignew7day, figincidence, 
                nrows=1, margin=0.05, shareY = FALSE, titleY = TRUE) %>%
            layout(  title ="", 
                     legend = list(font=list(size=9), 
                                   orientation = "h", xanchor = "center",  
                                   x=0.5, y=-0.15) 
            )
        
    })
    
    output$plot_county_deaths <- renderPlotly({

        dtafig<-dtabycounty%>%
            filter(county!="Unknown")%>%
            arrange(county, date)%>%
            filter(date==max(date))%>%
            select(date, county, deaths, newdeathssmoothpp, pop)%>%
            mutate(deathsrate=round(deaths*100000/pop), 2)
        
        dtafig$county<-factor(dtafig$county, 
                              levels = unique(dtafig$county) 
                              [order(dtafig$deaths, decreasing = FALSE)])
        
        figdeaths<-  plot_ly(dtafig, y = ~county, x = ~deaths, 
                             name="total confirmed deaths", 
                             type="bar", orientation="h" , 
                             marker = list(color = c( "gray"))) %>%
            layout(
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=9)
                ),
                xaxis = list(title = "Total cumulative number of confirmed deaths")
            )
        
        dtafig$county<-factor(dtafig$county, 
                              levels = unique(dtafig$county) 
                              [order(dtafig$deathsrate, decreasing = FALSE)])
        
        figdeathsrate<-  plot_ly(dtafig, y = ~county, x = ~deathsrate, 
                                name="Death, cumulative (per 100,000)",
                                type="bar", orientation="h",
                                marker = list(color = c( "#ECC5DB"))) %>%
            layout(
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=9)
                ),
                xaxis = list(title = "Number of cumulative confirmed deaths per 100,000 population")
            )
        
        dtafig$county<-factor(dtafig$county, 
                              levels = unique(dtafig$county) 
                              [order(dtafig$newdeathssmoothpp, decreasing = FALSE)])
        
        fignew7day<-plot_ly(dtafig, y = ~county, x = ~newdeathssmoothpp, 
                            name="Daily deaths, latest 14-day average (per 100,000)",
                            type="bar", orientation="h",
                            marker = list(color = c( "#D98DB9"))) %>%
            layout(
                yaxis = list(title = "",  
                             autotick = FALSE,
                             showticklabels = TRUE, 
                             tickfont = list(size=9)
                ),
                xaxis = list(title = "Number of confirmed deaths (daily, latest 14-day average) per 100,000 population")
            )
        
        #subplot(figdeaths, figincidence, fignew7day, 
        subplot(fignew7day, figdeathsrate, 
                nrows=1, margin=0.05, shareY = FALSE, titleY = TRUE) %>%
            layout(  title ="", 
                     legend = list(font=list(size=9), 
                                   orientation = "h", xanchor = "center",  
                                   x=0.5, y=-0.15) 
            )
        
    })

    ##### output: Tab 3 #####
    output$plot_county_trend_cases <- renderPlotly({
        
        dtafig<-dtabycounty%>%
            filter(county==input$county)%>%
            rename(value=newcasessmoothpp)%>%
            filter(is.na(value)==FALSE)%>%
            arrange(date)
        
        annotated_value<-round(dtafig$value[nrow(dtafig)], 0)
              
        panel <- . %>% 
            plot_ly(x=~date,
                    y=~newcasessmoothppMD, type='scatter', mode = 'lines', 
                    marker=list(color = c( "#D6D6D6"), size = 0.3),
                    line=list(color = c( "#D6D6D6")),
                    name="Maryland"
            )%>%
            add_trace(
                y=~newcasessmoothpp, type='scatter', mode = 'lines', 
                marker=list(color = c( "#828282"), size = 1),
                line=list(color = c( "#828282")),
                name="County"
            )%>%
            add_trace(
                y=~newcasessmoothpp_2m, type='scatter', mode = 'lines', 
                marker=list(color = c( "#5e3c99"), size = 1),
                line=list(color = c( "#5e3c99")),
                name="County"
            )%>%   
            add_trace(
                y=~newcasessmoothpp_2w, type='scatter', mode = 'lines', 
                marker=list(color = c( "#e66101"), size = 1),
                line=list(color = c( "#e66101")),
                name="County"
            )%>%      
            
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.95, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
            ) %>%
            layout(
                showlegend = FALSE,
                xaxis=list(title = "", font=list(size=10),tickfont = list(size=8),
                           showgrid = FALSE),
                yaxis=list(title = "New daily cases/100,000", 
                           showgrid = FALSE),
                
                annotations = list(
                    xref = 'paper',
                    x = 0.95,
                    y = annotated_value,
                    xanchor = 'left',
                    yanchor = 'middle',
                    text = paste(annotated_value),
                                 #, "cases per day"),
                    font = list(family = 'Arial',
                                size = 12,
                                color = c( "#e66101")),
                    showarrow = FALSE)
            )
        
        dtabycounty%>%
            filter(county==input$county)%>%
            left_join(dtaMDsummary, by = "date")%>%
            filter(if (input$retroday=="Last 6 months") retroday<=180 
                   else retroday>=0)%>%
            
            group_by(group)%>%
            do(p = panel(.))%>%
            subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
            layout(
                title ="", 
                yaxis=list(title = "New daily cases/100,000")
            )    
        
    })    
    
    output$plot_county_trend_deaths <- renderPlotly({
        
        dtafig<-dtabycounty%>%
            filter(county==input$county)%>%
            rename(value=newdeathssmoothpp)%>%
            filter(is.na(value)==FALSE)%>%
            arrange(date)
        
        annotated_value<-round(dtafig$value[nrow(dtafig)], 2)
                               
        panel <- . %>% 
            plot_ly(x=~date,
                    y=~newdeathssmoothppMD, type='scatter', mode = 'lines', 
                    marker=list(color = c( "#D6D6D6"), size = 0.3),
                    line=list(color = c( "#D6D6D6")),
                    name="Maryland"
            )%>%
            add_trace(
                y=~newdeathssmoothpp, type='scatter', mode = 'lines', 
                marker=list(color = c( "#828282"), size = 1),
                line=list(color = c( "#828282")),
                name="County"
            )%>%
            add_trace(
                y=~newdeathssmoothpp_2m, type='scatter', mode = 'lines', 
                marker=list(color = c( "#5e3c99"), size = 1),
                line=list(color = c( "#5e3c99")),
                name="County"
            )%>%   
            add_trace(
                y=~newdeathssmoothpp_2w, type='scatter', mode = 'lines', 
                marker=list(color = c( "#e66101"), size = 1),
                line=list(color = c( "#e66101")),
                name="County"
            )%>%      
            
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.95, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
            ) %>%
            #add_annotations(
            #    text = ~("(Light grey line is Maryland state-level trend)"),
            #    x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
            #    xanchor = "center", yanchor = "bottom", showarrow = FALSE,
            #    font = list(size = 12)
            #) %>%            
            layout(
                showlegend = FALSE,
                xaxis=list(title = "", font=list(size=10),tickfont = list(size=8),
                           showgrid = FALSE),
                yaxis=list(title = "New daily deaths/100,000", 
                           showgrid = FALSE),
                
                annotations = list(
                    xref = 'paper',
                    x = 0.95,
                    y = annotated_value,
                    xanchor = 'left',
                    yanchor = 'middle',
                    text = paste(annotated_value),
                    #, "cases per day"),
                    font = list(family = 'Arial',
                                size = 12,
                                color = c( "#e66101")),
                    showarrow = FALSE)
            )
        
        dtabycounty%>%
            filter(county==input$county)%>%
            left_join(dtaMDsummary, by = "date")%>%
            filter(if (input$retroday=="Last 6 months") retroday<=180 
                   else retroday>=0)%>%
            
            group_by(group)%>%
            do(p = panel(.))%>%
            subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
            layout(
                title ="", 
                yaxis=list(title = "New daily deaths/100,000")
            )    
        
    })    
    
    output$plot_county_trend_positivity <- renderPlotly({
        
        dtafig<-dtabycounty%>%
            filter(county==input$county)%>%
            rename(value=positiveratesmooth)%>%
            filter(is.na(value)==FALSE)%>%
            arrange(date)
        
        annotated_value<-round(dtafig$value[nrow(dtafig)], 0)
        
        panel <- . %>% 
            plot_ly(x=~date,
                    y=~positiveratesmoothMD, type='scatter', mode = 'lines', 
                    marker=list(color = c( "#D6D6D6"), size = 0.3),
                    line=list(color = c( "#D6D6D6")),
                    name="Maryland"
            )%>%
            add_trace(
                y=~positiveratesmooth, type='scatter', mode = 'lines', 
                marker=list(color = c( "#828282"), size = 1),
                line=list(color = c( "#828282")),
                name="County"
            )%>%
            add_trace(
                y=~positiveratesmooth_2m, type='scatter', mode = 'lines', 
                marker=list(color = c( "#5e3c99"), size = 1),
                line=list(color = c( "#5e3c99")),
                name="County"
            )%>%   
            add_trace(
                y=~positiveratesmooth_2w, type='scatter', mode = 'lines', 
                marker=list(color = c( "#e66101"), size = 1),
                line=list(color = c( "#e66101")),
                name="County"
            )%>%   
            
            add_segments(x = min(dtabycounty$date), xend = max(dtabycounty$date), 
                         y = 5, yend = 5, 
                         marker = list(color = "gray",size = 2),
                         line= list(color = "gray",  dash = 'dot'),
                         showlegend=FALSE)%>%            
            
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.95, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
            ) %>%
            #add_annotations(
            #    text = ~("(Light grey line is Maryland state-level trend)"),
            #    x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
            #    xanchor = "center", yanchor = "bottom", showarrow = FALSE,
            #    font = list(size = 12)
            #) %>%            
            layout(
                showlegend = FALSE,
                xaxis=list(title = "", font=list(size=10),tickfont = list(size=8),
                           showgrid = FALSE),
                yaxis=list(title = "Positivity rate (%)", 
                           showgrid = FALSE),
                
                annotations = list(
                    xref = 'paper',
                    x = 0.95,
                    y = annotated_value,
                    xanchor = 'left',
                    yanchor = 'middle',
                    text = paste(annotated_value),
                    #, "cases per day"),
                    font = list(family = 'Arial',
                                size = 12,
                                color = c( "#e66101")),
                    showarrow = FALSE)
            )
        
        dtabycounty%>%
            filter(county==input$county)%>%
            left_join(dtaMDsummary, by = "date")%>%
            filter(if (input$retroday=="Last 6 months") retroday<=180 
                   else retroday>=0)%>%
            
            group_by(group)%>%
            do(p = panel(.))%>%
            subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
            layout(
                title ="", 
                yaxis=list(title = "Positivity rate (%)")
            )    
        
    })    
    
    output$plot_county_trend_covax <- renderPlotly({
        
        panel <- . %>% 
            plot_ly(x=~date,
                    y=~covaxcomp_cumpctMD, type='scatter', mode = 'lines', 
                    marker=list(color = c( "#D6D6D6"), size = 0.3),
                    line=list(color = c( "#D6D6D6")),
                    name="Maryland"
            )%>%
            add_trace(
                y=~covaxcomp_cumpct, type='scatter', mode = 'lines', 
                marker=list(color = c( "#828282"), size = 1),
                line=list(color = c( "#828282"))
            )%>%
            add_trace(
                y=~covaxcomp_cumpct_2m, type='scatter', mode = 'lines', 
                marker=list(color = c( "#5e3c99"), size = 1),
                line=list(color = c( "#5e3c99"))
            )%>%   
            add_trace(
                y=~covaxcomp_cumpct_2w, type='scatter', mode = 'lines', 
                marker=list(color = c( "#e66101"), size = 1),
                line=list(color = c( "#e66101"))   
            )%>%   
            
            add_annotations(
                text = ~unique(group),
                x = 0.5, y = 0.95, xref = "paper", yref = "paper",    
                xanchor = "center", yanchor = "bottom", showarrow = FALSE,
                font = list(size = 12)
            ) %>%
            #add_annotations(
            #    text = ~("(Light grey line is Maryland state-level trend)"),
            #    x = 0.5, y = 0.90, xref = "paper", yref = "paper",    
            #    xanchor = "center", yanchor = "bottom", showarrow = FALSE,
            #    font = list(size = 12)
            #) %>%            
            layout(
                showlegend = FALSE,
                xaxis=list(title = "", font=list(size=10),tickfont = list(size=8),
                           showgrid = FALSE),
                yaxis=list(title = "Positivity rate (%)", 
                           showgrid = FALSE)
            )
        
        dtabycounty%>%
            filter(county==input$county)%>%
            left_join(dtaMDsummary, by = "date")%>%
            filter(if (input$retroday=="Last 6 months") retroday<=180 
                   else retroday>=0)%>%
            
            group_by(group)%>%
            do(p = panel(.))%>%
            subplot(nrows = 1, shareX = TRUE, shareY = TRUE)%>%
            layout(
                title ="", 
                yaxis=list(title = "Percent of population with complete doses")
            )    
        
    })        
    
}       

#******************************####
# 3. CREATE APP 
#******************************

shinyApp(ui, server)