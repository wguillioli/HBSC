#HBSC

import pandas as pd
import os
import numpy as np
#import matplotlib.pyplot as plt


# set working directory
new_directory_path = "C:\\MisLocalFiles\\Github\\HBSC" 
os.chdir(new_directory_path)

# function that takes column and recodes
def recode1to5feelings(col2convert):
    
    col_converted = col2convert.replace({"1"	: "1 About every day",
                                                 "2" :	"2 More once/week",
                                                 "3" : 	"3 About every week",
                                                 "4" :	"4 About every month",
                                                 "5" :	"5 Rarely or never",
                                                 " " : np.nan})
    
    return col_converted


# function that takes column and recodes
def recode1to5agreement(col2convert):
    
    col_converted = col2convert.replace({"1"	: "1 Strongly agree",
                                         "2" :	"2 Agree",
                                         "3" : "3 Neither/nor",
                                         "4" :	"4 Disagree",
                                         "5" :	"5 Strongly disagree",
                                         " " : np.nan})
    
    return col_converted

# function that takes column; records and prints new counts
def recode_food(col2convert):
    
    col_converted = col2convert.replace({'1' : "Never",
                                        '2' : "LessOnceWeek", 
                                        '3' : "OnceWeek", 
                                        '4' : "2-4DayWeek",
                                        '5' : "5-6DaysWeek", 
                                        '6' : "OnceDaily", 
                                        '7' : "MoreOnceDaily",
                                        ' ' : np.nan})
    
    print(col_converted.value_counts(dropna = False))
    
    return col_converted

# function that takes column; recodes and prints new counts
def recode_vice(col2convert):
    
    col_converted = col2convert.replace({'1' : "Never",
                                         '2' : "1-2 days", 
                                         '3' : "3-5 days",
                                         '4' : "6-9 days",
                                         '5' : "10-19 days", 
                                         '6' : "20-29 days",
                                         '7' : "30 days (or more)",
                                         '-99' : "MissingInconsistentAnswer",
                                         ' ' : np.nan})
    
    print(col_converted.value_counts(dropna = False))
    
    return col_converted

# function that takes column, recodes and prints xtabs to corroborate
def recode_bullying(col2convert):
    
    col_converted = col2convert.replace({'1' : 'Havent',
                                        '2' : 'OnceTwice',
                                        '3' : '2-3XPerMonth',
                                        '4' : 'OnceWeek',
                                        '5' : 'SeveralWeek',
                                        ' ' : np.nan})
    
    return col_converted    

def recode_fight(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '0x',
        '2' : '1x',
        '3' : '2x',
        '4' : '3x',
        '5' : '+4x',
        ' ' : np.nan
        })
    
    return col_converted

def recode_friends(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1StrongDisagree',
        '2' : '2Disagree',
        '3' : '3SomewhatDisagree',
        '4' : '4Neutral',
        '5' : '5SomewhatAgree',
        '6' : '6Agree',
        '7' : '7StrongAgree',
        ' ' : np.nan
        })
    
    return col_converted
    
def recode_onlinecomms(col2convert):
    
    col_converted = col2convert.replace({
        '1' : 'DontKnow/NA',
        '2' : 'Never/AlmostNever',
        '3' : 'Weekly',
        '4' : 'Daily',
        '5' : 'SeveralDaily',
        '6' : 'AllTheTime',
        ' ' : np.nan
        })
    
    return col_converted
      
 
# load data
dat_file_path = "C:\\MisLocalFiles\\Github\\HBSC\\data\\HBSC2018OAed1.1.csv"
hbsc2018 = pd.read_csv(dat_file_path, decimal=',', sep=';')

# basic df exploration
hbsc2018.shape #244097, 120

hbsc2018.info()

# output columns to csv
cols_hbsc2018 = hbsc2018.columns
cols_hbsc2018 = pd.DataFrame(cols_hbsc2018)
cols_hbsc2018.to_csv("./other/cols_hbsc2018.csv")


# make working copy
d = hbsc2018.copy()

coltypes = d.columns.tolist()

d.isna().sum()

# univ eda and recoding 
hbsc2018_columns = hbsc2018.columns.tolist()
print(hbsc2018_columns)

# 'HBSC'
d['HBSC'].value_counts() #constant (2018)

# 'seqno_int' (Identify each row in cross national files)
len(d['seqno_int'].unique()) #unique ID

# 'cluster' (Identify each class or cluster in cross national files)
d['cluster'].value_counts(dropna = False) #17k diff values

# 'countryno' (Country/WHO region)
d['countryno'].value_counts()
len(d['countryno'].value_counts(dropna = False)) #47

recode_map = { 8000 : 'Albania',
            31000 : 'Azerbaijan',
            40000 : 'Austria',
            51000 : 'Armenia',
            56001 : 'Belgium (Flemish)',
            56002 : 'Belgium (French)',
            100000 : 'Bulgaria',
            124000 : 'Canada',
            191000 : 'Croatia',
            203000 : 'Czech Republic',
            208000 : 'Denmark',
            233000 : 'Estonia',
            246000 : 'Finland',
            250000 : 'France',
            268000 : 'Georgia',
            276000 : 'Germany',
            300000 : 'Greece',
            304000 : 'Greenland',
            348000 : 'Hungary',
            352000 : 'Iceland',
            372000 : 'Ireland',
            376000 : 'Israel',
            380000 : 'Italy',
            398000 : 'Kazakhstan',
            428000 : 'Latvia',
            440000 : 'Lithuania',
            442000 : 'Luxembourg',
            470000 : 'Malta',
            498000 : 'Republic of Moldova',
            528000 : 'Netherlands',
            578000 : 'Norway',
            616000 : 'Poland',
            620000 : 'Portugal',
            642000 : 'Romania',
            643000 : 'Russia',
            688000 : 'Serbia',
            703000 : 'Slovakia',
            705000 : 'Slovenia',
            724000 : 'Spain',
            752000 : 'Sweden',
            756000 : 'Switzerland',
            792000 : 'Turkey',
            804000 : 'Ukraine',
            807000 : 'Macedonia',
            826001 : 'England',
            826002 : 'Scotland',
            826003 : 'Wales',
            826004 : 'Northern Ireland',
            840000 : 'USA'
                }

d['country'] = d['countryno'].map(recode_map)

d['country'].value_counts(dropna = False)

# 'region'
d['region'].value_counts(dropna = False) #don't seem to add value, country is better

# 'id1', 'id2', 'id3', 'id4'
d['id1'].value_counts(dropna = False) #these are just number Ids that don't seem to mean anything

# DO 2 TO 4


# 'weight' weight of dataset. what is this?
d['weight']

# 'adm'
d['adm'].value_counts(dropna = False)

recode_map = { 1 : 'Paper',
               2 : 'Computer'
               }

d['adm_recoded'] = d['adm'].map(recode_map)

d['adm_recoded'].value_counts(dropna = False)

# 'year'
d['year'].value_counts(dropna = False) #2017 to 2019

# 'month'
d['month'].value_counts(dropna = False) #so odds that it repeats numbers... missing?

pd.crosstab(d['year'], d['month']) #that makes make sense so do yyyymm

#fix later
d['year'] = d['year'].astype(str)
d['month'] = d['month'].astype(str).str.zfill(2)
d['year_month'] = d['year'].str.cat(d['month'], sep='_')
d['year_month'].value_counts(dropna = False)


# 'age'
d['age'].info()
d['age'].describe()

d['age_recoded'] = d['age'].replace(",", ".").replace(' ', np.nan)
d['age_recoded'] = pd.to_numeric(d['age_recoded'], errors='coerce') 

d['age_recoded'].describe()
d['age_recoded'].hist()
d['age_recoded'].isna().sum() #do something with NAs

# 'agecat'
d['agecat'].astype(str).value_counts(dropna = False) #ok, but was is 1-3?

d.groupby('agecat')['age_recoded'].describe() # it seems to be 1=11; 2=13; 3=15 year olds

# 'sex'
d['sex'].value_counts(dropna = False)

d['sex_recoded'] = d['sex'].astype(str).replace({'1': 'male', '2': 'female'})
d['sex_recoded'].value_counts(dropna = False)

# 'grade'
d['grade'].value_counts(dropna = False)

d['grade_recoded'] = d['grade'].astype(str).replace({'1': '11yo', '2': '13yo', '3': '15yo'})
d['grade_recoded'].value_counts(dropna = False) #huge SysMiss

pd.crosstab(d['grade_recoded'], d['agecat']) #kind of 1:1 but sadly many NAs

# 'health'
d['health'].value_counts(dropna = False)

d['health_recoded'] = d['health'].astype(str).replace({'1': 'Excellent', 
                                                       '2': 'Good', 
                                                       '3': 'Fair',
                                                       '4': 'Poor'})

d['health_recoded'].value_counts(dropna = False)

# 'lifesat'
d['lifesat'].value_counts(dropna = False) # 0=worst; 10=best 

d['lifesat_recoded'] = d['lifesat'].replace({'0': 0, 
                                                         '1': 1,
                                                         '2': 2,
                                                         '3': 3,
                                                         '4': 4,
                                                         '5': 5,
                                                         '6': 6,
                                                         '7': 7,
                                                         '8': 8,
                                                         '9': 9,
                                                         '10' : 10,
                                                         ' ' : -1}) #-1 is NA

d['lifesat_recoded'].describe()

d['lifesat_recoded'].hist()

# 'feellow'
d['feellow'].value_counts(dropna = False)



d['feellow_recoded'] = recode1to5feelings(d['feellow'])
d['feellow_recoded'].value_counts(dropna = False)
d['feellow_recoded'].value_counts(dropna = False).plot.bar()
                         
# 'irritable'
d['irritable_recoded'] = recode1to5feelings(d['irritable'])
d['irritable_recoded'].value_counts(dropna = False)

# 'nervous'
d['nervous_recoded'] = recode1to5feelings(d['nervous'])
d['nervous_recoded'].value_counts(dropna = False)

# 'sleepdificulty'
d['sleepdificulty_recoded'] = recode1to5feelings(d['sleepdificulty'])
d['sleepdificulty_recoded'].value_counts(dropna = False)

#'dizzy'
d['dizzy_recoded'] = recode1to5feelings(d['dizzy'])
d['dizzy_recoded'].value_counts(dropna = False)

#'headache'
d['headache_recoded'] = recode1to5feelings(d['headache'])
d['headache_recoded'].value_counts(dropna = False)

#'stomachache'
d['stomachache_recoded'] = recode1to5feelings(d['stomachache'])
d['stomachache_recoded'].value_counts(dropna = False)

#'backache'
d['backache_recoded'] = recode1to5feelings(d['backache'])
d['backache_recoded'].value_counts(dropna = False)

# 'studtogether'
d['studtogether_recoded'] = recode1to5agreement(d['studtogether'])
d['studtogether_recoded'].value_counts(dropna = False)

# 'studhelpful'
d['studhelpful_recoded'] = recode1to5agreement(d['studhelpful'])
d['studhelpful_recoded'].value_counts(dropna = False)

# 'studaccept'
d['studaccept_recoded'] = recode1to5agreement(d['studaccept'])
d['studaccept_recoded'].value_counts(dropna = False)

# 'teacheraccept'
d['teacheraccept_recoded'] = recode1to5agreement(d['teacheraccept'])
d['teacheraccept_recoded'].value_counts(dropna = False)

# 'teachercare'
d['teachercare_recoded'] = recode1to5agreement(d['teachercare'])
d['teachercare_recoded'].value_counts(dropna = False)

# 'teachertrust'
d['teachertrust_recoded'] = recode1to5agreement(d['teachertrust'])
d['teachertrust_recoded'].value_counts(dropna = False)

# 'monthbirth', 'yearbirth'
# useless since we have age and grade above
d['yearbirth'].astype(str).value_counts(dropna = False)
d['monthbirth'].astype(str).value_counts(dropna = False)

# 'fasfamcar', family car
d['fasfamcar_recoded'] = d['fasfamcar'].replace({'1': 'No', 
                                                 '2': 'Yes1', 
                                                 '3': 'Yes2+',
                                                 ' ': np.nan})

d['fasfamcar_recoded'].value_counts(dropna=False)

# 'fasbedroom', Own bedroom
d['fasbedroom_recoded'] = d['fasbedroom'].replace({'1': 'No', 
                                                   '2': 'Yes', 
                                                   ' ': np.nan})

d['fasbedroom_recoded'].value_counts(dropna=False)

# fascomputers: # computers
d['fascomputers_recoded'] = d['fascomputers'].replace({'1': 'None', 
                                                 '2': 'Yes1', 
                                                 '3': 'Yes2',
                                                 '4': 'Yes3+',
                                                 ' ': np.nan})

d['fascomputers_recoded'].value_counts(dropna=False)

# fasbathroom: # bathrooms
d['fasbathroom_recoded'] = d['fasbathroom'].replace({'1': 'None', 
                                                 '2': 'Yes1', 
                                                 '3': 'Yes2',
                                                 '4': 'Yes3+',
                                                 ' ': np.nan})
d['fasbathroom_recoded'].value_counts(dropna=False)

# fasdishwash: dishwasher in home
d['fasdishwash_recoded'] = d['fasdishwash'].replace({'1': 'No', 
                                                     '2': 'Yes', 
                                                     ' ': np.nan})

d['fasdishwash_recoded'].value_counts(dropna=False)

# fasholidays: family holidays (# trips last year)
d['fasholidays_recoded'] = d['fasholidays'].replace({'1': 'None', 
                                                     '2': 'Once',
                                                     '3' : 'Twice',
                                                     '4' : 'MoreThanTwice',
                                                     ' ': np.nan})

d['fasholidays_recoded'].value_counts(dropna=False)

#thinkbody: think about body (size)
d['thinkbody_recoded'] = d['thinkbody'].replace({'1': 'MuchTooThin', 
                                                 '2': 'BitTooThin',
                                                 '3' : 'AboutRight',
                                                 '4' : 'BitTooFat',
                                                 '5' : 'MuchTooFat',
                                                 ' ': np.nan})

d['thinkbody_recoded'].value_counts(dropna=False)

# physact60: Physical activity past 7 days (# days at least 60+ minutes)
# no need to recode since each number represents # of days
d['physact60'].value_counts(dropna=False)

#'breakfastwd': # weekday days with breakfast
d['breakfastwd_recoded'] = d['breakfastwd'].replace({'1': 0, 
                                                 '2': 1,
                                                 '3' : 2,
                                                 '4' : 3,
                                                 '5' : 4,
                                                 '6' : 5,
                                                 ' ': np.nan})

d['breakfastwd_recoded'].value_counts(dropna=False)

# 'breakfastwe': # weekend days with breakfast
d['breakfastwe_recoded'] = d['breakfastwe'].replace({'1': 0, 
                                                 '2': 1,
                                                 '3' : 2,
                                                 ' ': np.nan})

d['breakfastwe_recoded'].value_counts(dropna=False)


# 'fruits_2'
d['fruits_2_recoded'] = recode_food(d['fruits_2'])

# 'vegetables_2'
d['vegetables_2_recoded'] = recode_food(d['vegetables_2'])

# 'sweets_2'
d['sweets_2_recoded'] = recode_food(d['sweets_2'])

# 'softdrinks_2'
d['softdrinks_2_recoded'] = recode_food(d['softdrinks_2'])

# 'fmeal': family meals together
d['fmeal_2_recoded'] = d['fmeal'].replace({'1' : "Daily", 
                                           '2' : "MostDays",
                                           '3' : "OnceWeek",
                                           '4' : "LessOften",
                                           '5' : "Never",
                                           ' ': np.nan})

d['fmeal_2_recoded'].value_counts(dropna=False)

# 'toothbr: often brush teeth
d['toothbr_recoded'] = d['toothbr'].replace({'1' : "MoreThanOnceDay",
                                             '2' : "OnceDay",
                                             '3' : "OnceWeek", 
                                             '4' : "LessThanWeekly",
                                             '5' : "Never", 
                                             ' ' : np.nan})

d['toothbr_recoded'].value_counts(dropna=False)

# 'timeexe': vigorous phys activity
d['timeexe_recoded'] = d['timeexe'].replace({'1' : "EveryDay", 
                                             '2' : "4-6xWeek",
                                             '3' : "2-3x",
                                             '4' : "OnceWeek",
                                             '5' : "OnceMonth",
                                             '6' : "LessThanOnceMonth",
                                             '7' : "Never", 
                                             ' ' : np.nan})

d['timeexe_recoded'].value_counts(dropna=False)

# 'smokltm': # days smoked ever
d['smokltm_recoded'] = recode_vice(d['smokltm'])

# 'smok30d_2': days smoked last 30 days
d['smok30d_2_recoded'] = recode_vice(d['smok30d_2'])

# alcltm: #days drank in lifetime
d['alcltm_recoded'] = recode_vice(d['alcltm'])

# alc30d_2: #days drank last 30 days
d['alc30d_2_recoded'] = recode_vice(d['alc30d_2'])

# drunkltm: Have you ever had so much alcohol that you were really drunk? in lifetime
d['drunkltm'].value_counts(dropna=False)

d['drunkltm_recoded'] = d['drunkltm'].replace({'1' : "Never", 
                                             '2' : "Once",
                                             '3' : "2-3x",
                                             '4' : "4-10x",
                                             '5' : "MoreThan10",
                                             '-99' : "InconsistentAnswer",
                                             ' ' : np.nan})

pd.crosstab(d['drunkltm'], d['drunkltm_recoded'], dropna=False)

# drunk30d; really drunk past 30 days
d['drunk30d_recoded'] = d['drunk30d'].replace({'1' : "Never", 
                                             '2' : "Once",
                                             '3' : "2-3x",
                                             '4' : "4-10x",
                                             '5' : "MoreThan10",
                                             '-99' : "InconsistentAnswer",
                                             ' ' : np.nan})

pd.crosstab(d['drunk30d'], d['drunk30d_recoded'], dropna=False)

# cannabisltm_2: Cannabis life time
d['cannabisltm_2_recoded'] = d['cannabisltm_2'].replace({'1' : "Never", 
                                             '2' : "1-2Days",
                                             '3' : "3-5Days",
                                             '4' : "6-9Days",
                                             '5' : "10-19Days",
                                             '6' : "20-29Days",
                                             '7' : "30DaysOrMore",
                                             ' ' : np.nan})

pd.crosstab(d['cannabisltm_2'], d['cannabisltm_2_recoded'], dropna=False)

# cannabis30d_2: Cannabis last 30 days
d['cannabis30d_2_recoded'] = d['cannabis30d_2'].replace({'1' : "Never", 
                                             '2' : "1-2Days",
                                             '3' : "3-5Days",
                                             '4' : "6-9Days",
                                             '5' : "10-19Days",
                                             '6' : "20-29Days",
                                             '7' : "30DaysOrMore",
                                             ' ' : np.nan})

pd.crosstab(d['cannabis30d_2'], d['cannabis30d_2_recoded'], dropna=False)

# bodyweight, weight without clothes- how much you weight? Kilo
d['bodyweight'].replace(' ', np.nan).astype(float).describe()

d['bodyweight_recoded'] = d['bodyweight'].replace(' ', np.nan).astype(float)

#bodyheight, how tall? cm no shoes
d['bodyheight'].replace(' ', np.nan).astype(float).describe()

d['bodyheight_recoded'] = d['bodyheight'].replace(' ', np.nan).astype(float)

# likeschool, How do you feel about school at present?
d['likeschool_recoded'] = d['likeschool'].replace({'1' : 'ALot',
                                                  '2' : 'ABit',
                                                  '3' : 'NotMuch',
                                                  '4' : 'NotAtAll',
                                                  ' ' : np.nan})

print(pd.crosstab(d['likeschool'], d['likeschool_recoded'], dropna=False))

# schoolpressure: How pressured do you feel by the schoolwork you have to do?
d['schoolpressure_recoded'] = d['schoolpressure'].replace({'1' : 'NotAtAll',
                                                  '2' : 'ALittle',
                                                  '3' : 'Some',
                                                  '4' : 'ALot',
                                                  ' ' : np.nan})

print(pd.crosstab(d['schoolpressure'], d['schoolpressure_recoded'], dropna=False))

# bulliedothers
d['bulliedothers_recoded'] = recode_bullying(d['bulliedothers'])

print(pd.crosstab(d['bulliedothers_recoded'], d['bulliedothers']))


# beenbullied
d['beenbullied_recoded'] = recode_bullying(d['beenbullied'])

print(pd.crosstab(d['beenbullied_recoded'], d['beenbullied']))

# cbulliedothers
d['cbulliedothers_recoded'] = recode_bullying(d['cbulliedothers'])

print(pd.crosstab(d['cbulliedothers_recoded'], d['cbulliedothers']))

# cbeenbullied
d['cbeenbullied_recoded'] = recode_bullying(d['cbeenbullied'])

print(pd.crosstab(d['cbeenbullied_recoded'], d['cbeenbullied']))

# fight12m, #During the past 12 months, how many times were you in a physical fight?
d['fight12m_recoded'] = recode_fight(d['fight12m'])

print(pd.crosstab(d['fight12m_recoded'], d['fight12m']))
    
# injured12m, During the past 12 months, how many times were you injured and had to be treated
#by a doctor or nurse
d['injured12m_recoded'] = recode_fight(d['injured12m'])

print(pd.crosstab(d['injured12m_recoded'], d['injured12m']))

# friendhelp: Literal question My friends really try to help me
d['friendhelp_recoded'] = recode_friends(d['friendhelp'])

pd.crosstab(d['friendhelp_recoded'], d['friendhelp'], dropna=False)

# friendcounton: I can count on my friends when things go wrong
d['friendcounton_recoded'] = recode_friends(d['friendcounton'])

pd.crosstab(d['friendcounton_recoded'], d['friendcounton'], dropna=False)

# friendshare: I have friends with whom I can share my joys and sorrows
d['friendshare_recoded'] = recode_friends(d['friendshare'])

pd.crosstab(d['friendshare_recoded'], d['friendshare'], dropna=False)

# friendtalk: I can talk about my problems with my friends
d['friendtalk_recoded'] = recode_friends(d['friendtalk'])

pd.crosstab(d['friendtalk_recoded'], d['friendtalk'], dropna=False)

#emconlfreq1: Onl contact close friends
d['emconlfreq1_recoded'] = recode_onlinecomms(d['emconlfreq1'])

pd.crosstab(d['emconlfreq1_recoded'], d['emconlfreq1'], dropna=False)

# emconlfreq2: Onl contact larger friend group
d['emconlfreq2_recoded'] = recode_onlinecomms(d['emconlfreq2'])

pd.crosstab(d['emconlfreq2_recoded'], d['emconlfreq2'], dropna=False)

# emconlfreq3: Onl contact online friends
d['emconlfreq3_recoded'] = recode_onlinecomms(d['emconlfreq3'])

pd.crosstab(d['emconlfreq3_recoded'], d['emconlfreq3'], dropna=False)

# emconlfreq4: Onl contact other
d['emconlfreq4_recoded'] = recode_onlinecomms(d['emconlfreq4'])

pd.crosstab(d['emconlfreq4_recoded'], d['emconlfreq4'], dropna=False)


#'', 'emconlpref1', 'emconlpref2', 'emconlpref3', #
#'emcsocmed1', 'emcsocmed2', 'emcsocmed3', 
#'emcsocmed4', 'emcsocmed5', 'emcsocmed6', 'emcsocmed7', 'emcsocmed8', 'emcsocmed9', 
#'hadsex', 'agesex', 'contraceptcondom', 'contraceptpill', 'countryborn', 'countrybornmo', 'countrybornfa', 
#'motherhome1', 'fatherhome1', 'stepmohome1', 'stepfahome1', 'fosterhome1', 'elsehome1_2', 
#'employfa', 'employmo', 'employnotfa', 'employnotmo', 'talkfather', 'talkstepfa', 'talkmother', 
#'talkstepmo', 'famhelp', 'famsup', 'famtalk', 'famdec', 
#'MBMI', 'IRFAS', 'IRRELFAS_LMH', #'IOTF4', 'oweight_who']


# to dos
# explicit NA when reading file csv
# remove useless vars after redoding... ir 1x1
# order factors like this https://bitl.to/5k99
#df_sorted = df.sort_index(axis=1)

# feature eng: 
    #continent



# # playground / dump

# # just a test
# # plot teacher trust distribution by country
# x = d[['country','teachertrust_recoded']]

# xagg = d.groupby(['country','teachertrust_recoded']).size().reset_index(name='Count')
# xagg

# xagg2 = pd.crosstab(d['country'],d['teachertrust_recoded']) 
# xagg2['country'] = xagg2.index

# xagg2.plot(x='country', kind='barh', stacked=True,
#         title='Stacked Bar Graph by dataframe')

# plt.show()





































