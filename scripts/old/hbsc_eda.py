# HBSC
# Script recodes all vars for friendly EDA

# remove all variables
for name in list(globals().keys()):
    if not name.startswith('_'):
        del globals()[name]

del name

import pandas as pd
import os
import numpy as np

# set working directory
new_directory_path = "C:\\MisLocalFiles\\Github\\HBSC" 
os.chdir(new_directory_path)

# many functions that takes column and recodes
def recode1to5feelings(col2convert):

    col_converted = col2convert.replace({"1"	: "1_Abouteveryday",
                                         "2" : "2_Moreonce/week",
                                         "3" : "3_Abouteveryweek",
                                         "4" : "4_Abouteverymonth",
                                         "5" : "5_Rarelyornever",
                                         " " : np.nan})
    
    return col_converted

def recode1to5agreement(col2convert):
    
    col_converted = col2convert.replace({"1"	: "1_Strongly agree",
                                         "2" : "2_Agree",
                                         "3" : "3_Neither/nor",
                                         "4" : "4_Disagree",
                                         "5" : "5_Strongly disagree",
                                         " " : np.nan})
    
    return col_converted

def recode_food(col2convert):
    
    col_converted = col2convert.replace({'1' : "1_Never",
                                        '2' : "2_LessOnceWeek", 
                                        '3' : "3_OnceWeek", 
                                        '4' : "4_2-4DayWeek",
                                        '5' : "5_5-6DaysWeek", 
                                        '6' : "6_OnceDaily", 
                                        '7' : "7_MoreOnceDaily",
                                        ' ' : np.nan})
    
    print(col_converted.value_counts(dropna = False))
    return col_converted

def recode_vice(col2convert):
    
    col_converted = col2convert.replace({'1' : "1_Never",
                                         '2' : "2_1-2 days", 
                                         '3' : "3_3-5 days",
                                         '4' : "4_6-9 days",
                                         '5' : "5_10-19 days", 
                                         '6' : "6_20-29 days",
                                         '7' : "7_30 days (or more)",
                                         '-99' : "-99_MissingInconsistentAnswer",
                                         ' ' : np.nan})
    
    print(col_converted.value_counts(dropna = False))
    return col_converted

def recode_bullying(col2convert):
    
    col_converted = col2convert.replace({'1' : '1_Havent',
                                        '2' : '2_OnceTwice',
                                        '3' : '3_2-3XPerMonth',
                                        '4' : '4_OnceWeek',
                                        '5' : '5_SeveralWeek',
                                        ' ' : np.nan})
    
    return col_converted    

def recode_fight(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_0x',
        '2' : '2_1x',
        '3' : '3_2x',
        '4' : '4_3x',
        '5' : '5_+4x',
        ' ' : np.nan
        })
    
    return col_converted

def recode_friends(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_StrongDisagree',
        '2' : '2_Disagree',
        '3' : '3_SomewhatDisagree',
        '4' : '4_Neutral',
        '5' : '5_SomewhatAgree',
        '6' : '6_Agree',
        '7' : '7_StrongAgree',
        ' ' : np.nan
        })
    
    return col_converted
    
def recode_onlinecomms(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_DontKnow/NA',
        '2' : '2_Never/AlmostNever',
        '3' : '3_Weekly',
        '4' : '4_Daily',
        '5' : '5_SeveralDaily',
        '6' : '6_AllTheTime',
        ' ' : np.nan
        })
    
    return col_converted

def recode_onlineprefs(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_StronglyDisagree', 
        '2' : '2_Disagree', 
        '3' : '3_Neither', 
        '4' : '4_Agree',
        '5' : '5_StronglyAgree', 
        '99' : '99_MissingDueToSkipPattern',
        ' ' : np.nan        
        })
    
    return col_converted

def recode_socmed(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_No',
        '2' : '2_Yes',
        '99' : '99_MissingDueToSkipPattern',
        ' ' : np.nan        
        })
    
    return col_converted

def recode_sex(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_Yes',
        '2' : '2_No',
        '3' : '3_DontKnow',
        ' ' : np.nan        
        })
    
    return col_converted

def recode_presence(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_Yes',
        '2' : '2_No',
        ' ' : np.nan        
        })
    
    return col_converted

def recode_easy_to_talk(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_Very easy', 
        '2' : '2_Easy',
        '3' : '3_Difficult',
        '4' : '4_VeryDifficult',
        '5' : '5_DontHaveOrSee',
        ' ' : np.nan        
        })
    
    return col_converted

def recode_support(col2convert):
    
    col_converted = col2convert.replace({
        '1' : '1_Very strongly disagree',
        '2' : '2',
        '3' : '3',
        '4' : '4',
        '5' : '5',
        '6' : '6',
        '7' : '7_Very strongly agree',
        ' ' : np.nan        
        })
    
    return col_converted
   
# load data, basic EDA and makes working copy
dat_file_path = "C:\\MisLocalFiles\\Github\\HBSC\\data\\HBSC2018OAed1.1.csv"
hbsc2018 = pd.read_csv(dat_file_path, decimal=',', sep=';')

hbsc2018.shape #244097, 120
hbsc2018.info()

d = hbsc2018.copy()
coltypes = d.columns.tolist()
d.isna().sum()

# output columns to csv
cols_hbsc2018 = hbsc2018.columns
cols_hbsc2018 = pd.DataFrame(cols_hbsc2018)
cols_hbsc2018.to_csv("./other/cols_hbsc2018.csv")

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
del (d['countryno'])

# 'region'
d['region'].value_counts(dropna = False) #don't seem to add value, country is better

# 'id1', 'id2', 'id3', 'id4'
# these are just number Ids that don't seem to mean anything
d['id1'].value_counts(dropna = False)
d['id2'].value_counts(dropna = False)
d['id3'].value_counts(dropna = False)
d['id4'].value_counts(dropna = False)

# 'weight' weight of dataset. what is this?
d['weight']

# 'adm'
d['adm'].value_counts(dropna = False)

recode_map = { 1 : '1_Paper',
               2 : '2_Computer'
               }

d['adm_recoded'] = d['adm'].map(recode_map)
d['adm_recoded'].value_counts(dropna = False)
del d['adm']

# 'year'
d['year'].value_counts(dropna = False) #2017 to 2019

# 'month'
d['month'].value_counts(dropna = False) #so odds that it repeats numbers... missing?
pd.crosstab(d['year'], d['month']) #that makes make sense so do yyyymm

#fix later
#d['year'] = d['year'].astype(str)
#d['month'] = d['month'].astype(str).str.zfill(2)
#d['year_month'] = d['year'].str.cat(d['month'], sep='_')
#d['year_month'].value_counts(dropna = False)

# 'age'
d['age'].info()
d['age'].describe()

d['age_recoded'] = d['age'].replace(",", ".").replace(' ', np.nan)
d['age_recoded'] = pd.to_numeric(d['age_recoded'], errors='coerce') 
d['age_recoded'].describe()
d['age_recoded'].hist()
d['age_recoded'].isna().sum() #do something with NAs
del d['age']

# 'agecat'
d['agecat'].astype(str).value_counts(dropna = False) #ok, but was is 1-3?
d.groupby('agecat')['age_recoded'].describe() # it seems to be 1=11; 2=13; 3=15 year olds

# 'sex'
d['sex'].value_counts(dropna = False)
d['sex_recoded'] = d['sex'].astype(str).replace({'1': '1_male', 
                                                 '2': '2_female'})
d['sex_recoded'].value_counts(dropna = False)
del d['sex']

# 'grade'
d['grade'].value_counts(dropna = False)
d['grade_recoded'] = d['grade'].astype(str).replace({'1': '1_11yo', 
                                                     '2': '2_13yo', 
                                                     '3': '3_15yo'})
d['grade_recoded'].value_counts(dropna = False) #huge SysMiss
del d['grade']

pd.crosstab(d['grade_recoded'], d['agecat']) #kind of 1:1 but sadly many NAs

# 'health'
d['health'].value_counts(dropna = False)

d['health_recoded'] = d['health'].astype(str).replace({'1': '1_Excellent', 
                                                       '2': '2_Good', 
                                                       '3': '3_Fair',
                                                       '4': '4_Poor'})

d['health_recoded'].value_counts(dropna = False)
del d['health']

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
del d['lifesat']

# 'feellow'
d['feellow'].value_counts(dropna = False)

d['feellow_recoded'] = recode1to5feelings(d['feellow'])
d['feellow_recoded'].value_counts(dropna = False)
d['feellow_recoded'].value_counts(dropna = False).plot.bar()
del d['feellow']
                         
# 'irritable'
d['irritable_recoded'] = recode1to5feelings(d['irritable'])
d['irritable_recoded'].value_counts(dropna = False)
del d['irritable']

# 'nervous'
d['nervous_recoded'] = recode1to5feelings(d['nervous'])
d['nervous_recoded'].value_counts(dropna = False)
del d['nervous']

# 'sleepdificulty'
d['sleepdificulty_recoded'] = recode1to5feelings(d['sleepdificulty'])
d['sleepdificulty_recoded'].value_counts(dropna = False)
del d['sleepdificulty']

#'dizzy'
d['dizzy_recoded'] = recode1to5feelings(d['dizzy'])
d['dizzy_recoded'].value_counts(dropna = False)
del d['dizzy']

#'headache'
d['headache_recoded'] = recode1to5feelings(d['headache'])
d['headache_recoded'].value_counts(dropna = False)
del d['headache']

#'stomachache'
d['stomachache_recoded'] = recode1to5feelings(d['stomachache'])
d['stomachache_recoded'].value_counts(dropna = False)
del d['stomachache']

#'backache'
d['backache_recoded'] = recode1to5feelings(d['backache'])
d['backache_recoded'].value_counts(dropna = False)
del d['backache']

# 'studtogether'
d['studtogether_recoded'] = recode1to5agreement(d['studtogether'])
d['studtogether_recoded'].value_counts(dropna = False)
del d['studtogether']

# 'studhelpful'
d['studhelpful_recoded'] = recode1to5agreement(d['studhelpful'])
d['studhelpful_recoded'].value_counts(dropna = False)
del d['studhelpful']

# 'studaccept'
d['studaccept_recoded'] = recode1to5agreement(d['studaccept'])
d['studaccept_recoded'].value_counts(dropna = False)
del d['studaccept']

# 'teacheraccept'
d['teacheraccept_recoded'] = recode1to5agreement(d['teacheraccept'])
d['teacheraccept_recoded'].value_counts(dropna = False)
del d['teacheraccept']

# 'teachercare'
d['teachercare_recoded'] = recode1to5agreement(d['teachercare'])
d['teachercare_recoded'].value_counts(dropna = False)
del d['teachercare']

# 'teachertrust'
d['teachertrust_recoded'] = recode1to5agreement(d['teachertrust'])
d['teachertrust_recoded'].value_counts(dropna = False)
del d['teachertrust']

# 'monthbirth', 'yearbirth'
# useless since we have age and grade above
d['yearbirth'].astype(str).value_counts(dropna = False)
d['monthbirth'].astype(str).value_counts(dropna = False)

# 'fasfamcar', family car
d['fasfamcar_recoded'] = d['fasfamcar'].replace({'1': '1_No', 
                                                 '2': '2_Yes1', 
                                                 '3': '3_Yes2+',
                                                 ' ': np.nan})

d['fasfamcar_recoded'].value_counts(dropna=False)
del d['fasfamcar']

# 'fasbedroom', Own bedroom
d['fasbedroom_recoded'] = d['fasbedroom'].replace({'1': '1_No', 
                                                   '2': '2_Yes', 
                                                   ' ': np.nan})

d['fasbedroom_recoded'].value_counts(dropna=False)
del d['fasbedroom']

# fascomputers: # computers
d['fascomputers_recoded'] = d['fascomputers'].replace({'1': '1_None', 
                                                 '2': '2_Yes1', 
                                                 '3': '3_Yes2',
                                                 '4': '4_Yes3+',
                                                 ' ': np.nan})

d['fascomputers_recoded'].value_counts(dropna=False)
del d['fascomputers']

# fasbathroom: # bathrooms
d['fasbathroom_recoded'] = d['fasbathroom'].replace({'1': '1_None', 
                                                 '2': '2_Yes1', 
                                                 '3': '3_Yes2',
                                                 '4': '4_Yes3+',
                                                 ' ': np.nan})

d['fasbathroom_recoded'].value_counts(dropna=False)
del d['fasbathroom']

# fasdishwash: dishwasher in home
d['fasdishwash_recoded'] = d['fasdishwash'].replace({'1': '1_No', 
                                                     '2': '2_Yes', 
                                                     ' ': np.nan})

d['fasdishwash_recoded'].value_counts(dropna=False)
del d['fasdishwash']

# fasholidays: family holidays (# trips last year)
d['fasholidays_recoded'] = d['fasholidays'].replace({'1': '1_None', 
                                                     '2': '2_Once',
                                                     '3' : '3_Twice',
                                                     '4' : '4_MoreThanTwice',
                                                     ' ': np.nan})

d['fasholidays_recoded'].value_counts(dropna=False)
del d['fasholidays']

#thinkbody: think about body (size)
d['thinkbody_recoded'] = d['thinkbody'].replace({'1': '1_MuchTooThin', 
                                                 '2': '2_BitTooThin',
                                                 '3' : '3_AboutRight',
                                                 '4' : '4_BitTooFat',
                                                 '5' : '5_MuchTooFat',
                                                 ' ': np.nan})

d['thinkbody_recoded'].value_counts(dropna=False)
del d['thinkbody']

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
del d['breakfastwd']

# 'breakfastwe': # weekend days with breakfast
d['breakfastwe_recoded'] = d['breakfastwe'].replace({'1': 0, 
                                                 '2': 1,
                                                 '3' : 2,
                                                 ' ': np.nan})

d['breakfastwe_recoded'].value_counts(dropna=False)
del d['breakfastwe']

# 'fruits_2'
d['fruits_2_recoded'] = recode_food(d['fruits_2'])
del d['fruits_2']

# 'vegetables_2'
d['vegetables_2_recoded'] = recode_food(d['vegetables_2'])
del d['vegetables_2']

# 'sweets_2'
d['sweets_2_recoded'] = recode_food(d['sweets_2'])
del d['sweets_2']

# 'softdrinks_2'
d['softdrinks_2_recoded'] = recode_food(d['softdrinks_2'])
del d['softdrinks_2']

# 'fmeal': family meals together
d['fmeal_recoded'] = d['fmeal'].replace({'1' : "1_Daily", 
                                           '2' : "2_MostDays",
                                           '3' : "3_OnceWeek",
                                           '4' : "4_LessOften",
                                           '5' : "5_Never",
                                           ' ': np.nan})

d['fmeal_recoded'].value_counts(dropna=False)
del d['fmeal']

# 'toothbr: often brush teeth
d['toothbr_recoded'] = d['toothbr'].replace({'1' : "1_MoreThanOnceDay",
                                             '2' : "2_OnceDay",
                                             '3' : "3_OnceWeek", 
                                             '4' : "4_LessThanWeekly",
                                             '5' : "5_Never", 
                                             ' ' : np.nan})

d['toothbr_recoded'].value_counts(dropna=False)
del d['toothbr']

# 'timeexe': vigorous phys activity
d['timeexe_recoded'] = d['timeexe'].replace({'1' : "1_EveryDay", 
                                             '2' : "2_4-6xWeek",
                                             '3' : "3_2-3x",
                                             '4' : "4_OnceWeek",
                                             '5' : "5_OnceMonth",
                                             '6' : "6_LessThanOnceMonth",
                                             '7' : "7_Never", 
                                             ' ' : np.nan})

d['timeexe_recoded'].value_counts(dropna=False)
del d['timeexe']

# 'smokltm': # days smoked ever
d['smokltm_recoded'] = recode_vice(d['smokltm'])
del d['smokltm']

# 'smok30d_2': days smoked last 30 days
d['smok30d_2_recoded'] = recode_vice(d['smok30d_2'])
del d['smok30d_2']

# alcltm: #days drank in lifetime
d['alcltm_recoded'] = recode_vice(d['alcltm'])
del d['alcltm']

# alc30d_2: #days drank last 30 days
d['alc30d_2_recoded'] = recode_vice(d['alc30d_2'])
del d['alc30d_2']

# drunkltm: Have you ever had so much alcohol that you were really drunk? in lifetime
d['drunkltm'].value_counts(dropna=False)

d['drunkltm_recoded'] = d['drunkltm'].replace({'1' : "1_Never", 
                                             '2' : "2_Once",
                                             '3' : "3_2-3x",
                                             '4' : "4_4-10x",
                                             '5' : "5_MoreThan10",
                                             '-99' : "-99_InconsistentAnswer",
                                             ' ' : np.nan})

pd.crosstab(d['drunkltm'], d['drunkltm_recoded'], dropna=False)
del d['drunkltm']

# drunk30d; really drunk past 30 days
d['drunk30d_recoded'] = d['drunk30d'].replace({'1' : "1_Never", 
                                             '2' : "2_Once",
                                             '3' : "3_2-3x",
                                             '4' : "4_4-10x",
                                             '5' : "5_MoreThan10",
                                             '-99' : "-99_InconsistentAnswer",
                                             ' ' : np.nan})

pd.crosstab(d['drunk30d'], d['drunk30d_recoded'], dropna=False)
del d['drunk30d']

# cannabisltm_2: Cannabis life time
d['cannabisltm_2_recoded'] = d['cannabisltm_2'].replace({'1' : "1_Never", 
                                             '2' : "2_1-2Days",
                                             '3' : "3_3-5Days",
                                             '4' : "4_6-9Days",
                                             '5' : "5_10-19Days",
                                             '6' : "6_20-29Days",
                                             '7' : "7_30DaysOrMore",
                                             ' ' : np.nan})

pd.crosstab(d['cannabisltm_2'], d['cannabisltm_2_recoded'], dropna=False)
del d['cannabisltm_2']

# cannabis30d_2: Cannabis last 30 days
d['cannabis30d_2_recoded'] = d['cannabis30d_2'].replace({'1' : "1_Never", 
                                             '2' : "2_1-2Days",
                                             '3' : "3_3-5Days",
                                             '4' : "4_6-9Days",
                                             '5' : "5_10-19Days",
                                             '6' : "6_20-29Days",
                                             '7' : "7_30DaysOrMore",
                                             ' ' : np.nan})

pd.crosstab(d['cannabis30d_2'], d['cannabis30d_2_recoded'], dropna=False)
del d['cannabis30d_2']

# bodyweight, weight without clothes- how much you weight? Kilo
d['bodyweight'].replace(' ', np.nan).astype(float).describe()
d['bodyweight_recoded'] = d['bodyweight'].replace(' ', np.nan).astype(float)
del d['bodyweight']

#bodyheight, how tall? cm no shoes
d['bodyheight'].replace(' ', np.nan).astype(float).describe()
d['bodyheight_recoded'] = d['bodyheight'].replace(' ', np.nan).astype(float)
del d['bodyheight']

# likeschool, How do you feel about school at present?
d['likeschool_recoded'] = d['likeschool'].replace({'1' : '1_ALot',
                                                  '2' : '2_ABit',
                                                  '3' : '3_NotMuch',
                                                  '4' : '4_NotAtAll',
                                                  ' ' : np.nan})

print(pd.crosstab(d['likeschool'], d['likeschool_recoded'], dropna=False))
del d['likeschool']

# schoolpressure: How pressured do you feel by the schoolwork you have to do?
d['schoolpressure_recoded'] = d['schoolpressure'].replace({'1' : '1_NotAtAll',
                                                  '2' : '2_ALittle',
                                                  '3' : '3_Some',
                                                  '4' : '4_ALot',
                                                  ' ' : np.nan})

print(pd.crosstab(d['schoolpressure'], d['schoolpressure_recoded'], dropna=False))
d['schoolpressure']

# bulliedothers
d['bulliedothers_recoded'] = recode_bullying(d['bulliedothers'])
print(pd.crosstab(d['bulliedothers_recoded'], d['bulliedothers']))
del d['bulliedothers']

# beenbullied
d['beenbullied_recoded'] = recode_bullying(d['beenbullied'])
print(pd.crosstab(d['beenbullied_recoded'], d['beenbullied']))
del d['beenbullied']

# cbulliedothers
d['cbulliedothers_recoded'] = recode_bullying(d['cbulliedothers'])
print(pd.crosstab(d['cbulliedothers_recoded'], d['cbulliedothers']))
del d['cbulliedothers']

# cbeenbullied
d['cbeenbullied_recoded'] = recode_bullying(d['cbeenbullied'])
print(pd.crosstab(d['cbeenbullied_recoded'], d['cbeenbullied']))
del d['cbeenbullied']

# fight12m, #During the past 12 months, how many times were you in a physical fight?
d['fight12m_recoded'] = recode_fight(d['fight12m'])
print(pd.crosstab(d['fight12m_recoded'], d['fight12m']))
del d['fight12m']
    
# injured12m, During the past 12 months, how many times were you injured and had to be treated
#by a doctor or nurse
d['injured12m_recoded'] = recode_fight(d['injured12m'])
print(pd.crosstab(d['injured12m_recoded'], d['injured12m']))
del d['injured12m']

# friendhelp: Literal question My friends really try to help me
d['friendhelp_recoded'] = recode_friends(d['friendhelp'])
pd.crosstab(d['friendhelp_recoded'], d['friendhelp'], dropna=False)
del d['friendhelp']

# friendcounton: I can count on my friends when things go wrong
d['friendcounton_recoded'] = recode_friends(d['friendcounton'])
pd.crosstab(d['friendcounton_recoded'], d['friendcounton'], dropna=False)
del d['friendcounton']

# friendshare: I have friends with whom I can share my joys and sorrows
d['friendshare_recoded'] = recode_friends(d['friendshare'])
pd.crosstab(d['friendshare_recoded'], d['friendshare'], dropna=False)
del d['friendshare']

# friendtalk: I can talk about my problems with my friends
d['friendtalk_recoded'] = recode_friends(d['friendtalk'])
pd.crosstab(d['friendtalk_recoded'], d['friendtalk'], dropna=False)
del d['friendtalk']

#emconlfreq1: Onl contact close friends
d['emconlfreq1_recoded'] = recode_onlinecomms(d['emconlfreq1'])
pd.crosstab(d['emconlfreq1_recoded'], d['emconlfreq1'], dropna=False)
del d['emconlfreq1']

# emconlfreq2: Onl contact larger friend group
d['emconlfreq2_recoded'] = recode_onlinecomms(d['emconlfreq2'])
pd.crosstab(d['emconlfreq2_recoded'], d['emconlfreq2'], dropna=False)
del d['emconlfreq2']

# emconlfreq3: Onl contact online friends
d['emconlfreq3_recoded'] = recode_onlinecomms(d['emconlfreq3'])
pd.crosstab(d['emconlfreq3_recoded'], d['emconlfreq3'], dropna=False)
del d['emconlfreq3']

# emconlfreq4: Onl contact other
d['emconlfreq4_recoded'] = recode_onlinecomms(d['emconlfreq4'])
pd.crosstab(d['emconlfreq4_recoded'], d['emconlfreq4'], dropna=False)
del d['emconlfreq4']

# emconlpref1: On the internet, I talk more easily about secrets than in a
# face-to-face encounter
d['emconlpref1_recoded'] = recode_onlineprefs(d['emconlpref1'])
pd.crosstab(d['emconlpref1_recoded'], d['emconlpref1'], dropna=False)
del d['emconlpref1']

# emconlpref2: talk more easily about feelings on internet
d['emconlpref2_recoded'] = recode_onlineprefs(d['emconlpref2'])
pd.crosstab(d['emconlpref2_recoded'], d['emconlpref2'], dropna=False)
del d['emconlpref2']

# emconlpref3: talk more easilyt about concerns on internet
d['emconlpref3_recoded'] = recode_onlineprefs(d['emconlpref3'])
pd.crosstab(d['emconlpref3_recoded'], d['emconlpref3'], dropna=False)
del d['emconlpref3']

# emcsocmed1: Social media: Can't think of anything else
d['emcsocmed1_recoded'] = recode_socmed(d['emcsocmed1'])
pd.crosstab(d['emcsocmed1_recoded'], d['emcsocmed1'], dropna=False)
del d['emcsocmed1']

# emcsocmed2: SM, dissatisfied cause want to spend more time
d['emcsocmed2_recoded'] = recode_socmed(d['emcsocmed2'])
pd.crosstab(d['emcsocmed2_recoded'], d['emcsocmed2'], dropna=False)
del d['emcsocmed2']

# emcsocmed3: SM, often felt bad when you could not use social media?
d['emcsocmed3_recoded'] = recode_socmed(d['emcsocmed3'])
pd.crosstab(d['emcsocmed3_recoded'], d['emcsocmed3'], dropna=False)
del d['emcsocmed3']

# emcsocmed4: ... tried to spend less time on social media, but failed?
d['emcsocmed4_recoded'] = recode_socmed(d['emcsocmed4'])
pd.crosstab(d['emcsocmed4_recoded'], d['emcsocmed4'], dropna=False)
del d['emcsocmed4']

# emcsocmed5: neglected other activities (e.g. hobbies, sport) because sm media?
d['emcsocmed5_recoded'] = recode_socmed(d['emcsocmed5'])
pd.crosstab(d['emcsocmed5_recoded'], d['emcsocmed5'], dropna=False)
del d['emcsocmed5']

# emcsocmed6 , ... regularly had arguments with others because of your social
d['emcsocmed6_recoded'] = recode_socmed(d['emcsocmed6'])
pd.crosstab(d['emcsocmed6_recoded'], d['emcsocmed6'], dropna=False)
del d['emcsocmed6']

# emcsocmed7, .. regularly lied to your parents or friends about the amount of SM
d['emcsocmed7_recoded'] = recode_socmed(d['emcsocmed7'])
pd.crosstab(d['emcsocmed7_recoded'], d['emcsocmed7'], dropna=False)
del d['emcsocmed7']

#emcsocmed8, uses SM to escape feelings
d['emcsocmed8_recoded'] = recode_socmed(d['emcsocmed8'])
pd.crosstab(d['emcsocmed8_recoded'], d['emcsocmed8'], dropna=False)
del d['emcsocmed8']

#emcsocmed9,  had serious conflict with your parents, brother(s) or sister(s) cause of SM
d['emcsocmed9_recoded'] = recode_socmed(d['emcsocmed9'])
pd.crosstab(d['emcsocmed9_recoded'], d['emcsocmed9'], dropna=False)
del d['emcsocmed9']

# hadsex, ever?
d['hadsex_recoded'] = d['hadsex'].replace({'1' : '1_Yes',
                                           '2' : '2_No',
                                           ' ' : np.nan})

pd.crosstab(d['hadsex_recoded'], d['hadsex'], dropna=False)
del d['hadsex']

# agesex
d['agesex_recoded'] = d['agesex'].replace({
    '1' : '1_11YearsOrYounger', 
    '2' : '2_12Years', 
    '3' : '3_13Years',
    '4' : '4_14Years', 
    '5' : '5_15Years',
    '6' : '6_16YearsOrOlder',
    ' ' : np.nan    
    })

pd.crosstab(d['agesex_recoded'], d['agesex'], dropna=False)
del d['agesex']

#contraceptcondom, used last time?
d['contraceptcondom_recoded'] = recode_socmed(d['contraceptcondom'])
pd.crosstab(d['contraceptcondom_recoded'], d['contraceptcondom'], dropna=False)
del d['contraceptcondom']

# contraceptpill, used last time?
d['contraceptpill_recoded'] = recode_socmed(d['contraceptpill'])
pd.crosstab(d['contraceptpill_recoded'], d['contraceptpill'], dropna=False)
del d['contraceptpill']

# countryborn, ISO 3166 country child born
d['countryborn'].value_counts(dropna=False)
#179 values, 69k ' '

# countrybornmo, ISO 3166 country mom born
d['countrybornmo'].value_counts(dropna=False)
#189 values, 70k+ ' '

# countrybornfa, iso 3166 country father born
d['countrybornfa'].value_counts(dropna=False)
#189 values, 70k+ ' '

# motherhome1: mom lives in main home
d['motherhome1_recoded'] = recode_presence(d['motherhome1'])
pd.crosstab(d['motherhome1_recoded'], d['motherhome1'], dropna=False)
del d['motherhome1']

# fatherhome1: father lives in main home
d['fatherhome1_recoded'] = recode_presence(d['fatherhome1'])
pd.crosstab(d['fatherhome1_recoded'], d['fatherhome1'], dropna=False)
del d['fatherhome1']

# stepmohome1: stepmom lives in main home
d['stepmohome1_recoded'] = recode_presence(d['stepmohome1'])
pd.crosstab(d['stepmohome1_recoded'], d['stepmohome1'], dropna=False)
del d['stepmohome1']

# stepfahome1, step father in main home
d['stepfahome1_recoded'] = recode_presence(d['stepfahome1'])
pd.crosstab(d['stepfahome1_recoded'], d['stepfahome1'], dropna=False)
del d['stepfahome1']

# fosterhome1, live in foster home?
d['fosterhome1_recoded'] = recode_presence(d['fosterhome1'])
pd.crosstab(d['fosterhome1_recoded'], d['fosterhome1'], dropna=False)
del d['fosterhome1']

# elsehome1_2, living with someone else?
d['elsehome1_2_recoded'] = recode_presence(d['elsehome1_2'])
pd.crosstab(d['elsehome1_2_recoded'], d['elsehome1_2'], dropna=False)
del d['elsehome1_2']

# employfa, father has job?
d['employfa_recoded'] = d['employfa'].replace({
    '1' : '1_Yes', 
    '2' : '2_No', 
    '3' : '3_DontKnow',
    '4' : '4_DontKnowOrSee',
    ' ' : np.nan
    })

pd.crosstab(d['employfa_recoded'], d['employfa'], dropna=False)
del d['employfa']

# employmo, mom has job?
d['employmo_recoded'] = d['employmo'].replace({
    '1' : '1_Yes', 
    '2' : '2_No', 
    '3' : '3_DontKnow',
    '4' : '4_DontKnowOrSee',
    ' ' : np.nan
    })

pd.crosstab(d['employmo_recoded'], d['employmo'], dropna=False)
del d['employmo']

# employnotfa, if no father employed, why not?
d['employnotfa_recoded'] = d['employnotfa'].replace({
    '1' : '1_Sick/retired/stud', 
    '2' : '2_Looking for work',
    '3' : '3_Care/home',
    '4' : '4_Dont know',
    ' ' : np.nan    
    })

pd.crosstab(d['employnotfa_recoded'], d['employnotfa'], dropna=False)
del d['employnotfa']

# employnotmo, If NO , why does your mother not have a job?
d['employnotmo_recoded'] = d['employnotmo'].replace({
    '1' : '1_Sick/retired/stud', 
    '2' : '2_Looking for work',
    '3' : '3_Care/home',
    '4' : '4_Dont know',
    ' ' : np.nan    
    })

pd.crosstab(d['employnotmo_recoded'], d['employnotmo'], dropna=False)
del d['employnotmo']

# talkfather, How easy is it for you to talk to about things that really bother you?
d['talkfather_recoded'] = recode_easy_to_talk(d['talkfather'])
pd.crosstab(d['talkfather_recoded'], d['talkfather'], dropna=False)
del d['talkfather']

# talkstepfa: Talk to stepfather
d['talkstepfa_recoded'] = recode_easy_to_talk(d['talkstepfa'])
pd.crosstab(d['talkstepfa_recoded'], d['talkstepfa'], dropna=False)
del d['talkstepfa']

# talkmother, talk to mother
d['talkmother_recoded'] = recode_easy_to_talk(d['talkmother'])
pd.crosstab(d['talkmother_recoded'], d['talkmother'], dropna=False)
del d['talkmother']

# talkstepmo, talk to step mother
d['talkstepmo_recoded'] = recode_easy_to_talk(d['talkstepmo'])
pd.crosstab(d['talkstepmo_recoded'], d['talkstepmo'], dropna=False)
del d['talkstepmo']

# famhelp: Family tries to help
d['famhelp_recoded'] = recode_support(d['famhelp'])
pd.crosstab(d['famhelp_recoded'], d['famhelp'], dropna=False)
del d['famhelp']

# famsup: Get emotional help
d['famsup_recoded'] = recode_support(d['famsup'])
pd.crosstab(d['famsup_recoded'], d['famsup'], dropna=False)
del d['famsup']

# famtalk: Talk about problems
d['famtalk_recoded'] = recode_support(d['famtalk'])
pd.crosstab(d['famtalk_recoded'], d['famtalk'], dropna=False)
del d['famtalk']

# famdec: Help make decisions
d['famdec_recoded'] = recode_support(d['famdec'])
pd.crosstab(d['famdec_recoded'], d['famdec'], dropna=False)
del d['famdec']

# MBMI: Body Mass Index
# 0 Outside overall range
d['MBMI_recoded'] = np.where(d['MBMI'] == ' ', np.nan, d['MBMI'].str.replace(',','.'))
d['MBMI_recoded'] = d['MBMI_recoded'].astype(float)
d['MBMI_recoded'].head(25)
d['MBMI_recoded'].describe()
del d['MBMI']                       
 
# IRFAS: Family affluence scale III - continuous
# Based on the fas-variables in the dataset
# 1-13 integer
d['IRFAS_recoded'] = np.where(d['IRFAS'] == ' ', np.nan, d['IRFAS']) 
d['IRFAS_recoded'] = d['IRFAS_recoded'].str.zfill(width=2)
d['IRFAS_recoded'].value_counts(dropna=False).sort_index()
del d['IRFAS']

# IRRELFAS_LMH: Relative family affluence categorical
# Definition Based on the fas-variables in the dataset.
d['IRRELFAS_LMH_recoded'] = d['IRRELFAS_LMH'].replace({
    '1' : '1_Lowest20pct', 
    '2' : '2_Medium60pct', 
    '3' : '3_Highest20pct',
    ' ' : np.nan 
    })

pd.crosstab(d['IRRELFAS_LMH_recoded'], d['IRRELFAS_LMH'], dropna=False)
del d['IRRELFAS_LMH']

#'IOTF4', Definition BMI GROUP USING COLE ET AL METHOD 2002.
d['IOTF4_recoded'] = d['IOTF4'].replace({
    '1' : '1_Thinness', 
    '2' : '2_Normalweight',
    '3' : '3_Overweight', 
    '4' : '4_Obesity',
    ' ' : np.nan    
    })

pd.crosstab(d['IOTF4_recoded'], d['IOTF4'], dropna=False)
del d['IOTF4']

# oweight_who: Classified as overweight or obese by WHO (bmiplus1=1)
d['oweight_who_recoded'] = d['oweight_who'].replace({
    '0' : '0_No',
    '1' : '1_Yes',
    ' ' : np.nan
    })

pd.crosstab(d['oweight_who_recoded'], d['oweight_who'], dropna=False)
del d['oweight_who']

# export friendly csv
d.to_csv('d.csv', index=False)



