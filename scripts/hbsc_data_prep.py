# -*- coding: utf-8 -*-
"""
Created on Wed Jun  3 16:42:08 2026

@author: wagui
"""

import os
import pandas as pd
import numpy as np

project_folder = "C:/MisLocalFiles/Github/HBSC/"
os.chdir(project_folder)

def recode_pmsu(old_col):
    new_col = old_col.replace({"1" : 0,
                               "2" : 1,
                               "99" : 99,
                               " " : np.nan})
    return new_col

hbsc2018 = pd.read_csv("./data/HBSC2018OAed1.1.csv",
                       sep = ";")
hbsc2018_cols = hbsc2018.columns
hbsc2018_cols.to_series().to_csv("./data/hbsc2018_cols.csv")

dat = hbsc2018.copy()

# dat['emcsocmed1_r'] = recode_pmsu(dat['emcsocmed1'])

# pd.crosstab(dat["emcsocmed1"], 
#             dat["emcsocmed1_r"],
#             dropna=False)

# recode all 9 pmsu variables and double check cross tabs
cols = [f"emcsocmed{i}" for i in range(1, 10)]

for col in cols:
    dat[f"{col}_r"] = recode_pmsu(dat[col])
    print(f"\n\nCross‑tab for {col}:")
    print(pd.crosstab(dat[col], dat[f"{col}_r"], dropna=False))

dat.drop(columns=cols, inplace=True)

# drop rows with na or 99 in pmsu vars and then SUM
cols_r = [f"emcsocmed{i}_r" for i in range(1, 10)]

for v in cols_r:
    dat = dat[(dat[v] != 99) & (dat[v].notna())]

dat["emcsocmed_sum"] = dat[cols_r].sum(axis=1)
dat["emcsocmed_sum"].describe()
dat["emcsocmed_sum"].value_counts()

dat["emcsocmed_pmsu_bin"] = pd.cut(
    dat["emcsocmed_sum"],
    bins=[-1, 1, 5, 9],
    labels=["Low(0-1)", "Med(2-5)", "High(6-9)"]
)
print(dat["emcsocmed_pmsu_bin"].value_counts(dropna=False))

# recode family support variables from likert to H/M/L
dat['fam_support'] = (
    pd.to_numeric(dat["famhelp"], errors="coerce") +
    pd.to_numeric(dat["famsup"], errors="coerce") +
    pd.to_numeric(dat["famtalk"], errors="coerce")
)
dat['fam_support'].describe()

dat["family_support_bin"] = pd.cut(
    dat["fam_support"],
    bins=[2, 8, 14, 21],
    labels=["Low(3-8)", "Med(9-14)", "High(15-21)"]
)
dat["family_support_bin"].value_counts(dropna=False)

# family affluence
dat["IRRELFAS_LMH"].value_counts()
dat["IRRELFAS_LMH_r"] = dat["IRRELFAS_LMH"].replace({
    "1" : "1_Low20",
    "2" : "2_Mid60",
    "3" : "3_High20",
    "": np.nan, 
    " ": np.nan} 
    )
dat["IRRELFAS_LMH_r"].value_counts(dropna=False).sort_index()

