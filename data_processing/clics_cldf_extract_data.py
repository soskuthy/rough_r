# importing relevant modules

from cltoolkit import Wordlist
from pyclts import CLTS
from glob import glob
from csvw.dsv import UnicodeDictReader
import pycldf
import csv
from pathlib import Path
from lingpy import Wordlist as LPWordlist

# This script exports all forms that instantiate
# the concepts "rough" and "smooth" in the CLICS3
# database. The data are extracted from the Lexibank
# cross-linguistic lexical database using tools from
# the Cross-Linguistic Data Formats (CLDF) initiative.

# Instructions for installing & setting up lexibank:
# https://github.com/lexibank/lexibank-analysed/blob/main/workflow.md

# Note 1: the tryonsolomon and leejaponic data sets had to be
# added manually to the raw folder inside lexibank-analysed
# (via https://github.com/lexibank/tryonsolomon and
# https://github.com/lexibank/leejaponic).

# Note 2: the lexirumah data set, which is part of CLICS3,
# was not included in the version of the Lexibank data set
# available via GitHub. We therefore download and process 
# this data set separately.
# (using git clone https://github.com/lessersunda/lexirumah-data).

# opening a list of the clics data sets downloaded from
# the clics3 website
with open("/Users/soskuthy/Documents/Research/current/2019/bodo_r/lexibank/clics_data.txt", "r") as clics_f:
    clics_data = [l.strip() for l in clics_f.readlines()]

# setting up variables for the data extraction
data = {}
idx = 1
smooth_rough = ["SMOOTH", "ROUGH"]

# main loop iterating through data sets
for row in clics_data:
    try:
        # importing CLDF data set from metadata &
        # generating wordlist
        pp = Path("/Users/soskuthy/Documents/Research/current/2019/bodo_r/lexibank/lexibank-analysed/raw", row, 
                    "cldf", "cldf-metadata.json")
        print(pp)
        ds = pycldf.Dataset.from_metadata(pp)
        wl = Wordlist([ds])

        # out of rough / smooth, which are available in the
        # current data set?
        concept_list = [c for c in smooth_rough if c in wl.concepts]

        # iterate through rough and smooth
        for concept in concept_list:
            # iterate through all forms in the data set
            # instantiating the relevant concept
            for form in wl.concepts[concept].forms:
                # the form (usually a phonemic transcription)
                form_out = form.form
                # in IDS, the form is often represented in 
                # cyrillic script, but an alternative phonemic
                # transcription is typically also available;
                # use the phonemic transcription!
                if row=="ids" and "Transcriptions" in form.data.keys():
                    # skip forms with only cyrillic script
                    if len(form.data["Transcriptions"])==1 and form.data["Transcriptions"][0]=="CyrillTrans":
                        continue
                    # use phonemic transcription where available
                    if len(form.data["Transcriptions"]) > 1 and form.data["Transcriptions"][1]=="Phonemic":
                        form_out = form.data["AlternativeValues"][0]
                # adding a new row to the data frame
                data[idx] = [
                        row,
                        form.language.id, 
                        form.language.name, 
                        form.language.glottocode,
                        form.language.latitude,
                        form.language.longitude,
                        form.language.family,
                        concept,
                        form.id,
                        form.value,
                        form_out
                            ]
                idx += 1
    # error handling
    except FileNotFoundError:
        print(row, "missing")
                    
# importing lexirumah from separate database
                    
pp = Path("/Users/soskuthy/Documents/Research/current/2019/bodo_r/lexibank/lexirumah-data/cldf",
    "cldf-metadata.json")
ds = pycldf.Dataset.from_metadata(pp)
for row in ds.objects("FormTable"):
    if row.cldf.parameterReference in ['rough','smooth']:
        data[idx] = [
                    'lexirumah',
                    row.language.cldf.id, 
                    row.language.cldf.name, 
                    row.language.cldf.glottocode,
                    row.language.cldf.latitude,
                    row.language.cldf.longitude,
                    row.language.data['Family'],
                    row.cldf.parameterReference,
                    row.cldf.id,
                    row.cldf.value,
                    row.cldf.form
                    ]
        idx += 1

# exporting data
# column header
data[0] = ["Dataset", "language.id", "language.name", 
        "glottocode", "latitude", "longitude", "family", "concept",
        "form_id", "value", "form"]

# making sure all data is of class str
data_str = [[str(i) for i in data[k]] for k in sorted(data.keys())]

# writing the data to a csv file
with open('/Users/soskuthy/Documents/Research/current/2019/bodo_r/lexibank/cldf_clics_rough_smooth.csv', 'w') as f:
    # using csv.writer method from CSV package
    write = csv.writer(f)
    write.writerows(data_str)
