#!/usr/bin/env python
# -*- utf-8 -*-

import config as c
import functions as f
import os
import re
import pandas as pd

# PRINT PARAMETERS #

print("Parameters:")
print("bills_path = " + c.bills_path)
print("existing bills.txt files will " + ("" if c.bills_overwrite else "not ") + "be replaced")
print("-" * 64)

# LOAD FILES #

files_results = f.search_directory_files(c.bills_path, r'results\.txt$')
df_results = f.import_multiple_files(files_results)
df_results['bill_url'] = [url + '/text' for url in df_results['govtrack_url']]
df_results = df_results[['sourcefile', 'bill_id', 'bill_url']]
df_results = df_results.drop_duplicates(['sourcefile', 'bill_id'])

# DOWNLOAD FILES #

for sourcefile in df_results['sourcefile'].unique():
    outfile = re.sub(r'results\.txt$', 'bills.txt', sourcefile)
    if not os.path.isfile(outfile) or c.bills_overwrite:
        df_bills = df_results.loc[df_results['sourcefile'] == sourcefile, ['bill_id', 'bill_url']]
        df_bills['bill_text'] = '[ERROR] Text not loaded from URL.'
        for index, row in df_bills.iterrows():
            df_bills.loc[index, 'bill_text'] = f.download_from_url_govtrack_bill_text(row['bill_url'])
        with open(outfile, "w", encoding='utf8') as file:
            df_bills.to_csv(file, sep='\t', header=True, index=False, encoding="utf-8")
    elif os.path.isfile(outfile):
        with open(outfile, "r", encoding='utf8') as file:
            df_bills = pd.read_csv(file, sep='\t', encoding="utf-8")
        for index, row in df_bills.iterrows():
            if str(row['bill_text']).startswith('[ERROR]'):
                df_bills.loc[index, 'bill_text'] = f.download_from_url_govtrack_bill_text(row['bill_url'])
        with open(outfile, "w", encoding='utf8') as file:
            df_bills.to_csv(file, sep='\t', header=True, index=False, encoding="utf-8")
