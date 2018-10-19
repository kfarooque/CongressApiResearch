#!/usr/bin/env python
# -*- utf-8 -*-

import sys
import ast

# INPUTS/OUTPUTS #

infile_keys = ".keys"
outpath_data = "data/"

# PARAMETERS - GET API RESULTS #

max_results = 100
endpoint = "bills/search.json"
query = ["tax shelter", "loophole"]

# PARAMETERS - GET TEXT BILLS #

bills_path = outpath_data + "bills_search"
bills_overwrite = False

# PASS SYSTEM ARGUMENTS #

if sys.argv[0].endswith('get_api_results.py'):
    if len(sys.argv) >= 2:
        max_results = int(sys.argv[1])
    if len(sys.argv) >= 3:
        endpoint = sys.argv[2]
        if len(sys.argv) == 3:
            query = None
        elif len(sys.argv) == 4:
            query = sys.argv[3]
            if query[0] == '[' and query[len(query)-1] == ']':
                query = ast.literal_eval(query)
        else:
            query = sys.argv[3:]
        if type(query) == str:
            query = [query]
if sys.argv[0].endswith('get_text_bills.py'):
    if len(sys.argv) >= 2:
        bills_path = sys.argv[1]
    if len(sys.argv) >= 3:
        bills_overwrite = ['overwrite' in argument.lower() for argument in sys.argv[2:]]
