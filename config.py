#!/usr/bin/env python
# -*- utf-8 -*-

import sys
import ast
import re

# INPUTS/OUTPUTS #

infile_keys = ".keys"
outpath_data = "data/"

# PARAMETERS - GET API RESULTS #

max_results = 100
endpoint = "bills/search.json"
query = ["tax shelter", "loophole"]

# PARAMETERS - GET TEXT BILLS #

bills_overwrite = False

# PASS SYSTEM ARGUMENTS #

if re.search('(^|\\/|\\\)(get_api_results)\\.py$', sys.argv[0]):
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
            query = sys.argv[3:len(sys.argv)]
        if type(query) == str:
            query = [query]
