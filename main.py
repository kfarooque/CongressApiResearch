#!/usr/bin/env python
# -*- utf-8 -*-

import config as c
import functions as f

# SEND REQUEST #

api_key = f.extract_value_from_file(c.infile_keys, "api_key")
api_query = f.format_congress_api_query(c.query)
api_url = f.define_congress_api_url(c.endpoint, api_query)
api_results = f.download_congress_api_results(api_key, api_url, max_results=c.max_results)

# SAVE RESULTS #

out_path = f.define_api_output_path(c.outpath_data, c.endpoint, api_query, create_path=True)
f.output_api_data_to_json(api_results[2], out_path + "results.json")
f.output_api_data_to_txt(api_results[2], out_path + "results.txt", sep="\t")
print("Output data sent to: " + out_path)
print("-" * 64)

# PRINT STATUS #

out_message = f.define_api_result_message(api_results, api_url)
with open(out_path + "message.txt", "w") as file:
    file.write("\n".join(out_message))
print("\n".join(out_message))
print("-" * 64)
