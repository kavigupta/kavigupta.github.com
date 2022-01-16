#!/usr/bin/env python3

import sys
import requests

DIRECTIVE = "download_include:"
for line in sys.stdin:
    if line.startswith(DIRECTIVE):
        line = line[len(DIRECTIVE) :].strip()
        print(requests.get(line).content.decode("utf-8"))
    else:
        print(line)
