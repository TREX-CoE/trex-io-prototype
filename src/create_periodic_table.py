#!/usr/bin/env python

import periodictable
import json
elements = {}
elements["name"] = {}
elements["symbol"] = {}
elements["number"] = {}

for el in periodictable.elements:
    if el.symbol != "n":
      elements["symbol"][el.number] = el.symbol
      elements["name"][el.number] = el.name
      elements["number"][el.symbol] = el.number

j = json.dumps(elements)
print(j)

