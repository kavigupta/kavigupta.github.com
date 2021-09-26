
import sys
from os.path import exists, isfile, basename, getmtime, abspath
from os import makedirs, system, getcwd
from shutil import copyfile

import hashlib

svg = sys.argv[1]
png = sys.argv[1] + ".png"

if not exists("_cache"):
    makedirs("_cache")

md5 = hashlib.md5()

with open(svg, "rb") as f:
    md5.update(f.read())

pngcache = "_cache/%s.png" % md5.hexdigest()

if not isfile(pngcache):
    command = "inkscape -z %s -o %s"%(svg, pngcache)
    print(command)
    assert not system(command)

copyfile(pngcache, png)
