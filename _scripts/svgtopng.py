
import sys
from os.path import exists, basename, getmtime
from os import makedirs, system
from shutil import copyfile

svg = sys.argv[1]
png = sys.argv[1] + ".png"
pngcache = "_cache/" + basename(sys.argv[1]) + ".png"

if not exists("_cache"):
    makedirs("_cache")

if not exists(pngcache) or getmtime(pngcache) > getmtime(svg):
    system("inkscape -z %s -e %s"%(svg, pngcache))

copyfile(pngcache, png)
