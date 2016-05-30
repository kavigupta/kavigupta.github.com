
import sys
from os.path import exists, basename, getmtime
from os import makedirs, system
from shutil import copyfile

tex = sys.argv[1]
jobname = tex[:-len(".tex")]
png = jobname + ".png"
basejob = basename(jobname)
bpdf = basejob + ".pdf"
blog = basejob + ".log"
baux = basejob + ".aux"

pngcache = "_cache/" + basename(png)

if not exists("_cache"):
    makedirs("_cache")

print(getmtime(tex))

if not exists(pngcache) or getmtime(pngcache) < getmtime(tex):
    system("pdflatex " + tex)
    system("convert -density 300 {bpdf} -quality 90 {png}".format(**locals()))
    copyfile(png, pngcache)
    system("rm {bpdf} {baux} {blog}".format(**locals()))

print("ABC " + tex)