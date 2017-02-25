
import sys
from os.path import exists, basename, getmtime
from os import makedirs, system
from shutil import copyfile
import hashlib

tex = sys.argv[1]
jobname = tex[:-len(".tex")]
png = jobname + ".png"
basejob = basename(jobname)
bpdf = basejob + ".pdf"
blog = basejob + ".log"
baux = basejob + ".aux"

md5 = hashlib.md5()

with open(tex, "rb") as f:
    md5.update(f.read())

pngcache = "_cache/%s.png" % md5.hexdigest()

if not exists(pngcache):
    system("pdflatex " + tex)
    system("convert -density 300 {bpdf} -quality 90 {png}".format(**locals()))
    copyfile(png, pngcache)
    system("rm {bpdf} {baux} {blog}".format(**locals()))
else:
    copyfile(pngcache, png)
