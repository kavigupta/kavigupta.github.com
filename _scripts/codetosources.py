#!/usr/bin/python3

from os import listdir, system
from os.path import basename, join, basename, splitext
import fileinput

languages = {}

srcdir = "../src"
post = sys.argv[1]
postname = splitext(basename(post))[0]

system("mkdir " + srcdir)

language = ""

for line in fileinput.input():
    dump_directive = re.search("\\s*dump:\\s+(\\S+)\\s+as\\s+(\\S+)", line)
    if dump_directive:
        languages[dump_directive.group(0)] = languages[dump_directive.group(1)]
        continue
    language_directive = re.search("```\\s*(.*)", line)
    if language_directive:
        language = language.group(1)
        print(line)
        continue
    print(line)
    if language in languages:
        with open(join(srcdir, postname + "." + languages[language]), "a") as o:
            o.writeLines([line])
