#!/usr/bin/python3

from os import listdir, system
from os.path import basename

languages = {"haskell": "hs"}

postsdir = "../_posts"
srcdir = "../src"

system("mkdir " + srcdir)

posts = listdir(postsdir)

for unqual in posts:
    with open(postsdir + "/" + unqual) as p:
        used_languages = {}
        language = ''
        for line in p:
            if line[0:3] == '```':
                language = line[3:].strip().lower()
                for l in used_languages:
                    used_languages[l].append("\n")
                continue
            if language not in used_languages:
                used_languages[language] = []
            used_languages[language].append(line)
    maximum_count = 0
    maximum_language = None
    print (used_languages)
    for language in list(used_languages.keys()):
        lines = used_languages[language]
        if language in languages:
            if len(lines) > maximum_count:
                maximum_count = len(lines)
                maximum_language = language
    print (maximum_language)
    if maximum_language is not None:
        lines = used_languages[maximum_language]
        print (lines)
        dot = unqual.rfind(".")
        if dot > 0:
            unqual = unqual[:dot]
        out = srcdir + "/" + unqual + "." + languages[maximum_language]
        with open(out, "w") as o:
            o.writelines(lines)
