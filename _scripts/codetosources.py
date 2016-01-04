#!/usr/bin/python3

from os import listdir, system, makedirs
import sys
import re
from os.path import join, basename, splitext, exists, dirname
import fileinput
import traceback

class Directive:
    def __init__(self, language, extension, location):
        self.language = language
        self.extension = extension
        self.location = location
        self.block_number = 0

    def process(to_process):
        dump_directive = re.search("dump:\\s+(\\S+)\\s+as\\s+(\\S+)(?:\\s+to\\s+(\\S+))?", line)
        if not dump_directive:
            return None
        return Directive(dump_directive.group(1), dump_directive.group(2), dump_directive.group(3))

    def get_location(self):
        repls = {"block_number" : self.block_number}
        if self.location is None:
            return join(srcdir, postname + "." + self.extension)
        else:
            return self.location.format(**repls)
    def terminate_block(self):
        self.block_number += 1


class DirectiveList:
    def __init__(self):
        self.directives = {}
    def process(self, to_process):
        direct = Directive.process(to_process)
        if direct is None:
            return False
        self.directives[direct.language] = direct
        return True
    def get_location(self, language):
        if language not in self.directives:
            return False
        return self.directives[language].get_location()
    def terminate_block(self):
        if language not in self.directives:
            return
        self.directives[language].terminate_block()

class LanguageList:
    def __init__(self):
        self.dumps = {}
    def add_dump(self, language, location, text):
        if language not in self.dumps:
            self.dumps[language] = {}
        if location not in self.dumps[language]:
            self.dumps[language][location] = ""
        self.dumps[language][location] += text
    def terminate_block(self, language):
        self.write_to_language(language, "\n")
        directives.terminate_block()
    def write_to_language(self, language, text):
        loc = directives.get_location(language)
        if not loc:
            return False
        self.add_dump(language, loc, text)
        return True
    def write_all(self):
        for _, lingual in self.dumps.items():
            for location, text in lingual.items():
                create_if_exists(dirname(location))
                with open(location, "w") as f:
                    f.write(text)

srcdir = "../src"
post = sys.argv[1]
postname = splitext(basename(post))[0]

def create_if_exists(path):
    if not exists(path):
        makedirs(path)

directives = DirectiveList()
languages = LanguageList()

language = ""
for line in sys.stdin:
    if directives.process(line):
        continue
    language_directive = re.search("```\\s*(.*)", line)
    if language_directive:
        languages.terminate_block(language)
        language = language_directive.group(1)
        print(line, end = "")
        continue
    print(line, end = "")
    languages.write_to_language(language, line)

languages.write_all()

for line in sys.stdin:
    pass
