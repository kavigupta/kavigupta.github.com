#!/usr/bin/python3

try:
    from os import listdir, system
    import sys
    import re
    from os.path import basename, join, basename, splitext
    import fileinput
    import traceback

    languages = {}
    outputs = {}

    def get_handle(language):
        a = ""
        if language in outputs:
            a = "a"
        else:
            a = "w"
            outputs[language] = True
        return open(join(srcdir, postname + "." + languages[language]), a)

    def write_to_language(language, line):
        if language in languages:
            with get_handle(language) as o:
                o.write(line)

    def output(line):
        print(line[0:-1])

    srcdir = "../src"
    post = sys.argv[1]
    postname = splitext(basename(post))[0]

    system("mkdir " + srcdir)

    language = ""
    for line in sys.stdin:
        dump_directive = re.search("dump:\\s+(\\S+)\\s+as\\s+(\\S+)", line)
        if dump_directive:
            languages[dump_directive.group(1)] = dump_directive.group(2)
            continue
        language_directive = re.search("```\\s*(.*)", line)
        if language_directive:
            write_to_language(language, "\n")
            language = language_directive.group(1)
            output(line)
            continue
        output(line)
        write_to_language(language, line)
except Exception:
    exc_type, exc_value, exc_traceback = sys.exc_info()
    lines = traceback.format_exception(exc_type, exc_value, exc_traceback)
    print (''.join('!! ' + line for line in lines))  # Log it or whatever here

for line in sys.stdin:
    pass
