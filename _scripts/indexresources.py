#!/usr/bin/python3

import os
import os.path
import sys

if len(sys.argv) == 1:
    clean = False
elif len(sys.argv) == 2 and sys.argv[1] == 'clean':
    clean = True
else:
    raise Exception("Usage: indexresources.py [clean]")

def header(path):
    return """---
layout: default
title: "Directory listing for {path}"
---""".format(**locals())

def indexPage(direct, files):
    page = header(direct)
    page += """
            <div class="page">
                <table id = "dirlist">
            """
    for f in files:
        qualifiedpath = os.path.join(direct, f)
        webpath = os.path.join("/", direct, f)
        isdir = os.path.isdir(qualifiedpath)
        img = "folder" if isdir else "file"
        page += """
                    <tr>
                        <td>
                            <a href={webpath}>
                                <img src="/resources/hidden/{img}.png" alt = "{img}">
                            </a>
                        </td>
                        <td>
                            {f}
                        </td>
                    </tr>
                """.format(**locals())
    page += "\t</table>\n</div>"
    with open(os.path.join(direct, "index.html"), "w") as index:
        index.write(page)

def cleanPage(direct):
    print("rmfrom", direct)
    if os.path.isfile(os.path.join(direct, "index.html")):
        os.remove(os.path.join(direct, "index.html"))

def indexResources(direct):
    print(direct)
    files = os.listdir(direct)
    list.sort(files)
    files = list(filter(lambda f: f != "index.html" and f != "hidden", files))
    if clean:
        cleanPage(direct)
    else:
        indexPage(direct, files)
    files = list(map (lambda f: os.path.join(direct, f), files))
    print(files)
    for f in filter(os.path.isdir, files):
        print ("File", f)
        indexResources(f)

indexResources("resources")
