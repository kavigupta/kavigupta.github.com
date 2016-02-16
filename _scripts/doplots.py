#!/usr/bin/python3

from sys import stdin

try:
    import numpy as np
    from matplotlib.pyplot import *
    from sympy.solvers import solve
    from sympy import Symbol
    from ntpath import basename
    import os

    from re import compile
    with open(".spp-current-file", "r") as f:
        directory = basename(f.read())[:10]
        if not os.path.exists("../resources/" + directory):
            os.makedirs("../resources/" + directory)

    inplot = False
    lines = []
    path = ""

    start = compile(r"\{dump plot:[ ]*([^ ]*)[ ]*\}")
    end = "{end plot}"

    def writeplot():
        axis('equal')
        xlim(-1, 1)
        ylim(-1, 1)

        def fnplots(*fnc):
            for f, c in fnc:
                xmin, xmax = gca().get_xlim()
                ymin, ymax = gca().get_ylim()
                x = np.arange(xmin, xmax, (xmax-xmin)/100)
                y = np.vectorize(f)(x)
                plot(x, y, c)
        code = '\n'.join(lines) + "\n"
        exec(code)
        savefig("../resources/" + path)

        clf()
        cla()

    for line in stdin:
        line = line[:-1]
        if inplot:
            if line == end:
                writeplot()
                lines = []
                inplot = False
                print('<img src="/resources/' + path + '"/>')
            else:
                lines.append(line)
            continue
        m = start.match(line)
        if m:
            inplot = True
            path = directory + "/" + m.group(1)
        else:
            print(line)
except Exception as e:
    import traceback
    print(traceback.format_exc())
    
for line in stdin:
    pass
