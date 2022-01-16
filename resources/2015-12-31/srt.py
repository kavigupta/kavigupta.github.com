#!/usr/bin/python3
import sys
import re
import fileinput

class AutoRepr:
    def __repr__(self):
        return repr(self.__dict__)

class Simple(AutoRepr):
    # Takes something in the form "regex/replacement"
    def __init__(self, sregrepl):
        regex = ""
        for i in range (0, len(sregrepl)):
            if sregrepl[i] != '/':
                regex += sregrepl[i]
                continue
            if i != len(sregrepl) - 1 and sregrepl[i+1] == '/':
                i += 1
                regex += '/'
            self.regex = regex
            self.repl = sregrepl[i+1:]
            break
        try:
            self.regex = re.compile(self.regex)
        except Exception as e:
            print ("error in compiling", self.regex, e)
            raise Exception("error in compiling " + self.regex)
    def apply(self, inp):
        result = self.regex.sub(self.repl, inp)
        if inp != result and debug:
            print(result)
        return result, result != inp

class Compound(AutoRepr):
    def __init__(self, components):
        self.components = components
    def apply(self, inp):
        result = inp
        match = False
        for comp in self.components:
            result, componentMatch = comp.apply(result)
            match = match or componentMatch
        return result, match

class Repeat(AutoRepr):
    def __init__(self, sub):
        self.sub = sub
    def apply(self, inp):
        result = inp
        anymatch = False
        match = True
        while match:
            result, match = self.sub.apply(result)
            anymatch = match or anymatch
        return result, anymatch

def parse(inp):
    inp = [re.sub(r"(^|(?<=\s))#[^\n]+", "", x) for x in inp]
    i = 0
    parsed = []
    while i < len(inp):
        inp[i] = inp[i].strip()
        if inp[i] == '':
            i += 1
        elif inp[i] == 'repeat':
            lines = []
            i += 1;
            while i < len(inp) and inp[i].startswith('\t'):
                lines.append(inp[i][1:])
                i += 1
            parsed.append(Repeat(parse(lines)))
        elif inp[i].startswith('s/'):
            assert inp[i][-1] == '/', "s/regex/repl/ statements must end with a / but " + repr(inp[i]) + " does not"
            parsed.append(Simple(inp[i][2:-1]))
            i+= 1
        else:
            assert False, inp[i] + " is an invalid line"
    return Compound(parsed)

debug = False
if '--debug' in sys.argv:
    sys.argv.remove('--debug')
    debug = True

assert len(sys.argv) == 2, "Format: recregex [--debug] <path to file>"
with open(sys.argv[1], 'r') as myfile:
    inp = myfile.readlines()

inp = list(map(lambda st: st.replace("    ", "\t").replace("\n", ""), inp))

parser = parse(inp)

while True:
    try:
        text = input("--$ ")
        while True:
            try:
                text += "\n" + input("... ")
            except KeyboardInterrupt:
                print ("")
                break
    except KeyboardInterrupt:
        print ("")
        continue
    except EOFError:
        print ("")
        break
    print(parser.apply(text)[0])
