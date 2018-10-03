
from itertools import count
from math import gcd, log, sin, cos, pi
import matplotlib.pyplot as plt
from matplotlib.colors import hsv_to_rgb

class ModularGraph:
    def __init__(self, p):
        self.fracs = [None] * p
        self.count = len([i for i in range(p) if gcd(i, p) != 1])
    def add(self, idx, rat):
        if self.fracs[idx] is None:
            self.fracs[idx] = [rat]
            self.count += 1
        else:
            self.fracs[idx] += [rat]
    def done(self):
        return self.count == len(self.fracs)
    @staticmethod
    def create(p):
        graph = ModularGraph(p)
        for total in count():
            for a in range(1, total):
                b = total - a
                if a % p == 0 or b % p == 0:
                    continue
                if gcd(a, b) != 1:
                    continue
                if gcd(b, p) != 1:
                    continue
                mod_val = a * mod_inv(b, p) % p
                if gcd(mod_val, p) != 1:
                    continue
                graph.add(mod_val, (a, b))
                graph.add((-mod_val) % p, (-a, b))
            if graph.done():
                break
        return graph

def mod_inv(x, p):
    # From wikibooks: https://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
    # return (g, x, y) a*x + b*y = gcd(x, y)
    def egcd(a, b):
        if a == 0:
            return (b, 0, 1)
        else:
            g, x, y = egcd(b % a, a)
            return (g, y - (b // a) * x, x)

    # x = mulinv(b) mod n, (x * b) % n == 1
    def mulinv(b, n):
        g, x, _ = egcd(b, n)
        if g == 1:
            return x % n
    return mulinv(x, p)

def text(a, b):
    return "$%s\\dfrac{%s}{%s}$" % ("+-"[a < 0], abs(a), b)
def color(a, _):
    return "rb"[a < 0]
def yval(a, b):
    return log(abs(a/b))
def size(a, b):
    return 60 / (abs(a) + b)

def line_color(i, p):
    if gcd(i, p) == 1:
        return hsv_to_rgb([i/(p) * 4/5, 1, 1])
    return [0.75] * 3

def is_prime(n):
    for p in range(2, n):
        if n % p == 0:
            return False
    return True

def circular_plot(p, path):
    fs = ModularGraph.create(p)

    plt.figure(figsize=(20, 20))

    circ_radius = 2 * max(abs(yval(a, b)) for f in [x for x in fs.fracs if x is not None] for a, b in f)
    circ_radius = max(circ_radius, 1)

    for i in range(p):
        th = -i / p * 2 * pi + pi/2
        plt.plot([0, circ_radius * 10 * cos(th)], [0, circ_radius * 10 * sin(th)],
                 alpha=0.3,
                 color=line_color(i, p),
                 lw=100/p)
        if fs.fracs[i] is None:
            continue
        for a, b in fs.fracs[i]:
            r = circ_radius + yval(a, b)
            x, y = r * cos(th), r * sin(th)
            plt.text(x, y, text(a, b),
                     color=color(a, b),
                     ha='center',
                     va='center',
                     rotation=th * 180/pi - 90,
                     fontdict={'size' : size(a, b), 'family' : 'serif'})

    circ = plt.Circle((0, 0), circ_radius, fill=False, alpha=0.25)
    plt.gca().add_artist(circ)
    plt.xlim((-circ_radius*2, circ_radius*2))
    plt.ylim((-circ_radius*2, circ_radius*2))
    plt.axis('off')
    plt.savefig(path, bbox_inches='tight', dpi=100)
    plt.close("all")

for p in range(2, 1000):
    if not is_prime(p):
        continue
    print(p)
    circular_plot(p, "prime/%02d.png" % p)
    circular_plot(2 * p, "dprime/%02d.png" % (2 * p))
