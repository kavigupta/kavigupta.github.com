
import matplotlib.pyplot as plt
from matplotlib.image import imread
from set_fig_size import *

for f in "direct", "tight-layout", "update-size", "bad":
    exec(open("%s.py" % f).read())
