import numpy as np
import matplotlib.pyplot as plt
import sys
sys.path.insert(0, "../gerrymandering_data/")
from gerrymandering import *

district_map("img/red_state.png",
             [range(10), range(8), dict(color="red")],
             [range(10), range(8, 10), dict(color="blue")])
district_map("img/blue_state.png",
             [range(4), range(0, 10, 2), dict(color="red", alpha=0.25)],
             [range(4), range(1, 10, 2), dict(color="red")],
             [range(4, 10), range(6, 10), dict(color="red")],
             [range(4, 10), range(6), dict(color="blue")])

plt.figure(figsize=(20, 10))
for v, vfn in enumerate((efficiency_gap, efficiency_percentage_gap)):
    plt.subplot(121 + v)
    scatterplot(2016, democratic_totals, vfn, ms=[], y_axes=[0, 100],
                draw_linear_regressor=True, new_figure=False, include_colormap=False)
plt.savefig("img/party_affil_vs_effics.png")

scatterplot(2016, efficiency_gap, efficiency_percentage_gap, "img/effic_vs_effic_perc.png")
scatterplot(2016, efficiency_percentage_gap, effectiveness_gap, "img/effic_perc_vs_effect")
scatterplot(2016, weighted_effectiveness_gap, effectiveness_gap, "img/effect_vs_weighted_effect.png")

plt.figure(figsize=(10, 10))
nvotes = np.arange(0, 1001)
wasted = (nvotes > 500) * (nvotes - 500) + (nvotes <= 500) * nvotes
plt.plot(nvotes, wasted, color="black")
plt.xlabel("Number of votes for Party X")
plt.ylabel("Number of wasted votes for Party X")
for a, b, c in [(500, 500), (0, 500), "cracking"], [(1000, 500), (500, 200), "packing"]:
    plt.annotate('optimal %s against party X' % c,
                xy=a, xytext=b, arrowprops=dict(facecolor='black', shrink=0.05),
                size=12)
plt.grid()
plt.title("Wasted Votes in a 2 Party election with 1000 voters")
increase_size(plt.gca(), 12)
plt.savefig("img/wasted_votes.png")
