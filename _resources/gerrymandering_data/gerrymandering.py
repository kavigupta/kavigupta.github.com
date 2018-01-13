import csv
from collections import defaultdict
import numpy as np
import us
import matplotlib.colors as mcolors
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import matplotlib
from scipy.stats import linregress
from os import mkdir
import os

matplotlib.rc('font', family='serif')
matplotlib.rc('figure', dpi=200)

def named(name):
    def modify_fn(func):
        func.name = name
        return func
    return modify_fn

def draw_district(*args):
    def draw_rectangle(x, y, w, h, **kwargs):
        plt.gca().add_patch(patches.Rectangle((x, y), w, h, **kwargs))
    draw_rectangle(*args, fill=True, color="black", alpha=0.05)
    draw_rectangle(*args, fill=False)

def add_identity(axes, m=1, b=0, *line_args, **line_kwargs):
    identity, = axes.plot([], [], *line_args, **line_kwargs)
    low_x, high_x = axes.get_xlim()
    low_y, high_y = axes.get_ylim()
    low = max(low_x, (low_y - b) / m)
    high = min(high_x, (high_y - b) / m)
    identity.set_data([low, high], [m * low + b, m * high + b])
    return axes

def dotgrid(xrange, yrange, **kwargs):
    yrange = list(yrange)
    xs, ys = [], []
    for x in xrange:
        for y in yrange:
            xs.append(x)
            ys.append(y)
    plt.scatter(xs, ys, **kwargs)

def district_map(path, *segments):
    plt.figure(figsize=(10, 10))
    for idx in range(10):
        draw_district(idx - 0.4, -0.5, 0.8, 10)
    for xrange, yrange, params in segments:
        dotgrid(xrange, yrange, **params, s=100)
    plt.axis('off')
    plt.xlim(-1, 10)
    plt.ylim(-1, 10)
    plt.savefig(path, bbox_inches='tight')

def increase_size(ax, font_size):
    for item in ([ax.title, ax.xaxis.label, ax.yaxis.label] + ax.get_xticklabels() + ax.get_yticklabels()):
        item.set_fontsize(font_size)

POLITICS_COLORBAR = mcolors.LinearSegmentedColormap.from_list("politics", ((1, 0, 0), (0, 0, 1)))

def process_state(mapping):
    for k in mapping:
        mapping[k] = sorted(mapping[k])
    totals = []
    for k in mapping:
        if len(mapping[k]) == 1 and mapping[k][0][1] == 'NA':
            continue
        totals += [sum(int(v) for _, v in mapping[k])]
    average_per_district = np.mean(totals)
    new_mapping = {}
    for k in mapping:
        dems_reps = {"D" : 0., "R" : 0.}
        for party, total in mapping[k]:
            if total != "NA":
                dems_reps[party] += float(total)
        if sum(dems_reps.values()) == 0:
            assert [x for _, x in mapping[k]] == ["NA"]
            dems_reps[mapping[k][0][0]] = average_per_district
        new_mapping[k] = [dems_reps["D"], dems_reps["R"]]
    return new_mapping

def total_votes(state):
    return tuple(np.sum(list(state.values()), axis=0))

def wasted_votes(state):
    wasted_d, wasted_r = 0, 0
    for d, r in state.values():
        if d > r:
            wasted_d += d - (d + r) / 2
            wasted_r += r
        else:
            wasted_d += d
            wasted_r += r - (d + r) / 2
    return wasted_d, wasted_r

@named("Efficiency Gap")
def efficiency_gap(state):
    wd, wr = wasted_votes(state)
    return (wd - wr) / sum(total_votes(state))

@named("Efficiency Percentage Gap")
def efficiency_percentage_gap(state):
    wd, wr = wasted_votes(state)
    d, r = total_votes(state)
    if d == 0 or r == 0:
        return np.nan
    return (wd/d - wr/r) / 2

@named("Effectiveness Gap")
def effectiveness_gap(state):
    d_total, r_total = total_votes(state)
    d_wins = np.mean([d > r for d, r in state.values()])
    return d_total / (d_total + r_total) - d_wins

@named("Weighted Effectiveness Gap")
def weighted_effectiveness_gap(state):
    percs = np.array([d / (d + r) for d, r in state.values()])
    weighted_d_votes = np.mean(percs)
    d_wins = np.mean(percs >= 0.5)
    return weighted_d_votes - d_wins

def number_votes(state):
    d_total, r_total = total_votes(state)
    return d_total + r_total

@named("Democratic Vote Share")
def democratic_totals(state):
    d_total, r_total = total_votes(state)
    return d_total / (d_total + r_total)

def load_2016_data():
    with open(os.path.dirname(os.path.realpath(__file__)) + "/2016.csv") as votes_file:
        lines = list(csv.reader(votes_file))
    dr_votes = defaultdict(lambda: defaultdict(list))
    for line in lines[1:]:
        _, candidate, _, vote_total, district, state = line
        if candidate[0] in "RD":
            dr_votes[state][district].append((candidate[0], vote_total.replace(",", "")))
    return dr_votes

def load_pre2016_data(date):
    with open(os.path.dirname(os.path.realpath(__file__)) + "/congressional-election-results/data/results_%s.csv" % date) as csv_file:
        lines = np.array(list(csv.reader(csv_file)))
    votes_float = defaultdict(lambda: defaultdict(list))
    for id, year, name, state, district, votes, parties in lines:
        if district == "S":
            continue
        if votes == "Unopposed":
            votes = "NA"
        if "Democrat" in parties and "Republican" in parties:
            for party_abbr in "DR":
                votes_float[state][district].append((party_abbr, str(int(votes) // 2)))
        elif "Democrat" in parties:
            votes_float[state][district].append(("D", votes))
        elif "Republican" in parties:
            votes_float[state][district].append(("R", votes))
    return votes_float

def load_data():
    by_year = {year : fill_in_unopposed(load_pre2016_data(year)) for year in range(2004, 2015, 2)}
    by_year[2016] = fill_in_unopposed(load_2016_data())
    return by_year

def fill_in_unopposed(dr_votes):
    dr_votes_float = {"US" : {}}
    for state, mapping in dr_votes.items():
        dr_votes_float[state] = process_state(mapping)
        for k, v in dr_votes_float[state].items():
            dr_votes_float["US"]["%s-%s" % (state, k)] = v
    return dr_votes_float

def state_abbreviations(state_names):
    for txt in sorted(state_names):
        txt = txt.replace("-", " ")
        if txt == "US":
            yield "US"
        else:
            state = us.states.lookup(txt)
            assert state != us.states.lookup("")
            yield state.abbr

def get_columns(*fns):
    return np.array([[fn(state) for fn in fns]
                for _, state in sorted(DR_VOTES[2016].items())])

def scatterplot(xfn, yfn, path=None, ms=[1], y_axes=[0],
                draw_linear_regressor=False, new_figure=True, include_colormap=True):
    if new_figure:
        plt.figure(figsize=(20, 20))
    xs = get_columns(xfn)[:,0] * 100
    ys = get_columns(yfn)[:,0] * 100
    nonnan = list(np.where(1 - np.isnan(xs + ys))[0])
    xs, ys = xs[nonnan], ys[nonnan]
    totals = get_columns(number_votes)
    democratic_perc = get_columns(democratic_totals)[:,0]
    plt.scatter(xs, ys, s=totals / np.mean(totals) * 500,
                c=democratic_perc[nonnan] * 100, cmap=POLITICS_COLORBAR, alpha=0.75, vmin=25, vmax=75)
    if include_colormap:
        cbar = plt.colorbar(aspect=50)
        cbar.set_label('Democratic Vote Percentage', size=18)
        cbar.ax.tick_params(labelsize=18)
    plt.grid()
    for y_val in y_axes:
        plt.axvline(y_val, linestyle="-.", color="black")
    plt.axhline(0, linestyle="-.", color="black")
    plt.xlabel(xfn.name + " [%]")
    plt.ylabel(yfn.name + " [%]")
    for i, txt in enumerate(np.array(STATE_ABBR)[nonnan]):
        if not np.isnan(xs[i] + ys[i]):
            plt.annotate(txt, (xs[i],ys[i]))
    for m in ms:
        add_identity(plt.gca(), color="black", ls="-.", m=m, label="y=%sx" % ("" if m == 1 else str(m)))
    increase_size(plt.gca(), 24)
    if draw_linear_regressor:

        result = linregress(xs, ys)
        add_identity(plt.gca(),
                     m=result.slope, b=result.intercept,
                     color="black",
                     label="y=%.2fx %s %.2f [r=%.4f]" % (
                         result.slope,
                         "-+"[result.intercept >= 0],
                         abs(result.intercept),
                         result.rvalue))
    plt.legend(fontsize=24)
    plt.axis('equal')
    try:
        mkdir("img")
    except FileExistsError:
        pass
    if path is not None:
        plt.savefig(path)

DR_VOTES = load_data()
STATE_ABBR = list(state_abbreviations(DR_VOTES[2016]))
