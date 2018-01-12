import numpy as np
from collections import defaultdict
import csv
import us
import matplotlib.colors as mcolors

def add_identity(axes, m=1, b=0, *line_args, **line_kwargs):
    identity, = axes.plot([], [], *line_args, **line_kwargs)
    def callback(axes):
        low_x, high_x = axes.get_xlim()
        low_y, high_y = axes.get_ylim()
        low = max(low_x, (low_y - b) / m)
        high = min(high_x, (high_y - b) / m)
        identity.set_data([low, high], [m * low + b, m * high + b])
    callback(axes)
    axes.callbacks.connect('xlim_changed', callback)
    axes.callbacks.connect('ylim_changed', callback)
    return axes

def increase_size(ax, font_size):
    for item in ([ax.title, ax.xaxis.label, ax.yaxis.label] + ax.get_xticklabels() + ax.get_yticklabels()):
        item.set_fontsize(font_size)

def make_colormap(seq):
    return mcolors.LinearSegmentedColormap.from_list("politics", ((1, 0, 0), (0, 0, 1)))

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

def efficiency_gap(state):
    wd, wr = wasted_votes(state)
    return (wd - wr) / sum(total_votes(state))

def efficiency_percentage_gap(state):
    wd, wr = wasted_votes(state)
    d, r = total_votes(state)
    if d == 0 or r == 0:
        return np.nan
    return (wd/d - wr/r) / 2

def effectiveness_gap(state):
    d_total, r_total = total_votes(state)
    d_wins = np.mean([d > r for d, r in state.values()])
    return d_total / (d_total + r_total) - d_wins

def weighted_effectiveness_gap(state):
    percs = np.array([d / (d + r) for d, r in state.values()])
    weighted_d_votes = np.mean(percs)
    d_wins = np.mean(percs >= 0.5)
    return weighted_d_votes - d_wins

def number_votes(state):
    d_total, r_total = total_votes(state)
    return d_total + r_total

def democratic_totals(state):
    d_total, r_total = total_votes(state)
    return d_total / (d_total + r_total)

def load_data():
    with open("voting.csv") as votes_file:
        lines = list(csv.reader(votes_file))
    dr_votes = defaultdict(lambda: defaultdict(list))
    for line in lines[1:]:
        _, candidate, _, vote_total, district, state = line
        if candidate[0] in "RD":
            dr_votes[state][district].append((candidate[0], vote_total.replace(",", "")))
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
                for _, state in sorted(DR_VOTES.items())])

DR_VOTES = load_data()
STATE_ABBR = list(state_abbreviations(DR_VOTES))
