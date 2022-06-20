from functools import lru_cache
import pandas as pd
import numpy as np

import geopandas as gpd

from permacache import permacache
import tqdm

from downloader import refine_location


def get_precincts():
    return pd.read_csv("/home/kavi/full-usa-precinct-map/out/centroids.csv")


@lru_cache(None)
def get_shapefile_cached():
    return gpd.read_file("/home/kavi/full-usa-precinct-map/out/result.shp")


@permacache("voter-sampler/data/select_precincts")
def select_precincts(sample_column, num_samples=1000):
    precincts = get_shapefile_cached()
    weights = np.array(precincts[sample_column])
    weights[precincts.state == "KY"] = 0
    weights = weights / weights.sum()
    samples = np.random.RandomState(ord(sample_column)).choice(
        len(weights), p=weights, replace=False, size=int(num_samples)
    )
    selected_precincts = precincts.iloc[samples].copy()
    selected_precincts["selected_party"] = [sample_column] * len(selected_precincts)
    return selected_precincts.copy()


def select_precincts_with_count(sample_column, num_samples):
    num_samples = int(num_samples)
    x = select_precincts(sample_column)
    assert len(x) >= num_samples
    return x[:num_samples]


def sample_voters_with_precincts(total_samples):
    precincts = get_precincts()
    overall = precincts[["R", "D"]].sum()
    samples_each = (total_samples / overall.sum() * overall).round()
    to_interleave = [
        select_precincts_with_count(k, samples_each[k])
        for k in sorted(samples_each.index)
    ]
    rows = []
    for i in range(max(x.shape[0] for x in to_interleave)):
        for ti in to_interleave:
            if i < ti.shape[0]:
                rows.append(ti.iloc[i])
    return rows


def load_data(total_samples):
    data = pd.DataFrame(sample_voters_with_precincts(total_samples))
    results = []
    errors = []
    for p in tqdm.tqdm(data.geometry):
        try:
            results.append(refine_location(p))
        except RuntimeError as e:
            print(f"ERROR: {e}")
            errors.append(str(e))
    if errors:
        print(errors)
        raise RuntimeError()
    data["x"], data["y"] = np.array(results).T
    data["ox"], data["oy"] = data["x"], data["y"]
    return data
