import io
import requests

from permacache import permacache

import numpy as np
from geopy.geocoders import Nominatim
from shapely.geometry import Point
from PIL import Image


def api_key():
    with open("/home/kavi/.google_maps_api_key.txt") as f:
        return f.read().strip()


@permacache("voter-sampler/downloader/download_streetview_2")
def download_streetview_with_radius(x, y, radius=250, fov=120):
    key = api_key()
    url = (
        "https://maps.googleapis.com/maps/api/streetview?"
        + "size=2000x2000&"
        + f"location={y:.6f},{x:.6f}&"
        + f"radius={radius}&pitch=0&key={key}&fov={fov}"
    )
    print(url)
    response = requests.get(url)
    return response.content


def download_streetview(x, y):
    radius = 100
    while True:
        content = download_streetview_with_radius(x, y, radius)
        image = Image.open(io.BytesIO(content))
        beigeness = (np.array(image) == [228, 227, 223]).all(-1).mean()
        if beigeness < 0.95:
            return image
        radius *= 2


@permacache("voter-sampler/downloader/reverse_geocode_2")
def reverse_geocode(x, y):
    print("reverse geocode", y, x)
    locator = Nominatim(user_agent="kavig")
    result = locator.reverse(f"{y}, {x}")
    print(result)
    return result


road_types = {
    "Avenue": 1,
    "Ave": 1,
    "Street": 1,
    "St": 1,
    "Road": 1,
    "Crossing": 1,
    "Lane": 1,
    "Ln": 1,
    "Circle": 1,
    "Terrace": 1,
    "Trail": 1,
    "Way": 1,
    "Run": 1,
    "Place": 1,
    "Park": 1,
    "Ridge": 1,
    "Creek": 1,
    "Bend": 1,
    "Walk": 1,
    "Overlook": 1,
    "Path": 1,
    "Loop": 1,
    "Alley": 1,
    "Pike": 1,
    "Track": 1,
    "Drive": 0,
    "Dr": 0,
    "Court": 0,
    "Ct": 0,
    "Center": 0,
    "Point": 0,
    "Square": 0,
    "Broadway": 0,
    "Boulevard": 0,
    "Blvd": 0,
    "Tunnel": 0,
    "Expressway": 0,
    "Skyway": 0,
    "Pavillion": 0,
    "Parkway": -1,
    "Freeway": -1,
    "Business": -1,
    "Turnpike": -1,
    "Thruway": -1,
    "Real": -1,
    "Connector": -1,
    "Bypass": -1,
    "Greenway": -1,
}

directions = (
    "Northeast",
    "Northwest",
    "Southeast",
    "Southwest",
    "South",
    "S",
    "East",
    "E",
    "North",
    "N",
    "West",
    "W",
)

invalid_names = (
    "Loundenville",
    "Place Marquette",
    "Melendy",
    "Loundenville",
    "Grand Pavilion",
    "Champney",
    "Bluffestates",
    "Via Primero",
    "Via Roma",
    "John F. Kennedy Causeway",
)
road_names = (
    "West Mesquital del Oro",
    "Snead Fairway",
    "Yacht Wanderer",
    "Taft Pointe",
    "Avenida Encinas",
    "Scarlet Oak",
    "HiHo Hill",
    "Appaloosa Cove",
    "Hanover Close",
    "Avenue 19 1/2",
    "Avenue 348 A",
    "Bluff Hollow",
    "Trouville Esplanade",
)


def classify_road_type(road_name):
    if (
        "Highway" in road_name
        or "NDOT" in road_name
        or "County Road" in road_name
        or "County Route" in road_name
    ):
        return -1
    if road_name in invalid_names:
        return -1
    if road_name.endswith(")"):
        road_name = road_name[:-1]
    for direction in directions:
        if road_name.endswith(f" {direction}"):
            road_name = road_name[: -(len(direction) + 1)]
    road_type = road_name.split(" ")[-1].title()
    if road_type in road_types:
        return road_types[road_type]
    if road_type.isnumeric():
        return -1
    if road_name in road_names:
        return True
    raise RuntimeError(road_name)


def refine_location(poly, tries=10):
    rng = np.random.RandomState(1)

    min_x, min_y, max_x, max_y = poly.bounds

    with_score = []

    while tries:
        x = rng.rand() * (max_x - min_x) + min_x
        y = rng.rand() * (max_y - min_y) + min_y

        if not poly.contains(Point(x, y)):
            continue
        tries -= 1
        addr = reverse_geocode(x, y)

        score = (
            classify_road_type(addr.raw["address"]["road"])
            if "road" in addr.raw["address"]
            else -2
        )
        if not poly.contains(Point(addr.longitude, addr.latitude)):
            score -= 10
        with_score.append((addr, score))
        if score == 1:
            break
    best_addr = max(with_score, key=lambda x: x[1])[0]
    return best_addr.longitude, best_addr.latitude
