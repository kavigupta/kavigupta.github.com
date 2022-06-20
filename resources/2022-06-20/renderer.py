import re
import numpy as np
from PIL import Image, ImageDraw, ImageFont
import plotly.graph_objects as go

from permacache import permacache

from data import load_data
from downloader import reverse_geocode

FOLDER = "/resources/2022-06-20/out"


def precinct_label(row):
    total = row.R + row.D + row.O
    margin = row.D - row.R
    margin = margin / total
    if margin < 0:
        return f'<div class="text gop precinct">Precinct Margin: R+{-margin:.0%}</div>'
    else:
        return f'<div class="text dem precinct">Precinct Margin: D+{margin:.0%}</div>'


def render_degree(val, dirs):
    direction = dirs[int(val > 0)]
    val = abs(val)
    degree = int(val)
    val -= degree
    val *= 60
    minutes = int(val)
    val -= minutes
    val *= 60
    seconds = int(val)
    return str(degree) + "&deg;" + str(minutes) + "'" + str(seconds) + '"' + direction


def render_coordinate(row):
    return render_degree(row.y, "SN") + " " + render_degree(row.x, "WE")


def render_voter(row, idx):
    if row.selected_party == "D":
        return f'<div class="text dem voter">Voter {idx + 1} voted for Biden</div>'
    else:
        return f'<div class="text gop voter">Voter {idx + 1} voted for Trump</div>'


def render_row(idx, row):
    x, y = row.ox, row.oy
    a = f"""
    <td style="width: 50%;">
        <h3 class="text voter_header" id="voter-{idx+1}">Voter {idx + 1}'s Neighborhood</h3>

        <!-- {render_voter(row, idx)} -->
        <div class="text coordinate"> {render_coordinate(row)}</div>
        <div class="text address"><i>approx.</i> {reverse_geocode(row.x, row.y)}</div>
        {precinct_label(row)}
    </td>
    """
    b = f"""
    <td style="width: 50%;">
        <div class="fill">
            <a href="https://maps.google.com/?q={y},{x}&ll={y},{x}&z=8" target="_blank">
                <image src="{FOLDER}/{idx}.png"/>
            </a>
        </div>
    </td>
    """
    return a, b


def render_full_table(selection):
    result = []
    for idx in range(len(selection)):
        row = selection.iloc[idx]
        if row.selected_party == "D":
            result.extend([[], []])
        a, b = render_row(idx, row)
        result[-2].append(a)
        result[-1].append(b)
    result = ["<tr>" + "\n".join(x) + "</tr>" for x in result]
    return "\n".join(result)


def populate_template(selection):
    content = render_full_table(selection)

    with open("template.html") as f:
        template = f.read()

    return (
        template.replace("$CONTENT", content)
        .replace("$COUNT", str(selection.shape[0]))
        .replace("$DEM", str(selection[selection.selected_party == "D"].shape[0]))
        .replace("$GOP", str(selection[selection.selected_party == "R"].shape[0]))
    )


def plot_voters(data):
    fig = go.Figure(
        data=go.Scattergeo(
            lat=data.y,
            lon=data.x,
            text=[f"Voter {i + 1}" for i in range(len(data))],
            customdata=[f"#voter-{i + 1}" for i in range(len(data))],
            mode="markers",
            marker_color=[
                "#ff3322" if row.selected_party == "R" else "#2266ff"
                for _, row in data.iterrows()
            ],
        )
    )
    fig.update_traces(hovertemplate="%{text}<extra></extra>")
    fig.update_layout(
        title="Voter locations (click to jump to the relevant voter)",
        geo_scope="usa",
        margin=dict(l=0, r=0, b=0, t=0),
    )
    return fig


def add_targets(graph_file):
    with open(graph_file) as f:
        plot_div = f.read()
    # Get id of html div element that looks like
    # <div id="301d22ab-bfba-4621-8f5d-dc4fd855bb33" ... >
    res = re.search('<div id="([^"]*)"', plot_div)
    div_id = res.groups()[0]

    # Build JavaScript callback for handling clicks
    # and opening the URL in the trace's customdata
    js_callback = """
    <script>
    var plot_element = document.getElementById("{div_id}");
    plot_element.on('plotly_click', function(data){{
        console.log(data);
        var point = data.points[0];
        if (point) {{
            console.log(point.customdata);
            let top = window.top.location.href.split('#')[0];
            top = top + point.customdata;
            console.log(top);
            window.top.location.href = top;
        }}
    }})
    </script>
    """.format(
        div_id=div_id
    )

    # Build HTML string
    html_str = """
    <html>
    <body>
    {plot_div}
    {js_callback}
    </body>
    </html>
    """.format(
        plot_div=plot_div, js_callback=js_callback
    )

    # Write out HTML file
    with open(graph_file, "w") as f:
        f.write(html_str)


def render_image_grid(images, ncols):
    size = max(max(x.size) for x in images)
    nrows = (len(images) + ncols - 1) // ncols
    out = np.ones((size * nrows, size * ncols, 3), dtype=np.uint8) * 255
    for i in range(len(images)):
        row, col = i // ncols, i % ncols
        out[row * size : (row + 1) * size, col * size : (col + 1) * size] = images[i]
    return out


def render_image_grid_for_party(data, party, size):
    images = [
        Image.open(f"out/{i}.png") for i in np.where(data.selected_party == party)[0]
    ]
    for image in images:
        image.thumbnail((size, size), Image.ANTIALIAS)

    ncols = int(round(len(data) ** 0.5 * 1.2)) // 2
    return render_image_grid(images, ncols)


def centered(im, W, H, msg, font_size):
    W, H = W * 2, H * 2
    draw = ImageDraw.Draw(im)
    myFont = ImageFont.truetype("Cantarell-Regular.otf", int(font_size))
    w, h = draw.textsize(msg, font=myFont)
    draw.text(((W - w) / 2, (H - h) / 2), msg, fill="black", font=myFont)


@permacache("voter-sampler/renderer/single-image")
def single_image(num_voters, size):
    data = load_data(num_voters)
    d = render_image_grid_for_party(data, "D", size)
    r = render_image_grid_for_party(data, "R", size)
    out = (
        np.ones(
            (max(d.shape[0], r.shape[0]), d.shape[1] + r.shape[1] + size, 3), np.uint8
        )
        * 255
    )
    out[: d.shape[0], : d.shape[1]] = d
    out[: r.shape[0], d.shape[1] + size : d.shape[1] + size + r.shape[1]] = r

    out = np.concatenate(
        [np.ones((size, out.shape[1], 3), np.uint8) * 255, out], axis=0
    )

    out = Image.fromarray(out)

    centered(out, d.shape[1] / 2, size / 2, "Democratic Voters", font_size=size / 2)
    centered(
        out,
        d.shape[1] + size + r.shape[1] / 2,
        size / 2,
        "Republican Voters",
        font_size=size / 2,
    )
    centered(
        out,
        d.shape[1] + r.shape[1],
        out.size[1] - size / 4,
        "@notkavi",
        font_size=size / 4,
    )

    return out
