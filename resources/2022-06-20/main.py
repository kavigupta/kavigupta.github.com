from data import load_data, get_precincts
from downloader import download_streetview
from renderer import add_targets, plot_voters, populate_template, single_image


def main(count):
    selection = load_data(count)
    total = get_precincts()
    rs_in_d, ds_in_r = (
        total.R[total.R < total.D].sum() / total.R.sum(),
        total.D[total.R > total.D].sum() / total.D.sum(),
    )
    print(rs_in_d, ds_in_r)
    images = []
    for _, row in selection.iterrows():
        images.append(download_streetview(row.x, row.y))
    for idx in range(len(images)):
        images[idx].save(f"out/{idx}.png")

    with open("index.html", "w") as f:
        f.write(populate_template(selection))

    plot_voters(selection).write_html("graph.html")
    add_targets("graph.html")

    out = single_image(count, 100)
    out.save("header_image.jpg")


main(1000)
