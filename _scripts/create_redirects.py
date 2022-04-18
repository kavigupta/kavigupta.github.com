import os
from sys import argv

_, links_file, out_folder = argv

template = """
<!DOCTYPE html>
<html>

<head>
    <link rel="canonical" href="{link}" />
    <meta http-equiv="content-type" content="text/html; charset=utf-8" />
    <meta http-equiv="refresh" content="0;url={link}" />
</head>

<body>
</body>

</html>
"""


def run():
    class X:
        exec(open(links_file).read())

    return X.links


try:
    os.makedirs(out_folder)
except FileExistsError:
    pass

for k, v in run().items():
    with open(f"{out_folder}/{k}.html", "w") as f:
        f.write(template.format(link=v))
