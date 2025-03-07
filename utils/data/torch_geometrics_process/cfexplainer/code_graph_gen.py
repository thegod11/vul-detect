import os
import sys
import numpy as np
from .helpers import utils
from .helpers import joern
from .data_pre import bigvul
from sklearn.utils import shuffle
import re

def preprocess(row):
    """Parallelise joern functions.

    Example:
    df = bigvul()
    row = df.iloc[180189]  # PAPER EXAMPLE
    row = df.iloc[177860]  # EDGE CASE 1
    preprocess(row)
    """
    savedir_before = utils.get_dir(utils.processed_dir() / row["dataset"] / "before")
    savedir_after = utils.get_dir(utils.processed_dir() / row["dataset"] / "after")

    # Write C Files
    fpath1 = savedir_before / f"{row['id']}.c"
    with open(fpath1, "w") as f:
        f.write(row["before"])
    fpath2 = savedir_after / f"{row['id']}.c"
    if len(row["diff"]) > 0:
        with open(fpath2, "w") as f:
            f.write(row["after"])

    # Run Joern on "before" code
    if not os.path.exists(f"{fpath1}.edges.json"):
        joern.full_run_joern(fpath1, verbose=3)

    # Run Joern on "after" code
    if not os.path.exists(f"{fpath2}.edges.json") and len(row["diff"]) > 0:
        joern.full_run_joern(fpath2, verbose=3)

def process_cpp_file(code):
    processed_string = re.split(r'\(', code)[0]
    output = re.sub(r'\s+', ' ', processed_string.strip())
    sig = output.split(" ")

    if sig[-1] == "override":
        del sig[-1]
    if len(sig) == 1:
        sig.insert(0, "void")
        
    result = re.sub(r'^[^\(]*', " ".join(sig), code)
    return result

def preprocess_devign(row):
    """Parallelise joern functions.

    Example:
    df = bigvul()
    row = df.iloc[180189]  # PAPER EXAMPLE
    row = df.iloc[177860]  # EDGE CASE 1
    preprocess(row)
    """
    savedir = utils.get_dir(utils.processed_dir() / "CVEfixes" / "code")

    # Write Code Files
    if 'programming_language' in row.keys():
        if row['programming_language'] == 'C':
            fpath1 = savedir / f"{row['idx']}.c"
        elif row['programming_language'] == 'Java':
            row['func'] = "class Mock {\n" + row['func'] + "\n}"
            fpath1 = savedir / f"{row['idx']}.java"
        elif row['programming_language'] == 'C++':
            row['func'] = process_cpp_file(row['func'])
            fpath1 = savedir / f"{row['idx']}.cpp"
        elif row['programming_language'] == 'C#':
            row['func'] = "class Mock {\n" + row['func'] + "\n}"
            fpath1 = savedir / f"{row['idx']}.cs"
    else:
        fpath1 = savedir / f"{row['idx']}.c"
        
    if not os.path.exists(fpath1):
        with open(fpath1, "w") as f:
            f.write(row["func"])

    # Run Joern on "before" code
    if not os.path.exists(f"{fpath1}.edges.json") or not os.path.exists(f"{fpath1}.nodes.json"):
        joern.full_run_joern(fpath1, verbose=3)


if __name__ == "__main__":
    # SETUP
    NUM_JOBS = 5
    JOB_ARRAY_NUMBER = 0 if "ipykernel" in sys.argv[0] else int(sys.argv[1]) - 1
    # Read Data
    df = bigvul()
    df = df.iloc[::-1]
    df = shuffle(df)
    splits = np.array_split(df, NUM_JOBS)
    # Generate Graphs
    processed_list = utils.dfmp(splits[JOB_ARRAY_NUMBER], preprocess, ordr=False, workers=64)
    print(len(processed_list))
