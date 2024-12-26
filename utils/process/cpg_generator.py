import json
import re
import subprocess
import os.path
import os
import time
from .cpg_client_wrapper import CPGClientWrapper
#from ..data import datamanager as data


def funcs_to_graphs(funcs_path):
    client = CPGClientWrapper()
    # query the cpg for the dataset
    print(f"Creating CPG.")
    graphs_string = client(funcs_path)
    # removes unnecessary namespace for object references
    graphs_string = re.sub(r"io\.shiftleft\.codepropertygraph\.generated\.", '', graphs_string)
    graphs_json = json.loads(graphs_string)

    return graphs_json["functions"]


def graph_indexing(graph):
    if graph['file'] != "<includes>" and graph['file'] != "<empty>" :
        idx = int(graph["file"].split(".c")[0].split("/")[-1])
        del graph["file"]
        return idx, {"functions": [graph]}


def joern_parse(joern_path, input_path, output_path, file_name):
    out_file = file_name + ".bin"
    print('=== BEGIN joern_parse of ', out_file, '===')
    joern_parse_call = subprocess.run(["./" + joern_path + "joern-parse", input_path, "--output", output_path + out_file],
                                      stdout=subprocess.PIPE, text=True, check=True)
    print(str(joern_parse_call))
    return out_file


def joern_create(joern_path, in_path, out_path, cpg_files):
    joern_process = subprocess.Popen(["./" + joern_path + "joern"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
    json_files = []
    for cpg_file in cpg_files:
        json_file_name = f"{cpg_file.split('.')[0]}.json"
        json_files.append(json_file_name)

        print('=== BEGIN joern_create of', in_path+cpg_file, '===')
        print(in_path+cpg_file)
        if os.path.exists(in_path+cpg_file):
            json_out = f"{os.path.abspath(out_path)}/{json_file_name}"
            import_cpg_cmd = f"importCpg(\"{os.path.abspath(in_path)}/{cpg_file}\")\r".encode()
            script_path = f"{os.path.dirname(os.path.abspath(joern_path))}/graph-for-funcs.sc"
            run_script_cmd = f"cpg.runScript(\"{script_path}\").toString() |> \"{json_out}\"\r".encode()
            print('=== DEBUG1 ', in_path+cpg_file, '===')
            joern_process.stdin.write(import_cpg_cmd)
            # print('=== DEBUG2 ===')
            # print(joern_process.stdout.readline().decode())
            # print('=== DEBUG3 ===')
            joern_process.stdin.write(run_script_cmd)
            # print('=== DEBUG4 ===')
            # print(joern_process.stdout.readline().decode())
            # print('=== DEBUG5 ===')
            joern_process.stdin.write("delete\r".encode())
            # print('=== DEBUG6 ===')
            # print(joern_process.stdout.readline().decode())
            print('=== DEBUG7 ===')
    try:
        print('=== DEBUG8 ===')
        outs, errs = joern_process.communicate(timeout=600) # give more time for the process to finish
        print('=== DEBUG9 ===')
    except subprocess.TimeoutExpired:
        joern_process.kill()
        outs, errs = joern_process.communicate()
    if outs is not None:
        print(f"Outs: {outs.decode()}")
    if errs is not None:
        print(f"Errs: {errs.decode()}")
    return json_files


def json_process(in_path, json_file):
    if os.path.exists(in_path+json_file):
        with open(in_path+json_file) as jf:
            cpg_string = jf.read()
            cpg_string = re.sub(r"io\.shiftleft\.codepropertygraph\.generated\.", '', cpg_string)
            cpg_json = json.loads(cpg_string)
            # remove file attribute and give index for file that is not N/A
            # function with file = N/A is not included in the dataset 
            container = [graph_indexing(graph) for graph in cpg_json["functions"] if graph["file"] != "N/A"]
            return container
    return None

'''
def generate(dataset, funcs_path):
    dataset_size = len(dataset)
    print("Size: ", dataset_size)
    graphs = funcs_to_graphs(funcs_path[2:])
    print(f"Processing CPG.")
    container = [graph_indexing(graph) for graph in graphs["functions"] if graph["file"] != "N/A"]
    graph_dataset = data.create_with_index(container, ["Index", "cpg"])
    print(f"Dataset processed.")

    return data.inner_join_by_index(dataset, graph_dataset)
'''

# client = CPGClientWrapper()
# client.create_cpg("../../data/joern/")
# joern_parse("../../joern/joern-cli/", "../../data/joern/", "../../joern/joern-cli/", "gen_test")
# print(funcs_to_graphs("/data/joern/"))
"""
while True:
    raw = input("query: ")
    response = client.query(raw)
    print(response)
"""
