#!/bin/bash
fn=$(echo $1 | tr ' ' _ | tr '[:upper:]' '[:lower:]')
tred tmp/graph.dot | dot -Tpdf > "results/"$fn"_arrows.pdf"
python3 bar_graph.py "results/"$fn"_bar.pdf"

