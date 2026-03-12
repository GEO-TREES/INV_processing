#!/usr/bin/env sh

drawio() {
	/Applications/draw.io.app/Contents/MacOS/./draw.io --crop -x -o $2 $1 \;
}

drawio ./tree_inv_geo-trees_flow.drawio ./tree_inv_geo-trees_flow.pdf

pdftoppm -singlefile -png -r 300 ./tree_inv_geo-trees_flow.pdf ./tree_inv_geo-trees_flow 
