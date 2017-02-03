#!/bin/bash

## Generate inline data: for PNG icon


files=`ls -1 *.png`

for file in ${files[@]}; do
    echo -e "\n$file ::\n"

    base64 -w0 "$file"

    echo ""
done
