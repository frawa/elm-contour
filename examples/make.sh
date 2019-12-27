#!/bin/bash

for file in *.elm
do
  elm make "${file}" --output="out/$(basename ${file} '.elm').html" --debug
done

cp out/Main.html out/index.html
