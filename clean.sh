#!/bin/bash

find fsharp* -type d -name obj | xargs rm -Rf
find fsharp* -type d -name bin | xargs rm -Rf
