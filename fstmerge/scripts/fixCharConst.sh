#!/bin/bash

# replace occurances of \uXXXX with \u
# find $1 -name "*.java" -exec sed -i 's/\\u..../\\u/g' {} \;

# replace occurances of \uXX - \uXXXXX with \u
find $1 -name "*.java" -exec sed -i 's/\\u[[:alnum:]?]\{2,5\}/\\u/g' {} \;

