#!/bin/bash

#diff <(hd $1) <(hd $2)

tmp1=/tmp/hd.out1.$$
tmp2=/tmp/hd.out2.$$
hd $1 > $tmp1
hd $2 > $tmp2
git diff --no-index $tmp1 $tmp2
rm $tmp1 $tmp2
