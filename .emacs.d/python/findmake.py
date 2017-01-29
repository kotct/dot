#!/usr/bin/python

import sys
from pathlib import Path
from subprocess import Popen

if (len(sys.argv) != 2):
    print('Usage \'findmake PATH\'')
    sys.exit(1)

directory = Path(sys.argv[1]).parent

max_dir = min(len(directory.parents), 8) + 1

i = 0

while i < max_dir and not (directory.joinpath('Makefile').exists()):
    directory = directory.parent
    i += 1

if directory.joinpath('Makefile').exists():
   Popen("make", cwd=str(directory)).wait()
else:
    print("No makefile found")
