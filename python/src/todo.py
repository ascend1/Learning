#!/usr/bin/python

import os
import sys

def add(x):
    f = file("todo.txt", 'a')
    f.write(x + os.linesep)
    f.close()

def view():
    try:
        with open("todo.txt"): pass
    except IOError:
        print "todo.txt doesn't exist. Add something first?"
        return None
    f = file("todo.txt", 'r')
    i = 1
    while True:
        line = f.readline()
        if len(line) == 0:
            break
        print "%d - %s" % (i, line),
        i += 1
    f.close()

def rm(i):
    try:
        with open("todo.txt"): pass
    except IOError:
        print "todo.txt doesn't exist. Add something first?"
        return None
    items = []
    f = file("todo.txt", 'r')
    while True:
        line = f.readline()
        if len(line) == 0:
            break
        items.append(line)
    f.close()
    if (i > len(items)):
        print "Invalid index"
        return None
    del items[i - 1]
    f = file("todo.txt", 'w')
    for item in items:
        f.write(item)
    f.close()

print sys.version
argc = len(sys.argv)
if argc == 1:
    print "missing command"
    sys.exit(-1)
content = ""
if sys.argv[1] == "add":
    if argc == 2:
        print "missing argument"
        sys.exit(-2)
    for i in range(2, argc):
        content += (sys.argv[i] + " ")
    add(content)
elif sys.argv[1] == "view":
    view()
elif sys.argv[1] == "del":
    if argc == 2:
        print "missing argument"
        sys.exit(-2)
    index = int(sys.argv[2])
    rm(index)
else:
    print "unkown command"
    sys.exit(-3)
