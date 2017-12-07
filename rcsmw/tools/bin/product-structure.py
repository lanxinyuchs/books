#!/usr/bin/env python
'''
This program prints the product structure for an artifact, based on the the
Eiffel messaging data.

product-structure.py <artifact> <version>

Example:

    product-structure.py CXA11448_2 R4B04

'''

import json
import os
from subprocess import check_output
import sys
from eiffel.artifact import make_class


def print_product_structure(artifact, levels=1):
    '''Recursively print product structure to the given level, which is 1
    by default. Use 0 for infinite levels'''

    global report_format

    # Print the product structure to stdout

    for dep in artifact.contains():
        print '{} contains {}'.format(artifact.id(), dep.gav())
    for dep in artifact.provides():
        print '{} provides {}'.format(artifact.id(), dep.gav())

    if levels == 1:
        return

    levels -= 1

    for dep in artifact.contains() + artifact.provides():
        print_product_structure(dep, levels)


if len(sys.argv) < 3:
    print "Usage: product-structure.py <artifact> <version>"
    print "Example: product-structure.py CXP1234567 R1A234"
    sys.exit()

artifact = make_class('com.ericsson', sys.argv[1], sys.argv[2])

print_product_structure(artifact)
