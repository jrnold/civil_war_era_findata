#!python3
import datetime
import yaml
import json
import glob
from os.path import join, splitext, basename

import sys

def main(src, dst):
    with open(join(src, 'root.yaml'), 'r', encoding = 'utf-8') as f:
        print("reading root.yaml")
        data = yaml.load(f)
        data['last_modified'] = datetime.datetime.today().isoformat()
    data['resources'] = []
    for filename in glob.glob('%s/resources/*.yaml' % src):
        print("reading %s" % filename)
        with open(filename, 'r', encoding = 'utf-8') as f:
            name = splitext(basename(filename))[0]
            res = yaml.load(f)
            ## add name
            res['name'] = name
            data['resources'].append(res)
    with open(dst, 'w') as f:
        print("writing to %s" % dst)
        json.dump(data, f, indent = 2)

if __name__ == '__main__':
    srcdir = sys.argv[1]
    outfile = sys.argv[2]
    main(srcdir, outfile)
