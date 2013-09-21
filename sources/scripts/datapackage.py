import yaml
import json
import glob
from os.path import join

import sys

def main(src, dst):
    with open(join(src, 'root.yaml'), 'r') as f:
        print("reading root.yaml")
        data = yaml.load(f)
    data['resources'] = []
    for filename in glob.glob('%s/resources/*.yaml' % src):
        print("reading %s" % filename)
        with open(filename, 'r') as f:
            res = yaml.load(f)
            data['resources'].append(res)
    with open(dst, 'w') as f:
        print("writing to %s" % dst)
        json.dump(data, f, indent = 2)

if __name__ == '__main__':
    srcdir = sys.argv[1]
    outfile = sys.argv[2]
    main(srcdir, outfile)
