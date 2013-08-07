import yaml
import json
import glob
import os.path

if __name__ == '__main__':
    with open('datapackage/root.yaml', 'r') as f:
        print("reading datapackage/root.yaml")
        data = yaml.load(f)
    data['resources'] = []
    for filename in glob.glob('datapackage/resources/*.yaml'):
        print("reading %s" % filename)
        with open(filename, 'r') as f:
            res = yaml.load(f)
            data['resources'].append(res)
    with open('../datapackage.json', 'w') as f:
        print("writing to ../datapackage.json")
        json.dump(data, f, indent = 2)
