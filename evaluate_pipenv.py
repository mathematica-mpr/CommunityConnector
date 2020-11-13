import os
import sys
from packaging import version
import operator
import re

# TODO: automate this using bash script and subprocess.call
# run pipenv graph > pipenv_graph.txt first

graph = os.path.join(sys.path[0],'pipenv_graph.txt')
with open(graph,'r') as f:
    graph = f.readlines()

operator_dict = {'>=': operator.ge,
                '>': operator.gt,
                '<=': operator.le,
                '<': operator.lt,
                '!=': operator.ne,
                '==': operator.eq,
                '~=': operator.eq}

graph = [line.strip() for line in graph if 'required' in line and 'Any' not in line]
for line in graph:
    package = line.split(' ')[1]
    rest = " ".join(line.split(' ')[2:])
    requirements = rest.split(':')[1].replace(', installed','').strip().split(',')
    installed = rest.split(':')[2].replace(']','')
    for rq in requirements:
        op_end_ind = re.search('\d',rq).span()[0]
        op = rq[:op_end_ind]
        comparison = operator_dict[op]
        rq_ver = rq.replace(op,'')
        if not (comparison(version.parse(installed), version.parse(rq_ver))):
            print(package)
            print(rq)
            print(installed)
            print('\n')
print('done')