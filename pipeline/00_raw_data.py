# https://bionics.it/posts/luigi-tutorial
# Targets = data that is persisted between task runs
# Tasks = python classes that subclass the luigi.Task super class. Methods:
# - requires()
# - output()
# - run() --> all the code the task should run
# - input()

import luigi
import os
import sys
sys.path.insert(1, 'pipeline')
import pipeline_utilities as pu

raw_output = 'data/pipeline_raw/'
cleaned_output = 'data/pipeline_cleaned/'

# class DemGeographicPUF(luigi.Task):
#     def requires(self):
#         return None
#     def output(self):
#         return None
#     def run(self):
#         pu.GeographicPUF(outdir = raw_output)

# TODO: UserWarning: Task DemOppAtlas() without outputs has no custom complete() method
class DemOppAtlas(luigi.Task):
    def requires(self):
        return None
    def output(self):
        return None
    def run(self):
        pu.OppAtlas(output = os.path.join(cleaned_output, 'opp_atlas_cleaned.csv'))

if __name__ == '__main__':
    luigi.run()
