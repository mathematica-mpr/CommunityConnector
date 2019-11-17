# https://bionics.it/posts/luigi-tutorial

import luigi
import os
import sys
sys.path.insert(1, 'pipeline')
import pipeline_utilities as pu
import methodology_utilities as mu

raw_output = 'data/01_raw/'
cleaned_output = 'data/02_cleaned/'
# TODO: replace this with cleaned_output when ready
old_cleaned_output = 'data/cleaned'
final_output = 'data/03_final'

######################################
###### Data scraping & cleaning ######
######################################

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
        return luigi.LocalTarget(os.path.join(cleaned_output, 'opp_atlas_cleaned.csv'))
    def run(self):
        pu.OppAtlas(output = self.output().path)

class MergeCleaned(luigi.Task):
    def requires(self):
        # return DemOppAtlas()
        # TODO: eventually will require all of the scraping/data cleaning
        return None
    def output(self):
        return luigi.LocalTarget(os.path.join(final_output, 'data_1_merged.csv'))
    def run(self):
        # TODO: will eventually move this to the pipeline_cleaned/folder
        pu.MergeCleaned(cleaned_drive = old_cleaned_output, output = self.output().path)

######################################
######        Methodology       ######
######################################

class SelectVariables(luigi.Task):
    def requires(self):
        return MergeCleaned()
    def output(self):
        return luigi.LocalTarget(os.path.join(final_output,'data_2_selected_variables.csv'))
    def run(self):
        pu.SelectVariables(input = self.input().path, output = self.output().path)

## TODO: from here, we can run SPCA
## which data and dictionary should 01_variable_reduction.R use?

class SdohScores(luigi.Task):
    def requires(self):
        return SelectVariables()
    def output(self):
        return luigi.LocalTarget(os.path.join(final_output, 'data_3_sdoh_scores.csv'))
    def run(self):
        mu.SdohScores(input = self.input().path, spca_dictionary = 'data/DictionaryPostSPCA.csv', output = self.output().path,
        output_data_dictionary = os.path.join(final_output, 'dictionary_2_sdoh_scores.csv'))

if __name__ == '__main__':
    luigi.run()
