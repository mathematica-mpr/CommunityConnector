# https://bionics.it/posts/luigi-tutorial

import luigi
import os
import sys
sys.path.insert(1, 'pipeline')
import pipeline_utilities as pu
import methodology_utilities as mu

raw_output = 'data/raw/'
cleaned_output = 'data/cleaned/'
final_output = 'data/'

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
        pu.MergeCleaned(cleaned_drive = cleaned_output, output = self.output().path)

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
## this can't be part of the pipeline because there is a manual component to it

class SdohScores(luigi.Task):
    def requires(self):
        return SelectVariables()
    def output(self):
        return luigi.LocalTarget(os.path.join(final_output, 'data_3_sdoh_scores.csv'))
    def run(self):
        mu.SdohScores(input = self.input().path, spca_dictionary = 'data/DictionaryPostSPCA.csv', output = self.output().path,
        output_data_dictionary = os.path.join(final_output, 'dictionary_2_sdoh_scores.csv'))

## TODO: change inputs to R methodology code as these outputs

class ReduceDisplayVars(luigi.Task):
    def requires(self):
        return SdohScores()
    def output(self):
        return luigi.LocalTarget(os.path.join(final_output, 'final_data.csv'))
    def run(self):
        # TODO: multiple outputs from SdohScores to read in the data dictionary directly
        pu.ReduceDisplayVars(input = self.input().path, input_data_dictionary = os.path.join(final_output, 'dictionary_2_sdoh_scores.csv'),
        output = self.output().path, output_data_dictionary = os.path.join(final_output, 'final_data_dictionary.csv'))
if __name__ == '__main__':
    luigi.run()
