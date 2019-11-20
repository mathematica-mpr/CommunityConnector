# https://bionics.it/posts/luigi-tutorial

import luigi
import os
import sys
sys.path.insert(1, 'pipeline')
import pipeline_utilities as pu
import methodology_utilities as mu

raw_output = 'data/raw/'
cleaned_output = 'data/cleaned/'
output = 'data/'
final_output = 'DockerShinyApp/app/data/'

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

# eventually would add all scraping & cleaning codes

class MergeCleaned(luigi.Task):
    def requires(self):
        # return DemOppAtlas()
        # TODO: eventually will require all of the scraping/data cleaning
        # This task also allows us to pull in data that is stored in the data/cleaned
        # folder, even if it was pulled manually or wasn't incorporated into this pipeline
        return None
    def output(self):
        return luigi.LocalTarget(os.path.join(output, 'data_1_merged.csv'))
    def run(self):
        pu.MergeCleaned(cleaned_drive = cleaned_output, output = self.output().path)

######################################
######        Methodology       ######
######################################

class SelectVariables(luigi.Task):
    def requires(self):
        return MergeCleaned()
    def output(self):
        return luigi.LocalTarget(os.path.join(output,'data_2_selected_variables.csv'))
    def run(self):
        pu.SelectVariables(input = self.input().path, output = self.output().path)

## TODO: from here, we can run SPCA
## this can't be part of the pipeline because there is a manual component to it

class FinalDictionary(luigi.Task):
    def requires(self):
        return SelectVariables()
    def output(self):
        return luigi.LocalTarget(os.path.join(output, 'dictionary_2_sdoh_scores.csv'))
    def run(self):
        mu.FinalDictionary(spca_dictionary = 'data/DictionaryPostSPCA.csv', output_data_dictionary = self.output().path)

class SdohScores(luigi.Task):
    def requires(self):
        return {'data': SelectVariables(), 'dictionary': FinalDictionary()}
    def output(self):
        return luigi.LocalTarget(os.path.join(output, 'data_3_sdoh_scores.csv'))
    def run(self):
        mu.SdohScores(input = self.input()['data'].path,
                      input_data_dictionary = self.input()['dictionary'].path,
                      output = self.output().path)

class ReduceDisplayVars(luigi.Task):
    def requires(self):
        return SdohScores()
    def output(self):
        return luigi.LocalTarget(os.path.join(final_output, 'final_data.csv'))
    def run(self):
        # TODO: multiple outputs from SdohScores to read in the data dictionary directly
        pu.ReduceDisplayVars(input = self.input().path, input_data_dictionary = os.path.join(output, 'dictionary_2_sdoh_scores.csv'),
        output = self.output().path, output_data_dictionary = os.path.join(final_output, 'final_data_dictionary.csv'))

if __name__ == '__main__':
    luigi.build([SdohScores()], local_scheduler=True)
