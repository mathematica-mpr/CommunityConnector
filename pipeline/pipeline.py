# https://bionics.it/posts/luigi-tutorial

import luigi
import os
import sys
sys.path.insert(1, 'pipeline')
import pipeline_utilities as pu
import methodology_utilities as mu

class ParameterDefinitions(object):
    cleaned_output = luigi.parameter.Parameter(default = 'data/cleaned/',
                                     description = 'Input directory of cleaned, scraped data files')
    output_dir = luigi.parameter.Parameter(default = 'data/',
                             description = 'Intermediate data output directory')
    manual_dictionary = luigi.parameter.Parameter(default = 'data/dictionary_1_manual.csv',
                             description = 'Data dictionary created manually')
    final_output = luigi.parameter.Parameter(default = 'DockerShinyApp/app/data/',
                             description = 'Data directory for app')

######################################
###### Data scraping & cleaning ######
######################################

# TODO: change this to a parameter
# raw_output = 'data/raw/'
# class DemGeographicPUF(luigi.Task):
#     def requires(self):
#         return None
#     def output(self):
#         return None
#     def run(self):
#         pu.GeographicPUF(outdir = raw_output)
# class DemOppAtlas(luigi.Task):
#     # TODO: eventually will move cleaned_output parameter to this task
#     def requires(self):
#         return None
#     def output(self):
#         return luigi.LocalTarget(os.path.join(cleaned_output, 'opp_atlas_cleaned.csv'))
#     def run(self):
#         pu.OppAtlas(output = self.output().path)

# eventually would add all scraping & cleaning codes

class MergeCleaned(ParameterDefinitions, luigi.Task):

    def requires(self):
        # return DemOppAtlas()
        # TODO: eventually will require all of the scraping/data cleaning
        # This task also allows us to pull in data that is stored in the data/cleaned
        # folder, even if it was pulled manually or wasn't incorporated into this pipeline
        return None
    def output(self):
        return luigi.LocalTarget(os.path.join(self.output_dir, 'data_1_merged.csv'))
    def run(self):
        pu.MergeCleaned(cleaned_drive = self.cleaned_output, output = self.output().path)

######################################
######        Methodology       ######
######################################

class SelectVariables(ParameterDefinitions, luigi.Task):
    def requires(self):
        return MergeCleaned()
    def output(self):
        return luigi.LocalTarget(os.path.join(self.output_dir,'data_2_selected_variables.csv'))
    def run(self):
        pu.SelectVariables(input = self.input().path, output = self.output().path,
                           data_dictionary = self.manual_dictionary)

## TODO: from here, we can run SPCA
## this can't be part of the pipeline because there is a manual component to it

class FinalDictionary(ParameterDefinitions, luigi.Task):
    def requires(self):
        return SelectVariables()
    def output(self):
        return luigi.LocalTarget(os.path.join(self.output_dir, 'dictionary_2_sdoh_scores.csv'))
    def run(self):
        mu.FinalDictionary(spca_dictionary = 'data/DictionaryPostSPCA.csv',
                           output_data_dictionary = self.output().path,
                           input_data_dictionary = self.manual_dictionary)

class SdohScores(ParameterDefinitions, luigi.Task):
    def requires(self):
        return {'data': SelectVariables(), 'dictionary': FinalDictionary()}
    def output(self):
        return luigi.LocalTarget(os.path.join(self.output_dir, 'data_3_sdoh_scores.csv'))
    def run(self):
        mu.SdohScores(input = self.input()['data'].path,
                      input_data_dictionary = self.input()['dictionary'].path,
                      output = self.output().path)

class ReduceDisplayVars(ParameterDefinitions, luigi.Task):
    def requires(self):
        return {'data': SdohScores(), 'dictionary': FinalDictionary()}
    def output(self):
        return luigi.LocalTarget(os.path.join(self.final_output, 'final_data.csv'))
    def run(self):
        pu.ReduceDisplayVars(input = self.input()['data'].path,
                             input_data_dictionary = self.input()['dictionary'].path,
                             output = self.output().path,
                             output_data_dictionary = os.path.join(self.final_output, 'final_data_dictionary.csv'))

if __name__ == '__main__':
    luigi.build([ReduceDisplayVars()], local_scheduler=True)
