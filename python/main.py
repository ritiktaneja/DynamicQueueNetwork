
import sys

import numpy as np
from abcpy.probabilisticmodels import ProbabilisticModel, Continuous, InputConnector
from abcpy.continuousmodels import Uniform

from scipy.stats import norm
from random import sample

### R interface -----------------------

import rpy2.robjects as robjects
import rpy2.robjects.numpy2ri
import rpy2.rinterface
from rpy2.robjects.packages import importr
rpy2.robjects.numpy2ri.activate()

robjects.r('''
       suppressMessages(source('../../script/abcpy.R'))
''')
c = robjects.r['c']

r_inp           = robjects.globalenv['inp']
r_inp_synthetic = robjects.globalenv['inp_s']
r_sim           = robjects.globalenv['simulator']

import warnings
from rpy2.rinterface import RRuntimeWarning
warnings.filterwarnings("ignore", category=RRuntimeWarning)

# define backend
#from abcpy.backends import BackendDummy as Backend
from abcpy.backends import BackendMPI as Backend

#import pyspark
#sc = pyspark.SparkContext.getOrCreate()
#from abcpy.backends import BackendSpark as Backend
#backend = Backend(sc, parallelism=7)
backend = Backend()

### ---------------------------------

class Airport(ProbabilisticModel, Continuous):
  def __init__(self, parameters, name = 'Airport', model_num = 1, synthetic = True):
    self.model_num = model_num
    self.synthetic = synthetic
    if not isinstance(parameters, list):
        raise TypeError('Input should be type list.')

    input_connector = InputConnector.from_list(parameters)
    super().__init__(input_connector, name)

  def get_output_dimension(self):
    return 1

  def sample_from_prior(self):
    sample = self.prior.sample(1).reshape(-1)
    self.set_parameters(sample)

  def _check_input(self, input_values):
    return True

  def _check_output(self, values):
    return True

  def forward_simulate(self, input_values, k, rng = np.random.RandomState()):

    x = []

    for i in range(0, k):
      if(self.model_num == 1):
        if(self.synthetic):
          x.append(np.array([r_sim(c(input_values[0], input_values[1], input_values[2], input_values[3]), inp = r_inp_synthetic, type = "MMD")[0]]))
        elif(not(self.synthetic)):
          x.append(np.array([r_sim(c(input_values[0], input_values[1], input_values[2], input_values[3]), inp = r_inp, type = "MMD")[0]]))
      elif(self.model_num == 2):
        if(self.synthetic):
          x.append(np.array([r_sim(c(input_values[0], input_values[1], input_values[2], input_values[3]), inp = r_inp_synthetic, type = "Wass")[0]]))
        elif(not(self.synthetic)):
          x.append(np.array([r_sim(c(input_values[0], input_values[1], input_values[2], input_values[3]), inp = r_inp, type = "Wass")[0]]))

    return x

def infer_parameters(model_num, synthetic, T, n_sample, ar_cutoff):

  y_obs = [0]

  #prior = Uniform([[0, 0, 0, 0], [5, 5, 2.5, 2.5]])
  prior1 = Uniform([[0], [5]])
  prior2 = Uniform([[0], [5]])
  prior3 = Uniform([[0], [2.5]])
  prior4 = Uniform([[0], [2.5]])

  model = Airport([prior1, prior2, prior3, prior4], name = "Airport", model_num = model_num, synthetic = synthetic)

  from abcpy.statistics import Identity
  statistics_calculator = Identity(degree = 1, cross = False)

  from abcpy.distances import Euclidean
  distance_calculator = Euclidean(statistics_calculator)

  # define sampling scheme
  from abcpy.inferences import SABC
  sampler = SABC([model], [distance_calculator], backend)

  # define sampling scheme
  #from abcpy.inferences import PMCABC
  #sampler = PMCABC([model], [distance_calculator], backend, kernel, seed=1)

  # sample from scheme
  journal = sampler.sample([y_obs],  T, 1000, n_sample, 1,  ar_cutoff = ar_cutoff)

  return journal

def analyse_journal(journal, file):
    journal.save(file + ".jnl")
    np.savetxt(file + ".csv", journal.parameters[0], delimiter = ",")

if __name__  == "__main__":
    T = int(sys.argv[1])
    n_sample = int(sys.argv[2])
    ar_cutoff = float(sys.argv[3])
    journal = infer_parameters(model_num = 1, synthetic = True, T = T, n_sample = n_sample, ar_cutoff = ar_cutoff)
    analyse_journal(journal, file = "MMD_syn")
    journal = infer_parameters(model_num = 1, synthetic = False, T = T, n_sample = n_sample, ar_cutoff = ar_cutoff)
    analyse_journal(journal, file = "MMD_real")
    journal = infer_parameters(model_num = 2, synthetic = True, T = T, n_sample = n_sample, ar_cutoff = ar_cutoff)
    analyse_journal(journal, file = "Wass_syn")
    journal = infer_parameters(model_num = 2, synthetic = False, T = T, n_sample = n_sample, ar_cutoff = ar_cutoff)
    analyse_journal(journal, file = "Wass_real")




