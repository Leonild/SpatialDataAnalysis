import os
import subprocess
import sys
os.environ['RPY2_CFFI_MODE'] = "API" # bug in cffi 1.13.0 https://bitbucket.org/rpy2/rpy2/issues/591/runtimeerror-found-a-situation-in-which-we

# Tested with rpy2 2.9.2-1
import numpy as np
from rpy2.robjects.packages import importr
from rpy2.robjects import r as R
from rpy2.robjects import numpy2ri
numpy2ri.activate()
from rpy2.interactive import process_revents
process_revents.start()

import matplotlib.pyplot as plt

eaf = importr("eaf")
path = R('system.file(package="eaf", "extdata")')[0] + "/"
alg1 = eaf.read_data_sets_(path + "ALG_1_dat")
alg1 = np.asarray(alg1)
alg2 = np.asarray(eaf.read_data_sets_(path + "ALG_2_dat"))

plt = eaf.eafplot(alg1[:, 0:2], sets=alg1[:,2])

plt.savefig('books_read.png')

input("Press ENTER to see next plot: ")

eaf.eafdiffplot(alg1, alg2, title_left="A", title_right="B")
