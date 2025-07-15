from mrg32k3a.mrg32k3a import MRG32k3a
import simopt
from simopt.models import cntnv
import numpy as np

model = simopt.models.cntnv.CntNV()
responses, gradients = model.replicate([MRG32k3a()])
print(responses)
