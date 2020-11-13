import numpy as np
import matplotlib.pyplot as plt
arr = np.genfromtxt("tsunami.out")

times = arr[:,0]
data = arr[:,1:]

t = int(len(times)/2)

plt.figure()
plt.plot(data[t,:])
plt.savefig("out.png")
