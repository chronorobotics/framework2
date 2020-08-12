from tmp.results import *
import numpy as np
import matplotlib.pyplot as plt
import sys

#labels = ['Train.', 'Test 1', 'Test 2', 'Test 3', 'Test 4', 'Test 5', 'Test 6', 'Test 7', 'Test 8', 'Test 9']
x = np.arange(len(labels))
width = 0.9
fig, ax = plt.subplots()
ax.grid(axis="y")

for i in range(len(methods)):
	err = np.array(errors[i])
	rects = ax.bar(x + (width/len(methods)*i) - width/2, err, width/len(methods), label=methods[i])

ax.set_ylabel('Mean Square Error')
ax.set_xlabel('Dataset')
ax.set_title(name)
ax.set_xticks(x)
ax.set_xticklabels(labels)
ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))
plt.xticks(rotation=90)

fig.tight_layout()

plt.savefig(sys.argv[1])

