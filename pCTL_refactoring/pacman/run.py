# from visualization import visualizeBlocksworld
import subprocess

expFolder = 'experiments/'


tasks = [
    ["exp1", ['swipl','-g','experiment1','-g','halt','properties.pl']]
]


for exp in tasks:
    name = exp[0]
    expargs = exp[1]
    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()
