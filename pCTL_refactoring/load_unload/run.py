# from visualization import visualizeBlocksworld
import subprocess

expFolder = 'experiments/'

s = subprocess.Popen(['rm', '-r', expFolder])
s.communicate()
s = subprocess.Popen(['mkdir', expFolder])
s.communicate()


l = [
    ["exp1", ['swipl','-g','reachNsteps(3)','-g','halt','properties.pl']]
    # ["exp2", ['swipl','-g','experiment2','-g','halt','properties.pl']]
    # ["experimentX_iter_1", ['swipl','-g','experimentX_iter_1','-g','halt','properties.pl']],
    # ["experimentF_iter_1", ['swipl','-g','experimentF_iter_1','-g','halt','properties.pl']],
    # ["experimentU_iter_1", ['swipl','-g','experimentU_iter_1','-g','halt','properties.pl']]
]


for exp in l:
    name = exp[0]
    expargs = exp[1]
    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()
