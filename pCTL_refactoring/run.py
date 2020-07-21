from visualization import visualizeBlocksworld
import subprocess
from time import sleep

expFolder = 'experiments/'

s = subprocess.Popen(['rm', '-r', expFolder])
s.communicate()
s = subprocess.Popen(['mkdir', expFolder])
s.communicate()


l = [
    ["exp1", ['swipl','-g','experiment1','-g','halt','properties.pl']]
    # ["exp2", ['swipl','-g','experiment2','-g','halt','properties.pl']]
]


for exp in l:
    name = exp[0]
    expargs = exp[1]
    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()
    # read outputfile
    with open(outputfilename, 'r') as f:
        content = f.read()
        last_vf = visualizeBlocksworld.getlastvaluefunction(content)
        imgfolder = "visualization/tempfigures/"
        imgfile = expFolder+name+".jpg"
        visualizeBlocksworld.main(last_vf, imgfolder, imgfile)
