from visualization import visualizeSARS
import subprocess

expFolder = 'experiments/'

s = subprocess.Popen(['rm', '-r', expFolder])
s.communicate()
s = subprocess.Popen(['mkdir', expFolder])
s.communicate()


tasks = [
    ["exp1", ['swipl','-g','experiment1','-g','halt','properties.pl']]
    # ["exp2", ['swipl','-g','experiment2','-g','halt','properties.pl']]
    # ["experimentX_iter_1", ['swipl','-g','experimentX_iter_1','-g','halt','properties.pl']],
    # ["experimentF_iter_1", ['swipl','-g','experimentF_iter_1','-g','halt','properties.pl']],
    # ["experimentU_iter_1", ['swipl','-g','experimentU_iter_1','-g','halt','properties.pl']]
]


for exp in tasks:
    name = exp[0]
    expargs = exp[1]
    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()
    # read outputfile
    with open(outputfilename, 'r') as f:
        content = f.read()
        last_vf = visualizeSARS.getlastvaluefunctionSARS(content)
        # print(last_vf)
        imgfolder = "tempfigures/"
        valuefunctionfile = name+".png" # without file extension
        main(last_vf, imgfolder, valuefunctionfile)
