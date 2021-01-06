from visualization import visualizeBlocksworld
import subprocess

expFolder = 'experiments/'

# s = subprocess.Popen(['rm', '-r', expFolder])
# s.communicate()
# s = subprocess.Popen(['mkdir', expFolder])
# s.communicate()


tasks = [
    ["exp1", ['swipl','-g','experiment1','-g','halt','properties.pl']]
    # ["expOR", ['swipl','-g','experimentOR','-g','halt','properties.pl']]
    # ["expMOT", ['swipl','-g','experimentMOT','-g','halt','properties.pl']]
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
        last_vf = visualizeBlocksworld.getlastvaluefunction(content)
        imgfolder = "visualization/tempfigures/"
        imgfile = expFolder+name+".jpg"
        visualizeBlocksworld.main(last_vf, imgfolder, imgfile)

        imggoodfile = expFolder+name+"good.jpg"
        goods = visualizeBlocksworld.getgoodinterpretations(content)
        visualizeBlocksworld.main(goods, imgfolder, imggoodfile)
