from visualization import visualizeSARS
import subprocess


def run():
    expFolder = 'experiments/'

    s = subprocess.Popen(['rm', '-r', expFolder])
    s.communicate()
    s = subprocess.Popen(['mkdir', expFolder])
    s.communicate()


    tasks = [
        ["exp1", ['swipl','-g','experiment1','-g','halt','properties.pl']],
        # ["expXX", ['swipl','-g','experimentXX','-g','halt','properties.pl']]

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
            imgfolder = "visualization/tempfigures/"
            valuefunctionfile = expFolder+name+".png" # without file extension
            visualizeSARS.main(last_vf, imgfolder, valuefunctionfile)

def test():
    task = ["exp1", ['swipl','-g','experiment1','-g','halt','properties.pl']]
    name = task[0]
    expargs = task[1]
    expFolder = "experiments/"
    outputfilename = expFolder+name+".txt"
    content =\
"""
## value function with action ##
partialQ(s_([cl(a),cl(b),on(a,_107326)]),a_(move(a,b,_107326)),r_(0.9),ss_([on(a,b)]))
partialQ(s_([cl(_107502),cl(a),on(a,b),on(_107502,_107654)]),a_(move(_107502,a,_107654)),r_(0.9),ss_([on(a,b)]))
partialQ(s_([cl(_107966),cl(_108118),on(a,b),on(_107966,a)]),a_(move(_107966,_108118,a)),r_(0.9),ss_([on(a,b)]))
partialQ(s_([cl(_108474),cl(_108834),on(a,b),on(_108474,_108626)]),a_(move(_108474,_108834,_108626)),r_(0.9),ss_([on(a,b)]))
partialQ(s_([cl(a),cl(_109182),on(a,b)]),a_(move(a,_109182,b)),r_(0.81),ss_([cl(a),cl(b),on(a,_109182)]))
partialQ(s_([cl(a),cl(b),cl(_109554),on(a,_109618)]),a_(move(a,_109554,_109618)),r_(0.81),ss_([cl(a),cl(b),on(a,_109554)]))
partialQ(s_([cl(b),cl(_110056),on(b,a),on(a,_110120)]),a_(move(b,_110056,a)),r_(0.81),ss_([cl(a),cl(b),on(a,_110120)]))
partialQ(s_([cl(a),cl(b),cl(_110516),on(a,_110580),on(b,_110644)]),a_(move(b,_110516,_110644)),r_(0.81),ss_([cl(a),cl(b),on(a,_110580)]))
partialQ(s_([cl(a),cl(_111206),cl(_111334),on(a,_111270),on(_111206,b)]),a_(move(_111206,_111334,b)),r_(0.81),ss_([cl(a),cl(b),on(a,_111270)]))
partialQ(s_([cl(b),cl(_111896),cl(_112024),on(a,_111960),on(_111896,a)]),a_(move(_111896,_112024,a)),r_(0.81),ss_([cl(a),cl(b),on(a,_111960)]))
partialQ(s_([cl(a),cl(b),cl(_112586),cl(_112922),on(a,_112650),on(_112586,_112714)]),a_(move(_112586,_112922,_112714)),r_(0.81),ss_([cl(a),cl(b),on(a,_112650)]))
partialQ(s_([cl(_113562),cl(_113690),on(_113562,_113626)]),a_(move(_113562,_113690,_113626)),r_(0.0),ss_([cl(_113562),cl(_113626),on(_113562,_113690)]))
Number of abstract states: 12
########
"""

    last_vf = visualizeSARS.getlastvaluefunctionSARS(content)
    # print(last_vf)
    imgfolder = "visualization/tempfigures/"
    valuefunctionfile = expFolder+name+".png" # without file extension
    visualizeSARS.main(last_vf, imgfolder, valuefunctionfile)


if __name__ == '__main__':
    run()
    # test()
