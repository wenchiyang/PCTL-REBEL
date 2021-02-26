from visualization import visualizeBlocksworld
import subprocess
import pathlib
import os
import errno

MIN_NUM_BLOCKS = 3
MAX_NUM_BLOCKS = 10

CWD = pathlib.Path(__file__).parent.absolute()
EXPFOLDER = os.path.join(CWD,"experiments")
if not os.path.exists(EXPFOLDER):
    try:
        os.makedirs(EXPFOLDER)
    except OSError as exc:  # Guard against race condition
        if exc.errno != errno.EEXIST:
            raise


def reachability_tasks_state_bound():
    # reachability_tasks_state_bound
    for block_limit in range(MIN_NUM_BLOCKS,5): # TODO change to MAX
        log_file = os.path.join(EXPFOLDER, "reachability_state_bound_{0}blocks.log".format(block_limit))
        with open(log_file, 'w+') as f:
            expargs = ['swipl','-g','reach('+str(block_limit)+')','-g','halt','properties.pl']
            process = subprocess.Popen(expargs, stdout=f)
            process.communicate()

reachability_tasks_state_bound()


# tasks = [
#     # ["test_untilequal", ['swipl','-g','test_untilequal','-g','halt','properties.pl']]
#     ["exp1", ['swipl','-g','reach(4)','-g','halt','properties.pl']]
#     # ["exp1", ['swipl','-g','experimentMOT','-g','halt','properties.pl']]
#     # ["exp2", ['swipl','-g','experiment2','-g','halt','properties.pl']]
#     # ["experimentX_iter_1", ['swipl','-g','experimentX_iter_1','-g','halt','properties.pl']],
#     # ["experimentF_iter_1", ['swipl','-g','experimentF_iter_1','-g','halt','properties.pl']],
#     # ["experimentU_iter_1", ['swipl','-g','experimentU_iter_1','-g','halt','properties.pl']]
# ]
#

# for exp in tasks:
#     name = exp[0]
#     expargs = exp[1]
#     outputfilename = expFolder+name+".txt"
#     with open(outputfilename, 'w') as f:
#         process = subprocess.Popen(expargs, stdout=f)
#         process.communicate()
    # read outputfile
    # with open(outputfilename, 'r') as f:
    #     content = f.read()
    #     last_vf = visualizeBlocksworld.getlastvaluefunction(content)
    #     imgfolder = "visualization/tempfigures/"
    #     imgfile = expFolder+name+".jpg"
    #     visualizeBlocksworld.main(last_vf, imgfolder, imgfile)
