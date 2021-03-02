from visualization import visualizeBlocksworld
import subprocess
import pathlib
import os
import errno

MIN_NUM_BLOCKS = 3
MAX_NUM_BLOCKS = 5

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
    for block_limit in range(MIN_NUM_BLOCKS,MAX_NUM_BLOCKS+1): # TODO change to MAX
        log_file = os.path.join(EXPFOLDER, "reachability_state_bound_{0}blocks.log".format(block_limit))
        with open(log_file, 'w+') as f:
            expargs = ['swipl','-g','reach_within_step('+str(block_limit)+')','-g','halt','properties.pl']
            process = subprocess.Popen(expargs, stdout=f)
            process.communicate()

if __name__ == "__main__":
    reachability_tasks_state_bound()


