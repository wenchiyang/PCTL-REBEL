# from visualization import visualizeBlocksworld
import subprocess
import os, pathlib, errno

CWD = pathlib.Path(__file__).parent.absolute()
EXPFOLDER = os.path.join(CWD,"experiments")
if not os.path.exists(EXPFOLDER):
    try:
        os.makedirs(EXPFOLDER)
    except OSError as exc:  # Guard against race condition
        if exc.errno != errno.EEXIST:
            raise



def reachability_tasks_with_a_policy():
    step = 5
    log_file = os.path.join(EXPFOLDER, "reachability_tasks_with_a_policy.log")
    with open(log_file, 'w+') as f:
        expargs = ['swipl','-g','reachability_tasks_with_a_policy({0})'.format(step),'-g','halt','properties.pl']
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()

if __name__ == "__main__":
    reachability_tasks_with_a_policy()

