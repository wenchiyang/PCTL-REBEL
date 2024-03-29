import re
import os.path
def scrapTimes(filename):
    """
    This function returns
        1. model construction time in seconds
        2. model checking time in seconds
    filename is a PRISM model checker a output file.
    return type is time, time
    """
    fh = open(filename, 'r')
    content = fh.readlines()
    relevantLines = [line for line in content if 'Time for ' in line]

    time1 = None
    time2 = None
    # get 1. model construction time in seconds
    line1 = [line for line in relevantLines if "Time for model construction:" in line]
    if len(line1) != 0:
        time1 = float(re.findall("\d+\.\d+", line1[0])[0])

    # get 2. model checking time in seconds
    line2 = [line for line in relevantLines if "Time for model checking:" in line]
    if len(line2) != 0:
        time2 = float(re.findall("\d+\.\d+", line2[0])[0])

    return time1, time2

def getPids(content):
    """
    This function takes a list of strings of the format
    "PID=96903 NAME=java MEM=33M" and returns all the pids
    """
    pids = []
    for line in content:
        pid = int(re.findall("PID=(\d+) ", line)[0])
        if pid not in pids:
            pids.append(pid)
    return pids

def getMaxMem(content, pids):
    """
    This function takes a list of strings of the format
    "PID=96903 NAME=java MEM=33M" and returns the maximum
    memory use for each pid
    This function returns a list of maximum memory use
    """

    # initialize maxmemdict, format is {pid : memory use}
    maxmemdict = {}
    for pid in pids:
        maxmemdict[pid] = 0

    # extract maximum memoryuse and store in maxmemdict
    for line in content:
        pid = int(re.findall("PID=(\d+) ", line)[0])
        mem_value = int(re.findall("MEM=(\d+)([MG])", line)[0][0])
        mem_unit = 1024 if re.findall("MEM=(\d+)([MG])", line)[0][1] == "G" else 1
        mem = mem_value * mem_unit
        if mem > maxmemdict[pid]:
            maxmemdict[pid] = mem

    # extract maximum memory in order
    maxmem = []
    for pid in pids:
        maxmem.append(maxmemdict[pid])

    return maxmem

def scrapMemoryUse(filename):
    """ TODO rewrite
    This function returns model construction memory in MB
    filename is a logfile generated by monitor.sh
    return type is a number
    """
    fh = open(filename, 'r')
    content = fh.readlines()

    maxmem = 0
    for line in content:
        mem_value = int(re.findall("MEM=(\d+)([MG])", line)[0][0])
        mem_unit = 1024 if re.findall("MEM=(\d+)([MG])", line)[0][1] == "G" else 1
        mem = mem_value * mem_unit
        if mem > maxmem:
            maxmem = mem


    return maxmem

def writeToTable(fh, constructiontimes, mctimes, maxmems):
    for i in range(8): # number of blocks from 3 to 10 included
        if len(constructiontimes) > i and constructiontimes[i] is not None:
            c1 = '{:8.3f}'.format(constructiontimes[i])
        else:
            c1 = '{:>8}'.format("None")
        if len(mctimes) > i and mctimes[i] is not None:
            c2 = "{:8.3f}".format(mctimes[i])
        else:
            c2 = '{:>8}'.format("None")

        if len(maxmems) > i and maxmems[i] is not None:
            c3 = "{:8d}".format(maxmems[i])
        else:
            c3 = '{:>8}'.format("None")
        fh.write("%d\t%s\t%s\t%s\n"%(i+3,c1,c2,c3))
    fh.write("\n\n")

def main(expfolder):
    """
    This function creates the information ~~~
    """
    dets = ["Det", "NonDet"]
    engines = ["mtbdd", "sparse", "hybrid", "explicit"]
    number_blocks = range(3,11)

    #################
    fh = open(expfolder+"tables.txt", "w+")
    for det in dets:
        fh.write("=========== "+det+" actions"+" ===========\n")
        expsubfolder = expfolder + det + "Results/"
        for engine in engines:
            fh.write("Engine: "+engine+"\n")
            fh.write("   consttime      mctime      maxmem\n")
            constructiontimes = []
            mctimes = []
            maxmems = []
            for nblocks in number_blocks:
                filename = expsubfolder+det+engine+"BW"+str(nblocks)+".log"
                if os.path.exists(filename):
                    constructiontime, mctime = scrapTimes(filename)
                else:
                    constructiontime, mctime = None, None
                constructiontimes.append(constructiontime)
                mctimes.append(mctime)

                filename = expsubfolder+"mem"+det+engine+"BW"+str(nblocks)+".log"
                if os.path.exists(filename):
                    maxmem = scrapMemoryUse(filename)
                else:
                    maxmem = None
                maxmems.append(maxmem)
            writeToTable(fh, constructiontimes, mctimes, maxmems)
        fh.write("\n\n")

expfolder = "Feb6/"
main(expfolder)
