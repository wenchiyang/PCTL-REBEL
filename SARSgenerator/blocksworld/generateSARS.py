import run
from visualization import visualizeSARS


def ground(ground_pl_file):

    task = ["ground", ['swipl','-g','ground','-g','halt',ground_pl_file]]


    name = task[0]
    taskargs = task[1]
    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(taskargs, stdout=f)
        process.communicate()


if __name__ == '__main__':

    # parameters
    expFolder = "experiments/"
    ground_pl_file = expFolder + "sars.pl"
    domain_objs = "[a,b]"

    # run the model checker
    run.run()
    # learn pl file
    with open(expFolder+"exp1.txt", 'r') as f:
        content = f.read()
        last_vf = visualizeSARS.getlastvaluefunctionSARS(content)

        with open(ground_pl_file, 'w') as f:
            f.write(":- use_module(\"../util\").\n\n")
            for line in last_vf:
                # f.write(line+".\n")
                sars_list = visualizeSARS.parseSARS(line)
                sars = visualizeSARS.SARS(sars_list)
                f.write("vf("+ str(sars.s.cls + sars.s.ons) + "," + \
                                str(sars.a) + "," + \
                                str(sars.r) + "," + \
                                str(sars.ss.cls + sars.ss.ons) + "):-\n    ")

                dif_pairs = set()
                for i, block in enumerate(sars.s.blocks):
                    for j in sars.s.blocks[i+1:]:
                        dif_pairs.add((block, j))
                dif_str = str(dif_pairs)[1:-1].replace("(", "dif(")
                f.write(dif_str+".\n")
                # for pair in dif_pairs:

                # f.write(".\n")
            f.write("domain("+domain_objs+").\n\n")
            f.write(
"""
groundtask(S):-
     domain(Blocks),
     vf(S,A,R,SS),
     termsInState(S, Terms),
     append(Blocks,Terms, NewTerms),
     generateOIstate(NewTerms, _).
     % thetasubsumes(Block, S).
"""
            )




    # ground with domain
    # ground(ground_pl_file)
