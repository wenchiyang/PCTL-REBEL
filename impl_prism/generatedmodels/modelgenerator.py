def alldif(x,y,z):
    return x!=y and y!=z and z!=x

def generateVariables(n):
    """
    This function generates variables needed in the ground blocksworld model with n blocks.
    return type: string
    """
    res = ""
    # initialize cl variables
    for i in range(1, n+1):
        res += "    cl%d : [0..1] init 1;\n"%(i)
    res += "\n"
    # initialize on variables
    res += "    // block initially on floor\n"
    for i in range(1, n+1):
        # block initially on floor
        res += "    on%d0 : [0..1] init 1;\n"%(i)
        for j in range(1, n+1):
            if j != i:
                res += "    on%d%d : [0..1] init 0;\n"%(i,j)
        res += "\n"
    res += "\n"
    return res

def generateDeterministicRules(n):
    """
    This function generates determinitic ground action rules for n blocks.
    return type: string
    """
    res = "    // clX=1 & clY=1 onXZ=1 -> (clY'=0) & (onXZ'=0) & (clZ'=1) & (onXY'=1); // (X, Y, Z) \n\n"
    for X in range(1, n+1):
        for Y in range(0, n+1):
            for Z in range(0, n+1):
                if alldif(X,Y,Z):
                    x = str(X)
                    y = str(Y)
                    z = str(Z)
                    post1 = " (cl" + y + "'=0) &" if Y != 0 else "           "
                    post3 = " (cl" + z + "'=1) &" if Z != 0 else "           "
                    line = "    [] cl" + x + "=1 & cl"+ y +"=1 & on"+ x + z +"=1 ->" +\
                           post1 + " (on"+x+z+"'=0) &" + post3 + " (on" + x + y + "'=1);"+\
                           " // (" + x + ", " + y + ", " + z + ") \n"

                    res += line
        res += "\n"
    return res

def generateNondeterministicRules(n):
    """
    This function generates determinitic ground action rules for n blocks.
    return type: string
    """
    res = "    // clX=1 & clY=1 onXZ=1 -> (clY'=0) & (onXZ'=0) & (clZ'=1) & (onXY'=1); // (X, Y, Z) \n\n"
    for X in range(1, n+1):
        for Y in range(0, n+1):
            for Z in range(0, n+1):
                if alldif(X,Y,Z):
                    x = str(X)
                    y = str(Y)
                    z = str(Z)
                    post1 = " (cl" + y + "'=0) &" if Y != 0 else "           "
                    post3 = " (cl" + z + "'=1) &" if Z != 0 else "           "
                    line = "    [] cl" + x + "=1 & cl"+ y +"=1 & on"+ x + z +"=1 ->" +\
                           " 0.9:" +\
                           post1 + " (on"+x+z+"'=0) &" + post3 + " (on" + x + y + "'=1) +"+\
                           " 0.1: (cl" + x + "' = cl" + x + ");"\
                           " // (" + x + ", " + y + ", " + z + ") \n"


                    res += line
        res += "\n"
    return res

def generateModel(filename, n , det):
    """
    This function generates a ground blocks world model with n blocks.
    The model is saved in the given filename.
    The action rules can be deterministic if det == true, otherwise they are
    non-deterministic.
    """

    fh = open(filename, 'w')
    fh.writelines([
        "mdp\n\n\n",
        "// with %s blocks \n\n" %n,
        "const cl0 = 1; // floor is always clear\n\n",
        "module blocksworld\n"
    ])

    fh.write(generateVariables(n))
    # move/3 rules
    if det:
        fh.write(generateDeterministicRules(n))
    else:
        fh.write(generateNondeterministicRules(n))
    fh.write("endmodule\n\n")
    # goal state : on(a,b)
    fh.write("label \"goal\" = on12=1 ;")
    fh.close()


for i in range(3,11):
    generateModel("detBW%d.nm"%i, i, True)
    generateModel("nondetBW%d.nm"%i, i, False)
