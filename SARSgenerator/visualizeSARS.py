import cairo
import re
import math
import cv2
import os
from os import path
import shutil
import getopt, sys
import subprocess

class Block:
    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return self.name

class Cl:
    def __init__(self, block):
        self.block = block
    def __repr__(self):
        return "cl(" + str(self.block) +")"

class NonCl:
    def __init__(self, block):
        self.block = block
    def __repr__(self):
        return "noncl(" + str(self.block) +")"

class On:
    def __init__(self, topblock, downblock):
        self.topblock = topblock
        self.downblock = downblock
    def __repr__(self):
        return "on(" + str(self.topblock) + "," + str(self.downblock) +")"


class State:
    def __init__(self, q, move, blocks, cls, ons):
        self.q = q
        self.move = move
        self.blocks = blocks
        self.cls = cls
        self.ons = ons
        self.stacks = self.compilestack()

    def __repr__(self):
        return "Blocks:\t" + str(self.blocks) + "\n" + \
                "Cls:\t" + str(self.cls) + "\n" + \
                "Ons:\t" + str(self.ons) + "\n" + \
                "Stacks:\t" + str(self.stacks) + "\n"

    def compilestack(self):
        ons = self.ons * 1
        stacks = []
        for cl in self.cls:
            stack = [cl]
            ons, on = self.rm_topblock(ons, cl.block)
            while on is not None:
                stack.append(on)
                ons, on = self.rm_topblock(ons, on.downblock)
            stacks.append(stack)

        while ons:
            start = ons.pop(0)
            stack = [start]
            ons, on = self.rm_topblock(ons, start.downblock)
            while on is not None:
                stack.append(on)
                ons, on = self.rm_topblock(ons, on.downblock)
            ons, on = self.rm_downblock(ons, start.topblock)
            while on is not None:
                stack = [on] + stack # add to beginnning
                ons, on = self.rm_downblock(ons, on.topblock)
            stack = [NonCl(stack[0].topblock)] + stack
            stacks.append(stack)

        stacks.sort(reverse=True, key=len)
        return stacks

    def rm_topblock(self, ons, topblock):
        """
        If On(topblock, downblock) exists in ons, pops it from ons
        and returns ons and On(topblock, downblock).
        If not, return ons and None
        """
        found_on = None
        for on in ons:
            if on.topblock == topblock:
                found_on = on
                break
        if found_on is not None:
            ons.remove(found_on)
        return ons, found_on

    def rm_downblock(self, ons, downblock):
        """
        If On(topblock, downblock) exists in ons, pops it from ons
        and returns ons and On(topblock, downblock).
        If not, return ons and None
        """
        found_on = None
        for on in ons:
            if on.downblock == downblock:
                found_on = on
                break
        if found_on is not None:
            ons.remove(found_on)
        return ons, found_on

    def save_to_file(self, filename, with_frame=False):
        NUM_STACKS = len(self.stacks)
        try:
            HIGHEST_STACK_NUM = len(self.stacks[0])
        except:
            HIGHEST_STACK_NUM = 1
        BLOCK_WIDTH = 50
        IN_STACK_SPACING = 10
        TOP_SPACING = IN_STACK_SPACING * 5
        WIDTH = BLOCK_WIDTH * NUM_STACKS + IN_STACK_SPACING * (NUM_STACKS + 1)
        HEIGHT = BLOCK_WIDTH * HIGHEST_STACK_NUM + TOP_SPACING
        surface = cairo.ImageSurface(cairo.FORMAT_RGB24, WIDTH, HEIGHT)
        ctx = cairo.Context(surface)

        ##### draw background #####
        ctx.rectangle(0, 0, WIDTH, HEIGHT)
        # white background
        ctx.set_source_rgb(1, 1, 1)
        # # transparent black ??
        # ctx.set_source_rgba(0, 0, 0, 0)
        ctx.fill()


        ctx.set_source_rgb(0, 0, 0)
        ctx.set_line_width(4)
        if with_frame:
            ctx.rectangle(0, 0, WIDTH, HEIGHT)
            ctx.stroke()
        ##### draw frame #####


        ##### draw blocks #####
        ctx.set_source_rgb(0, 0, 0)
        ctx.set_line_width(2)
        ctx.select_font_face("Courier",
             cairo.FONT_SLANT_NORMAL,
             cairo.FONT_WEIGHT_NORMAL)
        for stack_index, stack in enumerate(self.stacks):
            STACK_NUM = len(stack)
            for block_index, block in enumerate(stack):
                x = BLOCK_WIDTH * stack_index + IN_STACK_SPACING * (stack_index + 1)
                y = BLOCK_WIDTH * (HIGHEST_STACK_NUM - STACK_NUM + block_index) + \
                    (TOP_SPACING - IN_STACK_SPACING)
                ctx.rectangle(x, y, BLOCK_WIDTH, BLOCK_WIDTH)
                ctx.stroke()
                if isinstance(block, Cl):
                    # draw clear block sign
                    ctx.arc(x + BLOCK_WIDTH/2, y-IN_STACK_SPACING, 5, 0, 2*math.pi)
                    ctx.stroke()
                    # draw block label
                    ctx.move_to(x + BLOCK_WIDTH/6, y + BLOCK_WIDTH/2)
                    if not block.block.startswith("_"):
                        ctx.set_source_rgb(0, 0, 1)
                        ctx.set_font_size(20)
                        ctx.show_text(block.block)
                        ctx.set_source_rgb(0, 0, 0)
                        ctx.set_font_size(10)
                    else:
                        ctx.show_text(block.block)
                elif isinstance(block, NonCl):
                    # draw block label
                    ctx.move_to(x + BLOCK_WIDTH/6, y + BLOCK_WIDTH/2)
                    if not block.block.startswith("_"):
                        ctx.set_source_rgb(0, 0, 1)
                        ctx.set_font_size(20)
                        ctx.show_text(block.block)
                        ctx.set_source_rgb(0, 0, 0)
                        ctx.set_font_size(10)
                    else:
                        ctx.show_text(block.block)

                elif isinstance(block, On):
                    # draw block label
                    ctx.move_to(x + BLOCK_WIDTH/6, y + BLOCK_WIDTH/2)
                    if not block.downblock.startswith("_"):
                        ctx.set_source_rgb(0, 0, 1)
                        ctx.set_font_size(20)
                        ctx.show_text(block.downblock)
                        ctx.set_source_rgb(0, 0, 0)
                        ctx.set_font_size(10)
                    else:
                        ctx.show_text(block.downblock)
                ctx.stroke()
        # print(str(filename))
        surface.write_to_png(filename)


        ##### draw value #####
        # text_q = "q = " + self.q
        #
        # xbearing, ybearing, width, height, dx, dy = ctx.text_extents(text_q)
        # ctx.move_to(WIDTH - width - 10, IN_STACK_SPACING)
        # ctx.show_text(text_q)
        #
        # text_move = str(self.move)
        # xbearing, ybearing, width, height, dx, dy = ctx.text_extents(text_move)
        # ctx.move_to(WIDTH - width - 10, IN_STACK_SPACING + 10)
        # ctx.show_text(text_move)


        # return surface




class SARS:
    def __init__(self, sarsTuple):
        self.s = sarsTuple[0]
        self.a = sarsTuple[1]
        self.r = sarsTuple[2]
        self.ss = sarsTuple[3]


    def __repr__(self):
        return \
        "Precondition:\n\t" + str(self.s) + "\n" + \
        "Action:\t" + str(self.a) + "\n" + \
        "Reward:\t" + str(self.r) + "\n" + \
        "Postcondition:\n\t" + str(self.ss) + "\n"

    def drawaction(self,filename):
        NUM_STACKS = 3
        try:
            HIGHEST_STACK_NUM = max(len(self.s.stacks[0]), len(self.ss.stacks[0]))
        except:
            HIGHEST_STACK_NUM = 1
        BLOCK_WIDTH = 50
        IN_STACK_SPACING = 10
        TOP_SPACING = IN_STACK_SPACING * 5
        WIDTH = BLOCK_WIDTH * NUM_STACKS + IN_STACK_SPACING * (NUM_STACKS + 1)
        HEIGHT = BLOCK_WIDTH * HIGHEST_STACK_NUM + TOP_SPACING
        surface = cairo.ImageSurface(cairo.FORMAT_RGB24, WIDTH, HEIGHT)
        ctx = cairo.Context(surface)

        ##### draw background #####
        ctx.rectangle(0, 0, WIDTH, HEIGHT)
        # white background
        ctx.set_source_rgb(1, 1, 1)
        # # transparent black ??
        # ctx.set_source_rgba(0, 0, 0, 0)
        ctx.fill()

        # ctx.rectangle(0, 0, WIDTH, HEIGHT)
        ctx.set_source_rgb(0, 0, 0)
        ctx.set_line_width(4)
        # ctx.stroke()

        # draw an arrow
        arrow_length = WIDTH
        arrow_angle = 0.0
        arrowhead_angle = math.pi/6
        arrowhead_length = WIDTH/8

        ctx.move_to(0, HEIGHT/2) # move to center of canvas

        ctx.rel_line_to(arrow_length * math.cos(arrow_angle), arrow_length * math.sin(arrow_angle))
        ctx.rel_move_to(-arrowhead_length * math.cos(arrow_angle - arrowhead_angle), -arrowhead_length * math.sin(arrow_angle - arrowhead_angle))
        ctx.rel_line_to(arrowhead_length * math.cos(arrow_angle - arrowhead_angle), arrowhead_length * math.sin(arrow_angle - arrowhead_angle))
        ctx.rel_line_to(-arrowhead_length * math.cos(arrow_angle + arrowhead_angle), -arrowhead_length * math.sin(arrow_angle + arrowhead_angle))

        ctx.set_source_rgb(0,0,0)
        ctx.set_line_width(4)
        ctx.stroke()

        # draw action and reward
        text_a = str(self.r) + " : " + str(self.a)
        xbearing, ybearing, width, height, dx, dy = ctx.text_extents(text_a)
        ctx.move_to((WIDTH - width)/2, HEIGHT/2 - 10)
        ctx.show_text(text_a)

        surface.write_to_png(filename)

    def save_to_file(self, imgfolder, filename):
        self.drawaction(imgfolder+"arrow.png")
        self.s.save_to_file(imgfolder+"s.png")
        self.ss.save_to_file(imgfolder+"ss.png")

        imgs = [cv2.imread(imgfolder+"s.png"),
                cv2.imread(imgfolder+"arrow.png"),
                cv2.imread(imgfolder+"ss.png")]
        imgs_resize = hconcat_resize_max(imgs)
        cv2.imwrite(filename, imgs_resize)



def parseSARS(rule):
    """ Rule is a string of some form of [_a-zA-Z]+(S, A, R, S').
    This function parses the given rule and returns [S, A, R, S']
    of the type [State, str, str, State]
    """
    pattern_r = re.compile("s_\((\[[_,()0-9a-zA-Z]*\])\),a_\(([_,()0-9a-zA-Z]+)\),r_\(([-0-9.]+)\),ss_\((\[[_,()0-9a-zA-Z]*\])\)")
    s, a, r, ss = re.findall(pattern_r, rule)[0]

    # Parses a state string to cl/1 and on/2 objects,
    pattern_cl = re.compile("cl\(([_a-zA-Z0-9]+)\)")
    pattern_on = re.compile("on\(([_a-zA-Z0-9]+),([_a-zA-Z0-9]+)\)")

    cl_blocks_s = re.findall(pattern_cl, s)
    ons = re.findall(pattern_on, s)
    on_blocks_s = [blo for onblocks in ons for blo in onblocks]
    blocks_s = set(cl_blocks_s + on_blocks_s)

    # create objects...
    block_objs_s = [Block(b) for b in blocks_s]
    cl_objs_s = [Cl(b) for b in cl_blocks_s]
    on_objs_s = [On(top,down) for (top,down) in ons]

    # create a state object
    state_s = State(0.0, a, block_objs_s, cl_objs_s, on_objs_s)


    cl_blocks_ss = re.findall(pattern_cl, ss)
    ons = re.findall(pattern_on, ss)
    on_blocks_ss = [blo for onblocks in ons for blo in onblocks]
    blocks_ss = set(cl_blocks_ss + on_blocks_ss)

    block_objs_ss = [Block(b) for b in blocks_ss]
    cl_objs_ss = [Cl(b) for b in cl_blocks_ss]
    on_objs_ss = [On(top,down) for (top,down) in ons]
    state_ss = State(0.0, a, block_objs_ss, cl_objs_ss, on_objs_ss)

    # pattern_move = re.compile("(move\([_a-zA-Z0-9]+,[_a-zA-Z0-9]+),")
    # movetemp = re.findall(pattern_move, state)
    # move = movetemp[0]+")" if movetemp else ""

    return [state_s, a, r, state_ss]



def parse(state):
    """Parses the string to cl/1 and on/2 objects,
    """
    pattern_q = re.compile("[a-zA-Z]+\(([0-9.]+),")
    q = re.findall(pattern_q, state)[0]

    pattern_cl = re.compile("cl\(([_a-zA-Z0-9]+)\)")
    cl_blocks = re.findall(pattern_cl, state)
    pattern_on = re.compile("on\(([_a-zA-Z0-9]+),([_a-zA-Z0-9]+)\)")
    ons = re.findall(pattern_on, state)
    on_blocks = [blo for onblocks in ons for blo in onblocks]
    blocks = set(cl_blocks + on_blocks)

    pattern_move = re.compile("(move\([_a-zA-Z0-9]+,[_a-zA-Z0-9]+),")
    movetemp = re.findall(pattern_move, state)
    move = movetemp[0]+")" if movetemp else ""


    block_objs = [Block(b) for b in blocks]
    cl_objs = [Cl(b) for b in cl_blocks]
    on_objs = [On(top,down) for (top,down) in ons]

    blocksworld = State(q, move, block_objs, cl_objs, on_objs)

    return blocksworld



def hconcat_resize_max(im_list, interpolation=cv2.INTER_CUBIC):
    h_max = max(im.shape[0] for im in im_list)
    im_list_resize = [cv2.resize(im, (int(im.shape[1] * h_max / im.shape[0]), h_max), interpolation=interpolation)
                      for im in im_list]
    return cv2.hconcat(im_list_resize)

def vconcat_resize_max(im_list, interpolation=cv2.INTER_CUBIC):
    w_max = max(im.shape[1] for im in im_list)
    im_list_resize = [cv2.resize(im, (w_max, int(im.shape[0] * w_max / im.shape[1])), interpolation=interpolation)
                      for im in im_list]
    return cv2.vconcat(im_list_resize)



def createVFImages(valuefunction, imgfolder, imgfile):

    if path.exists(imgfolder):
        shutil.rmtree(imgfolder)
    os.mkdir(imgfolder)

    # lines = valuefunction.split("\n")
    counter = 0
    vfimgs = []
    for line in valuefunction:
        if line:
            counter += 1
            sars_list = parseSARS(line)
            sars = SARS(sars_list)
            sars.save_to_file(imgfolder, imgfolder+"vf"+str(counter)+".png")
            vfimgs.append(cv2.imread(imgfolder+"vf"+str(counter)+".png"))
    imgs_resize = vconcat_resize_max(vfimgs)
    cv2.imwrite(imgfile, imgs_resize)



def drawseq():
    folder = "tempfigures/"
    if path.exists(folder):
        shutil.rmtree(folder)
    os.mkdir(folder)

    lines = valuefunction.split("\n")
    counter = 0
    imgs = []
    for line in lines:
        if line:
            filename = folder+"img"+str(counter)+".png"
            counter += 1
            blocksworld = parse(line)
            surface = blocksworld.save_to_file()
            surface.write_to_png(filename)
            imgs.append(cv2.imread(filename))


def getlastvaluefunction(content):
    content = content.replace("\n", "|")
    pattern_vf = re.compile(r'## value function ##([^#]*)########')
    # capture the last value function
    lastvf = re.findall(pattern_vf, content)[-1].replace("|","\n")
    # remove irrelevant lines
    lastvf = lastvf.split("\n")[1:-2]
    return lastvf

def getlastvaluefunctionSARS(content):
    content = content.replace("\n", "|")
    pattern_vf = re.compile(r'## value function with action ##([^#]*)########')
    # capture the last value function
    lastvf = re.findall(pattern_vf, content)[-1].replace("|","\n")
    # remove irrelevant lines
    lastvf = lastvf.split("\n")[1:-2]
    return lastvf



def partitions(n):
	# base case of recursion: zero is the sum of the empty list
	if n == 0:
		yield []
		return

	# modify partitions of n-1 to form partitions of n
	for p in partitions(n-1):
		yield [1] + p
		if p and (len(p) < 2 or p[1] > p[0]):
			yield [p[0] + 1] + p[1:]


def cl(blockNum):
    return "cl(V"+str(blockNum)+")"

def on(blockNum):
    return "on(V"+str(blockNum-1)+", V"+str(blockNum)+")"


def generateStructuresProlog(domain, filename):
    """Given a domain,
       Creates all blocksworld ground structures
       and output to the given filename
    """
    # counter = 0
    states = []
    for parti in partitions(len(domain)):
        parti.sort(reverse = True)
        # counter += 1
        # print("partition " + str(counter) + " " + str(parti))
        cls = []
        ons = []
        blockNum = 1
        for stack in parti:
            cls.append(cl(blockNum))
            blockNum += 1
            for stackcounter in range(1,stack):
                ons.append(on(blockNum))
                blockNum += 1
        state = cls + ons
        state = "[" + ", ".join(state) + "]"
        states.append(state)

    structurestext = "[" + ", \n    ".join(states) + "]"
    diff = []
    for i in range(1,len(domain)+1):
        for j in range(i+1, len(domain)+1):
            diff.append("dif(V"+str(i)+", V"+str(j)+")")
    difftext = ", ".join(diff)

    text = "allgroundstructures(\n    "+structurestext+\
           "):- \n    "+difftext+".\n"

    domaintext = "domain([" + ", ".join(domain) +"]).\n\n"
    with open(filename, "w") as f:
        f.write(":- module(allgroundstructures, [allgroundstructures/1, domain/1]).\n\n")
        f.write(domaintext)
        f.write(text)




def parseState(rule):
    pattern_r = re.compile("(\[[_,()0-9a-zA-Z]*\])")
    s = re.findall(pattern_r, rule)[0]

    # Parses a state string to cl/1 and on/2 objects,
    pattern_cl = re.compile("cl\(([_a-zA-Z0-9]+)\)")
    pattern_on = re.compile("on\(([_a-zA-Z0-9]+),([_a-zA-Z0-9]+)\)")

    cl_blocks_s = re.findall(pattern_cl, s)
    ons = re.findall(pattern_on, s)
    on_blocks_s = [blo for onblocks in ons for blo in onblocks]
    blocks_s = set(cl_blocks_s + on_blocks_s)

    # create objects...
    block_objs_s = [Block(b) for b in blocks_s]
    cl_objs_s = [Cl(b) for b in cl_blocks_s]
    on_objs_s = [On(top,down) for (top,down) in ons]

    # create a state object
    state_s = State(0.0, " ", block_objs_s, cl_objs_s, on_objs_s)

    return state_s

def allgroundstates(domain=["a","b","c","d","e"]):
    """Given a domain, creates all ground states.
    """
    expFolder = "experiments/"

    # if path.exists(expFolder):
    #     shutil.rmtree(expFolder)
    if not path.exists(expFolder):
        os.mkdir(expFolder)

    generateStructuresProlog(domain, "allgroundstates.pl")
    task = ["allInterpretations", ['swipl','-g','allInterpretations','-g','halt','simulate.pl']]
    name = task[0]
    expargs = task[1]

    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()

    ### create images
    with open(outputfilename, 'r') as f:
        content = f.read()

    states = content.split("\n")[:-2]

    # print(last_vf)
    imgfolder = "tempfigures/"

    if path.exists(imgfolder):
        shutil.rmtree(imgfolder)
    os.mkdir(imgfolder)
    statesfile = expFolder+name+".png" # without file extension


    # lines = valuefunction.split("\n")
    counter = 0
    vfimgs = []
    for line in states:
        if line:
            counter += 1
            state = parseState(line)
            state.save_to_file(imgfolder+"gs"+str(counter)+".png", with_frame=True)
            vfimgs.append(cv2.imread(imgfolder+"gs"+str(counter)+".png"))
    imgs_resize = hconcat_resize_max(vfimgs)
    cv2.imwrite(statesfile, imgs_resize)




def main_run_simulate_old():
    """ This creates all SARS pairs (with bugs)
    """

    expFolder = "experiments/"

    # if path.exists(expFolder):
    #     shutil.rmtree(expFolder)
    # # if not path.exists(expFolder):
    # os.mkdir(expFolder)
    #
    # task = ["simulate", ['swipl','-g','p','-g','halt','simulate.pl']]
    # name = task[0]
    # expargs = task[1]
    #
    # outputfilename = expFolder+name+".txt"
    # with open(outputfilename, 'w') as f:
    #     process = subprocess.Popen(expargs, stdout=f)
    #     process.communicate()
    #
    # with open(outputfilename, 'r') as f:
    #     content = f.read()
    # last_vf = getlastvaluefunctionSARS(content)
    # # print(last_vf)
    # imgfolder = "tempfigures/"
    #
    # sarsfile = expFolder+name+".png" # without file extension
    # createVFImages(last_vf, imgfolder, sarsfile)

    content = [0,0,0,0]
    content[0] = """## value function with action ##
partialQ(s_([cl(a),cl(b),on(a,_29126)]),a_(move(a,b,_29126)),r_(7.2),ss_([on(a,b)]))
partialQ(s_([cl(_29302),cl(_29486),on(a,b),on(_29302,_29366)]),a_(move(_29302,_29486,_29366)),r_(7.2),ss_([on(a,b)]))
partialQ(s_([cl(_29746),cl(_29886),on(_29746,_29810)]),a_(move(_29746,_29886,_29810)),r_(-1.8),ss_([]))
Number of abstract states: 3
########"""


    content[1] = """## value function with action ##
partialQ(s_([cl(a),cl(_39644),on(a,b)]),a_(move(a,_39644,b)),r_(0.8),ss_([on(a,b)]))
partialQ(s_([cl(_39820),cl(_40004),on(a,b),on(_39820,_39884)]),a_(move(_39820,_40004,_39884)),r_(0.8),ss_([on(a,b)]))
partialQ(s_([cl(_40196),cl(_40336),on(_40196,_40260)]),a_(move(_40196,_40336,_40260)),r_(-0.2),ss_([]))
Number of abstract states: 3
########"""


    content[2] = """## value function with action ##
partialQ(s_([cl(a),cl(b),on(a,_171892)]),a_(move(a,b,_171892)),r_(7.2),ss_([on(a,b)]))
partialQ(s_([cl(_172068),cl(_172252),on(a,b),on(_172068,_172132)]),a_(move(_172068,_172252,_172132)),r_(7.2),ss_([on(a,b)]))
partialQ(s_([cl(a),cl(_172512),on(a,b)]),a_(move(a,_172512,b)),r_(4.5),ss_([cl(a),cl(b),on(a,_172512)]))
partialQ(s_([cl(a),cl(b),cl(_172884),on(a,_172948)]),a_(move(a,_172884,_172948)),r_(4.5),ss_([cl(a),cl(b),on(a,_172884)]))
partialQ(s_([cl(b),cl(_173386),on(b,a),on(a,_173538)]),a_(move(b,_173386,a)),r_(4.5),ss_([cl(a),cl(b),on(a,_173538)]))
partialQ(s_([cl(a),cl(b),cl(_173790),on(a,_174104),on(b,_173898)]),a_(move(b,_173790,_173898)),r_(4.5),ss_([cl(a),cl(b),on(a,_174104)]))
partialQ(s_([cl(a),cl(_174368),cl(_174520),on(a,_174682),on(_174368,b)]),a_(move(_174368,_174520,b)),r_(4.5),ss_([cl(a),cl(b),on(a,_174682)]))
partialQ(s_([cl(b),cl(_174946),cl(_175098),on(a,_175260),on(_174946,a)]),a_(move(_174946,_175098,a)),r_(4.5),ss_([cl(a),cl(b),on(a,_175260)]))
partialQ(s_([cl(a),cl(b),cl(_175524),cl(_175840),on(a,_176068),on(_175524,_175676)]),a_(move(_175524,_175840,_175676)),r_(4.5),ss_([cl(a),cl(b),on(a,_176068)]))
partialQ(s_([cl(_176332),cl(_176472),on(_176332,_176396)]),a_(move(_176332,_176472,_176396)),r_(-1.8),ss_([]))
Number of abstract states: 10
########"""

    content[3] = """## value function with action ##
partialQ(s_([cl(a),cl(_296918),on(a,b)]),a_(move(a,_296918,b)),r_(0.8),ss_([on(a,b)]))
partialQ(s_([cl(_297094),cl(_297278),on(a,b),on(_297094,_297158)]),a_(move(_297094,_297278,_297158)),r_(0.8),ss_([on(a,b)]))
partialQ(s_([cl(a),cl(b),on(a,_297470)]),a_(move(a,b,_297470)),r_(0.5),ss_([cl(a),cl(b),on(a,_297470)]))
partialQ(s_([cl(a),cl(b),cl(_297842),on(a,_297950)]),a_(move(a,_297842,_297950)),r_(0.5),ss_([cl(a),cl(b),on(a,_297950)]))
partialQ(s_([cl(a),cl(b),on(a,_298484),on(b,_298344)]),a_(move(b,a,_298344)),r_(0.5),ss_([cl(a),cl(b),on(a,_298484)]))
partialQ(s_([cl(a),cl(b),cl(_298748),on(a,_299018),on(_298748,_298856)]),a_(move(_298748,b,_298856)),r_(0.5),ss_([cl(a),cl(b),on(a,_299018)]))
partialQ(s_([cl(a),cl(b),cl(_299282),on(a,_299552),on(_299282,_299390)]),a_(move(_299282,a,_299390)),r_(0.5),ss_([cl(a),cl(b),on(a,_299552)]))
partialQ(s_([cl(a),cl(b),cl(_299816),on(a,_300086),on(b,_299924)]),a_(move(b,_299816,_299924)),r_(0.5),ss_([cl(a),cl(b),on(a,_300086)]))
partialQ(s_([cl(a),cl(b),cl(_300350),cl(_300578),on(a,_300806),on(_300350,_300502)]),a_(move(_300350,_300578,_300502)),r_(0.5),ss_([cl(a),cl(b),on(a,_300806)]))
partialQ(s_([cl(_301070),cl(_301210),on(_301070,_301134)]),a_(move(_301070,_301210,_301134)),r_(-0.2),ss_([]))
Number of abstract states: 10
########"""
    for i in range(4):
        last_vf = getlastvaluefunctionSARS(content[i])
        # print(last_vf)
        imgfolder = "tempfigures/"

        sarsfile = expFolder+"test"+str(i)+".png" # without file extension
        createVFImages(last_vf, imgfolder, sarsfile)

if __name__ == '__main__':
    allgroundstates()
    # main_run_simulate_old()
