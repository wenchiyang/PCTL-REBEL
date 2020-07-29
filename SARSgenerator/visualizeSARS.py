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

    def save_to_file(self, filename):
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

        # ctx.rectangle(0, 0, WIDTH, HEIGHT)
        ctx.set_source_rgb(0, 0, 0)
        ctx.set_line_width(4)
        # ctx.stroke()
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



def main(valuefunction, imgfolder, imgfile):


    # if path.exists(imgfolder):
    #     shutil.rmtree(imgfolder)
    # os.mkdir(imgfolder)

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


if __name__ == '__main__':
    expFolder = "experiments/"

    # if path.exists(expFolder):
    #     shutil.rmtree(expFolder)
    # if not path.exists(expFolder):
    # os.mkdir(expFolder)

    task = ["simulate", ['swipl','-g','p','-g','halt','simulate.pl']]
    name = task[0]
    expargs = task[1]

    outputfilename = expFolder+name+".txt"
    with open(outputfilename, 'w') as f:
        process = subprocess.Popen(expargs, stdout=f)
        process.communicate()

    with open(outputfilename, 'r') as f:
        content = f.read()

    last_vf = getlastvaluefunctionSARS(content)
    # print(last_vf)
    imgfolder = "tempfigures/"


    sarsfile = expFolder+name+".png" # without file extension
    main(last_vf, imgfolder, sarsfile)
