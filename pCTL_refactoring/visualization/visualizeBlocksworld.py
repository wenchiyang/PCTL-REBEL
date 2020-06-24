import cairo
import re
import math
import cv2
import os
from os import path
import shutil


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


class BlocksWorld:
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

    def save_to_file(self):
        NUM_STACKS = len(self.stacks)
        HIGHEST_STACK_NUM = len(self.stacks[0])
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

        ctx.rectangle(0, 0, WIDTH, HEIGHT)
        ctx.set_source_rgb(0, 0, 0)
        ctx.set_line_width(4)
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
                    ctx.show_text(block.block)
                elif isinstance(block, NonCl):
                    # draw block label
                    ctx.move_to(x + BLOCK_WIDTH/6, y + BLOCK_WIDTH/2)
                    ctx.show_text(block.block)
                elif isinstance(block, On):
                    # draw block label
                    ctx.move_to(x + BLOCK_WIDTH/6, y + BLOCK_WIDTH/2)
                    ctx.show_text(block.downblock)
                ctx.stroke()
        ##### draw value #####
        text_q = "q = " + self.q

        xbearing, ybearing, width, height, dx, dy = ctx.text_extents(text_q)
        ctx.move_to(WIDTH - width - 10, IN_STACK_SPACING)
        ctx.show_text(text_q)

        text_move = str(self.move)
        xbearing, ybearing, width, height, dx, dy = ctx.text_extents(text_move)
        ctx.move_to(WIDTH - width - 10, IN_STACK_SPACING + 10)
        ctx.show_text(text_move)


        return surface


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

    blocksworld = BlocksWorld(q, move, block_objs, cl_objs, on_objs)

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


def main(valuefunction):
    # state1 = "v(0.972,[cl(a),cl(_135230),cl(b),on(a,a),on(a,_135294),on(_135230,_135326)])"
    # blocksworld1 = parse(state1)
    # surface = blocksworld1.save_to_file()
    # surface.write_to_png("rectangle.png")
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

    im_h_resize = hconcat_resize_max(imgs)
    cv2.imwrite('opencv_hconcat.jpg', im_h_resize)
    # cv2.imwrite('opencv_hconcat2.jpg', im_h_resize)
    # im_v_resize = vconcat_resize_max(imgs)
    # cv2.imwrite('opencv_vconcat.jpg', im_v_resize)
    # cv2.imwrite('opencv_vconcat2.jpg', im_v_resize)

valuefunction = \
"""
partialQ(0.1,move(_368924,_369108,_368988),[cl(_368924),cl(_369108),on(a,b),on(_368924,_368988)])
partialQ(0.0999,move(_374074,_374302,_374226),[cl(a),cl(b),cl(_374074),cl(_374302),on(a,_374530),on(_374074,_374226)])
partialQ(0.09720000000000001,move(_385860,_386000,_385924),[cl(b),cl(_385860),cl(_386000),on(b,a),on(a,_386714),on(_385860,_385924)])
partialQ(0.09720000000000001,move(_391528,_391668,_391592),[cl(b),cl(_391668),cl(_391528),on(a,_392838),on(_391668,a),on(_391528,_391592)])
partialQ(0.09720000000000001,move(_394608,_394748,_394672),[cl(a),cl(_394748),cl(_394608),on(a,_395918),on(_394748,b),on(_394608,_394672)])
partialQ(0.09720000000000001,move(_396160,_396300,_396224),[cl(b),cl(_396408),cl(_396160),cl(_396300),on(a,_397600),on(_396408,a),on(_396160,_396224)])
partialQ(0.09720000000000001,move(_397842,_397982,_397906),[cl(a),cl(_398090),cl(_397842),cl(_397982),on(a,_399282),on(_398090,b),on(_397842,_397906)])
partialQ(0.0729,move(_403814,_404206,_403878),[cl(_404206),cl(_403814),on(b,a),on(a,_403910),on(_404206,b),on(_403814,_403878)])
partialQ(0.0729,move(_410624,_410752,_410688),[cl(a),cl(_410624),cl(_410752),on(a,_410688),on(_410688,b),on(_410624,_410688)])
partialQ(0.0729,move(_429724,_429852,_429788),[cl(b),cl(_429724),cl(_429852),on(a,_432046),on(_429788,a),on(_429724,_429788)])
partialQ(0.0729,move(_437368,_437760,_437432),[cl(_437824),cl(_437368),cl(_437760),on(b,a),on(a,_437464),on(_437824,b),on(_437368,_437432)])
partialQ(0.0729,move(_441944,_442084,_442008),[cl(b),cl(_441944),cl(_442084),on(a,_444106),on(b,_442160),on(_442160,a),on(_441944,_442008)])
partialQ(0.0729,move(_444360,_444752,_444424),[cl(_444752),cl(_444872),cl(_444360),on(a,_444456),on(_444752,a),on(_444872,b),on(_444360,_444424)])
partialQ(0.0729,move(_446856,_447248,_446920),[cl(_448292),cl(_447248),cl(_446856),on(a,_446952),on(_448292,a),on(_447248,b),on(_446856,_446920)])
partialQ(0.0729,move(_457148,_457540,_457212),[cl(b),cl(_457540),cl(_457148),on(a,_457244),on(_457616,a),on(_457540,_457616),on(_457148,_457212)])
partialQ(0.0729,move(_462396,_462536,_462460),[cl(a),cl(_462536),cl(_462396),on(a,_464738),on(_462612,b),on(_462536,_462612),on(_462396,_462460)])
partialQ(0.0729,move(_464992,_465492,_465056),[cl(a),cl(_464992),cl(_465120),cl(_465492),on(a,_467444),on(_465056,b),on(_464992,_465056)])
partialQ(0.0729,move(_467686,_468078,_467750),[cl(_469142),cl(_468142),cl(_467686),cl(_468078),on(a,_467782),on(_469142,a),on(_468142,b),on(_467686,_467750)])
partialQ(0.0729,move(_470324,_470716,_470388),[cl(b),cl(_470824),cl(_470324),cl(_470716),on(a,_470420),on(_470888,a),on(_470824,_470888),on(_470324,_470388)])
partialQ(0.0729,move(_473130,_473270,_473194),[cl(a),cl(_473378),cl(_473130),cl(_473270),on(a,_475602),on(_473442,b),on(_473378,_473442),on(_473130,_473194)])
partialQ(0.0,move(_475856,_475984,_475920),[cl(_475856),cl(_475984),on(_475856,_475920)])
"""


main(valuefunction)
