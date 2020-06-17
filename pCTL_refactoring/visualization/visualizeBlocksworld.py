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
    def __init__(self, q, blocks, cls, ons):
        self.q = q
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
        WIDTH = BLOCK_WIDTH * NUM_STACKS + IN_STACK_SPACING * (NUM_STACKS + 1)
        HEIGHT = BLOCK_WIDTH * HIGHEST_STACK_NUM + IN_STACK_SPACING * 4
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
                y = BLOCK_WIDTH * (HIGHEST_STACK_NUM - STACK_NUM + block_index) + IN_STACK_SPACING * 3
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
        xbearing, ybearing, width, height, dx, dy = ctx.text_extents("v = " + self.q)
        ctx.move_to(WIDTH - width - 10, IN_STACK_SPACING)
        ctx.show_text("v = " + self.q)

        return surface


def parse(state):
    """Parses the string to cl/1 and on/2 objects,
    """
    pattern_q = re.compile("v\(([0-9.]+),")
    q = re.findall(pattern_q, state)[0]
    pattern_cl = re.compile("cl\(([_a-zA-Z0-9]+)\)")
    cl_blocks = re.findall(pattern_cl, state)
    pattern_on = re.compile("on\(([_a-zA-Z0-9]+),([_a-zA-Z0-9]+)\)")
    ons = re.findall(pattern_on, state)
    on_blocks = [blo for onblocks in ons for blo in onblocks]
    blocks = set(cl_blocks + on_blocks)

    block_objs = [Block(b) for b in blocks]
    cl_objs = [Cl(b) for b in cl_blocks]
    on_objs = [On(top,down) for (top,down) in ons]

    blocksworld = BlocksWorld(q, block_objs, cl_objs, on_objs)

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
    cv2.imwrite('opencv_hconcat2.jpg', im_h_resize)


t = "[cl(_111918), cl(_111928), on(b, a), on(a, _111952), on(_111918, b)]"


valuefunction = \
"""
v(1.0,[on(a,b)])
v(0.9999,[cl(a),cl(b),on(a,_312838)])
v(0.9963,[cl(b),cl(_313154),on(b,a),on(a,_313458)])
v(0.9963,[cl(b),cl(_313494),cl(_313646),on(a,_314160),on(_313494,a)])
v(0.9963,[cl(a),cl(_314208),cl(_314360),on(a,_314874),on(_314208,b)])
v(0.9477,[cl(_314912),cl(_315108),on(b,a),on(a,_314976),on(_314912,b)])
v(0.9477,[cl(a),cl(_315882),cl(_316530),on(a,_315946),on(_315946,b)])
v(0.9477,[cl(b),cl(_316860),cl(_317572),on(a,_317810),on(b,_316924),on(_316924,a)])
v(0.9477,[cl(_317860),cl(_318056),cl(_318252),on(a,_317924),on(_318252,a),on(_317860,b)])
v(0.9477,[cl(b),cl(_319148),cl(_319572),on(a,_319212),on(_319244,a),on(_319148,_319244)])
v(0.9477,[cl(a),cl(_320478),cl(_320870),on(a,_321468),on(_320586,b),on(_320478,_320586)])
v(0.6561000000000001,[cl(a),cl(_321528),on(a,_321592),on(_321528,b)])
v(0.6561000000000001,[cl(b),cl(_322200),on(a,_322824),on(b,_322264),on(_322200,a)])
v(0.6561000000000001,[cl(b),cl(_322884),on(a,_323156),on(_322948,a),on(_322884,_322948)])
v(0.6561000000000001,[cl(_323726),cl(_324086),on(b,a),on(a,_324394),on(_323790,b),on(_323726,_323790)])
v(0.6561000000000001,[cl(_324874),cl(_324982),cl(_325322),on(a,_325146),on(_324874,a),on(_325146,b)])
v(0.6561000000000001,[cl(a),cl(_325874),cl(_326290),on(a,_325938),on(_326014,b),on(_325938,_326014)])
v(0.6561000000000001,[cl(_326890),cl(_326998),cl(_327482),on(a,_327338),on(b,_327162),on(_327162,a),on(_326890,b)])
v(0.6561000000000001,[cl(_328060),cl(_328364),cl(_328760),on(a,_328616),on(_328760,a),on(_328124,b),on(_328060,_328124)])
v(0.6561000000000001,[cl(_329270),cl(_329410),cl(_329806),on(a,_329662),on(_329334,a),on(_329806,b),on(_329270,_329334)])
v(0.6561000000000001,[cl(b),cl(_330490),cl(_330906),on(a,_331476),on(b,_330554),on(_330630,a),on(_330554,_330630)])
v(0.6561000000000001,[cl(b),cl(_331548),cl(_331764),on(a,_332104),on(_331688,a),on(_331548,_331612),on(_331612,_331688)])
v(0.6561000000000001,[cl(a),cl(_332882),cl(_333098),on(a,_333964),on(_333022,b),on(_332882,_332946),on(_332946,_333022)])
v(0.0729,[cl(a),cl(_334036),on(a,_334100),on(_334100,b)])
v(0.0729,[cl(b),cl(_334708),on(a,_335332),on(b,_334772),on(_334772,a)])
v(0.0729,[cl(_335382),cl(_335578),on(a,_335446),on(_335578,a),on(_335382,b)])
v(0.0,[cl(_336154),cl(_336294),on(_336154,_336218)])
"""

main(valuefunction)
