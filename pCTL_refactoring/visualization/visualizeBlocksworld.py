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
    # im_v_resize = vconcat_resize_max(imgs)
    # cv2.imwrite('opencv_vconcat.jpg', im_v_resize)
    # cv2.imwrite('opencv_vconcat2.jpg', im_v_resize)

t = "[cl(_111918), cl(_111928), on(b, a), on(a, _111952), on(_111918, b)]"


valuefunction = \
"""
v(1.0,[on(a,b)])
v(0.999,[cl(a),cl(b),on(a,_163416)])
v(0.972,[cl(b),cl(_163644),on(b,a),on(a,_163860)])
v(0.972,[cl(b),cl(_163896),cl(_164048),on(a,_164418),on(_163896,a)])
v(0.972,[cl(a),cl(_164466),cl(_164618),on(a,_164988),on(_164466,b)])
v(0.81,[cl(a),cl(_165036),on(a,_165240),on(_165036,b)])
v(0.81,[cl(b),cl(_165288),on(a,_165492),on(_165288,a)])
v(0.7290000000000001,[cl(a),cl(_165540),on(a,_165604),on(_165604,b)])
v(0.7290000000000001,[cl(_165914),cl(_166022),on(b,a),on(a,_166186),on(_165914,b)])
v(0.7290000000000001,[cl(b),cl(_166488),cl(_166672),on(a,_167010),on(b,_166552),on(_166552,a)])
v(0.7290000000000001,[cl(_167060),cl(_167168),cl(_167476),on(a,_167332),on(_167476,a),on(_167060,b)])
v(0.7290000000000001,[cl(b),cl(_167820),cl(_168004),on(a,_168212),on(_167884,a),on(_167820,_167884)])
v(0.7290000000000001,[cl(a),cl(_168606),cl(_168790),on(a,_169152),on(_168670,b),on(_168606,_168670)])
v(0.0,[cl(_169202),cl(_169342),on(_169202,_169266)])
"""

main(valuefunction)
