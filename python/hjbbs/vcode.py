# -*- coding: utf-8 -*-
# vcode.py --- Time-stamp: <Qian Julian 2012-02-05 18:56:47>
# Copyright 2010 Julian Qian
# Author: Julian@JULIAN-PC
# Version: $Id: vcode.py,v 0.0 2010/06/11 16:06:55 Julian Exp $
# Keywords:

import Image
def img2four(image):
    width = 10
    height = 10
    left=0
    upper=0
    right=10
    lower=10

    c = 4
    while(c):
        box = (left, upper, right, lower)
        im = Image.open(image)
        region = im.crop(box)
        region.convert('L').save('crop_'+str(4-c)+'.bmp')
        left = left + 10
        right = right + 10
        c = c - 1

def printPixel(image):
    img = Image.open(image)

    for y in range(0, 10):
        for x in range(0, 10):
            print img.getpixel((x,y)),
        print
def cross(color):
    if color !=238:
        return True
    else:
        return False

def recognize(image):
    bgcolor = 238
    img = Image.open(image)

    p = img.getpixel((1,8))
    if cross(p):
        return 7

    p = img.getpixel((0,0))
    if cross(p):
        return 5

    p = img.getpixel((2,1))
    if cross(p):
        return 1

    p = img.getpixel((3,1))
    if cross(p):
        return 4

    p = img.getpixel((1,1))
    if cross(p):
        # not 1, must be 6
        return 6

    p = img.getpixel((1,7))
    if cross(p):
        return 2

    p = img.getpixel((2,5))
    if cross(p):
        return 9

    p = img.getpixel((5,4))
    if cross(p):
        # not 9, must be 0
        return 0

    p = img.getpixel((1,4))
    if cross(p):
        return 8
    else:
        return 3


def getCode(image):
    img2four(image)
    n0 = recognize('crop_0.bmp')
    n1 = recognize('crop_1.bmp')
    n2 = recognize('crop_2.bmp')
    n3 = recognize('crop_3.bmp')

    # remove crop files
    import os
    # import time
    # time.sleep(3)
    # if you want to see the temp images, sleep 3 secs
    if os.path.exists('crop_0.bmp'):
        os.remove('crop_0.bmp')

    if os.path.exists('crop_1.bmp'):
        os.remove('crop_1.bmp')

    if os.path.exists('crop_2.bmp'):
        os.remove('crop_2.bmp')

    if os.path.exists('crop_3.bmp'):
        os.remove('crop_3.bmp')

    return str(n0) + str(n1) + str(n2) + str(n3)

if __name__ == '__main__':
    print getcode('6487.bmp')
    print getcode('7771.bmp')
    print getcode('2077.bmp')
    print getcode('3519.bmp')
    print getcode('4098.bmp')
    print getcode('5756.bmp')
