#!/usr/bin/env python3

import argparse
import time
import sys
import copy
import math
import random

import numpy
import pygame

import ether


CLICK_RANDOMISE = 1
CLICK_INVERT = 2

_size = lambda s: tuple(map(int, s.split('x')))
arg_parser = argparse.ArgumentParser(description='Instructions in README.md')
arg_parser.add_argument('--size', type=_size, metavar='WIDTHxHEIGHT',
                        help='set the size of the window')
args = arg_parser.parse_args()
size = args.size or (800, 600)

ether = ether.ether(interactive=True)


pygame.init()
pygame.display.set_caption('ether')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size, depth=32)
clock = pygame.time.Clock()
font = pygame.font.SysFont('DejaVu Sans Mono, monospace', 18, bold=True)

show_stats = True
show_stats_fully = True

seed = lambda: random.randrange(2**31)

data = ether.initial_ether(size[0], size[1], seed())
diam = 200.0

def show_text(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

def render():
    global data
    step_start = time.time()
    data = ether.step(data)
    step_end = time.time()

    render_start = time.time()
    frame = ether.render(data)
    render_end = time.time()
    frame = frame.get()
    frame.shape = size
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    texts = []
    if show_stats_fully:
        texts.append('(Press h to hide/show stats)')
        if show_stats:
            texts.extend([
                ' Futhark step: {:.02f} ms'.format((step_end - step_start) * 1000),
                ' Futhark render: {:.02f} ms'.format((render_end - render_start) * 1000),
                ' Brush diameter: {:.02f}'.format(diam),
                ' FPS: {:.02f}'.format(clock.get_fps()),
            ])
        for text, i in zip(texts, range(len(texts))):
            show_text(text, (10, 10 + i * 20))

    pygame.display.flip()

leftmousedown = False
rightmousedown = False
lastinvert = time.time()
orgpos = (0,0)
while True:
    clock.tick()

    render()
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.KEYDOWN:
            if event.key == pygame.K_h:
                show_stats = not show_stats
            if event.key == pygame.K_g:
                show_stats_fully = not show_stats_fully
            if event.key == pygame.K_r:
                data = ether.shuffle_ethons(data, seed())
            elif event.key == pygame.K_q or event.key == pygame.K_ESCAPE:
                sys.exit()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                diam = diam + 1
            elif event.button == 5:
                diam = max(diam - 1, 1)
            elif event.button == 3:
                rightmousedown = True
            elif event.button == 1:
                leftmousedown = True
                orgpos = pygame.mouse.get_pos()
        elif event.type == pygame.MOUSEBUTTONUP:
            if event.button == 1:
                leftmousedown = False
            elif event.button == 3:
                rightmousedown = False
    
    if leftmousedown and rightmousedown:
        pos = pygame.mouse.get_pos()
        data = ether.colour_at(data, orgpos[0], orgpos[1], pos[0], pos[1], diam)
    elif leftmousedown:
        pos = pygame.mouse.get_pos()
        data = ether.click_at(data, pos[0], pos[1], CLICK_RANDOMISE, diam, seed())
    elif rightmousedown:
        if time.time() - lastinvert > 0.2:
            pos = pygame.mouse.get_pos()
            data = ether.click_at(data, pos[0], pos[1], CLICK_INVERT, diam, seed())
            lastinvert = time.time()
