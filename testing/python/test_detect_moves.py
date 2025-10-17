#!/usr/bin/env python

from dataclasses import dataclass
from typing import List, TypeVar, Sequence
import logging
from collections import defaultdict
import pytest

logging.basicConfig(
    level=logging.DEBUG,
    format="%(asctime)s - %(filename)s:%(lineno)d - %(levelname)s - %(message)s"
)

logger = logging.getLogger()

T = TypeVar('T')

import copy


@dataclass
class SeqMove:
    srcIndex: int
    dstIndex: int
    count: int


def apply_move(src: List[T], move: SeqMove) -> None:
    logger.info(f"apply move {move}")
    logger.info(f"src = {src}")
    block = src[move.srcIndex:move.srcIndex + move.count]
    del src[move.srcIndex:move.srcIndex + move.count]
    dest_idx = move.dstIndex
    src[dest_idx:dest_idx] = block
    logger.info(f"src = {src} dest_idx = {dest_idx} block = {block}")


def detect_moves(src: List[T], dst: List[T]) -> List[SeqMove]:
    src = copy.copy(src)
    n = len(src)
    if len(dst) != n:
        raise ValueError("Sequences must have the same length")

    moves = []

    isrc = 0
    idst = 0
    maxCount = 0
    while isrc < len(src):
        isrcStart = isrc
        if src[isrc] == dst[isrc]:
            isrc += 1
            idst += 1
            continue

        itmp = idst
        while itmp < len(dst) and dst[itmp] != src[isrc]:
            logger.info(f"dst[{itmp}] {dst[itmp]} ?? {src[isrc]}")
            itmp += 1

        logger.info(f"itmp = {itmp} idst = {idst} len(dst) = {len(dst)}")

        assert itmp < len(
            dst
        ), f"could not find match for source item src[{isrc}] = {src[isrc]}, started from {idst}, no much on {itmp}"

        if itmp < len(dst):
            count = 0
            while itmp + count < len(dst) and dst[itmp + count] == src[isrc +
                                                                       count]:
                logger.info(
                    f"dst[{itmp}] {dst[itmp]} == src[{isrc}] = {src[isrc]}")
                count += 1

            move = SeqMove(srcIndex=isrc, dstIndex=itmp, count=count)
            apply_move(src, move)
            moves.append(move)
            isrc += count
            idst = move.srcIndex
            logger.info(f"src = {src} dst = {dst}")
            logger.info(f"found move {move} resetting idst = {move.srcIndex}")

        maxCount += 1
        assert isrcStart != isrc

    assert src == dst

    return moves


def test_apply_move():
    src = [1, 2, 3, 4, 5]
    apply_move(src, SeqMove(0, 4, 1))
    assert src == [2, 3, 4, 5, 1]


def test_apply_move_1():
    src = [1, 2, 3, 4, 5]
    apply_move(src, SeqMove(4, 0, 1))
    assert src == [5, 1, 2, 3, 4]


def test_apply_move_2():
    src = [1, 2, 3, 4, 5]
    apply_move(src, SeqMove(0, 2, 2))
    assert src == [3, 4, 1, 2, 5]


def test_apply_move_3():
    src = [1, 2, 3, 4, 5]
    apply_move(src, SeqMove(2, 0, 2))
    assert src == [3, 4, 1, 2, 5]


def test_apply_move_4():
    src = [1, 2, 3, 4, 5, 6]
    apply_move(src, SeqMove(1, 4, 2))
    assert src == [1, 4, 5, 6, 2, 3]


def test_apply_move_5():
    src = [1]
    apply_move(src, SeqMove(0, 0, 1))
    assert src == [1]


def test_apply_move_6():
    src = [1, 2, 3, 4, 5]
    apply_move(src, SeqMove(0, 5, 5))
    assert src == [1, 2, 3, 4, 5]


def test_apply_move_7():
    src = [1, 2, 3, 4, 5, 6, 7, 8]
    apply_move(src, SeqMove(1, 3, 3))
    assert src == [1, 5, 6, 2, 3, 4, 7, 8]


def test_empty_sequences():
    src = []
    dst = []
    moves = detect_moves(src, dst)
    assert len(moves) == 0


def test_single_element_no_move():
    src = [1]
    dst = [1]
    moves = detect_moves(src, dst)
    assert len(moves) == 0


def test_single_element_move():
    src = [1, 2]
    dst = [2, 1]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 0
    assert moves[0].dstIndex == 1
    assert moves[0].count == 1


def test_no_moves_identical_sequences():
    src = [1, 2, 3, 4, 5]
    dst = [1, 2, 3, 4, 5]
    moves = detect_moves(src, dst)
    assert len(moves) == 0


def test_simple_move():
    src = [1, 2, 3, 4]
    dst = [2, 1, 3, 4]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 0
    assert moves[0].dstIndex == 1
    assert moves[0].count == 1


def test_move_to_end():
    src = [1, 2, 3, 4]
    dst = [2, 3, 4, 1]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 0
    assert moves[0].dstIndex == 3
    assert moves[0].count == 1


def test_move_from_end():
    src = [1, 2, 3, 4]
    dst = [4, 1, 2, 3]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 0
    assert moves[0].dstIndex == 1
    assert moves[0].count == 3


def test_multiple_moves():
    src = [1, 2, 3, 4, 5, 6]
    dst = [5, 6, 3, 4, 1, 2]
    moves = detect_moves(src, dst)
    assert len(moves) == 2


def test_duplicate_elements():
    src = [1, 1, 2, 2]
    dst = [2, 2, 1, 1]
    moves = detect_moves(src, dst)
    assert len(moves) == 1


def test_duplicate_elements_partial_move():
    src = [1, 1, 2, 3]
    dst = [1, 2, 1, 3]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 1
    assert moves[0].dstIndex == 2
    assert moves[0].count == 1


def test_all_same_elements():
    src = [1, 1, 1, 1]
    dst = [1, 1, 1, 1]
    moves = detect_moves(src, dst)
    assert len(moves) == 0


def test_string_elements():
    src = ["a", "b", "c"]
    dst = ["c", "a", "b"]
    moves = detect_moves(src, dst)
    assert len(moves) == 1


def test_consecutive_elements_move():
    src = [1, 2, 3, 4, 5]
    dst = [3, 4, 5, 1, 2]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 0
    assert moves[0].dstIndex == 3
    assert moves[0].count == 2


def test_larger_sequence():
    src = [1, 2, 3, 4, 5, 6, 7, 8]
    dst = [1, 5, 6, 2, 3, 4, 7, 8]
    moves = detect_moves(src, dst)
    assert len(moves) == 1
    assert moves[0].srcIndex == 1
    assert moves[0].dstIndex == 3
    assert moves[0].count == 3


def test_complex_duplicates():
    src = [1, 2, 2, 3, 2]
    dst = [2, 1, 3, 2, 2]
    moves = detect_moves(src, dst)
    assert len(moves) == 2
    assert moves[0].srcIndex == 0
    assert moves[0].dstIndex == 1


@pytest.mark.parametrize("src", [
    [56, 75, 65, 46, 67, 57, 37, 64, 14, 6, 24, 8, 60, 11],
    [
        56, 75, 65, 46, 67, 57, 37, 64, 14, 6, 24, 8, 60, 11, 87, 79, 58, 40,
        30, 36, 82, 42, 27, 88, 12, 50, 2, 53, 86, 66, 19
    ],
])
def test_long_random_shuffle(src):
    dst = sorted(src)
    moves = detect_moves(src, dst)
