#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later

'''
Emacs lisp auto formatter.
'''

from __future__ import annotations
from collections.abc import (
    Callable,
    Generator,
    Iterable,
    Sequence,

)
from typing import (
    Any,
    NamedTuple,
    TextIO,
)

import sys
import os
import argparse

HintType = dict[
    str,
    str | int | tuple[int, int],
]
NdSexp_WrapState = tuple[bool, ...]

__all__ = (
    'main',
)


# ------------------------------------------------------------------------------
# Globals

# Disable mainly for testing what happens when we don't wrap lines at all.
USE_WRAP_LINES = True

# For debugging, check who added the newline.
USE_DEBUG_TRACE_NEWLINES = False

# Extract argument count from functions and macros.
USE_EXTRACT_DEFS = True

# Report missing definitions.
LOG_MISSING_DEFS = None  # '/tmp/out.log'

# Asserts that incur performance penalties which should not be enabled by default.
USE_PARANOID_ASSERT = False


# ------------------------------------------------------------------------------
# Exceptions

# Exception for failure to parse the file,
# show this in the command line output.
class FmtException(Exception):
    '''
    An exception raised for malformed files, where formatting cannot complete.
    '''


class FmtExceptionEarlyExit(Exception):
    '''
    Early exit from within callbacks.
    '''


# ------------------------------------------------------------------------------
# Utilities

def is_hash_prefix_special_case(text: str) -> bool:
    '''
    Return true if the hash should be connected to the following text.
    '''
    return (
        text.startswith('#') and
        (not text.startswith('#\''))
    )


# ------------------------------------------------------------------------------
# Line Length Checks

def calc_over_long_line_score(data: str, fill_column: int, trailing_parens: int, line_terminate: int) -> int:
    '''
    The resulting score is zero when all values are within the fill column.
    Otherwise a score will be returned which is used to compare the state of wrapped lines
    (bigger is worse).
    '''

    # This is the accumulated ``2 ** overflow``.
    # Note that the power is used so breaking a single line into two which both overflow
    # return a better (lower) score than a single line that overflows.

    # Step over `\n` characters instead of `data.split('\n')`
    # so consecutive newlines can be handled separately.
    line_step = 0
    i = 0

    score = 0
    if line_terminate != -1:
        while line_step != -1:
            line_step_next = data.find('\n', line_step)
            if line_step_next == -1:
                line_length = len(data) - line_step
                line_step = -1
            else:
                line_length = line_step_next - line_step
                line_step = line_step_next + 1

            if line_terminate == i:
                line_length += trailing_parens
                if line_length > fill_column:
                    score += 2 ** (line_length - fill_column)
                break
            if line_length > fill_column:
                score += 2 ** (line_length - fill_column)
            i += 1
    else:
        while line_step != -1:
            line_step_next = data.find('\n', line_step)
            if line_step_next == -1:
                line_length = len(data) - line_step
                line_step = -1
            else:
                line_length = line_step_next - line_step
                line_step = line_step_next + 1

            if line_length > fill_column:
                score += 2 ** (line_length - fill_column)
            i += 1

    return score


def calc_over_long_line_length_test(data: str, fill_column: int, trailing_parens: int, line_terminate: int) -> int:
    '''
    Return zero when all lines are within the ``fill_column``, otherwise 1.

    Even though this logically returns a boolean,
    use an int type since this is used by logic that calculates a score,
    and the return value from this function is part of that score.
    '''

    # Step over `\n` characters instead of `data.split('\n')`
    # so multiple characters are handled separately.
    line_step = 0
    i = 0

    if line_terminate != -1:
        while line_step != -1:
            line_step_next = data.find('\n', line_step)
            if line_step_next == -1:
                line_length = len(data) - line_step
                line_step = -1
            else:
                line_length = line_step_next - line_step
                line_step = line_step_next + 1

            if line_terminate == i:
                line_length += trailing_parens
                if line_length > fill_column:
                    return 1
                break
            if line_length > fill_column:
                return 1
            i += 1
    else:
        while line_step != -1:
            line_step_next = data.find('\n', line_step)
            if line_step_next == -1:
                line_length = len(data) - line_step
                line_step = -1
            else:
                line_length = line_step_next - line_step
                line_step = line_step_next + 1

            if line_length > fill_column:
                return 1
            i += 1

    return 0


# ------------------------------------------------------------------------------
# Formatting Utilities

def apply_comment_force_newline(root: NdSexp) -> None:
    '''
    Ensure new-lines are properly inserted
    to prevent code being placed at the end of a comment (turning it into a comment).
    '''

    # Special calculation for lines with comments on same line,
    # don't merge these lines since it would make it seem as if
    # the comment applies to the statements on that line.
    if not root.nodes:
        return

    node_line_start = root.nodes[0]
    for node, node_parent in root.iter_nodes_recursive_with_parent():
        if node is node_line_start:
            continue
        if isinstance(node, NdComment) and node.is_own_line is False:
            if not isinstance(node_line_start, NdWs):
                if node_line_start.force_newline:
                    pass
                elif node_line_start is node_parent.nodes[0]:
                    node_parent.force_newline = True
                else:
                    node_line_start.force_newline = True
        if node_line_start.original_lines[0] != node.original_lines[0]:
            node_line_start = node


def apply_relaxed_wrap(node_parent: NdSexp, style: FmtStyle) -> None:
    '''
    Wrap nodes in ``node_parent``, without taking the fill column into account.
    Some rules of thumb are applied such as:
    - Grouping :key value pairs.
    - Grouping as a hint (needed for ``setq`` argument pairing).

    This is done since every argument having it's own line is often not what users want,
    especially for keyword-value pairs. So it's best to first perform a relaxed wrap,
    then only further wrap arguments if they exceed the fill-column (which must be done as a separate step).
    '''

    node_prev = None
    force_newline = False

    if node_parent.hints is not None:
        hint_group = node_parent.hints.get('group')
    else:
        hint_group = None

    if hint_group is not None:
        assert isinstance(hint_group, list) and len(hint_group) == 2
        group_beg, group_len = hint_group
        assert isinstance(group_beg, int)
        assert isinstance(group_len, int)

        nodes_iter = node_parent.nodes_only_code[group_beg + 1:]
    else:
        nodes_iter = node_parent.nodes_only_code[node_parent.index_wrap_hint:]

    i_last = len(nodes_iter) - 1

    # Make a map, we could store this for reuse.
    nodes_with_trailing_comment_or_newline = set()
    node_prev = None
    for node in node_parent.nodes:
        if isinstance(node, (NdComment, NdWs)):
            nodes_with_trailing_comment_or_newline.add(id(node_prev))
        node_prev = node
    # Finish building 'nodes_with_trailing_comment_or_newline'.

    if hint_group is not None:
        group_len = hint_group[1]
        for i, node in enumerate(nodes_iter):
            ok = True

            if (i % group_len) != 0:
                ok = False

            if ok:
                node.force_newline = True
            force_newline |= node.force_newline

            node_prev = node

    else:
        for i, node in enumerate(nodes_iter):
            node_next = None if i == i_last else nodes_iter[i + 1]

            ok = True
            # Keep pairs:
            #     (foo
            #       :keyword value)
            #
            # Or:
            #     (foo
            #       :keyword value
            #       :other other-value)
            #
            # But only pairs, so multiple non keyword values after the (keyword, value)
            # split onto their own lines.
            #
            #     (foo
            #       :keyword
            #       value
            #       other-value)
            #
            # .. Better for use-package 'config' sections.
            #
            # But keep their pairs in the case of a blank line or comment.
            #     (foo
            #       :keyword value
            #
            #       other-value)
            #
            # .. Useful for macros such as 'define-minor-mode' which have properties, then a &rest.
            #
            if (
                    (isinstance(node_prev, NdSymbol) and node_prev.data.startswith(':')) and
                    (
                        (node_next is None) or
                        (id(node) in nodes_with_trailing_comment_or_newline) or
                        (isinstance(node_next, NdSymbol) and node_next.data.startswith(':'))
                    )
            ):
                ok = False

            if ok:
                node.force_newline = True
            force_newline |= node.force_newline

            node_prev = node

    if not style.use_native:
        if force_newline:
            node_parent.force_newline = True


def apply_relaxed_wrap_when_multiple_args(node_parent: NdSexp, style: FmtStyle) -> None:
    '''
    Relaxed wrap when the S-expression as 2 or more arguments passed in.
    '''
    if len(node_parent.nodes_only_code) - node_parent.index_wrap_hint > 1:
        apply_relaxed_wrap(node_parent, style)


def parse_local_defs(defs: FmtDefs, node_parent: NdSexp) -> None:
    '''
    Extract definitions from the file being formatted.

    While it's possible to store definitions for all files and load them in from JSON,
    this isn't practical when the file being edited would have to re-generate definitions
    every time.

    So extract definitions from ourselves (function properties and function argument counts).
    '''
    # Extract the number of functions from local definitions.
    # Currently only used so we can break the number of arguments at '&rest'
    # since it's nearly always where we have the body of macros which is logically
    # where we want to break.
    if node_parent.nodes_only_code and node_parent.brackets == '()':
        node = node_parent.nodes_only_code[0]
        if isinstance(node, NdSymbol):
            symbol_type = None
            if node.data in {
                    'defsubst',
                    'defun',
                    'defadvice',
                    'iter-defun',
            }:
                symbol_type = 'func'
            elif node.data in {
                    'defmacro',
            }:
                symbol_type = 'macro'
            else:
                return

            # Sanity check, should never fail.
            if len(node_parent.nodes_only_code) >= 3:
                node_symbol = node_parent.nodes_only_code[1]
                node_args = node_parent.nodes_only_code[2]
                if isinstance(node_symbol, NdSymbol) and isinstance(node_args, NdSexp):
                    symbol = node_symbol.data
                    arg_index_min = 0
                    arg_index_max: int | str = 0
                    hints: HintType | None = None
                    for i, node_arg in enumerate(node_args.nodes_only_code):
                        if not isinstance(node_arg, NdSymbol):
                            continue

                        if node_arg.data == '&rest':
                            arg_index_min = i
                            arg_index_max = 'many'
                            break
                        if node_arg.data == '&optional':
                            arg_index_min = i
                            arg_index_max = i
                            # Count remaining arguments and exit.
                            for j in range(i + 1, len(node_args.nodes_only_code)):
                                if isinstance(node_args.nodes_only_code[j], NdSymbol):
                                    arg_index_max = j
                            break

                    if node.data in {'defun', 'defmacro'}:
                        if len(node_parent.nodes_only_code) >= 4:
                            node_decl = None
                            if isinstance(node_parent.nodes_only_code[3], NdString):
                                if len(node_parent.nodes_only_code) >= 5:
                                    if isinstance(node_parent.nodes_only_code[4], NdSexp):
                                        node_decl = node_parent.nodes_only_code[4]
                            else:
                                if isinstance(node_parent.nodes_only_code[3], NdSexp):
                                    node_decl = node_parent.nodes_only_code[3]

                            # First argument after a function may be 'declare'.
                            if node_decl is not None and len(node_decl.nodes_only_code) > 1:
                                if (
                                        (isinstance(node_decl.nodes_only_code[0], NdSymbol)) and
                                        (node_decl.nodes_only_code[0].data == 'declare')
                                ):
                                    # The second value is currently unused (max arguments).
                                    for node_iter in node_decl.nodes_only_code[1:]:
                                        if (
                                                isinstance(node_iter, NdSexp) and
                                                len(node_iter.nodes_only_code) == 2
                                        ):
                                            node_key, node_val = node_iter.nodes_only_code
                                            if isinstance(node_key, NdSymbol):
                                                key = node_key.data
                                                if key == 'indent':
                                                    if hints is None:
                                                        hints = {}
                                                    if isinstance(node_val, NdSymbol):
                                                        val = node_val.data
                                                        hints[key] = int(val) if val.isdigit() else val
                                                elif key == 'doc-string':
                                                    if hints is None:
                                                        hints = {}
                                                    if isinstance(node_val, NdSymbol):
                                                        val = node_val.data
                                                        hints[key] = int(val) if val.isdigit() else val

                    defs.fn_arity[symbol] = FnArity(
                        symbol_type=symbol_type,
                        nargs_min=arg_index_min,
                        nargs_max=arg_index_max,
                        hints=hints,
                    )

    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            parse_local_defs(defs, node)


def scan_used_fn_defs(defs: FmtDefs, node_parent: NdSexp, fn_used: set[str]) -> None:
    '''
    Fill ``fn_used`` with a list of definitions used in this document.
    Used to implement ``FmtDefs.prune_unused``.
    '''
    if node_parent.nodes_only_code:
        node = node_parent.nodes_only_code[0]
        # When `is_data`, the `fn_arity` value should be ignored.
        if isinstance(node, NdSymbol) and (not node_parent.hints.get("is_data")):
            symbol = node.data
            len_prev = len(fn_used)
            fn_used.add(symbol)
            if len_prev != len(fn_used):
                # These wont come from local definitions (only ones generated by Emacs)
                # so it's save to use this as a lookup even though this data is being populated.
                fn_data = defs.fn_arity.get(symbol)
                if fn_data is not None:
                    hints: HintType | None = fn_data[3]
                    if hints:
                        for hint_key in ('doc-string', 'indent'):
                            hint_value = hints.get(hint_key)
                            if isinstance(hint_value, str):
                                fn_used.add(hint_value)
    for node in node_parent.nodes_only_code:
        if isinstance(node, NdSexp):
            scan_used_fn_defs(defs, node, fn_used)


def apply_rules_from_comments(node_parent: NdSexp) -> None:
    '''
    Apply rules from literal text in comments.
    '''
    import string
    punctuation_space_or_empty = {'', ' ', '\t'} | set(string.punctuation)
    del string

    wrap_locked = False
    wrap_locked_next = False  # Should not be used but reads like a bug if this isn't set.
    autofmt_text = 'format'
    autofmt_text_next = '-next-line:'
    do_next_line = False
    for node in node_parent.iter_nodes_recursive():
        if isinstance(node, NdComment):
            autofmt_text_index = node.data.find(autofmt_text)
            if autofmt_text_index != -1:
                comment = node.data
                # Ensure there is only space or ';' beforehand.
                ok = True
                for index in range(autofmt_text_index):
                    if comment[index] not in {';', ' ', '\t'}:
                        ok = False
                        break
                if ok:
                    # After format either: `:` or `-next-line:` are expected.
                    autofmt_text_index += len(autofmt_text)
                    if comment[autofmt_text_index] == ':':
                        autofmt_text_index += 1
                        do_next_line = False
                    elif comment[autofmt_text_index:autofmt_text_index + len(autofmt_text_next)] == autofmt_text_next:
                        autofmt_text_index += len(autofmt_text_next)
                        do_next_line = True
                    else:
                        ok = False

                    if ok:
                        while autofmt_text_index < len(comment) and comment[autofmt_text_index] in {' ', '\t'}:
                            autofmt_text_index += 1
                        # Allow text after the boolean (use split method).
                        bool_value = node.data[autofmt_text_index:autofmt_text_index + 4]
                        if bool_value.startswith('on'):
                            if bool_value[2:3] in punctuation_space_or_empty:
                                if do_next_line:
                                    wrap_locked_next = False
                                else:
                                    wrap_locked = False
                            else:
                                ok = False
                        elif bool_value.startswith('off'):
                            if bool_value[3:4] in punctuation_space_or_empty:
                                if do_next_line:
                                    wrap_locked_next = True
                                else:
                                    wrap_locked = True
                            else:
                                ok = False
                    if ok:
                        continue

        elif isinstance(node, NdSexp):
            if do_next_line:
                if wrap_locked_next:
                    node.wrap_locked = True
                    apply_rules_recursive_locked(node)
            elif wrap_locked:
                node.wrap_locked = True
                apply_rules_recursive_locked(node)
                do_next_line = False

        # Ensure the comment afterwards is immediately afterwards,
        # not just the next S-expression.
        do_next_line = False


def apply_rules_recursive_locked(node_parent: NdSexp) -> None:
    '''
    Recursively apply new-lines based on the input, needed so existing formatting can be kept.
    Used for ``--fmt-quoted=0`` support.
    '''
    # Also detect line changes between the parent and the child.
    prev_original_line = node_parent.original_lines[0]
    for node in node_parent.iter_nodes_recursive():
        if isinstance(node, NdSexp):
            node.wrap_locked = True
        if prev_original_line != node.original_lines[0]:
            node.force_newline = True
            prev_original_line = node.original_lines[0]


def apply_rules_recursive(cfg: FmtConfig, node_parent: NdSexp) -> None:
    '''
    Define line breaks using rules set by:

    - Function properties.
    - Function argument count.
    - Hard coded checks (``let`` for e.g.).
      NOTE: ideally there would be no hard coded checks, remove wherever possible.

    Without this, the LISP will be correct but not formatted in a way users might expect.
    '''
    use_native = cfg.style.use_native
    wrap_locked = False

    # Optional.
    if node_parent.nodes_only_code and node_parent.brackets == '()':
        if not cfg.use_quoted and '\'' in node_parent.prefix:
            wrap_locked = True
        elif isinstance(node := node_parent.nodes_only_code[0], NdSymbol):
            data_strip = node.data.rstrip('-*')
            node_parent.index_wrap_hint = 1
            # NOTE: this also captures `-let` and `-when-let` which are defined by dash.
            if data_strip in {
                    'and-let',  # Also: `and-let*`.
                    'cl-letf',  # Also: `cl-letf*`.
                    'if-let',  # Also: `if-let`.
                    'let',  # Also: `let*`.
                    'letrec',
                    'pcase-let',  # Also: `pcase-let*`.
                    'when-let',  # Also: `when-let*`.
            }:
                # A new line for each body of the let-statement.
                node_parent.index_wrap_hint = 2
                node_parent.hints['indent'] = 1

                # Only wrap with multiple declarations.
                if cfg.use_wrap:
                    # Regarding the following `is_data` assignment.
                    #
                    # This applies to:
                    #    (cond
                    #      (symbol
                    #        (function X Y Z)
                    #        result))
                    # In this case it's incorrect to wrap `symbol` as if it is a function with two arguments.
                    # Instead, the first argument should be considered "data".
                    if use_native:
                        if isinstance(node_parent.nodes_only_code[1], NdSexp):
                            if len(node_parent.nodes_only_code[1].nodes_only_code) > 1:
                                for subnode in node_parent.nodes_only_code[1].nodes_only_code[1:]:
                                    subnode.force_newline = True
                                    if isinstance(subnode, NdSexp):
                                        subnode.hints["is_data"] = True
                    else:
                        if isinstance(node_parent.nodes_only_code[1], NdSexp):
                            if len(node_parent.nodes_only_code[1].nodes_only_code) > 1:
                                for subnode in node_parent.nodes_only_code[1].nodes_only_code:
                                    subnode.force_newline = True
                                    if isinstance(subnode, NdSexp):
                                        subnode.hints["is_data"] = True

                    if not use_native:
                        if len(node_parent.nodes_only_code) > 2:
                            # While this should always be true, while editing it can be empty at times.
                            # Don't error in this case because it's annoying.
                            node_parent.nodes_only_code[2].force_newline = True

                    apply_relaxed_wrap(node_parent, cfg.style)

            elif data_strip == 'cond':  # Also: `cond*`.
                if cfg.use_wrap:
                    # If the S-expression is data, don't attempt to evaluate it as a function call.
                    # This applies to:
                    #    (cond
                    #      (symbol
                    #        (function X Y Z)
                    #        result))
                    # In this case it's incorrect to wrap `symbol` as if it is a function with two arguments.
                    # Instead, the first argument should be considered "data".
                    for subnode in node_parent.nodes_only_code[1:]:
                        subnode.force_newline = True
                        if isinstance(subnode, NdSexp):
                            if len(subnode.nodes_only_code) >= 2:
                                subnode.nodes_only_code[1].force_newline = True
                                apply_relaxed_wrap_when_multiple_args(subnode, cfg.style)
                            subnode.hints["is_data"] = True

            elif data_strip == 'pcase':
                if cfg.use_wrap:
                    # If the S-expression is data, don't attempt to evaluate it as a function call.
                    # This applies to:
                    #    (pcase var
                    #      (symbol x))
                    # In this case it's incorrect to wrap `symbol` as if it is a function with two arguments.
                    # Instead, the first argument should be considered "data".
                    for subnode in node_parent.nodes_only_code[2:]:
                        # subnode.force_newline = True
                        if isinstance(subnode, NdSexp):
                            if len(subnode.nodes_only_code) >= 3:
                                subnode.nodes_only_code[1].force_newline = True
                                apply_relaxed_wrap_when_multiple_args(subnode, cfg.style)
                            subnode.hints["is_data"] = True

                    # Needed so the "value" is is not wrapped.
                    node_parent.index_wrap_hint = 2
                    # Needed so the body of the error cases is de-dented.
                    node_parent.hints['indent'] = 1
                    apply_relaxed_wrap(node_parent, cfg.style)

            elif node.data in {
                    'condition-case',
                    'condition-case-unless-debug',
            }:
                if cfg.use_wrap:
                    # If the S-expression is data, don't attempt to evaluate it as a function call.
                    # This applies to:
                    #    (condition-case error
                    #        (progn
                    #          result)
                    #      (error
                    #       result)
                    # In this case it's incorrect to wrap `error` as if it is a function with two arguments.
                    # Instead, the first argument should be considered "data".
                    for subnode in node_parent.nodes_only_code[3:]:
                        subnode.force_newline = True
                        if isinstance(subnode, NdSexp):
                            if len(subnode.nodes_only_code) >= 2:
                                subnode.nodes_only_code[1].force_newline = True
                                apply_relaxed_wrap_when_multiple_args(subnode, cfg.style)
                            subnode.hints["is_data"] = True

                    # Needed so the "error" id is not indented.
                    node_parent.index_wrap_hint = 2
                    # Needed so the body of the error cases is de-dented.
                    node_parent.hints['indent'] = 2
                    apply_relaxed_wrap(node_parent, cfg.style)
            else:
                # First lookup built-in definitions, if they exist.
                if node_parent.hints.get("is_data"):
                    pass
                elif (fn_data := cfg.defs.fn_arity.get(node.data)) is not None:
                    # May be `FnArity` or a list.
                    symbol_type, nargs_min, nargs_max, hints = fn_data
                    if nargs_min is None:
                        nargs_min = 0

                    if hints is not None:
                        node_parent.hints.update(hints)
                    hints = node_parent.hints

                    hint_indent = hints.get('indent')
                    if hint_indent is not None:
                        # node_parent.index_wrap_hint = 1 + hint_indent
                        if symbol_type in {'special', 'macro'}:
                            if 'break' not in hints:
                                hints['break'] = 'always'

                    # First symbol counts for 1, another since wrapping takes place after this argument.
                    node_parent.index_wrap_hint = nargs_min + 1

                    # Wrap the first argument, instead of the last argument
                    # so all arguments are at an equal level as having the last
                    # argument split from the rest doesn't signify an important difference.
                    if symbol_type == 'func':
                        if hint_indent is None:
                            if hints is not None and hints.get('break_point') != 'overflow':
                                if node_parent.index_wrap_hint >= len(node_parent.nodes_only_code):
                                    node_parent.index_wrap_hint = 1

                    elif symbol_type == 'macro':
                        if nargs_max == 'many':
                            # So (with ...) macros don't keep the first argument aligned.
                            node_parent.wrap_all_or_nothing_hint = True
                            hints['break_point'] = 'overflow'

                    elif symbol_type == 'special':
                        # Used for special forms `unwind-protect`, `progn` .. etc.
                        node_parent.wrap_all_or_nothing_hint = True
                        if hints is None:
                            hints = {}
                        if 'break' not in hints:
                            hints['break'] = 'always'

                    if hints:
                        # Always wrap the doc-string.
                        if (hint_docstring := hints.get('doc-string')) is not None:
                            # NOTE: no support for evaluating EMACS-lisp from Python
                            # (so no support for symbol types).
                            if isinstance(hint_docstring, int):
                                node_parent.index_wrap_hint = min(node_parent.index_wrap_hint, hint_docstring)

                        if (hint_group := hints.get('group')) is not None:
                            assert isinstance(hint_group, list) and len(hint_group) == 2
                            group_beg, group_len = hint_group
                            assert isinstance(group_beg, int)
                            assert isinstance(group_len, int)
                            if len(node_parent.nodes_only_code) > group_beg + group_len + 1:
                                node_parent.index_wrap_hint = group_beg + group_len + 1
                            else:
                                # Group not in use.
                                del hints['group']

                        if cfg.use_wrap:
                            if (val := hints.get('break')) is not None:
                                if val == 'always':
                                    apply_relaxed_wrap(node_parent, cfg.style)
                                elif val == 'multi':
                                    apply_relaxed_wrap_when_multiple_args(node_parent, cfg.style)
                                elif val == 'to_wrap':  # Default
                                    pass
                                else:
                                    raise FmtException((
                                        'unknown "break" for {:s}, expected a value in '
                                        '["always", "multi", "to_wrap"]'
                                    ).format(node.data))
                else:
                    if LOG_MISSING_DEFS is not None:
                        with open(LOG_MISSING_DEFS, 'a', encoding='utf-8') as fh:
                            fh.write('Missing: {:s}\n'.format(node.data))

    if wrap_locked:
        if not node_parent.wrap_locked:
            node_parent.wrap_locked = True
            apply_rules_recursive_locked(node_parent)
    else:
        for node in node_parent.nodes_only_code:
            if isinstance(node, NdSexp):
                apply_rules_recursive(cfg, node)
            if not use_native:
                if node.force_newline:
                    node_parent.force_newline = True

    if not use_native:
        node_parent.flush_newlines_from_nodes()


# ------------------------------------------------------------------------------
# Classes

# Immutable configuration,
# created from command line arguments.

class FmtStyle(NamedTuple):
    '''
    Details relating to formatting style.
    '''
    use_native: bool


class FmtConfig(NamedTuple):
    '''
    Configuration options relating to how the file should be formatted.
    '''
    style: FmtStyle
    use_trailing_parens: bool
    use_quoted: bool
    use_multiprocessing: bool

    # When disabled, the entire script may be wrapped onto a single line,
    # although comments and multi-line strings prevent this in some cases
    # as "rules" regarding wrapping all or nothing still apply,
    # see: `NdSexp.wrap_all_or_nothing_hint`.
    use_wrap: bool

    fill_column: int

    empty_lines: int
    defs: FmtDefs


class FnArity(NamedTuple):
    '''
    Data associated with a function.
    '''
    # Type in: [`macro`, `func`, `special`].
    symbol_type: str
    # Minimum number of arguments.
    nargs_min: int
    # Maximum number of arguments, or strings: `many`, `unevalled`.
    nargs_max: int | str
    # Optional additional hints.
    hints: HintType | None


class FmtDefs:
    '''
    Function definition data, hints about when to wrap arguments.
    '''
    __slots__ = (
        'fn_arity',
    )

    def __init__(
            self,
            *,
            # The key is the function name.
            fn_arity: dict[str, FnArity],
    ):
        self.fn_arity = fn_arity

    def copy(self) -> FmtDefs:
        '''
        Return a copy of ``self``.
        '''
        return FmtDefs(fn_arity=self.fn_arity.copy())

    def prune_unused(self, fn_used: set[str]) -> None:
        '''
        Remove unused identifiers using a ``fn_used`` set.
        '''
        fn_arity = self.fn_arity
        self.fn_arity = {k: v for k, v in fn_arity.items() if k in fn_used}
        fn_arity.clear()

    def from_json_files(self, fmt_defs: Iterable[str]) -> None:
        '''
        Load definitions from JSON files.
        '''
        import json
        for filepath in fmt_defs:
            with open(filepath, 'r', encoding='utf-8') as fh:
                try:
                    fh_as_json = json.load(fh)
                except Exception as ex:  # pylint: disable=W0703
                    sys.stderr.write('JSON definition: error ({:s}) parsing {!r}!\n'.format(str(ex), filepath))
                    continue

                functions_from_json = fh_as_json.get('functions')
                if functions_from_json is None:
                    continue

                if type(functions_from_json) is not dict:
                    sys.stderr.write(
                        'JSON definition: "functions" entry is a {!r}, expected a dict in {!r}!\n'.format(
                            type(functions_from_json).__name__,
                            filepath,
                        )
                    )
                    continue

                self.fn_arity.update(functions_from_json)


class FmtWriteCtx:
    '''
    Track context while writing.
    '''
    __slots__ = (
        'last_node',
        'is_newline',
        'line',
        'column',
        'line_terminate',
        'cfg',
    )

    last_node: Node | None
    is_newline: bool
    line: int
    column: int
    line_terminate: int
    cfg: FmtConfig

    def __init__(self, cfg: FmtConfig):
        self.last_node = None
        self.is_newline = True
        self.line = 0
        self.column = 0
        self.line_terminate = -1
        self.cfg = cfg


class Node:
    '''
    Base class for all kinds of Lisp elements.
    '''
    __slots__ = (
        'force_newline',
        'original_lines',
    )

    force_newline: bool
    # Zero based line indices.
    original_lines: tuple[int, int]

    def calc_force_newline(self, style: FmtStyle) -> None:
        '''
        Function which must be overridden.
        '''
        raise RuntimeError('All subclasses must define this')

    def __repr__(self) -> str:
        return 'force_newline={:d} line={:d}-{:d}'.format(
            self.force_newline,
            self.original_lines[0],
            self.original_lines[1],
        )

    def fmt(
            self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        '''
        Format function which must be overridden.
        '''
        raise RuntimeError('All subclasses must define this')


# This is not enabled by default, hack for debugging only.
if USE_DEBUG_TRACE_NEWLINES:

    def _function_id(num_frames_up: int) -> str:
        '''
        Create a string naming the function n frames up on the stack.
        '''
        co = sys._getframe(num_frames_up + 1).f_code
        return '{:d} {:s}'.format(co.co_firstlineno, co.co_name)

    _Node = Node
    Node.__slots__ = tuple(s for s in Node.__slots__ if s != 'force_newline')  # type: ignore
    del Node

    class NodeTraceLines(_Node):
        '''
        Base class for all kinds of Lisp elements.
        '''
        __slots__ = (
            '_force_newline',
            '_force_newline_tracepoint',
        )

        original_lines: tuple[int, int]

        @property
        def force_newline(self) -> bool:
            '''
            Wrapper for ``_force_newline`` internal property.
            '''
            return self._force_newline

        @force_newline.setter
        def force_newline(self, value: bool) -> None:
            assert (value is True or value is False)
            if getattr(self, '_force_newline', None) != value:
                if value:
                    self._force_newline_tracepoint = (
                        _function_id(1) + ' ' +
                        _function_id(2)
                    )
            self._force_newline = value

    Node = NodeTraceLines  # type: ignore


class NdSexp(Node):
    '''
    Represents S-expressions (lists with curved or square brackets).
    '''
    __slots__ = (
        'prefix',
        'brackets',
        'nodes',
        'nodes_only_code',
        'index_wrap_hint',
        'wrap_all_or_nothing_hint',
        # Disallow re-wrapping for `nodes` (does not apply to this nodes wrapped state).
        'wrap_locked',
        # Documentation for hints:
        # - `is_data`: boolean
        #   When true, treat the the S-expression is data,
        #   don't attempt to evaluate its first symbol as a function call.
        #   This is needed for `let` & `cond` style statements where elements
        #   where an S-expression doesn't define a function call.
        'hints',
        'prior_states',
        'fmt_cache',
    )

    def __init__(
            self,
            lines: tuple[int, int],
            brackets: str,
            nodes: list[Node] | None = None,
    ):
        self.original_lines = lines
        self.prefix: str = ''
        self.brackets = brackets
        self.nodes = nodes or []
        self.index_wrap_hint: int = 1
        self.wrap_all_or_nothing_hint: bool = False
        self.wrap_locked = False
        self.hints: HintType = {}
        self.prior_states: list[NdSexp_WrapState] = []
        self.fmt_cache = ''

    def __repr__(self) -> str:
        import textwrap
        return '{:s}({:s} prefix=({:s})\n{:s})'.format(
            self.__class__.__name__,
            Node.__repr__(self),
            self.prefix,
            '\n'.join(textwrap.indent(repr(node), '  ') for node in self.nodes),
        )

    def fn_arity_get_from_first_symbol(self, defs: FmtDefs) -> FnArity | None:
        '''
        Return the ``FnArity`` from the first argument of this S-expressions symbol (if it is a symbol).
        '''
        if self.nodes_only_code and (not self.hints.get("is_data")):
            node = self.nodes_only_code[0]
            if isinstance(node, NdSymbol):
                return defs.fn_arity.get(node.data)
        return None

    def maybe_function_call(self, *, allow_data: bool) -> bool:
        '''
        Return true if this may be a function call signature.
        '''
        if not allow_data:
            if self.hints.get("is_data"):
                return False

        if self.nodes_only_code:
            node = self.nodes_only_code[0]
            # Currently numbers are treated as symbols, check their contents here.
            if isinstance(node, NdSymbol) and node.data.isdigit() is False:
                return True
        return False

    def is_multiline(self) -> bool:
        '''
        Return true if this S-expression spans multiple lines.
        '''
        for node in self.iter_nodes_recursive():
            if node.force_newline:
                return True
        return False

    def count_recursive(self) -> int:
        '''
        Return the number of elements within this node (recursively), excluding it's self.
        '''
        count = len(self.nodes)
        for node in self.nodes_only_code:
            if isinstance(node, NdSexp):
                count += node.count_recursive()
        return count

    def node_last_for_trailing_parens_test(self) -> Node | None:
        '''
        Return the node which would have trialing parenthesis written after it or None if it's not a code node
        since a trailing comment for e.g. will never have parenthesis written directly after it.
        '''
        if self.nodes and self.nodes_only_code:
            if self.nodes[-1] is self.nodes_only_code[-1]:
                return self.nodes[-1]
        return None

    def iter_nodes_recursive(self) -> Generator[Node, None, None]:
        '''
        Iterate over all nodes recursively.
        '''
        for node in self.nodes:
            yield node
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive()

    def iter_nodes_recursive_only_sexp(self) -> Generator[NdSexp, None, None]:
        '''
        Iterate over all S-expression nodes recursively.
        '''
        for node in self.nodes_only_code:
            if isinstance(node, NdSexp):
                yield node
                yield from node.iter_nodes_recursive_only_sexp()

    def iter_nodes_recursive_with_self(self) -> Generator[Node, None, None]:
        '''
        Iterate over all nodes recursively, including this node (first).
        '''
        yield self
        for node in self.nodes:
            yield node
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive()

    def iter_nodes_recursive_with_self_only_sexp(self) -> Generator[NdSexp, None, None]:
        '''
        Iterate over all S-expression nodes recursively, including this node (first).
        '''
        yield self
        for node in self.nodes_only_code:
            if isinstance(node, NdSexp):
                yield node
                yield from node.iter_nodes_recursive_only_sexp()

    def iter_nodes_recursive_with_self_only_sexp_without_wrap_locked(self) -> Generator[NdSexp, None, None]:
        '''
        Iterate over all S-expression nodes recursively, including this node (first).
        '''
        if not self.wrap_locked:
            yield self
            for node in self.nodes_only_code:
                if isinstance(node, NdSexp) and not node.wrap_locked:
                    yield node
                    yield from node.iter_nodes_recursive_only_sexp()

    def iter_nodes_recursive_with_parent(self) -> Generator[tuple[Node, NdSexp], None, None]:
        '''
        Iterate over all nodes recursively, with the parent node as well.
        '''
        for node in self.nodes:
            yield (node, self)
            if isinstance(node, NdSexp):
                yield from node.iter_nodes_recursive_with_parent()

    def iter_nodes_recursive_with_prior_state(self, visited: set[int]) -> Generator[NdSexp, None, None]:
        '''
        Specialized iterator for looping over nodes that have a ``prior_state`` set.

        This takes a ``visited`` argument to prevent over-iteration, so this can be called multiple times
        on different levels of the S-expression tree, without re-looping over data a large number of times.
        '''
        for node in self.nodes:
            if isinstance(node, NdSexp):
                visited_len = len(visited)
                visited.add(id(node))
                if visited_len != len(visited):
                    if node.prior_states:
                        yield node
                    yield from node.iter_nodes_recursive_with_prior_state(visited)

    def iter_nodes_recursive_with_prior_state_and_self(self, visited: set[int]) -> Generator[NdSexp, None, None]:
        '''
        A version of ``iter_nodes_recursive_with_prior_state`` that includes ``self`` (last).
        '''
        yield from self.iter_nodes_recursive_with_prior_state(visited)
        visited_len = len(visited)
        visited.add(id(self))
        if visited_len != len(visited):
            if self.prior_states:
                yield self

    def newline_state_get(self) -> NdSexp_WrapState:
        '''
        Return the wrapped state of this S-expressions nodes.
        '''
        return tuple(node.force_newline for node in self.nodes)

    def newline_state_set(self, state: NdSexp_WrapState) -> None:
        '''
        set the wrapped state of this S-expressions nodes.
        '''
        for data, node in zip(state, self.nodes, strict=True):
            node.force_newline = data

    def calc_nodes_level_next(self, cfg: FmtConfig, level: int) -> Sequence[int]:
        '''
        Return a ``self.nodes`` aligned list of next levels.
        The list may be shorter, in this case the last element should be used
        for node indices that exceed this lists range.
        '''
        if not cfg.style.use_native or cfg.fill_column == 0:
            if level == -1:
                return [0]
            return [level + 2]
        if self.wrap_locked:
            # Support formatting such as:
            #    '(choice (const :tag "Foo" test)
            #             (const :tag "Bar" case))
            #
            # Even when formatting is locked.
            if (len(self.nodes_only_code) > 1) and isinstance(self.nodes_only_code[0], NdSymbol):
                if self.nodes_only_code[0].force_newline is False:
                    if self.nodes_only_code[1].force_newline is False:
                        return [level + len(self.prefix) + len(self.nodes_only_code[0].data) + 2]

            return [level + 1 + len(self.prefix)]

        # The complex 'native' case.
        node_code_index_pre_newline = 0
        if self.hints:
            indent = indent_orig = self.hints.get('indent')
            if type(indent) is str:
                fn_data_test = cfg.defs.fn_arity.get(indent)
                if fn_data_test is not None:
                    hints_test = fn_data_test[3]
                    if hints_test is not None:
                        indent = hints_test.get('indent')
                        if not isinstance(indent, int):
                            # Unlikely, avoid unexpected cases.
                            indent = None
                del fn_data_test

            if indent is not None:
                if isinstance(indent, int):
                    hint_docstring = self.hints.get('doc-string')
                    if hint_docstring is not None:
                        # TODO: no support for evaluating EMACS-lisp from Python.
                        if isinstance(hint_docstring, int):
                            indent = min(hint_docstring - 1, indent)
                    del hint_docstring
                else:
                    indent = None
        else:
            indent_orig = None
            indent = None

        if level == -1:
            level_next_first = 0
            level_next_pre = 0
            level_next_post = 0
        else:
            level_next_base = level + len(self.prefix)
            level_next_first = level_next_base + 1
            level_next_pre = level_next_base + 1
            level_next_post = level_next_base + 1

            if (len(self.nodes_only_code) > 1) and isinstance(self.nodes_only_code[0], NdSymbol):
                if self.nodes_only_code[0].force_newline is False:
                    if self.nodes_only_code[1].force_newline is False:
                        # Add 1 for the space for the trailing space.
                        # Values may be overwritten below.
                        level_next_first = level_next_pre = level_next_base + len(self.nodes_only_code[0].data) + 2
                        node_code_index_pre_newline = 1

            if indent is not None:
                if isinstance(self.nodes_only_code[0], NdSymbol):
                    fn_data = cfg.defs.fn_arity.get(self.nodes_only_code[0].data)
                else:
                    fn_data = None
                if fn_data is not None:
                    symbol_type, nargs_min, nargs_max, _hints = fn_data

                # Perhaps this should be indented further.
                if (
                        len(self.nodes_only_code) > 1 and
                        isinstance(self.nodes_only_code[0], NdSymbol) and
                        self.nodes_only_code[0].force_newline is False
                ):

                    two_or_more_non_wrapped_args = True
                    for node in self.nodes_only_code[1:3]:
                        if node.force_newline:
                            two_or_more_non_wrapped_args = False
                            break

                    if self.nodes_only_code[1].force_newline:
                        is_aligned = False
                        # pylint: disable-next=possibly-used-before-assignment
                    elif fn_data and symbol_type == 'special':
                        # This is used for e.g.
                        #    (condition-case err
                        #        (progn
                        #          test
                        #          case)
                        #      (error case))
                        is_aligned = False
                    elif (
                            fn_data is not None and
                            # pylint: disable-next=possibly-used-before-assignment
                            (nargs_min <= indent) and
                            two_or_more_non_wrapped_args and
                            # fancy-compilation needs this.
                            # WARNING: this is odd but for e.g.
                            #    (defun foo
                            #        (long arg list)
                            #      "Doc string.")
                            # is what emacs does. so follow this.
                            self.nodes_only_code[0].data != 'defun'
                    ):
                        # This is used for e.g.
                        #
                        #    (example foo bar
                        #             test)
                        is_aligned = True
                    else:
                        # This is used for e.g.
                        #    (when
                        #        (progn
                        #          a
                        #          b)
                        #      c)
                        is_aligned = False

                    if is_aligned:
                        level_next_pre = level_next_base + len(self.nodes_only_code[0].data) + 2
                    else:
                        level_next_pre = level_next_base + 4

                    level_next_post = level_next_base + 2

                    if fn_data is not None:
                        # All wrapped, no special indents handling.
                        if indent_orig == 'defun':
                            if (
                                    (nargs_max == 'unevalled' and symbol_type == 'special') or
                                    symbol_type in {'func', 'macro'}
                            ):
                                level_next_pre = level_next_base + 2
                                level_next_post = level_next_base + 2

        node_code_index = 0
        node_code_index_next = 0
        if indent is not None:
            assert isinstance(indent, int)
            node_code_index_to_indent = indent
        else:
            # Never use 'level_next_post', when no indent, everything is 'pre'.
            node_code_index_to_indent = len(self.nodes_only_code) + 1

        level_next_data = []
        for node in self.nodes:
            node_code_index = node_code_index_next
            if node_code_index <= node_code_index_pre_newline:
                level_next_data.append(level_next_first)
            elif node_code_index <= node_code_index_to_indent:
                level_next_data.append(level_next_pre)
            else:
                level_next_data.append(level_next_post)
                # All future elements will use this value.
                break

            if isinstance(node, NODE_CODE_TYPES):
                node_code_index_next = node_code_index + 1
            else:
                node_code_index_next = node_code_index

        return level_next_data

    def flush_newlines_from_nodes(self) -> bool:
        '''
        Ensure parent nodes are on their own-line when any of their children are multi-line.
        '''
        # `assert not cfg.use_native` (if we had `cfg`).
        changed = False
        if not self.force_newline:
            for node in self.nodes_only_code:
                if node.force_newline:
                    self.force_newline = True
                    changed = True
                    break
        return changed

    def flush_newlines_from_nodes_for_native(self) -> bool:
        '''
        Ensure some kinds of expressions are wrapped onto new files.
        '''
        # Ensure There is never trailing non-wrapped S-expressions: e.g:
        #
        #    (a b c d (e
        #              f
        #              g))
        #
        # This is only permissible for the first or second arguments, e.g:
        #
        #    (a (e
        #        f
        #        g))
        #
        # While this could be supported currently it's not and I feel this adds awkward right shift.
        # `assert cfg.use_native` # If we have `cfg`.
        changed = False
        for i, node in enumerate(self.nodes_only_code):
            if i > 1 and isinstance(node, NdSexp) and not node.force_newline and node.is_multiline():
                node.force_newline = True
                changed = True
                # if not self.force_newline:
                #     self.force_newline = True
        return changed

    def flush_newlines_from_nodes_for_native_recursive(self) -> bool:
        '''
        Run ``flush_newlines_from_nodes_for_native`` recursively.
        '''
        changed = False
        for i, node in enumerate(self.nodes_only_code):
            if i > 1 and isinstance(node, NdSexp) and not node.force_newline and node.is_multiline():
                node.force_newline = True
                changed = True
                # if not self.force_newline:
                #     self.force_newline = True
            if isinstance(node, NdSexp):
                changed |= node.flush_newlines_from_nodes_for_native_recursive()
        return changed

    def flush_newlines_from_nodes_recursive(self) -> bool:
        '''
        Flush new-lines from children to parents.
        '''
        # `assert not cfg.use_native` # If we have `cfg`.
        changed = False
        for node in self.nodes_only_code:
            if node.force_newline:
                if not self.force_newline:
                    changed = True
                    self.force_newline = True
            if isinstance(node, NdSexp):
                changed |= node.flush_newlines_from_nodes_recursive()
        return changed

    def calc_force_newline(self, style: FmtStyle) -> None:
        force_newline = False
        node_prev = None
        for node in self.nodes:
            node.calc_force_newline(style)
            if not node.force_newline:
                if node_prev:
                    if isinstance(node_prev, (NdComment, NdWs)):
                        node.force_newline = True
                    elif isinstance(node_prev, NdSymbol):
                        # Always keep trailing back-slashes,
                        # these are used to define a string literal across multiple lines.
                        if node_prev.data == '\\' and (node_prev.original_lines[0] != node.original_lines[0]):
                            node.force_newline = True
            force_newline |= node.force_newline
            node_prev = node

        if not style.use_native:
            self.force_newline = force_newline
        else:
            self.force_newline = False

    def finalize_parse(self) -> None:
        '''
        Perform final operations after parsing.
        '''
        # Connect: ' (  to '(
        i = len(self.nodes) - 1
        while i > 0:
            node = self.nodes[i]
            if isinstance(node, NdSexp):
                node_prev = self.nodes[i - 1]
                if isinstance(node_prev, NdSymbol):
                    if (
                            not node_prev.data.strip('#,`\'') or
                            # Some macros use `#foo(a(b(c)))` which need to be connected.
                            is_hash_prefix_special_case(node_prev.data)
                    ):
                        del self.nodes[i - 1]
                        node.prefix = node_prev.data
                        i -= 1
            i -= 1

        self.nodes_only_code: list[Node] = [
            node for node in self.nodes
            if isinstance(node, NODE_CODE_TYPES)
        ]
        for node in self.nodes_only_code:
            if isinstance(node, NdSexp):
                node.finalize_parse()

    def finalize_style(self, cfg: FmtConfig) -> None:
        '''
        Perform final operations after parsing.
        '''
        # Strip blank lines at the start or end of S-expressions.
        for i in (-1, 0):
            while self.nodes and isinstance(self.nodes[i], NdWs):
                del self.nodes[i]

        # Apply maximum blank lines.
        i = len(self.nodes) - 1

        count = 0
        while i > 1:
            node = self.nodes[i]
            if isinstance(node, NdWs):
                if count == cfg.empty_lines:
                    del self.nodes[i]
                else:
                    count += 1
            else:
                count = 0
            i -= 1
        del count

        for node in self.nodes_only_code:
            if isinstance(node, NdSexp):
                node.finalize_style(cfg)

    def fmt_check_exceeds_column_max(
            self,
            cfg: FmtConfig,
            level: int,
            trailing_parens: int,
            *,
            calc_score: bool,
            test_node_terminate: Node | None = None,
    ) -> int:
        '''
        :arg calc_score: When true, the return value is a score.
        '''
        if cfg.fill_column == 0:
            raise RuntimeError('internal error, this should not be called')

        # Simple optimization, don't calculate excess white-space.
        fill_column = cfg.fill_column - level
        level = 0
        _ctx = FmtWriteCtx(cfg)

        # Avoid writing the string:
        # Either calculate a score or early exit with an exception on the first over-length line found.
        # This block can be removed without causing any problems, it just avoids some excessive work.
        if test_node_terminate is None:

            line_length = 0
            score = 0

            if calc_score:
                # Accumulate a score.

                def write_fn_fast(text: str) -> None:
                    nonlocal line_length
                    nonlocal score
                    i = text.find('\n')
                    if i == -1:
                        line_length += len(text)
                    else:
                        i_prev = 0
                        while True:
                            line_length += i - i_prev
                            if line_length > fill_column:
                                score += 2 ** (line_length - fill_column)

                            i_prev = i + 1
                            i = text.find('\n', i_prev)
                            if i == -1:
                                line_length = len(text) - i_prev
                                break
                            line_length = 0

                self.fmt_with_terminate_node(_ctx, write_fn_fast, level, test=True)
                line_length += trailing_parens
                if line_length > fill_column:
                    score += 2 ** (line_length - fill_column)
            else:
                # Simple, detect if the line length is exceeded.
                # (score is effectively a boolean).

                def write_fn_fast(text: str) -> None:
                    nonlocal line_length
                    i = text.find('\n')
                    if i == -1:
                        line_length += len(text)
                        if line_length > fill_column:
                            raise FmtExceptionEarlyExit
                    else:
                        i_prev = 0
                        while True:
                            line_length += i - i_prev
                            if line_length > fill_column:
                                raise FmtExceptionEarlyExit

                            i_prev = i + 1
                            i = text.find('\n', i_prev)
                            if i == -1:
                                line_length = len(text) - i_prev
                                break
                            line_length = 0

                try:
                    self.fmt_with_terminate_node(_ctx, write_fn_fast, level, test=True)
                except FmtExceptionEarlyExit:
                    score = 1
                if line_length + trailing_parens > fill_column:
                    score = 1

            return score

        _data: list[str] = []
        write_fn = _data.append

        self.fmt_with_terminate_node(_ctx, write_fn, level, test=True, test_node_terminate=test_node_terminate)

        line_terminate = -1
        if not cfg.use_trailing_parens:
            if _ctx.line_terminate == _ctx.line:
                line_terminate = _ctx.line_terminate
            elif test_node_terminate is None:
                line_terminate = _ctx.line

        data = ''.join(_data)

        if calc_score:
            return calc_over_long_line_score(data, fill_column, trailing_parens, line_terminate)
        return calc_over_long_line_length_test(data, fill_column, trailing_parens, line_terminate)

    def fmt(self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
            ) -> None:
        '''
        Write this node to a file.
        '''
        if self.fmt_cache:
            write_fn(self.fmt_cache)
            return
        self.fmt_with_terminate_node(ctx, write_fn, level, test=test)

    def fmt_with_terminate_node(
            self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
            test_node_terminate: Node | None = None,
    ) -> None:
        '''
        Write this node to a file with support for terminating early.
        '''

        line_sexpr_prev = ctx.line

        if level != -1:
            if ctx.is_newline:
                write_fn(' ' * level)
                ctx.column += level
            else:
                # Needed to properly indent:
                #   (a b c d (e
                #             f))
                if self.wrap_locked:
                    level = ctx.column

            if self.prefix:
                write_fn(self.prefix)
                ctx.column += len(self.prefix)

                if ctx.cfg.style.use_native:
                    pass
                else:
                    # A single `#` is used for advanced macros,
                    # we can't put a new-line after them.
                    if (not is_hash_prefix_special_case(self.prefix)) and self.is_multiline():
                        write_fn('\n')

                        if USE_DEBUG_TRACE_NEWLINES:
                            if self.force_newline:
                                if not test:
                                    write_fn(' $' + self._force_newline_tracepoint)  # type: ignore

                        ctx.line += 1
                        ctx.column = 0
                        ctx.is_newline = True

                        write_fn(' ' * level)
                        ctx.column += level
                        ctx.is_newline = False

            write_fn(self.brackets[0])
            ctx.column += 1
            ctx.is_newline = False

        ctx.last_node = self

        is_first = True

        node_prev_is_multiline = False

        level_next_data = self.calc_nodes_level_next(ctx.cfg, level)

        level_next_data_last = len(level_next_data) - 1
        for i, node in enumerate(self.nodes):
            level_next = level_next_data[min(i, level_next_data_last)]

            if test:
                # Use only for testing.
                if node is test_node_terminate:
                    # We could return however this misses trailing parentheses on the same line.
                    assert ctx.line_terminate == -1
                    ctx.line_terminate = ctx.line

            if (
                    node.force_newline or
                    (
                        node_prev_is_multiline and
                        # Always respect `wrap_locked` when set.
                        not self.wrap_locked and
                        # Don't push trailing comments onto new line.
                        not (isinstance(node, NdComment) and node.is_own_line is False)
                    )
            ):
                if not ctx.is_newline:
                    write_fn('\n')
                    ctx.line += 1
                    ctx.column = 0
                    ctx.is_newline = True

            if isinstance(node, NdWs):
                pass
            else:
                if ctx.is_newline:

                    if USE_DEBUG_TRACE_NEWLINES:
                        if node.force_newline:
                            if not test:
                                write_fn(' $' + node._force_newline_tracepoint)  # type: ignore

                    write_fn(' ' * level_next)
                    ctx.column += level_next
                    ctx.is_newline = False
                else:
                    if (
                            (not is_first) or

                            # Ensure we have:
                            #     ( ;; Comment.
                            #
                            # Instead of:
                            #     (;; Comment.
                            (isinstance(node, NdComment) and node.is_own_line is False)
                    ):
                        write_fn(' ')
                        ctx.column += 1

            if ctx.line_terminate not in (-1, ctx.line):
                return

            line = ctx.line

            node.fmt(ctx, write_fn, level_next, test=test)
            node_prev_is_multiline = line != ctx.line

            ctx.last_node = node

            is_first = False

            if ctx.line_terminate not in (-1, ctx.line):
                return

        if level != -1:
            if ctx.is_newline:
                write_fn(' ' * level_next)
                ctx.column += level_next
                ctx.is_newline = False
            else:
                if (
                        (ctx.cfg.use_trailing_parens and ctx.line != line_sexpr_prev) or
                        isinstance(ctx.last_node, NdComment)
                ):
                    write_fn('\n')
                    ctx.line += 1
                    ctx.column = 0
                    ctx.is_newline = True
                    write_fn(' ' * level_next)
                    ctx.column += level_next
                    ctx.is_newline = False
            write_fn(self.brackets[1])
            ctx.column += 1
            ctx.is_newline = False


# ------------------------------------------------------------------------------
# Formatting Solver

def fmt_solver_fill_column_wrap_relaxed(
        cfg: FmtConfig,
        node_parent: NdSexp,
        level: int,
        trailing_parens: int) -> None:
    '''
    Perform relaxed wrapping for blocks where any lines exceed the fill-column.
    '''
    # First be relaxed, then again if it fails.
    if node_parent.fmt_check_exceeds_column_max(cfg, level, trailing_parens, calc_score=False):

        if cfg.fill_column != 0:
            if not node_parent.wrap_all_or_nothing_hint:
                state_init = node_parent.newline_state_get()

        apply_relaxed_wrap(node_parent, cfg.style)

        if cfg.fill_column != 0:
            if not node_parent.wrap_all_or_nothing_hint:
                state_test = node_parent.newline_state_get()
                if state_init != state_test:
                    node_parent.prior_states.append(state_init)

        if not cfg.style.use_native:
            node_parent.force_newline = True


def fmt_solver_fill_column_wrap_each_argument(
        cfg: FmtConfig,
        node_parent: NdSexp,
        level: int,
        trailing_parens: int) -> None:
    '''
    Check that each argument fits within the fill-column,
    wrapping as necessary.
    Note that this uses much more involved checks than ``fmt_solver_fill_column_wrap_relaxed``.
    '''
    assert node_parent.index_wrap_hint != 0
    assert len(node_parent.nodes_only_code) > 1

    node_force_newline = []
    # Wrap items before if absolutely needed, one at a time.
    force_newline = False
    i_last = len(node_parent.nodes_only_code) - 1
    i = min(node_parent.index_wrap_hint, i_last)
    assert i > 0

    node = node_parent.nodes_only_code[i]
    score_init = node_parent.fmt_check_exceeds_column_max(
        cfg,
        level,
        trailing_parens,
        calc_score=True,
        test_node_terminate=node,
    )

    if score_init:
        # Don't attempt the wrap the first item,
        # as this will simply push it onto the line below.
        #
        # This:
        #   (argument "Long string that does not fit")
        #
        # Gets converted to this:
        #   (
        #     argument
        #     "Long string that does not fit")
        #
        # Where as we would prefer this:
        #   (argument
        #     "Long string that does not fit")
        #
        while i != 0:
            node = node_parent.nodes_only_code[i]
            if not node.force_newline:
                node.force_newline = True
                node_force_newline.append(node)
                force_newline = True

                if not node_parent.fmt_check_exceeds_column_max(
                    cfg,
                    level,
                    trailing_parens,
                    calc_score=False,
                    test_node_terminate=node,
                ):
                    # Imply 'node_parent.wrap_all_or_nothing_hint', even when not set.
                    hints = node_parent.hints
                    if hints.get('break_point') == 'overflow':
                        pass
                    else:
                        # TODO: warn about unknown break_point.

                        # When the break was added before `node_parent.index_wrap_hint`,
                        # wrap everything so there are no breaks added in random locations
                        # that might seems significant.
                        if i < node_parent.index_wrap_hint:
                            for j in range(1, i):
                                node_iter = node_parent.nodes_only_code[j]
                                if not node_iter.force_newline:
                                    node_iter.force_newline = True
                                    node_force_newline.append(node_iter)
                    break
            i -= 1

        i = min(node_parent.index_wrap_hint, i_last)
        node = node_parent.nodes_only_code[i]
        score_test = node_parent.fmt_check_exceeds_column_max(
            cfg,
            level,
            trailing_parens,
            calc_score=True,
            test_node_terminate=node,
        )

        # If none of the changes made an improvement, revert them.
        if score_init <= score_test:
            for node_iter in node_force_newline:
                node_iter.force_newline = False
            force_newline = False

    if not cfg.style.use_native:
        if force_newline:
            node_parent.force_newline = True

    # If after wrapping 'everything', we still overflow,
    # don't use  this for tests in future, it confuses checks
    # causing other lines to wrap because of this node.
    if cfg.style.use_native:
        node = node_parent.nodes_only_code[1]
        if not node.force_newline:
            score_init = node_parent.fmt_check_exceeds_column_max(
                cfg,
                level,
                trailing_parens,
                calc_score=True,
            )
            if score_init:
                node.force_newline = True
                score_test = node_parent.fmt_check_exceeds_column_max(
                    cfg,
                    level,
                    trailing_parens,
                    calc_score=True,
                )
                if score_test < score_init:
                    # Success, don't exclude.
                    pass
                else:
                    # Don't add line break.
                    node.force_newline = False


def fmt_solver_fill_column_wrap_recursive(
        cfg: FmtConfig,
        node_parent: NdSexp,
        level: int,
        trailing_parens: int,
) -> None:
    '''
    For lists that will need wrapping even when all parents are wrapped,
    wrap these beforehand.
    '''
    if node_parent.wrap_locked:
        return
    if not node_parent.nodes_only_code:
        return

    node_parent_is_multiline_prev = node_parent.is_multiline()

    node_trailing_parens = node_parent.node_last_for_trailing_parens_test()

    level_next_data = node_parent.calc_nodes_level_next(cfg, level)
    level_next_data_last = len(level_next_data) - 1
    for i, node in enumerate(node_parent.nodes):
        if isinstance(node, NdSexp):
            level_next = level_next_data[min(i, level_next_data_last)]
            fmt_solver_fill_column_wrap_recursive(
                cfg,
                node,
                level_next,
                trailing_parens + 1 if node is node_trailing_parens else 0,
            )
            if not cfg.style.use_native:
                if node.force_newline:
                    node_parent.force_newline = True

    if cfg.fill_column != 0:
        state_init = node_parent.newline_state_get()

        if len(node_parent.nodes_only_code) > 1:
            fmt_solver_fill_column_wrap_relaxed(cfg, node_parent, level, trailing_parens)
            fmt_solver_fill_column_wrap_each_argument(cfg, node_parent, level, trailing_parens)

    # Some blocks don't allow mixed wrapping.
    if node_parent.wrap_all_or_nothing_hint:
        if node_parent_is_multiline_prev or node_parent.is_multiline():
            apply_relaxed_wrap(node_parent, cfg.style)

    if cfg.fill_column != 0:
        state_test = node_parent.newline_state_get()
        if state_init != state_test:
            node_parent.prior_states.append(state_init)


def fmt_solver_fill_column_unwrap_aggressive(
        cfg: FmtConfig,
        node_parent: NdSexp,
        level: int,
        trailing_parens: int,
        visited: set[int],
) -> bool:
    '''
    First perform an unwrap: restore all nodes to their initial state recursively.
    This is important because of how ``fmt_solver_newline_constraints_apply_recursive`` wraps
    parent nodes based on the multi-line status of the child nodes (when ``check_parent_multiline`` is enabled).
    Making it important to unwrap nodes recursively.
    Without this more aggressive check - some S-expressions can become "stuck" in a multi-line state.
    NOTE: An alternative to recursive unwrapping would be to track dependencies of why an S-expression is wrapped
    and re-calculate the new-lines

    Important _not_ to include ``node_parent`` in ``nodes_recursive_all`` because the next node from ``node_parent``
    may not be on a new-line causing the score not to take into account sibling nodes which would
    exceed the line length.
    '''
    assert node_parent.force_newline

    nodes_recursive_all = list(node_parent.iter_nodes_recursive_with_prior_state(set()))

    if nodes_recursive_all:
        calc_score = True
        states_recursive_curr = []
        states_recursive_test = []
        nodes_recursive = []
        for node in nodes_recursive_all:
            state_curr = node.newline_state_get()
            state_test = node.prior_states[0]
            if state_curr != state_test:
                states_recursive_curr.append(state_curr)
                states_recursive_test.append(state_test)
                nodes_recursive.append(node)

        # It's possible there are no new states worth testing.
        if nodes_recursive:
            # Calculate this before making the first change.
            parent_score_curr = node_parent.fmt_check_exceeds_column_max(
                cfg,
                level,
                trailing_parens,
                calc_score=True,
            )
            if parent_score_curr == 0:
                calc_score = False

            # Set the new state.
            for node, state_test in zip(nodes_recursive, states_recursive_test, strict=True):
                node.newline_state_set(state_test)

            fmt_solver_newline_constraints_apply_recursive(node_parent, cfg, check_parent_multiline=True)
            parent_score_test = node_parent.fmt_check_exceeds_column_max(
                cfg,
                level,
                trailing_parens,
                calc_score=calc_score,
            )
            if parent_score_test <= parent_score_curr:
                # Keep the new state.
                for node in nodes_recursive_all:
                    node.prior_states.clear()
                    # No need to visit them again.
                    visited.add(id(node))
                return True

            # Restore the initial state.
            # TODO: Python 3.10 `strict=True`.
            for node, state in zip(nodes_recursive, states_recursive_curr):
                node.newline_state_set(state)

    return False


def fmt_solver_fill_column_unwrap_test_state(
        cfg: FmtConfig,
        node_parent: NdSexp,
        node: NdSexp,
        level: int,
        trailing_parens: int,
        parent_score_curr: int,
        state_visit: set[NdSexp_WrapState],
        state_curr: NdSexp_WrapState,
        # This is the only argument which is likely to change each call.
        state_test: NdSexp_WrapState,
) -> int | None:
    '''
    Set the line wrapping state to  ``state_test``, if it doesn't exceed the fill column,
    use it and return the new score,
    otherwise restore ``state_curr`` and return none.

    NOTE: ``node_parent`` may not be the direct parent of this node,
    there may be many levels of nesting between them.
    '''

    # Should never be an empty list.
    assert state_test

    if state_test in state_visit:
        return None

    node.newline_state_set(state_test)

    if fmt_solver_newline_constraints_apply(node, cfg, check_parent_multiline=True):
        state_test = node.newline_state_get()
        if state_test in state_visit:
            node.newline_state_set(state_curr)
            return None

    parent_score_test = node_parent.fmt_check_exceeds_column_max(
        cfg,
        level,
        trailing_parens,
        # If the current state has no over-length lines (such as long comments).
        # There is no need to do extra work. Any over-long line caused by the state being
        # tested can immediately be considered an error and early exit.
        # In this case the score will only ever be 0/1 but that's fine.
        calc_score=(parent_score_curr != 0),
    )
    if parent_score_test <= parent_score_curr:
        parent_score_curr = parent_score_test
        state_curr = state_test
        # The most ambitious (early) states are first, no need to try others.
        return parent_score_curr

    # This state was unsuccessful, don't attempt to test it again.
    state_visit.add(state_test)

    node.newline_state_set(state_curr)
    return None


def fmt_solver_fill_column_unwrap_test_state_permutations(
        cfg: FmtConfig,
        node_parent: NdSexp,
        node: NdSexp,
        level: int,
        trailing_parens: int,
        parent_score_curr: int,
) -> int | None:
    '''
    Scan the previous line wrapping states and attempt to apply them.

    - First scan ``node.prior_states``, to see if any states before wrapping are usable,
      If so, return.

    - If none are usable, generate wrapping states which were not previously seen,
      there is some room for opinionated decisions about what would be considered ideal,
      which can make a significant difference to the resulting formatting.
    '''

    # While in general duplicates states are not added,
    # it's also not guaranteed that this can never happen. And it in-fact does sometimes.
    # Since it's fairly rare, track states here which have already been tested.
    #
    # This avoids the need to de-duplicate when adding, and means if the first unwrap is successful
    # then there is no need to track visited states at all.
    state_curr = node.newline_state_get()
    state_visit = {state_curr}

    # These don't change each call, simplify code by not duplicating this line all over.
    static_test_state_args = cfg, node_parent, node, level, trailing_parens, parent_score_curr, state_visit, state_curr

    # Iterate over prior states and optionally generate additional states that can be used as a last resort.
    for state_test in node.prior_states:
        parent_score_test = fmt_solver_fill_column_unwrap_test_state(*static_test_state_args, state_test)
        if parent_score_test is not None:
            return parent_score_test

    # Existing states handled, if this is reached, calculate new states (fun!).
    # NOTE: in many cases one of the states from above will be used and the following code will not run.

    # The initial state is significant because any lines that _must_ be wrapped will be enabled in this state.
    state_init = node.prior_states[0]

    # Generate additional unwrapped states on the fly.
    # This is done so arguments that each onto their own line can have single line wrapped versions,
    # e.g:
    #
    #    (function-call
    #     a
    #     b
    #     c
    #     d
    #     e)
    #
    # When `d` and `e` are optional arguments a split in the argument list may be used:
    #
    #    (function-call
    #     a b c d e)
    #
    #    (function-call a b c
    #                   d e)
    #
    # Finally (if all else fails), attempt moving all arguments onto the previous line.
    # This is typically only done when each argument it's self is very long, otherwise
    # they will normally fit onto a single line.
    #
    #    (function-call a
    #                   b
    #                   c
    #                   d
    #                   e)
    #
    # Note that this is only attempted as a last resort because attempting this style early on
    # causes an awkward and unbalanced formatting.
    if (
            # Ensure this is not a literal list of strings or numbers for e.g.
            # NOTE: that we may want to handle data differently,
            # currently matching logic for values inside `let` & `cond` works nicely.
            node.maybe_function_call(allow_data=True) and
            # All arguments were wrapped onto separate lines.
            (False not in state_curr[1:])
    ):

        if (
                # No mixed wrapping allowed,
                # this is a hint that the block was for a macro or special such as `progn`.
                # In this case don't attempt to wrap arguments onto a single line,
                # reserve this for function-calls.
                (not node.wrap_all_or_nothing_hint) and
                # It only makes sense to run this logic if there are multiple arguments to deal with.
                (len(node.nodes_only_code) > 1) and
                # At the moment they are used interchangeably so mis-alignment is not supported.
                (len(node.nodes_only_code) == len(node.nodes)) and
                # Was on a single line (ignoring the first).
                (True not in state_init[1:])
        ):
            # All wrapped.
            if node.index_wrap_hint < len(node.nodes_only_code):
                index_wrap_hint = node.index_wrap_hint
            else:
                index_wrap_hint = 1
            state_gen = [False] * len(state_curr)

            # Provide:
            #
            #    (function-call
            #     a b c d e)

            state_gen[index_wrap_hint] = True
            parent_score_test = fmt_solver_fill_column_unwrap_test_state(*static_test_state_args, tuple(state_gen))
            if parent_score_test is not None:
                return parent_score_test

            # Check if the line can be broken up.
            if index_wrap_hint > 1:
                # Provide :
                #
                #    (function-call a b c
                #                   d e)
                #

                # Wrap both `index_wrap_hint` and 1.
                state_gen[1] = True
                parent_score_test = fmt_solver_fill_column_unwrap_test_state(*static_test_state_args, tuple(state_gen))
                if parent_score_test is not None:
                    return parent_score_test

                state_gen[1] = False
            state_gen[index_wrap_hint] = False

        if (
                # It only makes sense to run this logic if there are multiple arguments to deal with.
                (len(state_curr) > 2) and
                # All arguments were wrapped onto separate lines.
                (False not in state_curr[1:]) and
                # We never want to move comments or white-space around,
                # any other line may include comments or blank lines.
                isinstance(node.nodes[0], NODE_CODE_TYPES) and
                isinstance(node.nodes[1], NODE_CODE_TYPES)
        ):
            # Provide:
            #
            #    (function-call a
            #                   b
            #                   c
            #                   d
            #                   e)
            #

            # Skip when the indent hint is set as this means wrapping on the first line
            # doesn't typically make sense.
            has_indent = False
            if (fn_data := node.fn_arity_get_from_first_symbol(cfg.defs)) is not None:
                # May be `FnArity` or a list.
                hints = fn_data[3]
                if hints and 'indent' in hints:
                    has_indent = True

            # Don't attempt to wrap anything that defines an indent.
            # It's too likely to lead to:
            #
            #     (some-macro awkwardly
            #        indented
            #      arguments)
            if not has_indent:
                state_gen = [True] * len(state_curr)
                state_gen[0] = False
                state_gen[1] = False

                parent_score_test = fmt_solver_fill_column_unwrap_test_state(*static_test_state_args, tuple(state_gen))
                if parent_score_test is not None:
                    return parent_score_test

    return None


def fmt_solver_fill_column_unwrap_recursive(
        cfg: FmtConfig,
        node_parent: NdSexp,
        level: int,
        trailing_parens: int,
        visited: set[int],
) -> None:
    '''
    Wrap lines that were split back onto the same line.
    In some cases this is useful because:

    - A nested S-expression is wrapped to fit.
    - On of the S-expressions containing it is later wrapped onto multiple lines (de-indenting in some cases).
    - There is no longer a need for the original nested S-expression to be wrapped.

    In this case it makes sense to set the wrapping to previously known valid states.
    Note that testing this is quite computationally expensive, so add additional checks with care.
    '''
    if node_parent.wrap_locked:
        return
    if not node_parent.nodes_only_code:
        return

    if node_parent.force_newline:
        if fmt_solver_fill_column_unwrap_aggressive(cfg, node_parent, level, trailing_parens, visited):
            return

    node_trailing_parens = node_parent.node_last_for_trailing_parens_test()

    level_next_data = node_parent.calc_nodes_level_next(cfg, level)
    level_next_data_last = len(level_next_data) - 1
    for i, node in enumerate(node_parent.nodes):
        if isinstance(node, NdSexp):
            level_next = level_next_data[min(i, level_next_data_last)]
            fmt_solver_fill_column_unwrap_recursive(
                cfg,
                node,
                level_next,
                trailing_parens + 1 if node is node_trailing_parens else 0,
                visited,
            )
            if not cfg.style.use_native:
                if node.force_newline:
                    node_parent.force_newline = True

    # If this isn't a newline, let a parent node handle it
    # otherwise leading nodes won't be included.
    parent_score_curr = -1
    if node_parent.force_newline:
        if level == 0:
            # When at level zero, include ourself,
            # needed unless this function is called with a single `root` node.
            nodes_with_prior_state = node_parent.iter_nodes_recursive_with_prior_state_and_self
        else:
            nodes_with_prior_state = node_parent.iter_nodes_recursive_with_prior_state

        for node in nodes_with_prior_state(visited):
            # Locked nodes should _never_ have a prior state.
            # If this happens it's an internal error.
            assert not node.wrap_locked

            # Calculate the score to compare new states to.
            if parent_score_curr == -1:
                parent_score_curr = node_parent.fmt_check_exceeds_column_max(
                    cfg,
                    level,
                    trailing_parens,
                    calc_score=True,
                )

            # Attempt to load or calculate a better state
            parent_score_test = fmt_solver_fill_column_unwrap_test_state_permutations(
                cfg,
                node_parent,
                node,
                level,
                trailing_parens,
                parent_score_curr,
            )
            if parent_score_test is not None:
                assert parent_score_test <= parent_score_curr
                parent_score_curr = parent_score_test

            # Avoid checking these ever again - either they were useful or not.
            node.prior_states.clear()


def fmt_solver_newline_constraints_apply(
        node_parent: NdSexp,
        cfg: FmtConfig,
        check_parent_multiline: bool,
) -> bool:
    '''
    Add newlines based on constraints (untreated to the fill-column).
    Return true when a change was made.
    '''
    use_native = cfg.style.use_native

    changed = False

    # # From `fmt_solver_fill_column_wrap_recursive`, technically correct but not needed.
    # if node_parent.wrap_all_or_nothing_hint and node_parent.is_multiline():
    #     apply_relaxed_wrap(node_parent, cfg.style)

    # Finally, if the node is multi-line, ensure it's also split at the hinted location.
    # Ensures we don't get:
    #     (or foo
    #        (bar
    #          bob))
    #
    # Instead it's all one line:
    #     (or foo (bar bob))
    #
    # Or both are wrapped onto a new line:
    #     (or
    #       foo
    #       (bar bob))
    #
    # ... respecting the hint for where to split.
    #
    if check_parent_multiline and node_parent.is_multiline():
        # First node is a symbol (matches a function signature):
        if len(node_parent.nodes_only_code) > node_parent.index_wrap_hint:
            if (
                    # NOTE: that we may want to handle data differently,
                    # currently matching logic for values inside `let` & `cond` works nicely.
                    node_parent.maybe_function_call(allow_data=True) or
                    isinstance(node_parent.nodes_only_code[0], NdSexp)
            ):
                node = node_parent.nodes_only_code[node_parent.index_wrap_hint]
                if not node.force_newline:
                    node.force_newline = True
                    changed = True

        # Ensure colon prefixed arguments are on new-lines
        # if the block is multi-line.
        #
        # When multi-line, don't use this formatting:
        #     (foo
        #       :keyword long-value-which-causes-next-line-to-wrap
        #       :other value :third value)
        #
        # Instead do:
        #     (foo
        #       :keyword long-value-which-causes-next-line-to-wrap
        #       :other value
        #       :third value)
        # But don't do:
        #     (:eval
        #        ...)

        # node_prev = None
        for node in node_parent.nodes_only_code[node_parent.index_wrap_hint:]:
            if not node.force_newline:
                if (
                        isinstance(node, NdSymbol) and
                        node.data.startswith(':') and
                        (node is not node_parent.nodes_only_code[0])
                ):
                    # if (
                    #         isinstance(node_prev_prev, NdSymbol) and
                    #         node_prev_prev.data.startswith(':')
                    # ):
                    if not node.force_newline:
                        node.force_newline = True
                        changed = True

            # node_prev = node

    if use_native:
        changed |= node_parent.flush_newlines_from_nodes_for_native()

    return changed


def fmt_solver_newline_constraints_apply_recursive(
        node_parent: NdSexp,
        cfg: FmtConfig,
        check_parent_multiline: bool,
) -> None:
    '''
    Perform line wrapping, taking indent-levels into account.
    '''

    # Keep locked nodes as-is.
    if node_parent.wrap_locked:
        return

    # First handle S-expressions one at a time, then all of them.
    # not very efficient, but it avoids over wrapping.

    force_newline = False

    for node in node_parent.nodes:
        if isinstance(node, NdSexp):
            fmt_solver_newline_constraints_apply_recursive(node, cfg, check_parent_multiline)
        force_newline |= node.force_newline

    if cfg.style.use_native:
        pass
    else:
        if force_newline:
            node_parent.force_newline = True

    if fmt_solver_newline_constraints_apply(node_parent, cfg, check_parent_multiline):
        if not cfg.style.use_native:
            node_parent.force_newline = True


def fmt_solver_for_root_node(cfg: FmtConfig, node: NdSexp) -> None:
    '''
    Calculate line wrapping for top-level nodes.
    '''
    # It's important to store the initial newlines before `check_parent_multiline` is applied,
    # to allow restoring to a point that doesn't include lines wrapped by the parent state.
    apply_rules_recursive(cfg, node)
    fmt_solver_newline_constraints_apply_recursive(node, cfg, check_parent_multiline=False)
    # Ensures unwrap never attempts to change this node back to a previous state (as it's locked).
    for n in node.iter_nodes_recursive_with_self_only_sexp_without_wrap_locked():
        if n.nodes_only_code:
            n.prior_states.append(n.newline_state_get())

    fmt_solver_newline_constraints_apply_recursive(node, cfg, check_parent_multiline=True)

    fmt_solver_fill_column_wrap_recursive(cfg, node, 0, 0)
    fmt_solver_newline_constraints_apply_recursive(node, cfg, check_parent_multiline=True)

    if cfg.style.use_native and cfg.fill_column != 0:
        fmt_solver_fill_column_unwrap_recursive(cfg, node, 0, 0, set())
    if USE_PARANOID_ASSERT:
        if node.flush_newlines_from_nodes_for_native_recursive():
            raise RuntimeError('this should be maintained while unwrapping!')


def fmt_solver_for_root_node_multiprocessing(cfg: FmtConfig, node_group: Sequence[NdSexp]) -> Sequence[str]:
    '''
    A version of ``fmt_solver_for_root_node`` which supports multi-processing.
    '''
    result_group = []
    ctx = FmtWriteCtx(cfg)
    for node in node_group:
        fmt_solver_for_root_node(cfg, node)
        data: list[str] = []
        node.fmt(ctx, data.append, 0)
        result_group.append(''.join(data))
        del data
    return result_group


# ------------------------------------------------------------------------------
# Formatting Utilities

# Currently this always represents a blank line.
class NdWs(Node):
    '''
    This represents white-space to be kept in the output.
    '''
    __slots__ = ()

    def __init__(self, line: int):
        self.original_lines = line, line

    def __repr__(self) -> str:
        return '{:s}({:s} type=blank_line)'.format(
            self.__class__.__name__,
            Node.__repr__(self),
        )

    def calc_force_newline(self, style: FmtStyle) -> None:
        # False because this forces it's own newline
        self.force_newline = True

    def fmt(
            self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn('\n')
        ctx.line += 1
        ctx.column = 0
        ctx.is_newline = True


class NdComment(Node):
    '''
    Code-comment.
    '''
    __slots__ = (
        'data',
        'is_own_line',
    )

    data: str
    is_own_line: bool

    def __init__(self, line: int, data: str, is_own_line: bool):
        self.original_lines = line, line
        self.data = data
        self.is_own_line = is_own_line

    def __repr__(self) -> str:
        return '{:s}({:s} data=\'{:s}\')'.format(
            self.__class__.__name__,
            Node.__repr__(self),
            self.data,
        )

    def calc_force_newline(self, style: FmtStyle) -> None:
        self.force_newline = self.is_own_line

    def fmt(
            self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn(';')
        write_fn(self.data)
        ctx.column += 1 + len(self.data)
        ctx.is_newline = False


class NdString(Node):
    '''
    A string literal.
    '''
    __slots__ = (
        'data',
        'lines',
    )

    data: str
    lines: int

    def __init__(self, lines: tuple[int, int], data: str):
        self.original_lines = lines
        self.data = data
        # self.lines = self.data.count('\n')

        # No need to count '\n'.
        self.lines = lines[1] - lines[0]

    def __repr__(self) -> str:
        return '{:s}({:s} \'{:s}\')'.format(
            self.__class__.__name__,
            Node.__repr__(self),
            self.data,
        )

    def calc_force_newline(self, style: FmtStyle) -> None:
        if USE_WRAP_LINES:
            self.force_newline = ((not self.data.startswith('\n')) and self.lines > 0)
        else:
            self.force_newline = False

    def fmt(
            self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn('"')
        write_fn(self.data)
        write_fn('"')
        ctx.is_newline = False
        ctx.line += self.lines
        if self.lines:
            # Add one for the trailing quote,
            # that's accounted for as '\n' has a length of 1.
            ctx.column += (len(self.data) - self.data.rfind('\n'))
        else:
            # Add 2 for the quotes.
            ctx.column += len(self.data) + 2


class NdSymbol(Node):
    '''
    This represents any identifier that isn't an S-expression, string, comment or white-space.
    '''
    __slots__ = (
        'data',
    )

    data: str

    def __init__(self, line: int, data: str):
        self.original_lines = line, line
        self.data = data

    def __repr__(self) -> str:
        return '{:s}({:s} data=\'{:s}\')'.format(
            self.__class__.__name__,
            Node.__repr__(self),
            self.data,
        )

    def calc_force_newline(self, style: FmtStyle) -> None:
        self.force_newline = False

    def fmt(
            self,
            ctx: FmtWriteCtx,
            write_fn: Callable[[str], Any],
            level: int,
            *,
            test: bool = False,
    ) -> None:
        write_fn(self.data)
        ctx.column += len(self.data)
        ctx.is_newline = False


NODE_CODE_TYPES = (NdSymbol, NdString, NdSexp)


# ------------------------------------------------------------------------------
# File Diffing

def diff_range_calc(data_src: str, data_dst: str) -> tuple[str, int, int]:
    '''
    Takes a data source & destination,
    returns the sub-range of ``data_dst`` which is different from ``data_src``
    and the length of the beginning and end spans which match.
    '''
    # NOTE: this seems as if it might be slow, but in practice even files of 100's of
    # KB only take 1/100'th of a second or so to test, so it's not really worth optimizing
    # unless cases are found where it's a bottleneck.
    data_len_min = min(len(data_src), len(data_dst))
    if not data_dst:
        return data_dst, 0, 0
    i = 0
    for i in range(data_len_min):
        if data_src[i] != data_dst[i]:
            break

    # The buffers are a complete match.
    if i + 1 == data_len_min and len(data_src) == len(data_dst):
        return "", -1, -1

    ofs_beg = max(0, i - 1)
    i = len(data_src) - 1
    j = len(data_dst) - 1
    ofs_end = 0
    for _ in range(data_len_min - ofs_beg):
        if data_src[i] != data_dst[j]:
            break
        i -= 1
        j -= 1
    # As if this was incremented each iteration.
    ofs_end = (len(data_src) - 1) - i
    return data_dst[ofs_beg:len(data_dst) - ofs_end], ofs_beg, ofs_end


# ------------------------------------------------------------------------------
# File Parsing

def parse_file(fh: TextIO) -> tuple[str, NdSexp]:
    '''
    Parse the file ``fh``, returning:
    - The first un-parsed line (for ELISP files starting with a bang (``#!``)).
    - The ``NdSexp`` (root node of the S-expression tree).
    '''
    from io import StringIO

    line = 0

    # Fake top level S-expression to populate, (brackets aren't used).
    # Second line is updated on closing brace.
    root = NdSexp((line, line), brackets='()')

    # Current S-expressions.
    sexp_ctx = [root]
    sexp_level = 0

    line_has_contents = False

    # Special case, a lisp file with a shebang.
    first_line_unparsed = ''
    c_peek: str | None = fh.read(1)
    if c_peek == '#':
        first_line_chars = [c_peek]
        c_peek = None
        while c := fh.read(1):
            first_line_chars.append(c)
            if c == '\n':
                line += 1
                break
        first_line_unparsed = ''.join(first_line_chars)
        del first_line_chars

    while c := c_peek or fh.read(1):
        c_peek = None
        match c:
            case '(':  # Open S-expression.
                sexp_ctx.append(NdSexp((line, line), '()'))
                sexp_ctx[sexp_level].nodes.append(sexp_ctx[-1])
                sexp_level += 1
                line_has_contents = True

            case '[':  # Open vector.
                # Second line is updated on closing brace.
                sexp_ctx.append(NdSexp((line, line), '[]'))
                sexp_ctx[sexp_level].nodes.append(sexp_ctx[-1])
                sexp_level += 1
                line_has_contents = True
            case ')':  # Close S-expression.
                if sexp_level == 0:
                    raise FmtException('additional closing brackets, line {:d}'.format(line))
                node = sexp_ctx.pop()
                if node.brackets[0] != '(':
                    raise FmtException(
                        'closing bracket "{:s}" line {:d}, unmatched bracket types, expected ")"'.format(c, line)
                    )
                if node.original_lines[1] != line:
                    node.original_lines = node.original_lines[0], line
                del node
                sexp_level -= 1
                line_has_contents = True
            case ']':  # Close vector.
                if sexp_level == 0:
                    raise FmtException('additional closing brackets, line {:d}'.format(line))
                node = sexp_ctx.pop()
                if node.brackets[0] != '[':
                    raise FmtException(
                        'closing bracket "{:s}" line {:d}, unmatched bracket types, expected "]"'.format(c, line)
                    )
                if node.original_lines[1] != line:
                    node.original_lines = node.original_lines[0], line
                del node
                sexp_level -= 1
                line_has_contents = True
            case '"':  # Open & close string.
                line_beg = line
                data = StringIO()
                is_slash = False
                while (c := fh.read(1)):
                    if c == '"' and not is_slash:
                        break
                    data.write(c)
                    if c == '\\':
                        is_slash = not is_slash
                    else:
                        is_slash = False
                        if c == '\n':
                            line += 1

                if not c:
                    raise FmtException('unterminated string literal at line {:d}'.format(line))

                sexp_ctx[sexp_level].nodes.append(NdString((line_beg, line), data.getvalue()))
                del line_beg, data, is_slash, c
                line_has_contents = True
            case ';':  # Comment.
                data = StringIO()
                while (c_peek := fh.read(1)) not in {'', '\n'}:
                    c = c_peek
                    c_peek = None
                    data.write(c)

                is_own_line = not line_has_contents
                sexp_ctx[sexp_level].nodes.append(NdComment(line, data.getvalue(), is_own_line))
                del data, is_own_line
                line_has_contents = True
            case '\n':  # White-space (newline).
                line += 1
                # Respect blank lines up until the limit.
                if line_has_contents is False:
                    sexp_ctx[sexp_level].nodes.append(NdWs(line))
                line_has_contents = False
            case ' ' | '\t':  # White-space (space, tab) - ignored.
                pass
            case _:  # Symbol (any other character).
                data = StringIO()
                is_slash = False
                while c:
                    if c == '\\':
                        is_slash = not is_slash
                    else:
                        is_slash = False
                    data.write(c)
                    c_peek = fh.read(1)
                    if not c_peek:
                        break
                    if c_peek == '\n':
                        break
                    if not is_slash:
                        if c_peek in {
                                '(', ')',
                                '[', ']',
                                ';',
                                ' ', '\t',
                                # Lisp doesn't require spaces are between symbols and quotes.
                                '"',
                        }:
                            break

                    c = c_peek
                    c_peek = None

                text = data.getvalue()
                del data

                # Special support for character literals.
                if text[0] == '?':
                    if c_peek:
                        # Always include the next character
                        # even if it's normally a delimiting character such as ';', '"'
                        # (un-escaped literal support, even allowing for `?;` or `?\C-;`).
                        if (
                                # Support `? ` and `?;`.
                                (len(text) == 1) or
                                # Support `?\C- ` and `?\C-;` and `?\C-\s- `.
                                (len(text) >= 4 and (
                                    text[-1] == '-' and
                                    text[-2].isalpha() and
                                    text[-3] == '\\')
                                 )
                        ):
                            text = text + c_peek
                            c_peek = None

                sexp_ctx[sexp_level].nodes.append(NdSymbol(line, text))
                del is_slash
                line_has_contents = True

    if sexp_level != 0:
        raise FmtException('unbalanced S-expressions at file-end, found {:d} levels, expected 0'.format(sexp_level))

    # Maybe unnecessary, do this for correctness.
    if root.original_lines[1] != line:
        root.original_lines = root.original_lines[0], line

    root.finalize_parse()

    return first_line_unparsed, root


def write_file(cfg: FmtConfig, fh: TextIO, root: NdSexp, first_line: str) -> None:
    '''
    Write the ``root`` S-expression into ``fh``.
    '''

    ctx = FmtWriteCtx(cfg)

    if first_line:
        fh.write(first_line)
    root.fmt(ctx, fh.write, -1)
    fh.write('\n')


def node_group_by_count(root: NdSexp, *, chunk_size_limit: int) -> Sequence[Sequence[NdSexp]]:
    '''
    Return top-level nodes from ``root``, grouped by ``chunk_size_limit``.
    '''
    assert chunk_size_limit > 0

    if chunk_size_limit == 1:
        return [
            [node] for node in root.nodes_only_code
            if isinstance(node, NdSexp)
        ]

    chunk_size_curr = 0
    node_group_list: list[list[NdSexp]] = [[]]
    for node in root.nodes_only_code:
        if isinstance(node, NdSexp):
            count_recursive = node.count_recursive()
            if count_recursive >= chunk_size_limit:
                # This block can be it's own group, keep the current one at the end.
                group = node_group_list[-1]
                node_group_list[-1] = [node]
                node_group_list.append(group)

            elif chunk_size_curr >= chunk_size_limit:
                chunk_size_curr = count_recursive
                node_group_list.append([node])
            else:
                chunk_size_curr += count_recursive
                node_group_list[-1].append(node)

    # It's possible all blocks where over the size limit and an empty group remains at the end.
    if len(node_group_list[-1]) == 0:
        node_group_list.pop()

    return node_group_list


def do_wrap_level_0_multiprocessing(cfg: FmtConfig, root: NdSexp, parallel_jobs: int) -> None:
    '''
    A version of ``do_wrap_level_0`` which uses multi-processing.
    '''
    node_group_list = node_group_by_count(root, chunk_size_limit=256)

    args = [(cfg, node_group) for node_group in node_group_list]

    import multiprocessing

    if parallel_jobs == 0:
        parallel_jobs = multiprocessing.cpu_count()

    with multiprocessing.Pool(processes=parallel_jobs) as pool:
        result_group_list = pool.starmap(fmt_solver_for_root_node_multiprocessing, args)
        for node_group, result_group in zip(node_group_list, result_group_list):
            for node, result in zip(node_group, result_group):
                node.fmt_cache = result


def do_wrap_level_0(cfg: FmtConfig, root: NdSexp) -> None:
    '''
    Calculate wrapping for all nodes.
    '''
    for node in root.nodes_only_code:
        if isinstance(node, NdSexp):
            fmt_solver_for_root_node(cfg, node)


def format_file(
        filepath: str,
        cfg: FmtConfig,
        *,
        line_range: tuple[int, int] | None = None,
        parallel_jobs: int = 0,
        use_stdin: bool = False,
        use_stdout: bool = False,
        use_diff_range: bool = False,
) -> None:
    '''
    Main file formatting function.
    '''
    from io import StringIO

    # Needed as files may contain '\r' only, see emacs own:
    # `lisp/cedet/semantic/grammar-wy.el`
    newline = '\r\n' if (os.name == 'nt') else '\n'

    diff_range_data_src = ''
    if use_diff_range:
        assert use_stdin
        diff_range_data_src = sys.stdin.read()
        fh_as_text = StringIO(diff_range_data_src, newline=newline)
        first_line, root = parse_file(fh_as_text)
        del fh_as_text
    else:
        if use_stdin:
            first_line, root = parse_file(sys.stdin)
        else:
            with open(filepath, 'r', encoding='utf-8', newline=newline) as fh:
                first_line, root = parse_file(fh)

    if USE_EXTRACT_DEFS:
        parse_local_defs(cfg.defs, root)

    # Check for a newline at file start.
    # If so, this will disable the settings such as lexical binding.
    # The intention of re-formatting is not to make any functional changes,
    # so it's important to add the blank line back.
    starts_with_bank_line = False
    if root.nodes:
        if isinstance(root.nodes[0], NdWs):
            starts_with_bank_line = True

    root.finalize_style(cfg)

    if starts_with_bank_line:
        # Add back the blank line.
        root.nodes.insert(0, NdWs(0))

    root.calc_force_newline(cfg.style)

    apply_comment_force_newline(root)

    apply_rules_from_comments(root)

    # Without assertions this is redundant, however it's needed for
    # the assertion to succeed when `len(root.nodes_only_code) == 1`.
    root.force_newline = True

    if USE_WRAP_LINES:

        # All root level nodes get their own line always.
        for node in root.nodes_only_code:
            node.force_newline = True

        # Optionally lock content outside the range.
        if line_range is not None:
            for node in root.nodes_only_code:
                if isinstance(node, NdSexp):
                    if (
                        # Node begins after the range of interest.
                        node.original_lines[0] > line_range[1] or
                        # Node ends before the range of interest.
                        node.original_lines[1] < line_range[0]
                    ):
                        node.wrap_locked = True
                        apply_rules_recursive_locked(node)

        if cfg.use_multiprocessing:
            # Copying this information can be quite slow, prune unused items first.
            fn_used: set[str] = set()
            scan_used_fn_defs(cfg.defs, root, fn_used)
            cfg.defs.prune_unused(fn_used)

            do_wrap_level_0_multiprocessing(cfg, root, parallel_jobs)
        else:
            do_wrap_level_0(cfg, root)

        if USE_PARANOID_ASSERT:
            for node in root.iter_nodes_recursive_only_sexp():
                assert bool(node.prior_states) is False

    if cfg.style.use_native:
        pass
    else:
        if not cfg.use_multiprocessing:
            assert root.flush_newlines_from_nodes_recursive() is False

    if use_diff_range:
        assert use_stdout
        fh_as_text = StringIO(newline=newline)
        write_file(cfg, fh_as_text, root, first_line)
        diff_range_data_dst = fh_as_text.getvalue()

        diff_range_data_dst_sub, diff_ofs_beg, diff_ofs_end = diff_range_calc(diff_range_data_src, diff_range_data_dst)

        if diff_ofs_beg == -1 and diff_ofs_end == -1:
            # No change.
            sys.stdout.write("(-1 . -1)\n")
        else:
            sys.stdout.write("({:d} . {:d})\n".format(
                diff_ofs_beg + 1,
                (len(diff_range_data_src) - diff_ofs_end) + 1,
            ))
            sys.stdout.write(diff_range_data_dst_sub)
    else:
        if use_stdout:
            write_file(cfg, sys.stdout, root, first_line)
        else:
            with open(filepath, 'w', encoding='utf-8', newline=newline) as fh:
                write_file(cfg, fh, root, first_line)


# ------------------------------------------------------------------------------
# Argument Parsing


def argparse_create() -> argparse.ArgumentParser:
    '''
    Create the argument parser used to format from the command line.
    '''

    # When `--help` or no arguments are given, print this help.
    usage_text = 'Format emacs-lisp.'

    epilog = (
        'This program formats emacs lisp, from the standard input, '
        'or operating on files, in-place.'
    )

    parser = argparse.ArgumentParser(description=usage_text, epilog=epilog)

    parser.add_argument(
        '--fmt-defs-dir',
        dest='fmt_defs_dir',
        metavar='DIR',
        default='',
        type=str,
        required=False,
        help='Directory used for storing definitions.',
    )
    parser.add_argument(
        '--fmt-defs',
        dest='fmt_defs',
        metavar='FILES',
        default='',
        type=str,
        required=False,
        help=(
            'Definition filenames within "--fmt-defs-dir" when only a filename is specified, '
            'otherwise absolute paths are used. '
            'split by PATH_SEPARATOR. '
            '(internal use, this is written by Emacs).'
        ),
    )
    parser.add_argument(
        '--fmt-style',
        dest='fmt_style',
        default='native',
        help=(
            'Formatting style in where "native" mimics EMACS default indentation and "fixed" '
            'formats using a simple 2-space for each nested block rule.'
        ),
        required=False,
        choices=('native', 'fixed')
    )

    parser.add_argument(
        '--quiet',
        dest='use_quiet',
        default=False,
        action='store_true',
        required=False,
        help='Don\'t output any status messages.',
    )

    parser.add_argument(
        '--fmt-trailing-parens',
        dest='fmt_use_trailing_parens',
        default=False,
        action='store_true',
        required=False,
        help='Give each trailing parenthesis its own line.',
    )

    parser.add_argument(
        '--fmt-quoted',
        dest='fmt_use_quoted',
        default=1,
        type=int,
        required=False,
        help='Format quoted S-expressions.',
    )

    parser.add_argument(
        '--fmt-fill-column',
        dest='fmt_fill_column',
        default=99,
        nargs='?',
        type=int,
        required=False,
        help='Maximum column width (zero disables).',
    )

    parser.add_argument(
        '--fmt-empty-lines',
        dest='fmt_empty_lines',
        default=2,
        nargs='?',
        type=int,
        required=False,
        help='Maximum column width.',
    )
    parser.add_argument(
        '--fmt-line-range',
        dest='fmt_line_range',
        default='0:0',
        type=str,
        required=False,
        help=(
            'The line range (starting at 1 & inclusive) to format. '
            'Lines outside this range will output syntactically correct but the formatting is undefined. '
            'Note that this is intended for callers that are only only formatting a sub-region, '
            'in this case additional processing for anything outside this sub-region can be avoided.'
            'A value of 0-0 is ignored.'
        ),
    )
    parser.add_argument(
        '--parallel-jobs',
        dest='parallel_jobs',
        default=0,
        nargs='?',
        type=int,
        required=False,
        help='The number of parallel processes to use (zero to select automatically, -1 to disable multi-processing).',
    )

    parser.add_argument(
        '--stdin',
        dest='use_stdin',
        default=False,
        action='store_true',
        required=False,
        help='Use the stdin for file contents instead of the file name passed in.',
    )

    parser.add_argument(
        '--stdout',
        dest='use_stdout',
        default=False,
        action='store_true',
        required=False,
        help='Use the stdout to output the file contents instead of the file name passed in.',
    )

    parser.add_argument(
        '--use-diff-range',
        dest='use_diff_range',
        default=False,
        action='store_true',
        required=False,
        help='Calculate a diff range.',
    )
    parser.add_argument(
        '--exit-code',
        dest='exit_code',
        default=0,
        nargs='?',
        type=int,
        required=False,
        help='Exit code to use upon successfully re-formatting',
    )

    parser.add_argument(
        'files',
        nargs=argparse.REMAINDER,
        help='All trailing arguments are treated as file paths to format.'
    )
    return parser


# ------------------------------------------------------------------------------
# Main Function

def main_generate_defs() -> bool:
    '''
    A utility to generate definitions from a file without loading it.
    Needed when definitions are requested but the code may not be loaded into emacs.

    As it's not expected that a formatting tool would executed arbitrary (untrusted)
    LISP code. It's necessary to extract definitions of untrusted code so that formatting
    can be correctly performed.
    '''
    import json

    try:
        i = sys.argv.index('--gen-defs')
    except ValueError:
        return False

    args_rest = sys.argv[i + 1:]

    while args_rest:
        file_input = args_rest.pop(0)
        file_output = args_rest.pop(0)

        defs = FmtDefs(fn_arity={})

        with open(file_input, 'r', encoding='utf-8') as fh:
            _, root = parse_file(fh)

            parse_local_defs(defs, root)

        with open(file_output, 'w', encoding='utf-8') as fh:
            fh.write('{\n')
            fh.write('"functions": {\n')
            is_first = True
            for key, val in defs.fn_arity.items():
                if is_first:
                    is_first = False
                else:
                    fh.write(',\n')
                symbol_type, nargs_min, nargs_max, hints = val
                nargs_min_str = str(nargs_min) if isinstance(nargs_min, int) else '"{:s}"'.format(nargs_min)
                nargs_max_str = str(nargs_max) if isinstance(nargs_max, int) else '"{:s}"'.format(nargs_max)
                fh.write('"{:s}": ["{:s}", {:s}, {:s}, {:s}]'.format(
                    key, symbol_type, nargs_min_str, nargs_max_str, json.dumps(hints)
                ))
            fh.write('')
            fh.write('}\n')  # 'functions'.
            fh.write('}\n')

    return True


def main() -> None:
    '''
    The main function which handles problems parsing the document gracefully.
    Other kinds of errors are not expected so will show a typical (less user friendly) trace-back.
    '''
    if main_generate_defs():
        return

    args = argparse_create().parse_args()

    if args.use_stdin and args.use_stdout:
        if args.files:
            sys.stderr.write(
                'The \'--files\' argument cannot be used when both stdin and stdout are used {!r}\n'.format(args.files))
            sys.exit(1)
    elif args.use_stdin or args.use_stdout:
        if len(args.files) != 1:
            sys.stderr.write(
                'The \'--stdin\' & \'--stdout\' arguments are limited to a single file {!r}\n'.format(args.files))
            sys.exit(1)
    elif not args.files:
        sys.stderr.write(
            'No files passed in, pass in files or use both \'--stdin\' & \'--stdout\'\n')
        sys.exit(1)

    if args.use_diff_range:
        if not (args.use_stdin and args.use_stdout):
            sys.stderr.write(
                '\'--use-diff-range\' can only be used with both \'--stdin\' & \'--stdout\'\n')
            sys.exit(1)

    line_range: tuple[int, int] | None = None
    if args.fmt_line_range:
        if ":" not in args.fmt_line_range:
            sys.stderr.write(
                'The argument \'--fmt-line-range\' expects numbers to be separated by a \':\'\n')
            sys.exit(1)
        line_range_args = args.fmt_line_range.partition(':')[0::2]
        try:
            line_range = int(line_range_args[0]) - 1, int(line_range_args[1]) - 1
        except ValueError as ex:
            sys.stderr.write(
                'The argument \'--fmt-line-range\' could not interpret numbers ({!r})\n'.format(ex))
            sys.exit(1)
        if line_range[0] > line_range[1]:
            sys.stderr.write(
                'The argument \'--fmt-line-range\' found the start line was larger than the end\n')
            sys.exit(1)

        if line_range == (-1, -1):
            line_range = None
        del line_range_args

    defs_orig = FmtDefs(fn_arity={})

    if args.fmt_defs:
        defs_orig.from_json_files(
            os.path.join(args.fmt_defs_dir, filename) if (os.sep not in filename) else filename
            for filename in args.fmt_defs.split(os.pathsep)
        )

    count_files_error = 0
    count_files_total = 0

    for i, filepath in enumerate(args.files or ('',)):

        # Make a copy of the original definition if this is one of many files.
        if i == 0:
            defs = defs_orig if len(args.files) <= 1 else defs_orig.copy()
        elif i + 1 < len(args.files):
            defs = defs_orig.copy()
        else:
            defs = defs_orig  # Last iteration, no need to copy.

        # Use the `stderr` to avoid conflicting with `stdout` when it's set.
        if (not args.use_quiet) and (not args.use_stdout):
            sys.stdout.write('{:s}\n'.format(filepath))

        cfg = FmtConfig(
            style=FmtStyle(
                use_native=args.fmt_style == 'native',
            ),
            use_trailing_parens=args.fmt_use_trailing_parens,
            use_quoted=bool(args.fmt_use_quoted),
            use_multiprocessing=args.parallel_jobs >= 0,
            fill_column=max(0, args.fmt_fill_column),
            use_wrap=args.fmt_fill_column > 0,
            empty_lines=args.fmt_empty_lines,
            defs=defs,
        )

        try:
            format_file(
                filepath,
                cfg=cfg,
                parallel_jobs=args.parallel_jobs,
                line_range=line_range,
                use_stdin=args.use_stdin,
                use_stdout=args.use_stdout,
                use_diff_range=args.use_diff_range,
            )
        except FmtException as ex:
            if filepath:
                sys.stderr.write('Error: {:s} in {:s}\n'.format(str(ex), filepath))
            else:
                sys.stderr.write('Error: {:s}\n'.format(str(ex)))
            count_files_error += 1

        count_files_total += 1

    if count_files_error:
        if count_files_total > 1:
            sys.stderr.write('Error: {:d} of {:d} files failed to format!\n'.format(
                count_files_error,
                count_files_total,
            ))
        sys.exit(1)

    sys.exit(args.exit_code)


if __name__ == '__main__':
    main()
