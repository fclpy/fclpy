"""Process-level hang detection for the ANSI runners.

Why this exists
---------------
`LoopWatchdog` (evaluation_loops_conditionals.py) answers "is *this loop*
going around too many times?" It counts iterations, and it evaluates the
120s warning and the 600s cap inside `tick()`, which runs **once per
iteration**. That makes it structurally blind to the case that actually
wedges a run: a loop stuck *inside a single iteration*.

That is not hypothetical. `cons/make-list.lsp`'s MAKE-LIST.ERROR.1 calls
`(make-list x)` over `*universe*`; a float size reached `range(int(size))`
and allocated cons cells until the process held 27GB. A LOOP was on the
stack the whole time, with a LoopWatchdog attached -- and control never
returned to `tick()`, so neither the warning nor the hard cap was ever
evaluated. The run produced no diagnostic at all and never escaped.

The same blindness covers every other unbounded path: deep recursion,
DOLIST/DOTIMES/DO-SYMBOLS (which have no LoopWatchdog at all -- only LOOP,
DO and DO* do), and any runaway inside a builtin.

So the question this module asks is not "is this loop spinning?" but
**"has the run made any progress recently?"** That has one answer point no
matter which form is stuck, so it needs one watchdog rather than one per
looping construct.

Progress is "the harness wrote something". RT prints each test's name as it
runs it, so hooking the output stream gives a per-test heartbeat without
modifying ansi-test and without a hot-path cost in the evaluator.

Two independent escapes, because the cheap one is not guaranteed:
  * a daemon thread, which produces the readable warning and can report the
    phase label; and
  * `faulthandler.dump_traceback_later(exit=True)`, whose timer thread is
    implemented in C and does not need to re-acquire the GIL to fire. If the
    interpreter is wedged inside one long C call, or the machine is
    thrashing badly enough to starve an ordinary thread, this is what still
    ends the run.

Both paths dump the traceback of every thread before exiting: a run that
dies without saying *where* costs another full run to diagnose.
"""

import faulthandler
import os
import sys
import threading
import time

from fclpy.system.shell import shell

# Matches evaluation_loops_conditionals.LOOP_TIMEOUT_WARNING /
# LOOP_TIMEOUT_ERROR so a hang is described the same way wherever it is
# caught. These are "seconds without progress", not total runtime -- a full
# suite run legitimately takes ~67 minutes.
WARN_AFTER = 120       # 2 minutes
KILL_AFTER = 600       # 10 minutes

_state = {
    'last_progress': None,   # perf_counter of the most recent progress
    'label': 'startup',      # coarse phase, set by the runner
    'last_rearm': 0.0,
    'warned': False,
    'armed': False,
}


def set_label(label):
    """Name the current phase, quoted back in any warning."""
    _state['label'] = label
    note_progress()


def note_progress():
    """Record that the run advanced. Cheap: two attribute writes."""
    now = time.perf_counter()
    _state['last_progress'] = now
    if _state['warned']:
        # Say so explicitly. A warning with no resolution line cannot be told
        # from a run that is still stuck -- the same defect LoopWatchdog's
        # RESOLVED/ABORTED lines exist to fix.
        _state['warned'] = False
        _log("RESOLVED: progress resumed (%s)" % _state['label'])

    # Re-arm the C-level backstop, throttled: it is a timer reset, not free,
    # and progress can be noted many times per second.
    if _state['armed'] and now - _state['last_rearm'] > 5.0:
        _state['last_rearm'] = now
        faulthandler.dump_traceback_later(KILL_AFTER, exit=True)


def _log(message):
    shell.print("\n*** WATCHDOG [%s] %s ***" % (time.strftime('%H:%M:%S'), message),
                file=shell.get_stderr())


def _watch(warn_after, kill_after):
    while True:
        time.sleep(2.0)
        last = _state['last_progress']
        if last is None:
            continue
        idle = time.perf_counter() - last

        if idle > kill_after:
            _log("ABORTING: no progress for %.0fs during %s. "
                 "Tracebacks of all threads follow." % (idle, _state['label']))
            faulthandler.dump_traceback()
            sys.stderr.flush()
            os._exit(3)

        if idle > warn_after and not _state['warned']:
            _state['warned'] = True
            _log("no progress for %.0fs during %s -- still waiting (hard stop "
                 "at %ds). Tracebacks of all threads follow."
                 % (idle, _state['label'], kill_after))
            faulthandler.dump_traceback()
            sys.stderr.flush()


def arm(warn_after=WARN_AFTER, kill_after=KILL_AFTER):
    """Start hang detection. Idempotent."""
    if _state['armed']:
        return
    _state['armed'] = True
    _state['last_progress'] = time.perf_counter()
    _state['last_rearm'] = time.perf_counter()
    faulthandler.dump_traceback_later(kill_after, exit=True)
    threading.Thread(target=_watch, args=(warn_after, kill_after),
                     daemon=True).start()


class _ProgressStream:
    """Wraps a text stream so every write counts as progress.

    Also forces line buffering. The 2026-08-15 hang was misattributed to
    NSUBST.9/.10 for exactly this reason: `run_all_tests.log` was
    block-buffered, so its last line was ~30 minutes and several files behind
    the form that was actually stuck. A hang diagnostic that points at the
    wrong test is worse than none.
    """

    def __init__(self, stream):
        self._stream = stream

    def write(self, text):
        written = self._stream.write(text)
        try:
            self._stream.flush()
        except (ValueError, OSError):
            pass
        note_progress()
        return written

    def __getattr__(self, name):
        return getattr(self._stream, name)


def watch_output():
    """Route stdout/stderr through the progress heartbeat."""
    sys.stdout = _ProgressStream(sys.stdout)
    sys.stderr = _ProgressStream(sys.stderr)
