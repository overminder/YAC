""" This is a imperative code for poletto's linear scan regalloc algorithm.
    I choose to write it down first so as to understand that algorithm
    better. However, what confuses me is that when and how we shall do
    register spilling and reloading...

    Seems that I need to read MOAR papers and think MOAR.
"""

class Interval(object):
    INVALID = -1
    def __init__(self, vreg):
        self.start = Interval.INVALID
        self.end = Interval.INVALID
        self.vreg = vreg # virtual register
        self.mreg = None # machine register

    def live_until_here(self, insn_index):
        if self.start == Interval.INVALID:
            self.start = self.end = insn_index
        else:
            self.end = insn_index

def linear_scan_reg_alloc(insn_list):
    regalloc = RegAlloc()
    liveness_intervals = determine_intervals(insn_list)
    liveness_intervals.sort(key=lambda interval: interval.start)
    actives = []
    for interval in liveness_intervals:
        # Expire old intervals
        for active in actives[:]:
            if active.end >= interval.start:
                break # since actives is sorted by INCREASING endpoint
            else:
                actives.remove(active)
                regalloc.free_reg(active.mreg)
        if regalloc.has_free_reg():
            interval.mreg = regalloc.alloc_reg()
            actives.append(interval)
            actives.sort(key=lambda interval: interval.end)
        else:
            # Here comes the spill code
            to_spill = actives[-1]
            if to_spill.end > interval.end: # spill to_spill
                # Get the instruction where interval's vreg becomes alive
                stack_loc = regalloc.new_stack_loc(to_spill.vreg)
                insn_list[interval.start].add_pre_spill(stack_loc)
                # XXX: but when shall we add reload code?
                actives.remove(to_spill)
                actives.append(interval)
                actives.sort(key=lambda interval: interval.end)
            else: # spill this interval
                stack_loc = regalloc.new_stack_loc(interval.vreg)

def determine_intervals(insn_list):
    res = defaultdict(lambda: Interval())
    # Determine the intervals
    for i, insn in enumerate(insn_list):
        liveness = insn.get_liveness()
        for v in liveness.lives:
            res[v].live_until_here(i)
    return res


