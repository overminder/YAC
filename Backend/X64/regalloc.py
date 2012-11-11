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


def graph_coloring(flow_graph):
    i_graph = InterferenceGraph()

    # Build
    for liveness in flow_graph.get_liveness():
        i_graph.add_liveness(liveness)

    # Simplify
    node_stack = []
    while i_graph.not_empty():
        i_node = i_graph.find_simplify_node()
        if not i_node:
            # Cannot simplify more, spill this node.
            spill_node = i_graph.find_spill_node()
            spill_node.set_spill() # When shall we use it?
            node_stack.append(spill_node)
            continue
        i_graph.remove(i_node)
        node_stack.append(i_node)

    # Select
    actual_spills = []
    for i_node in node_stack:
        neighbour_colors = i_graph.get_neighbour_colors(i_node)
        usable_colors = neighbour_colors.get_usable_colors()
        if not usable_colors:
            # Actual spill
            i_node.color = None
            actual_spills.append(i_node)
        else:
            i_node.color = usable_colors[0]
        i_graph.add_back(i_node)

    # Startover
    if actual_spills:
        flow_graph.add_spill(actual_spills)
        return graph_coloring(flow_graph)
    else:
        return i_graph.replace_virtual_regs(flow_graph)

