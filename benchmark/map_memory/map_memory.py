import numpy
from subprocess import Popen, PIPE
from sys import stderr, stdout
import os

os.chdir(os.path.dirname(os.path.realpath(__file__)))

COMMANDS = {
    "DartVM": lambda (limit):
        ["dart", "--old_gen_heap_size=%d" % limit,"map_memory.dart"],
    "NodeJS": lambda (limit):
        ["node", "--max-old-space-size=%d" % limit,"map_memory.js"],
    "PureJS": lambda (limit):
        ["node", "--max-old-space-size=%d" % limit,"map_memory.pure.js"],
}


def run_inst(mode, name, size, limit):
    stderr.write("%s, %s, size: %d, limit %d\n" % (mode, name, size, limit))
    process = Popen(
        COMMANDS[mode](limit) +
        [str(size), name],
        stdin=PIPE, stdout=PIPE, stderr=PIPE
    )
    return int(process.communicate()[0].split()[-1])


def run_test(mode, name, size):
    limits = list(range(100, 251, 30))
    allocations = []

    for limit in limits:
        allocations.append(run_inst(mode, name, size, limit))

    if(allocations[0] == 0):
        return float("inf")

    return (numpy.polyfit(allocations, limits, 1)[0])*(10**6)


stdout.write("mode,size,persistent,transient,json,map,\n")
for mode in ["NodeJS", "PureJS", "DartVM"]:
    for size in [100, 500, 1500, 5000, 20000]:
        stdout.write("%s,%d," % (mode, size))
        for name in ["persistent","transient", "json", "map",]:
            res = run_test(mode, name, size)
            stdout.write("%f," % (res));
        stdout.write("\n")
        stdout.flush()