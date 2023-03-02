import os
from time import sleep

java_path = "weniger-krumme-touren/build/SimulatedAnnealing.jar"


def solveTA(path):
    if not os.path.exists(path + ".solution"):
        os.system(f"java -cp weniger-krumme-touren/build touren.SimulatedAnnealing {path}")
    with open(path + ".solution") as f:
        return tuple(map(int, f.readline()[1:-1].split(", ")))