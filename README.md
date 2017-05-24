# Abacus-Util

Docs: http://www.landawn.com

## Features:
* Benchmark test.

## Usage:
* Benchmark test.
One line, Easy, Simple and Accurate by running the test multiple rounds:
```java
final int threadNum = 1, loopNum = 30000, roundNum = 3;
Profiler.run(threadNum, loopNum, roundNum, "addByForLoop", () -> addByStream()).printResult();
Profiler.run(threadNum, loopNum, roundNum, "addByStream", () -> addByStream()).printResult();

public void addByForLoop() {
    int sum = 0;
    for (int i = 0; i < 1000; i++) {
        sum += i;
    }
    assertEquals(499500, sum);
}
public void addByStream() {
    assertEquals(499500, IntStream.range(1, 1000).reduce(0, (s, i) -> s += i));
}

```
And result:
```
========================================================================================================================
(unit: milliseconds)
threadNum=1; loops=30000
startTime: 2017-05-24 15:40:38.153
endTime:   2017-05-24 15:40:38.236
totalElapsedTime: 83.193

<method name>,  |avg time|, |min time|, |max time|, |0.01% >=|, |0.1% >=|,  |1% >=|,    |10% >=|,   |20% >=|,   |50% >=|,   |80% >=|,   |90% >=|,   |99% >=|,   |99.9% >=|, |99.99% >=|
addByForLoop,   0.003,      0.002,      0.096,      0.074,      0.016,      0.004,      0.003,      0.003,      0.003,      0.002,      0.002,      0.002,      0.002,      0.002,      

========================================================================================================================

========================================================================================================================
(unit: milliseconds)
threadNum=1; loops=30000
startTime: 2017-05-24 15:40:42.282
endTime:   2017-05-24 15:40:42.343
totalElapsedTime: 60.736

<method name>,  |avg time|, |min time|, |max time|, |0.01% >=|, |0.1% >=|,  |1% >=|,    |10% >=|,   |20% >=|,   |50% >=|,   |80% >=|,   |90% >=|,   |99% >=|,   |99.9% >=|, |99.99% >=|
addByStream,    0.002,      0.001,      0.499,      0.104,      0.015,      0.007,      0.003,      0.003,      0.001,      0.001,      0.001,      0.001,      0.001,      0.001,      

========================================================================================================================
```
