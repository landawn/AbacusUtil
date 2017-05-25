# Abacus-Util

A general programming library/framework in Java. It's simple, powerful and easy to use with concise APIs

Docs: http://www.landawn.com

## Features:
* Benchmark test.
* Most daily used APIs: [IOUtil][], Multiset, LongMultiset, BiMap, Multimap, ImmutableList, ImmutableSet, ImmutableMap, Sheet, Pair, Triple, Tuple, Splitter, Joiner, Builder, Difference, Profiler, AsyncExecutor, CompletableFuture, Futures, CodeGenerator, HttpClient, N ...

## Usage:
* Benchmark test:
One line: Easy, Simple and Accurate by running the test multiple rounds:
```java
Profiler.run(threadNum, loopNum, roundNum, "addByStream", () -> addByStream()).printResult();

public void addByStream() {
    assertEquals(499500, IntStream.range(1, 1000).reduce(0, (s, i) -> s += i));
}

```
And result:
```
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

[IOUtil]: http://www.landawn.com/IOUtil_view.html
