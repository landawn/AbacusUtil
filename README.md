# Abacus-Util

A general programming library/framework in Java. It's simple, powerful and easy to use with concise APIs

Docs: http://www.landawn.com

## Features:

* Most daily used APIs: [IOUtil][], [Multiset][], [LongMultiset][], [BiMap][], [Multimap][], [ImmutableList][], [ImmutableSet][], [ImmutableMap][], [Sheet][], [Pair][], [Triple][], [Tuple][], [Splitter][], [Joiner][], [Builder][], [Difference][], [Profiler][], [AsyncExecutor][], [CompletableFuture][], [Futures][], [CodeGenerator][], [HttpClient][], [N][] ...

* Primitive List: [BooleanList][], [CharList][], [ByteList][], [ShortList][], [IntList][], [LongList][], [FloatList][],[DoubleList][], [ExList][] and [Seq][].

* Streams, both sequential and parallel, are supported for JDK7/Anrdoid and primitive types with more functions: [Stream][], [CharStream][], [ByteStream][], [ShortStream][], [IntStream][], [LongStream][], [FloatStream][], [DoubleStream][] , [Fn][] and more [Collectors][].

* Programming in Android: [SQLiteExecutor][], [SQLBuilder][], [Async][], [CompletableFuture][], [Futures][], [EventBus][], [Observer][], and [Fu][]

* SQL Builder/ORM: [SQLExecutor][], [SQLBuilder][], [DataSet][], [JdbcUtil][], [CSVUtil][]...

* ORMs for NoSQL: [MongoDBExecutor][], [CassandraExecutor][] with [CQLBuilder][], [CouchbaseExecutor][], [HBaseExecutor][], [DynamoDBExecutor][] and [Neo4jExecutor][]

* Data Binding: [Parser][], [JSONParser][], [XMLParser][], [KryoParser][]...

* Matrix: [Matrix][], [IntMatrix][], [LongMatrix][], [DoubleMatrix][]...

* Benchmark test.

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
[Multiset]: http://www.landawn.com/Multiset_view.html
[LongMultiset]: http://www.landawn.com/LongMultiset_view.html
[BiMap]: http://www.landawn.com/BiMap_view.html
[Multimap]: http://www.landawn.com/Multimap_view.html
[ImmutableList]: http://www.landawn.com/ImmutableList_view.html
[ImmutableSet]: http://www.landawn.com/ImmutableSet_view.html
[ImmutableMap]: http://www.landawn.com/ImmutableMap_view.html
[Sheet]: http://www.landawn.com/Sheet_view.html
[Pair]: http://www.landawn.com/Pair_view.html
[Triple]: http://www.landawn.com/Triple_view.html
[Tuple]: http://www.landawn.com/Tuple_view.html
[Splitter]: http://www.landawn.com/Splitter_view.html
[Joiner]: http://www.landawn.com/Joiner_view.html
[Builder]: http://www.landawn.com/Builder_view.html
[Difference]: http://www.landawn.com/Difference_view.html
[Profiler]: http://www.landawn.com/Profiler_view.html
[AsyncExecutor]: http://www.landawn.com/AsyncExecutor_view.html
[CompletableFuture]: http://www.landawn.com/CompletableFuture_view.html
[Futures]: http://www.landawn.com/Futures_view.html
[CodeGenerator]: http://www.landawn.com/CodeGenerator_view.html
[HttpClient]: http://www.landawn.com/HttpClient_view.html
[N]:http://www.landawn.com/N_view.html

[BooleanList]: http://www.landawn.com/BooleanList_view.html
[CharList]: http://www.landawn.com/CharList_view.html
[ByteList]: http://www.landawn.com/ByteList_view.html
[ShortList]: http://www.landawn.com/ShortList_view.html
[IntList]: http://www.landawn.com/IntList_view.html
[LongList]: http://www.landawn.com/LongList_view.html
[FloatList]: http://www.landawn.com/FloatList_view.html
[DoubleList]: http://www.landawn.com/DoubleList_view.html
[ExList]: http://www.landawn.com/ExList_view.html
[Seq]: http://www.landawn.com/Seq_view.html

[Stream]: http://www.landawn.com/Stream_view.html
[CharStream]: http://www.landawn.com/CharStream_view.html
[ByteStream]: http://www.landawn.com/ByteStream_view.html
[ShortStream]: http://www.landawn.com/ShortStream_view.html
[IntStream]: http://www.landawn.com/IntStream_view.html
[LongStream]: http://www.landawn.com/LongStream_view.html
[FloatStream]: http://www.landawn.com/FloatStream_view.html
[DoubleStream]: http://www.landawn.com/DoubleStream_view.html
[Fn]: http://www.landawn.com/Fn_view.html
[Collectors]: http://www.landawn.com/Collectors_view.html

[SQLiteExecutor]: http://www.landawn.com/SQLiteExecutor_view.html
[SQLBuilder]: http://www.landawn.com/SQLBuilder_view.html
[Async]: http://www.landawn.com/Async_Android_view.html
[CompletableFuture]: http://www.landawn.com/CompletableFuture_Android_view.html
[Futures]: http://www.landawn.com/Futures_Android_view.html
[EventBus]: http://www.landawn.com/EventBus_view.html
[Observer]: http://www.landawn.com/api-docs/com/landawn/abacus/android/util/Observer.html
[Fu]: http://www.landawn.com/Fu_view.html

[SQLExecutor]: http://www.landawn.com/SQLExecutor_view.html
[SQLBuilder]: http://www.landawn.com/SQLBuilder_view.html
[DataSet]: http://www.landawn.com/DataSet_view.html
[JdbcUtil]: http://www.landawn.com/JdbcUtil_view.html
[CSVUtil]: http://www.landawn.com/CSVUtil_view.html

[MongoDBExecutor]: http://www.landawn.com/MongoDBExecutor_view.html
[CassandraExecutor]: http://www.landawn.com/MongoDBExecutor_view.html
[CQLBuilder]: http://www.landawn.com/CQLBuilder_view.html
[CouchbaseExecutor]: http://www.landawn.com/CouchbaseExecutor_view.html
[HBaseExecutor]: http://www.landawn.com/HBaseExecutor_view.html
[DynamoDBExecutor]: http://www.landawn.com/DynamoDBExecutor_view.html
[Neo4jExecutor]: http://www.landawn.com/Neo4jExecutor_view.html

[Parser]: http://www.landawn.com/Parser_view.html
[JSONParser]: http://www.landawn.com/JSONParser_view.html
[XMLParser]: http://www.landawn.com/XMLParser_view.html
[KryoParser]: http://www.landawn.com/KryoParser_view.html

[Matrix]: http://www.landawn.com/Matrix_view.html
[IntMatrix]: http://www.landawn.com/IntMatrix_view.html
[LongMatrix]: http://www.landawn.com/LongMatrix_view.html
[DoubleMatrix]: http://www.landawn.com/DoubleMatrix_view.html

