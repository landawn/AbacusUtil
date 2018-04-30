# [Abacus-Util](http://www.landawn.com)

[![Maven Central](https://img.shields.io/maven-central/v/com.landawn/abacus-util.svg)](https://maven-badges.herokuapp.com/maven-central/com.landawn/abacus-util/)
[![Javadocs](https://www.javadoc.io/badge/com.landawn/abacus-util-all.svg)](https://www.javadoc.io/doc/com.landawn/abacus-util-all)

A general programming library in Java/Android. It's easy to learn and simple to use with concise and powerful APIs.

## Features:

* Most daily used APIs: [N](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/N_view.html), 
[Seq](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Seq_view.html), 
[Iterators](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Iterators_view.html),
[Maps](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Maps_view.html), 
[DateUtil](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/DateUtil_view.html), 
[IOUtil](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/IOUtil_view.html), 
[Multiset](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Multiset_view.html), 
[LongMultiset](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/LongMultiset_view.html), 
[Multimap](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Multimap_view.html), 
[BiMap](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/BiMap_view.html), 
[ImmutableList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ImmutableList_view.html), 
[ImmutableSet](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ImmutableSet_view.html), 
[ImmutableMap](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ImmutableMap_view.html), 
[Pair](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Pair_view.html), 
[Triple](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Triple_view.html), 
[Tuple](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Tuple_view.html), 
[Splitter](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Splitter_view.html), 
[Joiner](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Joiner_view.html), [Builder](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Builder_view.html), 
[Difference](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Difference_view.html), 
[Profiler](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Profiler_view.html), 
[AsyncExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/AsyncExecutor_view.html), 
[CompletableFuture](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CompletableFuture_view.html), 
[Futures](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Futures_view.html), 
[CodeGenerator](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CodeGenerator_view.html), 
[HttpClient](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/HttpClient_view.html), 
[Sheet](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Sheet_view.html)...

* Primitive List: [BooleanList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/BooleanList_view.html), 
[CharList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CharList_view.html), 
[ByteList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ByteList_view.html), 
[ShortList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ShortList_view.html), 
[IntList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/IntList_view.html), 
[LongList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/LongList_view.html), 
[FloatList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/FloatList_view.html),
[DoubleList](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/DoubleList_view.html).

* Streams, both sequential and parallel, are supported for JDK7/Anrdoid and primitive types with more functions: 
[BaseStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/BaseStream_view.html), 
[Stream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Stream_view.html), 
[EntryStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/EntryStream_view.html), 
[CharStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CharStream_view.html), 
[ByteStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ByteStream_view.html), 
[ShortStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ShortStream_view.html), 
[IntStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/IntStream_view.html), 
[LongStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/LongStream_view.html), 
[FloatStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/FloatStream_view.html), 
[DoubleStream](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/DoubleStream_view.html), 
[Fn](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Fn_view.html) and more 
[Collectors](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Collectors_view.html).

* Programming in Android: 
[SQLiteExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/SQLiteExecutor_view.html), 
[SQLBuilder](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/SQLBuilder_view.html), 
[Async](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Async_Android_view.html), 
[CompletableFuture](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CompletableFuture_Android_view.html), 
[Futures](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Futures_Android_view.html), 
[EventBus](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/EventBus_view.html), 
[Observer](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Observer_view.html), 
[ObserverX](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/ObserverX_view.html) and 
[Fu](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Fu_view.html).

* SQL Builder/ORM: 
[SQLExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/SQLExecutor_view.html), 
[Mapper](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Mapper_view.html), 
[SQLBuilder](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/SQLBuilder_view.html), 
[DataSet](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/DataSet_view.html), 
[JdbcUtil](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/JdbcUtil_view.html), 
[CSVUtil](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CSVUtil_view.html)...

* ORMs for NoSQL: 
[MongoDBExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/MongoDBExecutor_view.html), 
[CassandraExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CassandraExecutor_view.html) with [CQLBuilder](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CQLBuilder_view.html), 
[CouchbaseExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/CouchbaseExecutor_view.html), 
[HBaseExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/HBaseExecutor_view.html), 
[DynamoDBExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/DynamoDBExecutor_view.html) and 
[Neo4jExecutor](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Neo4jExecutor_view.html).

* JSON/XML Data Binding: 
[Parser](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Parser_view.html), 
[JSONParser](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/JSONParser_view.html), 
[XMLParser](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/XMLParser_view.html), 
[KryoParser](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/KryoParser_view.html)...

* Matrix: 
[Matrix](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/Matrix_view.html), 
[IntMatrix](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/IntMatrix_view.html), 
[LongMatrix](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/LongMatrix_view.html), 
[DoubleMatrix](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/DoubleMatrix_view.html) ... and 
[AbstractMatrix](https://cdn.rawgit.com/landawn/AbacusUtil/master/docs/KryoParser_view.html).

* More: [RemoteExecutor](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/RemoteExecutor.html),
[If](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/If.html),
[Try](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Try.html),
[Retry](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Retry.html),
[Synchronized](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Synchronized.html),
[ObjectPool](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/pool/ObjectPool.html),
[KeyedObjectPool](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/pool/KeyedObjectPool.html),
[SpyMemcached](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/cache/SpyMemcached.html),
[JRedis](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/cache/JRedis.html),
[MemcachedLock](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/MemcachedLock.html),
[Properties](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Properties.html),
[PropertiesUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/PropertiesUtil.html),
[Charsets](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Charsets.html),
[Ascii](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Ascii.html),
[CalendarUnit](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CalendarUnit.html),
[NamingPolicy](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/NamingPolicy.html),
[Array](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Array.html),
[Wrapper](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Wrapper.html),
[ArrayHashSet](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ArrayHashSet.html),
[ArrayHashMap](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ArrayHashMap.html),
[Holder](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Holder.html),
[LineIterator](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/LineIterator.html),
[RowIterator](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/RowIterator.html),
[BooleanIterator](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/BooleanIterator.html)
...
[DoubleIterator](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/DoubleIterator.html),
[ObjIterator](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ObjIterator.html),
[Nullable](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Nullable.html),
[Optional](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Optional.html),
[OptionalBoolean](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/OptionalBoolean.html)
...
[OptionalDouble](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/OptionalDouble.html),
[Base64](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Base64.html),
[Clazz](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Clazz.html),
[ClassUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ClassUtil.html),
[EscapeUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/EscapeUtil.html),
[DigestUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/DigestUtil.html),
[Hex](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Hex.html),
[FilenameUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/FilenameUtil.html),
[JSONUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/JSONUtil.html),
[AWSJSONUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/AWSJSONUtil.html),
[AddrUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/AddrUtil.html),
[URLEncodedUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/URLEncodedUtil.html),
[WSSecurityUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/WSSecurityUtil.html),
[EmailUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/EmailUtil.html),
[IEEE754rUtil](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/IEEE754rUtil.html),
[Duration](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Duration.html),
[Range](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Range.html),
[Fraction](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Fraction.html),
[MutableBoolean](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/MutableBoolean.html)
...
[MutableDouble](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/MutableDouble.html),
[Index](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Index.html),
[Indexed](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Indexed.html),
[f](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/f.html),
[Hashing](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/hash/Hashing.html),
[Maths](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Maths.html),
[Comparators](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Comparators.html),
[Chain](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Chain.html),
[SafeInitializer](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/SafeInitializer.html),
[Stopwatch](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Stopwatch.html),
[RateLimiter](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/RateLimiter.html),
[Traverser](https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Traverser.html)(from JDK8, Apache commons, Google Guava...) ...


## Download/Installation & [Changes](https://github.com/landawn/AbacusUtil/blob/master/CHANGES.md):

* [Maven](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.landawn%22)

* Gradle:
```gradle
// JDK 1.8 or above:
compile 'com.landawn:abacus-util:1.2.7'

// JDK 1.8 or above with kryo-3.0.3, snappy-java-1.1.5.6 and lz4-1.3.0:
compile 'com.landawn:abacus-util-all:1.2.7'

// JDK 1.7:
compile 'com.landawn:abacus-util-jdk7:1.2.7'

// JDK 1.7 with kryo-3.0.3, snappy-java-1.1.5.6 and lz4-1.3.0:
compile 'com.landawn:abacus-util-all-jdk7:1.2.7'

// Android (Java 1.7):
compile 'abacus-android-jdk7:1.2.7'

// Android-SE (Java 1.7) - small edition without Stream/Matrix/Sheet/...:
compile 'abacus-android-se-jdk7:1.2.7'

// Android (Java 1.8 or above):
compile 'com.landawn:abacus-android:1.2.7'

// Android-SE (Java 1.8 or above) - small edition without Stream/Matrix/Sheet/...:
compile 'com.landawn:abacus-android-se:1.2.7'
```
### Functional Programming:
(It's very important to learn Lambdas and Stream APIs in Java 8 to get the best user experiences with the APIs provided in AbacusUtil)

[What's New in Java 8](https://leanpub.com/whatsnewinjava8/read)

[An introduction to the java.util.stream library](https://www.ibm.com/developerworks/library/j-java-streams-1-brian-goetz/index.html)

[When to use parallel streams](http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html)

[Top Java 8 stream questions on stackoverflow](./Top_java_8_stream_questions_so.md)

[Kotlin vs Java 8 on Collection](./Java_Kotlin.md)


## User Guide
Please refer to [wiki](https://github.com/landawn/AbacusUtil/wiki).


[IOUtil]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/IOUtil.html
[Multiset]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Multiset.html
[LongMultiset]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/LongMultiset.html
[BiMap]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/BiMap.html
[Multimap]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Multimap.html
[ImmutableList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ImmutableList.html
[ImmutableSet]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ImmutableSet.html
[ImmutableMap]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ImmutableMap.html
[Sheet]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Sheet.html
[Pair]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Pair.html
[Triple]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Triple.html
[Tuple]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Tuple.html
[Splitter]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Splitter.html
[Joiner]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Joiner.html
[Builder]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Builder.html
[Difference]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Difference.html
[Profiler]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Profiler.html
[AsyncExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/AsyncExecutor.html
[CompletableFuture]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CompletableFuture.html
[Futures]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Futures.html
[CodeGenerator]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CodeGenerator.html
[HttpClient]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/http/HttpClient.html
[N]:https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/N.html

[BooleanList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/BooleanList.html
[CharList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CharList.html
[ByteList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ByteList.html
[ShortList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/ShortList.html
[IntList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/IntList.html
[LongList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/LongList.html
[FloatList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/FloatList.html
[DoubleList]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/DoubleList.html
[Seq]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Seq.html

[Stream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/Stream.html
[EntryStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/EntryStream.html
[CharStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/CharStream.html
[ByteStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/ByteStream.html
[ShortStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/ShortStream.html
[IntStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/IntStream.html
[LongStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/LongStream.html
[FloatStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/FloatStream.html
[DoubleStream]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/DoubleStream.html
[Fn]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Fn.html
[Collectors]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/stream/Collectors.html

[SQLiteExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/SQLiteExecutor.html
[SQLBuilder]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/SQLBuilder.html
[Async]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/Async.html
[CompletableFuture_Android]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/CompletableFuture.html
[Futures_Android]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/Futures.html
[EventBus]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/eventBus/EventBus.html
[Observer]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/Observer.html
[ObserverX]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/ObserverX.html
[Fu]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/android/util/Fu.html

[SQLExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/SQLExecutor.html
[Mapper]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/SQLExecutor.Mapper.html
[SQLBuilder]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/SQLBuilder.html
[DataSet]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/DataSet.html
[JdbcUtil]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/JdbcUtil.html
[CSVUtil]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CSVUtil.html

[MongoDBExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/MongoDBExecutor.html
[CassandraExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CassandraExecutor.html
[CQLBuilder]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CQLBuilder.html
[CouchbaseExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/CouchbaseExecutor.html
[HBaseExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/HBaseExecutor.html
[DynamoDBExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/DynamoDBExecutor.html
[Neo4jExecutor]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Neo4jExecutor.html

[Parser]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/parser/Parser.html
[JSONParser]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/parser/JSONParser.html
[XMLParser]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/parser/XMLParser.html
[KryoParser]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/parser/KryoParser.html

[Matrix]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/Matrix.html
[IntMatrix]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/IntMatrix.html
[LongMatrix]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/LongMatrix.html
[DoubleMatrix]: https://static.javadoc.io/com.landawn/abacus-util-all/1.2.7/com/landawn/abacus/util/DoubleMatrix.html
