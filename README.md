# [Abacus-Util](http://www.landawn.com)

[![Maven Central](https://img.shields.io/maven-central/v/com.landawn/abacus-util.svg)](https://maven-badges.herokuapp.com/maven-central/com.landawn/abacus-util/)
[![Javadocs](https://www.javadoc.io/badge/com.landawn/abacus-util.svg)](https://www.javadoc.io/doc/com.landawn/abacus-util)

A general programming library in Java/Android. It's easy to learn and simple to use with concise and powerful APIs.

## Features:

* Most daily used APIs: [IOUtil][], [Multiset][], [LongMultiset][], [BiMap][], [Multimap][], [ImmutableList][], [ImmutableSet][], [ImmutableMap][], [Sheet][], [Pair][], [Triple][], [Tuple][], [Splitter][], [Joiner][], [Builder][], [Difference][], [Profiler][], [AsyncExecutor][], [CompletableFuture][], [Futures][], [CodeGenerator][], [HttpClient][], [N][] ...

* Primitive List: [BooleanList][], [CharList][], [ByteList][], [ShortList][], [IntList][], [LongList][], [FloatList][],[DoubleList][] and [Seq][].

* Streams, both sequential and parallel, are supported for JDK7/Anrdoid and primitive types with more functions: [Stream][], [EntryStream][], [CharStream][], [ByteStream][], [ShortStream][], [IntStream][], [LongStream][], [FloatStream][], [DoubleStream][], [Fn][] and more [Collectors][].

* Programming in Android: [SQLiteExecutor][], [SQLBuilder][], [Async][], [CompletableFuture][CompletableFuture_Android], [Futures][Futures_Android], [EventBus][], [Observer][], [ObserverX][] and [Fu][]

* SQL Builder/ORM: [SQLExecutor][], [Mapper](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/SQLExecutor.Mapper.html), [SQLBuilder][], [DataSet][], [JdbcUtil][], [CSVUtil][]...

* ORMs for NoSQL: [MongoDBExecutor][], [CassandraExecutor][] with [CQLBuilder][], [CouchbaseExecutor][], [HBaseExecutor][], [DynamoDBExecutor][] and [Neo4jExecutor][]

* JSON/XML Data Binding: [Parser][], [JSONParser][], [XMLParser][], [KryoParser][]...

* Matrix: [Matrix][], [IntMatrix][], [LongMatrix][], [DoubleMatrix][]...

* More: [RemoteExecutor](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/RemoteExecutor.html),
[If](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/If.html),
[Try](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Try.html),
[Retry](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Retry.html),
[Synchronized](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Synchronized.html),
[ObjectPool](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/pool/ObjectPool.html),
[KeyedObjectPool](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/pool/KeyedObjectPool.html),
[SpyMemcached](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/cache/SpyMemcached.html),
[JRedis](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/cache/JRedis.html),
[MemcachedLock](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/MemcachedLock.html),
[Properties](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Properties.html),
[PropertiesUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/PropertiesUtil.html),
[Charsets](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Charsets.html),
[Ascii](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Ascii.html),
[CalendarUnit](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CalendarUnit.html),
[NamingPolicy](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/NamingPolicy.html),
[Array](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Array.html),
[Wrapper](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Wrapper.html),
[ArrayHashSet](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ArrayHashSet.html),
[ArrayHashMap](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ArrayHashMap.html),
[Holder](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Holder.html),
[LineIterator](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/LineIterator.html),
[RowIterator](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/RowIterator.html),
[BooleanIterator](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/BooleanIterator.html)
...
[DoubleIterator](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/DoubleIterator.html),
[ObjIterator](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ObjIterator.html),
[Nullable](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Nullable.html),
[Optional](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Optional.html),
[OptionalBoolean](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/OptionalBoolean.html)
...
[OptionalDouble](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/OptionalDouble.html),
[Base64](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Base64.html),
[Clazz](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Clazz.html),
[ClassUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ClassUtil.html),
[EscapeUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/EscapeUtil.html),
[DigestUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/DigestUtil.html),
[Hex](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Hex.html),
[FilenameUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/FilenameUtil.html),
[JSONUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/JSONUtil.html),
[AWSJSONUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/AWSJSONUtil.html),
[AddrUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/AddrUtil.html),
[URLEncodedUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/URLEncodedUtil.html),
[WSSecurityUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/WSSecurityUtil.html),
[EmailUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/EmailUtil.html),
[IEEE754rUtil](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/IEEE754rUtil.html),
[Duration](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Duration.html),
[Range](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Range.html),
[Fraction](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Fraction.html),
[MutableBoolean](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/MutableBoolean.html)
...
[MutableDouble(https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/MutableDouble.html,
[Index(https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Index.html,
[Indexed(https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Indexed.html,
[f](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/f.html),
[Hashing](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/hash/Hashing.html),
[Maths](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Maths.html),
[Comparators](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Comparators.html),
[Chain](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Chain.html),
[Iterators](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Iterators.html),
[Maps](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Maps.html),
[SafeInitializer](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/SafeInitializer.html),
[Stopwatch](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Stopwatch.html),
[RateLimiter](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/RateLimiter.html),
[Traverser](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Traverser.html)(from JDK8, Apache commons, Google Guava...) ...


## Download/Installation & [Changes](https://github.com/landawn/AbacusUtil/blob/master/CHANGES.md):

* [Maven](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.landawn%22)

* Gradle:
```gradle
// JDK 1.8 or above:
compile 'com.landawn:abacus-util:1.2.4'

// JDK 1.8 or above with kryo-3.0.3, snappy-java-1.1.5.6 and lz4-1.3.0:
compile 'com.landawn:abacus-util-all:1.2.4'

// JDK 1.7:
compile 'com.landawn:abacus-util-jdk7:1.2.4'

// JDK 1.7 with kryo-3.0.3, snappy-java-1.1.5.6 and lz4-1.3.0:
compile 'com.landawn:abacus-util-all-jdk7:1.2.4'

// Android (Java 1.7):
compile 'abacus-android-jdk7:1.2.4'

// Android-SE (Java 1.7) - small edition without Stream/Matrix/Sheet/...:
compile 'abacus-android-se-jdk7:1.2.4'

// Android (Java 1.8 or above):
compile 'com.landawn:abacus-android:1.2.4'

// Android-SE (Java 1.8 or above) - small edition without Stream/Matrix/Sheet/...:
compile 'com.landawn:abacus-android-se:1.2.4'
```
### Functional Programming:
(It's very important to learn Lambdas and Stream APIs in Java 8 to get the best user experiences with the APIs provided in AbacusUtil)

[What's New in Java 8](https://leanpub.com/whatsnewinjava8/read)

[An introduction to the java.util.stream library](https://www.ibm.com/developerworks/library/j-java-streams-1-brian-goetz/index.html)

[When to use parallel streams](http://gee.cs.oswego.edu/dl/html/StreamParallelGuidance.html)

[Top Java 8 stream questions on stackoverflow](./Top_java_8_stream_questions_so.md)

[Kotlin vs Java 8 on Collection](./Java_Kotlin.md)


## Usage:

### Benchmark test by [Profiler][]:

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
totalElapsedTime: 60.736

<method name>,  |avg time|, |min time|, |max time|, |0.01% >=|, |0.1% >=|,  |1% >=|,    |10% >=|,   |20% >=|,   |50% >=|,   |80% >=|,   |90% >=|,   |99% >=|,   |99.9% >=|, |99.99% >=|
addByStream,    0.002,      0.001,      0.499,      0.104,      0.015,      0.007,      0.003,      0.003,      0.001,      0.001,      0.001,      0.001,      0.001,      0.001,      
========================================================================================================================
```
### The most simple [HttpClient][]:

```java
HttpClient.of("https://api.github.com/octocat").get()
```

### [CodeGenerator](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CodeGenerator.html) for entity classes with getter/setter methods. Here are the at least benefits of generating code by tool:

1. Productivity: generate tens, even hundreds of lines of code by couple of lines of codes in one minute.

2. Maintainability: It's easy and simple to add/remove fields or change types of fields. And all the codes follow the same format.

3. Bug free. No test is required for the auto-generated codes and no test coverage is counted. 

```java
// Prepare the class with fields first:
public class Account {
    private String firstName;
    private String lastName;
    private Date birthdate;
    private Map<String, List<Date>> attrs;
}

// Then just two lines to generate the mostly beautiful and well-formatted entity class:
final File srcDir = new File("./src");
CodeGenerator.writeClassMethod(srcDir, Account.class);
```
OR:

```java
String packageName = "com.x.y";

Map<String, Object> fields = N.asLinkedHashMap("firstName", String.class, "lastName", String.class, "birthdate", Date.class, "attrs", "Map<String, List<java.sql.Date>>");
CodeGenerator.generateEntity(srcDir, packageName, "Account", fields);
```

### A quick/fast way to/from JSON/XML.
```java
String json = N.toJSON(account); // {"firstName":"Jack", "lastName":"Do", "birthDate":1495815803177}

Account account2 = N.fromJSON(Account.class, json);
assertEquals(account, account2);

String xml = N.toXML(account); // <account><firstName>Jack</firstName><lastName>Do</lastName><birthDate>1495815803177</birthDate></account>
Account account3 = N.fromXML(Account.class, xml);
assertEquals(account, account3);
```

More see: [Parser](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/parser/Parser.html), [ParserFactory](https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/parser/ParserFactory.html)

### The Best [SQLBuilder][]/[SQLExecutor][]/[Mapper] Ever
A simple CRUD(create/read/update/delete) sample by SQLExecutor

```java
final DataSource ds = JdbcUtil.createDataSource(...); // Refer to: .\schema\DataSource.xsd
final SQLExecutor sqlExecutor = new SQLExecutor(ds);
Account account = createAccount();
// create
String sql_insert = NE.insert("gui", "firstName", "lastName", "lastUpdateTime").into(Account.class).sql();
sqlExecutor.insert(sql_insert, account);
// read
String sql_selectByGUI = NE.selectFrom(Account.class).where(L.eq("gui")).sql();
Account dbAccount = sqlExecutor.queryForEntity(Account.class, sql_selectByGUI, account);
// update
String sql_updateByLastName = NE.update(Account.class).set("firstName").where(L.eq("lastName")).sql();
dbAccount.setFirstName("newFirstName");
sqlExecutor.update(sql_updateByLastName, dbAccount);
// delete
String sql_deleteByFirstName = NE.deleteFrom(Account.class).where(L.eq("firstName)).sql();
sqlExecutor.update(sql_deleteByFirstName, dbAccount);
```

### NoSQL: [MongoDB][MongoDBExecutor]/[Cassandra][CassandraExecutor]/[Couchbase][CouchbaseExecutor]...
A simple CRUD(create/read/update/delete) sample for MongoDB:
```java
// create
collExecutor.insert(account);
// read
Account dbAccount = collExecutor.get(Account.class, account.getId());
// update
dbAccount.setFirstName("newFirstName");
collExecutor.update(dbAccount.getId(), N.asMap(FIRST_NAME, dbAccount.getFirstName()));
// delete
collExecutor.delete(dbAccount.getId());
// check
assertFalse(collExecutor.exists(dbAccount.getId()));
```

### Programming in Android with [retrolambda](https://github.com/orfjackal/retrolambda)

```java
Observer.of(inputEditText).debounce(3000).afterTextChanged(s -> {
    TPExecutor.execute(() -> searchService.search(s, ...))
        .thenRunOnUI((resp, error) -> {
            if (error != null) {
                // handle the error case.
            }
            
            // do other stuffs to display the search result.            
        });
});

// Get 'firstName' and 'lastName' of user with 'id' = 1.             
sqliteExecutor.queryForEntity(User.class, N.asList("firstName", "lastName"), eq("id", 1));
```

[IOUtil]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/IOUtil.html
[Multiset]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Multiset.html
[LongMultiset]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/LongMultiset.html
[BiMap]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/BiMap.html
[Multimap]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Multimap.html
[ImmutableList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ImmutableList.html
[ImmutableSet]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ImmutableSet.html
[ImmutableMap]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ImmutableMap.html
[Sheet]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Sheet.html
[Pair]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Pair.html
[Triple]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Triple.html
[Tuple]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Tuple.html
[Splitter]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Splitter.html
[Joiner]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Joiner.html
[Builder]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Builder.html
[Difference]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Difference.html
[Profiler]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Profiler.html
[AsyncExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/AsyncExecutor.html
[CompletableFuture]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CompletableFuture.html
[Futures]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Futures.html
[CodeGenerator]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CodeGenerator.html
[HttpClient]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/http/HttpClient.html
[N]:https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/N.html

[BooleanList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/BooleanList.html
[CharList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CharList.html
[ByteList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ByteList.html
[ShortList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/ShortList.html
[IntList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/IntList.html
[LongList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/LongList.html
[FloatList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/FloatList.html
[DoubleList]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/DoubleList.html
[Seq]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Seq.html

[Stream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/Stream.html
[EntryStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/EntryStream.html
[CharStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/CharStream.html
[ByteStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/ByteStream.html
[ShortStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/ShortStream.html
[IntStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/IntStream.html
[LongStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/LongStream.html
[FloatStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/FloatStream.html
[DoubleStream]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/DoubleStream.html
[Fn]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Fn.html
[Collectors]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/stream/Collectors.html

[SQLiteExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/SQLiteExecutor.html
[SQLBuilder]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/SQLBuilder.html
[Async]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/Async.html
[CompletableFuture_Android]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/CompletableFuture.html
[Futures_Android]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/Futures.html
[EventBus]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/eventBus/EventBus.html
[Observer]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/Observer.html
[ObserverX]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/ObserverX.html
[Fu]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/android/util/Fu.html

[SQLExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/SQLExecutor.html
[Mapper]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/SQLExecutor.Mapper.html
[SQLBuilder]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/SQLBuilder.html
[DataSet]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/DataSet.html
[JdbcUtil]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/JdbcUtil.html
[CSVUtil]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CSVUtil.html

[MongoDBExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/MongoDBExecutor.html
[CassandraExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CassandraExecutor.html
[CQLBuilder]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CQLBuilder.html
[CouchbaseExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/CouchbaseExecutor.html
[HBaseExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/HBaseExecutor.html
[DynamoDBExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/DynamoDBExecutor.html
[Neo4jExecutor]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Neo4jExecutor.html

[Parser]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/parser/Parser.html
[JSONParser]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/parser/JSONParser.html
[XMLParser]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/parser/XMLParser.html
[KryoParser]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/parser/KryoParser.html

[Matrix]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/Matrix.html
[IntMatrix]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/IntMatrix.html
[LongMatrix]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/LongMatrix.html
[DoubleMatrix]: https://static.javadoc.io/com.landawn/abacus-util/1.2.4/com/landawn/abacus/util/DoubleMatrix.html
