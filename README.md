# [Abacus-Util](http://www.landawn.com)

A general programming library/framework in Java. It's simple, powerful and easy to use with concise APIs


## Features:

* Most daily used APIs: [IOUtil][], [Multiset][], [LongMultiset][], [BiMap][], [Multimap][], [ImmutableList][], [ImmutableSet][], [ImmutableMap][], [Sheet][], [Pair][], [Triple][], [Tuple][], [Splitter][], [Joiner][], [Builder][], [Difference][], [Profiler][], [AsyncExecutor][], [CompletableFuture][], [Futures][], [CodeGenerator][], [HttpClient][], [N][] ...

* Primitive List: [BooleanList][], [CharList][], [ByteList][], [ShortList][], [IntList][], [LongList][], [FloatList][],[DoubleList][] and [Seq][].

* Streams, both sequential and parallel, are supported for JDK7/Anrdoid and primitive types with more functions: [Stream][], [EntryStream][], [CharStream][], [ByteStream][], [ShortStream][], [IntStream][], [LongStream][], [FloatStream][], [DoubleStream][], [Fn][] and more [Collectors][].

* Programming in Android: [SQLiteExecutor][], [SQLBuilder][], [Async][], [CompletableFuture][CompletableFuture_Android], [Futures][], [EventBus][], [Observer][], [ObserverX][] and [Fu][]

* SQL Builder/ORM: [SQLExecutor][], [SQLBuilder][], [DataSet][], [JdbcUtil][], [CSVUtil][]...

* ORMs for NoSQL: [MongoDBExecutor][], [CassandraExecutor][] with [CQLBuilder][], [CouchbaseExecutor][], [HBaseExecutor][], [DynamoDBExecutor][] and [Neo4jExecutor][]

* JSON/XML Data Binding: [Parser][], [JSONParser][], [XMLParser][], [KryoParser][]...

* Matrix: [Matrix][], [IntMatrix][], [LongMatrix][], [DoubleMatrix][]...

* [More](http://www.landawn.com)...

## Download/Installation:

* [Maven](http://search.maven.org/#search%7Cga%7C1%7Cg%3A%22com.landawn%22)

* Gradle:
```gradle
// JDK 1.8 or above:
compile 'com.landawn:abacus-util:0.9.75'

// JDK 1.8 or above with kryo-3.0.3, snappy-java-1.1.2.6 and lz4-1.3.0:
compile 'com.landawn:abacus-util-all:0.9.75'

// JDK 1.7:
compile 'com.landawn:abacus-util-jdk7:0.9.75'

// JDK 1.7 with kryo-3.0.3, snappy-java-1.1.2.6 and lz4-1.3.0:
compile 'com.landawn:abacus-util-all-jdk7:0.9.75'

// Android (Java 1.7):
compile 'abacus-android-jdk7:0.9.75'

// Android-SE (Java 1.7) - small edition without Stream/Matrix/Sheet/...:
compile 'abacus-android-se-jdk7:0.9.75'

// Android (Java 1.8 or above):
compile 'com.landawn:abacus-android:0.9.75'

// Android-SE (Java 1.8 or above) - small edition without Stream/Matrix/Sheet/...:
compile 'com.landawn:abacus-android-se:0.9.75'
```

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

### [CodeGenerator](http://www.landawn.com/api-docs/com/landawn/abacus/util/CodeGenerator.html) for entity classes with getter/setter methods. Here are the at least benefits of generating code by tool:

1. Productivity: generate tens, even hundreds of lines of code by couple of lines of codes in one minute.

2. Maintainability: It's easy and simple to add/remove fields or change types of fields. And all the codes follow the same format.

3. Bug free. No test is required for the auto-generated codes and no test coverage is counted. 

```java
File srcDir = new File("./src");
String packageName = "com.x.y";

Map<String, Object> fields = N.asLinkedHashMap("firstName", String.class, "lastName", String.class, "birthdate", Date.class, "attrs", "Map<String, List<java.sql.Date>>");
CodeGenerator.generateEntity(srcDir, packageName, "Account", fields);
```
OR:

```java
// Prepare the class with fields first:
public class Account {
    private String firstName;
    private String lastName;
    private Date birthdate;
    private Map<String, List<Date>> attrs;
}

// Then generate the constructors/getter/setter methods by one line code:
CodeGenerator.writeClassMethod(srcDir, Account.class);
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

More see: [ParserFactory](http://www.landawn.com/api-docs/com/landawn/abacus/parser/ParserFactory.html)

### Functional Programming
```java

List<String> myList = N.asList("a1", "a2", "b1", "c2", "c1");
Stream.of(myList) // very similiar to Java 8 myList.stream()
      .filter(s -> s.startsWith("c"))
      .map(N::toUpperCase)
      .sorted()
      .forEach(N::println);
// C1
// C2

// Group by person by name and sorted by name
// By Java 8
Map<String, Person> = persons.stream()
                             .collect(Collectors.groupingBy(Person::getName))
                             .enrySet()
                             .stream()
                             .sorted((a, b) -> a.getKey().compareTo(b.getKey())) // compare by name;
                             .collect(Collectors.toMap(e -> e.getKey(), e.getValue()));

// AbacusUtil
Map<String, Person> = Stream.of(persons)
                             .groupByToEntry(Person::getName)
                             .sortedBy(e -> e.getKey()) // compare by name;
                             .toMap();

```

### The Best [SQLBuilder][]/[SQLExecutor][]/Mapper Ever
A simple CRUD(create/read/update/delete) sample by SQLExecutor

```java
Account account = createAccount();
// create
String sql_insert = insert("gui", "firstName", "lastName", "lastUpdateTime").into(Account.class).sql();
sqlExecutor.insert(sql_insert, account);
// read
String sql_selectByGUI = selectFrom(Account.class).where(L.eq("gui")).sql();
Account dbAccount = sqlExecutor.queryForEntity(Account.class, sql_selectByGUI, account);
// update
String sql_updateByLastName = update(Account.class).set("firstName").where(L.eq("lastName")).sql();
dbAccount.setFirstName("newFirstName");
sqlExecutor.update(sql_updateByLastName, dbAccount);
// delete
String sql_deleteByFirstName = deleteFrom(Account.class).where(L.eq("firstName)).sql();
sqlExecutor.update(sql_deleteByFirstName, dbAccount);
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

### [Kotlin vs Java 8 on Collection](./Java_Kotlin.md)

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
[Seq]: http://www.landawn.com/Seq_view.html

[Stream]: http://www.landawn.com/Stream_view.html
[EntryStream]: http://www.landawn.com/EntryStream_view.html
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
[CompletableFuture_Android]: http://www.landawn.com/CompletableFuture_Android_view.html
[Futures]: http://www.landawn.com/Futures_Android_view.html
[EventBus]: http://www.landawn.com/EventBus_view.html
[Observer]: http://www.landawn.com/api-docs/com/landawn/abacus/android/util/Observer.html
[ObserverX]: http://www.landawn.com/api-docs/com/landawn/abacus/android/util/ObserverX.html
[Fu]: http://www.landawn.com/Fu_view.html

[SQLExecutor]: http://www.landawn.com/SQLExecutor_view.html
[SQLBuilder]: http://www.landawn.com/SQLBuilder_view.html
[DataSet]: http://www.landawn.com/DataSet_view.html
[JdbcUtil]: http://www.landawn.com/JdbcUtil_view.html
[CSVUtil]: http://www.landawn.com/CSVUtil_view.html

[MongoDBExecutor]: http://www.landawn.com/MongoDBExecutor_view.html
[CassandraExecutor]: http://www.landawn.com/CassandraExecutor_view.html
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
