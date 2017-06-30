Motivated by [What Java 8 Stream.collect equivalents are available in the standard Kotlin library?][1]

* **Accumulate names in a List**

```java
// Java:  
List<String> list = people.stream().map(Person::getName).collect(Collectors.toList());

// Kotlin:
val list = people.map { it.name }  // toList() not needed

// Java by Abacus-Util
List<String> list = Seq.of(people).map(Person::getName);
// Or:
List<String> list = Stream.of(people).map(Person::getName).toList();
```



[1]: https://stackoverflow.com/questions/34642254/what-java-8-stream-collect-equivalents-are-available-in-the-standard-kotlin-libr
