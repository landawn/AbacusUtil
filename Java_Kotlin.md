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

***

* **Collect example #5 - find people of legal age, output formatted string**

```java
// Java:
String phrase = persons
        .stream()
        .filter(p -> p.age >= 18)
        .map(p -> p.name)
        .collect(Collectors.joining(" and ", "In Germany ", " are of legal age."));

// Kotlin:
val phrase = persons
        .filter { it.age >= 18 }
        .map { it.name }
        .joinToString(" and ", "In Germany ", " are of legal age.")

// Java by Abacus-Util
String phrase = persons
        .stream()
        .filter(p -> p.age >= 18)
        .map(p -> p.name)
        .join(" and ", "In Germany ", " are of legal age.");
```

***

* **Counting items in a list after filter is applied**

```java
// Java:
long count = items.stream().filter( item -> item.startsWith("t")).count();
item -> item.startsWith("t")
// Kotlin:
val count = items.filter { it.startsWith('t') }.size
// but better to not filter, but count with a predicate
val count = items.count { it.startsWith('t') }

// Java by Abacus-Util
int count = Seq.of(items).count(item -> item.startsWith("t"));
// Or:
int count = Seq.of(items).count(Fn.startsWith("t"));
```

[1]: https://stackoverflow.com/questions/34642254/what-java-8-stream-collect-equivalents-are-available-in-the-standard-kotlin-libr
