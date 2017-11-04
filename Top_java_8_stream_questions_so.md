
### [Java 8 `List<V>` into `Map<K, V>`](https://stackoverflow.com/questions/20363719/java-8-listv-into-mapk-v)

* By Java 8
```java
// if choices are unique by name.
Map<String, Choice> result = choices.stream()
                                    .collect(Collectors.toMap(Choice::getName, Function.identity()));
// owtherwise group by
Map<String, List<Choice>> result = choices.stream()
                                          .collect(Collectors.groupingBy(Choice::getName));
```
* By Abacus-Util
```java
Map<String, Choice> result = Stream.of(choices).toMap(Choice::getName, Fn.identity());

// group by
Map<String, List<Choice>> result = Stream.of(choices).groupTo(Choice::getName);
```

### [Retrieving a List from a java.util.stream.Stream in Java 8](https://stackoverflow.com/questions/14830313/retrieving-a-list-from-a-java-util-stream-stream-in-java-8)

* By Java 8
```java
targetLongList = sourceLongList.stream()
                               .filter(l -> l > 100)
                               .collect(Collectors.toList());
```

* By Abacus-Util
```java
targetLongList = Stream.of(sourceLongList).filter(l -> l > 100).toList();
```

# [Custom thread pool in Java 8 parallel stream](https://stackoverflow.com/questions/21163108/custom-thread-pool-in-java-8-parallel-stream)
By Java 8
```java
???
```

By Abacus-Util
```java
stream.parallel(threadNum);
```

# [Is there a concise way to iterate over a stream with indices in Java 8?](https://stackoverflow.com/questions/18552005/is-there-a-concise-way-to-iterate-over-a-stream-with-indices-in-java-8)
By Java 8
```java
String[] names = {"Sam", "Pamela", "Dave", "Pascal", "Erik"};
IntStream.range(0, names.length)
         .filter(i -> names[i].length() <= i)
         .mapToObj(i -> names[i])
         .collect(Collectors.toList());
```

By Abacus-Util
```java
String[] names = {"Sam", "Pamela", "Dave", "Pascal", "Erik"};
IntStream.range(0, names.length)
         .filter(i -> names[i].length() <= i)
         .mapToObj(i -> names[i])
         .toList();

// Or: indexed for any type of collection/iterator.
Stream.of(collection).indexed()...;
```
