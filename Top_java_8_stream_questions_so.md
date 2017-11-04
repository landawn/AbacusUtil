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

---
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

---
### [Custom thread pool in Java 8 parallel stream](https://stackoverflow.com/questions/21163108/custom-thread-pool-in-java-8-parallel-stream)

* By Java 8
```java
???
```

* By Abacus-Util
```java
stream.parallel(threadNum);
```

---
### [Is there a concise way to iterate over a stream with indices in Java 8?](https://stackoverflow.com/questions/18552005/is-there-a-concise-way-to-iterate-over-a-stream-with-indices-in-java-8)

* By Java 8
```java
String[] names = {"Sam", "Pamela", "Dave", "Pascal", "Erik"};
IntStream.range(0, names.length)
         .filter(i -> names[i].length() <= i)
         .mapToObj(i -> names[i])
         .collect(Collectors.toList());
```

* By Abacus-Util
```java
String[] names = {"Sam", "Pamela", "Dave", "Pascal", "Erik"};
IntStream.range(0, names.length)
         .filter(i -> names[i].length() <= i)
         .mapToObj(i -> names[i])
         .toList();

// Or: indexed for any type of collection/iterator.
Stream.of(collection).indexed()...;
```

---
### [How can I throw CHECKED exceptions from inside Java 8 streams? (Not wrapping it into unchecked exceptions)](https://stackoverflow.com/questions/27644361/how-can-i-throw-checked-exceptions-from-inside-java-8-streams-not-wrapping-it)

* By Java 8
```java
Stream.of("java.lang.Object", "java.lang.Integer", "java.lang.String")
              .map(className -> try {
                            Class.forName(className))
                        } catch (ClassNotFoundException ) {
                            throw new RuntimeException(e);
                        }
              .collect(Collectors.toList());
```

* By Abacus-Util
```java
Stream.of("java.lang.Object", "java.lang.Integer", "java.lang.String")
              .map(className -> Try.call(Class::forName))
              .toList();
```

---
### [Using Java 8's Optional with Stream::flatMap](https://stackoverflow.com/questions/22725537/using-java-8s-optional-with-streamflatmap)

* By Java 8
```java
Optional<Other> result =
    things.stream()
          .map(this::resolve)
          .flatMap(o -> o.isPresent() ? Stream.of(o.get()) : Stream.empty())
          .findFirst();
```

* By Abacus-Util
```java
Optional<Other> result =
    things.stream()
          .map(this::resolve)
          .flatMap(Optional::stream)
          .first();
```

---
### [Limit a stream by a predicate](https://stackoverflow.com/questions/20746429/limit-a-stream-by-a-predicate)

* By Java 8
```java
// No easy way until Java 9
IntStream
    .iterate(1, n -> n + 1)
    .takeWhile(n -> n < 10)
    .forEach(System.out::println);
```

* By Abacus-Util
```java
IntStream
    .iterate(1, n -> n + 1)
    .takeWhile(n -> n < 10)
    .forEach(Fn.println);
```

---
### [Java 8 NullPointerException in Collectors.toMap](https://stackoverflow.com/questions/24630963/java-8-nullpointerexception-in-collectors-tomap)

* By Java 8
```java
List<Answer> answerList = new ArrayList<>();
answerList.add(new Answer(1, true));
answerList.add(new Answer(2, true));
answerList.add(new Answer(3, null));
Map<Integer, Boolean> answerMap = answerList.stream()
          .collect(Collectors.toMap(Answer::getId, Answer::getAnswer)); // throw NullPointerException
```

* By Abacus-Util
```java
// Works no problem
Map<Integer, Boolean> answerMap = answerList.stream().toMap(Answer::getId, Answer::getAnswer);
```


