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

---
### [Adding two Java 8 streams, or an extra element to a stream](https://stackoverflow.com/questions/22740464/adding-two-java-8-streams-or-an-extra-element-to-a-stream)

* By Java 8
```java
Stream stream = Stream.concat(
                       Stream.concat(
                              stream1.filter(x -> x!=0), stream2)
                              .filter(x -> x!=1),
                                  Stream.of(element))
                                  .filter(x -> x!=2);
```

* By Abacus-Util
```java
stream1.append(stream2).append(element);
```

---
### [Java8: HashMap<X, Y> to HashMap<X, Z> using Stream / Map-Reduce / Collector](https://stackoverflow.com/questions/25903137/java8-hashmapx-y-to-hashmapx-z-using-stream-map-reduce-collector)

* By Java 8
```java
Map<String, String> x;
Map<String, Integer> y =
    x.entrySet().stream()
        .collect(Collectors.toMap(
            e -> e.getKey(),
            e -> Integer.parseInt(e.getValue())
        ));
```

* By Abacus-Util
```java
Map<String, Integer> y = Stream.of(x).toMap(e -> e.getKey(), e -> Integer.parseInt(e.getValue()));
```

---
### [Does Java SE 8 have Pairs or Tuples?](https://stackoverflow.com/questions/24328679/does-java-se-8-have-pairs-or-tuples)

* By Java 8
```java
// No
```

* By Abacus-Util

Yes, with he most beautiful design: 
[Pair](https://static.javadoc.io/com.landawn/abacus-util-all/1.0.6/com/landawn/abacus/util/Pair.html), [Triple](https://static.javadoc.io/com.landawn/abacus-util-all/1.0.6/com/landawn/abacus/util/Triple.html), [Tuple](https://static.javadoc.io/com.landawn/abacus-util-all/1.0.6/com/landawn/abacus/util/Tuple.html)


---
### [Filter Java Stream to 1 and only 1 element](https://stackoverflow.com/questions/22694884/filter-java-stream-to-1-and-only-1-element)

* By Java 8
```java
List<User> resultUserList = users.stream()
        .filter(user -> user.getId() == 1)
        .limit(2)
        .collect(Collectors.toList());
if (resultUserList.size() != 1) {
    throw new IllegalStateException();
}
User resultUser = resultUserList.get(0);

// Or by singletonCollector()
public static <T> Collector<T, ?, T> singletonCollector() {
    return Collectors.collectingAndThen(
            Collectors.toList(),
            list -> {
                if (list.size() != 1) {
                    throw new IllegalStateException();
                }
                return list.get(0);
            }
    );
}
```

* By Abacus-Util
```java
users.stream().filter(user -> user.getId() == 1).limit(2)
      .toListAndThen(l -> Optional.ofNulable(l.size() == 1 ? l.get(0) : null))
      .orElseThrow(IllegalStateException::new)
```

