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

Yes, with the most beautiful design: 
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

---
### [What Java 8 Stream.collect equivalents are available in the standard Kotlin library?](https://stackoverflow.com/questions/34642254/what-java-8-stream-collect-equivalents-are-available-in-the-standard-kotlin-libr)

* By Java 8
```java
// Answers are included in the above link.
```

* By Abacus-Util

[Kotlin vs Java 8 on Collection](./Java_Kotlin.md)


---
### [Zipping streams using JDK8 with lambda (java.util.stream.Streams.zip)](https://stackoverflow.com/questions/17640754/zipping-streams-using-jdk8-with-lambda-java-util-stream-streams-zip)

* By Java 8
```java
// No?
```

* By Abacus-Util
```java
String[] a = {"a", "b", "c"};
String[] b = {"1", "2", "3"};
Stream.zip(a, b, Pair::of)...;
```

---
### [Ignore duplicates when producing map using streams](https://stackoverflow.com/questions/32312876/ignore-duplicates-when-producing-map-using-streams)

* By Java 8
```java
Map<String, String> phoneBook = 
    people.stream()
          .collect(Collectors.toMap(
             Person::getName,
             Person::getAddress,
             (address1, address2) -> {
                 System.out.println("duplicate key found!");
                 return address1;
             }
          ));
```

* By Abacus-Util
```java
Map<String, String> phoneBook = people.stream().toMap(Person::getName, Person::getAddress, Fn.ignoringMerger()); // Fn.replacingMerger()
```

---
### [Is it possible to cast a Stream in Java 8?](https://stackoverflow.com/questions/22511750/is-it-possible-to-cast-a-stream-in-java-8)

* By Java 8
```java
Stream.of(objects).filter(c -> c instanceof Client)
    .map(c -> ((Client) c).getID()).forEach(System.out::println);
```

* By Abacus-Util
```java
Stream.of(objects).select(Client.class).forEach(Fn.println);
```

---
### [Java 8: How do I work with exception throwing methods in streams?](https://stackoverflow.com/questions/23548589/java-8-how-do-i-work-with-exception-throwing-methods-in-streams)

* By Java 8
```java
Stream<A> as = ...
  as.forEach(a -> safeFoo(a));
  
private void safeFoo(final A a) {
    try {
        a.foo();
    } catch (Exception ex) {
        throw new RuntimeException(ex);
    }
}
```

* By Abacus-Util
```java
Stream<A> as = ...
  as.forEach(a -> Try.run(() -> a.foo()));
```

---
### [Java 8 stream reverse order](https://stackoverflow.com/questions/24010109/java-8-stream-reverse-order/24011264#24011264)

* By Java 8
```java
No?
```

* By Abacus-Util
```java
stream.reversed()... // Note: All elements will be loaded into memory.
```


