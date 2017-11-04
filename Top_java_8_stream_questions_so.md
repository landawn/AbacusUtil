
### [Java 8 `List<V>` into `Map<K, V>`](https://stackoverflow.com/questions/20363719/java-8-listv-into-mapk-v)

* By Java 8
```java
// if Choice is unique by name.
Map<String, Choice> result = choices.stream()
                                    .collect(Collectors.toMap(Choice::getName, Function.identity()));
// owtherwise group by
Map<String, List<Choice>> result = choices.stream()
                                          .collect(Collectors.groupingBy(Choice::getName));
```
* By Abacus
```java
Map<String, Choice> result = Stream.of(choices).toMap(Choice::getName, Fn.identity());

// group by
Map<String, List<Choice>> result = Stream.of(choices).groupTo(Choice::getName);
```
