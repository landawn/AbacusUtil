
1, Add mapFirst/mapFirstOrElse/mapLast/mapLastOrElse, Refer to [StreamEx][]:
   Parallel support is required.
   
```java
   public abstract Stream<T> mapFirst(Function<? super T, ? extends T> mapperForFirst);
   public abstract <R> Stream<R> mapFirstOrElse(Function<? super T, ? extends R> mapperForFirst ,Function<? super T, ? extends R> mapperForElse);
   public abstract Stream<T> mapLast(Function<? super T, ? extends T> mapperForLast);
   public abstract <R> Stream<R> mapLastOrElse(Function<? super T, ? extends R> mapperForLast ,Function<? super T, ? extends R> mapperForElse);
```

2, Add slidingMap, Refer to biMap/triMap. The purpose of these methods is reducing the creation of intermedia list. Parallel support is required.

```java
public abstract Stream<R> slidingMap(BiFunction<? super T, ? super T, R> mapper);
public abstract Stream<R> slidingMap(BiFunction<? super T, ? super T, R> mapper, boolean ignoreNotPaired);
public abstract Stream<R> slidingMap(TriFunction<? superT, ? super T, ? super T, R> mapper);
public abstract Stream<R> slidingMap(TriFunction<? superT, ? super T, ? super T, R> mapper, boolean ignoreNotPaired);
```
   
   [StreamEx]: https://github.com/amaembo/streamex
