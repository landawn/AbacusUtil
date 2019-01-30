Approaches o Keep backward compatibility:

1, Add flag parameter, for example: `Stream.scan(T seed, ...)`, `Stream.scan(T seed..., boolean includingSee)`.

2, Synonyms: `split/chunk`, `add/save`.

3, Version tagged package: `com.landawn.abacus.v2` or `com.landawn.abacus.http.v2/com.landawn.abacus.util.v2` (better?)

4, Add version conversion method `com.landawn.abacus.v2.DataSet DataSet.v2()`.

5, ...?
