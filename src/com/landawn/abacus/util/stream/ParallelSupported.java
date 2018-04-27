package com.landawn.abacus.util.stream;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Documented
@Retention(value = RetentionPolicy.CLASS)
@Target(value = { ElementType.METHOD, ElementType.TYPE })
public @interface ParallelSupported {

}
