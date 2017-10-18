/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.landawn.abacus.util;

import static com.landawn.abacus.util.D._BACKSLASH;
import static com.landawn.abacus.util.D._QUOTATION_D;
import static com.landawn.abacus.util.D._QUOTATION_S;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Serializable;
import java.io.Writer;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.security.SecureRandom;
import java.sql.Date;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.Normalizer;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Random;
import java.util.RandomAccess;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TimeZone;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.EntityId;
import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.annotation.Internal;
import com.landawn.abacus.core.EntityManagerUtil;
import com.landawn.abacus.core.MapEntity;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.exception.UncheckedSQLException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.parser.JSONDeserializationConfig;
import com.landawn.abacus.parser.JSONParser;
import com.landawn.abacus.parser.JSONSerializationConfig;
import com.landawn.abacus.parser.JSONSerializationConfig.JSC;
import com.landawn.abacus.parser.KryoParser;
import com.landawn.abacus.parser.ParserFactory;
import com.landawn.abacus.parser.XMLDeserializationConfig;
import com.landawn.abacus.parser.XMLParser;
import com.landawn.abacus.parser.XMLSerializationConfig;
import com.landawn.abacus.parser.XMLSerializationConfig.XSC;
import com.landawn.abacus.type.EntityType;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.type.TypeFactory;
import com.landawn.abacus.util.Pair.IntPair;
import com.landawn.abacus.util.Retry.Retry0;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BooleanPredicate;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBiFunction;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriConsumer;
import com.landawn.abacus.util.stream.DoubleStream;
import com.landawn.abacus.util.stream.FloatStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * <p>
 * Note: This class includes codes copied from Apache Commons Lang, Google Guava and other open source projects under the Apache License 2.0.
 * The methods copied from other libraries/frameworks/projects may be modified in this class.
 * </p>
 * Class <code>N</code> is a general java utility class. It provides the most daily used operations for Object/primitive types/String/Array/Collection/Map/Entity/Date...:
 *
 * <ul>
 * <br>
 * <li> =======================================================================
 * <li>String operations:<br>
 * <b>stringOf/valueOf/join/isNullOrEmpty/notNullOrEmpty/
 * checkNullOrEmpty/indexOfXXX/lastIndexOfXXX
 * /ordinalIndexOf/startsWithXXX/endsWithXXX
 * /containsXXX/commonPrefix/commonSuffix/difference/equals/equalsIgnoreCase/trim
 * /strip/capitalize/uncapitalize/lowerCase/upperCase/swapCase/padStart/padEnd
 * /repeat/reverse/replace/replaceAll/replacePattern/unicodeEscaped/formatQuotation
 * /normalizeSpace/deleteWhitespace/chomp/chop/abbreviate/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Array operations:<br>
 * <b>newArray/asArray/asXXX/array2XXX/isNullOrEmpty
 * /notNullOrEmpty/checkNullOrEmpty/sort/parallelSort/binarySearch
 * /copy/copyOf/copyOfRange/clone
 * /indexOfXXX/lastIndexOfXXX/containsXXX/fill/concat/add
 * /addAll/insert/remove/removeAll/delete/deleteAll/replaceAll
 * /reverse/swap/rotate/shuffle
 * /sum/avg/min/max/median/join/filter/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Collection operations:<br>
 * <b>newXXX/asXXX/sort/parallelSort/binarySearch/isNullOrEmpty
 * /notNullOrEmpty/checkNullOrEmpty/indexOfXXX/containsXXX
 * /xxx2String/string2XXX/fill/reverse/swap/sum/avg/min/max/median/filter/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Map operations:<br>
 * <b>newXXX/asXXX/isNullOrEmpty/notNullOrEmpty/checkNullOrEmpty/entity2Map/
 * deepEntity2Map/entity2FlatMap/map2Entity/filter/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Entity/Properties operations:<br>
 * <b>newXXX/asXXX/copy/clone/erase/eraseAll/getPropValue/setPropValue/
 * setPropValueByGet/entity2Map/deepEntity2Map/entity2FlatMap/map2Entity/formalizePropName/getPropNameByMethod/...</b>
 * <li>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Date/Calendar operations:<br>
 * <b>asXXX/format/currentXXX/roll/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>primitive types operations:<br>
 * <b>arrayOf/listOf/isPrimitive/isPrimitiveWapper/isPrimitiveOrWapper/sum/avg/min/max/median/unwrap/wrap/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Factory(create/new) operations/:<br>
 * <b>newXXX/createXXX/asXXX/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>Reflect Class/Method operations:<br>
 * <b>newXXX/getPackage/getPackageName/forClass/getClassName/getSimpleClassName
 * /getCanonicalClassName /getDeclaredConstructor
 * /getDeclaredMethod/findDeclaredMethodByName
 * /getEnclosingClass/getPropField/getPropGetMethod/getPropGetMethodList
 * /getPropSetMethod/getPropSetMethodList/invokeConstructor/invokeMethod/...</b>
 *
 * <br>
 * <br>
 * <li> =======================================================================
 * <li>General Object operations:<br>
 * <b>stringOf/valueOf/toString/hashCod/equals/deepToString/deepHashCodee
 * /deepEquals/compare/json2XML/xml2JSON
 * /base64XXXEncode/base64XXXDecode/urlEncode
 * /urlDecode/getDefaultValue/uuid/enumListOf/enumSetOf
 * /registerNonEntityClass/registerPropGetSetMethod/registerXMLBindingClassForPropGetSetMethod/getType
 * /asyncExecute/getCharsForReadOnly/sleep/println/...</b>
 *
 * </ul>
 *
 * <p>
 *
 * When to throw exception? It's designed to avoid throwing any unnecessary
 * exception if the contract defined by method is not broken. for example, if
 * user tries to reverse a null or empty String. the input String will be
 * returned. But exception will be thrown if trying to repeat/swap a null or
 * empty string or operate Array/Collection by adding/removing... <br>
 *
 * @author Haiyang Li
 *
 * @version $Revision: 0.8 $ 07/03/10
 */
public final class N {
    private static final Logger logger = LoggerFactory.getLogger(N.class);

    private static final AsyncExecutor asyncExecutor = new AsyncExecutor(256, 300L, TimeUnit.SECONDS);

    // ... it has to be big enough to make it's safety to add element to
    // ArrayBlockingQueue.
    private static final int POOL_SIZE;

    static {
        int multi = (int) (Runtime.getRuntime().maxMemory() / ((1024 * 1024) * 256));

        POOL_SIZE = N.max(1000, N.min(1000 * multi, 8192));
    }

    // ...
    static final String ELEMENT_SEPARATOR = Type.ELEMENT_SEPARATOR;
    static final char[] ELEMENT_SEPARATOR_CHAR_ARRAY = Type.ELEMENT_SEPARATOR_CHAR_ARRAY;

    /**
     * An empty immutable {@code Boolean} array.
     */
    static final Boolean[] EMPTY_BOOLEAN_OBJECT_ARRAY = new Boolean[0];
    /**
     * An empty immutable {@code Character} array.
     */
    static final Character[] EMPTY_CHARACTER_OBJECT_ARRAY = new Character[0];
    /**
     * An empty immutable {@code Byte} array.
     */
    static final Byte[] EMPTY_BYTE_OBJECT_ARRAY = new Byte[0];
    /**
     * An empty immutable {@code Short} array.
     */
    static final Short[] EMPTY_SHORT_OBJECT_ARRAY = new Short[0];
    /**
     * An empty immutable {@code Integer} array.
     */
    static final Integer[] EMPTY_INTEGER_OBJECT_ARRAY = new Integer[0];
    /**
     * An empty immutable {@code Long} array.
     */
    static final Long[] EMPTY_LONG_OBJECT_ARRAY = new Long[0];
    /**
     * An empty immutable {@code Float} array.
     */
    static final Float[] EMPTY_FLOAT_OBJECT_ARRAY = new Float[0];
    /**
     * An empty immutable {@code Double} array.
     */
    static final Double[] EMPTY_DOUBLE_OBJECT_ARRAY = new Double[0];
    /**
     * An empty immutable {@code Class} array.
     */
    static final Class<?>[] EMPTY_CLASS_ARRAY = new Class[0];
    static final String NULL_STRING = "null".intern();
    static final char[] NULL_CHAR_ARRAY = NULL_STRING.toCharArray();
    static final String TRUE = Boolean.TRUE.toString().intern();
    static final char[] TRUE_CHAR_ARRAY = TRUE.toCharArray();
    static final String FALSE = Boolean.FALSE.toString().intern();
    static final char[] FALSE_CHAR_ARRAY = FALSE.toCharArray();

    // ...
    static final char CHAR_0 = D.CHAR_0;

    /**
     *
     * @see <a
     *      href="http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.6">JLF:
     *      Escape Sequences for Character and String Literals</a>
     * @since 2.2
     */
    static final char CHAR_LF = D.CHAR_LF;

    /**
     *
     * @see <a
     *      href="http://docs.oracle.com/javase/specs/jls/se7/html/jls-3.html#jls-3.10.6">JLF:
     *      Escape Sequences for Character and String Literals</a>
     * @since 2.2
     */
    static final char CHAR_CR = D.CHAR_CR;

    // ...
    /**
     * The index value when an element is not found in a list or array:
     * {@code -1}. This value is returned by methods in this class and can also
     * be used in comparisons with values returned by various method from
     * {@link java.util.List} .
     */
    static final int INDEX_NOT_FOUND = -1;

    // ...
    public static final TimeZone UTC_TIME_ZONE = TimeZone.getTimeZone("UTC");
    /**
     * The system default time zone
     */
    public static final TimeZone LOCAL_TIME_ZONE = Calendar.getInstance().getTimeZone();

    /**
     * Date format.
     */
    public static final String LOCAL_YEAR_FORMAT = "yyyy";
    public static final String LOCAL_MONTH_DAY_FORMAT = "MM-dd";
    static final String LOCAL_MONTH_DAY_FORMAT_SLASH = "MM/dd";
    public static final String LOCAL_DATE_FORMAT = "yyyy-MM-dd";
    static final String LOCAL_DATE_FORMAT_SLASH = "yyyy/MM/dd";
    public static final String LOCAL_TIME_FORMAT = "HH:mm:ss";
    public static final String LOCAL_DATETIME_FORMAT = "yyyy-MM-dd HH:mm:ss";
    static final String LOCAL_DATETIME_FORMAT_SLASH = "yyyy/MM/dd HH:mm:ss";
    public static final String LOCAL_TIMESTAMP_FORMAT = "yyyy-MM-dd HH:mm:ss.SSS";
    static final String LOCAL_TIMESTAMP_FORMAT_SLASH = "yyyy/MM/dd HH:mm:ss.SSS";

    /**
     * It's default date/time format.
     */
    public static final String ISO_8601_DATETIME_FORMAT = "yyyy-MM-dd'T'HH:mm:ss'Z'";
    static final String ISO_8601_DATETIME_FORMAT_SLASH = "yyyy/MM/dd'T'HH:mm:ss'Z'";
    /**
     * It's default timestamp format.
     */
    public static final String ISO_8601_TIMESTAMP_FORMAT = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";
    static final String ISO_8601_TIMESTAMP_FORMAT_SLASH = "yyyy/MM/dd'T'HH:mm:ss.SSS'Z'";

    /**
     * This constant defines the date format specified by
     * RFC 1123 / RFC 822. Used for parsing via `SimpleDateFormat` as well as
     * error messages.
     */
    public final static String RFC1123_DATE_FORMAT = "EEE, dd MMM yyyy HH:mm:ss zzz";

    private static final Map<String, Queue<DateFormat>> dfPool = new ObjectPool<>(64);
    private static final Map<TimeZone, Queue<Calendar>> calendarPool = new ObjectPool<>(64);
    private static final Queue<DateFormat> utcTimestampDFPool = new ArrayBlockingQueue<>(POOL_SIZE);
    private static final Queue<DateFormat> utcDateTimeDFPool = new ArrayBlockingQueue<>(POOL_SIZE);
    private static final Queue<Calendar> utcCalendarPool = new ArrayBlockingQueue<>(POOL_SIZE);
    // ...
    private static final Queue<char[]> utcTimestampFormatCharsPool = new ArrayBlockingQueue<>(POOL_SIZE);
    private static final DatatypeFactory dataTypeFactory;

    static {
        DatatypeFactory temp = null;

        try {
            temp = DatatypeFactory.newInstance();
        } catch (Exception e) {
            // ignore.
            // logger.error("Failed to initialize XMLGregorianCalendarType: " +
            // e.getMessage(), e);
        }

        dataTypeFactory = temp;
    }

    /**
     * An empty immutable {@code boolean} array.
     */
    public static final boolean[] EMPTY_BOOLEAN_ARRAY = new boolean[0];
    /**
     * An empty immutable {@code char} array.
     */
    public static final char[] EMPTY_CHAR_ARRAY = new char[0];
    /**
     * An empty immutable {@code byte} array.
     */
    public static final byte[] EMPTY_BYTE_ARRAY = new byte[0];
    /**
     * An empty immutable {@code short} array.
     */
    public static final short[] EMPTY_SHORT_ARRAY = new short[0];
    /**
     * An empty immutable {@code int} array.
     */
    public static final int[] EMPTY_INT_ARRAY = new int[0];
    /**
     * An empty immutable {@code long} array.
     */
    public static final long[] EMPTY_LONG_ARRAY = new long[0];
    /**
     * An empty immutable {@code float} array.
     */
    public static final float[] EMPTY_FLOAT_ARRAY = new float[0];
    /**
     * An empty immutable {@code double} array.
     */
    public static final double[] EMPTY_DOUBLE_ARRAY = new double[0];
    /**
     * An empty immutable {@code String} array.
     */
    public static final String[] EMPTY_STRING_ARRAY = new String[0];
    /**
     * An empty immutable {@code Object} array.
     */
    public static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];
    @SuppressWarnings("rawtypes")
    public static final List EMPTY_LIST = Collections.EMPTY_LIST;
    @SuppressWarnings("rawtypes")
    public static final Set EMPTY_SET = Collections.EMPTY_SET;
    @SuppressWarnings("rawtypes")
    public static final Map EMPTY_MAP = Collections.EMPTY_MAP;
    @SuppressWarnings("rawtypes")
    public static final Iterator EMPTY_ITERATOR = EMPTY_LIST.iterator();
    // ...
    public static final String EMPTY_STRING = "".intern();

    // ...
    public static final Object NULL_MASK = new NullMask();

    // ...
    private static final char[][][] cbufOfSTDInt = new char[5][][];

    static {
        for (int i = 0, j = 1; i < 5; i++, j = j * 10) {
            cbufOfSTDInt[i] = new char[j][];

            for (int k = 0; k < j; k++) {
                if (i == 1) {
                    cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                } else if (i == 2) {
                    if (k < 10) {
                        cbufOfSTDInt[i][k] = ("0" + String.valueOf(k)).toCharArray();
                    } else {
                        cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                    }
                } else if (i == 3) {
                    if (k < 10) {
                        cbufOfSTDInt[i][k] = ("00" + String.valueOf(k)).toCharArray();
                    } else if (k < 100) {
                        cbufOfSTDInt[i][k] = ("0" + String.valueOf(k)).toCharArray();
                    } else {
                        cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                    }
                } else if (i == 4) {
                    if (k < 10) {
                        cbufOfSTDInt[i][k] = ("000" + String.valueOf(k)).toCharArray();
                    } else if (k < 100) {
                        cbufOfSTDInt[i][k] = ("00" + String.valueOf(k)).toCharArray();
                    } else if (k < 1000) {
                        cbufOfSTDInt[i][k] = ("0" + String.valueOf(k)).toCharArray();
                    } else {
                        cbufOfSTDInt[i][k] = String.valueOf(k).toCharArray();
                    }
                }
            }
        }
    }

    static final String BACKSLASH_ASTERISK = "*";

    // ...
    private static final int REVERSE_THRESHOLD = 18;
    private static final int FILL_THRESHOLD = 25;
    private static final int REPLACEALL_THRESHOLD = 11;

    // ...
    static final Random RAND = new SecureRandom();

    @SuppressWarnings("rawtypes")
    private static final Comparator NULL_MIN_COMPARATOR = Comparators.nullsFirst();
    @SuppressWarnings("rawtypes")
    private static final Comparator NULL_MAX_COMPARATOR = Comparators.nullsLast();
    @SuppressWarnings("rawtypes")
    private static final Comparator OBJ_COMPARATOR = Comparators.OBJ_COMPARATOR;

    // ...
    static final BiMap<Class<?>, Class<?>> PRIMITIVE_2_WRAPPER = new BiMap<>();

    static {
        PRIMITIVE_2_WRAPPER.put(boolean.class, Boolean.class);
        PRIMITIVE_2_WRAPPER.put(char.class, Character.class);
        PRIMITIVE_2_WRAPPER.put(byte.class, Byte.class);
        PRIMITIVE_2_WRAPPER.put(short.class, Short.class);
        PRIMITIVE_2_WRAPPER.put(int.class, Integer.class);
        PRIMITIVE_2_WRAPPER.put(long.class, Long.class);
        PRIMITIVE_2_WRAPPER.put(float.class, Float.class);
        PRIMITIVE_2_WRAPPER.put(double.class, Double.class);

        PRIMITIVE_2_WRAPPER.put(boolean[].class, Boolean[].class);
        PRIMITIVE_2_WRAPPER.put(char[].class, Character[].class);
        PRIMITIVE_2_WRAPPER.put(byte[].class, Byte[].class);
        PRIMITIVE_2_WRAPPER.put(short[].class, Short[].class);
        PRIMITIVE_2_WRAPPER.put(int[].class, Integer[].class);
        PRIMITIVE_2_WRAPPER.put(long[].class, Long[].class);
        PRIMITIVE_2_WRAPPER.put(float[].class, Float[].class);
        PRIMITIVE_2_WRAPPER.put(double[].class, Double[].class);
    }

    // ...
    static final Map<Class<?>, Object> CLASS_EMPTY_ARRAY = new ConcurrentHashMap<>();

    static {
        CLASS_EMPTY_ARRAY.put(boolean.class, N.EMPTY_BOOLEAN_ARRAY);
        CLASS_EMPTY_ARRAY.put(Boolean.class, N.EMPTY_BOOLEAN_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(char.class, N.EMPTY_CHAR_ARRAY);
        CLASS_EMPTY_ARRAY.put(Character.class, N.EMPTY_CHARACTER_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(byte.class, N.EMPTY_BYTE_ARRAY);
        CLASS_EMPTY_ARRAY.put(Byte.class, N.EMPTY_BYTE_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(short.class, N.EMPTY_SHORT_ARRAY);
        CLASS_EMPTY_ARRAY.put(Short.class, N.EMPTY_SHORT_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(int.class, N.EMPTY_INT_ARRAY);
        CLASS_EMPTY_ARRAY.put(Integer.class, N.EMPTY_INTEGER_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(long.class, N.EMPTY_LONG_ARRAY);
        CLASS_EMPTY_ARRAY.put(Long.class, N.EMPTY_LONG_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(float.class, N.EMPTY_FLOAT_ARRAY);
        CLASS_EMPTY_ARRAY.put(Float.class, N.EMPTY_FLOAT_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(double.class, N.EMPTY_DOUBLE_ARRAY);
        CLASS_EMPTY_ARRAY.put(Double.class, N.EMPTY_DOUBLE_OBJECT_ARRAY);

        CLASS_EMPTY_ARRAY.put(String.class, N.EMPTY_STRING_ARRAY);
        CLASS_EMPTY_ARRAY.put(Object.class, N.EMPTY_OBJECT_ARRAY);
    }

    // ...    
    static final Map<Class<?>, Integer> CLASS_TYPE_ENUM = new HashMap<>();

    static {
        CLASS_TYPE_ENUM.put(boolean.class, 1);
        CLASS_TYPE_ENUM.put(char.class, 2);
        CLASS_TYPE_ENUM.put(byte.class, 3);
        CLASS_TYPE_ENUM.put(short.class, 4);
        CLASS_TYPE_ENUM.put(int.class, 5);
        CLASS_TYPE_ENUM.put(long.class, 6);
        CLASS_TYPE_ENUM.put(float.class, 7);
        CLASS_TYPE_ENUM.put(double.class, 8);
        CLASS_TYPE_ENUM.put(String.class, 9);
        CLASS_TYPE_ENUM.put(boolean[].class, 11);
        CLASS_TYPE_ENUM.put(char[].class, 12);
        CLASS_TYPE_ENUM.put(byte[].class, 13);
        CLASS_TYPE_ENUM.put(short[].class, 14);
        CLASS_TYPE_ENUM.put(int[].class, 15);
        CLASS_TYPE_ENUM.put(long[].class, 16);
        CLASS_TYPE_ENUM.put(float[].class, 17);
        CLASS_TYPE_ENUM.put(double[].class, 18);
        CLASS_TYPE_ENUM.put(String[].class, 19);

        CLASS_TYPE_ENUM.put(Boolean.class, 21);
        CLASS_TYPE_ENUM.put(Character.class, 22);
        CLASS_TYPE_ENUM.put(Byte.class, 23);
        CLASS_TYPE_ENUM.put(Short.class, 24);
        CLASS_TYPE_ENUM.put(Integer.class, 25);
        CLASS_TYPE_ENUM.put(Long.class, 26);
        CLASS_TYPE_ENUM.put(Float.class, 27);
        CLASS_TYPE_ENUM.put(Double.class, 28);
    }

    // ...
    private static final Map<Class<? extends Enum<?>>, List<? extends Enum<?>>> enumListPool = new ObjectPool<>(POOL_SIZE);
    private static final Map<Class<? extends Enum<?>>, Set<? extends Enum<?>>> enumSetPool = new ObjectPool<>(POOL_SIZE);
    private static final Map<Class<? extends Enum<?>>, BiMap<? extends Enum<?>, String>> enumMapPool = new ObjectPool<>(POOL_SIZE);

    private static final Map<String, Type<?>> nameTypePool = new ObjectPool<>(POOL_SIZE);
    private static final Map<Class<?>, Type<?>> clsTypePool = new ObjectPool<>(POOL_SIZE);

    // ...
    private static final Map<Class<?>, Boolean> entityClassPool = new ObjectPool<>(POOL_SIZE);
    private static final Map<Class<?>, Boolean> dirtyMarkerClassPool = new ObjectPool<>(POOL_SIZE);
    private static final Map<Class<?>, Boolean> dirtyMarkerEntityClassPool = new ObjectPool<>(POOL_SIZE);

    private static final JSONParser jsonParser = ParserFactory.createJSONParser();
    private static final XMLParser abacusXMLParser = ParserFactory.isAbacusXMLAvailable() ? ParserFactory.createAbacusXMLParser() : null;
    private static final XMLParser xmlParser = ParserFactory.isXMLAvailable() ? ParserFactory.createXMLParser() : null;
    private static final KryoParser kryoParser = ParserFactory.isKryoAvailable() ? ParserFactory.createKryoParser() : null;
    private static final JSONSerializationConfig jsc = JSC.of(true, true);
    private static final XMLSerializationConfig xscForClone = XSC.create().setIgnoreTypeInfo(false);

    // ...
    static final Field strValueField;
    static volatile boolean isStringCharsGettable = true;
    static final Constructor<String> sharedStringConstructor;
    static final Field listElementDataField;
    static final Field listSizeField;
    static volatile boolean isListElementDataFieldGettable = true;
    static volatile boolean isListElementDataFieldSettable = true;

    static {
        Field tmp = null;

        try {
            tmp = String.class.getDeclaredField("offset");
        } catch (Exception e) {
            // ignore.
        }

        if (tmp == null) {
            try {
                tmp = String.class.getDeclaredField("count");
            } catch (Exception e) {
                // ignore.
            }
        }

        if (tmp == null) {
            try {
                tmp = String.class.getDeclaredField("value");
            } catch (Exception e) {
                // ignore.
            }
        }

        strValueField = ((tmp != null) && tmp.getName().equals("value") && tmp.getType().equals(char[].class)) ? tmp : null;

        if (strValueField != null) {
            strValueField.setAccessible(true);
        }

        Constructor<String> tmpConstructor = null;

        try {
            tmpConstructor = String.class.getDeclaredConstructor(char[].class, boolean.class);
            tmpConstructor.setAccessible(true);
        } catch (Exception e) {
            // ignore.
        }

        sharedStringConstructor = tmpConstructor;

        tmp = null;

        try {
            tmp = ArrayList.class.getDeclaredField("elementData");
        } catch (Exception e) {
            // ignore.
        }

        listElementDataField = tmp != null && tmp.getType().equals(Object[].class) ? tmp : null;

        if (listElementDataField != null) {
            listElementDataField.setAccessible(true);
        }

        tmp = null;

        try {
            tmp = ArrayList.class.getDeclaredField("size");
        } catch (Exception e) {
            // ignore.
        }

        listSizeField = tmp != null && tmp.getType().equals(int.class) ? tmp : null;

        if (listSizeField != null) {
            listSizeField.setAccessible(true);
        }
    }

    /**
     * A regex pattern for recognizing blocks of whitespace characters. The
     * apparent convolutedness of the pattern serves the purpose of ignoring
     * "blocks" consisting of only a single space: the pattern is used only to
     * normalize whitespace, condensing "blocks" down to a single space, thus
     * matching the same would likely cause a great many noop replacements.
     */
    private static final Pattern WHITESPACE_PATTERN = Pattern.compile("(?: |\\u00A0|\\s|[\\s&&[^ ]])\\s*");

    static final String[] charStringCache = new String[128];
    static final int intStringCacheLow = -1001;
    static final int intStringCacheHigh = 10001;
    static final String[] intStringCache = new String[intStringCacheHigh - intStringCacheLow];
    static final Map<String, Integer> stringIntCache = new HashMap<>((int) (intStringCache.length * 1.5));

    static {
        for (int i = 0, j = intStringCacheLow, len = intStringCache.length; i < len; i++, j++) {
            intStringCache[i] = Integer.valueOf(j).toString();
            stringIntCache.put(intStringCache[i], j);
        }

        for (int i = 0; i < charStringCache.length; i++) {
            charStringCache[i] = String.valueOf((char) i);
        }
    }

    static final Type<Boolean> booleanType = N.typeOf(boolean.class);
    static final Type<Character> charType = N.typeOf(char.class);
    static final Type<Byte> byteType = N.typeOf(byte.class);
    static final Type<Short> shortType = N.typeOf(short.class);
    static final Type<Integer> intType = N.typeOf(int.class);
    static final Type<Long> longType = N.typeOf(long.class);
    static final Type<Float> floatType = N.typeOf(float.class);
    static final Type<Double> doubleType = N.typeOf(double.class);

    /**
     * Constructor for
     */
    private N() {
        // no instance();
    }

    @SuppressWarnings("unchecked")
    public static <T> Type<T> typeOf(final String typeName) {
        if (typeName == null) {
            return null;
        }

        Type<?> type = nameTypePool.get(typeName);

        if (type == null) {
            type = TypeFactory.getType(typeName);

            nameTypePool.put(typeName, type);
        }

        return (Type<T>) type;
    }

    @SuppressWarnings("unchecked")
    public static <T> Type<T> typeOf(final Class<?> cls) {
        if (cls == null) {
            return null;
        }

        Type<?> type = clsTypePool.get(cls);

        if (type == null) {
            type = TypeFactory.getType(cls);
            clsTypePool.put(cls, type);
        }

        return (Type<T>) type;
    }

    @SuppressWarnings("unchecked")
    @SafeVarargs
    public static <T> List<Type<T>> typeOf(final Class<?>... classes) {
        if (N.isNullOrEmpty(classes)) {
            return new ArrayList<>();
        }

        final List<Type<T>> result = new ArrayList<>(classes.length);

        for (int i = 0, len = classes.length; i < len; i++) {
            result.add((Type<T>) typeOf(classes[i]));
        }

        return result;
    }

    @SuppressWarnings("unchecked")
    public static <T> List<Type<T>> typeOf(final Collection<? extends Class<?>> classes) {
        final List<Type<T>> result = new ArrayList<>(classes.size());

        for (Class<?> cls : classes) {
            result.add((Type<T>) typeOf(cls));
        }

        return result;
    }

    public static String stringOf(final boolean val) {
        return String.valueOf(val);
    }

    public static String stringOf(final char val) {
        if (val < 128) {
            return charStringCache[val];
        }

        return String.valueOf(val);
    }

    public static String stringOf(final byte val) {
        if (val > intStringCacheLow && val < intStringCacheHigh) {
            return intStringCache[val - intStringCacheLow];
        }

        return String.valueOf(val);
    }

    public static String stringOf(final short val) {
        if (val > intStringCacheLow && val < intStringCacheHigh) {
            return intStringCache[val - intStringCacheLow];
        }

        return String.valueOf(val);
    }

    public static String stringOf(final int val) {
        if (val > intStringCacheLow && val < intStringCacheHigh) {
            return intStringCache[val - intStringCacheLow];
        }

        return String.valueOf(val);
    }

    public static String stringOf(final long val) {
        if (val > intStringCacheLow && val < intStringCacheHigh) {
            return intStringCache[(int) (val - intStringCacheLow)];
        }

        return String.valueOf(val);
    }

    public static String stringOf(final float val) {
        return String.valueOf(val);
    }

    public static String stringOf(final double val) {
        return String.valueOf(val);
    }

    /**
     *
     * @param obj
     * @return <code>null</code> if the specified object is null.
     */
    public static String stringOf(final Object obj) {
        return (obj == null) ? null : N.typeOf(obj.getClass()).stringOf(obj);
    }

    /**
     *
     * @param targetClass
     * @param str
     * @return the default value of the specified <code>targetClass</code> if the specified string is null.
     */
    @SuppressWarnings("unchecked")
    public static <T> T valueOf(final Class<T> targetClass, final String str) {
        return (str == null) ? defaultValueOf(targetClass) : (T) N.typeOf(targetClass).valueOf(str);
    }

    @SuppressWarnings("unchecked")
    public static <T> T defaultValueOf(final Class<T> cls) {
        return (T) N.typeOf(cls).defaultValue();
    }

    public static <E extends Enum<E>> List<E> enumListOf(final Class<E> enumClass) {
        List<E> enumList = (List<E>) enumListPool.get(enumClass);

        if (enumList == null) {
            enumList = ImmutableList.of(N.asList(enumClass.getEnumConstants()));

            enumListPool.put(enumClass, enumList);
        }

        return enumList;
    }

    public static <E extends Enum<E>> Set<E> enumSetOf(final Class<E> enumClass) {
        Set<E> enumSet = (Set<E>) enumSetPool.get(enumClass);

        if (enumSet == null) {
            enumSet = ImmutableSet.of(EnumSet.allOf(enumClass));

            enumSetPool.put(enumClass, enumSet);
        }

        return enumSet;
    }

    public static <E extends Enum<E>> BiMap<E, String> enumMapOf(final Class<E> enumClass) {
        BiMap<E, String> enumMap = (BiMap<E, String>) enumMapPool.get(enumClass);

        if (enumMap == null) {
            final EnumMap<E, String> keyMap = new EnumMap<>(enumClass);
            final Map<String, E> valueMap = new HashMap<>();

            for (final E e : enumClass.getEnumConstants()) {
                keyMap.put(e, e.name());
                valueMap.put(e.name(), e);
            }

            enumMap = new BiMap<>(ImmutableMap.of(keyMap), ImmutableMap.of(valueMap));

            enumMapPool.put(enumClass, enumMap);
        }

        return enumMap;
    }

    /**
     * Method newInstance.
     *
     * @param cls
     * @return T
     */
    public static <T> T newInstance(final Class<T> cls) {
        if (Modifier.isAbstract(cls.getModifiers())) {
            if (cls.equals(Map.class)) {
                return (T) new HashMap<>();
            } else if (cls.equals(List.class)) {
                return (T) new ArrayList<>();
            } else if (cls.equals(Set.class)) {
                return (T) new HashSet<>();
            } else if (cls.equals(Queue.class)) {
                return (T) new ArrayDeque<>();
            } else if (cls.equals(SortedSet.class)) {
                return (T) new TreeSet<>();
            } else if (cls.equals(SortedMap.class)) {
                return (T) new TreeMap<>();
            }
        }

        try {
            if (Modifier.isStatic(cls.getModifiers()) == false && (cls.isAnonymousClass() || cls.isMemberClass())) {
                // http://stackoverflow.com/questions/2097982/is-it-possible-to-create-an-instance-of-nested-class-using-java-reflection

                final List<Class<?>> toInstantiate = new ArrayList<>();
                Class<?> parent = cls.getEnclosingClass();

                do {
                    toInstantiate.add(parent);
                    parent = parent.getEnclosingClass();
                } while (parent != null && Modifier.isStatic(parent.getModifiers()) == false && (parent.isAnonymousClass() || parent.isMemberClass()));

                if (parent != null) {
                    toInstantiate.add(parent);
                }

                N.reverse(toInstantiate);

                Object instance = null;
                for (Class<?> current : toInstantiate) {
                    instance = instance == null ? invoke(ClassUtil.getDeclaredConstructor(current))
                            : invoke(ClassUtil.getDeclaredConstructor(current, instance.getClass()), instance);
                }

                return invoke(ClassUtil.getDeclaredConstructor(cls, instance.getClass()), instance);
            } else {
                return invoke(ClassUtil.getDeclaredConstructor(cls));
            }
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
            throw N.toRuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private static <T> T invoke(final Constructor<T> c, final Object... args)
            throws InstantiationException, IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        if (c.isAccessible() == false) {
            c.setAccessible(true);
        }

        return c.newInstance(args);
    }

    public static <T> T newProxyInstance(final Class<T> interfaceClass, final InvocationHandler h) {
        return newProxyInstance(N.asArray(interfaceClass), h);
    }

    /**
     * Refer to {@code java.lang.reflect}
     *
     * @param interfaceClasses
     * @param h
     * @return
     */
    public static <T> T newProxyInstance(final Class<?>[] interfaceClasses, final InvocationHandler h) {
        return (T) Proxy.newProxyInstance(N.class.getClassLoader(), interfaceClasses, h);
    }

    /**
     * Method newArray.
     *
     * @param componentType
     * @param length
     * @return T[]
     */
    @SuppressWarnings("unchecked")
    public static <T> T newArray(final Class<?> componentType, final int length) {
        //        if (length == 0) {
        //            final Object result = CLASS_EMPTY_ARRAY.get(componentType);
        //
        //            if (result != null) {
        //                return (T) result;
        //            }
        //        }

        return (T) Array.newInstance(componentType, length);
    }

    @SuppressWarnings("unchecked")
    public static <T> T newEntity(final Class<T> cls) {
        return newEntity(cls, null);
    }

    public static <T> T newEntity(final Class<T> cls, final String entityName) {
        if (MapEntity.class.isAssignableFrom(cls)) {
            return (T) asMapEntity(entityName);
        }

        final Class<?> enclosingClass = ClassUtil.getEnclosingClass(cls);

        if (enclosingClass == null || Modifier.isStatic(cls.getModifiers())) {
            return newInstance(cls);
        } else {
            return ClassUtil.invokeConstructor(ClassUtil.getDeclaredConstructor(cls, enclosingClass), newInstance(enclosingClass));
        }
    }

    /**
     *
     * @param a
     *            the specified array should not be modified after it's used to
     *            create the new String.
     * @param share
     *            the same array will be shared with the new created ArrayList
     *            if it's true.
     * @return
     */
    @Internal
    static String newString(final char[] a, final boolean share) {
        if (share && sharedStringConstructor != null) {
            try {
                return sharedStringConstructor.newInstance(a, true);
            } catch (Exception e) {
                throw new AbacusException(e);
            }
        } else {
            return String.valueOf(a);
        }
    }

    /**
     * Returns a set backed by the specified map.
     *
     * @param map the backing map
     * @return the set backed by the map
     * @see Collections#newSetFromMap(Map)
     */
    public static <E> Set<E> newSetFromMap(final Map<E, Boolean> map) {
        return Collections.newSetFromMap(map);
    }

    public static int initHashCapacity(final int size) {
        return size < MAX_HASH_LENGTH ? (int) (size * 1.25) + 1 : MAX_ARRAY_SIZE;
    }

    public static <T> ArrayList<T> newArrayList() {
        return new ArrayList<>();
    }

    public static <T> ArrayList<T> newArrayList(int initialCapacity) {
        return new ArrayList<>(initialCapacity);
    }

    public static <T> ArrayList<T> newArrayList(Collection<? extends T> c) {
        return new ArrayList<>(c);
    }

    public static <T> LinkedList<T> newLinkedList() {
        return new LinkedList<>();
    }

    public static <T> LinkedList<T> newLinkedList(Collection<? extends T> c) {
        return new LinkedList<>(c);
    }

    public static <T> HashSet<T> newHashSet() {
        return new HashSet<>();
    }

    /**
     * 
     * @param initialCapacity multiply 1.25 as the initial capacity of new HashSet
     * @return
     */
    public static <T> HashSet<T> newHashSet(int initialCapacity) {
        return new HashSet<>(initHashCapacity(initialCapacity));
    }

    public static <T> HashSet<T> newHashSet(Collection<? extends T> c) {
        return new HashSet<>(c);
    }

    public static <T> LinkedHashSet<T> newLinkedHashSet() {
        return new LinkedHashSet<>();
    }

    /**
     * 
     * @param initialCapacity multiply 1.25 as the initial capacity of new HashSet
     * @return
     */
    public static <T> LinkedHashSet<T> newLinkedHashSet(int initialCapacity) {
        return new LinkedHashSet<>(initHashCapacity(initialCapacity));
    }

    public static <T> LinkedHashSet<T> newLinkedHashSet(Collection<? extends T> c) {
        return new LinkedHashSet<>(c);
    }

    public static <K, V> HashMap<K, V> newHashMap() {
        return new HashMap<>();
    }

    /**
     * 
     * @param initialCapacity multiply 1.25 as the initial capacity of new HashSet
     * @return
     */
    public static <K, V> HashMap<K, V> newHashMap(int initialCapacity) {
        return new HashMap<>(initHashCapacity(initialCapacity));
    }

    public static <K, V> HashMap<K, V> newHashMap(Map<? extends K, ? extends V> m) {
        return new HashMap<>(m);
    }

    public static <K, V> HashMap<K, V> newHashMap(final Collection<? extends V> c, final Function<? super V, ? extends K> keyExtractor) {
        N.requireNonNull(keyExtractor);

        if (isNullOrEmpty(c)) {
            return new HashMap<>();
        }

        final HashMap<K, V> result = new HashMap<>(N.initHashCapacity(c.size()));

        for (V v : c) {
            result.put(keyExtractor.apply(v), v);
        }

        return result;
    }

    public static <K, V> LinkedHashMap<K, V> newLinkedHashMap() {
        return new LinkedHashMap<>();
    }

    /**
     * 
     * @param initialCapacity multiply 1.25 as the initial capacity of new HashSet
     * @return
     */
    public static <K, V> LinkedHashMap<K, V> newLinkedHashMap(int initialCapacity) {
        return new LinkedHashMap<>(initHashCapacity(initialCapacity));
    }

    public static <K, V> LinkedHashMap<K, V> newLinkedHashMap(Map<? extends K, ? extends V> m) {
        return new LinkedHashMap<>(m);
    }

    public static <K, V> LinkedHashMap<K, V> newLinkedHashMap(final Collection<? extends V> c, final Function<? super V, ? extends K> keyExtractor) {
        N.requireNonNull(keyExtractor);

        if (isNullOrEmpty(c)) {
            return new LinkedHashMap<>();
        }

        final LinkedHashMap<K, V> result = new LinkedHashMap<>(N.initHashCapacity(c.size()));

        for (V v : c) {
            result.put(keyExtractor.apply(v), v);
        }

        return result;
    }

    public static <K, V> ConcurrentHashMap<K, V> newConcurrentHashMap() {
        return new ConcurrentHashMap<>();
    }

    /**
     * 
     * @param initCapacity multiply 1.25 as the initial capacity of new HashSet
     * @return
     */
    public static <K, V> ConcurrentHashMap<K, V> newConcurrentHashMap(int initialCapacity) {
        return new ConcurrentHashMap<>(initHashCapacity(initialCapacity));
    }

    public static <K, V> ConcurrentHashMap<K, V> newConcurrentHashMap(Map<? extends K, ? extends V> m) {
        return new ConcurrentHashMap<>(m);
    }

    public static <K, E> ListMultimap<K, E> newListMultimap() {
        return new ListMultimap<>();
    }

    public static <K, E> ListMultimap<K, E> newListMultimap(final int initialCapacity) {
        return new ListMultimap<>(initialCapacity);
    }

    public static <K, E> ListMultimap<K, E> newListMultimap(final Map<? extends K, ? extends E> m) {
        final ListMultimap<K, E> multiMap = newListMultimap();

        multiMap.putAll(m);

        return multiMap;
    }

    public static <K, E> ListMultimap<K, E> newListLinkedMultimap() {
        return new ListMultimap<>(LinkedHashMap.class, ArrayList.class);
    }

    public static <K, E> ListMultimap<K, E> newListLinkedMultimap(final int initialCapacity) {
        return new ListMultimap<>(new LinkedHashMap<K, List<E>>(initialCapacity < 0 ? 9 : initialCapacity), ArrayList.class);
    }

    public static <K, E> ListMultimap<K, E> newListLinkedMultimap(final Map<? extends K, ? extends E> m) {
        final ListMultimap<K, E> multiMap = new ListMultimap<>(new LinkedHashMap<K, List<E>>(), ArrayList.class);

        multiMap.putAll(m);

        return multiMap;
    }

    public static <K, E> ListMultimap<K, E> newListSortedMultimap() {
        return new ListMultimap<>(new TreeMap<K, List<E>>(), ArrayList.class);
    }

    public static <K, E> ListMultimap<K, E> newListSortedMultimap(final Map<? extends K, ? extends E> m) {
        final ListMultimap<K, E> multiMap = new ListMultimap<>(new TreeMap<K, List<E>>(), ArrayList.class);

        multiMap.putAll(m);

        return multiMap;
    }

    @SuppressWarnings("rawtypes")
    public static <K, E> ListMultimap<K, E> newListMultimap(final Class<? extends Map> mapType) {
        return new ListMultimap<>(mapType, ArrayList.class);
    }

    @SuppressWarnings("rawtypes")
    public static <K, E> ListMultimap<K, E> newListMultimap(final Class<? extends Map> mapType, final Class<? extends List> valueType) {
        return new ListMultimap<>(mapType, valueType);
    }

    public static <K, E> SetMultimap<K, E> newSetMultimap() {
        return new SetMultimap<>();
    }

    public static <K, E> SetMultimap<K, E> newSetMultimap(final int initialCapacity) {
        return new SetMultimap<>(initialCapacity);
    }

    public static <K, E> SetMultimap<K, E> newSetMultimap(final Map<? extends K, ? extends E> m) {
        final SetMultimap<K, E> multiMap = newSetMultimap();

        multiMap.putAll(m);

        return multiMap;
    }

    public static <K, E> SetMultimap<K, E> newSetLinkedMultimap() {
        return new SetMultimap<>(LinkedHashMap.class, HashSet.class);
    }

    public static <K, E> SetMultimap<K, E> newSetLinkedMultimap(final int initialCapacity) {
        return new SetMultimap<>(new LinkedHashMap<K, Set<E>>(initialCapacity < 0 ? 9 : initialCapacity), HashSet.class);
    }

    public static <K, E> SetMultimap<K, E> newSetLinkedMultimap(final Map<? extends K, ? extends E> m) {
        final SetMultimap<K, E> multiMap = new SetMultimap<>(new LinkedHashMap<K, Set<E>>(), HashSet.class);

        multiMap.putAll(m);

        return multiMap;
    }

    public static <K, E> SetMultimap<K, E> newSetSortedMultimap() {
        return new SetMultimap<>(new TreeMap<K, Set<E>>(), HashSet.class);
    }

    public static <K, E> SetMultimap<K, E> newSetSortedMultimap(final Map<? extends K, ? extends E> m) {
        final SetMultimap<K, E> multiMap = new SetMultimap<>(new TreeMap<K, Set<E>>(), HashSet.class);

        multiMap.putAll(m);

        return multiMap;
    }

    @SuppressWarnings("rawtypes")
    public static <K, E> SetMultimap<K, E> newSetMultimap(final Class<? extends Map> mapType) {
        return new SetMultimap<>(mapType, HashSet.class);
    }

    @SuppressWarnings("rawtypes")
    public static <K, E> SetMultimap<K, E> newSetMultimap(final Class<? extends Map> mapType, final Class<? extends Set> valueType) {
        return new SetMultimap<>(mapType, valueType);
    }

    static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;
    static final int MAX_HASH_LENGTH = (int) (MAX_ARRAY_SIZE / 1.25) - 1;

    public static DataSet newDataSet(final String keyColumnName, final String valueColumnName, final Map<?, ?> m) {
        return newDataSet(null, null, keyColumnName, valueColumnName, m);
    }

    /**
     * Convert the specified Map to a two columns <code>DataSet</code>: one column is for keys and one column is for values
     *
     * @param entityName
     * @param entityClass
     * @param keyColumnName
     * @param valueColumnName
     * @param m
     * @return
     */
    public static DataSet newDataSet(final String entityName, final Class<?> entityClass, final String keyColumnName, final String valueColumnName,
            final Map<?, ?> m) {
        final List<Object> keyColumn = new ArrayList<>(m.size());
        final List<Object> valueColumn = new ArrayList<>(m.size());

        for (Map.Entry<?, ?> entry : m.entrySet()) {
            keyColumn.add(entry.getKey());
            valueColumn.add(entry.getValue());
        }

        final List<String> columnNameList = N.asList(keyColumnName, valueColumnName);
        final List<List<Object>> columnList = N.asList(keyColumn, valueColumn);

        return newDataSet(columnNameList, columnList);
    }

    /**
     * Converts a list of row(which can be: Map/Entity/Array/Collection) into a <code>DataSet</code>.
     * 
     * @param rowList
     * @param rowList list of row which can be: Map/Entity/Array/Collection.
     */
    public static <T> DataSet newDataSet(final List<T> rowList) {
        return newDataSet(null, rowList);
    }

    /**
     * Converts a list of row(which can be: Map/Entity/Array/Collection) into a <code>DataSet</code>.
     * 
     * @param columnNameList
     * @param rowList list of row which can be: Map/Entity/Array/Collection.
     * @return
     */
    @SuppressWarnings("deprecation")
    public static <T> DataSet newDataSet(List<String> columnNameList, List<T> rowList) {
        if (N.isNullOrEmpty(columnNameList) && N.isNullOrEmpty(rowList)) {
            // throw new IllegalArgumentException("Column name list and row list can't be both null or empty");

            return new RowDataSet(new ArrayList<String>(0), new ArrayList<List<Object>>(0));
        }

        final int rowSize = rowList.size();

        if (N.isNullOrEmpty(columnNameList)) {
            final Set<String> columnNameSet = new LinkedHashSet<>();
            Set<Class<?>> clsSet = null;
            Map<Class<?>, Set<String>> clsSignedPropNameSetMap = new HashMap<>();

            Class<?> cls = null;
            Type<?> type = null;

            for (Object e : rowList) {
                if (e == null) {
                    continue;
                }

                cls = e.getClass();
                type = N.typeOf(cls);

                if (type.isMap()) {
                    columnNameSet.addAll(((Map<String, Object>) e).keySet());
                } else if (type.isEntity()) {
                    if (e instanceof DirtyMarker) {
                        Set<String> clsSignedPropNameSet = clsSignedPropNameSetMap.get(cls);

                        if (clsSignedPropNameSet == null) {
                            clsSignedPropNameSet = new HashSet<>();
                            clsSignedPropNameSetMap.put(cls, clsSignedPropNameSet);
                        }

                        Method method = null;

                        for (String signedPropName : ((DirtyMarker) e).signedPropNames()) {
                            if (clsSignedPropNameSet.add(signedPropName) == false) {
                                continue;
                            }

                            method = ClassUtil.getPropGetMethod(cls, signedPropName);

                            if (method != null) {
                                columnNameSet.add(ClassUtil.getPropNameByMethod(method));
                            }
                        }
                    } else {
                        if (clsSet == null) {
                            clsSet = new HashSet<>();
                        }

                        if (clsSet.contains(cls)) {
                            continue;
                        }

                        columnNameSet.addAll(ClassUtil.checkPropGetMethodList(cls).keySet());
                        clsSet.add(cls);
                    }
                } else {
                    throw new IllegalArgumentException("'columnNameList' is required if the sepcified row type is not Entity or Map");
                }
            }

            // re-order column.
            for (Map.Entry<Class<?>, Set<String>> entry : clsSignedPropNameSetMap.entrySet()) {
                final List<String> intersecion = N.intersection(ClassUtil.getPropGetMethodList(entry.getKey()).keySet(), columnNameSet);
                columnNameSet.removeAll(intersecion);
                columnNameSet.addAll(intersecion);
            }

            columnNameList = new ArrayList<>(columnNameSet);

            if (N.isNullOrEmpty(columnNameList)) {
                throw new IllegalArgumentException("Column name list can't be obtained from row list because it's empty or null");
            }
        }

        final int columnCount = columnNameList.size();
        final List<List<Object>> columnList = new ArrayList<>(columnCount);
        for (int i = 0; i < columnCount; i++) {
            columnList.add(new ArrayList<>(rowSize));
        }

        Type<?> type = null;
        for (Object e : rowList) {
            if (e == null) {
                for (int i = 0; i < columnCount; i++) {
                    columnList.get(i).add(null);
                }

                continue;
            }

            type = N.typeOf(e.getClass());

            if (type.isMap()) {
                Map<String, Object> props = (Map<String, Object>) e;

                for (int i = 0; i < columnCount; i++) {
                    columnList.get(i).add(props.get(columnNameList.get(i)));
                }
            } else if (type.isEntity()) {
                Class<?> cls = e.getClass();

                Method method = null;
                for (int i = 0; i < columnCount; i++) {
                    method = ClassUtil.getPropGetMethod(cls, columnNameList.get(i));

                    if (method == null) {
                        columnList.get(i).add(null);
                    } else {
                        columnList.get(i).add(ClassUtil.getPropValue(e, method));
                    }
                }
            } else if (type.isArray()) {
                if (type.isPrimitiveArray()) {
                    for (int i = 0; i < columnCount; i++) {
                        columnList.get(i).add(Array.get(e, i));
                    }
                } else {
                    Object[] array = (Object[]) e;

                    for (int i = 0; i < columnCount; i++) {
                        columnList.get(i).add(array[i]);
                    }
                }
            } else if (type.isCollection()) {
                final Iterator<Object> it = ((Collection<Object>) e).iterator();

                for (int i = 0; i < columnCount; i++) {
                    columnList.get(i).add(it.next());
                }
            } else {
                throw new IllegalArgumentException(
                        "Unsupported row type: " + ClassUtil.getCanonicalClassName(e.getClass()) + ". Only array, collection, map and entity are supported");
            }
        }

        return new RowDataSet(columnNameList, columnList);
    }

    @SuppressWarnings("deprecation")
    static MapEntity asMapEntity(final String entityName, final Object... props) {
        final MapEntity mapEntity = new MapEntity(entityName);

        if (N.isNullOrEmpty(props)) {
            return mapEntity;
        }

        if (props.length == 1) {
            if (props[0] instanceof Map) {
                Map<String, Object> map = (Map<String, Object>) props[0];
                for (String key : map.keySet()) {
                    mapEntity.set(key, map.get(key));
                }
            } else if (N.isEntity(props[0].getClass())) {
                Object anEntity = props[0];
                if (anEntity instanceof DirtyMarker) {
                    Class<?> entityClass = anEntity.getClass();
                    Method propGetMethod = null;
                    for (String propName : ((DirtyMarker) anEntity).signedPropNames()) {
                        propGetMethod = ClassUtil.getPropGetMethod(entityClass, propName);
                        propName = ClassUtil.getPropNameByMethod(propGetMethod);
                        mapEntity.set(propName, ClassUtil.getPropValue(anEntity, propGetMethod));
                    }
                } else {
                    final Map<String, Method> getterMethodList = ClassUtil.checkPropGetMethodList(anEntity.getClass());

                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        mapEntity.set(entry.getKey(), ClassUtil.getPropValue(anEntity, entry.getValue()));
                    }
                }
            } else {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }
        } else {
            if ((props.length % 2) != 0) {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }

            for (int i = 0; i < props.length; i++) {
                mapEntity.set((String) props[i], props[++i]);
            }
        }

        return mapEntity;
    }

    //    /**
    //     * Wrap the specified map with a unmodifiable BiMap.
    //     *
    //     * Note: To avoid the side-effects in the initialization of auto-generated interface *PNL/*CNL class,
    //     * This method just return null if the specified keyMap has duplicated values, instead of throwing exception.
    //     *
    //     * @param keyMap
    //     * @return null if the specified <code>keyMap</code> has duplicated values
    //     */
    //    @Internal
    //    public static <K, V> BiMap<K, V> newImmutableBiMapForInterface(final Map<? extends K, ? extends V> keyMap) {
    //        final Map<V, K> valueMap = new LinkedHashMap<>();
    //
    //        for (Map.Entry<? extends K, ? extends V> entry : keyMap.entrySet()) {
    //            valueMap.put(entry.getValue(), entry.getKey());
    //        }
    //
    //        if (valueMap.size() != keyMap.size()) {
    //            return null;
    //        }
    //
    //        return new BiMap<>(ImmutableMap.of(keyMap), ImmutableMap.of(valueMap));
    //    }

    /**
     * The input array is returned.
     *
     * @param a
     * @return
     */
    @SafeVarargs
    public static <T> T[] asArray(final T... a) {
        return a;
    }

    /**
     * Returns an empty array if the specified collection is null or empty.
     * 
     * @param c
     * @return
     */
    @SuppressWarnings("unchecked")
    public static Object[] toArray(final Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return N.EMPTY_OBJECT_ARRAY;
        }

        return c.toArray(new Object[c.size()]);
    }

    public static <T> T[] toArray(final Collection<? extends T> c, final T[] a) {
        if (N.isNullOrEmpty(c)) {
            return a;
        }

        return c.toArray(a);
    }

    public static <T> T[] toArray(final Class<T[]> targetClass, final Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return N.newArray(targetClass.getComponentType(), 0);
        }

        return c.toArray((T[]) N.newArray(targetClass.getComponentType(), c.size()));
    }

    /**
     *
     * @param a
     *            pairs of property name and value or a Java Entity Object what
     *            allows access to properties using getter and setter methods.
     * @return
     */
    @SafeVarargs
    public static Map<String, Object> asOptions(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new HashMap<>();
        }

        return newMap(new HashMap<String, Object>(initHashCapacity(a.length / 2)), a);
    }

    /**
     *
     * @param a
     *            pairs of property name and value or a Java Entity Object what
     *            allows access to properties using getter and setter methods.
     * @return
     */
    @SafeVarargs
    public static Map<String, Object> asProps(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new LinkedHashMap<>();
        }

        return newMap(new LinkedHashMap<String, Object>(initHashCapacity(a.length / 2)), a);
    }

    @SuppressWarnings("unchecked")
    static <K, V, T extends Map<K, V>> T newMap(final T m, final Object... a) {
        if (isNullOrEmpty(a)) {
            return m;
        }

        if (a.length == 1) {
            if (a[0] instanceof Map) {
                m.putAll((Map<K, V>) a[0]);
            } else if (N.isEntity(a[0].getClass())) {
                Maps.entity2Map((Map<String, Object>) m, a[0]);
            } else {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }
        } else {
            if (0 != (a.length % 2)) {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }

            for (int i = 0; i < a.length; i++) {
                m.put((K) a[i], (V) a[++i]);
            }
        }

        return m;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        return map;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1, final k k2, final v v2) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        return map;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        return map;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        return map;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        return map;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5, final k k6, final v v6) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        return map;
    }

    public static <K, V, k extends K, v extends V> Map<K, V> asMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        final Map<K, V> map = new HashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        map.put(k7, v7);
        return map;
    }

    @SafeVarargs
    public static <K, V> Map<K, V> asMap(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new HashMap<>();
        }

        return newMap(new HashMap<K, V>(initHashCapacity(a.length / 2)), a);
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        return map;
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1, final k k2, final v v2) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        return map;
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        return map;
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        return map;
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4, final k k5, final v v5) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        return map;
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4, final k k5, final v v5, final k k6, final v v6) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        return map;
    }

    public static <K, V, k extends K, v extends V> LinkedHashMap<K, V> asLinkedHashMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4, final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        final LinkedHashMap<K, V> map = new LinkedHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        map.put(k7, v7);
        return map;
    }

    @SafeVarargs
    public static <K, V> LinkedHashMap<K, V> asLinkedHashMap(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new LinkedHashMap<>();
        }

        return newMap(new LinkedHashMap<K, V>(initHashCapacity(a.length / 2)), a);
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        return map;
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1, final k k2, final v v2) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        return map;
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1, final k k2, final v v2, final k k3,
            final v v3) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        return map;
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1, final k k2, final v v2, final k k3,
            final v v3, final k k4, final v v4) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        return map;
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1, final k k2, final v v2, final k k3,
            final v v3, final k k4, final v v4, final k k5, final v v5) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        return map;
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1, final k k2, final v v2, final k k3,
            final v v3, final k k4, final v v4, final k k5, final v v5, final k k6, final v v6) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        return map;
    }

    public static <K, V, k extends K, v extends V> ConcurrentHashMap<K, V> asConcurrentHashMap(final k k1, final v v1, final k k2, final v v2, final k k3,
            final v v3, final k k4, final v v4, final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        final ConcurrentHashMap<K, V> map = new ConcurrentHashMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        map.put(k7, v7);
        return map;
    }

    @SafeVarargs
    public static <K, V> ConcurrentHashMap<K, V> asConcurrentHashMap(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new ConcurrentHashMap<>();
        }

        return newMap(new ConcurrentHashMap<K, V>(initHashCapacity(a.length / 2)), a);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        return map;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1, final k k2, final v v2) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        return map;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        return map;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        return map;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        return map;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5, final k k6, final v v6) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        return map;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> asBiMap(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        final BiMap<K, V> map = new BiMap<>();
        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        map.put(k7, v7);
        return map;
    }

    @SafeVarargs
    public static <K, V> BiMap<K, V> asBiMap(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new BiMap<>();
        }

        return newMap(new BiMap<K, V>(initHashCapacity(a.length / 2)), a);
    }

    /**
     * @param a
     * @return
     */
    @SafeVarargs
    public static <T> List<T> asList(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final List<T> list = new ArrayList<>(a.length);

        if (a.length < 9) {
            for (T e : a) {
                list.add(e);
            }
        } else {
            list.addAll(Arrays.asList(a));
        }

        return list;
    }

    @SafeVarargs
    public static <T> LinkedList<T> asLinkedList(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new LinkedList<>();
        }

        final LinkedList<T> list = new LinkedList<>();

        for (T e : a) {
            list.add(e);
        }

        return list;
    }

    @SafeVarargs
    public static <T> Set<T> asSet(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new HashSet<>();
        }

        final Set<T> set = new HashSet<>(initHashCapacity(a.length));

        for (T e : a) {
            set.add(e);
        }

        return set;
    }

    @SafeVarargs
    public static <T> LinkedHashSet<T> asLinkedHashSet(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new LinkedHashSet<>();
        }

        final LinkedHashSet<T> set = new LinkedHashSet<>(initHashCapacity(a.length));

        for (T e : a) {
            set.add(e);
        }

        return set;
    }

    @SafeVarargs
    public static <T> SortedSet<T> asSortedSet(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new TreeSet<>();
        }

        final SortedSet<T> set = new TreeSet<>();

        for (T e : a) {
            set.add(e);
        }

        return set;
    }

    @SafeVarargs
    public static <T> Queue<T> asQueue(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayDeque<>();
        }

        final Queue<T> queue = new ArrayDeque<>(a.length);

        for (T e : a) {
            queue.add(e);
        }

        return queue;
    }

    @SafeVarargs
    public static <T> ArrayBlockingQueue<T> asArrayBlockingQueue(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayBlockingQueue<>(0);
        }

        final ArrayBlockingQueue<T> queue = new ArrayBlockingQueue<>(a.length);

        for (T e : a) {
            queue.add(e);
        }

        return queue;
    }

    @SafeVarargs
    public static <T> LinkedBlockingQueue<T> asLinkedBlockingQueue(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new LinkedBlockingQueue<>();
        }

        final LinkedBlockingQueue<T> queue = new LinkedBlockingQueue<>(a.length);

        for (T e : a) {
            queue.add(e);
        }

        return queue;
    }

    @SafeVarargs
    public static <T> ConcurrentLinkedQueue<T> asConcurrentLinkedQueue(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ConcurrentLinkedQueue<>();
        }

        final ConcurrentLinkedQueue<T> queue = new ConcurrentLinkedQueue<>();

        for (T e : a) {
            queue.add(e);
        }

        return queue;
    }

    @SafeVarargs
    public static <T extends Delayed> DelayQueue<T> asDelayQueue(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new DelayQueue<>();
        }

        final DelayQueue<T> queue = new DelayQueue<>();

        for (T e : a) {
            queue.add(e);
        }

        return queue;
    }

    @SafeVarargs
    public static <T> PriorityQueue<T> asPriorityQueue(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new PriorityQueue<>();
        }

        final PriorityQueue<T> queue = new PriorityQueue<>(a.length);

        for (T e : a) {
            queue.add(e);
        }

        return queue;
    }

    @SafeVarargs
    public static <T> Deque<T> asDeque(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayDeque<>();
        }

        final Deque<T> deque = new ArrayDeque<>(a.length);

        for (T e : a) {
            deque.add(e);
        }

        return deque;
    }

    @SafeVarargs
    public static <T> LinkedBlockingDeque<T> asLinkedBlockingDeque(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new LinkedBlockingDeque<>();
        }

        final LinkedBlockingDeque<T> deque = new LinkedBlockingDeque<>(a.length);

        for (T e : a) {
            deque.add(e);
        }

        return deque;
    }

    @SafeVarargs
    public static <T> ConcurrentLinkedDeque<T> asConcurrentLinkedDeque(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ConcurrentLinkedDeque<>();
        }

        final ConcurrentLinkedDeque<T> deque = new ConcurrentLinkedDeque<>();

        for (T e : a) {
            deque.add(e);
        }

        return deque;
    }

    @SafeVarargs
    public static <T> Multiset<T> asMultiset(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new Multiset<>();
        }

        final Multiset<T> multiset = new Multiset<>(new HashMap<T, MutableInt>(initHashCapacity(a.length)));

        for (T e : a) {
            multiset.add(e);
        }

        return multiset;
    }

    /**
     * Wrap the specified value with a singleton list.
     *
     * @param o
     * @return
     * @see java.util.Collections#singletonList(Object)
     */
    public static <T> List<T> asSingletonList(final T o) {
        return Collections.singletonList(o);
    }

    /**
     * Wrap the specified value with a singleton set.
     *
     * @param o
     * @return
     * @see java.util.Collections#singleton(Object)
     */
    public static <T> Set<T> asSingletonSet(final T o) {
        return Collections.singleton(o);
    }

    /**
     * Wrap the specified key/value with a singleton map.
     *
     * @param key
     * @param value
     * @return
     * @see java.util.Collections#singletonMap(Object, Object)
     */
    public static <K, V> Map<K, V> asSingletonMap(final K key, final V value) {
        return Collections.singletonMap(key, value);
    }

    /**
     * Try to convert the specified {@code sourceObject} to the specified
     * {@code cls}. Default value of {@code cls} is returned if
     * {@code sourceObject} is null. An instance of {@code cls} is returned if
     * convert successfully
     *
     * @param targetClass
     * @param obj
     * @return
     * @throws ClassCastException
     */
    @SuppressWarnings("unchecked")
    public static <T> T as(final Class<T> targetClass, final Object obj) {
        //        if (obj == null) {
        //            return defaultValueOf(targetClass);
        //        }
        //
        //        final Class<?> srcPropClass = obj.getClass();
        //
        //        if (targetClass.isAssignableFrom(srcPropClass)) {
        //            return (T) obj;
        //        }
        //
        //        final Type<Object> targetPropType = targetClass.isEnum()
        //                ? getType(getCanonicalClassName(targetClass) + "(" + Integer.class.isAssignableFrom(srcPropClass) + ")") : N.getType(targetClass);
        //        final Type<Object> srcPropType = getType(srcPropClass);
        //
        //        if (targetPropType.isBoolean() && srcPropType.isNumber()) {
        //            return (T) ((Boolean) (((Number) obj).longValue() > 0));
        //        }
        //
        //        if (targetPropType.isEntity() && srcPropType.isMap()) {
        //            return map2Entity(targetClass, (Map<String, Object>) obj);
        //        } else if (targetPropType.isMap() && srcPropType.isEntity()) {
        //            try {
        //                return (T) entity2Map((Map<String, Object>) N.newInstance(targetClass), obj);
        //            } catch (Exception e) {
        //                // ignore.
        //            }
        //        } else if (targetPropType.isEntity() && srcPropType.isEntity()) {
        //            return copy(targetClass, obj);
        //        }
        //
        //        return (T) targetPropType.valueOf(srcPropType.stringOf(obj));

        final Type<T> type = typeOf(targetClass);
        return as(type, obj);
    }

    @SuppressWarnings("unchecked")
    public static <T> T as(final Type<T> targetType, final Object obj) {
        if (obj == null) {
            return targetType.defaultValue();
        }

        final Class<?> srcPropClass = obj.getClass();

        if (targetType.getTypeClass().isAssignableFrom(srcPropClass)) {
            return (T) obj;
        }

        final Type<Object> srcPropType = typeOf(srcPropClass);

        if (targetType.isBoolean() && srcPropType.isNumber()) {
            return (T) ((Boolean) (((Number) obj).longValue() > 0));
        }

        if (targetType.isEntity() && srcPropType.isMap()) {
            return Maps.map2Entity(targetType.getTypeClass(), (Map<String, Object>) obj);
        } else if (targetType.isMap() && srcPropType.isEntity()) {
            try {
                return (T) Maps.entity2Map((Map<String, Object>) N.newInstance(targetType.getTypeClass()), obj);
            } catch (Exception e) {
                // ignore.
            }
        } else if (targetType.isEntity() && srcPropType.isEntity()) {
            return copy(targetType.getTypeClass(), obj);
        } else if (targetType.isNumber() && srcPropType.isNumber() && CLASS_TYPE_ENUM.containsKey(targetType.getTypeClass())) {
            switch (CLASS_TYPE_ENUM.get(targetType.getTypeClass())) {
                case 3:
                case 13:
                    return (T) (Byte) ((Number) obj).byteValue();

                case 4:
                case 14:
                    return (T) (Short) ((Number) obj).shortValue();

                case 5:
                case 15:
                    return (T) (Integer) ((Number) obj).intValue();

                case 6:
                case 16:
                    return (T) (Long) ((Number) obj).longValue();

                case 7:
                case 17:
                    return (T) (Float) ((Number) obj).floatValue();

                case 8:
                case 18:
                    return (T) (Double) ((Number) obj).doubleValue();

            }
        }

        return targetType.valueOf(srcPropType.stringOf(obj));
    }

    /**
     * Returns a <code>Boolean</code> with a value represented by the specified
     * string. The <code>Boolean</code> returned represents a true value if the
     * string argument is not <code>null</code> and is equal, ignoring case, to
     * the string {@code "true"}.
     *
     * @param str
     *            a string.
     * @return the <code>Boolean</code> value represented by the string.
     */
    public static boolean asBoolean(final String str) {
        return isNullOrEmpty(str) ? false : Boolean.valueOf(str);
    }

    public static boolean asBoolean(final Boolean b) {
        return b == null ? false : b.booleanValue();
    }

    public static char asChar(final String str) {
        return isNullOrEmpty(str) ? CHAR_0 : ((str.length() == 1) ? str.charAt(0) : (char) Integer.parseInt(str));
    }

    public static char asChar(final Character c) {
        return c == null ? CHAR_0 : c.charValue();
    }

    /**
     * Returns the value by calling {@code Byte.valueOf(String)} if {@code str}
     * is not {@code null}, otherwise, the default value 0 for {@code byte} is
     * returned.
     *
     * @param str
     * @return
     */
    public static byte asByte(final String str) {
        if (N.isNullOrEmpty(str)) {
            return 0;
        }

        if (str.length() < 5) {
            Integer result = stringIntCache.get(str);
            if (result != null) {
                if (result.intValue() < Byte.MIN_VALUE || result.intValue() > Byte.MAX_VALUE) {
                    throw new NumberFormatException("Value out of range. Value:\"" + str + "\" Radix: 10");
                }

                return result.byteValue();
            }
        }

        return Byte.parseByte(str);
    }

    public static byte asByte(final Number num) {
        return num == null ? 0 : num.byteValue();
    }

    /**
     * Returns the value by calling {@code Short.valueOf(String)} if {@code str}
     * is not {@code null}, otherwise, the default value 0 for {@code short} is
     * returned.
     *
     * @param str
     * @return
     */
    public static short asShort(final String str) {
        if (N.isNullOrEmpty(str)) {
            return 0;
        }

        if (str.length() < 5) {
            Integer result = stringIntCache.get(str);
            if (result != null) {
                return result.shortValue();
            }
        }

        return Short.parseShort(str);
    }

    public static short asShort(final Number num) {
        return num == null ? 0 : num.shortValue();
    }

    /**
     * Returns the value by calling {@code Integer.valueOf(String)} if
     * {@code str} is not {@code null}, otherwise, the default value 0 for
     * {@code int} is returned.
     *
     * @param str
     * @return
     */
    public static int asInt(final String str) {
        if (N.isNullOrEmpty(str)) {
            return 0;
        }

        if (str.length() < 5) {
            Integer result = stringIntCache.get(str);
            if (result != null) {
                return result.intValue();
            }
        }

        return IOUtil.parseInt(N.getCharsForReadOnly(str), 0, str.length());
    }

    public static int asInt(final Number num) {
        return num == null ? 0 : num.intValue();
    }

    /**
     * Returns the value by calling {@code Long.valueOf(String)} if {@code str}
     * is not {@code null}, otherwise, the default value 0 for {@code long} is
     * returned.
     *
     * @param str
     * @return
     */
    public static long asLong(final String str) {
        if (N.isNullOrEmpty(str)) {
            return 0;
        }

        if (str.length() < 5) {
            Integer result = stringIntCache.get(str);
            if (result != null) {
                return result.intValue();
            }
        }

        return IOUtil.parseLong(N.getCharsForReadOnly(str), 0, str.length());
    }

    public static long asLong(final Number num) {
        return num == null ? 0l : num.longValue();
    }

    /**
     * Returns the value by calling {@code Float.valueOf(String)} if {@code str}
     * is not {@code null}, otherwise, the default value 0f for {@code float} is
     * returned.
     *
     * @param str
     * @return
     */
    public static float asFloat(final String str) {
        if (isNullOrEmpty(str)) {
            return 0f;
        }

        return Float.parseFloat(str);
    }

    public static float asFloat(final Number num) {
        return num == null ? 0f : num.floatValue();
    }

    /**
     * Returns the value by calling {@code Double.valueOf(String)} if {@code str}
     * is not {@code null}, otherwise, the default value 0d for {@code double} is
     * returned.
     *
     * @param str
     * @return
     */
    public static double asDouble(final String str) {
        return isNullOrEmpty(str) ? 0d : Double.parseDouble(str);
    }

    public static double asDouble(final Number num) {
        return num == null ? 0d : num.doubleValue();
    }

    public static java.util.Date asJUDate(final Calendar c) {
        return (c == null) ? null : asJUDate(c.getTimeInMillis());
    }

    public static java.util.Date asJUDate(final java.util.Date date) {
        return (date == null) ? null : asJUDate(date.getTime());
    }

    public static java.util.Date asJUDate(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new java.util.Date(timeInMillis);
    }

    public static java.util.Date asJUDate(final String date) {
        return asJUDate(date, null);
    }

    public static java.util.Date asJUDate(final String date, final String format) {
        return asJUDate(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of java.util.Date.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     *
     * @param date
     * @param format
     * @throws IllegalArgumentException
     *             if the date given can't be parsed with specified format.
     */
    public static java.util.Date asJUDate(final String date, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return asJUDate(parse(date, format, timeZone));
    }

    private static long parse(final String date, String format, TimeZone timeZone) {
        if ((format == null) && (date.charAt(2) >= '0' && date.charAt(2) <= '9' && date.charAt(4) >= '0' && date.charAt(4) <= '9')) {
            try {
                return Long.parseLong(date);
            } catch (NumberFormatException e) {
                // ignore.
            }
        }

        format = checkDateFormat(date, format);

        if (N.isNullOrEmpty(format)) {
            if (timeZone == null) {
                return ISO8601Util.parse(date).getTime();
            } else {
                throw new RuntimeException("Unsupported date format: " + format + " with time zone: " + timeZone);
            }
        }

        timeZone = checkTimeZone(format, timeZone);

        long timeInMillis = fastDateParse(date, format, timeZone);

        if (timeInMillis > 0) {
            return timeInMillis;
        }

        DateFormat sdf = getSDF(format, timeZone);

        try {
            return sdf.parse(date).getTime();
        } catch (ParseException e) {
            throw new IllegalArgumentException(e);
        } finally {
            recycleSDF(format, timeZone, sdf);
        }
    }

    private static DateFormat getSDF(final String format, final TimeZone timeZone) {
        DateFormat sdf = null;

        if (timeZone == UTC_TIME_ZONE) {
            if ((format.length() == 28) && (format == ISO_8601_TIMESTAMP_FORMAT)) {
                sdf = utcTimestampDFPool.poll();

                if (sdf == null) {
                    sdf = new SimpleDateFormat(format);
                    sdf.setTimeZone(timeZone);
                }

                return sdf;
            } else if ((format.length() == 24) && (format == ISO_8601_DATETIME_FORMAT)) {
                sdf = utcDateTimeDFPool.poll();

                if (sdf == null) {
                    sdf = new SimpleDateFormat(format);
                    sdf.setTimeZone(timeZone);
                }

                return sdf;
            }
        }

        Queue<DateFormat> queue = dfPool.get(format);

        if (queue == null) {
            queue = new ArrayBlockingQueue<>(POOL_SIZE);
            dfPool.put(format, queue);
        }

        sdf = queue.poll();

        if (sdf == null) {
            sdf = new SimpleDateFormat(format);
        }

        sdf.setTimeZone(timeZone);

        return sdf;
    }

    private static void recycleSDF(final String format, final TimeZone timeZone, final DateFormat sdf) {
        if (timeZone == UTC_TIME_ZONE) {
            if ((format.length() == 28) && (format == ISO_8601_TIMESTAMP_FORMAT)) {
                utcTimestampDFPool.add(sdf);
            } else if ((format.length() == 24) && (format == ISO_8601_DATETIME_FORMAT)) {
                utcDateTimeDFPool.add(sdf);
            } else {
                dfPool.get(format).add(sdf);
            }
        } else {
            dfPool.get(format).add(sdf);
        }
    }

    private static String checkDateFormat(final String str, final String format) {
        if (N.isNullOrEmpty(format)) {
            int len = str.length();

            switch (len) {
                case 4:
                    return LOCAL_YEAR_FORMAT;

                case 5:
                    if (str.charAt(2) == '/') {
                        return LOCAL_MONTH_DAY_FORMAT_SLASH;
                    } else {
                        return LOCAL_MONTH_DAY_FORMAT;
                    }

                case 8:
                    return LOCAL_TIME_FORMAT;

                case 10:
                    if (str.charAt(4) == '/') {
                        return LOCAL_DATE_FORMAT_SLASH;
                    } else {
                        return LOCAL_DATE_FORMAT;
                    }

                case 19:
                    if (str.charAt(4) == '/') {
                        return LOCAL_DATETIME_FORMAT_SLASH;
                    } else {
                        return LOCAL_DATETIME_FORMAT;
                    }

                case 23:
                    if (str.charAt(4) == '/') {
                        return LOCAL_TIMESTAMP_FORMAT_SLASH;
                    } else {
                        return LOCAL_TIMESTAMP_FORMAT;
                    }

                case 24:
                    if (str.charAt(4) == '/') {
                        return ISO_8601_DATETIME_FORMAT_SLASH;
                    } else {
                        return ISO_8601_DATETIME_FORMAT;
                    }

                case 28:
                    if (str.charAt(4) == '/') {
                        return ISO_8601_TIMESTAMP_FORMAT_SLASH;
                    } else {
                        return ISO_8601_TIMESTAMP_FORMAT;
                    }

                case 29:
                    if (str.charAt(3) == ',') {
                        return RFC1123_DATE_FORMAT;
                    }

                default:
                    // throw new AbacusException("No valid date format found for: " + str);
                    return null;
            }
        }

        return format;
    }

    private static TimeZone checkTimeZone(final String format, TimeZone timeZone) {
        if (timeZone == null) {
            timeZone = format.endsWith("'Z'") ? UTC_TIME_ZONE : LOCAL_TIME_ZONE;
        }

        return timeZone;
    }

    private static long fastDateParse(final String str, final String format, final TimeZone timeZone) {
        if (!((str.length() == 24) || (str.length() == 20) || (str.length() == 19) || (str.length() == 23))) {
            return 0;
        }

        if (!(format.equals(ISO_8601_TIMESTAMP_FORMAT) || format.equals(ISO_8601_DATETIME_FORMAT) || format.equals(LOCAL_DATETIME_FORMAT)
                || format.equals(LOCAL_TIMESTAMP_FORMAT))) {
            return 0;
        }

        //
        // if (!((str.charAt(4) == '-') && (str.charAt(7) == '-') &&
        // (str.charAt(10) == 'T') && (str.charAt(13) == ':') && (str.charAt(16)
        // == ':') && (str
        // .charAt(str.length() - 1) == 'Z'))) {
        // return 0;
        // }
        //
        // int year = Integer.valueOf(str.substring(0, 4));
        // int month = Integer.valueOf(str.substring(5, 7)) - 1;
        // int date = Integer.valueOf(str.substring(8, 10));
        // int hourOfDay = Integer.valueOf(str.substring(11, 13));
        // int minute = Integer.valueOf(str.substring(14, 16));
        // int second = Integer.valueOf(str.substring(17, 19));
        // int milliSecond = (str.length() == 24) ?
        // Integer.valueOf(str.substring(20, 23)) : 0;
        //
        //

        int year = parseInt(str, 0, 4);
        int month = parseInt(str, 5, 7) - 1;
        int date = parseInt(str, 8, 10);
        int hourOfDay = parseInt(str, 11, 13);
        int minute = parseInt(str, 14, 16);
        int second = parseInt(str, 17, 19);
        int milliSecond = ((str.length() == 24) || (str.length() == 23)) ? parseInt(str, 20, 23) : 0;

        Calendar c = null;
        Queue<Calendar> timeZoneCalendarQueue = null;

        if (timeZone == UTC_TIME_ZONE) {
            c = utcCalendarPool.poll();
        } else {
            timeZoneCalendarQueue = calendarPool.get(timeZone);

            if (timeZoneCalendarQueue == null) {
                timeZoneCalendarQueue = new ArrayBlockingQueue<>(POOL_SIZE);
                calendarPool.put(timeZone, timeZoneCalendarQueue);
            } else {
                c = timeZoneCalendarQueue.poll();
            }
        }

        if (c == null) {
            c = Calendar.getInstance(timeZone);
        }

        c.set(year, month, date, hourOfDay, minute, second);
        c.set(Calendar.MILLISECOND, milliSecond);

        long timeInMillis = c.getTimeInMillis();

        if (timeZone == UTC_TIME_ZONE) {
            utcCalendarPool.add(c);
        } else {
            timeZoneCalendarQueue.add(c);
        }

        return timeInMillis;
    }

    private static int parseInt(final String str, int from, final int to) {
        int result = 0;

        while (from < to) {
            result = (result * 10) + (str.charAt(from++) - 48);
        }

        return result;
    }

    public static Date asDate(final Calendar c) {
        return (c == null) ? null : asDate(c.getTimeInMillis());
    }

    public static Date asDate(final java.util.Date date) {
        return (date == null) ? null : asDate(date.getTime());
    }

    public static Date asDate(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new Date(timeInMillis);
    }

    public static Date asDate(final String date) {
        return asDate(date, null);
    }

    public static Date asDate(final String date, final String format) {
        return asDate(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of java.sql.Date.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param date
     * @param format
     * @param timeZone
     * @return
     */
    public static Date asDate(final String date, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return asDate(parse(date, format, timeZone));
    }

    public static Time asTime(final Calendar c) {
        return (c == null) ? null : asTime(c.getTimeInMillis());
    }

    public static Time asTime(final java.util.Date date) {
        return (date == null) ? null : asTime(date.getTime());
    }

    public static Time asTime(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new Time(timeInMillis);
    }

    public static Time asTime(final String date) {
        return asTime(date, null);
    }

    public static Time asTime(final String date, final String format) {
        return asTime(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of Time.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param date
     * @param format
     * @param timeZone
     * @return
     */
    public static Time asTime(final String date, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return asTime(parse(date, format, timeZone));
    }

    public static Timestamp asTimestamp(final Calendar c) {
        return (c == null) ? null : asTimestamp(c.getTimeInMillis());
    }

    public static Timestamp asTimestamp(final java.util.Date date) {
        return (date == null) ? null : asTimestamp(date.getTime());
    }

    public static Timestamp asTimestamp(final long timeInMillis) {
        return (timeInMillis == 0) ? null : new Timestamp(timeInMillis);
    }

    public static Timestamp asTimestamp(final String date) {
        return asTimestamp(date, null);
    }

    public static Timestamp asTimestamp(final String date, final String format) {
        return asTimestamp(date, format, null);
    }

    /**
     * Converts the specified <code>date</code> with the specified {@code format} to a new instance of Timestamp.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param date
     * @param format
     * @param timeZone
     * @return
     */
    public static Timestamp asTimestamp(final String date, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(date) || (date.length() == 4 && "null".equalsIgnoreCase(date))) {
            return null;
        }

        return asTimestamp(parse(date, format, timeZone));
    }

    public static Calendar asCalendar(final Calendar c) {
        return (c == null) ? null : asCalendar(c.getTimeInMillis());
    }

    public static Calendar asCalendar(final java.util.Date date) {
        return (date == null) ? null : asCalendar(date.getTime());
    }

    public static Calendar asCalendar(final long timeInMillis) {
        if (timeInMillis == 0) {
            return null;
        }

        final Calendar c = Calendar.getInstance();

        c.setTimeInMillis(timeInMillis);

        return c;
    }

    public static Calendar asCalendar(final String calendar) {
        return asCalendar(calendar, null);
    }

    public static Calendar asCalendar(final String calendar, final String format) {
        return asCalendar(calendar, format, null);
    }

    /**
     * Converts the specified <code>calendar</code> with the specified {@code format} to a new instance of Calendar.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param calendar
     * @param format
     * @param timeZone
     * @return
     */
    public static Calendar asCalendar(final String calendar, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(calendar) || (calendar.length() == 4 && "null".equalsIgnoreCase(calendar))) {
            return null;
        }

        return asCalendar(parse(calendar, format, timeZone));
    }

    public static GregorianCalendar asGregorianCalendar(final Calendar c) {
        return (c == null) ? null : asGregorianCalendar(c.getTimeInMillis());
    }

    public static GregorianCalendar asGregorianCalendar(final java.util.Date date) {
        return (date == null) ? null : asGregorianCalendar(date.getTime());
    }

    public static GregorianCalendar asGregorianCalendar(final long timeInMillis) {
        if (timeInMillis == 0) {
            return null;
        }

        final GregorianCalendar c = new GregorianCalendar();

        c.setTimeInMillis(timeInMillis);

        return c;
    }

    public static GregorianCalendar asGregorianCalendar(final String calendar) {
        return asGregorianCalendar(calendar, null);
    }

    public static GregorianCalendar asGregorianCalendar(final String calendar, final String format) {
        return asGregorianCalendar(calendar, format, null);
    }

    /**
     * Converts the specified <code>calendar</code> with the specified {@code format} to a new instance of GregorianCalendar.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param calendar
     * @param format
     * @param timeZone
     * @return
     */
    public static GregorianCalendar asGregorianCalendar(final String calendar, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(calendar) || (calendar.length() == 4 && "null".equalsIgnoreCase(calendar))) {
            return null;
        }

        return asGregorianCalendar(parse(calendar, format, timeZone));
    }

    public static XMLGregorianCalendar asXMLGregorianCalendar(final Calendar c) {
        return (c == null) ? null : asXMLGregorianCalendar(c.getTimeInMillis());
    }

    public static XMLGregorianCalendar asXMLGregorianCalendar(final java.util.Date date) {
        return (date == null) ? null : asXMLGregorianCalendar(date.getTime());
    }

    public static XMLGregorianCalendar asXMLGregorianCalendar(final long timeInMillis) {
        if (timeInMillis == 0) {
            return null;
        }

        return dataTypeFactory.newXMLGregorianCalendar(asGregorianCalendar(timeInMillis));
    }

    public static XMLGregorianCalendar asXMLGregorianCalendar(final String calendar) {
        return asXMLGregorianCalendar(calendar, null);
    }

    public static XMLGregorianCalendar asXMLGregorianCalendar(final String calendar, final String format) {
        return asXMLGregorianCalendar(calendar, format, null);
    }

    /**
     * Converts the specified <code>calendar</code> with the specified {@code format} to a new instance of XMLGregorianCalendar.
     * <code>null</code> is returned if the specified <code>date</code> is null or empty.
     * 
     * @param calendar
     * @param format
     * @param timeZone
     * @return
     */
    public static XMLGregorianCalendar asXMLGregorianCalendar(final String calendar, final String format, final TimeZone timeZone) {
        if (isNullOrEmpty(calendar) || (calendar.length() == 4 && "null".equalsIgnoreCase(calendar))) {
            return null;
        }

        return asXMLGregorianCalendar(parse(calendar, format, timeZone));
    }

    /**
     * @see System#currentTimeMillis()
     * @return
     */
    public static long currentMillis() {
        return System.currentTimeMillis();
    }

    /**
     * A new instance of <code>java.sql.Time</code> returned is based on the
     * current time in the default time zone with the default locale.
     *
     * @return
     */
    public static Time currentTime() {
        return new Time(System.currentTimeMillis());
    }

    /**
     * A new instance of <code>java.sql.Date</code> returned is based on the
     * current time in the default time zone with the default locale.
     *
     * @return
     */
    public static Date currentDate() {
        return new Date(System.currentTimeMillis());
    }

    /**
     * A new instance of <code>java.sql.Timestamp</code> returned is based on
     * the current time in the default time zone with the default locale.
     *
     * @return
     */
    public static Timestamp currentTimestamp() {
        return new Timestamp(System.currentTimeMillis());
    }

    /**
     * A new instance of <code>java.util.Date</code> returned is based on the
     * current time in the default time zone with the default locale.
     *
     * @return
     */
    public static java.util.Date currentJUDate() {
        return new java.util.Date();
    }

    /**
     * A new instance of <code>java.util.Calendar</code> returned is based on
     * the current time in the default time zone with the default locale.
     *
     * @return a Calendar.
     */
    public static Calendar currentCalendar() {
        return Calendar.getInstance();
    }

    public static GregorianCalendar currentGregorianCalendar() {
        return new GregorianCalendar();
    }

    public static XMLGregorianCalendar currentXMLGregorianCalendar() {
        return dataTypeFactory.newXMLGregorianCalendar(currentGregorianCalendar());
    }

    /**
     * Adds or subtracts the specified amount of time to the given time unit,
     * based on the calendar's rules. For example, to subtract 5 days from the
     * current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(date, -5, TimeUnit.DAYS)</code>.
     *
     * @param date
     * @param amount
     * @param unit
     * @return a new instance of Date with the specified amount rolled.
     */
    public static <T extends java.util.Date> T roll(final T date, final long amount, final TimeUnit unit) {
        return createDate(date.getClass(), date.getTime() + unit.toMillis(amount));
    }

    /**
     * Adds or subtracts the specified amount of time to the given calendar
     * unit, based on the calendar's rules. For example, to subtract 5 days from
     * the current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(date, -5, CalendarUnit.DAY)</code>.
     *
     * @param date
     * @param amount
     * @param unit
     * @return a new instance of Date with the specified amount rolled.
     */
    public static <T extends java.util.Date> T roll(final T date, final long amount, final CalendarUnit unit) {
        if (amount > Integer.MAX_VALUE || amount < Integer.MIN_VALUE) {
            throw new IllegalArgumentException("The amount :" + amount + " is too big for unit: " + unit);
        }

        switch (unit) {
            case MONTH:
            case YEAR:
                final Calendar c = asCalendar(date);
                c.add(unit.intValue(), (int) amount);

                return createDate(date.getClass(), c.getTimeInMillis());

            default:
                return createDate(date.getClass(), date.getTime() + unit.toMillis(amount));
        }
    }

    private static <T extends java.util.Date> T createDate(final Class<? extends java.util.Date> cls, final long millis) {
        java.util.Date result = null;

        if (cls.equals(java.util.Date.class)) {
            result = new java.util.Date(millis);
        } else if (cls.equals(java.sql.Date.class)) {
            result = new java.sql.Date(millis);
        } else if (cls.equals(Time.class)) {
            result = new Time(millis);
        } else if (cls.equals(Timestamp.class)) {
            result = new Timestamp(millis);
        } else {
            result = ClassUtil.invokeConstructor(ClassUtil.getDeclaredConstructor(cls, long.class), millis);
        }

        return (T) result;
    }

    /**
     * Adds or subtracts the specified amount of time to the given time unit,
     * based on the calendar's rules. For example, to subtract 5 days from the
     * current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(c, -5, TimeUnit.DAYS)</code>.
     *
     * @param c
     * @param amount
     * @param unit
     * @return a new instance of Calendar with the specified amount rolled.
     */
    public static <T extends Calendar> T roll(final T c, final long amount, final TimeUnit unit) {
        return createCalendar(c, c.getTimeInMillis() + unit.toMillis(amount));
    }

    /**
     * Adds or subtracts the specified amount of time to the given calendar
     * unit, based on the calendar's rules. For example, to subtract 5 days from
     * the current time of the calendar, you can achieve it by calling:
     * <p>
     * <code>N.roll(c, -5, CalendarUnit.DAY)</code>.
     *
     * @param c
     * @param amount
     * @param unit
     * @return a new instance of Calendar with the specified amount rolled.
     */
    public static <T extends Calendar> T roll(final T c, final long amount, final CalendarUnit unit) {
        if (amount > Integer.MAX_VALUE || amount < Integer.MIN_VALUE) {
            throw new IllegalArgumentException("The amount :" + amount + " is too big for unit: " + unit);
        }

        final T result = createCalendar(c, c.getTimeInMillis());

        result.add(unit.intValue(), (int) amount);

        return result;
    }

    private static <T extends Calendar> T createCalendar(final T c, final long millis) {
        final Class<T> cls = (Class<T>) c.getClass();

        Calendar result = null;

        if (cls.equals(Calendar.class)) {
            result = Calendar.getInstance();
        } else if (cls.equals(GregorianCalendar.class)) {
            result = GregorianCalendar.getInstance();
        } else {
            result = ClassUtil.invokeConstructor(ClassUtil.getDeclaredConstructor(cls, long.class), millis);
        }

        result.setTimeInMillis(millis);

        if (!equals(c.getTimeZone(), result.getTimeZone())) {
            result.setTimeZone(c.getTimeZone());
        }

        return (T) result;
    }

    public static String format(final java.util.Date date) {
        return format(date, null, null);
    }

    public static String format(final java.util.Date date, final String format) {
        return format(date, format, null);
    }

    public static String format(final java.util.Date date, final String format, final TimeZone timeZone) {
        return formatDate(null, date, format, timeZone);
    }

    public static void format(final Writer writer, final java.util.Date date) {
        format(writer, date, null, null);
    }

    public static void format(final Writer writer, final java.util.Date date, final String format, final TimeZone timeZone) {
        formatDate(writer, date, format, timeZone);
    }

    private static String formatDate(final Writer writer, final java.util.Date date, String format, TimeZone timeZone) {
        boolean isTimestamp = date instanceof Timestamp;

        if ((format == null) && (timeZone == null)) {
            if (writer == null) {
                final BufferedWriter bw = ObjectFactory.createBufferedWriter();

                fastDateFormat(bw, date.getTime(), isTimestamp);

                String str = bw.toString();

                ObjectFactory.recycle(bw);

                return str;
            } else {
                fastDateFormat(writer, date.getTime(), isTimestamp);

                return null;
            }
        }

        if (format == null) {
            format = isTimestamp ? ISO_8601_TIMESTAMP_FORMAT : ISO_8601_DATETIME_FORMAT;
        }

        timeZone = checkTimeZone(format, timeZone);

        DateFormat sdf = getSDF(format, timeZone);

        String str = sdf.format(date);

        if (writer != null) {
            try {
                writer.write(str);
            } catch (IOException e) {
                throw new UncheckedIOException();
            }
        }

        recycleSDF(format, timeZone, sdf);

        return str;
    }

    public static String format(final Calendar c) {
        return format(c, null, null);
    }

    public static String format(final Calendar c, final String format) {
        return format(c, format, null);
    }

    public static String format(final Calendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            final BufferedWriter cbuff = ObjectFactory.createBufferedWriter();
            fastDateFormat(cbuff, c.getTimeInMillis(), false);

            String str = cbuff.toString();

            ObjectFactory.recycle(cbuff);

            return str;
        }

        return format(asJUDate(c), format, timeZone);
    }

    public static void format(final Writer writer, final Calendar c) {
        format(writer, c, null, null);
    }

    public static void format(final Writer writer, final Calendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            fastDateFormat(writer, c.getTimeInMillis(), false);
        } else {
            format(writer, asJUDate(c), format, timeZone);
        }
    }

    public static String format(final XMLGregorianCalendar c) {
        return format(c, null, null);
    }

    public static String format(final XMLGregorianCalendar c, final String format) {
        return format(c, format, null);
    }

    public static String format(final XMLGregorianCalendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            final BufferedWriter cbuff = ObjectFactory.createBufferedWriter();

            fastDateFormat(cbuff, c.toGregorianCalendar().getTimeInMillis(), false);

            String str = cbuff.toString();

            ObjectFactory.recycle(cbuff);

            return str;
        }

        return format(asJUDate(c.toGregorianCalendar()), format, timeZone);
    }

    public static void format(final Writer writer, final XMLGregorianCalendar c) {
        format(writer, c, null, null);
    }

    public static void format(final Writer writer, final XMLGregorianCalendar c, final String format, final TimeZone timeZone) {
        if ((format == null) && (timeZone == null)) {
            fastDateFormat(writer, c.toGregorianCalendar().getTimeInMillis(), false);
        } else {
            format(writer, asJUDate(c.toGregorianCalendar()), format, timeZone);
        }
    }

    private static void fastDateFormat(final Writer writer, final long timeInMillis, final boolean isTimestamp) {
        Calendar c = utcCalendarPool.poll();

        if (c == null) {
            c = Calendar.getInstance(UTC_TIME_ZONE);
        }

        c.setTimeInMillis(timeInMillis);

        int year = c.get(Calendar.YEAR);
        int month = c.get(Calendar.MONTH) + 1;
        int day = c.get(Calendar.DAY_OF_MONTH);
        int hour = c.get(Calendar.HOUR_OF_DAY);
        int minute = c.get(Calendar.MINUTE);
        int second = c.get(Calendar.SECOND);

        char[] utcTimestamp = utcTimestampFormatCharsPool.poll();

        if (utcTimestamp == null) {
            utcTimestamp = new char[24];
            utcTimestamp[4] = '-';
            utcTimestamp[7] = '-';
            utcTimestamp[10] = 'T';
            utcTimestamp[13] = ':';
            utcTimestamp[16] = ':';
            utcTimestamp[19] = '.';
            utcTimestamp[23] = 'Z';
        }
        //
        // copy(cbufOfSTDInt[4][year], 0, utcTimestamp, 0, 4);
        // copy(cbufOfSTDInt[2][month], 0, utcTimestamp, 5, 2);
        // copy(cbufOfSTDInt[2][day], 0, utcTimestamp, 8, 2);
        // copy(cbufOfSTDInt[2][hour], 0, utcTimestamp, 11, 2);
        // copy(cbufOfSTDInt[2][minute], 0, utcTimestamp, 14, 2);
        // copy(cbufOfSTDInt[2][second], 0, utcTimestamp, 17, 2);
        //
        utcTimestamp[0] = cbufOfSTDInt[4][year][0];
        utcTimestamp[1] = cbufOfSTDInt[4][year][1];
        utcTimestamp[2] = cbufOfSTDInt[4][year][2];
        utcTimestamp[3] = cbufOfSTDInt[4][year][3];

        utcTimestamp[5] = cbufOfSTDInt[2][month][0];
        utcTimestamp[6] = cbufOfSTDInt[2][month][1];

        utcTimestamp[8] = cbufOfSTDInt[2][day][0];
        utcTimestamp[9] = cbufOfSTDInt[2][day][1];

        utcTimestamp[11] = cbufOfSTDInt[2][hour][0];
        utcTimestamp[12] = cbufOfSTDInt[2][hour][1];

        utcTimestamp[14] = cbufOfSTDInt[2][minute][0];
        utcTimestamp[15] = cbufOfSTDInt[2][minute][1];

        utcTimestamp[17] = cbufOfSTDInt[2][second][0];
        utcTimestamp[18] = cbufOfSTDInt[2][second][1];

        if (isTimestamp) {
            utcTimestamp[19] = '.';

            int milliSecond = c.get(Calendar.MILLISECOND);
            // copy(cbufOfSTDInt[3][milliSecond], 0, utcTimestamp,
            // 20, 3);
            utcTimestamp[20] = cbufOfSTDInt[3][milliSecond][0];
            utcTimestamp[21] = cbufOfSTDInt[3][milliSecond][1];
            utcTimestamp[22] = cbufOfSTDInt[3][milliSecond][2];
        } else {
            utcTimestamp[19] = 'Z';
        }

        try {
            if (isTimestamp) {
                writer.write(utcTimestamp);
            } else {
                writer.write(utcTimestamp, 0, 20);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            utcCalendarPool.add(c);
            utcTimestampFormatCharsPool.add(utcTimestamp);
        }
    }

    /**
     * @param src
     * @return
     */
    public static String base64Encode(final byte[] src) {
        return Base64.getEncoder().encodeToString(src);
    }

    /**
     * @param src
     * @return
     */
    public static byte[] base64Decode(final String src) {
        return Base64.getDecoder().decode(src);
    }

    /**
     * @param src
     * @return
     */
    public static String base64UrlEncode(final byte[] src) {
        return Base64.getUrlEncoder().encodeToString(src);
    }

    /**
     * @param src
     * @return
     */
    public static byte[] base64UrlDecode(final String src) {
        return Base64.getUrlDecoder().decode(src);
    }

    /**
     * @param src
     * @return
     */
    public static String base64MimeEncode(final byte[] src) {
        return Base64.getMimeEncoder().encodeToString(src);
    }

    /**
     * @param src
     * @return
     */
    public static byte[] base64MimeDecode(final String src) {
        return Base64.getMimeDecoder().decode(src);
    }

    public static String urlEncode(final Object parameters) {
        return URLEncodedUtil.encode(parameters);
    }

    public static String urlEncode(final Object parameters, final Charset charset) {
        return URLEncodedUtil.encode(parameters, charset);
    }

    public static Map<String, String> urlDecode(final String urlQuery) {
        return URLEncodedUtil.decode(urlQuery);
    }

    public static Map<String, String> urlDecode(final String urlQuery, final Charset charset) {
        return URLEncodedUtil.decode(urlQuery, charset);
    }

    public static <T> T urlDecode(final Class<T> targetClass, final String urlQuery) {
        return URLEncodedUtil.decode(targetClass, urlQuery);
    }

    public static <T> T urlDecode(final Class<T> targetClass, final String urlQuery, final Charset charset) {
        return URLEncodedUtil.decode(targetClass, urlQuery, charset);
    }

    /**
     * Returns the UUID without '-'.
     *
     * @see UUID#randomUUID().
     */
    public static String guid() {
        return uuid().replace("-", "");
    }

    /**
     * Returns an UUID
     *
     * @see UUID#randomUUID().
     * @return
     */
    public static String uuid() {
        return UUID.randomUUID().toString();
    }

    /**
     * <p>
     * Convert a <code>String</code> to a <code>Integer</code>, handling hex
     * (0xhhhh) and octal (0dddd) notations. N.B. a leading zero means octal;
     * spaces are not trimmed.
     * </p>
     *
     * <p>
     * Returns an empty {@code OptionalInt} if the string is {@code null} or can't be parsed as {@code Integer}.
     * </p>
     *
     * @param str a <code>String</code> to convert, may be null
     * @return
     */
    public static OptionalInt createInteger(final String str) {
        final Integer val = createInteger(str, true);

        return val == null ? OptionalInt.empty() : OptionalInt.of(val);
    }

    // It's copied from OpenJDK 1.7 on 3/9/2015
    /*
     * Copyright (c) 1994, 2009, Oracle and/or its affiliates. All rights reserved.
     * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
     *
     * This code is free software; you can redistribute it and/or modify it
     * under the terms of the GNU General Public License version 2 only, as
     * published by the Free Software Foundation.  Oracle designates this
     * particular file as subject to the "Classpath" exception as provided
     * by Oracle in the LICENSE file that accompanied this code.
     *
     * This code is distributed in the hope that it will be useful, but WITHOUT
     * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
     * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
     * version 2 for more details (a copy is included in the LICENSE file that
     * accompanied this code).
     *
     * You should have received a copy of the GNU General Public License version
     * 2 along with this work; if not, write to the Free Software Foundation,
     * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
     *
     * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
     * or visit www.oracle.com if you need additional information or have any
     * questions.
     */
    private static Integer createInteger(final String str, final boolean isNumberCheck) {
        if (N.isNullOrEmpty(str)) {
            return null;
        }

        boolean negative = false;
        int radix = 10;
        int index = 0;

        char firstChar = str.charAt(0);
        // Handle sign, if present
        if (firstChar == '-') {
            negative = true;
            index++;
        } else if (firstChar == '+') {
            index++;
        }

        // Handle radix specifier, if present
        if (str.startsWith("0x", index) || str.startsWith("0X", index)) {
            index += 2;
            radix = 16;
        } else if (str.startsWith("#", index)) {
            index++;
            radix = 16;
        } else if (str.startsWith("0", index) && str.length() > 1 + index) {
            index++;
            radix = 8;
        }

        if (str.startsWith("-", index) || str.startsWith("+", index)) {
            if (isNumberCheck) {
                return null;
            } else {
                throw new NumberFormatException("Sign character in wrong position");
            }
        }

        int value = 0;

        if (isNumberCheck) {
            try {
                value = IOUtil.parseInt(N.getCharsForReadOnly(str), index, str.length() - index, radix);
            } catch (NumberFormatException e) {
                return null;
            }
        } else {
            value = IOUtil.parseInt(N.getCharsForReadOnly(str), index, str.length() - index, radix);
        }

        return negative ? -value : value;
    }

    /**
     * <p>
     * Convert a <code>String</code> to a <code>Long</code>; since 3.1 it
     * handles hex (0Xhhhh) and octal (0ddd) notations. N.B. a leading zero
     * means octal; spaces are not trimmed.
     * </p>
     *
     * <p>
     * Returns an empty {@code OptionalLong} if the string is {@code null} or can't be parsed as {@code Long}.
     * </p>
     *
     * @param str a <code>String</code> to convert, may be null
     * @return
     */
    public static OptionalLong createLong(final String str) {
        final Long val = createLong(str, true);

        return val == null ? OptionalLong.empty() : OptionalLong.of(val);
    }

    // It's copied from OpenJDK 1.7 on 3/9/2015
    /*
     * Copyright (c) 1994, 2009, Oracle and/or its affiliates. All rights reserved.
     * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
     *
     * This code is free software; you can redistribute it and/or modify it
     * under the terms of the GNU General Public License version 2 only, as
     * published by the Free Software Foundation.  Oracle designates this
     * particular file as subject to the "Classpath" exception as provided
     * by Oracle in the LICENSE file that accompanied this code.
     *
     * This code is distributed in the hope that it will be useful, but WITHOUT
     * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
     * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
     * version 2 for more details (a copy is included in the LICENSE file that
     * accompanied this code).
     *
     * You should have received a copy of the GNU General Public License version
     * 2 along with this work; if not, write to the Free Software Foundation,
     * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
     *
     * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
     * or visit www.oracle.com if you need additional information or have any
     * questions.
     */
    private static Long createLong(final String str, final boolean isNumberCheck) {
        if (N.isNullOrEmpty(str)) {
            return null;
        }

        boolean negative = false;
        int radix = 10;
        int index = 0;

        char firstChar = str.charAt(0);
        // Handle sign, if present
        if (firstChar == '-') {
            negative = true;
            index++;
        } else if (firstChar == '+') {
            index++;
        }

        // Handle radix specifier, if present
        if (str.startsWith("0x", index) || str.startsWith("0X", index)) {
            index += 2;
            radix = 16;
        } else if (str.startsWith("#", index)) {
            index++;
            radix = 16;
        } else if (str.startsWith("0", index) && str.length() > 1 + index) {
            index++;
            radix = 8;
        }

        if (str.startsWith("-", index) || str.startsWith("+", index)) {
            if (isNumberCheck) {
                return null;
            } else {
                throw new NumberFormatException("Sign character in wrong position");
            }
        }

        long value = 0;

        if (isNumberCheck) {
            try {
                value = IOUtil.parseLong(N.getCharsForReadOnly(str), index, str.length() - index, radix);
            } catch (NumberFormatException e) {
                return null;
            }
        } else {
            value = IOUtil.parseLong(N.getCharsForReadOnly(str), index, str.length() - index, radix);
        }

        return negative ? -value : value;
    }

    // -----------------------------------------------------------------------
    /**
     * <p>
     * Convert a <code>String</code> to a <code>Float</code>.
     * </p>
     *
     * <p>
     * Returns an empty {@code OptionalFloat} if the string is {@code null} or can't be parsed as {@code Float}.
     * </p>
     *
     * @param str a <code>String</code> to convert, may be null
     * @return
     */
    public static OptionalFloat createFloat(final String str) {
        final Float val = N.isNullOrEmpty(str) ? null : createNumber(str, true).floatValue();

        return val == null ? OptionalFloat.empty() : OptionalFloat.of(val);
    }

    /**
     * <p>
     * Convert a <code>String</code> to a <code>Double</code>.
     * </p>
     *
     * <p>
     * <p>
     * Returns an empty {@code OptionalDouble} if the string is {@code null} or can't be parsed as {@code Double}.
     * </p>
     * </p>
     *
     * @param str a <code>String</code> to convert, may be null
     * @return
     */
    public static OptionalDouble createDouble(final String str) {
        final Double val = N.isNullOrEmpty(str) ? null : createNumber(str, true).doubleValue();

        return val == null ? OptionalDouble.empty() : OptionalDouble.of(val);
    }

    /**
     * <p>
     * Convert a <code>String</code> to a <code>BigInteger</code>; since 3.2 it
     * handles hex (0x or #) and octal (0) notations.
     * </p>
     *
     * <p>
     * Returns an empty {@code Optional} if the string is {@code null} or can't be parsed as {@code BigInteger}.
     * </p>
     *
     * @param str a <code>String</code> to convert, may be null
     * @return
     */
    public static Optional<BigInteger> createBigInteger(final String str) throws NumberFormatException {
        final BigInteger val = createBigInteger(str, true);

        return val == null ? Optional.<BigInteger> empty() : Optional.of(val);
    }

    private static BigInteger createBigInteger(final String str, final boolean isNumberCheck) {
        if (N.isNullOrEmpty(str)) {
            return null;
        }

        boolean negative = false;
        int radix = 10;
        int index = 0;

        char firstChar = str.charAt(0);
        // Handle sign, if present
        if (firstChar == '-') {
            negative = true;
            index++;
        } else if (firstChar == '+') {
            index++;
        }

        // Handle radix specifier, if present
        if (str.startsWith("0x", index) || str.startsWith("0X", index)) {
            index += 2;
            radix = 16;
        } else if (str.startsWith("#", index)) {
            index++;
            radix = 16;
        } else if (str.startsWith("0", index) && str.length() > 1 + index) {
            index++;
            radix = 8;
        }

        if (str.startsWith("-", index) || str.startsWith("+", index)) {
            if (isNumberCheck) {
                return null;
            } else {
                throw new NumberFormatException("Sign character in wrong position");
            }
        }

        final BigInteger value = new BigInteger(str.substring(index), radix);

        return negative ? value.negate() : value;
    }

    /**
     * <p>
     * Convert a <code>String</code> to a <code>BigDecimal</code>.
     * </p>
     *
     * <p>
     * Returns an empty {@code Optional} if the string is {@code null} or can't be parsed as {@code BigDecimal}.
     * </p>
     *
     * @param str a <code>String</code> to convert, may be null
     * @return
     */
    public static Optional<BigDecimal> createBigDecimal(final String str) throws NumberFormatException {
        final BigDecimal val = createBigDecimal(str, true);

        return val == null ? Optional.<BigDecimal> empty() : Optional.of(val);
    }

    private static BigDecimal createBigDecimal(final String str, final boolean isNumberCheck) {
        if (N.isNullOrEmpty(str)) {
            return null;
        }

        // handle JDK1.3.1 bug where "" throws IndexOutOfBoundsException
        if (N.containsWhitespace(str)) {
            if (isNumberCheck) {
                return null;
            } else {
                throw new NumberFormatException("A blank string is not a valid number");
            }
        }

        if (str.trim().startsWith("--")) {
            // this is protection for poorness in java.lang.BigDecimal.
            // it accepts this as a legal value, but it does not appear
            // to be in specification of class. OS X Java parses it to
            // a wrong value.

            if (isNumberCheck) {
                return null;
            } else {
                throw new NumberFormatException(str + " is not a valid number.");
            }
        }

        return new BigDecimal(str);
    }

    /**
     * <p>
     * Turns a string value into a java.lang.Number.
     * </p>
     *
     * <p>
     * If the string starts with {@code 0x} or {@code -0x} (lower or upper case)
     * or {@code #} or {@code -#}, it will be interpreted as a hexadecimal
     * Integer - or Long, if the number of digits after the prefix is more than
     * 8 - or BigInteger if there are more than 16 digits.
     * </p>
     * <p>
     * Then, the value is examined for a type qualifier on the end, i.e. one of
     * <code>'f','F','d','D','l','L'</code>. If it is found, it starts trying to
     * create successively larger types from the type specified until one is
     * found that can represent the value.
     * </p>
     *
     * <p>
     * If a type specifier is not found, it will check for a decimal point and
     * then try successively larger types from <code>Integer</code> to
     * <code>BigInteger</code> and from <code>double</code> to
     * <code>BigDecimal</code>.
     * </p>
     *
     * <p>
     * Integral values with a leading {@code 0} will be interpreted as octal;
     * the returned number will be Integer, Long or BigDecimal as appropriate.
     * </p>
     *
     * <p>
     * Returns an empty {@code Optional} if the string is {@code null} or can't be parsed as {@code Number}.
     * </p>
     *
     * <p>
     * This method does not trim the input string, i.e., strings with leading or
     * trailing spaces will generate NumberFormatExceptions.
     * </p>
     *
     * @param str a String containing a number, may be null
     * @return
     */
    public static Optional<Number> createNumber(final String str) {
        final Number val = createNumber(str, true);

        return val == null ? Optional.<Number> empty() : Optional.of(val);
    }

    private static Number createNumber(final String str, final boolean isNumberCheck) throws NumberFormatException {
        if (N.isNullOrEmpty(str)) {
            return null;
        }

        if (N.containsWhitespace(str)) {
            if (isNumberCheck) {
                return null;
            } else {
                throw new NumberFormatException("A blank string is not a valid number");
            }
        }

        // Need to deal with all possible hex prefixes here
        final String[] hex_prefixes = { "0x", "0X", "-0x", "-0X", "#", "-#" };
        int pfxLen = 0;
        for (final String pfx : hex_prefixes) {
            if (str.startsWith(pfx)) {
                pfxLen += pfx.length();

                break;
            }
        }

        if (pfxLen > 0) { // we have a hex number
            char firstSigDigit = 0; // strip leading zeroes
            for (int i = pfxLen; i < str.length(); i++) {
                firstSigDigit = str.charAt(i);
                if (firstSigDigit == '0') { // count leading zeroes
                    pfxLen++;
                } else {
                    break;
                }
            }

            final int hexDigits = str.length() - pfxLen;
            if (hexDigits > 16 || (hexDigits == 16 && firstSigDigit > '7')) { // too many for Long
                return createBigInteger(str, isNumberCheck);
            } else if (hexDigits > 8 || (hexDigits == 8 && firstSigDigit > '7')) { // too many for an int
                return createLong(str, isNumberCheck);
            } else {
                return createInteger(str, isNumberCheck);
            }
        }

        String mant;
        String dec;
        String exp;
        final int decPos = str.indexOf('.');
        final int expPos = str.indexOf('e') + str.indexOf('E') + 1; // assumes
        // both not
        // present
        // if both e and E are present, this is caught by the checks on expPos
        // (which prevent IOOBE)
        // and the parsing which will detect if e or E appear in a number due to
        // using the wrong offset

        int numDecimals = 0; // Check required precision (LANG-693)
        if (decPos > -1) { // there is a decimal point

            if (expPos > -1) { // there is an exponent
                if (expPos < decPos || expPos > str.length()) { // prevents double exponent causing IOOBE
                    if (isNumberCheck) {
                        return null;
                    } else {
                        throw new NumberFormatException(str + " is not a valid number.");
                    }
                }

                dec = str.substring(decPos + 1, expPos);
            } else {
                dec = str.substring(decPos + 1);
            }

            mant = str.substring(0, decPos);
            numDecimals = dec.length(); // gets number of digits past the
            // decimal to ensure no loss of
            // precision for floating point numbers.
        } else {
            if (expPos > -1) {
                if (expPos > str.length()) { // prevents double exponent causing IOOBE
                    if (isNumberCheck) {
                        return null;
                    } else {
                        throw new NumberFormatException(str + " is not a valid number.");
                    }
                }

                mant = str.substring(0, expPos);
            } else {
                mant = str;
            }

            dec = null;
        }

        final char lastChar = str.charAt(str.length() - 1);
        if (!Character.isDigit(lastChar) && lastChar != '.') {
            if (expPos > -1 && expPos < str.length() - 1) {
                exp = str.substring(expPos + 1, str.length() - 1);
            } else {
                exp = null;
            }

            // Requesting a specific type..
            final String numeric = str.substring(0, str.length() - 1);
            final boolean allZeros = isAllZeros(mant) && isAllZeros(exp);
            switch (lastChar) {
                case 'l':
                case 'L':
                    if (dec == null && exp == null && (numeric.charAt(0) == '-' && isNumeric(numeric.substring(1)) || isNumeric(numeric))) {
                        try {
                            return createLong(numeric, isNumberCheck);
                        } catch (final NumberFormatException nfe) { // NOPMD
                            // Too big for a long
                        }

                        return createBigInteger(numeric, isNumberCheck);
                    }

                    if (isNumberCheck) {
                        return null;
                    } else {
                        throw new NumberFormatException(str + " is not a valid number.");
                    }

                case 'f':
                case 'F':
                    try {
                        final Float f = Float.valueOf(numeric);
                        if (!(f.isInfinite() || (f.floatValue() == 0.0F && !allZeros))) {
                            // If it's too big for a float or the float value = 0
                            // and the string
                            // has non-zeros in it, then float does not have the
                            // precision we want
                            return f;
                        }
                    } catch (final NumberFormatException nfe) { // NOPMD
                        // ignore the bad number
                    }

                    //$FALL-THROUGH$
                case 'd':
                case 'D':
                    try {
                        final Double d = Double.valueOf(numeric);
                        if (!(d.isInfinite() || (d.floatValue() == 0.0D && !allZeros))) {
                            return d;
                        }
                    } catch (final NumberFormatException nfe) { // NOPMD
                        // ignore the bad number
                    }

                    try {
                        return createBigDecimal(numeric, isNumberCheck);
                    } catch (final NumberFormatException e) { // NOPMD
                        // ignore the bad number
                    }

                    //$FALL-THROUGH$
                default:
                    if (isNumberCheck) {
                        return null;
                    } else {
                        throw new NumberFormatException(str + " is not a valid number.");
                    }
            }
        }

        // User doesn't have a preference on the return type, so let's start
        // small and go from there...
        if (expPos > -1 && expPos < str.length() - 1) {
            exp = str.substring(expPos + 1, str.length());
        } else {
            exp = null;
        }

        if (dec == null && exp == null) { // no decimal point and no exponent
            // Must be an Integer, Long, Biginteger
            try {
                return createInteger(str, isNumberCheck);
            } catch (final NumberFormatException nfe) { // NOPMD
                // ignore the bad number
            }

            try {
                return createLong(str, isNumberCheck);
            } catch (final NumberFormatException nfe) { // NOPMD
                // ignore the bad number
            }

            return createBigInteger(str, isNumberCheck);
        }

        // Must be a Float, Double, BigDecimal
        final boolean allZeros = isAllZeros(mant) && isAllZeros(exp);

        // refer to: https://issues.apache.org/jira/browse/LANG-1018. skip float
        // try {
        // if (numDecimals <= 7) {// If number has 7 or fewer digits past the
        // decimal point then make it a float
        // final Float f = createFloat(str);
        // if (!(f.isInfinite() || (f.floatValue() == 0.0F && !allZeros))) {
        // return f;
        // }
        // }
        // } catch (final NumberFormatException nfe) { // NOPMD
        // // ignore the bad number
        // }
        //
        try {
            if (numDecimals <= 16) {// If number has between 8 and 16 digits
                // past the decimal point then make it a
                // double
                final Double d = Double.valueOf(str);
                if (!(d.isInfinite() || (d.doubleValue() == 0.0D && !allZeros))) {
                    return d;
                }
            }
        } catch (final NumberFormatException nfe) { // NOPMD
            // ignore the bad number
        }

        return createBigDecimal(str, isNumberCheck);
    }

    private static boolean isAllZeros(final String str) {
        if (str == null) {
            return true;
        }

        for (int i = str.length() - 1; i >= 0; i--) {
            if (str.charAt(i) != '0') {
                return false;
            }
        }

        return str.length() > 0;
    }

    /**
     * Returns a new created instance of the same class and set with same
     * properties retrieved by 'getXXX' method in the specified {@code entity}.
     *
     * @param entity
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T copy(final T entity) {
        return copy((Class<T>) entity.getClass(), entity);
    }

    public static <T> T copy(final T entity, final Set<String> selectPropNames) {
        return copy((Class<T>) entity.getClass(), entity, selectPropNames);
    }

    public static <T> T copy(final Class<T> targetClass, final Object entity) {
        return copy(targetClass, entity, null);
    }

    /**
     * Returns a new created instance of the specified {@code cls} and set with
     * same properties retrieved by 'getXXX' method in the specified
     * {@code entity}.
     *
     * @param targetClass
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     * @param entity
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     * @param selectPropNames
     * @return
     */
    @SuppressWarnings({ "unchecked", "deprecation" })
    public static <T> T copy(final Class<T> targetClass, final Object entity, final Set<String> selectPropNames) {
        T copy = null;

        if (selectPropNames == null && kryoParser != null && targetClass.equals(entity.getClass())) {
            try {
                copy = (T) kryoParser.copy(entity);
            } catch (Exception e) {
                // ignore
            }
        }

        final boolean ignoreUnknownProperty = selectPropNames == null;

        if (copy == null) {
            Class<?> srcCls = entity.getClass();
            copy = newInstance(targetClass);

            if (entity instanceof DirtyMarker) {
                Set<String> signedPropNames = ((DirtyMarker) entity).signedPropNames();

                if (signedPropNames.size() == 0) {
                    // logger.warn("no property is signed in the specified source entity: "
                    // + toString(entity));
                } else {
                    try {
                        Method srcPropGetMethod = null;

                        for (String propName : signedPropNames) {
                            if (selectPropNames == null || selectPropNames.contains(propName)) {
                                srcPropGetMethod = ClassUtil.getPropGetMethod(srcCls, propName);

                                ClassUtil.setPropValue(copy, propName, srcPropGetMethod.invoke(entity), ignoreUnknownProperty);
                            }
                        }
                    } catch (Exception e) {
                        throw new AbacusException(e);
                    }
                }
            } else {
                Map<String, Method> srcGetterMethodList = ClassUtil.checkPropGetMethodList(srcCls);

                try {
                    for (Map.Entry<String, Method> entry : srcGetterMethodList.entrySet()) {
                        if (selectPropNames == null || selectPropNames.contains(entry.getKey())) {
                            ClassUtil.setPropValue(copy, entry.getKey(), entry.getValue().invoke(entity), ignoreUnknownProperty);
                        }
                    }
                } catch (Exception e) {
                    throw new AbacusException(e);
                }
            }

            setDirtyMarker(entity, copy);
        }

        return copy;
    }

    public static <T> T copy(final T entity, final boolean ignoreUnknownProperty, final Set<String> ignorePropNames) {
        return copy((Class<T>) entity.getClass(), entity, ignoreUnknownProperty, ignorePropNames);
    }

    @SuppressWarnings({ "unchecked", "deprecation" })
    public static <T> T copy(final Class<T> targetClass, final T entity, final boolean ignoreUnknownProperty, final Set<String> ignorePropNames) {
        T copy = null;

        if (ignorePropNames == null && kryoParser != null && targetClass.equals(entity.getClass())) {
            try {
                copy = kryoParser.copy(entity);
            } catch (Exception e) {
                // ignore
            }
        }

        if (copy == null) {
            Class<?> srcCls = entity.getClass();
            copy = newInstance(targetClass);

            if (entity instanceof DirtyMarker) {
                Set<String> signedPropNames = ((DirtyMarker) entity).signedPropNames();

                if (signedPropNames.size() == 0) {
                    // logger.warn("no property is signed in the specified source entity: "
                    // + toString(entity));
                } else {
                    try {
                        Method srcPropGetMethod = null;

                        for (String propName : signedPropNames) {
                            if (ignorePropNames == null || ignorePropNames.contains(propName) == false) {
                                srcPropGetMethod = ClassUtil.getPropGetMethod(srcCls, propName);

                                ClassUtil.setPropValue(copy, propName, srcPropGetMethod.invoke(entity), ignoreUnknownProperty);
                            }
                        }
                    } catch (Exception e) {
                        throw new AbacusException(e);
                    }
                }
            } else {
                Map<String, Method> srcGetterMethodList = ClassUtil.checkPropGetMethodList(srcCls);

                try {
                    for (Map.Entry<String, Method> entry : srcGetterMethodList.entrySet()) {
                        if (ignorePropNames == null || ignorePropNames.contains(entry.getKey()) == false) {
                            ClassUtil.setPropValue(copy, entry.getKey(), entry.getValue().invoke(entity), ignoreUnknownProperty);
                        }
                    }
                } catch (Exception e) {
                    throw new AbacusException(e);
                }
            }

            setDirtyMarker(entity, copy);
        }

        return copy;
    }

    @SuppressWarnings("unchecked")
    public static <T> T clone(final T entity) {
        return (T) clone(entity.getClass(), entity);
    }

    /**
     * Deeply copy by: entity -> serialize -> String/bytes -> deserialize -> new
     * entity.
     *
     * @param targetClass
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     * @param entity
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T clone(final Class<T> targetClass, final Object entity) {
        Object copy = null;

        if (kryoParser != null && targetClass.equals(entity.getClass())) {
            try {
                copy = kryoParser.clone(entity);
            } catch (Exception e) {
                // ignore.
            }
        }

        if (copy == null) {
            String xml = abacusXMLParser.serialize(entity, xscForClone);
            copy = abacusXMLParser.deserialize(targetClass, xml);

            setDirtyMarker(entity, copy);
        }

        return (T) copy;
    }

    @SuppressWarnings("deprecation")
    private static void setDirtyMarker(final Object source, final Object target) {
        if (source instanceof DirtyMarker && target instanceof DirtyMarker) {
            DirtyMarker dirtyMarkerSource = (DirtyMarker) source;
            DirtyMarker dirtyMarkerTarget = (DirtyMarker) target;

            dirtyMarkerTarget.signedPropNames().clear();
            dirtyMarkerTarget.signedPropNames().addAll(dirtyMarkerSource.signedPropNames());

            dirtyMarkerTarget.dirtyPropNames().clear();
            dirtyMarkerTarget.dirtyPropNames().addAll(dirtyMarkerSource.dirtyPropNames());

            EntityManagerUtil.setVersion(dirtyMarkerTarget, dirtyMarkerSource.version());
        }
    }

    public static void merge(final Object sourceEntity, final Object targetEntity) {
        merge(sourceEntity, null, targetEntity);
    }

    /**
     * Set all the signed properties(including all primitive type properties) in
     * the specified {@code sourceEntity} to the specified {@code targetEntity}.
     *
     * @param sourceEntity
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     * @param selectPropNames
     * @param targetEntity
     *            a Java Object what allows access to properties using getter
     *            and setter methods.
     */
    @SuppressWarnings("deprecation")
    public static void merge(final Object sourceEntity, final Set<String> selectPropNames, final Object targetEntity) {
        final Class<?> sourceEntityClass = sourceEntity.getClass();
        final boolean ignoreUnknownProperty = selectPropNames == null;

        if (sourceEntity instanceof Map) {
            final Map<String, Object> m = (Map<String, Object>) sourceEntity;

            for (Map.Entry<String, Object> entry : m.entrySet()) {
                if (selectPropNames == null || selectPropNames.contains(entry.getKey())) {
                    ClassUtil.setPropValue(targetEntity, entry.getKey(), entry.getValue(), ignoreUnknownProperty);
                }
            }
        } else if (sourceEntity instanceof DirtyMarker) {
            Set<String> signedPropNames = ((DirtyMarker) sourceEntity).signedPropNames();

            if (signedPropNames.size() == 0) {
                // logger.warn("No property is signed in the specified source entity: " + toString(sourceEntity));
                return;
            }

            Method srcPropGetMethod = null;
            Object propValue = null;

            try {
                for (String propName : signedPropNames) {
                    if (selectPropNames == null || selectPropNames.contains(propName)) {
                        srcPropGetMethod = ClassUtil.getPropGetMethod(sourceEntityClass, propName);
                        propValue = srcPropGetMethod.invoke(sourceEntity);
                        ClassUtil.setPropValue(targetEntity, propName, propValue, ignoreUnknownProperty);
                    }
                }
            } catch (Exception e) {
                throw new AbacusException(e);
            }
        } else {
            final Map<String, Method> getterMethodList = ClassUtil.checkPropGetMethodList(sourceEntity.getClass());

            String propName = null;
            Object propValue = null;

            try {
                for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                    propName = entry.getKey();

                    if (selectPropNames == null || selectPropNames.contains(propName)) {
                        propValue = entry.getValue().invoke(sourceEntity);

                        if (propValue == null) {
                            continue;
                        }

                        ClassUtil.setPropValue(targetEntity, propName, propValue, ignoreUnknownProperty);
                    }
                }
            } catch (Exception e) {
                throw new AbacusException(e);
            }
        }
    }

    @SuppressWarnings("deprecation")
    @SafeVarargs
    public static void erase(final Object entity, final String... propNames) {
        if (entity == null || N.isNullOrEmpty(propNames) || (entity instanceof DirtyMarker && ((DirtyMarker) entity).signedPropNames().size() == 0)) {
            return;
        }

        for (String propName : propNames) {
            ClassUtil.setPropValue(entity, propName, null);
        }

        if (entity instanceof DirtyMarker) {
            final DirtyMarker dirtyMarkerEntity = (DirtyMarker) entity;

            for (String propName : propNames) {
                dirtyMarkerEntity.signedPropNames().remove(propName);
                dirtyMarkerEntity.dirtyPropNames().remove(propName);
            }
        }
    }

    @SuppressWarnings("deprecation")
    public static void erase(final Object entity, final Collection<String> propNames) {
        if (entity == null || N.isNullOrEmpty(propNames) || (entity instanceof DirtyMarker && ((DirtyMarker) entity).signedPropNames().size() == 0)) {
            return;
        }

        for (String propName : propNames) {
            ClassUtil.setPropValue(entity, propName, null);
        }

        if (entity instanceof DirtyMarker) {
            final DirtyMarker dirtyMarkerEntity = (DirtyMarker) entity;

            dirtyMarkerEntity.signedPropNames().removeAll(propNames);
            dirtyMarkerEntity.dirtyPropNames().removeAll(propNames);
        }
    }

    @SuppressWarnings("deprecation")
    public static void eraseAll(final Object entity) {
        if (entity == null) {
            return;
        }

        if (entity instanceof DirtyMarker) {
            final DirtyMarker dirtyMarkerEntity = (DirtyMarker) entity;
            final Set<String> signedPropNames = dirtyMarkerEntity.signedPropNames();

            if (signedPropNames.size() == 0) {
                // logger.warn("No property is signed in the specified source entity: " + toString(entity));
                return;
            }

            for (String propName : signedPropNames) {
                ClassUtil.setPropValue(entity, propName, null);
            }

            dirtyMarkerEntity.signedPropNames().clear();
            dirtyMarkerEntity.dirtyPropNames().clear();
        } else {
            Class<?> cls = entity.getClass();
            Map<String, Method> setterMethodList = ClassUtil.getPropSetMethodList(cls);

            if (setterMethodList.size() == 0) {
                throw new IllegalArgumentException("No property getter/setter method found in the specified entity: " + ClassUtil.getCanonicalClassName(cls));
            }

            for (Method propSetMethod : setterMethodList.values()) {
                ClassUtil.setPropValue(entity, propSetMethod, null);
            }
        }
    }

    public static int compare(final boolean a, final boolean b) {
        return (a == b) ? 0 : (a ? 1 : -1);
    }

    public static int compare(final byte a, final byte b) {
        return (a < b) ? -1 : ((a == b) ? 0 : 1);
    }

    public static int compare(final short a, final short b) {
        return (a < b) ? -1 : ((a == b) ? 0 : 1);
    }

    public static int compare(final int a, final int b) {
        return (a < b) ? -1 : ((a == b) ? 0 : 1);
    }

    public static int compare(final long a, final long b) {
        return (a < b) ? -1 : ((a == b) ? 0 : 1);
    }

    public static int compare(final float a, final float b) {
        return Float.compare(a, b);
    }

    public static int compare(final double a, final double b) {
        return Double.compare(a, b);
    }

    public static <T extends Comparable<? super T>> int compare(final T a, final T b) {
        return a == null ? (b == null ? 0 : -1) : (b == null ? 1 : a.compareTo(b));
    }

    /**
     * Returns 0 if the arguments are identical and {@code c.compare(a, b)}
     * otherwise. Consequently, if both arguments are {@code null} 0 is
     * returned.
     *
     * <p>
     * Note that if one of the arguments is {@code null}, a
     * {@code NullPointerException} may or may not be thrown depending on what
     * ordering policy, if any, the {@link Comparator Comparator} chooses to
     * have for {@code null} values.
     *
     * @param <T>
     *            the type of the objects being compared
     * @param a
     *            an object
     * @param b
     *            an object to be compared with {@code a}
     * @param cmp
     *            the {@code Comparator} to compare the first two arguments
     * @return 0 if the arguments are identical and {@code c.compare(a, b)}
     *         otherwise.
     * @see Comparable
     * @see Comparator
     */
    public static <T> int compare(final T a, final T b, final Comparator<? super T> cmp) {
        return a == null ? (b == null ? 0 : -1) : (b == null ? 1 : (cmp == null ? OBJ_COMPARATOR : cmp).compare(a, b));
    }

    /**
     * Continue to compare the pairs of values <code>(a1, b1), (a2, b2)</code> until they're not equal.
     * <code>0</code> is returned if all of the pairs of values are equal.
     * 
     * @param a1
     * @param b1
     * @param a2
     * @param b2
     * @return
     */
    public static <T1 extends Comparable<T1>, T2 extends Comparable<T2>> int compare(T1 a1, T1 b1, T2 a2, T2 b2) {
        int res = N.compare(a1, b1);

        if (res != 0) {
            return res;
        }

        return N.compare(a2, b2);
    }

    /**
     * Continue to compare the pairs of values <code>(a1, b1), (a2, b2), (a3, b3)</code> until they're not equal.
     * <code>0</code> is returned if all of the pairs of values are equal.
     * 
     * @param a1
     * @param b1
     * @param a2
     * @param b2
     * @param a3
     * @param b3
     * @return
     */
    public static <T1 extends Comparable<T1>, T2 extends Comparable<T2>, T3 extends Comparable<T3>> int compare(T1 a1, T1 b1, T2 a2, T2 b2, T3 a3, T3 b3) {
        int res = N.compare(a1, b1);

        if (res != 0) {
            return res;
        }

        res = N.compare(a2, b2);

        if (res != 0) {
            return res;
        }

        return N.compare(a3, b3);
    }

    /** 
     * Continue to compare the pairs of values <code>(a1, b1), (a2, b2), (a3, b3), (a4, b4)</code> until they're not equal.
     * <code>0</code> is returned if all of the pairs of values are equal.
     * 
     * @param a1
     * @param b1
     * @param a2
     * @param b2
     * @param a3
     * @param b3
     * @param a4
     * @param b4
     * @return
     */
    public static <T1 extends Comparable<T1>, T2 extends Comparable<T2>, T3 extends Comparable<T3>, T4 extends Comparable<T4>> int compare(T1 a1, T1 b1, T2 a2,
            T2 b2, T3 a3, T3 b3, T4 a4, T4 b4) {
        int res = N.compare(a1, b1);

        if (res != 0) {
            return res;
        }

        res = N.compare(a2, b2);

        if (res != 0) {
            return res;
        }

        res = N.compare(a3, b3);

        if (res != 0) {
            return res;
        }

        return N.compare(a4, b4);
    }

    /**
     * Continue to compare the pairs of values <code>(a1, b1), (a2, b2), (a3, b3), (a4, b4), (a5, b5)</code> until they're not equal.
     * <code>0</code> is returned if all of the pairs of values are equal.
     * 
     * @param a1
     * @param b1
     * @param a2
     * @param b2
     * @param a3
     * @param b3
     * @param a4
     * @param b4
     * @param a5
     * @param b5
     * @return
     */
    public static <T1 extends Comparable<T1>, T2 extends Comparable<T2>, T3 extends Comparable<T3>, T4 extends Comparable<T4>, T5 extends Comparable<T5>> int compare(
            T1 a1, T1 b1, T2 a2, T2 b2, T3 a3, T3 b3, T4 a4, T4 b4, T5 a5, T5 b5) {
        int res = N.compare(a1, b1);

        if (res != 0) {
            return res;
        }

        res = N.compare(a2, b2);

        if (res != 0) {
            return res;
        }

        res = N.compare(a3, b3);

        if (res != 0) {
            return res;
        }

        res = N.compare(a4, b4);

        if (res != 0) {
            return res;
        }

        return N.compare(a5, b5);
    }

    public static int compare(final boolean[] a, final boolean[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if (a[i] != b[i]) {
                return a[i] ? 1 : -1;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final char[] a, final char[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if (a[i] != b[i]) {
                return a[i] > b[i] ? 1 : -1;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final byte[] a, final byte[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if (a[i] != b[i]) {
                return a[i] > b[i] ? 1 : -1;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final short[] a, final short[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if (a[i] != b[i]) {
                return a[i] > b[i] ? 1 : -1;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final int[] a, final int[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if (a[i] != b[i]) {
                return a[i] > b[i] ? 1 : -1;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final long[] a, final long[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if (a[i] != b[i]) {
                return a[i] > b[i] ? 1 : -1;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final float[] a, final float[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        int value = 0;

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if ((value = Float.compare(a[i], b[i])) != 0) {
                return value;
            }
        }

        return a.length - b.length;
    }

    public static int compare(final double[] a, final double[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        int value = 0;

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if ((value = Double.compare(a[i], b[i])) != 0) {
                return value;
            }
        }

        return a.length - b.length;
    }

    public static <T extends Comparable<? super T>> int compare(final T[] a, final T[] b) {
        final Comparator<T> cmp = NULL_MIN_COMPARATOR;
        return compare(a, b, cmp);
    }

    public static <T> int compare(final T[] a, final T[] b, Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        cmp = cmp == null ? NULL_MIN_COMPARATOR : cmp;

        int value = 0;

        for (int i = 0, minLen = min(a.length, b.length); i < minLen; i++) {
            if ((value = cmp.compare(a[i], b[i])) != 0) {
                return value;
            }
        }

        return a.length - b.length;
    }

    public static <T extends Comparable<? super T>> int compare(final Collection<T> a, final Collection<T> b) {
        final Comparator<T> cmp = NULL_MIN_COMPARATOR;
        return compare(a, b, cmp);
    }

    public static <T> int compare(final Collection<T> a, final Collection<T> b, Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? 0 : -1;
        } else if (N.isNullOrEmpty(b)) {
            return 1;
        }

        cmp = cmp == null ? NULL_MIN_COMPARATOR : cmp;

        final Iterator<T> iterA = a.iterator();
        final Iterator<T> iterB = b.iterator();
        int value = 0;

        for (int i = 0, minLen = min(a.size(), b.size()); i < minLen; i++) {
            if ((value = cmp.compare(iterA.next(), iterB.next())) != 0) {
                return value;
            }
        }

        return a.size() - b.size();
    }

    public static int compareIgnoreCase(final String a, final String b) {
        return a == null ? (b == null ? 0 : -1) : (b == null ? 1 : a.compareToIgnoreCase(b));
    }

    // Abbreviating
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Abbreviates a String using ellipses. This will turn
     * "Now is the time for all good men" into "Now is the time for..."
     * </p>
     *
     * <p>
     * Specifically:
     * </p>
     * <ul>
     * <li>If {@code str} is less than or equals to {@code maxWidth} characters
     * long, return it.</li>
     * <li>Else abbreviate it to {@code (substring(str, 0, max-3) + "...")}.</li>
     * <li>If {@code maxWidth} is less than {@code 4}, throw an
     * {@code IllegalArgumentException}.</li>
     * <li>In no case will it return a String of length greater than
     * {@code maxWidth}.</li>
     * </ul>
     *
     * <pre>
     * N.abbreviate(null, *)      = null
     * N.abbreviate("", 4)        = ""
     * N.abbreviate("abcdefg", 6) = "abc..."
     * N.abbreviate("abcdefg", 7) = "abcdefg"
     * N.abbreviate("abcdefg", 8) = "abcdefg"
     * N.abbreviate("abcdefg", 4) = "a..."
     * N.abbreviate("abcdefg", 3) = IllegalArgumentException
     * </pre>
     *
     * @param str
     *            the String to check, may be null
     * @param maxWidth
     *            maximum length of result String, must be at least 4
     * @return abbreviated String, {@code ""} if null or "" String input
     * @throws IllegalArgumentException
     *             if the width is too small
     * @since 2.0
     */
    public static String abbreviate(final String str, final int maxWidth) {
        if (maxWidth < 4) {
            throw new IllegalArgumentException("Minimum abbreviation width is 4");
        }

        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return str.length() <= maxWidth ? str : str.substring(0, maxWidth - 3) + "...";
    }

    public static String reverse(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            sb.append(str);

            return sb.reverse().toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    /**
     * <p>
     * Reverses a String that is delimited by a specific character.
     * </p>
     *
     * <p>
     * The Strings between the delimiters are not reversed. Thus
     * java.lang.String becomes String.lang.java (if the delimiter is
     * {@code '.'}).
     * </p>
     *
     * <pre>
     * N.reverseDelimited(null, *)      = null
     * N.reverseDelimited("", *)        = ""
     * N.reverseDelimited("a.b.c", 'x') = "a.b.c"
     * N.reverseDelimited("a.b.c", ".") = "c.b.a"
     * </pre>
     *
     * @param str
     *            the String to reverse, may be null
     * @param delimiter
     *            the delimiter character to use
     * @return the reversed String, {@code null} if null String input
     * @since 2.0
     */
    public static String reverseDelimited(final String str, final char delimiter) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        // could implement manually, but simple way is to reuse other,
        // probably slower, methods.
        final String[] strs = N.split(str, delimiter);

        N.reverse(strs);

        return N.join(strs, delimiter);
    }

    public static String reverseDelimited(final String str, final String delimiter) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        // could implement manually, but simple way is to reuse other,
        // probably slower, methods.
        final String[] strs = N.split(str, delimiter);

        N.reverse(strs);

        return Joiner.with(delimiter).join(strs).toString();
    }

    public static String padStart(final String str, final int minLength) {
        return padStart(str, minLength, D._SPACE);
    }

    /**
     *
     * @param str
     * @param minLength
     * @param padChar
     * @return
     */
    public static String padStart(String str, final int minLength, final char padChar) {
        if (str == null) {
            str = N.EMPTY_STRING;
        } else if (str.length() >= minLength) {
            return str;
        }

        int delta = minLength - str.length();
        final char[] chars = new char[minLength];

        N.fill(chars, 0, delta, padChar);

        str.getChars(0, str.length(), chars, delta);

        return N.newString(chars, true);
    }

    public static String padStart(String str, final int minLength, final String padStr) {
        if (str == null) {
            str = N.EMPTY_STRING;
        } else if (str.length() >= minLength) {
            return str;
        }

        int delta = ((minLength - str.length()) % padStr.length() == 0) ? ((minLength - str.length()) / padStr.length())
                : ((minLength - str.length()) % padStr.length() + 1);
        switch (delta) {
            case 1:
                return padStr + str;

            case 2:
                return padStr + padStr + str;

            case 3:
                return padStr + padStr + padStr + str;

            default: {
                final StringBuilder sb = ObjectFactory.createStringBuilder();

                try {
                    for (int i = 0; i < delta; i++) {
                        sb.append(padStr);
                    }

                    sb.append(str);

                    return sb.toString();
                } finally {
                    ObjectFactory.recycle(sb);
                }
            }
        }
    }

    public static String padEnd(final String str, final int minLength) {
        return padEnd(str, minLength, D._SPACE);
    }

    /**
     *
     * @param str
     * @param minLength
     * @param padChar
     * @return
     */
    public static String padEnd(String str, final int minLength, final char padChar) {
        if (str == null) {
            str = N.EMPTY_STRING;
        } else if (str.length() >= minLength) {
            return str;
        }

        final char[] chars = new char[minLength];
        str.getChars(0, str.length(), chars, 0);

        N.fill(chars, str.length(), minLength, padChar);

        return N.newString(chars, true);
    }

    public static String padEnd(String str, final int minLength, final String padStr) {
        if (str == null) {
            str = N.EMPTY_STRING;
        } else if (str.length() >= minLength) {
            return str;
        }

        int delta = ((minLength - str.length()) % padStr.length() == 0) ? ((minLength - str.length()) / padStr.length())
                : ((minLength - str.length()) % padStr.length() + 1);

        switch (delta) {
            case 1:
                return str + padStr;

            case 2:
                return str + padStr + padStr;

            case 3:
                return str + padStr + padStr + padStr;

            default: {
                StringBuilder sb = ObjectFactory.createStringBuilder();

                try {
                    sb.append(str);

                    for (int i = 0; i < delta; i++) {
                        sb.append(padStr);
                    }

                    return sb.toString();
                } finally {
                    ObjectFactory.recycle(sb);
                }
            }
        }
    }

    public static String repeat(final char ch, final int repeat) {
        if (repeat < 1) {
            throw new IllegalArgumentException("The specified count must be greater than 0");
        }

        if (repeat == 1) {
            return String.valueOf(ch);
        }

        if (repeat < 16) {
            final char[] array = new char[repeat];
            Arrays.fill(array, ch);

            return N.newString(array, true);
        } else {
            final char[] array = new char[repeat];
            array[0] = ch;

            int n = 1;

            for (; n < repeat - n; n <<= 1) {
                copy(array, 0, array, n, n);
            }

            if (n < repeat) {
                copy(array, 0, array, n, repeat - n);
            }

            return N.newString(array, true);
        }
    }

    public static String repeat(final char ch, final int repeat, final char delimiter) {
        if (repeat < 1) {
            throw new IllegalArgumentException("The specified count must be greater than 0");
        }

        return repeat(String.valueOf(ch), repeat, String.valueOf(delimiter));
    }

    /**
     *
     * @param str
     * @param repeat
     * @return
     */
    public static String repeat(final String str, final int repeat) {
        return repeat(str, repeat, N.EMPTY_STRING);
    }

    public static String repeat(final String str, final int repeat, final String delimiter) {
        if (N.isNullOrEmpty(str)) {
            throw new IllegalArgumentException("The specified String can't be null or empty");
        }

        if (delimiter == null) {
            throw new IllegalArgumentException("The specified delimiter can't be null");
        }

        if (repeat < 1) {
            throw new IllegalArgumentException("The specified count must be greater than 0");
        }

        if (repeat == 1) {
            return str;
        }

        final int strLen = str.length();
        final int delimiterLen = delimiter.length();
        final int len = strLen + delimiterLen;
        if (Integer.MAX_VALUE / len < repeat) {
            throw new ArrayIndexOutOfBoundsException("Required array size too large: " + 1L * len * repeat);
        }

        final int size = len * repeat - delimiterLen;
        final char[] cbuf = new char[size];

        str.getChars(0, strLen, cbuf, 0);
        delimiter.getChars(0, delimiterLen, cbuf, strLen);

        int n = 0;

        for (n = len; n < size - n; n <<= 1) {
            copy(cbuf, 0, cbuf, n, n);
        }

        if (n < size) {
            copy(cbuf, 0, cbuf, n, size - n);
        }

        return newString(cbuf, true);
    }

    public static char toLowerCase(final char ch) {
        return Character.toLowerCase(ch);
    }

    /**
     * <p>
     * Converts a String to lower case as per {@link String#toLowerCase()}.
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}.
     * </p>
     *
     * <pre>
     * N.toLowerCase(null)  = null
     * N.toLowerCase("")    = ""
     * N.toLowerCase("aBc") = "abc"
     * </pre>
     *
     * <p>
     * <strong>Note:</strong> As described in the documentation for
     * {@link String#toLowerCase()}, the result of this method is affected by
     * the current locale. For platform-independent case transformations, the
     * method {@link #toLowerCase(String, Locale)} should be used with a specific
     * locale (e.g. {@link Locale#ENGLISH}).
     * </p>
     *
     * @param str
     *            the String to lower case, may be null
     * @return the lower cased String, {@code null} if null String input
     */
    public static String toLowerCase(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return str.toLowerCase();
    }

    /**
     * <p>
     * Converts a String to lower case as per {@link String#toLowerCase(Locale)}
     * .
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}.
     * </p>
     *
     * <pre>
     * N.toLowerCase(null, Locale.ENGLISH)  = null
     * N.toLowerCase("", Locale.ENGLISH)    = ""
     * N.toLowerCase("aBc", Locale.ENGLISH) = "abc"
     * </pre>
     *
     * @param str
     *            the String to lower case, may be null
     * @param locale
     *            the locale that defines the case transformation rules, must
     *            not be null
     * @return the lower cased String, {@code null} if null String input
     * @since 2.5
     */
    public static String toLowerCase(final String str, final Locale locale) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return str.toLowerCase(locale);
    }

    public static String toLowerCaseWithUnderscore(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = 0, len = str.length(); i < len; i++) {
                char ch = str.charAt(i);

                if (Character.isUpperCase(ch)) {
                    if (i > 0 && (Character.isLowerCase(str.charAt(i - 1)) || (i < len - 1 && Character.isLowerCase(str.charAt(i + 1))))) {
                        sb.append(D._UNDERSCORE);
                    }

                    sb.append(Character.toLowerCase(ch));
                } else {
                    sb.append(ch);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static char toUpperCase(final char ch) {
        return Character.toUpperCase(ch);
    }

    // Case conversion
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Converts a String to upper case as per {@link String#toUpperCase()}.
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}.
     * </p>
     *
     * <pre>
     * N.toUpperCase(null)  = null
     * N.toUpperCase("")    = ""
     * N.toUpperCase("aBc") = "ABC"
     * </pre>
     *
     * <p>
     * <strong>Note:</strong> As described in the documentation for
     * {@link String#toUpperCase()}, the result of this method is affected by
     * the current locale. For platform-independent case transformations, the
     * method {@link #toLowerCase(String, Locale)} should be used with a specific
     * locale (e.g. {@link Locale#ENGLISH}).
     * </p>
     *
     * @param str
     *            the String to upper case, may be null
     * @return the upper cased String, {@code null} if null String input
     */
    public static String toUpperCase(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return str.toUpperCase();
    }

    /**
     * <p>
     * Converts a String to upper case as per {@link String#toUpperCase(Locale)}
     * .
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}.
     * </p>
     *
     * <pre>
     * N.toUpperCase(null, Locale.ENGLISH)  = null
     * N.toUpperCase("", Locale.ENGLISH)    = ""
     * N.toUpperCase("aBc", Locale.ENGLISH) = "ABC"
     * </pre>
     *
     * @param str
     *            the String to upper case, may be null
     * @param locale
     *            the locale that defines the case transformation rules, must
     *            not be null
     * @return the upper cased String, {@code null} if null String input
     * @since 2.5
     */
    public static String toUpperCase(final String str, final Locale locale) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return str.toUpperCase(locale);
    }

    public static String toUpperCaseWithUnderscore(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = 0, len = str.length(); i < len; i++) {
                char ch = str.charAt(i);

                if (Character.isUpperCase(ch)) {
                    if (i > 0 && (Character.isLowerCase(str.charAt(i - 1)) || (i < len - 1 && Character.isLowerCase(str.charAt(i + 1))))) {
                        sb.append(D._UNDERSCORE);
                    }

                    sb.append(ch);
                } else {
                    sb.append(Character.toUpperCase(ch));
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static char swapCase(final char ch) {
        return Character.isLowerCase(ch) ? Character.toUpperCase(ch) : Character.toLowerCase(ch);
    }

    /**
     * <p>
     * Swaps the case of a String changing upper and title case to lower case,
     * and lower case to upper case.
     * </p>
     *
     * <ul>
     * <li>Upper case character converts to Lower case</li>
     * <li>Title case character converts to Lower case</li>
     * <li>Lower case character converts to Upper case</li>
     * </ul>
     *
     * <p>
     * For a word based algorithm, see
     * {@link org.apache.commons.lang3.text.WordUtils#swapCase(String)}. A
     * {@code null} input String returns {@code null}.
     * </p>
     *
     * <pre>
     * N.swapCase(null)                 = null
     * N.swapCase("")                   = ""
     * N.swapCase("The dog has a BONE") = "tHE DOG HAS A bone"
     * </pre>
     *
     * <p>
     * NOTE: This method changed in Lang version 2.0. It no longer performs a
     * word based algorithm. If you only use ASCII, you will notice no change.
     * That functionality is available in
     * org.apache.commons.lang3.text.WordUtils.
     * </p>
     *
     * @param str
     *            the String to swap case, may be null
     * @return the changed String, {@code null} if null String input
     */
    public static String swapCase(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final char[] cbuf = str.toCharArray();
        char ch = 0;
        for (int i = 0, len = cbuf.length; i < len; i++) {
            ch = cbuf[i];

            if (Character.isUpperCase(ch) || Character.isTitleCase(ch)) {
                cbuf[i] = Character.toLowerCase(ch);
            } else if (Character.isLowerCase(ch)) {
                cbuf[i] = Character.toUpperCase(ch);
            }
        }

        return newString(cbuf, true);
    }

    /**
     *
     * @param str
     * @return
     */
    public static String capitalize(final String str) {
        if (isNullOrEmpty(str)) {
            return str;
        }

        char ch = str.charAt(0);

        if (Character.isTitleCase(ch)) {
            return str;
        }

        if (str.length() == 1) {
            return String.valueOf(Character.toTitleCase(ch));
        } else {
            return Character.toTitleCase(ch) + str.substring(1);
        }
    }

    /**
     *
     * @param str
     * @return
     */
    public static String uncapitalize(final String str) {
        if (isNullOrEmpty(str)) {
            return str;
        }

        char ch = str.charAt(0);

        if (Character.isLowerCase(ch)) {
            return str;
        }

        if (str.length() == 1) {
            return String.valueOf(Character.toLowerCase(ch));
        } else {
            return Character.toLowerCase(ch) + str.substring(1);
        }
    }

    /**
     * Replace ''' or '"' with '\'' or '\"' if the previous char of the
     * quotation is not '\'. original String is returned if the specified String
     * is {@code null} or empty.
     *
     * @param str
     * @return
     */
    public static String quoteEscaped(final String str) {
        if (isNullOrEmpty(str)) {
            return str;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();
        final char[] chars = N.getCharsForReadOnly(str);

        try {
            char ch = 0;
            for (int i = 0, len = str.length(); i < len; i++) {
                ch = chars[i];

                if ((ch == _BACKSLASH) && (i < (len - 1))) {
                    sb.append(ch);
                    sb.append(str.charAt(++i));
                } else if ((ch == _QUOTATION_S) || (ch == _QUOTATION_D)) {
                    sb.append(_BACKSLASH);
                    sb.append(ch);
                } else {
                    sb.append(ch);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    // --------------------------------------------------------------------------
    /**
     * <p>
     * Converts the char to the unicode format '\u0020'.
     * </p>
     *
     * <p>
     * This format is the Java source code format.
     * </p>
     *
     * <pre>
     *   CharUtils.unicodeEscaped(' ') = "\u0020"
     *   CharUtils.unicodeEscaped('A') = "\u0041"
     * </pre>
     *
     * @param ch
     *            the character to convert
     * @return the escaped Unicode string
     */
    public static String unicodeEscaped(final char ch) {
        if (ch < 0x10) {
            return "\\u000" + Integer.toHexString(ch);
        } else if (ch < 0x100) {
            return "\\u00" + Integer.toHexString(ch);
        } else if (ch < 0x1000) {
            return "\\u0" + Integer.toHexString(ch);
        }

        return "\\u" + Integer.toHexString(ch);
    }

    /**
     * <p>
     * Similar to <a
     * href="http://www.w3.org/TR/xpath/#function-normalize-space">
     * http://www.w3.org/TR/xpath/#function-normalize -space</a>
     * </p>
     * <p>
     * The function returns the argument string with whitespace normalized by
     * using <code>{@link #trim(String)}</code> to remove leading and trailing
     * whitespace and then replacing sequences of whitespace characters by a
     * single space.
     * </p>
     * In XML Whitespace characters are the same as those allowed by the <a
     * href="http://www.w3.org/TR/REC-xml/#NT-S">S</a> production, which is S
     * ::= (#x20 | #x9 | #xD | #xA)+
     * <p>
     * Java's regexp pattern \s defines whitespace as [ \t\n\x0B\f\r]
     *
     * <p>
     * For reference:
     * </p>
     * <ul>
     * <li>\x0B = vertical tab</li>
     * <li>\f = #xC = form feed</li>
     * <li>#x20 = space</li>
     * <li>#x9 = \t</li>
     * <li>#xA = \n</li>
     * <li>#xD = \r</li>
     * </ul>
     *
     * <p>
     * The difference is that Java's whitespace includes vertical tab and form
     * feed, which this functional will also normalize. Additionally
     * <code>{@link #trim(String)}</code> removes control characters (char &lt;=
     * 32) from both ends of this String.
     * </p>
     *
     * @see Pattern
     * @see #trim(String)
     * @see <a
     *      href="http://www.w3.org/TR/xpath/#function-normalize-space">http://www.w3.org/TR/xpath/#function-normalize-space</a>
     * @param str
     *            the source String to normalize whitespaces from, may be null
     * @return the modified string with whitespace normalized, {@code null} if
     *         null String input
     *
     * @since 3.0
     */
    public static String normalizeSpace(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return WHITESPACE_PATTERN.matcher(str.trim()).replaceAll(D.SPACE);
    }

    /**
     * <p>
     * Replaces all occurrences of a String within another String.
     * </p>
     *
     * <p>
     * A {@code null} reference passed to this method is a no-op.
     * </p>
     *
     * <pre>
     * N.replace(null, *, *)        = null
     * N.replace("", *, *)          = ""
     * N.replace("any", null, *)    = "any"
     * N.replace("any", *, null)    = "any"
     * N.replace("any", "", *)      = "any"
     * N.replace("aba", "a", null)  = "aba"
     * N.replace("aba", "a", "")    = "b"
     * N.replace("aba", "a", "z")   = "zbz"
     * </pre>
     *
     * @see #replaceAll(String text, String searchString, String replacement,
     *      int max)
     * @param str
     *            text to search and replace in, may be null
     * @param substr
     *            the String to search for, may be null
     * @param replacement
     *            the String to replace it with, may be null
     * @return the text with any replacements processed, {@code null} if null
     *         String input
     */
    public static String replaceAll(final String str, final String substr, final String replacement) {
        return replaceAll(str, 0, substr, replacement);
    }

    /**
     * <p>
     * Replaces a String with another String inside a larger String, for the
     * first {@code max} values of the search String.
     * </p>
     *
     * <p>
     * A {@code null} reference passed to this method is a no-op.
     * </p>
     *
     * <pre>
     * N.replace(null, *, *, *)         = null
     * N.replace("", *, *, *)           = ""
     * N.replace("any", null, *, *)     = "any"
     * N.replace("any", "", *, *)       = "any"
     * N.replace("any", *, *, 0)        = "any"
     * N.replace("abaa", "a", null, -1) = "abaa"
     * N.replace("abaa", "a", "", -1)   = "b"
     * N.replace("abaa", "a", "z", 0)   = "abaa"
     * N.replace("abaa", "a", "z", 1)   = "zbaa"
     * N.replace("abaa", "a", "z", 2)   = "zbza"
     * N.replace("abaa", "a", "z", -1)  = "zbzz"
     * </pre>
     *
     * @param str
     *            text to search and replace in, may be null
     * @param fromIndex
     * @param substr
     *            the String to search for, may be null
     * @param replacement
     *            the String to replace it with, can't be null
     * @param max
     *            maximum number of values to replace, or {@code -1} if no
     *            maximum
     * @return the text with any replacements processed, {@code null} if null
     *         String input
     */

    public static String replaceAll(final String str, final int fromIndex, final String substr, final String replacement) {
        return replace(str, fromIndex, substr, replacement, -1);
    }

    public static String replace(final String str, final int fromIndex, final String substr, final String replacement, int max) {
        if (replacement == null) {
            throw new IllegalArgumentException("Replacement can't be null");
        }

        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || max == 0) {
            return str;
        }

        int start = fromIndex;
        int end = str.indexOf(substr, start);
        if (end == N.INDEX_NOT_FOUND) {
            return str;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();
        final int substrLength = substr.length();

        try {
            while (end != N.INDEX_NOT_FOUND) {
                sb.append(str.substring(start, end)).append(replacement);
                start = end + substrLength;

                if (--max == 0) {
                    break;
                }

                end = str.indexOf(substr, start);
            }

            sb.append(str.substring(start));

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    /**
     * Replaces each substring of the source String that matches the given
     * regular expression with the given replacement using the
     * {@link Pattern#DOTALL} option. DOTALL is also know as single-line mode in
     * Perl. This call is also equivalent to:
     * <ul>
     * <li>{@code source.replaceAll(&quot;(?s)&quot; + regex, replacement)}</li>
     * <li>
     * {@code Pattern.compile(regex, Pattern.DOTALL).filter(source).replaceAll(replacement)}
     * </li>
     * </ul>
     *
     * @param source
     *            the source string
     * @param regex
     *            the regular expression to which this string is to be matched
     * @param replacement
     *            the string to be substituted for each match
     * @return The resulting {@code String}
     * @see String#replaceAll(String, String)
     * @see Pattern#DOTALL
     * @since 3.2
     */
    public static String replacePattern(final String source, final String regex, final String replacement) {
        return Pattern.compile(regex, Pattern.DOTALL).matcher(source).replaceAll(replacement);
    }

    // Remove
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Removes a substring only if it is at the beginning of a source string,
     * otherwise returns the source string.
     * </p>
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("")
     * source string will return the empty string. A {@code null} search string
     * will return the source string.
     * </p>
     *
     * <pre>
     * N.removeStart(null, *)      = null
     * N.removeStart("", *)        = ""
     * N.removeStart(*, null)      = *
     * N.removeStart("www.domain.com", "www.")   = "domain.com"
     * N.removeStart("domain.com", "www.")       = "domain.com"
     * N.removeStart("www.domain.com", "domain") = "www.domain.com"
     * N.removeStart("abc", "")    = "abc"
     * </pre>
     *
     * @param str
     *            the source String to search, may be null
     * @param removeStr
     *            the String to search for and remove, may be null
     * @return the substring with the string removed if found, {@code null} if
     *         null String input
     * @since 2.1
     */
    public static String removeStart(final String str, final String removeStr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(removeStr)) {
            return str;
        }

        if (str.startsWith(removeStr)) {
            return str.substring(removeStr.length());
        }

        return str;
    }

    /**
     * <p>
     * Case insensitive removal of a substring if it is at the beginning of a
     * source string, otherwise returns the source string.
     * </p>
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("")
     * source string will return the empty string. A {@code null} search string
     * will return the source string.
     * </p>
     *
     * <pre>
     * N.removeStartIgnoreCase(null, *)      = null
     * N.removeStartIgnoreCase("", *)        = ""
     * N.removeStartIgnoreCase(*, null)      = *
     * N.removeStartIgnoreCase("www.domain.com", "www.")   = "domain.com"
     * N.removeStartIgnoreCase("www.domain.com", "WWW.")   = "domain.com"
     * N.removeStartIgnoreCase("domain.com", "www.")       = "domain.com"
     * N.removeStartIgnoreCase("www.domain.com", "domain") = "www.domain.com"
     * N.removeStartIgnoreCase("abc", "")    = "abc"
     * </pre>
     *
     * @param str
     *            the source String to search, may be null
     * @param removeStr
     *            the String to search for (case insensitive) and remove, may be
     *            null
     * @return the substring with the string removed if found, {@code null} if
     *         null String input
     * @since 2.4
     */
    public static String removeStartIgnoreCase(final String str, final String removeStr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(removeStr)) {
            return str;
        }

        if (startsWithIgnoreCase(str, removeStr)) {
            return str.substring(removeStr.length());
        }

        return str;
    }

    /**
     * <p>
     * Removes a substring only if it is at the end of a source string,
     * otherwise returns the source string.
     * </p>
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("")
     * source string will return the empty string. A {@code null} search string
     * will return the source string.
     * </p>
     *
     * <pre>
     * N.removeEnd(null, *)      = null
     * N.removeEnd("", *)        = ""
     * N.removeEnd(*, null)      = *
     * N.removeEnd("www.domain.com", ".com.")  = "www.domain.com"
     * N.removeEnd("www.domain.com", ".com")   = "www.domain"
     * N.removeEnd("www.domain.com", "domain") = "www.domain.com"
     * N.removeEnd("abc", "")    = "abc"
     * </pre>
     *
     * @param str
     *            the source String to search, may be null
     * @param removeStr
     *            the String to search for and remove, may be null
     * @return the substring with the string removed if found, {@code null} if
     *         null String input
     * @since 2.1
     */
    public static String removeEnd(final String str, final String removeStr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(removeStr)) {
            return str;
        }

        if (str.endsWith(removeStr)) {
            return str.substring(0, str.length() - removeStr.length());
        }

        return str;
    }

    /**
     * <p>
     * Case insensitive removal of a substring if it is at the end of a source
     * string, otherwise returns the source string.
     * </p>
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("")
     * source string will return the empty string. A {@code null} search string
     * will return the source string.
     * </p>
     *
     * <pre>
     * N.removeEndIgnoreCase(null, *)      = null
     * N.removeEndIgnoreCase("", *)        = ""
     * N.removeEndIgnoreCase(*, null)      = *
     * N.removeEndIgnoreCase("www.domain.com", ".com.")  = "www.domain.com"
     * N.removeEndIgnoreCase("www.domain.com", ".com")   = "www.domain"
     * N.removeEndIgnoreCase("www.domain.com", "domain") = "www.domain.com"
     * N.removeEndIgnoreCase("abc", "")    = "abc"
     * N.removeEndIgnoreCase("www.domain.com", ".COM") = "www.domain")
     * N.removeEndIgnoreCase("www.domain.COM", ".com") = "www.domain")
     * </pre>
     *
     * @param str
     *            the source String to search, may be null
     * @param removeStr
     *            the String to search for (case insensitive) and remove, may be
     *            null
     * @return the substring with the string removed if found, {@code null} if
     *         null String input
     * @since 2.4
     */
    public static String removeEndIgnoreCase(final String str, final String removeStr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(removeStr)) {
            return str;
        }

        if (endsWithIgnoreCase(str, removeStr)) {
            return str.substring(0, str.length() - removeStr.length());
        }

        return str;
    }

    /**
     * <p>
     * Removes all occurrences of a character from within the source string.
     * </p>
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("")
     * source string will return the empty string.
     * </p>
     *
     * <pre>
     * N.remove(null, *)       = null
     * N.remove("", *)         = ""
     * N.remove("queued", 'u') = "qeed"
     * N.remove("queued", 'z') = "queued"
     * </pre>
     *
     * @param str
     *            the source String to search, may be null
     * @param removeChar
     *            the char to search for and remove, may be null
     * @return the substring with the char removed if found, {@code null} if
     *         null String input
     * @since 2.1
     */
    public static String removeAll(final String str, final char removeChar) {
        return removeAll(str, 0, removeChar);
    }

    public static String removeAll(final String str, final int fromIndex, final char removeChar) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        int index = str.indexOf(removeChar, fromIndex);
        if (index == N.INDEX_NOT_FOUND) {
            return str;
        } else {
            final char[] chars = N.getCharsForReadOnly(str);
            final char[] cbuf = new char[str.length()];

            if (index > 0) {
                str.getChars(0, index, cbuf, 0);
            }

            int count = index;
            for (int i = index + 1, len = chars.length; i < len; i++) {
                if (chars[i] != removeChar) {
                    cbuf[count++] = chars[i];
                }
            }

            return count == chars.length ? str : new String(cbuf, 0, count);
        }
    }

    /**
     * <p>
     * Removes all occurrences of a substring from within the source string.
     * </p>
     *
     * <p>
     * A {@code null} source string will return {@code null}. An empty ("")
     * source string will return the empty string. A {@code null} remove string
     * will return the source string. An empty ("") remove string will return
     * the source string.
     * </p>
     *
     * <pre>
     * N.remove(null, *)        = null
     * N.remove("", *)          = ""
     * N.remove(*, null)        = *
     * N.remove(*, "")          = *
     * N.remove("queued", "ue") = "qd"
     * N.remove("queued", "zz") = "queued"
     * </pre>
     *
     * @param str
     *            the source String to search, may be null
     * @param removeStr
     *            the String to search for and remove, may be null
     * @return the substring with the string removed if found, {@code null} if
     *         null String input
     * @since 2.1
     */
    public static String removeAll(final String str, final String removeStr) {
        return removeAll(str, 0, removeStr);
    }

    public static String removeAll(final String str, final int fromIndex, final String removeStr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(removeStr)) {
            return str;
        }

        return replace(str, fromIndex, removeStr, N.EMPTY_STRING, -1);
    }

    /**
     * Removes each substring of the source String that matches the given
     * regular expression using the DOTALL option.
     *
     * @param source
     *            the source string
     * @param regex
     *            the regular expression to which this string is to be matched
     * @return The resulting {@code String}
     * @see String#replaceAll(String, String)
     * @see Pattern#DOTALL
     * @since 3.2
     */
    public static String removePattern(final String source, final String regex) {
        return replacePattern(source, regex, N.EMPTY_STRING);
    }

    public static String[] split(final String str, final char delimiter) {
        return splitWorker(str, delimiter, false);
    }

    public static String[] split(final String str, final char delimiter, final boolean trim) {
        final String[] strs = split(str, delimiter);

        if (trim && N.notNullOrEmpty(strs)) {
            for (int i = 0, len = strs.length; i < len; i++) {
                strs[i] = strs[i].trim();
            }
        }

        return strs;
    }

    public static String[] split(final String str, final String delimiter) {
        return split(str, delimiter, false);
    }

    public static String[] split(final String str, final String delimiter, final boolean trim) {
        return split(str, delimiter, Integer.MAX_VALUE, trim);
    }

    public static String[] split(final String str, final String delimiter, final int max) {
        return splitWorker(str, delimiter, max, false);
    }

    public static String[] split(final String str, final String delimiter, final int max, final boolean trim) {
        final String[] strs = split(str, delimiter, max);

        if (trim && N.notNullOrEmpty(strs)) {
            for (int i = 0, len = strs.length; i < len; i++) {
                strs[i] = strs[i].trim();
            }
        }

        return strs;
    }

    public static String[] splitPreserveAllTokens(final String str, final char delimiter) {
        return splitPreserveAllTokens(str, delimiter, false);
    }

    public static String[] splitPreserveAllTokens(final String str, final char delimiter, boolean trim) {
        final String[] strs = splitWorker(str, delimiter, true);

        if (trim && N.notNullOrEmpty(strs)) {
            for (int i = 0, len = strs.length; i < len; i++) {
                strs[i] = strs[i].trim();
            }
        }

        return strs;
    }

    public static String[] splitPreserveAllTokens(final String str, final String delimiter) {
        return splitPreserveAllTokens(str, delimiter, false);
    }

    public static String[] splitPreserveAllTokens(final String str, final String delimiter, boolean trim) {
        return splitPreserveAllTokens(str, delimiter, Integer.MAX_VALUE, trim);
    }

    public static String[] splitPreserveAllTokens(final String str, final String delimiter, final int max) {
        return splitPreserveAllTokens(str, delimiter, max, false);
    }

    public static String[] splitPreserveAllTokens(final String str, final String delimiter, final int max, boolean trim) {
        final String[] strs = splitWorker(str, delimiter, max, true);

        if (trim && N.notNullOrEmpty(strs)) {
            for (int i = 0, len = strs.length; i < len; i++) {
                strs[i] = strs[i].trim();
            }
        }

        return strs;
    }

    private static String[] splitWorker(final String str, final char delimiter, final boolean preserveAllTokens) {
        // Performance tuned for 2.0 (JDK1.4)

        //    if (str == null) {
        //        return null;
        //    }
        //
        //    final int len = str.length();
        //    if (len == 0) {
        //        return N.EMPTY_STRING_ARRAY;
        //    }

        if (N.isNullOrEmpty(str)) {
            return N.EMPTY_STRING_ARRAY;
        }

        final int len = str.length();
        final char[] chs = N.getCharsForReadOnly(str);
        final List<String> list = ObjectFactory.createList();

        try {
            int i = 0, start = 0;
            boolean match = false;
            boolean lastMatch = false;
            while (i < len) {
                if (chs[i] == delimiter) {
                    if (match || preserveAllTokens) {
                        list.add(str.substring(start, i));
                        match = false;
                        lastMatch = true;
                    }

                    start = ++i;
                    continue;
                }

                lastMatch = false;
                match = true;
                i++;
            }

            if (match || preserveAllTokens && lastMatch) {
                list.add(str.substring(start, i));
            }

            return list.toArray(new String[list.size()]);
        } finally {
            ObjectFactory.recycle(list);
        }
    }

    private static String[] splitWorker(final String str, final String delimiter, final int max, final boolean preserveAllTokens) {
        // Performance tuned for 2.0 (JDK1.4)
        // Direct code is quicker than StringTokenizer.
        // Also, StringTokenizer uses isSpace() not isWhitespace()

        //    if (str == null) {
        //        return null;
        //    }
        //
        //    final int len = str.length();
        //    if (len == 0) {
        //        return N.EMPTY_STRING_ARRAY;
        //    }

        if (N.isNullOrEmpty(str)) {
            return N.EMPTY_STRING_ARRAY;
        }

        final int len = str.length();
        final List<String> list = ObjectFactory.createList();
        int cnt = 1;
        int i = 0, start = 0;
        boolean match = false;
        boolean lastMatch = false;
        try {
            if (delimiter == null) {
                // Null delimiter means use whitespace
                final char[] chs = N.getCharsForReadOnly(str);
                while (i < len) {
                    if (Character.isWhitespace(chs[i])) {
                        if (match || preserveAllTokens) {
                            lastMatch = true;
                            if (cnt++ == max) {
                                i = len;
                                lastMatch = false;
                            }

                            list.add(str.substring(start, i));
                            match = false;
                        }

                        start = ++i;
                        continue;
                    }

                    lastMatch = false;
                    match = true;
                    i++;
                }

                if (match || preserveAllTokens && lastMatch) {
                    list.add(str.substring(start, i));
                }
            } else if (delimiter.length() == 1) {
                final char[] chs = N.getCharsForReadOnly(str);
                final char sep = delimiter.charAt(0);

                while (i < len) {
                    if (chs[i] == sep) {
                        if (match || preserveAllTokens) {
                            lastMatch = true;
                            if (cnt++ == max) {
                                i = len;
                                lastMatch = false;
                            }

                            list.add(str.substring(start, i));
                            match = false;
                        }

                        start = ++i;
                        continue;
                    }

                    lastMatch = false;
                    match = true;
                    i++;
                }

                if (match || preserveAllTokens && lastMatch) {
                    list.add(str.substring(start, i));
                }
            } else {
                final int delimiterLength = delimiter.length();
                int beginIndex = 0;
                int idx = 0;
                while (idx < len) {
                    idx = str.indexOf(delimiter, beginIndex);

                    if (idx > -1) {
                        if (idx > beginIndex) {
                            if (cnt++ == max) {
                                idx = len;
                                list.add(str.substring(beginIndex));
                            } else {
                                // The following is OK, because String.substring( beg, end ) excludes
                                // the character at the position 'end'.
                                list.add(str.substring(beginIndex, idx));

                                // Set the starting point for the next search.
                                // The following is equivalent to beg = end + (delimiterLength - 1) + 1,
                                // which is the right calculation:
                                beginIndex = idx + delimiterLength;
                            }
                        } else {
                            // We found a consecutive occurrence of the delimiter, so skip it.
                            if (preserveAllTokens) {
                                if (cnt++ == max) {
                                    idx = len;
                                    list.add(str.substring(beginIndex));
                                } else {
                                    list.add(N.EMPTY_STRING);
                                }
                            }
                            beginIndex = idx + delimiterLength;
                        }
                    } else {
                        // String.substring( beg ) goes from 'beg' to the end of the String.
                        list.add(str.substring(beginIndex));
                        idx = len;
                    }
                }
            }

            return list.toArray(new String[list.size()]);
        } finally {
            ObjectFactory.recycle(list);
        }
    }

    // Trim
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Removes control characters (char &lt;= 32) from both ends of this String,
     * handling {@code null} by returning {@code null}.
     * </p>
     *
     * <p>
     * The String is trimmed using {@link String#trim()}. Trim removes start and
     * end characters &lt;= 32. To strip whitespace use {@link #strip(String)}.
     * </p>
     *
     * <p>
     * To trim your choice of characters, use the {@link #strip(String, String)}
     * methods.
     * </p>
     *
     * <pre>
     * N.trim(null)          = null
     * N.trim("")            = ""
     * N.trim("     ")       = ""
     * N.trim("abc")         = "abc"
     * N.trim("    abc    ") = "abc"
     * </pre>
     *
     * @param str
     *            the String to be trimmed, may be null
     * @return the trimmed string, {@code null} if null String input
     */
    public static String trim(final String str) {
        return N.isNullOrEmpty(str) ? str : str.trim();
    }

    public static String[] trim(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = trim(strs[i]);
        }

        return res;
    }

    /**
     * <p>
     * Removes control characters (char &lt;= 32) from both ends of this String
     * returning {@code null} if the String is empty ("") after the trim or if
     * it is {@code null}.
     *
     * <p>
     * The String is trimmed using {@link String#trim()}. Trim removes start and
     * end characters &lt;= 32. To strip whitespace use
     * {@link #stripToNull(String)}.
     * </p>
     *
     * <pre>
     * N.trimToNull(null)          = null
     * N.trimToNull("")            = null
     * N.trimToNull("     ")       = null
     * N.trimToNull("abc")         = "abc"
     * N.trimToNull("    abc    ") = "abc"
     * </pre>
     *
     * @param str
     *            the String to be trimmed, may be null
     * @return the trimmed String, {@code null} if only chars &lt;= 32, empty or
     *         null String input
     * @since 2.0
     */
    public static String trimToNull(String str) {
        str = trim(str);

        return N.isNullOrEmpty(str) ? null : str;
    }

    public static String[] trimToNull(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = trimToNull(strs[i]);
        }

        return res;
    }

    /**
     * <p>
     * Removes control characters (char &lt;= 32) from both ends of this String
     * returning an empty String ("") if the String is empty ("") after the trim
     * or if it is {@code null}.
     *
     * <p>
     * The String is trimmed using {@link String#trim()}. Trim removes start and
     * end characters &lt;= 32. To strip whitespace use
     * {@link #stripToEmpty(String)}.
     * </p>
     *
     * <pre>
     * N.trimToEmpty(null)          = ""
     * N.trimToEmpty("")            = ""
     * N.trimToEmpty("     ")       = ""
     * N.trimToEmpty("abc")         = "abc"
     * N.trimToEmpty("    abc    ") = "abc"
     * </pre>
     *
     * @param str
     *            the String to be trimmed, may be null
     * @return the trimmed String, or an empty String if {@code null} input
     * @since 2.0
     */
    public static String trimToEmpty(final String str) {
        return N.isNullOrEmpty(str) ? N.EMPTY_STRING : str.trim();
    }

    public static String[] trimToEmpty(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = trimToEmpty(strs[i]);
        }

        return res;
    }

    // Stripping
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Strips whitespace from the start and end of a String.
     * </p>
     *
     * <p>
     * This is similar to {@link #trim(String)} but removes whitespace.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}.
     * </p>
     *
     * <pre>
     * N.strip(null)     = null
     * N.strip("")       = ""
     * N.strip("   ")    = ""
     * N.strip("abc")    = "abc"
     * N.strip("  abc")  = "abc"
     * N.strip("abc  ")  = "abc"
     * N.strip(" abc ")  = "abc"
     * N.strip(" ab c ") = "ab c"
     * </pre>
     *
     * @param str
     *            the String to remove whitespace from, may be null
     * @return the stripped String, {@code null} if null String input
     */
    public static String strip(final String str) {
        return strip(str, null);
    }

    public static String[] strip(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = strip(strs[i]);
        }

        return res;
    }

    /**
     * <p>
     * Strips whitespace from the start and end of a String returning
     * {@code null} if the String is empty ("") after the strip.
     * </p>
     *
     * <p>
     * This is similar to {@link #trimToNull(String)} but removes whitespace.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.
     * </p>
     *
     * <pre>
     * N.stripToNull(null)     = null
     * N.stripToNull("")       = null
     * N.stripToNull("   ")    = null
     * N.stripToNull("abc")    = "abc"
     * N.stripToNull("  abc")  = "abc"
     * N.stripToNull("abc  ")  = "abc"
     * N.stripToNull(" abc ")  = "abc"
     * N.stripToNull(" ab c ") = "ab c"
     * </pre>
     *
     * @param str
     *            the String to be stripped, may be null
     * @return the stripped String, {@code null} if whitespace, empty or null
     *         String input
     * @since 2.0
     */
    public static String stripToNull(String str) {
        str = strip(str, null);

        return N.isNullOrEmpty(str) ? null : str;
    }

    public static String[] stripToNull(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = stripToNull(strs[i]);
        }

        return res;
    }

    /**
     * <p>
     * Strips whitespace from the start and end of a String returning an empty
     * String if {@code null} input.
     * </p>
     *
     * <p>
     * This is similar to {@link #trimToEmpty(String)} but removes whitespace.
     * Whitespace is defined by {@link Character#isWhitespace(char)}.
     * </p>
     *
     * <pre>
     * N.stripToEmpty(null)     = ""
     * N.stripToEmpty("")       = ""
     * N.stripToEmpty("   ")    = ""
     * N.stripToEmpty("abc")    = "abc"
     * N.stripToEmpty("  abc")  = "abc"
     * N.stripToEmpty("abc  ")  = "abc"
     * N.stripToEmpty(" abc ")  = "abc"
     * N.stripToEmpty(" ab c ") = "ab c"
     * </pre>
     *
     * @param str
     *            the String to be stripped, may be null
     * @return the trimmed String, or an empty String if {@code null} input
     * @since 2.0
     */
    public static String stripToEmpty(final String str) {
        return N.isNullOrEmpty(str) ? N.EMPTY_STRING : strip(str, null);
    }

    public static String[] stripToEmpty(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = trimToEmpty(strs[i]);
        }

        return res;
    }

    /**
     * <p>
     * Strips any of a set of characters from the start and end of a String.
     * This is similar to {@link String#trim()} but allows the characters to be
     * stripped to be controlled.
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}. An empty string ("")
     * input returns the empty string.
     * </p>
     *
     * <p>
     * If the stripChars String is {@code null}, whitespace is stripped as
     * defined by {@link Character#isWhitespace(char)}. Alternatively use
     * {@link #strip(String)}.
     * </p>
     *
     * <pre>
     * N.strip(null, *)          = null
     * N.strip("", *)            = ""
     * N.strip("abc", null)      = "abc"
     * N.strip("  abc", null)    = "abc"
     * N.strip("abc  ", null)    = "abc"
     * N.strip(" abc ", null)    = "abc"
     * N.strip("  abcyx", "xyz") = "  abc"
     * </pre>
     *
     * @param str
     *            the String to remove characters from, may be null
     * @param stripChars
     *            the characters to remove, null treated as whitespace
     * @return the stripped String, {@code null} if null String input
     */
    public static String strip(final String str, final String stripChars) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        return stripEnd(stripStart(str, stripChars), stripChars);
    }

    public static String[] strip(final String[] strs, final String stripChars) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = strip(strs[i], stripChars);
        }

        return res;
    }

    /**
     * <p>
     * Strips any of a set of characters from the start of a String.
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}. An empty string ("")
     * input returns the empty string.
     * </p>
     *
     * <p>
     * If the stripChars String is {@code null}, whitespace is stripped as
     * defined by {@link Character#isWhitespace(char)}.
     * </p>
     *
     * <pre>
     * N.stripStart(null, *)          = null
     * N.stripStart("", *)            = ""
     * N.stripStart("abc", "")        = "abc"
     * N.stripStart("abc", null)      = "abc"
     * N.stripStart("  abc", null)    = "abc"
     * N.stripStart("abc  ", null)    = "abc  "
     * N.stripStart(" abc ", null)    = "abc "
     * N.stripStart("yxabc  ", "xyz") = "abc  "
     * </pre>
     *
     * @param str
     *            the String to remove characters from, may be null
     * @param stripChars
     *            the characters to remove, null treated as whitespace
     * @return the stripped String, {@code null} if null String input
     */
    public static String stripStart(final String str, final String stripChars) {
        if (N.isNullOrEmpty(str) || (stripChars != null && stripChars.isEmpty())) {
            return str;
        }

        final int strLen = str.length();
        int start = 0;
        if (stripChars == null) {
            while (start != strLen && Character.isWhitespace(str.charAt(start))) {
                start++;
            }
        } else {
            while (start != strLen && stripChars.indexOf(str.charAt(start)) != N.INDEX_NOT_FOUND) {
                start++;
            }
        }

        return start == 0 ? str : str.substring(start);
    }

    public static String[] stripStart(final String[] strs, final String stripChars) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = stripStart(strs[i], stripChars);
        }

        return res;
    }

    /**
     * <p>
     * Strips any of a set of characters from the end of a String.
     * </p>
     *
     * <p>
     * A {@code null} input String returns {@code null}. An empty string ("")
     * input returns the empty string.
     * </p>
     *
     * <p>
     * If the stripChars String is {@code null}, whitespace is stripped as
     * defined by {@link Character#isWhitespace(char)}.
     * </p>
     *
     * <pre>
     * N.stripEnd(null, *)          = null
     * N.stripEnd("", *)            = ""
     * N.stripEnd("abc", "")        = "abc"
     * N.stripEnd("abc", null)      = "abc"
     * N.stripEnd("  abc", null)    = "  abc"
     * N.stripEnd("abc  ", null)    = "abc"
     * N.stripEnd(" abc ", null)    = " abc"
     * N.stripEnd("  abcyx", "xyz") = "  abc"
     * N.stripEnd("120.00", ".0")   = "12"
     * </pre>
     *
     * @param str
     *            the String to remove characters from, may be null
     * @param stripChars
     *            the set of characters to remove, null treated as whitespace
     * @return the stripped String, {@code null} if null String input
     */
    public static String stripEnd(final String str, final String stripChars) {
        if (N.isNullOrEmpty(str) || (stripChars != null && stripChars.isEmpty())) {
            return str;
        }

        int end = str.length();

        if (stripChars == null) {
            while (end > 0 && Character.isWhitespace(str.charAt(end - 1))) {
                end--;
            }
        } else {
            while (end > 0 && stripChars.indexOf(str.charAt(end - 1)) != N.INDEX_NOT_FOUND) {
                end--;
            }
        }

        return end == str.length() ? str : str.substring(0, end);
    }

    public static String[] stripEnd(final String[] strs, final String stripChars) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = stripEnd(strs[i], stripChars);
        }

        return res;
    }

    /**
     * <p>
     * Removes diacritics (~= accents) from a string. The case will not be
     * altered.
     * </p>
     * <p>
     * For instance, '&agrave;' will be replaced by 'a'.
     * </p>
     * <p>
     * Note that ligatures will be left as is.
     * </p>
     *
     * <pre>
     * N.stripAccents(null)                = null
     * N.stripAccents("")                  = ""
     * N.stripAccents("control")           = "control"
     * N.stripAccents("&eacute;clair")     = "eclair"
     * </pre>
     *
     * @param strs
     *            String to be stripped
     * @return input text with diacritics removed
     *
     * @since 3.0
     */
    // See also Lucene's ASCIIFoldingFilter (Lucene 2.9) that replaces accented
    // characters by their unaccented equivalent (and uncommitted bug fix:
    // https://issues.apache.org/jira/browse/LUCENE-1343?focusedCommentId=12858907&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#action_12858907).
    public static String stripAccents(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final Pattern pattern = Pattern.compile("\\p{InCombiningDiacriticalMarks}+");//$NON-NLS-1$
        final String decomposed = Normalizer.normalize(str, Normalizer.Form.NFD);
        // Note that this doesn't correctly remove ligatures...
        return pattern.matcher(decomposed).replaceAll("");//$NON-NLS-1$
    }

    public static String[] stripAccents(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = stripAccents(strs[i]);
        }

        return res;
    }

    // Chomping
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Removes one newline from end of a String if it's there, otherwise leave
     * it alone. A newline is &quot;{@code \n} &quot;, &quot;{@code \r}&quot;,
     * or &quot;{@code \r\n}&quot;.
     * </p>
     *
     * <p>
     * NOTE: This method changed in 2.0. It now more closely matches Perl chomp.
     * </p>
     *
     * <pre>
     * N.chomp(null)          = null
     * N.chomp("")            = ""
     * N.chomp("abc \r")      = "abc "
     * N.chomp("abc\n")       = "abc"
     * N.chomp("abc\r\n")     = "abc"
     * N.chomp("abc\r\n\r\n") = "abc\r\n"
     * N.chomp("abc\n\r")     = "abc\n"
     * N.chomp("abc\n\rabc")  = "abc\n\rabc"
     * N.chomp("\r")          = ""
     * N.chomp("\n")          = ""
     * N.chomp("\r\n")        = ""
     * </pre>
     *
     * @param str
     *            the String to chomp a newline from, may be null
     * @return String without newline, {@code null} if null String input
     */
    public static String chomp(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        if (str.length() == 1) {
            final char ch = str.charAt(0);
            if (ch == N.CHAR_CR || ch == N.CHAR_LF) {
                return N.EMPTY_STRING;
            }

            return str;
        }

        int lastIdx = str.length() - 1;
        final char last = str.charAt(lastIdx);

        if (last == N.CHAR_LF) {
            if (str.charAt(lastIdx - 1) == N.CHAR_CR) {
                lastIdx--;
            }
        } else if (last != N.CHAR_CR) {
            lastIdx++;
        }

        return lastIdx == str.length() ? str : str.substring(0, lastIdx);
    }

    public static String[] chomp(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = chomp(strs[i]);
        }

        return res;
    }

    // Chopping
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Remove the last character from a String.
     * </p>
     *
     * <p>
     * If the String ends in {@code \r\n}, then remove both of them.
     * </p>
     *
     * <pre>
     * N.chop(null)          = null
     * N.chop("")            = ""
     * N.chop("abc \r")      = "abc "
     * N.chop("abc\n")       = "abc"
     * N.chop("abc\r\n")     = "abc"
     * N.chop("abc")         = "ab"
     * N.chop("abc\nabc")    = "abc\nab"
     * N.chop("a")           = ""
     * N.chop("\r")          = ""
     * N.chop("\n")          = ""
     * N.chop("\r\n")        = ""
     * </pre>
     *
     * @param str
     *            the String to chop last character from, may be null
     * @return String without last character, {@code null} if null String input
     */
    public static String chop(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final int strLen = str.length();

        if (strLen < 2) {
            return N.EMPTY_STRING;
        }

        final int lastIdx = strLen - 1;

        if (str.charAt(lastIdx) == N.CHAR_LF && str.charAt(lastIdx - 1) == N.CHAR_CR) {
            return str.substring(0, lastIdx - 1);
        } else {
            return str.substring(0, lastIdx);
        }
    }

    public static String[] chop(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = chop(strs[i]);
        }

        return res;
    }

    // Delete
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Deletes all white spaces from a String as defined by
     * {@link Character#isWhitespace(char)}.
     * </p>
     *
     * <pre>
     * N.deleteWhitespace(null)         = null
     * N.deleteWhitespace("")           = ""
     * N.deleteWhitespace("abc")        = "abc"
     * N.deleteWhitespace("   ab  c  ") = "abc"
     * </pre>
     *
     * @param str
     *            the String to delete whitespace from, may be null
     * @return the String without whitespaces, {@code null} if null String input
     */
    public static String deleteWhitespace(final String str) {
        if (N.isNullOrEmpty(str)) {
            return str;
        }

        final char[] chars = N.getCharsForReadOnly(str);
        final char[] cbuf = new char[chars.length];
        int count = 0;
        for (int i = 0, len = chars.length; i < len; i++) {
            if (!Character.isWhitespace(chars[i])) {
                cbuf[count++] = chars[i];
            }
        }

        return count == chars.length ? str : new String(cbuf, 0, count);
    }

    public static String[] deleteWhitespace(final String[] strs) {
        if (N.isNullOrEmpty(strs)) {
            return strs;
        }

        final String[] res = new String[strs.length];

        for (int i = 0, len = strs.length; i < len; i++) {
            res[i] = deleteWhitespace(strs[i]);
        }

        return res;
    }

    public static boolean isLowerCase(final char ch) {
        return Character.isLowerCase(ch);
    }

    public static boolean isAsciiLowerCase(final char ch) {
        return (ch >= 'a') && (ch <= 'z');
    }

    public static boolean isUpperCase(final char ch) {
        return Character.isUpperCase(ch);
    }

    public static boolean isAsciiUpperCase(final char ch) {
        return (ch >= 'A') && (ch <= 'Z');
    }

    public static boolean isAllLowerCase(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isUpperCase(chars[i])) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isUpperCase(cs.charAt(i))) {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean isAllUpperCase(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isLowerCase(chars[i])) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isLowerCase(cs.charAt(i))) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     *
     * @param ch
     * @return
     * @see Character#isDigit(char)
     */
    public static boolean isDigit(final char ch) {
        return Character.isDigit(ch);
    }

    /**
     *
     * @param ch
     * @return
     * @see Character#isLetter(char)
     */
    public static boolean isLetter(final char ch) {
        return Character.isLetter(ch);
    }

    /**
     *
     * @param ch
     * @return
     * @see Character#isLetterOrDigit(char)
     */
    public static boolean isLetterOrDigit(final char ch) {
        return Character.isLetterOrDigit(ch);
    }

    // --------------------------------------------------------------------------
    /**
     * <p>
     * Checks whether the character is ASCII 7 bit.
     * </p>
     *
     * <pre>
     *   CharUtils.isAscii('a')  = true
     *   CharUtils.isAscii('A')  = true
     *   CharUtils.isAscii('3')  = true
     *   CharUtils.isAscii('-')  = true
     *   CharUtils.isAscii('\n') = true
     *   CharUtils.isAscii('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if less than 128
     */
    public static boolean isAscii(final char ch) {
        return ch < 128;
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit printable.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiPrintable('a')  = true
     *   CharUtils.isAsciiPrintable('A')  = true
     *   CharUtils.isAsciiPrintable('3')  = true
     *   CharUtils.isAsciiPrintable('-')  = true
     *   CharUtils.isAsciiPrintable('\n') = false
     *   CharUtils.isAsciiPrintable('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if between 32 and 126 inclusive
     */
    public static boolean isAsciiPrintable(final char ch) {
        return ch > 31 && ch < 127;
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit control.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiControl('a')  = false
     *   CharUtils.isAsciiControl('A')  = false
     *   CharUtils.isAsciiControl('3')  = false
     *   CharUtils.isAsciiControl('-')  = false
     *   CharUtils.isAsciiControl('\n') = true
     *   CharUtils.isAsciiControl('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if less than 32 or equals 127
     */
    public static boolean isAsciiControl(final char ch) {
        return ch < 32 || ch == 127;
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit alphabetic.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiAlpha('a')  = true
     *   CharUtils.isAsciiAlpha('A')  = true
     *   CharUtils.isAsciiAlpha('3')  = false
     *   CharUtils.isAsciiAlpha('-')  = false
     *   CharUtils.isAsciiAlpha('\n') = false
     *   CharUtils.isAsciiAlpha('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if between 65 and 90 or 97 and 122 inclusive
     */
    public static boolean isAsciiAlpha(final char ch) {
        return isAsciiAlphaUpper(ch) || isAsciiAlphaLower(ch);
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit alphabetic upper case.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiAlphaUpper('a')  = false
     *   CharUtils.isAsciiAlphaUpper('A')  = true
     *   CharUtils.isAsciiAlphaUpper('3')  = false
     *   CharUtils.isAsciiAlphaUpper('-')  = false
     *   CharUtils.isAsciiAlphaUpper('\n') = false
     *   CharUtils.isAsciiAlphaUpper('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if between 65 and 90 inclusive
     */
    public static boolean isAsciiAlphaUpper(final char ch) {
        return ch >= 'A' && ch <= 'Z';
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit alphabetic lower case.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiAlphaLower('a')  = true
     *   CharUtils.isAsciiAlphaLower('A')  = false
     *   CharUtils.isAsciiAlphaLower('3')  = false
     *   CharUtils.isAsciiAlphaLower('-')  = false
     *   CharUtils.isAsciiAlphaLower('\n') = false
     *   CharUtils.isAsciiAlphaLower('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if between 97 and 122 inclusive
     */
    public static boolean isAsciiAlphaLower(final char ch) {
        return ch >= 'a' && ch <= 'z';
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit numeric.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiNumeric('a')  = false
     *   CharUtils.isAsciiNumeric('A')  = false
     *   CharUtils.isAsciiNumeric('3')  = true
     *   CharUtils.isAsciiNumeric('-')  = false
     *   CharUtils.isAsciiNumeric('\n') = false
     *   CharUtils.isAsciiNumeric('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if between 48 and 57 inclusive
     */
    public static boolean isAsciiNumeric(final char ch) {
        return ch >= '0' && ch <= '9';
    }

    /**
     * <p>
     * Checks whether the character is ASCII 7 bit numeric.
     * </p>
     *
     * <pre>
     *   CharUtils.isAsciiAlphanumeric('a')  = true
     *   CharUtils.isAsciiAlphanumeric('A')  = true
     *   CharUtils.isAsciiAlphanumeric('3')  = true
     *   CharUtils.isAsciiAlphanumeric('-')  = false
     *   CharUtils.isAsciiAlphanumeric('\n') = false
     *   CharUtils.isAsciiAlphanumeric('&copy;') = false
     * </pre>
     *
     * @param ch
     *            the character to check
     * @return true if between 48 and 57 or 65 and 90 or 97 and 122 inclusive
     */
    public static boolean isAsciiAlphanumeric(final char ch) {
        return isAsciiAlpha(ch) || isAsciiNumeric(ch);
    }

    public static boolean isAsciiPrintable(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (N.isAsciiPrintable(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (N.isAsciiPrintable(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean isAsciiAlpha(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlpha(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlpha(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean isAsciiAlphaSpace(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlpha(chars[i]) == false && chars[i] != ' ') {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlpha(cs.charAt(i)) == false && cs.charAt(i) != ' ') {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean isAsciiAlphanumeric(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlphanumeric(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlphanumeric(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean isAsciiAlphanumericSpace(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlphanumeric(chars[i]) == false && chars[i] != ' ') {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (N.isAsciiAlphanumeric(cs.charAt(i)) == false && cs.charAt(i) != ' ') {
                    return false;
                }
            }
        }

        return true;
    }

    public static boolean isAsciiNumeric(final CharSequence cs) {
        if (isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (isAsciiNumeric(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (isAsciiNumeric(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    // Character Tests
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Checks if the CharSequence contains only Unicode letters.
     * </p>
     *
     * <p>
     * {@code null} or empty CharSequence (length()=0) will return {@code false}
     * .
     * </p>
     *
     * <pre>
     * N.isAlpha(null)   = false
     * N.isAlpha("")     = false
     * N.isAlpha("  ")   = false
     * N.isAlpha("abc")  = true
     * N.isAlpha("ab2c") = false
     * N.isAlpha("ab-c") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains letters, and is non-null
     * @since 3.0 Changed signature from isAlpha(String) to
     *        isAlpha(CharSequence)
     * @since 3.0 Changed "" to return false and not true
     */
    public static boolean isAlpha(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isLetter(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isLetter(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * <p>
     * Checks if the CharSequence contains only Unicode letters and space (' ').
     * </p>
     *
     * <p>
     * {@code null} or empty CharSequence (length()=0) will return {@code false}
     * .
     * </p>
     *
     * <pre>
     * N.isAlphaSpace(null)   = false
     * N.isAlphaSpace("")     = false
     * N.isAlphaSpace("  ")   = true
     * N.isAlphaSpace("abc")  = true
     * N.isAlphaSpace("ab c") = true
     * N.isAlphaSpace("ab2c") = false
     * N.isAlphaSpace("ab-c") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains letters and space, and is non-null
     * @since 3.0 Changed signature from isAlphaSpace(String) to
     *        isAlphaSpace(CharSequence)
     */
    public static boolean isAlphaSpace(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isLetter(chars[i]) == false && chars[i] != ' ') {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isLetter(cs.charAt(i)) == false && cs.charAt(i) != ' ') {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * <p>
     * Checks if the CharSequence contains only Unicode letters or digits.
     * </p>
     *
     * <p>
     * {@code null} or empty CharSequence (length()=0) will return {@code false}
     * .
     * </p>
     *
     * <pre>
     * N.isAlphanumeric(null)   = false
     * N.isAlphanumeric("")     = false
     * N.isAlphanumeric("  ")   = false
     * N.isAlphanumeric("abc")  = true
     * N.isAlphanumeric("ab c") = false
     * N.isAlphanumeric("ab2c") = true
     * N.isAlphanumeric("ab-c") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains letters or digits, and is non-null
     * @since 3.0 Changed signature from isAlphanumeric(String) to
     *        isAlphanumeric(CharSequence)
     * @since 3.0 Changed "" to return false and not true
     */
    public static boolean isAlphanumeric(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isLetterOrDigit(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isLetterOrDigit(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * <p>
     * Checks if the CharSequence contains only Unicode letters, digits or space
     * ({@code ' '}).
     * </p>
     *
     * <p>
     * {@code null} or empty CharSequence (length()=0) will return {@code false}
     * .
     * </p>
     *
     * <pre>
     * N.isAlphanumericSpace(null)   = false
     * N.isAlphanumericSpace("")     = false
     * N.isAlphanumericSpace("  ")   = true
     * N.isAlphanumericSpace("abc")  = true
     * N.isAlphanumericSpace("ab c") = true
     * N.isAlphanumericSpace("ab2c") = true
     * N.isAlphanumericSpace("ab-c") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains letters, digits or space, and is
     *         non-null
     * @since 3.0 Changed signature from isAlphanumericSpace(String) to
     *        isAlphanumericSpace(CharSequence)
     */
    public static boolean isAlphanumericSpace(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isLetterOrDigit(chars[i]) == false && chars[i] != ' ') {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isLetterOrDigit(cs.charAt(i)) == false && cs.charAt(i) != ' ') {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * <p>
     * Checks if the CharSequence contains only Unicode digits. A decimal point
     * is not a Unicode digit and returns false.
     * </p>
     *
     * <p>
     * {@code null} will return {@code false}. An empty CharSequence
     * (length()=0) will return {@code false}.
     * </p>
     *
     * <p>
     * Note that the method does not allow for a leading sign, either positive
     * or negative. Also, if a String passes the numeric test, it may still
     * generate a NumberFormatException when parsed by Integer.parseInt or
     * Long.parseLong, e.g. if the value is outside the range for int or long
     * respectively.
     * </p>
     *
     * <pre>
     * N.isNumeric(null)   = false
     * N.isNumeric("")     = false
     * N.isNumeric("  ")   = false
     * N.isNumeric("123")  = true
     * N.isNumeric("12 3") = false
     * N.isNumeric("ab2c") = false
     * N.isNumeric("12-3") = false
     * N.isNumeric("12.3") = false
     * N.isNumeric("-123") = false
     * N.isNumeric("+123") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains digits, and is non-null
     * @since 3.0 Changed signature from isNumeric(String) to
     *        isNumeric(CharSequence)
     * @since 3.0 Changed "" to return false and not true
     */
    public static boolean isNumeric(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isDigit(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isDigit(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * <p>
     * Checks if the CharSequence contains only Unicode digits or space (
     * {@code ' '}). A decimal point is not a Unicode digit and returns false.
     * </p>
     *
     * <p>
     * {@code null} or empty CharSequence (length()=0) will return {@code false}
     * .
     * </p>
     *
     * <pre>
     * N.isNumericSpace(null)   = false
     * N.isNumericSpace("")     = false
     * N.isNumericSpace("  ")   = true
     * N.isNumericSpace("123")  = true
     * N.isNumericSpace("12 3") = true
     * N.isNumericSpace("ab2c") = false
     * N.isNumericSpace("12-3") = false
     * N.isNumericSpace("12.3") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains digits or space, and is non-null
     * @since 3.0 Changed signature from isNumericSpace(String) to
     *        isNumericSpace(CharSequence)
     */
    public static boolean isNumericSpace(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isDigit(chars[i]) == false && chars[i] != ' ') {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isDigit(cs.charAt(i)) == false && cs.charAt(i) != ' ') {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * <p>
     * Checks if the CharSequence contains only whitespace.
     * </p>
     *
     * <p>
     * {@code null} or empty CharSequence (length()=0) will return {@code false}
     * .
     * </p>
     *
     * <pre>
     * N.isWhitespace(null)   = false
     * N.isWhitespace("")     = false
     * N.isWhitespace("  ")   = true
     * N.isWhitespace("abc")  = false
     * N.isWhitespace("ab2c") = false
     * N.isWhitespace("ab-c") = false
     * </pre>
     *
     * @param cs
     *            the CharSequence to check, may be null
     * @return {@code true} if only contains whitespace, and is non-null
     * @since 2.0
     * @since 3.0 Changed signature from isWhitespace(String) to
     *        isWhitespace(CharSequence)
     */
    public static boolean isWhitespace(final CharSequence cs) {
        if (N.isNullOrEmpty(cs)) {
            return false;
        }

        final int len = cs.length();

        if (cs.getClass().equals(String.class)) {
            final char[] chars = N.getCharsForReadOnly((String) cs);

            for (int i = 0; i < len; i++) {
                if (Character.isWhitespace(chars[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0; i < len; i++) {
                if (Character.isWhitespace(cs.charAt(i)) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Note: It's copied from NumberUtils in Apache Commons Lang under Apache
     * License 2.0
     *
     * <p>
     * Checks whether the String a valid Java number. <code>true</code> is
     * returned if there is a number which can be initialized by
     * <code>createNumber</code> with specified String.
     * </p>
     *
     * <p>
     * <code>Null</code> and empty String will return <code>false</code>.
     * </p>
     *
     * @param str
     *            the <code>String</code> to check
     * @return <code>true</code> if the string is a correctly formatted number
     * @since 3.3 the code supports hex {@code 0Xhhh} and octal {@code 0ddd}
     *        validation
     */
    public static boolean isNumber(final String str) {
        return N.createNumber(str, true) != null;
    }

    /**
     * <code>true</code> is returned if the specified <code>str</code> only
     * includes characters ('0' ~ '9', '.', '-', '+', 'e').
     * <code>false</code> is return if the specified String is null/empty, or contains empty chars.
     * 
     *  "0" => true
     *  " 0.1 " => false
     *  "abc" => false
     *  "1 a" => false
     *  "2e10" => true
     *  "2E-10" => true
     *
     * @param value
     * @return
     */
    public static boolean isAsciiDigtalNumber(final String str) {
        if (N.isNullOrEmpty(str)) {
            return false;
        }

        final char[] chs = N.getCharsForReadOnly(str);

        int i = 0, num = 0;
        if (chs[i] == '+' || chs[i] == '-') {
            i++;
        }

        for (; i < chs.length && (chs[i] >= '0' && chs[i] <= '9'); i++) {
            num++;
        }

        if (i < chs.length && chs[i] == '.') {
            if (num == 0) {
                return false;
            } else {
                num = 0;
            }

            i++;
        }

        for (; i < chs.length && (chs[i] >= '0' && chs[i] <= '9'); i++) {
            num++;
        }

        if (num == 0) {
            return false;
        }

        if (i == chs.length) {
            return true;
        } else if (chs[i] != 'e' && chs[i] != 'E') {
            return false;
        } else {
            i++;
        }

        num = 0;
        if (i < chs.length && (chs[i] == '+' || chs[i] == '-')) {
            i++;
        }

        for (; i < chs.length && (chs[i] >= '0' && chs[i] <= '9'); i++) {
            num++;
        }

        if (num == 0) {
            return false;
        } else if (i == chs.length) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * <code>true</code> is returned if the specified <code>str</code> only
     * includes characters ('0' ~ '9', '-', '+' ).
     * <code>false</code> is return if the specified String is null/empty, or contains empty chars.
     * 
     *  "-123" => true
     *  "+123" => true
     *  "123" => true
     *  "+0" => true
     *  "-0" => true
     *  "0" => true
     *  " 0.1 " => false
     *  "abc" => false
     *  "1 a" => false
     *  "2e10" => false
     *
     * @param value
     * @return
     */
    public static boolean isAsciiDigtalInteger(final String str) {
        if (N.isNullOrEmpty(str)) {
            return false;
        }

        final char[] chs = N.getCharsForReadOnly(str);

        int i = 0, num = 0;
        if (chs[i] == '+' || chs[i] == '-') {
            i++;
        }

        for (; i < chs.length && (chs[i] >= '0' && chs[i] <= '9'); i++) {
            num++;
        }

        if (num == 0) {
            return false;
        }

        return i == chs.length;
    }

    public static boolean isEntity(final Class<?> cls) {
        Boolean b = entityClassPool.get(cls);

        if (b == null) {
            b = typeOf(cls) instanceof EntityType;
            entityClassPool.put(cls, b);
        }

        return b;
    }

    public static boolean isDirtyMarker(final Class<?> cls) {
        Boolean b = dirtyMarkerClassPool.get(cls);

        if (b == null) {
            b = DirtyMarker.class.isAssignableFrom(cls);
            dirtyMarkerClassPool.put(cls, b);
        }

        return b;
    }

    static boolean isDirtyMarkerEntity(final Class<?> cls) {
        Boolean b = dirtyMarkerEntityClassPool.get(cls);

        if (b == null) {
            b = isDirtyMarker(cls) && N.isEntity(cls);
            dirtyMarkerEntityClassPool.put(cls, b);
        }

        return b;
    }

    public static boolean isPrimitive(final Class<?> cls) {
        return typeOf(cls).isPrimitiveType();
    }

    public static boolean isPrimitiveWapper(final Class<?> cls) {
        return typeOf(cls).isPrimitiveWrapper();
    }

    public static boolean isPrimitiveOrWapper(final Class<?> cls) {
        final Type<?> type = typeOf(cls);

        return type.isPrimitiveType() || type.isPrimitiveWrapper();
    }

    public static boolean isPrimitiveArray(final Class<?> cls) {
        return typeOf(cls).isPrimitiveArray();
    }

    public static Class<?> wrapperOf(final Class<?> primitiveClass) {
        final Class<?> result = PRIMITIVE_2_WRAPPER.get(primitiveClass);

        if (result == null) {
            throw new IllegalArgumentException(ClassUtil.getCanonicalClassName(primitiveClass) + " is not a primitive (array) type");
        }

        return result;
    }

    public static Class<?> primitiveOf(final Class<?> primitiveWrapperClass) {
        Class<?> result = PRIMITIVE_2_WRAPPER.getByValue(primitiveWrapperClass);

        if (result == null) {
            throw new IllegalArgumentException(ClassUtil.getCanonicalClassName(primitiveWrapperClass) + " is not a wrapper of primitive (array) type");
        }

        return result;
    }

    public static <T> T collection2Array(final Class<T> arrayClass, final Collection<?> c) {
        if (c == null) {
            return N.newArray(arrayClass.getComponentType(), 0);
        }

        return (T) N.typeOf(arrayClass).collection2Array(c);
    }

    public static <T> List<T> array2List(final Object a) {
        if (a == null) {
            return asList();
        }

        final List<T> c = asList();

        N.typeOf(a.getClass()).array2Collection(c, a);

        return c;
    }

    public static <T> Set<T> array2Set(final Object a) {
        if (a == null) {
            return asSet();
        }

        final Set<T> c = asSet();

        N.typeOf(a.getClass()).array2Collection(c, a);

        return c;
    }

    /**
     * The input collection is returned
     * @param c
     * @param a
     * @return the input collection.
     */
    @SuppressWarnings({ "unchecked" })
    public static <T extends Collection<?>> T array2Collection(final T c, final Object a) {
        if (a == null) {
            return c;
        }

        N.typeOf(a.getClass()).array2Collection((Collection<?>) c, a);

        return c;
    }

    public static String join(final boolean[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final boolean[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final boolean[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final boolean[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final boolean[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final char[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final char[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final char[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final char[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final char[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final byte[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final byte[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final byte[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final byte[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final byte[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final short[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final short[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final short[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final short[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final short[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final int[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final int[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final int[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final int[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final int[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final long[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final long[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final long[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final long[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final long[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final float[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final float[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final float[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final float[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final float[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final double[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final double[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final double[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final double[] a, final int fromIndex, final int toIndex, final char delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(a[i]);
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final double[] a, final int fromIndex, final int toIndex, final String delimiter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(a[i]);
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(a[i]);
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final Object[] a) {
        return join(a, N.ELEMENT_SEPARATOR);
    }

    public static String join(final Object[] a, final char delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final Object[] a, final String delimiter) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_STRING;
        }

        return join(a, 0, a.length, delimiter);
    }

    public static String join(final Object[] a, final int fromIndex, final int toIndex, final char delimiter) {
        return N.join(a, fromIndex, toIndex, delimiter, false);
    }

    public static String join(final Object[] a, final int fromIndex, final int toIndex, final char delimiter, final boolean trim) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (int i = fromIndex; i < toIndex; i++) {
                if (i > fromIndex) {
                    sb.append(delimiter);
                }

                sb.append(trim ? toString(a[i]).trim() : toString(a[i]));
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final Object[] a, final int fromIndex, final int toIndex, final String delimiter) {
        return N.join(a, fromIndex, toIndex, delimiter, false);
    }

    public static String join(final Object[] a, final int fromIndex, final int toIndex, final String delimiter, final boolean trim) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || fromIndex == toIndex) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (N.isNullOrEmpty(delimiter)) {
                for (int i = fromIndex; i < toIndex; i++) {
                    sb.append(trim ? toString(a[i]).trim() : toString(a[i]));
                }
            } else {
                for (int i = fromIndex; i < toIndex; i++) {
                    if (i > fromIndex) {
                        sb.append(delimiter);
                    }

                    sb.append(trim ? toString(a[i]).trim() : toString(a[i]));
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final Collection<?> c) {
        return join(c, N.ELEMENT_SEPARATOR);
    }

    public static String join(final Collection<?> c, final char delimiter) {
        if (N.isNullOrEmpty(c)) {
            return N.EMPTY_STRING;
        }

        return join(c, 0, c.size(), delimiter);
    }

    public static String join(final Collection<?> c, final String delimiter) {
        if (N.isNullOrEmpty(c)) {
            return N.EMPTY_STRING;
        }

        return join(c, 0, c.size(), delimiter);
    }

    public static String join(final Collection<?> c, final int fromIndex, final int toIndex, final char delimiter) {
        return N.join(c, fromIndex, toIndex, delimiter, false);
    }

    public static String join(final Collection<?> c, final int fromIndex, final int toIndex, final char delimiter, final boolean trim) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < c.size())) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            int i = 0;
            for (Object e : c) {
                if (i++ > fromIndex) {
                    sb.append(delimiter);
                }

                if (i > fromIndex) {
                    sb.append(trim ? toString(e).trim() : toString(e));
                }

                if (i >= toIndex) {
                    break;
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final Collection<?> c, final int fromIndex, final int toIndex, final String delimiter) {
        return N.join(c, fromIndex, toIndex, delimiter, false);
    }

    public static String join(final Collection<?> c, final int fromIndex, final int toIndex, final String delimiter, final boolean trim) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < c.size())) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            if (c instanceof List && c instanceof RandomAccess) {
                final List<?> list = (List<?>) c;

                if (N.isNullOrEmpty(delimiter)) {
                    for (int i = fromIndex; i < toIndex; i++) {
                        sb.append(trim ? toString(list.get(i)).trim() : toString(list.get(i)));
                    }
                } else {
                    for (int i = fromIndex; i < toIndex; i++) {
                        if (i > fromIndex) {
                            sb.append(delimiter);
                        }

                        sb.append(trim ? toString(list.get(i)).trim() : toString(list.get(i)));
                    }
                }
            } else {
                int i = 0;
                if (N.isNullOrEmpty(delimiter)) {
                    for (Object e : c) {
                        if (i++ >= fromIndex) {
                            sb.append(trim ? toString(e).trim() : toString(e));
                        }

                        if (i >= toIndex) {
                            break;
                        }
                    }
                } else {
                    for (Object e : c) {
                        if (i++ > fromIndex) {
                            sb.append(delimiter);
                        }

                        if (i > fromIndex) {
                            sb.append(trim ? toString(e).trim() : toString(e));
                        }

                        if (i >= toIndex) {
                            break;
                        }
                    }
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    /**
     * Returns an immutable list that is empty.
     * 
     * @return
     * @see Collections#emptyList()
     */
    public static <T> List<T> emptyList() {
        return Collections.<T> emptyList();
    }

    /**
     * Returns an immutable set that is empty.
     * 
     * @return
     * @see Collections#emptySet()
     */
    public static <T> Set<T> emptySet() {
        return Collections.<T> emptySet();
    }

    /**
     * Returns an immutable <code>SortedSet</code> that is empty.
     * 
     * @return
     * @see Collections#emptySortedSet()
     */
    public static <T> SortedSet<T> emptySortedSet() {
        return Collections.<T> emptySortedSet();
    }

    /**
     * Returns an immutable <code>NavigableSet</code> that is empty.
     * 
     * @return
     * @see Collections#emptyNavigableSet()
     */
    public static <T> NavigableSet<T> emptyNavigableSet() {
        return Collections.<T> emptyNavigableSet();
    }

    /**
     * Returns an immutable map that is empty.
     * 
     * @return
     * @see Collections#emptyMap()
     */
    public static <K, V> Map<K, V> emptyMap() {
        return Collections.<K, V> emptyMap();
    }

    /**
     * Returns an immutable <code>SortedMap</code> that is empty.
     * 
     * @return
     * @see Collections#emptySortedMap()
     */
    public static <K, V> SortedMap<K, V> emptySortedMap() {
        return Collections.<K, V> emptySortedMap();
    }

    /**
     * Returns an immutable <code>NavigableMap</code> that is empty.
     * 
     * @return
     * @see Collections#emptyNavigableMap()
     */
    public static <K, V> NavigableMap<K, V> emptyNavigableMap() {
        return Collections.<K, V> emptyNavigableMap();
    }

    /**
     * Returns an immutable iterator that has no elements
     * 
     * @return
     * @see Collections#emptyIterator()
     */
    public static <T> Iterator<T> emptyIterator() {
        return Collections.<T> emptyIterator();
    }

    /**
     * Returns an immutable list iterator that has no elements
     * 
     * @return
     * @see Collections#emptyListIterator()
     */
    public static <T> ListIterator<T> emptyListIterator() {
        return Collections.<T> emptyListIterator();
    }

    /**
     * Returns an immutable empty list if the specified List is <code>null</code>, otherwise itself is returned.
     * 
     * @param list
     * @return
     */
    public static <T> List<T> nullToEmpty(final List<T> list) {
        return list == null ? N.<T> emptyList() : list;
    }

    /**
     * Returns an immutable empty set if the specified Set is <code>null</code>, otherwise itself is returned.
     * 
     * @param set
     * @return
     */
    public static <T> Set<T> nullToEmpty(final Set<T> set) {
        return set == null ? N.<T> emptySet() : set;
    }

    /**
     * Returns an immutable empty <code>SortedSet</code> if the specified SortedSet is <code>null</code>, otherwise itself is returned.
     * 
     * @param set
     * @return
     */
    public static <T> SortedSet<T> nullToEmpty(final SortedSet<T> set) {
        return set == null ? N.<T> emptySortedSet() : set;
    }

    /**
     * Returns an immutable empty <code>NavigableSet</code> if the specified NavigableSet is <code>null</code>, otherwise itself is returned.
     * 
     * @param set
     * @return
     */
    public static <T> NavigableSet<T> nullToEmpty(final NavigableSet<T> set) {
        return set == null ? N.<T> emptyNavigableSet() : set;
    }

    /**
     * Returns an immutable empty map if the specified Map is <code>null</code>, otherwise itself is returned.
     * 
     * @param map
     * @return
     */
    public static <K, V> Map<K, V> nullToEmpty(final Map<K, V> map) {
        return map == null ? N.<K, V> emptyMap() : map;
    }

    /**
     * Returns an immutable empty <code>SortedMap</code> if the specified SortedMap is <code>null</code>, otherwise itself is returned.
     * 
     * @param map
     * @return
     */
    public static <K, V> SortedMap<K, V> nullToEmpty(final SortedMap<K, V> map) {
        return map == null ? N.<K, V> emptySortedMap() : map;
    }

    /**
     * Returns an immutable empty <code>NavigableMap</code> if the specified NavigableMap is <code>null</code>, otherwise itself is returned.
     * 
     * @param map
     * @return
     */
    public static <K, V> NavigableMap<K, V> nullToEmpty(final NavigableMap<K, V> map) {
        return map == null ? N.<K, V> emptyNavigableMap() : map;
    }

    /**
     * Returns an immutable empty <code>Iterator</code> if the specified Iterator is <code>null</code>, otherwise itself is returned.
     * 
     * @param iter
     * @return
     */
    public static <T> Iterator<T> nullToEmpty(final Iterator<T> iter) {
        return iter == null ? N.<T> emptyIterator() : iter;
    }

    /**
     * Returns an immutable empty <code>ListIterator</code> if the specified ListIterator is <code>null</code>, otherwise itself is returned.
     * 
     * @param iter
     * @return
     */
    public static <T> ListIterator<T> nullToEmpty(final ListIterator<T> iter) {
        return iter == null ? N.<T> emptyListIterator() : iter;
    }

    public static String nullToEmpty(final String str) {
        return str == null ? EMPTY_STRING : str;
    }

    public static boolean[] nullToEmpty(final boolean[] a) {
        return a == null ? EMPTY_BOOLEAN_ARRAY : a;
    }

    public static char[] nullToEmpty(final char[] a) {
        return a == null ? EMPTY_CHAR_ARRAY : a;
    }

    public static byte[] nullToEmpty(final byte[] a) {
        return a == null ? EMPTY_BYTE_ARRAY : a;
    }

    public static short[] nullToEmpty(final short[] a) {
        return a == null ? EMPTY_SHORT_ARRAY : a;
    }

    public static int[] nullToEmpty(final int[] a) {
        return a == null ? EMPTY_INT_ARRAY : a;
    }

    public static long[] nullToEmpty(final long[] a) {
        return a == null ? EMPTY_LONG_ARRAY : a;
    }

    public static float[] nullToEmpty(final float[] a) {
        return a == null ? EMPTY_FLOAT_ARRAY : a;
    }

    public static double[] nullToEmpty(final double[] a) {
        return a == null ? EMPTY_DOUBLE_ARRAY : a;
    }

    public static String[] nullToEmpty(final String[] a) {
        return a == null ? EMPTY_STRING_ARRAY : a;
    }

    public static Object[] nullToEmpty(final Object[] a) {
        return a == null ? EMPTY_OBJECT_ARRAY : a;
    }

    public static <T> T[] nullToEmpty(final Class<T[]> arrayType, final T[] a) {
        return a == null ? (T[]) N.newArray(arrayType.getComponentType(), 0) : a;
    }

    public static boolean isNullOrEmpty(final CharSequence s) {
        return (s == null) || (s.length() == 0);
    }

    public static boolean isNullOrEmpty(final boolean[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final char[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final byte[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final short[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final int[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final long[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final float[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final double[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final Object[] a) {
        return (a == null) || (a.length == 0);
    }

    public static boolean isNullOrEmpty(final Collection<?> c) {
        return (c == null) || (c.isEmpty());
    }

    public static boolean isNullOrEmpty(final Map<?, ?> m) {
        return (m == null) || (m.isEmpty());
    }

    public static boolean isNullOrEmpty(final List<?> list) {
        return (list == null) || (list.isEmpty());
    }

    @SuppressWarnings("rawtypes")
    public static boolean isNullOrEmpty(final PrimitiveList list) {
        return (list == null) || (list.isEmpty());
    }

    public static boolean isNullOrEmpty(final Multiset<?> s) {
        return (s == null) || (s.isEmpty());
    }

    public static boolean isNullOrEmpty(final LongMultiset<?> s) {
        return (s == null) || (s.isEmpty());
    }

    public static boolean isNullOrEmpty(final Multimap<?, ?, ?> m) {
        return (m == null) || (m.isEmpty());
    }

    public static boolean isNullOrEmpty(final DataSet rs) {
        return (rs == null) || (rs.isEmpty());
    }

    public static boolean isNullOrEmpty(final EntityId entityId) {
        return (entityId == null) || (entityId.isEmpty());
    }

    public static boolean isNullOrEmptyOrBlank(final CharSequence s) {
        if (N.isNullOrEmpty(s)) {
            return true;
        }

        for (int i = 0, len = s.length(); i < len; i++) {
            if (Character.isWhitespace(s.charAt(i)) == false) {
                return false;
            }
        }

        return true;
    }

    public static boolean notNullOrEmpty(final CharSequence s) {
        return (s != null) && (s.length() > 0);
    }

    public static boolean notNullOrEmpty(final boolean[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final char[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final byte[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final short[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final int[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final long[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final float[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final double[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final Object[] a) {
        return (a != null) && (a.length > 0);
    }

    public static boolean notNullOrEmpty(final Collection<?> c) {
        return (c != null) && (c.size() > 0);
    }

    public static boolean notNullOrEmpty(final Map<?, ?> m) {
        return (m != null) && (m.size() > 0);
    }

    public static boolean notNullOrEmpty(final List<?> list) {
        return (list != null) && (list.size() > 0);
    }

    @SuppressWarnings("rawtypes")
    public static boolean notNullOrEmpty(final PrimitiveList list) {
        return (list != null) && (list.size() > 0);
    }

    public static boolean notNullOrEmpty(final Multiset<?> s) {
        return (s != null) && (s.size() > 0);
    }

    public static boolean notNullOrEmpty(final LongMultiset<?> s) {
        return (s != null) && (s.size() > 0);
    }

    public static boolean notNullOrEmpty(final Multimap<?, ?, ?> m) {
        return (m != null) && (m.size() > 0);
    }

    public static boolean notNullOrEmpty(final DataSet rs) {
        return (rs != null) && (rs.size() > 0);
    }

    public static boolean notNullOrEmpty(final EntityId entityId) {
        return (entityId != null) && (!entityId.isEmpty());
    }

    public static boolean notNullOrEmptyOrBlank(final CharSequence s) {
        return !N.isNullOrEmptyOrBlank(s);
    }

    /**
     *
     * @param obj
     * @return
     * @throws NullPointerException if {@code obj} is {@code null}
     */
    public static <T> T requireNonNull(final T obj) {
        if (obj == null) {
            throw new NullPointerException();
        }

        return obj;
    }

    /**
     *
     * @param obj
     * @param errorMessage
     * @return
     * @throws NullPointerException if {@code obj} is {@code null}
     */
    public static <T> T requireNonNull(final T obj, final String errorMessage) {
        if (obj == null) {
            if (isErrorMsg(errorMessage)) {
                throw new NullPointerException(errorMessage);
            } else {
                throw new NullPointerException(errorMessage + " can not be null");
            }
        }

        return obj;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <T extends CharSequence> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.length() == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static boolean[] checkNullOrEmpty(final boolean[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static char[] checkNullOrEmpty(final char[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static byte[] checkNullOrEmpty(final byte[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static short[] checkNullOrEmpty(final short[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static int[] checkNullOrEmpty(final int[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static long[] checkNullOrEmpty(final long[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static float[] checkNullOrEmpty(final float[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static double[] checkNullOrEmpty(final double[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <T> T[] checkNullOrEmpty(final T[] parameter, final String msg) {
        if (parameter == null || parameter.length == 0) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    @SuppressWarnings("rawtypes")
    public static <T extends PrimitiveList> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <E, T extends Collection<E>> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <K, V, T extends Map<K, V>> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <E> Multiset<E> checkNullOrEmpty(final Multiset<E> parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <E> LongMultiset<E> checkNullOrEmpty(final LongMultiset<E> parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <K, E, V extends Collection<E>, T extends Multimap<K, E, V>> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <T extends DataSet> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <T extends EntityId> T checkNullOrEmpty(final T parameter, final String msg) {
        if (parameter == null || parameter.isEmpty()) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty");
            }
        }

        return parameter;
    }

    /**
     * Check if the specified parameter is null or empty or blank
     *
     * @param parameter
     * @param msg name of parameter or error message
     * @return the input parameter
     * @throws IllegalArgumentException if the specified parameter is null or empty.
     */
    public static <T extends CharSequence> T checkNullOrEmptyOrBlank(final T parameter, final String msg) {
        if (N.isNullOrEmptyOrBlank(parameter)) {
            if (isErrorMsg(msg)) {
                throw new IllegalArgumentException(msg);
            } else {
                throw new IllegalArgumentException(msg + " can not be null or empty or blank");
            }
        }

        return parameter;
    }

    /*
     * All recent hotspots (as of 2009) *really* like to have the natural code
     *
     * if (guardExpression) {
     *    throw new BadException(messageExpression);
     * }
     *
     * refactored so that messageExpression is moved to a separate String-returning method.
     *
     * if (guardExpression) {
     *    throw new BadException(badMsg(...));
     * }
     *
     * The alternative natural refactorings into void or Exception-returning methods are much slower.
     * This is a big deal - we're talking factors of 2-8 in microbenchmarks, not just 10-20%. (This is
     * a hotspot optimizer bug, which should be fixed, but that's a separate, big project).
     *
     * The coding pattern above is heavily used in java.util, e.g. in ArrayList. There is a
     * RangeCheckMicroBenchmark in the JDK that was used to test this.
     *
     * But the methods in this class want to throw different exceptions, depending on the args, so it
     * appears that this pattern is not directly applicable. But we can use the ridiculous, devious
     * trick of throwing an exception in the middle of the construction of another exception. Hotspot
     * is fine with that.
     */

    private static boolean isErrorMsg(final String msg) {
        // shortest message: "it is null"
        return msg.length() > 9 && msg.indexOf(D._SPACE) > 0;
    }

    /**
     * <br />
     * Copied from JDK 9 through: StreamSupport at: https://github.com/streamsupport/streamsupport.
     * <br />
     * 
     * Checks if the {@code index} is within the bounds of the range from
     * {@code 0} (inclusive) to {@code length} (exclusive).
     *
     * <p>The {@code index} is defined to be out-of-bounds if any of the
     * following inequalities is true:
     * <ul>
     *  <li>{@code index < 0}</li>
     *  <li>{@code index >= length}</li>
     *  <li>{@code length < 0}, which is implied from the former inequalities</li>
     * </ul>
     *
     * @param index the index
     * @param length the upper-bound (exclusive) of the range
     * @return {@code index} if it is within bounds of the range
     * @throws IndexOutOfBoundsException if the {@code index} is out-of-bounds
     * @since 9
     */
    public static void checkIndex(final int index, final int length) {
        if (index < 0 || index >= length) {
            throw new IndexOutOfBoundsException(String.format("Index %d out-of-bounds for length %d", index, length));
        }
    }

    /**
     * <br />
     * Copied from JDK 9 through: StreamSupport at: https://github.com/streamsupport/streamsupport.
     * <br />
     * 
     * Checks if the sub-range from {@code fromIndex} (inclusive) to
     * {@code toIndex} (exclusive) is within the bounds of range from {@code 0}
     * (inclusive) to {@code length} (exclusive).
     *
     * <p>The sub-range is defined to be out-of-bounds if any of the following
     * inequalities is true:
     * <ul>
     *  <li>{@code fromIndex < 0}</li>
     *  <li>{@code fromIndex > toIndex}</li>
     *  <li>{@code toIndex > length}</li>
     *  <li>{@code length < 0}, which is implied from the former inequalities</li>
     * </ul>
     *
     * @param fromIndex the lower-bound (inclusive) of the sub-range
     * @param toIndex the upper-bound (exclusive) of the sub-range
     * @param length the upper-bound (exclusive) the range
     * @return {@code fromIndex} if the sub-range is within bounds of the range
     * @throws IndexOutOfBoundsException if the sub-range is out-of-bounds
     * @since 9
     */
    public static void checkFromToIndex(final int fromIndex, final int toIndex, final int length) {
        if (fromIndex < 0 || fromIndex > toIndex || toIndex > length) {
            throw new IndexOutOfBoundsException(String.format("Range [%d, %d) out-of-bounds for length %d", fromIndex, toIndex, length));
        }
    }

    /**
     * <br />
     * Copied from JDK 9 through: StreamSupport at: https://github.com/streamsupport/streamsupport.
     * <br />
     * 
     * Checks if the sub-range from {@code fromIndex} (inclusive) to
     * {@code fromIndex + size} (exclusive) is within the bounds of range from
     * {@code 0} (inclusive) to {@code length} (exclusive).
     *
     * <p>The sub-range is defined to be out-of-bounds if any of the following
     * inequalities is true:
     * <ul>
     *  <li>{@code fromIndex < 0}</li>
     *  <li>{@code size < 0}</li>
     *  <li>{@code fromIndex + size > length}, taking into account integer overflow</li>
     *  <li>{@code length < 0}, which is implied from the former inequalities</li>
     * </ul>
     *
     * @param fromIndex the lower-bound (inclusive) of the sub-interval
     * @param size the size of the sub-range
     * @param length the upper-bound (exclusive) of the range
     * @return {@code fromIndex} if the sub-range is within bounds of the range
     * @throws IndexOutOfBoundsException if the sub-range is out-of-bounds
     * @since 9
     */
    public static void checkFromIndexSize(final int fromIndex, final int size, final int length) {
        if ((length | fromIndex | size) < 0 || size > length - fromIndex) {
            throw new IndexOutOfBoundsException(String.format("Range [%d, %<d + %d) out-of-bounds for length %d", fromIndex, size, length));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * @param expression a boolean expression
     * @throws IllegalArgumentException if {@code expression} is false
     */
    public static void checkArgument(boolean expression) {
        if (!expression) {
            throw new IllegalArgumentException();
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * @param expression a boolean expression
     * @param errorMessage the exception message to use if the check fails; will be converted to a
     *     string using {@link String#valueOf(Object)}
     * @throws IllegalArgumentException if {@code expression} is false
     */
    public static void checkArgument(boolean expression, String errorMessage) {
        if (!expression) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, boolean p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, boolean p1, boolean p2) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, boolean p1, boolean p2, boolean p3) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, char p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, byte p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, short p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, int p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, int p1, int p2) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, int p1, int p2, int p3) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, long p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, long p1, long p2) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, long p1, long p2, long p3) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, float p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, float p1, float p2) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, float p1, float p2, float p3) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, double p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, double p1, double p2) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, double p1, double p2, double p3) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, Object p) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, Object p1, Object p2) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving one or more parameters to the calling method.
     *
     * <p>See {@link #checkArgument(boolean, String, Object...)} for details.
     */
    public static void checkArgument(boolean b, String errorMessageTemplate, Object p1, Object p2, Object p3) {
        if (!b) {
            throw new IllegalArgumentException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * @param expression a boolean expression
     * @throws IllegalStateException if {@code expression} is false
     */
    public static void checkState(boolean expression) {
        if (!expression) {
            throw new IllegalStateException();
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * @param expression a boolean expression
     * @param errorMessage the exception message to use if the check fails; will be converted to a
     *     string using {@link String#valueOf(Object)}
     * @throws IllegalStateException if {@code expression} is false
     */
    public static void checkState(boolean expression, String errorMessage) {
        if (!expression) {
            throw new IllegalStateException(errorMessage);
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, int p) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, int p1, int p2) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, int p1, int p2, int p3) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, double p) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, double p1, double p2) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, double p1, double p2, double p3) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, Object p) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, Object p1, Object p2) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p1, p2));
        }
    }

    /**
     * Ensures the truth of an expression involving the state of the calling instance, but not
     * involving any parameters to the calling method.
     *
     * <p>See {@link #checkState(boolean, String, Object...)} for details.
     */
    public static void checkState(boolean b, String errorMessageTemplate, Object p1, Object p2, Object p3) {
        if (!b) {
            throw new IllegalStateException(format(errorMessageTemplate, p1, p2, p3));
        }
    }

    /*
     * All recent hotspots (as of 2009) *really* like to have the natural code
     *
     * if (guardExpression) {
     *    throw new BadException(messageExpression);
     * }
     *
     * refactored so that messageExpression is moved to a separate String-returning method.
     *
     * if (guardExpression) {
     *    throw new BadException(badMsg(...));
     * }
     *
     * The alternative natural refactorings into void or Exception-returning methods are much slower.
     * This is a big deal - we're talking factors of 2-8 in microbenchmarks, not just 10-20%. (This is
     * a hotspot optimizer bug, which should be fixed, but that's a separate, big project).
     *
     * The coding pattern above is heavily used in java.util, e.g. in ArrayList. There is a
     * RangeCheckMicroBenchmark in the JDK that was used to test this.
     *
     * But the methods in this class want to throw different exceptions, depending on the args, so it
     * appears that this pattern is not directly applicable. But we can use the ridiculous, devious
     * trick of throwing an exception in the middle of the construction of another exception. Hotspot
     * is fine with that.
     */

    /**
     * Substitutes each {@code %s} in {@code template} with an argument. These are matched by
     * position: the first {@code %s} gets {@code args[0]}, etc. If there are more arguments than
     * placeholders, the unmatched arguments will be appended to the end of the formatted message in
     * square braces.
     *
     * @param template a non-null string containing 0 or more {@code %s} placeholders.
     * @param args the arguments to be substituted into the message template. Arguments are converted
     *     to strings using {@link String#valueOf(Object)}. Arguments can be null.
     */
    // Note that this is somewhat-improperly used from Verify.java as well.
    static String format(String template, Object... args) {
        template = String.valueOf(template); // null -> "null"

        // start substituting the arguments into the '%s' placeholders
        StringBuilder builder = new StringBuilder(template.length() + 16 * args.length);
        int templateStart = 0;
        int i = 0;
        while (i < args.length) {
            int placeholderStart = template.indexOf("%s", templateStart);
            if (placeholderStart == -1) {
                break;
            }
            builder.append(template, templateStart, placeholderStart);
            builder.append(args[i++]);
            templateStart = placeholderStart + 2;
        }
        builder.append(template, templateStart, template.length());

        // if we run out of placeholders, append the extra args in square braces
        if (i < args.length) {
            builder.append(" [");
            builder.append(args[i++]);
            while (i < args.length) {
                builder.append(", ");
                builder.append(args[i++]);
            }
            builder.append(']');
        }

        return builder.toString();
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final boolean a, final boolean b) {
        return a == b;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final char a, final char b) {
        return a == b;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final byte a, final byte b) {
        return a == b;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final short a, final short b) {
        return a == b;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final int a, final int b) {
        return a == b;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final long a, final long b) {
        return a == b;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final float a, final float b) {
        return Float.compare(a, b) == 0;
    }

    /**
     * Method equals.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final double a, final double b) {
        return Double.compare(a, b) == 0;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static boolean equals(final String a, final String b) {
        return (a == null) ? b == null : (b == null ? false : a.length() == b.length() && a.equals(b));
    }

    /**
     * @param a
     * @param b
     * @return
     */
    public static boolean equalsIgnoreCase(final String a, final String b) {
        return (a == null) ? b == null : (b == null ? false : a.equalsIgnoreCase(b));
    }

    /**
     * compare {@code a} and {@code b} by
     * {@link Arrays#equals(Object[], Object[])} if both of them are array.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final Object a, final Object b) {
        if ((a == null) ? b == null : (b == null ? false : a.equals(b))) {
            return true;
        }

        if ((a != null) && (b != null)) {
            final Type<Object> typeA = typeOf(a.getClass());

            if (typeA.isPrimitiveArray()) {
                final Type<Object> typeB = typeOf(b.getClass());

                return typeA.getTypeClass().equals(typeB.getTypeClass()) && typeA.equals(a, b);
            } else if (typeA.isObjectArray()) {
                final Type<Object> typeB = typeOf(b.getClass());

                return typeB.isObjectArray() && typeA.equals(a, b);
            }
        }

        return false;
    }

    /**
     * compare {@code a} and {@code b} by
     * {@link Arrays#equals(Object[], Object[])} if both of them are array.
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean deepEquals(final Object a, final Object b) {
        if ((a == null) ? b == null : (b == null ? false : a.equals(b))) {
            return true;
        }

        if ((a != null) && (b != null) && a.getClass().isArray() && a.getClass().equals(b.getClass())) {
            return typeOf(a.getClass()).deepEquals(a, b);
        }

        return false;
    }

    /**
     * @see Arrays#equals(boolean[], boolean[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final boolean[] a, final boolean[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final boolean[] a, final int fromIndexA, final boolean[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (a[i] != b[j]) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(char[], char[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final char[] a, final char[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final char[] a, final int fromIndexA, final char[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (a[i] != b[j]) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(byte[], byte[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final byte[] a, final byte[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final byte[] a, final int fromIndexA, final byte[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (a[i] != b[j]) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(short[], short[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final short[] a, final short[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final short[] a, final int fromIndexA, final short[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (a[i] != b[j]) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(int[], int[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final int[] a, final int[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final int[] a, final int fromIndexA, final int[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (a[i] != b[j]) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(long[], long[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final long[] a, final long[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final long[] a, final int fromIndexA, final long[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (a[i] != b[j]) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(float[], float[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final float[] a, final float[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final float[] a, final int fromIndexA, final float[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (Float.compare(a[i], b[j]) != 0) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(double[], double[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final double[] a, final double[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final double[] a, final int fromIndexA, final double[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (Double.compare(a[i], b[j]) != 0) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#equals(Object[], Object[])
     *
     * @param a
     * @param b
     * @return boolean
     */
    public static boolean equals(final Object[] a, final Object[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equals(final Object[] a, final int fromIndexA, final Object[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        } else if (a.getClass().equals(b.getClass()) == false) {
            return false;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (N.equals(a[i], b[j]) == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * @see Arrays#deepEquals(Object[], Object[])
     * @param a
     * @param b
     * @return
     */
    public static boolean deepEquals(final Object[] a, final Object[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && deepEquals(a, 0, b, 0, a.length));
    }

    public static boolean deepEquals(final Object[] a, final int fromIndexA, final Object[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        } else if (a.getClass().equals(b.getClass()) == false) {
            return false;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (N.deepEquals(a[i], b[j]) == false) {
                return false;
            }
        }

        return true;
    }

    public static boolean equalsIgnoreCase(final String[] a, final String[] b) {
        return (a == null || b == null) ? a == b : (a.length == b.length && equalsIgnoreCase(a, 0, b, 0, a.length));
    }

    /**
     * 
     * @param a
     * @param fromIndexA
     * @param b
     * @param fromIndexB
     * @param len
     * @return
     */
    public static boolean equalsIgnoreCase(final String[] a, final int fromIndexA, final String[] b, final int fromIndexB, final int len) {
        if (len < 0) {
            throw new IllegalArgumentException("'len' can't be negative");
        }

        N.checkFromIndexSize(fromIndexA, len, a == null ? 0 : a.length);
        N.checkFromIndexSize(fromIndexB, len, b == null ? 0 : b.length);

        if ((fromIndexA == fromIndexB && a == b) || len == 0) {
            return true;
        } else if (a.getClass().equals(b.getClass()) == false) {
            return false;
        }

        for (int i = fromIndexA, j = fromIndexB, k = 0; k < len; i++, j++, k++) {
            if (((a[i] == null || b[j] == null) ? a == b : a[i].equalsIgnoreCase(b[j])) == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final boolean value) {
        return value ? 1231 : 1237;
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final char value) {
        return value;
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final byte value) {
        return value;
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final short value) {
        return value;
    }

    /**
     * Method hashCode.
     *
     * @param vaue
     * @return int
     */
    public static int hashCode(final int value) {
        return value;
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final long value) {
        return (int) (value ^ (value >>> 32));
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final float value) {
        return Float.floatToIntBits(value);
    }

    /**
     * Method hashCode.
     *
     * @param value
     * @return int
     */
    public static int hashCode(final double value) {
        long bits = Double.doubleToLongBits(value);

        return (int) (bits ^ (bits >>> 32));
    }

    /**
     * Method hashCode.
     *
     * @param obj
     * @return int
     */
    public static int hashCode(final Object obj) {
        if (obj == null) {
            return 0;
        }

        if (obj.getClass().isArray()) {
            return typeOf(obj.getClass()).hashCode(obj);
        }

        return obj.hashCode();
    }

    /**
     * Method deepHashCode.
     *
     * @param obj
     * @return int
     */
    public static int deepHashCode(final Object obj) {
        if (obj == null) {
            return 0;
        }

        if (obj.getClass().isArray()) {
            return typeOf(obj.getClass()).deepHashCode(obj);
        }

        return obj.hashCode();
    }

    /**
     * @see Arrays#hashCode(boolean[])
     * @param a
     * @return
     */
    public static int hashCode(final boolean[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final boolean[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + (a[i] ? 1231 : 1237);
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(char[])
     * @param a
     * @return
     */
    public static int hashCode(final char[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final char[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + a[i];
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(byte[])
     * @param a
     * @return
     */
    public static int hashCode(final byte[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final byte[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + a[i];
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(short[])
     * @param a
     * @return
     */
    public static int hashCode(final short[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final short[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + a[i];
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(int[])
     * @param a
     * @return
     */
    public static int hashCode(final int[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final int[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + a[i];
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(long[])
     * @param a
     * @return
     */
    public static int hashCode(final long[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final long[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + (int) (a[i] ^ (a[i] >>> 32));
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(float[])
     * @param a
     * @return
     */
    public static int hashCode(final float[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final float[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + Float.floatToIntBits(a[i]);
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(double[])
     * @param a
     * @return
     */
    public static int hashCode(final double[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final double[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            long bits = Double.doubleToLongBits(a[i]);
            result = 31 * result + (int) (bits ^ (bits >>> 32));
        }

        return result;
    }

    /**
     * @see Arrays#hashCode(Object[])
     * @param a
     * @return
     */
    public static int hashCode(final Object[] a) {
        return a == null ? 0 : hashCode(a, 0, a.length);
    }

    public static int hashCode(final Object[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + (a[i] == null ? 0 : a[i].hashCode());
        }

        return result;
    }

    /**
     * @see Arrays#deepHashCode(Object[])
     * @param a
     * @return
     */
    public static int deepHashCode(final Object[] a) {
        return a == null ? 0 : deepHashCode(a, 0, a.length);
    }

    public static int deepHashCode(final Object[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return 0;
        }

        int result = 1;

        for (int i = fromIndex; i < toIndex; i++) {
            result = 31 * result + (a[i] == null ? 0 : deepHashCode(a[i]));
        }

        return result;
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final boolean value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final char value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final byte value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final short value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param vaue
     * @return int
     */
    public static String toString(final int value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final long value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final float value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param value
     * @return int
     */
    public static String toString(final double value) {
        return stringOf(value);
    }

    /**
     * Method toString.
     *
     * @param obj
     * @return int
     */
    public static String toString(final Object obj) {
        if (obj == null) {
            return NULL_STRING;
        } else if (obj instanceof CharSequence) {
            return obj.toString();
        }

        if (obj.getClass().isArray()) {
            return typeOf(obj.getClass()).toString(obj);
        }

        final Integer typeIdx = CLASS_TYPE_ENUM.get(obj.getClass());

        if (typeIdx == null) {
            return obj.toString();
        }

        switch (typeIdx.intValue()) {
            case 21:
                return toString(((Boolean) obj).booleanValue());

            case 22:
                return toString(((Character) obj).charValue());

            case 23:
                return toString(((Byte) obj).byteValue());

            case 24:
                return toString(((Short) obj).shortValue());

            case 25:
                return toString(((Integer) obj).intValue());

            case 26:
                return toString(((Long) obj).longValue());

            case 27:
                return toString(((Float) obj).floatValue());

            case 28:
                return toString(((Double) obj).doubleValue());

            default:
                return obj.toString();
        }
    }

    /**
     * Method deepToString.
     *
     * @param obj
     * @return int
     */
    public static String deepToString(final Object obj) {
        if (obj == null) {
            return NULL_STRING;
        }

        if (obj.getClass().isArray()) {
            return typeOf(obj.getClass()).deepToString(obj);
        }

        return obj.toString();
    }

    /**
     * @see Arrays#toString(boolean[])
     * @param a
     * @return
     */
    public static String toString(final boolean[] a) {
        if (a == null) {
            return N.NULL_STRING;
        }

        if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final boolean[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final boolean[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final boolean[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(char[])
     * @param a
     * @return
     */
    public static String toString(final char[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final char[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final char[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final char[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(byte[])
     * @param a
     * @return
     */
    public static String toString(final byte[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final byte[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final byte[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final byte[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(short[])
     * @param a
     * @return
     */
    public static String toString(final short[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final short[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final short[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final short[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(int[])
     * @param a
     * @return
     */
    public static String toString(final int[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final int[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final int[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final int[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(long[])
     * @param a
     * @return
     */
    public static String toString(final long[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final long[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final long[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final long[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(float[])
     * @param a
     * @return
     */
    public static String toString(final float[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final float[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final float[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final float[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(double[])
     * @param a
     * @return
     */
    public static String toString(final double[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final double[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final double[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final double[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(a[i]);
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#toString(Object[])
     * @param a
     * @return
     */
    public static String toString(final Object[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return toString(a, 0, a.length);
    }

    public static String toString(final Object[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            toString(sb, a, from, to);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    static void toString(final StringBuilder sb, final Object[] a) {
        if (a == null) {
            sb.append(NULL_STRING);
        } else if (a.length == 0) {
            sb.append("[]");
        } else {
            toString(sb, a, 0, a.length);
        }
    }

    static void toString(final StringBuilder sb, final Object[] a, final int from, final int to) {
        sb.append(D._BRACKET_L);

        for (int i = from; i < to; i++) {
            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            sb.append(toString(a[i]));
        }

        sb.append(D._BRACKET_R);
    }

    /**
     * @see Arrays#deepToString(Object[])
     * @param a
     * @return
     */
    public static String deepToString(final Object[] a) {
        if (a == null) {
            return N.NULL_STRING;
        } else if (a.length == 0) {
            return "[]";
        }

        return deepToString(a, 0, a.length);
    }

    public static String deepToString(final Object[] a, final int from, final int to) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();
        final Set<Object[]> set = ObjectFactory.createSet();

        try {
            deepToString(sb, a, from, to, set);

            return sb.toString();
        } finally {
            ObjectFactory.recycle(set);
            ObjectFactory.recycle(sb);
        }
    }

    static void deepToString(final StringBuilder sb, final Object[] a, final Set<Object[]> processedElements) {
        deepToString(sb, a, 0, a.length, processedElements);
    }

    static void deepToString(final StringBuilder sb, final Object[] a, final int from, final int to, final Set<Object[]> processedElements) {
        processedElements.add(a);

        sb.append(D._BRACKET_L);

        Object element = null;
        Class<?> eClass = null;
        for (int i = from; i < to; i++) {
            element = a[i];

            if (i > from) {
                sb.append(D.COMMA_SPACE);
            }

            if (element == null) {
                sb.append(N.NULL_CHAR_ARRAY);

                continue;
            }

            eClass = element.getClass();

            if (eClass.isArray()) {
                Integer enumInt = CLASS_TYPE_ENUM.get(eClass);

                int num = enumInt == null ? 0 : enumInt.intValue();

                switch (num) {
                    case 11:
                        toString(sb, (boolean[]) element);
                        break;

                    case 12:
                        toString(sb, (char[]) element);
                        break;

                    case 13:
                        toString(sb, (byte[]) element);
                        break;

                    case 14:
                        toString(sb, (short[]) element);
                        break;

                    case 15:
                        toString(sb, (int[]) element);
                        break;

                    case 16:
                        toString(sb, (long[]) element);
                        break;

                    case 17:
                        toString(sb, (float[]) element);
                        break;

                    case 18:
                        toString(sb, (double[]) element);
                        break;

                    default:
                        if (processedElements.contains(element)) {
                            sb.append("[...]");
                        } else {
                            deepToString(sb, (Object[]) element, processedElements);
                        }
                }
            } else { // element is non-null and not an array
                sb.append(element.toString());
            }
        }

        sb.append(D._BRACKET_R);

        processedElements.remove(a);
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final boolean[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final boolean[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        boolean tmp = false;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final char[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final char[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        char tmp = 0;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final byte[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        byte tmp = 0;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final short[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final short[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        short tmp = 0;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final int[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        int tmp = 0;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final long[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        long tmp = 0L;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final float[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        float tmp = 0f;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * @param a
     */
    public static void reverse(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final double[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        double tmp = 0d;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    // Reverse
    // -----------------------------------------------------------------------
    /**
     * <p>
     * Reverses the order of the given array.
     * </p>
     *
     * <p>
     * There is no special handling for multi-dimensional arrays.
     * </p>
     *
     * @param a
     */
    public static void reverse(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        reverse(a, 0, a.length);
    }

    /**
     * <p>
     * Reverses the order of the given array in the given range.
     * </p>
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     */
    public static void reverse(final Object[] a, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        Object tmp = null;

        for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
            tmp = a[i];
            a[i] = a[j];
            a[j] = tmp;
        }
    }

    public static void reverse(final List<?> list) {
        if (N.isNullOrEmpty(list)) {
            return;
        }

        reverse(list, 0, list.size());
    }

    public static void reverse(final List<?> list, int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex, list == null ? 0 : list.size());

        if (N.isNullOrEmpty(list) || list.size() == 1) {
            return;
        }

        final List<Object> l = (List<Object>) list;

        if (toIndex - fromIndex < REVERSE_THRESHOLD || list instanceof RandomAccess) {
            for (int i = fromIndex, j = toIndex - 1; i < j; i++, j--) {
                l.set(i, l.set(j, l.get(i)));
            }
        } else {
            final ListIterator<Object> fwd = l.listIterator(fromIndex);
            final ListIterator<Object> rev = l.listIterator(toIndex);

            for (int i = 0, mid = (toIndex - fromIndex) / 2; i < mid; i++) {
                Object tmp = fwd.next();
                fwd.set(rev.previous());
                rev.set(tmp);
            }
        }
    }

    public static void rotate(final boolean[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            boolean tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final char[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            char tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final byte[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            byte tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final short[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            short tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final int[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            int tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final long[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            long tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final float[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            float tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final double[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            double tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    public static void rotate(final Object[] a, int distance) {
        if (a == null || a.length <= 1 || distance % a.length == 0) {
            return;
        }

        final int len = a.length;
        distance = distance % len;

        if (distance < 0) {
            distance += len;
        }

        if (distance == 0) {
            return;
        }

        for (int i = 0, count = 0; count < len; i++) {
            Object tmp = a[i];
            int curr = i;
            int next = curr < distance ? curr - distance + len : curr - distance;

            while (next != i) {
                a[curr] = a[next];
                curr = next;
                next = curr < distance ? curr - distance + len : curr - distance;
                count++;
            }

            a[curr] = tmp;
            count++;
        }
    }

    /**
     *
     * @see java.util.Collections#rotate(List, int)
     */
    public static void rotate(final List<?> list, final int distance) {
        if (list == null || list.size() <= 1 || distance % list.size() == 0) {
            return;
        }

        Collections.rotate(list, distance);
    }

    public static void shuffle(final boolean[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final boolean[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final char[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final char[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final byte[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final byte[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final short[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final short[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final int[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final int[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final long[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final long[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final float[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final float[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final double[] a) {
        shuffle(a, RAND);
    }

    public static void shuffle(final double[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static <T> void shuffle(final T[] a) {
        shuffle(a, RAND);
    }

    public static <T> void shuffle(final T[] a, final Random rnd) {
        if (N.isNullOrEmpty(a) || a.length == 1) {
            return;
        }

        for (int i = a.length; i > 1; i--) {
            swap(a, i - 1, rnd.nextInt(i));
        }
    }

    public static void shuffle(final List<?> list) {
        shuffle(list, RAND);
    }

    /**
     *
     * @see java.util.Collections#shuffle(List, Random)
     */
    public static void shuffle(final List<?> list, final Random rnd) {
        if (N.isNullOrEmpty(list) || list.size() == 1) {
            return;
        }

        Collections.shuffle(list, rnd);
    }

    public static void swap(final boolean[] a, final int i, final int j) {
        final boolean tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final char[] a, final int i, final int j) {
        final char tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final byte[] a, final int i, final int j) {
        final byte tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final short[] a, final int i, final int j) {
        final short tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final int[] a, final int i, final int j) {
        final int tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final long[] a, final int i, final int j) {
        final long tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final float[] a, final int i, final int j) {
        final float tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final double[] a, final int i, final int j) {
        final double tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final Object[] a, final int i, final int j) {
        final Object tmp = a[i];
        a[i] = a[j];
        a[j] = tmp;
    }

    public static void swap(final List<?> list, final int i, final int j) {
        Collections.swap(list, i, j);
    }

    public static void fill(final boolean[] a, final boolean val) {
        Arrays.fill(a, val);
    }

    public static void fill(final boolean[] a, final int fromIndex, final int toIndex, final boolean val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final char[] a, final char val) {
        Arrays.fill(a, val);
    }

    public static void fill(final char[] a, final int fromIndex, final int toIndex, final char val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final byte[] a, final byte val) {
        Arrays.fill(a, val);
    }

    public static void fill(final byte[] a, final int fromIndex, final int toIndex, final byte val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final short[] a, final short val) {
        Arrays.fill(a, val);
    }

    public static void fill(final short[] a, final int fromIndex, final int toIndex, final short val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final int[] a, final int val) {
        Arrays.fill(a, val);
    }

    public static void fill(final int[] a, final int fromIndex, final int toIndex, final int val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final long[] a, final long val) {
        Arrays.fill(a, val);
    }

    public static void fill(final long[] a, final int fromIndex, final int toIndex, final long val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final float[] a, final float val) {
        Arrays.fill(a, val);
    }

    public static void fill(final float[] a, final int fromIndex, final int toIndex, final float val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final double[] a, final double val) {
        Arrays.fill(a, val);
    }

    public static void fill(final double[] a, final int fromIndex, final int toIndex, final double val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static void fill(final Object[] a, final Object val) {
        Arrays.fill(a, val);
    }

    public static void fill(final Object[] a, final int fromIndex, final int toIndex, final Object val) {
        Arrays.fill(a, fromIndex, toIndex, val);
    }

    public static <T> void fill(final List<? super T> list, final T val) {
        fill(list, 0, list.size(), val);
    }

    /**
     * The specified value will be added/inserted into the specified List.
     * The List will be extended automatically if the size of the List is less than the specified toIndex.
     * @param list
     * @param fromIndex
     * @param toIndex
     * @param val
     */
    public static <T> void fill(final List<? super T> list, final int fromIndex, final int toIndex, final T val) {
        checkFromToIndex(fromIndex, toIndex, Integer.MAX_VALUE);

        int size = list.size();

        if (size < toIndex) {
            if (fromIndex < size) {
                for (int i = fromIndex; i < size; i++) {
                    list.set(i, val);
                }
            } else {
                for (int i = size; i < fromIndex; i++) {
                    list.add(null);
                }
            }

            for (int i = 0, len = toIndex - list.size(); i < len; i++) {
                list.add(val);
            }
        } else {
            if (toIndex - fromIndex < FILL_THRESHOLD || list instanceof RandomAccess) {
                for (int i = fromIndex; i < toIndex; i++) {
                    list.set(i, val);
                }
            } else {
                final ListIterator<? super T> itr = list.listIterator(fromIndex);

                for (int i = fromIndex; i < toIndex; i++) {
                    itr.next();

                    itr.set(val);
                }
            }
        }
    }

    /**
     * Fill the properties of the entity with random values.
     * 
     * @param entity an entity object with getter/setter method
     */
    public static void fill(Object entity) {
        final Class<?> entityClass = entity.getClass();

        if (N.isEntity(entityClass) == false) {
            throw new AbacusException(entityClass.getCanonicalName() + " is not a valid entity class with property getter/setter method");
        }

        Type<Object> type = null;
        Class<?> parameterClass = null;
        Object propValue = null;

        for (Method method : ClassUtil.getPropSetMethodList(entityClass).values()) {
            parameterClass = method.getParameterTypes()[0];
            type = N.typeOf(parameterClass);

            if (String.class.equals(parameterClass)) {
                propValue = N.uuid().substring(0, 16);
            } else if (boolean.class.equals(parameterClass) || Boolean.class.equals(parameterClass)) {
                propValue = RAND.nextInt() % 2 == 0 ? false : true;
            } else if (char.class.equals(parameterClass) || Character.class.equals(parameterClass)) {
                propValue = (char) ('a' + RAND.nextInt() % 26);
            } else if (int.class.equals(parameterClass) || Integer.class.equals(parameterClass)) {
                propValue = RAND.nextInt();
            } else if (long.class.equals(parameterClass) || Long.class.equals(parameterClass)) {
                propValue = RAND.nextLong();
            } else if (float.class.equals(parameterClass) || Float.class.equals(parameterClass)) {
                propValue = RAND.nextFloat();
            } else if (double.class.equals(parameterClass) || Double.class.equals(parameterClass)) {
                propValue = RAND.nextDouble();
            } else if (byte.class.equals(parameterClass) || Byte.class.equals(parameterClass)) {
                propValue = Integer.valueOf(RAND.nextInt()).byteValue();
            } else if (short.class.equals(parameterClass) || Short.class.equals(parameterClass)) {
                propValue = Integer.valueOf(RAND.nextInt()).shortValue();
            } else if (Number.class.isAssignableFrom(parameterClass)) {
                propValue = type.valueOf(String.valueOf(RAND.nextInt()));
            } else if (java.util.Date.class.isAssignableFrom(parameterClass) || Calendar.class.isAssignableFrom(parameterClass)) {
                propValue = type.valueOf(String.valueOf(N.currentMillis()));
            } else if (N.isEntity(parameterClass)) {
                propValue = fill(parameterClass);
            } else {
                propValue = type.defaultValue();
            }

            ClassUtil.setPropValue(entity, method, propValue);
        }
    }

    /**
     * Fill the properties of the entity with random values.
     * 
     * @param entityClass entity class with getter/setter methods
     * @return
     */
    public static <T> T fill(Class<T> entityClass) {
        if (N.isEntity(entityClass) == false) {
            throw new AbacusException(entityClass.getCanonicalName() + " is not a valid entity class with property getter/setter method");
        }

        final T entity = N.newInstance(entityClass);

        fill(entity);

        return entity;
    }

    /**
     * Fill the properties of the entity with random values.
     * 
     * @param entityClass entity class with getter/setter methods
     * @param len
     * @return
     */
    public static <T> List<T> fill(Class<T> entityClass, int len) {
        if (N.isEntity(entityClass) == false) {
            throw new AbacusException(entityClass.getCanonicalName() + " is not a valid entity class with property getter/setter method");
        }

        final List<T> resultList = new ArrayList<>(len);

        for (int i = 0; i < len; i++) {
            final T entity = N.newInstance(entityClass);
            fill(entity);
            resultList.add(entity);
        }

        return resultList;
    }

    /**
     * Copies all of the elements from one list into another.  After the
     * operation, the index of each copied element in the destination list
     * will be identical to its index in the source list.  The destination
     * list must be at least as long as the source list.  If it is longer, the
     * remaining elements in the destination list are unaffected. <p>
     *
     * This method runs in linear time.
     * @param  src The source list.
     * @param  dest The destination list.
     * @throws IndexOutOfBoundsException if the destination list is too small
     *         to contain the entire source List.
     * @throws UnsupportedOperationException if the destination list's
     *         list-iterator does not support the <tt>set</tt> operation.
     *
     * @see java.util.Collections#copy(List, List)
     */
    public static <T> void copy(final List<? extends T> src, final List<? super T> dest) {
        if (src.size() > dest.size()) {
            throw new IllegalArgumentException("Source does not fit in dest");
        }

        Collections.copy(dest, src);
    }

    public static <T> void copy(final List<? extends T> src, final int srcPos, final List<? super T> dest, final int destPos, final int length) {
        if (src.size() < srcPos + length) {
            throw new IllegalArgumentException("The size of src list less than " + (srcPos + length));
        }

        if (dest.size() < destPos + length) {
            throw new IllegalArgumentException("The size of dest list less than " + (destPos + length));
        }

        if (src instanceof RandomAccess && dest instanceof RandomAccess) {
            for (int i = 0; i < length; i++) {
                dest.set(destPos + i, src.get(srcPos + i));
            }
        } else {
            final ListIterator<? extends T> srcIterator = src.listIterator();
            final ListIterator<? super T> destIterator = dest.listIterator();

            int idx = 0;
            while (idx < srcPos) {
                srcIterator.next();
                idx++;
            }

            idx = 0;
            while (idx < destPos) {
                destIterator.next();
                idx++;
            }

            for (int i = 0; i < length; i++) {
                destIterator.next();
                destIterator.set(srcIterator.next());
            }
        }
    }

    public static void copy(final boolean[] src, final int srcPos, final boolean[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final char[] src, final int srcPos, final char[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final byte[] src, final int srcPos, final byte[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final short[] src, final int srcPos, final short[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final int[] src, final int srcPos, final int[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final long[] src, final int srcPos, final long[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final float[] src, final int srcPos, final float[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final double[] src, final int srcPos, final double[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    public static void copy(final Object[] src, final int srcPos, final Object[] dest, final int destPos, final int length) {
        if (src.length < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (dest.length < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        if (length < 9) {
            // for same array copy.
            if (destPos > srcPos) {
                for (int i = length - 1; i >= 0; i--) {
                    dest[destPos + i] = src[srcPos + i];
                }
            } else {
                for (int i = 0; i < length; i++) {
                    dest[destPos + i] = src[srcPos + i];
                }
            }
        } else {
            System.arraycopy(src, srcPos, dest, destPos, length);
        }
    }

    /**
     * @see System#arraycopy(Object, int, Object, int, int) is called
     *
     * @param src
     * @param srcPos
     * @param dest
     * @param destPos
     * @param length
     */
    public static void copy(final Object src, final int srcPos, final Object dest, final int destPos, final int length) {
        if (Array.getLength(src) < srcPos + length) {
            throw new IllegalArgumentException("The size of src array less than " + (srcPos + length));
        }

        if (Array.getLength(dest) < destPos + length) {
            throw new IllegalArgumentException("The size of dest array less than " + (destPos + length));
        }

        System.arraycopy(src, srcPos, dest, destPos, length);
    }

    /**
     * @see Arrays#copyOf(boolean[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static boolean[] copyOf(final boolean[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final boolean[] copy = new boolean[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(char[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static char[] copyOf(final char[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final char[] copy = new char[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(byte[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static byte[] copyOf(final byte[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final byte[] copy = new byte[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(short[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static short[] copyOf(final short[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final short[] copy = new short[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(int[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static int[] copyOf(final int[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final int[] copy = new int[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(long[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static long[] copyOf(final long[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final long[] copy = new long[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(float[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static float[] copyOf(final float[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final float[] copy = new float[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(double[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static double[] copyOf(final double[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        final double[] copy = new double[newLength];

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOf(Object[], int)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static <T> T[] copyOf(final T[] original, final int newLength) {
        if (newLength == original.length) {
            return original.clone();
        }

        return (T[]) copyOf(original, newLength, original.getClass());
    }

    /**
     * @see Arrays#copyOf(Object[], int, Class)
     *
     * @param original
     * @param newLength
     * @return
     */
    public static <T, U> T[] copyOf(final U[] original, final int newLength, final Class<? extends T[]> newType) {
        final T[] copy = Object[].class.equals(newType) ? (T[]) new Object[newLength] : (T[]) N.newArray(newType.getComponentType(), newLength);

        if (N.notNullOrEmpty(original)) {
            copy(original, 0, copy, 0, Math.min(original.length, newLength));
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(boolean[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static boolean[] copyOfRange(final boolean[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final boolean[] copy = new boolean[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static boolean[] copyOfRange(final boolean[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final boolean[] copy = new boolean[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(char[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static char[] copyOfRange(final char[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final char[] copy = new char[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static char[] copyOfRange(final char[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_CHAR_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final char[] copy = new char[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(byte[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static byte[] copyOfRange(final byte[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final byte[] copy = new byte[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static byte[] copyOfRange(final byte[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_BYTE_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final byte[] copy = new byte[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(short[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static short[] copyOfRange(final short[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final short[] copy = new short[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static short[] copyOfRange(final short[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_SHORT_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final short[] copy = new short[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(int[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static int[] copyOfRange(final int[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final int[] copy = new int[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * <pre>
     * <code>
     * int[] a = { 0, 1, 2, 3, 4, 5 };
     * N.copyOfRange(a, 1, 5, 1)); // [1, 2, 3, 4]
     * N.copyOfRange(a, 1, 5, 2); // [1, 3]
     * 
     * N.copyOfRange(a, 5, 1, -1); // [5, 4, 3, 2]
     * N.copyOfRange(a, 5, 1, -2); // [5, 3]
     * N.copyOfRange(a, 5, -1, -1); // [5, 4, 3, 2, 1, 0]
     * N.copyOfRange(a, 6, -1, -1); // [5, 4, 3, 2, 1, 0]
     * </code>
     * </pre>
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     */
    public static int[] copyOfRange(final int[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_INT_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final int[] copy = new int[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(long[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static long[] copyOfRange(final long[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final long[] copy = new long[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static long[] copyOfRange(final long[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_LONG_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final long[] copy = new long[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(float[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static float[] copyOfRange(final float[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final float[] copy = new float[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static float[] copyOfRange(final float[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final float[] copy = new float[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(double[], int, int)
     *
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static double[] copyOfRange(final double[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        final int newLength = to - from;
        final double[] copy = new double[newLength];
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static double[] copyOfRange(final double[] original, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final double[] copy = new double[len];

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(T[], int, int)
     * @param original
     * @param from
     * @param to
     * @return
     */
    public static <T> T[] copyOfRange(final T[] original, final int from, final int to) {
        if (from == 0 && to == original.length) {
            return original.clone();
        }

        return copyOfRange(original, from, to, (Class<T[]>) original.getClass());
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     */
    public static <T> T[] copyOfRange(final T[] original, final int from, final int to, final int step) {
        return copyOfRange(original, from, to, step, (Class<T[]>) original.getClass());
    }

    /**
     * {@link Arrays#copyOfRange(Object[], int, int, Class)}
     *
     * @param original
     * @param from
     * @param to
     * @param newType
     * @return
     */
    public static <T, U> T[] copyOfRange(final U[] original, final int from, final int to, final Class<? extends T[]> newType) {
        final int newLength = to - from;
        final T[] copy = Object[].class.equals(newType) ? (T[]) new Object[newLength] : (T[]) N.newArray(newType.getComponentType(), newLength);
        copy(original, from, copy, 0, Math.min(original.length - from, newLength));
        return copy;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param original
     * @param from
     * @param to
     * @param step
     * @return
     * @see N#copyOfRange(int[], int, int, int)
     */
    public static <T> T[] copyOfRange(final T[] original, int from, final int to, final int step, final Class<? extends T[]> newType) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, original.length);

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return Object[].class.equals(newType) ? (T[]) new Object[0] : (T[]) N.newArray(newType.getComponentType(), 0);
        }

        if (step == 1) {
            return copyOfRange(original, from, to);
        }

        from = from > to ? N.min(original.length - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        final T[] copy = Object[].class.equals(newType) ? (T[]) new Object[len] : (T[]) N.newArray(newType.getComponentType(), len);

        for (int i = 0, j = from; i < len; i++, j += step) {
            copy[i] = original[j];
        }

        return copy;
    }

    /**
     * @see Arrays#copyOfRange(T[], int, int)
     * @param c
     * @param from
     * @param to
     * @return
     */
    public static <T> List<T> copyOfRange(final List<T> c, final int from, final int to) {
        N.checkFromToIndex(from, to, c.size());

        final List<T> result = new ArrayList<>(to - from);
        result.addAll(c.subList(from, to));
        return result;
    }

    /**
     * Copy all the elements in <code>original</code>, through <code>to</code>-<code>from</code>, by <code>step</code>.
     * 
     * @param c
     * @param from
     * @param to
     * @param step
     * @return
     */
    public static <T> List<T> copyOfRange(final List<T> c, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, c.size());

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return new ArrayList<>();
        }

        if (step == 1) {
            return copyOfRange(c, from, to);
        }

        from = from > to ? N.min(c.size() - 1, from) : from;
        final int len = (to - from) / step + ((to - from) % step == 0 ? 0 : 1);
        List<T> result = null;

        if (c instanceof RandomAccess) {
            result = new ArrayList<>(len);

            for (int i = 0, j = from; i < len; i++, j += step) {
                result.add(c.get(j));
            }
        } else {
            final T[] a = (T[]) c.toArray();
            result = createList(N.copyOfRange(a, from, to, step));
        }

        return result;
    }

    /**
     * Create an array list by initializing its elements data with the specified array <code>a</code>.
     * The returned list may share the same elements with the specified array <code>a</code>.
     * That's to say any change on the List/Array will affect the Array/List.
     * 
     * @param a
     * @return
     */
    @SafeVarargs
    private static <T> List<T> createList(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        if (N.isListElementDataFieldSettable && N.listElementDataField != null && N.listSizeField != null) {
            final List<T> list = new ArrayList<>();

            try {
                N.listElementDataField.set(list, a);
                N.listSizeField.set(list, a.length);

                return list;
            } catch (Throwable e) {
                // ignore;
                N.isListElementDataFieldSettable = false;
            }
        }

        return N.asList(a);
    }

    /**
    *
    * @param str
    * @param from
    * @param to
    * @return
    */
    public static String copyOfRange(final String str, final int from, final int to) {
        return str.substring(from, to);
    }

    /**
    *
    * @param str
    * @param from
    * @param to
    * @param step
    * @return
    * @see N#copyOfRange(int[], int, int, int)
    */
    public static String copyOfRange(final String str, int from, final int to, final int step) {
        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, str.length());

        if (step == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (from == to || from < to != step > 0) {
            return N.EMPTY_STRING;
        }

        if (step == 1) {
            return copyOfRange(str, from, to);
        }

        return N.newString(copyOfRange(N.getCharsForReadOnly(str), from, to, step), true);
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static boolean[] clone(final boolean[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static char[] clone(final char[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static byte[] clone(final byte[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static short[] clone(final short[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static int[] clone(final int[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static long[] clone(final long[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static float[] clone(final float[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static double[] clone(final double[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static <T> T[] clone(final T[] original) {
        if (original == null) {
            return null;
        }

        return original.clone();
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static boolean[][] clone(final boolean[][] original) {
        if (original == null) {
            return null;
        }

        final boolean[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static char[][] clone(final char[][] original) {
        if (original == null) {
            return null;
        }

        final char[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static byte[][] clone(final byte[][] original) {
        if (original == null) {
            return null;
        }

        final byte[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static short[][] clone(final short[][] original) {
        if (original == null) {
            return null;
        }

        final short[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static int[][] clone(final int[][] original) {
        if (original == null) {
            return null;
        }

        final int[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static long[][] clone(final long[][] original) {
        if (original == null) {
            return null;
        }

        final long[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static float[][] clone(final float[][] original) {
        if (original == null) {
            return null;
        }

        final float[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static double[][] clone(final double[][] original) {
        if (original == null) {
            return null;
        }

        final double[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static <T> T[][] clone(final T[][] original) {
        if (original == null) {
            return null;
        }

        final T[][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static boolean[][][] clone(final boolean[][][] original) {
        if (original == null) {
            return null;
        }

        final boolean[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static char[][][] clone(final char[][][] original) {
        if (original == null) {
            return null;
        }

        final char[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static byte[][][] clone(final byte[][][] original) {
        if (original == null) {
            return null;
        }

        final byte[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static short[][][] clone(final short[][][] original) {
        if (original == null) {
            return null;
        }

        final short[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static int[][][] clone(final int[][][] original) {
        if (original == null) {
            return null;
        }

        final int[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static long[][][] clone(final long[][][] original) {
        if (original == null) {
            return null;
        }

        final long[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static float[][][] clone(final float[][][] original) {
        if (original == null) {
            return null;
        }

        final float[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static double[][][] clone(final double[][][] original) {
        if (original == null) {
            return null;
        }

        final double[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    /**
     * Clone the original array and its sub arrays. <code>null</code> is returned if the input array is <code>null</code>.
     * 
     * @param original
     * @return
     */
    public static <T> T[][][] clone(final T[][][] original) {
        if (original == null) {
            return null;
        }

        final T[][][] cp = original.clone();

        for (int i = 0, len = cp.length; i < len; i++) {
            cp[i] = clone(original[i]);
        }

        return cp;
    }

    public static void sort(final boolean[] a) {
        Array.sort(a);
    }

    public static void sort(final char[] a) {
        Array.sort(a);
    }

    public static void sort(final char[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final byte[] a) {
        Array.sort(a);
    }

    public static void sort(final byte[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final short[] a) {
        Array.sort(a);
    }

    public static void sort(final short[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final int[] a) {
        Array.sort(a);
    }

    public static void sort(final int[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final long[] a) {
        Array.sort(a);
    }

    public static void sort(final long[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final float[] a) {
        Array.sort(a);
    }

    public static void sort(final float[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final double[] a) {
        Array.sort(a);
    }

    public static void sort(final double[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static void sort(final Object[] a) {
        Array.sort(a);
    }

    public static void sort(final Object[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
    }

    public static <T> void sort(final T[] a, final Comparator<? super T> cmp) {
        Array.sort(a, cmp);
    }

    public static <T> void sort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        Array.sort(a, fromIndex, toIndex, cmp);
    }

    public static <T extends Comparable<? super T>> void sort(final List<? extends T> c) {
        Array.sort(c);
    }

    public static <T extends Comparable<? super T>> void sort(final List<? extends T> c, final int fromIndex, final int toIndex) {
        Array.sort(c, fromIndex, toIndex);
    }

    public static <T> void sort(final List<? extends T> c, final Comparator<? super T> cmp) {
        Array.sort(c, cmp);
    }

    public static <T> void sort(final List<? extends T> c, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        Array.sort(c, fromIndex, toIndex, cmp);
    }

    public static void parallelSort(final char[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final char[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final byte[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final byte[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final short[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final short[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final int[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final int[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final long[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final long[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final float[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final float[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final double[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final double[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static void parallelSort(final Object[] a) {
        Array.parallelSort(a);
    }

    public static void parallelSort(final Object[] a, final int fromIndex, final int toIndex) {
        Array.parallelSort(a, fromIndex, toIndex);
    }

    public static <T> void parallelSort(final T[] a, final Comparator<? super T> cmp) {
        Array.parallelSort(a, cmp);
    }

    public static <T> void parallelSort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        Array.parallelSort(a, fromIndex, toIndex, cmp);
    }

    public static <T extends Comparable<? super T>> void parallelSort(final List<? extends T> c) {
        Array.parallelSort(c);
    }

    public static <T extends Comparable<? super T>> void parallelSort(final List<? extends T> c, final int fromIndex, final int toIndex) {
        Array.parallelSort(c, fromIndex, toIndex);
    }

    public static <T> void parallelSort(final List<? extends T> c, final Comparator<? super T> cmp) {
        Array.parallelSort(c, cmp);
    }

    public static <T> void parallelSort(final List<? extends T> c, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        Array.parallelSort(c, fromIndex, toIndex, cmp);
    }

    public static void reverseSort(final boolean[] a) {
        Array.reverseSort(a);
    }

    public static void reverseSort(final char[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final char[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final byte[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final byte[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final short[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final short[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final int[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final int[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final long[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final long[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final float[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final float[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final double[] a) {
        Array.sort(a);
        reverse(a);
    }

    public static void reverseSort(final double[] a, final int fromIndex, final int toIndex) {
        Array.sort(a, fromIndex, toIndex);
        reverse(a, fromIndex, toIndex);
    }

    public static void reverseSort(final Object[] a) {
        //        Array.sort(a);
        //        reverse(a);

        sort(a, Fn.reversedOrder());
    }

    public static void reverseSort(final Object[] a, final int fromIndex, final int toIndex) {
        //        Array.sort(a, fromIndex, toIndex);
        //        reverse(a, fromIndex, toIndex);

        sort(a, fromIndex, toIndex, Fn.reversedOrder());
    }

    public static <T extends Comparable<? super T>> void reverseSort(final List<? extends T> c) {
        //        Array.sort(c);
        //        reverse(c);

        sort(c, Fn.reversedOrder());
    }

    public static <T extends Comparable<? super T>> void reverseSort(final List<? extends T> c, final int fromIndex, final int toIndex) {
        //        Array.sort(c, fromIndex, toIndex);
        //        reverse(c, fromIndex, toIndex);

        sort(c, fromIndex, toIndex, Fn.reversedOrder());
    }

    /*
    public static void bucketSort(final char[] a) {
        M.bucketSort(a);
    }
    
    public static void bucketSort(final char[] a, final int fromIndex, final int toIndex) {
        M.bucketSort(a, fromIndex, toIndex);
    }
    
    public static void bucketSort(final byte[] a) {
        M.bucketSort(a);
    }
    
    public static void bucketSort(final byte[] a, final int fromIndex, final int toIndex) {
        M.bucketSort(a, fromIndex, toIndex);
    }
    
    public static void bucketSort(final short[] a) {
        M.bucketSort(a);
    }
    
    public static void bucketSort(final short[] a, final int fromIndex, final int toIndex) {
        M.bucketSort(a, fromIndex, toIndex);
    }
    */

    public static void bucketSort(final int[] a) {
        Array.bucketSort(a);
    }

    public static void bucketSort(final int[] a, final int fromIndex, final int toIndex) {
        Array.bucketSort(a, fromIndex, toIndex);
    }

    public static void bucketSort(final long[] a) {
        Array.bucketSort(a);
    }

    public static void bucketSort(final long[] a, final int fromIndex, final int toIndex) {
        Array.bucketSort(a, fromIndex, toIndex);
    }

    public static void bucketSort(final float[] a) {
        Array.bucketSort(a);
    }

    public static void bucketSort(final float[] a, final int fromIndex, final int toIndex) {
        Array.bucketSort(a, fromIndex, toIndex);
    }

    public static void bucketSort(final double[] a) {
        Array.bucketSort(a);
    }

    public static void bucketSort(final double[] a, final int fromIndex, final int toIndex) {
        Array.bucketSort(a, fromIndex, toIndex);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     *   
     * @param a
     */
    public static void bucketSort(final Object[] a) {
        Array.bucketSort(a);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param a the elements in the array must implements the <code>Comparable</code> interface.
     * @param fromIndex
     * @param toIndex
     */
    public static void bucketSort(final Object[] a, final int fromIndex, final int toIndex) {
        Array.bucketSort(a, fromIndex, toIndex);
    }

    public static <T> void bucketSort(final T[] a, final Comparator<? super T> cmp) {
        Array.bucketSort(a, cmp);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param cmp
     */
    public static <T> void bucketSort(final T[] a, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        Array.bucketSort(a, fromIndex, toIndex, cmp);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     */
    public static <T extends Comparable<T>> void bucketSort(final List<T> c) {
        Array.bucketSort(c);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     */
    public static <T extends Comparable<T>> void bucketSort(final List<T> c, final int fromIndex, final int toIndex) {
        Array.bucketSort(c, fromIndex, toIndex);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     * @param cmp
     */
    public static <T> void bucketSort(final List<? extends T> c, final Comparator<? super T> cmp) {
        Array.bucketSort(c, cmp);
    }

    /**
     * Note: All the objects with same value will be replaced with first element with the same value.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param cmp
     */
    public static <T> void bucketSort(final List<? extends T> c, final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        Array.bucketSort(c, fromIndex, toIndex, cmp);
    }

    /**
     * {@link Arrays#binarySearch(boolean[], boolean)}
     *
     * @param a
     * @param key
     * @return
     */
    static int binarySearch(final boolean[] a, final boolean key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(char[], char)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final char[] a, final char key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(char[], int, int, char)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final char[] a, final int fromIndex, final int toIndex, final char key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(byte[], byte)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final byte[] a, final byte key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(byte[], int, int, byte)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final byte[] a, final int fromIndex, final int toIndex, final byte key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(short[], short)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final short[] a, final short key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(short[], int, int, short)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final short[] a, final int fromIndex, final int toIndex, final short key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(int[], int)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final int[] a, final int key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(int[], int, int, int)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final int[] a, final int fromIndex, final int toIndex, final int key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(long[], long)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final long[] a, final long key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(long[], int, int, long)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final long[] a, final int fromIndex, final int toIndex, final long key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(float[], float)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final float[] a, final float key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(float[], int, int, float)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final float[] a, final int fromIndex, final int toIndex, final float key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(double[], double)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final double[] a, final double key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(double[], int, int, double)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final double[] a, final int fromIndex, final int toIndex, final double key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(Object[], Object)}
     *
     * @param a
     * @param key
     * @return
     */
    public static int binarySearch(final Object[] a, final Object key) {
        return Array.binarySearch(a, key);
    }

    /**
     * {@link Arrays#binarySearch(Object[], int, int, Object)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public static int binarySearch(final Object[] a, final int fromIndex, final int toIndex, final Object key) {
        return Array.binarySearch(a, fromIndex, toIndex, key);
    }

    /**
     * {@link Arrays#binarySearch(Object[], Object, Comparator)}
     *
     * @param a
     * @param key
     * @param cmp
     * @return
     */
    public static <T> int binarySearch(final T[] a, final T key, final Comparator<? super T> cmp) {
        return Array.binarySearch(a, key, cmp);
    }

    /**
     * {@link Arrays#binarySearch(Object[], int, int, Object, Comparator)}
     *
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param key
     * @param c
     * @return
     */
    public static <T> int binarySearch(final T[] a, final int fromIndex, final int toIndex, final T key, final Comparator<? super T> cmp) {
        return Array.binarySearch(a, fromIndex, toIndex, key, cmp);
    }

    /**
     * {@link Collections#binarySearch(List, Object)}
     *
     * @param items
     * @param key
     * @return
     */
    public static <T extends Comparable<? super T>> int binarySearch(final List<? extends T> c, final T key) {
        return Array.binarySearch(c, key);
    }

    public static <T extends Comparable<? super T>> int binarySearch(final List<? extends T> c, final int fromIndex, final int toIndex, final T key) {
        return Array.binarySearch(c, fromIndex, toIndex, key);
    }

    public static <T> int binarySearch(final List<? extends T> c, final T key, final Comparator<? super T> cmp) {
        return Array.binarySearch(c, key, cmp);
    }

    /**
     *
     * @param c
     * @param key
     * @param fromIndex
     * @param toIndex
     * @param cmp
     * @return
     * @see Collections#binarySearch(List, Object, Comparator)
     */
    public static <T> int binarySearch(final List<? extends T> c, final int fromIndex, final int toIndex, final T key, final Comparator<? super T> cmp) {
        return Array.binarySearch(c, fromIndex, toIndex, key, cmp);
    }

    //    /**
    //     * 
    //     * <code>findFirst("3[a2[c]]2[a]", "[", "]") = "a2[c"</code>
    //     * 
    //     * @param str
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     */
    //    public static Optional<String> findFirst(String str, String prefix, String postfix) {
    //        return findFirst(String.class, str, prefix, postfix);
    //    }
    //
    //    /**
    //     * 
    //     * <code>findFirst("3[a2[c]]2[a]", "[", "]") = "a2[c"</code>
    //     * 
    //     * @param str
    //     * @param fromIndex
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     */
    //    public static Optional<String> findFirst(String str, int fromIndex, String prefix, String postfix) {
    //        return findFirst(String.class, str, fromIndex, prefix, postfix);
    //    }
    //
    //    /**
    //     * 
    //     * <code>findFirst("3[a2[c]]2[a]", "[", "]") = "a2[c"</code>
    //     * 
    //     * @param targetClass
    //     * @param str
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     */
    //    public static <T> Optional<T> findFirst(Class<T> targetClass, String str, String prefix, String postfix) {
    //        return findFirst(targetClass, str, 0, prefix, postfix);
    //    }
    //
    //    /**
    //     * Returns first substring between the specified <code>prefix</code> and </code>postfix</code>, or default value if there is no substring found.
    //     * 
    //     * <code>findFirst("3[a2[c]]2[a]", "[", "]") = "a2[c"</code>
    //     * 
    //     * @param targetClass
    //     * @param str
    //     * @param fromIndex
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     * @see String#indexOf(String, int)
    //     */
    //    public static <T> Optional<T> findFirst(Class<T> targetClass, String str, int fromIndex, String prefix, String postfix) {
    //        final Type<T> type = typeOf(targetClass);
    //
    //        if (N.isNullOrEmpty(str)) {
    //            return Optional.empty();
    //        }
    //
    //        int beginIndex = N.isNullOrEmpty(prefix) ? 0 : str.indexOf(prefix, fromIndex);
    //
    //        if (beginIndex < 0) {
    //            return Optional.empty();
    //        }
    //
    //        beginIndex += N.isNullOrEmpty(prefix) ? 0 : prefix.length();
    //
    //        int endIndex = N.isNullOrEmpty(postfix) ? str.length() : str.indexOf(postfix, beginIndex);
    //
    //        if (endIndex < 0) {
    //            return Optional.empty();
    //        }
    //
    //        return Optional.of(N.as(type, str.subSequence(beginIndex, endIndex)));
    //    }
    //
    //    /**
    //     * 
    //     * <code>findLast("3[a2[c]]2a]", "[", "]") = "c]]2a"</code>
    //     * 
    //     * @param str
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     */
    //    public static Optional<String> findLast(String str, String prefix, String postfix) {
    //        return findLast(String.class, str, prefix, postfix);
    //    }
    //
    //    /**
    //     * 
    //     * <code>findLast("3[a2[c]]2a]", "[", "]") = "c]]2a"</code>
    //     * 
    //     * @param str
    //     * @param fromIndex
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     */
    //    public static Optional<String> findLast(String str, int fromIndex, String prefix, String postfix) {
    //        return findLast(String.class, str, fromIndex, prefix, postfix);
    //    }
    //
    //    /**
    //     * 
    //     * <code>findLast("3[a2[c]]2a]", "[", "]") = "c]]2a"</code>
    //     * 
    //     * @param targetClass
    //     * @param str
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     */
    //    public static <T> Optional<T> findLast(Class<T> targetClass, String str, String prefix, String postfix) {
    //        return findLast(targetClass, str, N.isNullOrEmpty(str) ? 0 : str.length(), prefix, postfix);
    //    }
    //
    //    /**
    //     * Returns last substring between the specified <code>prefix</code> and </code>postfix</code>, or default value if there is no substring found.
    //     * 
    //     * <code>findLast("3[a2[c]]2a]", "[", "]") = "c]]2a"</code>
    //     * 
    //     * @param targetClass
    //     * @param str
    //     * @param fromIndex
    //     * @param prefix
    //     * @param postfix
    //     * @return
    //     * @see String#lastIndexOf(String, int)
    //     */
    //    public static <T> Optional<T> findLast(Class<T> targetClass, String str, int fromIndex, String prefix, String postfix) {
    //        final Type<T> type = typeOf(targetClass);
    //
    //        if (N.isNullOrEmpty(str)) {
    //            return Optional.empty();
    //        }
    //
    //        int endIndex = N.isNullOrEmpty(postfix) ? str.length() : str.lastIndexOf(postfix, fromIndex);
    //
    //        if (endIndex < 0 || (N.notNullOrEmpty(prefix) && endIndex < prefix.length())) {
    //            return Optional.empty();
    //        }
    //
    //        int beginIndex = N.isNullOrEmpty(prefix) ? 0 : str.lastIndexOf(prefix, endIndex - prefix.length());
    //
    //        if (beginIndex < 0) {
    //            return Optional.empty();
    //        }
    //
    //        beginIndex += N.isNullOrEmpty(prefix) ? 0 : prefix.length();
    //
    //        return Optional.of(N.as(type, str.subSequence(beginIndex, endIndex)));
    //    }

    /**
     * Returns an empty <code>Optional</code> if {@code inclusiveBeginIndex < 0 || exclusiveEndIndex < 0 || inclusiveBeginIndex > exclusiveEndIndex}, 
     * otherwise an {@code Optional} with String value: {@code str.substring(exclusiveBeginIndex, exclusiveEndIndex)} is returned.
     * 
     * @param str
     * @param inclusiveBeginIndex
     * @param exclusiveEndIndex
     * @return
     */
    public static Optional<String> substring(String str, int inclusiveBeginIndex, int exclusiveEndIndex) {
        if (inclusiveBeginIndex < 0 || exclusiveEndIndex < 0 || inclusiveBeginIndex > exclusiveEndIndex) {
            return Optional.<String> empty();
        }

        return Optional.of(str.substring(inclusiveBeginIndex, exclusiveEndIndex));
    }

    /**
     * Returns an empty <code>Optional</code> if {@code inclusiveBeginIndex < 0}, 
     * otherwise an {@code Optional} with String value: {@code str.substring(inclusiveBeginIndex)} is returned.
     * 
     * @param str
     * @param inclusiveBeginIndex
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, int inclusiveBeginIndex) {
        if (inclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return Optional.of(str.substring(inclusiveBeginIndex));
    }

    /**
     * Returns an empty <code>Optional</code> if {@code N.isNullOrEmpty(str) || str.indexOf(delimiterOfInclusiveBeginIndex) < 0}, 
     * otherwise an {@code Optional} with String value: {@code str.substring(str.indexOf(delimiterOfInclusiveBeginIndex))} is returned.
     * 
     * @param str
     * @param delimiterOfInclusiveBeginIndex {@code inclusiveBeginIndex <- str.indexOf(delimiterOfInclusiveBeginIndex)}
     * @return
     * @see #substring(String, int)
     */
    public static Optional<String> substring(String str, char delimiterOfInclusiveBeginIndex) {
        if (N.isNullOrEmpty(str)) {
            return Optional.<String> empty();
        }

        return substring(str, str.indexOf(delimiterOfInclusiveBeginIndex));
    }

    /**
     * Returns an empty <code>Optional</code> if {@code N.isNullOrEmpty(str) || str.indexOf(delimiterOfInclusiveBeginIndex) < 0}, 
     * otherwise an {@code Optional} with String value: {@code str.substring(str.indexOf(delimiterOfInclusiveBeginIndex))} is returned.
     * 
     * @param str
     * @param delimiterOfInclusiveBeginIndex {@code inclusiveBeginIndex <- str.indexOf(delimiterOfInclusiveBeginIndex)}
     * @return
     * @see #substring(String, int)
     */
    public static Optional<String> substring(String str, String delimiterOfInclusiveBeginIndex) {
        if (N.isNullOrEmpty(str)) {
            return Optional.<String> empty();
        }

        return substring(str, str.indexOf(delimiterOfInclusiveBeginIndex));
    }

    /**
     * 
     * @param str
     * @param inclusiveBeginIndex
     * @param delimiterOfExclusiveEndIndex {@code exclusiveEndIndex <- str.indexOf(delimiterOfExclusiveEndIndex, inclusiveBeginIndex + 1) if inclusiveBeginIndex >= 0}
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, int inclusiveBeginIndex, char delimiterOfExclusiveEndIndex) {
        if (inclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return substring(str, inclusiveBeginIndex, str.indexOf(delimiterOfExclusiveEndIndex, inclusiveBeginIndex + 1));
    }

    /**
     * 
     * @param str
     * @param inclusiveBeginIndex
     * @param delimiterOfExclusiveEndIndex {@code exclusiveEndIndex <- str.indexOf(delimiterOfExclusiveEndIndex, inclusiveBeginIndex + 1) if inclusiveBeginIndex >= 0}
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, int inclusiveBeginIndex, String delimiterOfExclusiveEndIndex) {
        if (inclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return substring(str, inclusiveBeginIndex, str.indexOf(delimiterOfExclusiveEndIndex, inclusiveBeginIndex + 1));
    }

    /**
     * 
     * @param str
     * @param inclusiveBeginIndex
     * @param funcOfExclusiveEndIndex {@code exclusiveEndIndex <- funcOfExclusiveEndIndex.applyAsInt(inclusiveBeginIndex) if inclusiveBeginIndex >= 0}
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, int inclusiveBeginIndex, IntUnaryOperator funcOfExclusiveEndIndex) {
        if (inclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return substring(str, inclusiveBeginIndex, funcOfExclusiveEndIndex.applyAsInt(inclusiveBeginIndex));
    }

    /**
     * 
     * @param str
     * @param delimiterOfInclusiveBeginIndex {@code inclusiveBeginIndex <- str.lastIndexOf(delimiterOfInclusiveBeginIndex, exclusiveEndIndex - 1) if exclusiveEndIndex > 0}
     * @param exclusiveEndIndex
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, char delimiterOfInclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveEndIndex <= 0) {
            return Optional.<String> empty();
        }

        return substring(str, str.lastIndexOf(delimiterOfInclusiveBeginIndex, exclusiveEndIndex - 1), exclusiveEndIndex);
    }

    /**
     * 
     * @param str
     * @param delimiterOfInclusiveBeginIndex {@code inclusiveBeginIndex <- str.lastIndexOf(delimiterOfInclusiveBeginIndex, exclusiveEndIndex - 1) if exclusiveEndIndex > 0}
     * @param exclusiveEndIndex
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, String delimiterOfInclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveEndIndex <= 0) {
            return Optional.<String> empty();
        }

        return substring(str, str.lastIndexOf(delimiterOfInclusiveBeginIndex, exclusiveEndIndex - 1), exclusiveEndIndex);
    }

    /**
     * 
     * @param str
     * @param funcOfInclusiveBeginIndex {@code inclusiveBeginIndex <- funcOfInclusiveBeginIndex.applyAsInt(exclusiveEndIndex)) if exclusiveEndIndex > 0}
     * @param exclusiveEndIndex
     * @return
     * @see #substring(String, int, int)
     */
    public static Optional<String> substring(String str, IntUnaryOperator funcOfInclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveEndIndex <= 0) {
            return Optional.<String> empty();
        }

        return substring(str, funcOfInclusiveBeginIndex.applyAsInt(exclusiveEndIndex), exclusiveEndIndex);
    }

    /**
     * Returns an empty <code>Optional</code> if {@code exclusiveBeginIndex < 0 || exclusiveEndIndex < 0 || exclusiveBeginIndex >= exclusiveEndIndex}, 
     * otherwise an {@code Optional} with String value: {@code str.substring(exclusiveBeginIndex + 1, exclusiveEndIndex)} is returned.
     * 
     * @param str
     * @param exclusiveBeginIndex
     * @param exclusiveEndIndex
     * @return
     */
    public static Optional<String> between(String str, int exclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveBeginIndex < 0 || exclusiveEndIndex < 0 || exclusiveBeginIndex >= exclusiveEndIndex) {
            return Optional.<String> empty();
        }

        return Optional.of(str.substring(exclusiveBeginIndex + 1, exclusiveEndIndex));
    }

    /**
     * 
     * @param str
     * @param exclusiveBeginIndex
     * @param delimiterOfExclusiveEndIndex {@code exclusiveEndIndex <- str.indexOf(delimiterOfExclusiveEndIndex, beginIndex + 1) if exclusiveBeginIndex >= 0}
     * @return
     * @see #between(String, int, int)
     */
    public static Optional<String> between(String str, int exclusiveBeginIndex, char delimiterOfExclusiveEndIndex) {
        if (exclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return between(str, exclusiveBeginIndex, str.indexOf(delimiterOfExclusiveEndIndex, exclusiveBeginIndex + 1));
    }

    /**
     * 
     * @param str
     * @param exclusiveBeginIndex
     * @param delimiterOfExclusiveEndIndex {@code exclusiveEndIndex <- str.indexOf(delimiterOfExclusiveEndIndex, beginIndex + 1) if exclusiveBeginIndex >= 0}
     * @return
     * @see #between(String, int, int)
     */
    public static Optional<String> between(String str, int exclusiveBeginIndex, String delimiterOfExclusiveEndIndex) {
        if (exclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return between(str, exclusiveBeginIndex, str.indexOf(delimiterOfExclusiveEndIndex, exclusiveBeginIndex + 1));
    }

    /**
     * 
     * @param str
     * @param exclusiveBeginIndex
     * @param funcOfExclusiveEndIndex {@code exclusiveEndIndex <- funcOfExclusiveEndIndex.applyAsInt(inclusiveBeginIndex) if inclusiveBeginIndex >= 0}
     * @return
     * @see #between(String, int, int)
     */
    public static Optional<String> between(String str, int exclusiveBeginIndex, IntUnaryOperator funcOfExclusiveEndIndex) {
        if (exclusiveBeginIndex < 0) {
            return Optional.<String> empty();
        }

        return between(str, exclusiveBeginIndex, funcOfExclusiveEndIndex.applyAsInt(exclusiveBeginIndex));
    }

    /**
     * 
     * @param str
     * @param delimiterOfExclusiveBeginIndex {@code exclusiveBeginIndex <- str.lastIndexOf(delimiterOfExclusiveBeginIndex, exclusiveEndIndex - 1) if exclusiveEndIndex > 0}
     * @param exclusiveEndIndex
     * @return
     * @see #between(String, int, int)
     */
    public static Optional<String> between(String str, char delimiterOfExclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveEndIndex <= 0) {
            return Optional.<String> empty();
        }

        return between(str, str.lastIndexOf(delimiterOfExclusiveBeginIndex, exclusiveEndIndex - 1), exclusiveEndIndex);
    }

    /**
     * 
     * @param str
     * @param delimiterOfExclusiveBeginIndex {@code exclusiveBeginIndex <- str.lastIndexOf(delimiterOfExclusiveBeginIndex, exclusiveEndIndex - 1) if exclusiveEndIndex > 0}
     * @param exclusiveEndIndex
     * @return
     * @see #between(String, int, int)
     */
    public static Optional<String> between(String str, String delimiterOfExclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveEndIndex <= 0) {
            return Optional.<String> empty();
        }

        return between(str, str.lastIndexOf(delimiterOfExclusiveBeginIndex, exclusiveEndIndex - 1), exclusiveEndIndex);
    }

    /**
     * 
     * @param str
     * @param funcOfExclusiveBeginIndex {@code exclusiveBeginIndex <- funcOfExclusiveBeginIndex.applyAsInt(exclusiveEndIndex)) if exclusiveEndIndex > 0}
     * @param exclusiveEndIndex
     * @return
     * @see #between(String, int, int)
     */
    public static Optional<String> between(String str, IntUnaryOperator funcOfExclusiveBeginIndex, int exclusiveEndIndex) {
        if (exclusiveEndIndex <= 0) {
            return Optional.<String> empty();
        }

        return between(str, funcOfExclusiveBeginIndex.applyAsInt(exclusiveEndIndex), exclusiveEndIndex);
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<IntPair> findAllIndices(final String str, final char prefix, final char postfix) {
        return N.isNullOrEmpty(str) ? new ArrayList<IntPair>() : findAllIndices(str, 0, str.length(), prefix, postfix);
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param fromIndex
     * @param toIndex
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<IntPair> findAllIndices(final String str, final int fromIndex, final int toIndex, final char prefix, final char postfix) {
        N.checkFromToIndex(fromIndex, toIndex, str == null ? 0 : str.length());

        final List<IntPair> res = new ArrayList<>();

        if (N.isNullOrEmpty(str)) {
            return res;
        }

        int idx = str.indexOf(prefix, fromIndex);

        if (idx < 0) {
            return res;
        }

        final char[] chs = N.getCharsForReadOnly(str);
        final Deque<Integer> queue = new LinkedList<>();

        for (int i = idx; i < toIndex; i++) {
            if (chs[i] == prefix) {
                queue.push(i + 1);
            } else if (chs[i] == postfix && queue.size() > 0) {
                final int startIndex = queue.pop();

                if (res.size() > 0 && startIndex < res.get(res.size() - 1)._1) {
                    while (res.size() > 0 && startIndex < res.get(res.size() - 1)._1) {
                        res.remove(res.size() - 1);
                    }
                }

                res.add(IntPair.of(startIndex, i));
            }
        }

        return res;
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<IntPair> findAllIndices(final String str, final String prefix, final String postfix) {
        return N.isNullOrEmpty(str) ? new ArrayList<IntPair>() : findAllIndices(str, 0, str.length(), prefix, postfix);
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param fromIndex
     * @param toIndex
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<IntPair> findAllIndices(final String str, final int fromIndex, final int toIndex, final String prefix, final String postfix) {
        N.checkFromToIndex(fromIndex, toIndex, str == null ? 0 : str.length());

        final List<IntPair> res = new ArrayList<>();

        if (N.isNullOrEmpty(str)) {
            return res;
        }

        int idx = str.indexOf(prefix, fromIndex);

        if (idx < 0) {
            return res;
        }

        final Deque<Integer> queue = new LinkedList<>();
        queue.add(idx + prefix.length());
        int next = -1;

        for (int i = idx + prefix.length(), len = toIndex; i < len;) {
            if (queue.size() == 0) {
                idx = next >= i ? next : str.indexOf(prefix, i);

                if (idx < 0) {
                    break;
                } else {
                    queue.add(idx + prefix.length());
                    i = idx + prefix.length();
                }
            }

            idx = str.indexOf(postfix, i);

            if (idx < 0) {
                break;
            } else {
                final int endIndex = idx;
                idx = res.size() > 0 ? Math.max(res.get(res.size() - 1)._2 + postfix.length(), queue.peekLast()) : queue.peekLast();

                while ((idx = str.indexOf(prefix, idx)) >= 0 && idx < endIndex) {
                    queue.push(idx + prefix.length());
                    idx = idx + prefix.length();
                }

                if (idx > 0) {
                    next = idx;
                }

                final int startIndex = queue.pop();

                if (res.size() > 0 && startIndex < res.get(res.size() - 1)._1) {
                    while (res.size() > 0 && startIndex < res.get(res.size() - 1)._1) {
                        res.remove(res.size() - 1);
                    }
                }

                res.add(IntPair.of(startIndex, endIndex));

                i = endIndex + postfix.length();
            }
        }

        return res;
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<String> findAll(final String str, final char prefix, final char postfix) {
        return N.isNullOrEmpty(str) ? new ArrayList<String>() : findAll(str, 0, str.length(), prefix, postfix);
    }

    public static <T> List<T> findAll(final String str, final char prefix, final char postfix, final Function<? super String, T> func) {
        return N.isNullOrEmpty(str) ? new ArrayList<T>() : findAll(str, 0, str.length(), prefix, postfix, func);
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param fromIndex
     * @param toIndex
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<String> findAll(final String str, final int fromIndex, final int toIndex, final char prefix, final char postfix) {
        final List<IntPair> points = findAllIndices(str, prefix, postfix);
        final List<String> res = new ArrayList<>(points.size());

        for (IntPair p : points) {
            res.add(str.substring(p._1, p._2));
        }

        return res;
    }

    public static <T> List<T> findAll(final String str, final int fromIndex, final int toIndex, final char prefix, final char postfix,
            final Function<? super String, T> func) {
        final List<String> strs = findAll(str, fromIndex, toIndex, prefix, postfix);
        final List<T> res = new ArrayList<>(strs.size());

        for (String s : strs) {
            res.add(func.apply(s));
        }

        return res;
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<String> findAll(final String str, final String prefix, final String postfix) {
        return N.isNullOrEmpty(str) ? new ArrayList<String>() : findAll(str, 0, str.length(), prefix, postfix);
    }

    public static <T> List<T> findAll(final String str, final String prefix, final String postfix, final Function<? super String, T> func) {
        return N.isNullOrEmpty(str) ? new ArrayList<T>() : findAll(str, 0, str.length(), prefix, postfix, func);
    }

    /**
     * 
     * <code>findAllIndices("3[a2[c]]2[a]", '[', ']') = [[2, 7], [10, 11]]</code>
     * 
     * @param str
     * @param fromIndex
     * @param toIndex
     * @param prefix
     * @param postfix
     * @return
     */
    public static List<String> findAll(final String str, final int fromIndex, final int toIndex, final String prefix, final String postfix) {
        final List<IntPair> points = findAllIndices(str, prefix, postfix);
        final List<String> res = new ArrayList<>(points.size());

        for (IntPair p : points) {
            res.add(str.substring(p._1, p._2));
        }

        return res;
    }

    public static <T> List<T> findAll(final String str, final int fromIndex, final int toIndex, final String prefix, final String postfix,
            final Function<? super String, T> func) {
        final List<String> strs = findAll(str, fromIndex, toIndex, prefix, postfix);
        final List<T> res = new ArrayList<>(strs.size());

        for (String s : strs) {
            res.add(func.apply(s));
        }

        return res;
    }

    public static int indexOf(final boolean[] a, final boolean e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final boolean[] a, final int fromIndex, final boolean e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final char[] a, final char e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final char[] a, final int fromIndex, final char e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final byte[] a, final byte e) {
        return indexOf(a, 0, e);

    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final byte[] a, final int fromIndex, final byte e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final short[] a, final short e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final short[] a, final int fromIndex, final short e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final int[] a, final int e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final int[] a, final int fromIndex, final int e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final long[] a, final long e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final long[] a, final int fromIndex, final long e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final float[] a, final float e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final float[] a, final int fromIndex, final float e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (Float.compare(a[i], e) == 0) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final double[] a, final double e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final double[] a, final int fromIndex, final double e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (Double.compare(a[i], e) == 0) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final Object[] a, final Object e) {
        return indexOf(a, 0, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final Object[] a, final int fromIndex, final Object e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = a.length; i < len; i++) {
            if (equals(a[i], e)) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final List<?> list, final Object e) {
        return indexOf(list, 0, e);
    }

    /**
     *
     * @param list
     * @param fromIndex
     *            the index from which to start the search.
     * @param e
     * @return
     */
    public static int indexOf(final List<?> list, final int fromIndex, final Object e) {
        if (N.isNullOrEmpty(list)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = list.size(); i < len; i++) {
            if (equals(list.get(i), e)) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOf(final String str, final char ch) {
        if (N.isNullOrEmpty(str)) {
            return N.INDEX_NOT_FOUND;
        }

        return str.indexOf(ch);
    }

    public static int indexOf(final String str, final int fromIndex, final char ch) {
        if (N.isNullOrEmpty(str)) {
            return N.INDEX_NOT_FOUND;
        }

        return str.indexOf(ch, fromIndex);
    }

    public static int indexOf(final String str, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length()) {
            return N.INDEX_NOT_FOUND;
        }

        return str.indexOf(substr);
    }

    public static int indexOf(final String str, final int fromIndex, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length() - fromIndex) {
            return N.INDEX_NOT_FOUND;
        }

        return str.indexOf(substr, fromIndex);
    }

    @SafeVarargs
    public static int indexOfAny(final String str, final char... chs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(chs)) {
            return N.INDEX_NOT_FOUND;
        }

        final int strLen = str.length();
        final int strLast = strLen - 1;
        final int chsLen = chs.length;
        final int chsLast = chsLen - 1;
        for (int i = 0; i < strLen; i++) {
            final char ch = str.charAt(i);
            for (int j = 0; j < chsLen; j++) {
                if (chs[j] == ch) {
                    if (i < strLast && j < chsLast && Character.isHighSurrogate(ch)) {
                        // ch is a supplementary character
                        if (chs[j + 1] == str.charAt(i + 1)) {
                            return i;
                        }
                    } else {
                        return i;
                    }
                }
            }
        }

        return N.INDEX_NOT_FOUND;
    }

    @SafeVarargs
    public static int indexOfAny(final String str, final String... substrs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substrs)) {
            return N.INDEX_NOT_FOUND;
        }

        int result = N.INDEX_NOT_FOUND;
        int tmp = 0;

        for (String substr : substrs) {
            if (N.isNullOrEmpty(substr)) {
                continue;
            }

            tmp = indexOf(str, substr);

            if (tmp == N.INDEX_NOT_FOUND) {
                continue;
            } else if (result == N.INDEX_NOT_FOUND || tmp < result) {
                result = tmp;
            }
        }

        return result;
    }

    @SafeVarargs
    public static int indexOfAnyBut(final String str, final char... chs) {
        if (N.isNullOrEmpty(str)) {
            return N.INDEX_NOT_FOUND;
        }

        if (N.isNullOrEmpty(chs)) {
            return 0;
        }

        final int strLen = str.length();
        final int strLast = strLen - 1;
        final int chsLen = chs.length;
        final int chsLast = chsLen - 1;
        outer: for (int i = 0; i < strLen; i++) {
            final char ch = str.charAt(i);
            for (int j = 0; j < chsLen; j++) {
                if (chs[j] == ch) {
                    if (i < strLast && j < chsLast && Character.isHighSurrogate(ch)) {
                        if (chs[j + 1] == str.charAt(i + 1)) {
                            continue outer;
                        }
                    } else {
                        continue outer;
                    }
                }
            }
            return i;
        }

        return N.INDEX_NOT_FOUND;
    }

    public static int indexOf(final String str, final String substr, final String delimiter) {
        return indexOf(str, 0, substr, delimiter);
    }

    /**
     *
     * @param str
     * @param fromIndex
     *            the index from which to start the search.
     * @param substr
     * @param delimiter
     * @return
     */
    public static int indexOf(final String str, final int fromIndex, final String substr, final String delimiter) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr)) {
            return INDEX_NOT_FOUND;
        }

        int index = str.indexOf(substr, fromIndex);

        if (index < 0) {
            return INDEX_NOT_FOUND;
        }

        if (index + substr.length() == str.length()) {
            return index;
        } else if (str.length() >= index + substr.length() + delimiter.length()) {
            for (int i = 0, j = index + substr.length(), seperatorLen = delimiter.length(); i < seperatorLen;) {
                if (delimiter.charAt(i++) != str.charAt(j++)) {
                    return INDEX_NOT_FOUND;
                }
            }

            return index;
        }

        return INDEX_NOT_FOUND;
    }

    public static int indexOfIgnoreCase(final String str, final String substr) {
        return indexOfIgnoreCase(str, 0, substr);
    }

    public static int indexOfIgnoreCase(final String str, final int fromIndex, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length() - fromIndex) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = fromIndex, len = str.length(), substrLen = substr.length(), end = len - substrLen + 1; i < end; i++) {
            if (str.regionMatches(true, i, substr, 0, substrLen)) {
                return i;
            }
        }

        return N.INDEX_NOT_FOUND;
    }

    /**
     *
     * @see java.util.Collections#indexOfSubList(List, List)
     *
     */
    public static int indexOfSubList(final List<?> sourceList, final List<?> targetSubList) {
        if (N.isNullOrEmpty(sourceList) || N.isNullOrEmpty(targetSubList)) {
            return N.INDEX_NOT_FOUND;
        }

        return Collections.indexOfSubList(sourceList, targetSubList);
    }

    /**
     * <p>
     * Finds the n-th index within a String, handling {@code null}.
     * </p>
     *
     * @param str
     * @param substr
     * @param ordinal
     *            the n-th {@code searchStr} to find
     * @return the n-th index of the search String, {@code -1} (
     *         {@code INDEX_NOT_FOUND}) if no match or {@code null} or empty
     *         string input
     */
    public static int ordinalIndexOf(final String str, final String substr, final int ordinal) {
        return ordinalIndexOf(str, substr, ordinal, false);
    }

    // Shared code between ordinalIndexOf(String,String,int) and
    // lastOrdinalIndexOf(String,String,int)
    private static int ordinalIndexOf(final String str, final String substr, final int ordinal, final boolean isLastIndex) {
        if (ordinal < 1) {
            throw new IllegalArgumentException("ordinal(" + ordinal + ") must be >= 1");
        }

        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length()) {
            return N.INDEX_NOT_FOUND;
        }

        int fromIndex = isLastIndex ? str.length() : 0;

        for (int found = 0; fromIndex >= 0;) {
            fromIndex = isLastIndex ? str.lastIndexOf(substr, fromIndex) : str.indexOf(substr, fromIndex);

            if (fromIndex < 0) {
                return N.INDEX_NOT_FOUND;
            }

            if (++found >= ordinal) {
                break;
            }

            fromIndex = isLastIndex ? (fromIndex - substr.length()) : (fromIndex + substr.length());
        }

        return fromIndex;
    }

    public static int lastIndexOf(final boolean[] a, final boolean e) {
        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final boolean[] a, final int fromIndex, final boolean e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final char[] a, final char e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final char[] a, final int fromIndex, final char e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final byte[] a, final byte e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);

    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final byte[] a, final int fromIndex, final byte e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final short[] a, final short e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final short[] a, final int fromIndex, final short e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final int[] a, final int e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final int[] a, final int fromIndex, final int e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final long[] a, final long e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final long[] a, final int fromIndex, final long e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (a[i] == e) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final float[] a, final float e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final float[] a, final int fromIndex, final float e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (Float.compare(a[i], e) == 0) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final double[] a, final double e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final double[] a, final int fromIndex, final double e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (Double.compare(a[i], e) == 0) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final Object[] a, final Object e) {
        if (N.isNullOrEmpty(a)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(a, a.length - 1, e);
    }

    /**
     *
     * @param a
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final Object[] a, final int fromIndex, final Object e) {
        if (N.isNullOrEmpty(a)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, a.length - 1); i >= 0; i--) {
            if (equals(a[i], e)) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final List<?> list, final Object e) {
        if (N.isNullOrEmpty(list)) {
            return INDEX_NOT_FOUND;
        }

        return lastIndexOf(list, list.size() - 1, e);
    }

    /**
     *
     * @param list
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param e
     * @return
     */
    public static int lastIndexOf(final List<?> list, final int fromIndex, final Object e) {
        if (N.isNullOrEmpty(list)) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = min(fromIndex, list.size() - 1); i >= 0; i--) {
            if (equals(list.get(i), e)) {
                return i;
            }
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOf(final String str, final char ch) {
        if (N.isNullOrEmpty(str)) {
            return N.INDEX_NOT_FOUND;
        }

        return str.lastIndexOf(ch);
    }

    /**
     * Returns the index within this string of the last occurrence of the
     * specified character, searching backward starting at the specified index.
     * For values of <code>ch</code> in the range from 0 to 0xFFFF (inclusive),
     * the index returned is the largest value <i>k</i> such that: <blockquote>
     *
     * <pre>
     * (this.charAt(<i>k</i>) == ch) && (<i>k</i> &lt;= fromIndex)
     * </pre>
     *
     * </blockquote> is true. For other values of <code>ch</code>, it is the
     * largest value <i>k</i> such that: <blockquote>
     *
     * <pre>
     * (this.codePointAt(<i>k</i>) == ch) && (<i>k</i> &lt;= fromIndex)
     * </pre>
     *
     * </blockquote> is true. In either case, if no such character occurs in
     * this string at or before position <code>fromIndex</code>, then
     * <code>-1</code> is returned.
     *
     * <p>
     * All indices are specified in <code>char</code> values (Unicode code
     * units).
     *
     * @param str
     * @param fromIndex
     *            the index to start the search from. There is no restriction on
     *            the value of <code>fromIndex</code>. If it is greater than or
     *            equal to the length of this string, it has the same effect as
     *            if it were equal to one less than the length of this string:
     *            this entire string may be searched. If it is negative, it has
     *            the same effect as if it were -1: -1 is returned.
     * @param ch
     *            a character (Unicode code point).
     * @return the index of the last occurrence of the character in the
     *         character sequence represented by this object that is less than
     *         or equal to <code>fromIndex</code>, or <code>-1</code> if the
     *         character does not occur before that point.
     */
    public static int lastIndexOf(final String str, final int fromIndex, final char ch) {
        if (N.isNullOrEmpty(str)) {
            return N.INDEX_NOT_FOUND;
        }

        return str.lastIndexOf(ch, fromIndex);
    }

    public static int lastIndexOf(final String str, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length()) {
            return N.INDEX_NOT_FOUND;
        }

        return str.lastIndexOf(substr);
    }

    /**
     * Returns the index within <code>str</code> of the last occurrence of the
     * specified <code>substr</code>, searching backward starting at the
     * specified index.
     *
     * <p>
     * The returned index is the largest value <i>k</i> for which: <blockquote>
     *
     * <pre>
     * <i>k</i> &lt;= fromIndex && str.startsWith(substr, <i>k</i>)
     * </pre>
     *
     * </blockquote> If no such value of <i>k</i> exists, then {@code -1} is
     * returned.
     *
     * @param str
     * @param fromIndex
     * @param substr
     * @return
     */
    public static int lastIndexOf(final String str, final int fromIndex, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length()) {
            return N.INDEX_NOT_FOUND;
        }

        return str.lastIndexOf(substr, fromIndex);
    }

    @SafeVarargs
    public static int lastIndexOfAny(final String str, final char... chs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(chs)) {
            return N.INDEX_NOT_FOUND;
        }

        int result = N.INDEX_NOT_FOUND;
        int tmp = 0;

        for (char ch : chs) {
            tmp = str.lastIndexOf(ch);

            if (tmp == N.INDEX_NOT_FOUND) {
                continue;
            } else if (result == N.INDEX_NOT_FOUND || tmp > result) {
                result = tmp;
            }
        }

        return result;
    }

    @SafeVarargs
    public static int lastIndexOfAny(final String str, final String... substrs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substrs)) {
            return N.INDEX_NOT_FOUND;
        }

        int result = N.INDEX_NOT_FOUND;
        int tmp = 0;

        for (String substr : substrs) {
            if (N.isNullOrEmpty(substr)) {
                continue;
            }

            tmp = str.lastIndexOf(substr);

            if (tmp == N.INDEX_NOT_FOUND) {
                continue;
            } else if (result == N.INDEX_NOT_FOUND || tmp > result) {
                result = tmp;
            }
        }

        return result;
    }

    public static int lastIndexOf(final String str, final String substr, final String delimiter) {
        return lastIndexOf(str, str.length(), substr, delimiter);
    }

    /**
     *
     * @param str
     * @param fromIndex
     *            the start index to traverse backwards from
     * @param substr
     * @param delimiter
     * @return
     */
    public static int lastIndexOf(final String str, final int fromIndex, final String substr, final String delimiter) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr)) {
            return INDEX_NOT_FOUND;
        }

        // int index = str.lastIndexOf(substr, min(fromIndex, str.length() -
        // 1)); // Refer to String.lastIndexOf(String, int). the result is same
        // as below line.
        int index = str.lastIndexOf(substr, min(fromIndex, str.length()));

        if (index < 0) {
            return INDEX_NOT_FOUND;
        }

        if (index + substr.length() == str.length()) {
            return index;
        } else if (str.length() >= index + substr.length() + delimiter.length()) {
            for (int i = 0, j = index + substr.length(), len = delimiter.length(); i < len;) {
                if (delimiter.charAt(i++) != str.charAt(j++)) {
                    return INDEX_NOT_FOUND;
                }
            }

            return index;
        }

        return INDEX_NOT_FOUND;
    }

    public static int lastIndexOfIgnoreCase(final String str, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length()) {
            return N.INDEX_NOT_FOUND;
        }

        return lastIndexOfIgnoreCase(str, str.length(), substr);
    }

    public static int lastIndexOfIgnoreCase(final String str, final int fromIndex, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr) || substr.length() > str.length()) {
            return N.INDEX_NOT_FOUND;
        }

        for (int i = N.min(fromIndex, str.length() - substr.length()), substrLen = substr.length(); i >= 0; i--) {
            if (str.regionMatches(true, i, substr, 0, substrLen)) {
                return i;
            }
        }

        return N.INDEX_NOT_FOUND;
    }

    /**
     *
     * @see java.util.Collections#lastIndexOfSubList(List, List)
     */
    public static int lastIndexOfSubList(final List<?> sourceList, final List<?> targetSubList) {
        if (N.isNullOrEmpty(sourceList) || N.isNullOrEmpty(targetSubList)) {
            return N.INDEX_NOT_FOUND;
        }

        return Collections.lastIndexOfSubList(sourceList, targetSubList);
    }

    /**
     * <p>
     * Finds the n-th last index within a String, handling {@code null}.
     * </p>
     *
     * @param str
     * @param substr
     * @param ordinal
     *            the n-th last {@code searchStr} to find
     * @return the n-th last index of the search CharSequence, {@code -1} (
     *         {@code INDEX_NOT_FOUND}) if no match or {@code null} or empty
     *         string input
     */
    public static int lastOrdinalIndexOf(final String str, final String substr, final int ordinal) {
        return ordinalIndexOf(str, substr, ordinal, true);
    }

    public static int occurrencesOf(final boolean[] a, final boolean objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == objectToFind) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final char[] a, final char objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == objectToFind) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final byte[] a, final byte objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == objectToFind) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final short[] a, final short objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == objectToFind) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final int[] a, final int objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == objectToFind) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final long[] a, final long objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == objectToFind) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final float[] a, final float objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (Float.compare(a[i], objectToFind) == 0) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final double[] a, final double objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (Double.compare(a[i], objectToFind) == 0) {
                occurrences++;
            }
        }

        return occurrences;
    }

    public static int occurrencesOf(final Object[] a, final Object objectToFind) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int occurrences = 0;

        if (objectToFind == null) {
            for (int i = 0, len = a.length; i < len; i++) {
                if (a[i] == null) {
                    occurrences++;
                }
            }
        } else {
            for (int i = 0, len = a.length; i < len; i++) {
                if (objectToFind.equals(a[i])) {
                    occurrences++;
                }
            }
        }

        return occurrences;
    }

    /**
     *
     * @param c
     * @param objectToFind
     * @return
     *
     * @see java.util.Collections#frequency(Collection, Object)
     */
    public static int occurrencesOf(final Collection<?> c, final Object objectToFind) {
        if (N.isNullOrEmpty(c)) {
            return 0;
        }

        return Collections.frequency(c, objectToFind);
    }

    public static boolean contains(final boolean[] a, final boolean objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final char[] a, final char objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final byte[] a, final byte objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final short[] a, final short objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final int[] a, final int objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final long[] a, final long objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final float[] a, final float objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final double[] a, final double objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final Object[] a, final Object objectToFind) {
        return indexOf(a, objectToFind) != INDEX_NOT_FOUND;
    }

    public static boolean contains(final Collection<?> c, final Object e) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return c.contains(e);
    }

    public static boolean contains(final String str, final char ch) {
        if (N.isNullOrEmpty(str)) {
            return false;
        }

        return indexOf(str, ch) != N.INDEX_NOT_FOUND;
    }

    public static boolean contains(final String str, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr)) {
            return false;
        }

        return indexOf(str, substr) != N.INDEX_NOT_FOUND;
    }

    @SafeVarargs
    public static boolean containsAny(final String str, final char... chs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(chs)) {
            return false;
        }

        return indexOfAny(str, chs) != N.INDEX_NOT_FOUND;
    }

    @SafeVarargs
    public static boolean containsOnly(final String str, final char... chs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(chs)) {
            return false;
        }

        return indexOfAnyBut(str, chs) == N.INDEX_NOT_FOUND;
    }

    @SafeVarargs
    public static boolean containsNone(final String str, final char... chs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(chs)) {
            return true;
        }

        final int strLen = str.length();
        final int strLast = strLen - 1;
        final int chsLen = chs.length;
        final int chsLast = chsLen - 1;
        for (int i = 0; i < strLen; i++) {
            final char ch = str.charAt(i);
            for (int j = 0; j < chsLen; j++) {
                if (chs[j] == ch) {
                    if (Character.isHighSurrogate(ch)) {
                        if (j == chsLast) {
                            // missing low surrogate, fine, like
                            // String.indexOf(String)
                            return false;
                        }
                        if (i < strLast && chs[j + 1] == str.charAt(i + 1)) {
                            return false;
                        }
                    } else {
                        // ch is in the Basic Multilingual Plane
                        return false;
                    }
                }
            }
        }

        return true;
    }

    public static boolean contains(final String str, final String substr, final String delimiter) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr)) {
            return false;
        }

        return indexOf(str, substr, delimiter) != INDEX_NOT_FOUND;
    }

    public static boolean containsIgnoreCase(final String str, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr)) {
            return false;
        }

        return indexOfIgnoreCase(str, substr) != N.INDEX_NOT_FOUND;
    }

    // From org.springframework.util.StringUtils, under Apache License 2.0
    public static boolean containsWhitespace(final String str) {
        if (N.isNullOrEmpty(str)) {
            return false;
        }

        final char[] chars = N.getCharsForReadOnly(str);
        for (int i = 0, len = str.length(); i < len; i++) {
            if (Character.isWhitespace(chars[i])) {
                return true;
            }
        }

        return false;
    }

    public static boolean startsWith(final String str, final String prefix) {
        return startsWith(str, prefix, false);
    }

    public static boolean startsWithIgnoreCase(final String str, final String prefix) {
        return startsWith(str, prefix, true);
    }

    private static boolean startsWith(final String str, final String prefix, final boolean ignoreCase) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(prefix) || prefix.length() > str.length()) {
            return false;
        }

        return ignoreCase ? str.regionMatches(true, 0, prefix, 0, prefix.length()) : str.startsWith(prefix);
    }

    @SafeVarargs
    public static boolean startsWithAny(final String str, final String... substrs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substrs)) {
            return false;
        }

        for (final String substr : substrs) {
            if (startsWith(str, substr)) {

                return true;
            }
        }

        return false;
    }

    public static boolean endsWith(final String str, final String suffix) {
        return endsWith(str, suffix, false);
    }

    public static boolean endsWithIgnoreCase(final String str, final String suffix) {
        return endsWith(str, suffix, true);
    }

    private static boolean endsWith(final String str, final String suffix, final boolean ignoreCase) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(suffix) || suffix.length() > str.length()) {
            return false;
        }

        final int strOffset = str.length() - suffix.length();

        return ignoreCase ? str.regionMatches(true, strOffset, suffix, 0, suffix.length()) : str.endsWith(suffix);
    }

    @SafeVarargs
    public static boolean endsWithAny(final String str, final String... substrs) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substrs)) {
            return false;
        }

        for (final String searchString : substrs) {
            if (endsWith(str, searchString)) {
                return true;
            }
        }

        return false;
    }

    /**
     * <p>
     * Compares two Strings, and returns the index at which the Stringss begin
     * to differ.
     * </p>
     *
     * <p>
     * For example,
     * {@code indexOfDifference("i am a machine", "i am a robot") -> 7}
     * </p>
     *
     * <pre>
     * N.indexOfDifference(null, null) = -1
     * N.indexOfDifference("", "") = -1
     * N.indexOfDifference("", "abc") = 0
     * N.indexOfDifference("abc", "") = 0
     * N.indexOfDifference("abc", "abc") = -1
     * N.indexOfDifference("ab", "abxyz") = 2
     * N.indexOfDifference("abcde", "abxyz") = 2
     * N.indexOfDifference("abcde", "xyz") = 0
     * </pre>
     *
     * @param a
     *            the first String, may be null
     * @param b
     *            the second String, may be null
     * @return the index where cs1 and cs2 begin to differ; -1 if they are equal
     */
    public static int indexOfDifference(final String a, final String b) {
        if (a == b || (N.isNullOrEmpty(a) && N.isNullOrEmpty(b))) {
            return N.INDEX_NOT_FOUND;
        }

        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return 0;
        }

        int i = 0;
        for (int len = N.min(a.length(), b.length()); i < len; i++) {
            if (a.charAt(i) != b.charAt(i)) {
                break;
            }
        }

        if (i < b.length() || i < a.length()) {
            return i;
        }

        return N.INDEX_NOT_FOUND;
    }

    /**
     * <p>
     * Compares all Strings in an array and returns the index at which the
     * Strings begin to differ.
     * </p>
     *
     * <p>
     * For example,
     * <code>indexOfDifference(new String[] {"i am a machine", "i am a robot"}) -&gt; 7</code>
     * </p>
     *
     * <pre>
     * N.indexOfDifference(null) = -1
     * N.indexOfDifference(new String[] {}) = -1
     * N.indexOfDifference(new String[] {"abc"}) = -1
     * N.indexOfDifference(new String[] {null, null}) = -1
     * N.indexOfDifference(new String[] {"", ""}) = -1
     * N.indexOfDifference(new String[] {"", null}) = -1
     * N.indexOfDifference(new String[] {"abc", null, null}) = 0
     * N.indexOfDifference(new String[] {null, null, "abc"}) = 0
     * N.indexOfDifference(new String[] {"", "abc"}) = 0
     * N.indexOfDifference(new String[] {"abc", ""}) = 0
     * N.indexOfDifference(new String[] {"abc", "abc"}) = -1
     * N.indexOfDifference(new String[] {"abc", "a"}) = 1
     * N.indexOfDifference(new String[] {"ab", "abxyz"}) = 2
     * N.indexOfDifference(new String[] {"abcde", "abxyz"}) = 2
     * N.indexOfDifference(new String[] {"abcde", "xyz"}) = 0
     * N.indexOfDifference(new String[] {"xyz", "abcde"}) = 0
     * N.indexOfDifference(new String[] {"i am a machine", "i am a robot"}) = 7
     * </pre>
     *
     * @param strs
     *            array of Strings, entries may be null
     * @return the index where the strings begin to differ; -1 if they are all
     *         equal or null/empty
     */
    @SafeVarargs
    public static int indexOfDifference(final String... strs) {
        if (N.isNullOrEmpty(strs) || strs.length == 1) {
            return N.INDEX_NOT_FOUND;
        }

        final int arrayLen = strs.length;
        int shortestStrLen = Integer.MAX_VALUE;
        int longestStrLen = 0;

        // find the min and max string lengths; this avoids checking to make
        // sure we are not exceeding the length of the string each time through
        // the bottom loop.
        for (int i = 0; i < arrayLen; i++) {
            if (strs[i] == null) {
                shortestStrLen = 0;
            } else {
                shortestStrLen = Math.min(strs[i].length(), shortestStrLen);
                longestStrLen = Math.max(strs[i].length(), longestStrLen);
            }
        }

        // handle lists containing all nulls or all empty strings
        if (longestStrLen == 0) {
            return N.INDEX_NOT_FOUND;
        }

        if (shortestStrLen == 0) {
            return 0;
        }

        // find the position with the first difference across all strings
        int firstDiff = -1;
        for (int stringPos = 0; stringPos < shortestStrLen; stringPos++) {
            final char comparisonChar = strs[0].charAt(stringPos);
            for (int arrayPos = 1; arrayPos < arrayLen; arrayPos++) {
                if (strs[arrayPos].charAt(stringPos) != comparisonChar) {
                    firstDiff = stringPos;
                    break;
                }
            }

            if (firstDiff != -1) {
                break;
            }
        }

        if (firstDiff == -1 && shortestStrLen != longestStrLen) {
            // we compared all of the characters up to the length of the
            // shortest string and didn't find a match, but the string lengths
            // vary, so return the length of the shortest string.
            return shortestStrLen;
        }

        return firstDiff;
    }

    // --------- from Google Guava

    /**
     * Note: copy rights: Google Guava.
     *
     * Returns the longest string {@code prefix} such that
     * {@code a.toString().startsWith(prefix) && b.toString().startsWith(prefix)}
     * , taking care not to split surrogate pairs. If {@code a} and {@code b}
     * have no common prefix, returns the empty string.
     *
     */
    public static String commonPrefix(final String a, final String b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_STRING;
        }

        int maxPrefixLength = Math.min(a.length(), b.length());
        int p = 0;

        while (p < maxPrefixLength && a.charAt(p) == b.charAt(p)) {
            p++;
        }

        if (validSurrogatePairAt(a, p - 1) || validSurrogatePairAt(b, p - 1)) {
            p--;
        }

        if (p == a.length()) {
            return a.toString();
        } else if (p == b.length()) {
            return b.toString();
        } else {
            return a.subSequence(0, p).toString();
        }
    }

    @SafeVarargs
    public static String commonPrefix(final String... strs) {
        if (N.isNullOrEmpty(strs)) {
            return N.EMPTY_STRING;
        }

        if (strs.length == 1) {
            return N.isNullOrEmpty(strs[0]) ? N.EMPTY_STRING : strs[0];
        }

        String commonPrefix = commonPrefix(strs[0], strs[1]);

        if (N.isNullOrEmpty(commonPrefix)) {
            return N.EMPTY_STRING;
        }

        for (int i = 2, len = strs.length; i < len; i++) {
            commonPrefix = commonPrefix(commonPrefix, strs[i]);

            if (N.isNullOrEmpty(commonPrefix)) {
                return commonPrefix;
            }
        }

        return commonPrefix;
    }

    /**
     * Note: copy rights: Google Guava.
     *
     * Returns the longest string {@code suffix} such that
     * {@code a.toString().endsWith(suffix) && b.toString().endsWith(suffix)},
     * taking care not to split surrogate pairs. If {@code a} and {@code b} have
     * no common suffix, returns the empty string.
     *
     */
    public static String commonSuffix(final String a, final String b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_STRING;
        }

        final int aLength = a.length();
        final int bLength = b.length();
        int maxSuffixLength = Math.min(aLength, bLength);
        int s = 0;

        while (s < maxSuffixLength && a.charAt(aLength - s - 1) == b.charAt(bLength - s - 1)) {
            s++;
        }

        if (validSurrogatePairAt(a, aLength - s - 1) || validSurrogatePairAt(b, bLength - s - 1)) {
            s--;
        }

        if (s == aLength) {
            return a.toString();
        } else if (s == bLength) {
            return b.toString();
        } else {
            return a.subSequence(aLength - s, aLength).toString();
        }
    }

    @SafeVarargs
    public static String commonSuffix(final String... strs) {
        if (N.isNullOrEmpty(strs)) {
            return N.EMPTY_STRING;
        }

        if (strs.length == 1) {
            return N.isNullOrEmpty(strs[0]) ? N.EMPTY_STRING : strs[0];
        }

        String commonSuffix = commonSuffix(strs[0], strs[1]);

        if (N.isNullOrEmpty(commonSuffix)) {
            return N.EMPTY_STRING;
        }

        for (int i = 2, len = strs.length; i < len; i++) {
            commonSuffix = commonSuffix(commonSuffix, strs[i]);

            if (N.isNullOrEmpty(commonSuffix)) {
                return commonSuffix;
            }
        }

        return commonSuffix;
    }

    // --------- from Google Guava

    /**
     * Note: copy rights: Google Guava.
     *
     * True when a valid surrogate pair starts at the given {@code index} in the
     * given {@code string}. Out-of-range indexes return false.
     */
    static boolean validSurrogatePairAt(final String str, final int index) {
        return index >= 0 && index <= (str.length() - 2) && Character.isHighSurrogate(str.charAt(index)) && Character.isLowSurrogate(str.charAt(index + 1));
    }

    public static int countMatches(final String str, final char ch) {
        if (N.isNullOrEmpty(str)) {
            return 0;
        }

        int count = 0;
        final char[] chs = N.getCharsForReadOnly(str);

        for (int i = 0, len = chs.length; i < len; i++) {
            if (chs[i] == ch) {
                count++;
            }
        }

        return count;
    }

    public static int countMatches(final String str, final String substr) {
        if (N.isNullOrEmpty(str) || N.isNullOrEmpty(substr)) {
            return 0;
        }

        int count = 0;
        int index = 0;

        while ((index = str.indexOf(substr, index)) != INDEX_NOT_FOUND) {
            count++;
            index += substr.length();
        }

        return count;
    }

    public static <T> void forEach(final T[] a, final Consumer<? super T> action) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (T e : a) {
            action.accept(e);
        }
    }

    public static <T> void forEach(final T[] a, final int fromIndex, final int toIndex, final Consumer<? super T> action) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return;
        }

        if (fromIndex <= toIndex) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(a[i]);
            }
        } else {
            for (int i = min(a.length - 1, toIndex); i > toIndex; i--) {
                action.accept(a[i]);
            }
        }
    }

    public static <T> void forEach(final T[] a, final IndexedConsumer<T> action) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        forEach(a, 0, a.length, action);
    }

    public static <T> void forEach(final T[] a, final int fromIndex, final int toIndex, final IndexedConsumer<? super T> action) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return;
        }

        if (fromIndex <= toIndex) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(i, a[i]);
            }
        } else {
            for (int i = min(a.length - 1, toIndex); i > toIndex; i--) {
                action.accept(i, a[i]);
            }
        }
    }

    public static <T, R> R forEach(final T[] a, final R seed, final BiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super R, ? super T> conditionToBreak) {
        if (N.isNullOrEmpty(a)) {
            return seed;
        }

        return forEach(a, 0, a.length, seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public static <T, R> R forEach(final T[] a, final int fromIndex, final int toIndex, final R seed, final BiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super R, ? super T> conditionToBreak) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return seed;
        }

        R result = seed;

        if (fromIndex <= toIndex) {
            for (int i = fromIndex; i < toIndex; i++) {
                result = accumulator.apply(result, a[i]);

                if (conditionToBreak.test(result, a[i])) {
                    break;
                }
            }
        } else {
            for (int i = N.min(a.length - 1, fromIndex); i > toIndex; i--) {
                result = accumulator.apply(result, a[i]);

                if (conditionToBreak.test(result, a[i])) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T, R> R forEach(final T[] a, final R seed, final IndexedBiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super R, ? super T> conditionToBreak) {
        if (N.isNullOrEmpty(a)) {
            return seed;
        }

        return forEach(a, 0, a.length, seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public static <T, R> R forEach(final T[] a, final int fromIndex, final int toIndex, final R seed, final IndexedBiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super R, ? super T> conditionToBreak) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return seed;
        }

        R result = seed;

        if (fromIndex <= toIndex) {
            for (int i = fromIndex; i < toIndex; i++) {
                result = accumulator.apply(result, i, a[i]);

                if (conditionToBreak.test(result, a[i])) {
                    break;
                }
            }
        } else {
            for (int i = N.min(a.length - 1, fromIndex); i > toIndex; i--) {
                result = accumulator.apply(result, i, a[i]);

                if (conditionToBreak.test(result, a[i])) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T, C extends Collection<? extends T>> void forEach(final C c, final Consumer<? super T> action) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        for (T e : c) {
            action.accept(e);
        }
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     *
     * Note: This is NOT a replacement of traditional for loop statement. 
     * The traditional for loop is still recommended in regular programming.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param action
     */
    public static <T, C extends Collection<? extends T>> void forEach(final C c, int fromIndex, final int toIndex, final Consumer<? super T> action) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return;
        }

        fromIndex = N.min(c.size() - 1, fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(list.get(i));
                }
            } else {
                for (int i = fromIndex; i > toIndex; i--) {
                    action.accept(list.get(i));
                }
            }
        } else {
            final Iterator<? extends T> iter = c.iterator();
            int idx = 0;

            if (fromIndex <= toIndex) {
                while (idx < fromIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                while (iter.hasNext()) {
                    action.accept(iter.next());

                    if (++idx >= toIndex) {
                        break;
                    }
                }
            } else {
                while (idx <= toIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                final T[] a = (T[]) new Object[fromIndex - toIndex];

                while (iter.hasNext()) {
                    a[idx - 1 - toIndex] = iter.next();

                    if (idx++ >= fromIndex) {
                        break;
                    }
                }

                for (int i = a.length - 1; i >= 0; i--) {
                    action.accept(a[i]);
                }
            }
        }
    }

    public static <T, C extends Collection<? extends T>> void forEach(final C c, final IndexedConsumer<? super T> action) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        int idx = 0;
        for (T e : c) {
            action.accept(idx++, e);
        }
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     *
     * Note: This is NOT a replacement of traditional for loop statement. 
     * The traditional for loop is still recommended in regular programming.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param action
     */
    public static <T, C extends Collection<? extends T>> void forEach(final C c, int fromIndex, final int toIndex, final IndexedConsumer<? super T> action) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return;
        }

        fromIndex = N.min(c.size() - 1, fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(i, list.get(i));
                }
            } else {
                for (int i = fromIndex; i > toIndex; i--) {
                    action.accept(i, list.get(i));
                }
            }
        } else {
            final Iterator<? extends T> iter = c.iterator();
            int idx = 0;

            if (fromIndex < toIndex) {
                while (idx < fromIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                while (iter.hasNext()) {
                    action.accept(idx, iter.next());

                    if (++idx >= toIndex) {
                        break;
                    }
                }
            } else {
                while (idx <= toIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                final T[] a = (T[]) new Object[fromIndex - toIndex];

                while (iter.hasNext()) {
                    a[idx - 1 - toIndex] = iter.next();

                    if (idx++ >= fromIndex) {
                        break;
                    }
                }

                for (int i = a.length - 1; i >= 0; i--) {
                    action.accept(i + toIndex + 1, a[i]);
                }
            }
        }
    }

    public static <T, C extends Collection<? extends T>, R> R forEach(final C c, final R seed, BiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super R, ? super T> conditionToBreak) {
        if (N.isNullOrEmpty(c)) {
            return seed;
        }

        return forEach(c, 0, c.size(), seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public static <T, C extends Collection<? extends T>, R> R forEach(final C c, int fromIndex, final int toIndex, final R seed,
            final BiFunction<R, ? super T, R> accumulator, final BiPredicate<? super R, ? super T> conditionToBreak) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return seed;
        }

        fromIndex = N.min(c.size() - 1, fromIndex);

        R result = seed;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    result = accumulator.apply(result, list.get(i));

                    if (conditionToBreak.test(result, list.get(i))) {
                        break;
                    }
                }
            } else {
                for (int i = fromIndex; i > toIndex; i--) {
                    result = accumulator.apply(result, list.get(i));

                    if (conditionToBreak.test(result, list.get(i))) {
                        break;
                    }
                }
            }
        } else {
            final Iterator<? extends T> iter = c.iterator();
            int idx = 0;

            if (fromIndex <= toIndex) {
                while (idx < fromIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                T next = null;
                while (iter.hasNext()) {
                    next = iter.next();

                    result = accumulator.apply(result, next);

                    if (conditionToBreak.test(result, next)) {
                        break;
                    }

                    if (++idx >= toIndex) {
                        break;
                    }
                }
            } else {
                while (idx <= toIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                final T[] a = (T[]) new Object[fromIndex - toIndex];

                while (iter.hasNext()) {
                    a[idx - 1 - toIndex] = iter.next();

                    if (idx++ >= fromIndex) {
                        break;
                    }
                }

                for (int i = a.length - 1; i >= 0; i--) {
                    result = accumulator.apply(result, a[i]);

                    if (conditionToBreak.test(result, a[i])) {
                        break;
                    }
                }
            }
        }

        return result;
    }

    public static <T, C extends Collection<? extends T>, R> R forEach(final C c, final R seed, final IndexedBiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super R, ? super T> conditionToBreak) {
        if (N.isNullOrEmpty(c)) {
            return seed;
        }

        return forEach(c, 0, c.size(), seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public static <T, C extends Collection<? extends T>, R> R forEach(final C c, int fromIndex, final int toIndex, final R seed,
            final IndexedBiFunction<R, ? super T, R> accumulator, final BiPredicate<? super R, ? super T> conditionToBreak) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex,
                c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return seed;
        }

        fromIndex = N.min(c.size() - 1, fromIndex);

        R result = seed;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    result = accumulator.apply(result, i, list.get(i));

                    if (conditionToBreak.test(result, list.get(i))) {
                        break;
                    }
                }
            } else {
                for (int i = fromIndex; i > toIndex; i--) {
                    result = accumulator.apply(result, i, list.get(i));

                    if (conditionToBreak.test(result, list.get(i))) {
                        break;
                    }
                }
            }
        } else {
            final Iterator<? extends T> iter = c.iterator();
            int idx = 0;

            if (fromIndex < toIndex) {
                while (idx < fromIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                T next = null;

                while (iter.hasNext()) {
                    next = iter.next();
                    result = accumulator.apply(result, idx, iter.next());

                    if (conditionToBreak.test(result, next)) {
                        break;
                    }

                    if (++idx >= toIndex) {
                        break;
                    }
                }
            } else {
                while (idx <= toIndex && iter.hasNext()) {
                    iter.next();
                    idx++;
                }

                final T[] a = (T[]) new Object[fromIndex - toIndex];

                while (iter.hasNext()) {
                    a[idx - 1 - toIndex] = iter.next();

                    if (idx++ >= fromIndex) {
                        break;
                    }
                }

                for (int i = a.length - 1; i >= 0; i--) {
                    result = accumulator.apply(result, i + toIndex + 1, a[i]);

                    if (conditionToBreak.test(result, a[i])) {
                        break;
                    }
                }
            }
        }

        return result;
    }

    public static <T, U> void forEach(final T[] a, final Function<? super T, ? extends Collection<U>> flatMapper,
            final BiConsumer<? super T, ? super U> action) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (T e : a) {
            final Collection<U> c2 = flatMapper.apply(e);

            if (N.notNullOrEmpty(c2)) {
                for (U u : c2) {
                    action.accept(e, u);
                }
            }
        }
    }

    public static <T, U> void forEach(final Collection<T> c, final Function<? super T, ? extends Collection<U>> flatMapper,
            final BiConsumer<? super T, ? super U> action) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        for (T e : c) {
            final Collection<U> c2 = flatMapper.apply(e);

            if (N.notNullOrEmpty(c2)) {
                for (U u : c2) {
                    action.accept(e, u);
                }
            }
        }
    }

    public static <T, T2, T3> void forEach(final T[] a, final Function<? super T, ? extends Collection<T2>> flatMapper,
            final Function<? super T2, ? extends Collection<T3>> flatMapper2, final TriConsumer<? super T, ? super T2, ? super T3> action) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (T e : a) {
            final Collection<T2> c2 = flatMapper.apply(e);

            if (N.notNullOrEmpty(c2)) {
                for (T2 t2 : c2) {
                    final Collection<T3> c3 = flatMapper2.apply(t2);

                    if (N.notNullOrEmpty(c3)) {
                        for (T3 t3 : c3) {
                            action.accept(e, t2, t3);
                        }
                    }
                }
            }
        }
    }

    public static <T, T2, T3> void forEach(final Collection<T> c, final Function<? super T, ? extends Collection<T2>> flatMapper,
            final Function<? super T2, ? extends Collection<T3>> flatMapper2, final TriConsumer<? super T, ? super T2, ? super T3> action) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        for (T e : c) {
            final Collection<T2> c2 = flatMapper.apply(e);

            if (N.notNullOrEmpty(c2)) {
                for (T2 t2 : c2) {
                    final Collection<T3> c3 = flatMapper2.apply(t2);

                    if (N.notNullOrEmpty(c3)) {
                        for (T3 t3 : c3) {
                            action.accept(e, t2, t3);
                        }
                    }
                }
            }
        }
    }

    public static <A, B> void forEach(final A[] a, final B[] b, final BiConsumer<? super A, ? super B> action) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return;
        }

        for (int i = 0, minLen = N.min(a.length, b.length); i < minLen; i++) {
            action.accept(a[i], b[i]);
        }
    }

    public static <A, B> void forEach(final Collection<A> a, final Collection<B> b, final BiConsumer<? super A, ? super B> action) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return;
        }

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();

        for (int i = 0, minLen = N.min(a.size(), b.size()); i < minLen; i++) {
            action.accept(iterA.next(), iterB.next());
        }
    }

    public static <A, B, C> void forEach(final A[] a, final B[] b, final C[] c, final TriConsumer<? super A, ? super B, ? super C> action) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b) || N.isNullOrEmpty(c)) {
            return;
        }

        for (int i = 0, minLen = N.min(a.length, b.length, c.length); i < minLen; i++) {
            action.accept(a[i], b[i], c[i]);
        }
    }

    public static <A, B, C> void forEach(final Collection<A> a, final Collection<B> b, final Collection<C> c,
            final TriConsumer<? super A, ? super B, ? super C> action) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b) || N.isNullOrEmpty(c)) {
            return;
        }

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final Iterator<C> iterC = c.iterator();

        for (int i = 0, minLen = N.min(a.size(), b.size(), c.size()); i < minLen; i++) {
            action.accept(iterA.next(), iterB.next(), iterC.next());
        }
    }

    public static <A, B> void forEach(final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB, final BiConsumer<? super A, ? super B> action) {
        final int lenA = a == null ? 0 : a.length;
        final int lenB = b == null ? 0 : b.length;

        for (int i = 0, maxLen = N.max(lenA, lenB); i < maxLen; i++) {
            action.accept(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB);
        }
    }

    public static <A, B> void forEach(final Collection<A> a, final Collection<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiConsumer<? super A, ? super B> action) {

        final Iterator<A> iterA = a == null ? ObjIterator.<A> empty() : a.iterator();
        final Iterator<B> iterB = b == null ? ObjIterator.<B> empty() : b.iterator();
        final int lenA = a == null ? 0 : a.size();
        final int lenB = b == null ? 0 : b.size();

        for (int i = 0, maxLen = N.max(lenA, lenB); i < maxLen; i++) {
            action.accept(i < lenA ? iterA.next() : valueForNoneA, i < lenB ? iterB.next() : valueForNoneB);
        }
    }

    public static <A, B, C> void forEach(final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriConsumer<? super A, ? super B, ? super C> action) {
        final int lenA = a == null ? 0 : a.length;
        final int lenB = b == null ? 0 : b.length;
        final int lenC = c == null ? 0 : c.length;

        for (int i = 0, maxLen = N.max(lenA, lenB, lenC); i < maxLen; i++) {
            action.accept(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
        }
    }

    public static <A, B, C> void forEach(final Collection<A> a, final Collection<B> b, final Collection<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriConsumer<? super A, ? super B, ? super C> action) {

        final Iterator<A> iterA = a == null ? ObjIterator.<A> empty() : a.iterator();
        final Iterator<B> iterB = b == null ? ObjIterator.<B> empty() : b.iterator();
        final Iterator<C> iterC = c == null ? ObjIterator.<C> empty() : c.iterator();
        final int lenA = a == null ? 0 : a.size();
        final int lenB = b == null ? 0 : b.size();
        final int lenC = c == null ? 0 : c.size();

        for (int i = 0, maxLen = N.max(lenA, lenB, lenC); i < maxLen; i++) {
            action.accept(i < lenA ? iterA.next() : valueForNoneA, i < lenB ? iterB.next() : valueForNoneB, i < lenC ? iterC.next() : valueForNoneC);
        }
    }

    //    public static <T> void forEachNonNull(final T[] a, final Consumer<? super T> action) {
    //        if (N.isNullOrEmpty(a)) {
    //            return;
    //        }
    //
    //        for (T e : a) {
    //            if (e != null) {
    //                action.accept(e);
    //            }
    //        }
    //    }
    //
    //    public static <T, C extends Collection<? extends T>> void forEachNonNull(final C c, final Consumer<? super T> action) {
    //        if (N.isNullOrEmpty(c)) {
    //            return;
    //        }
    //
    //        for (T e : c) {
    //            if (e != null) {
    //                action.accept(e);
    //            }
    //        }
    //    }

    public static <T, U> void forEachNonNull(final T[] a, final Function<? super T, ? extends Collection<U>> flatMapper,
            final BiConsumer<? super T, ? super U> action) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (T e : a) {
            if (e != null) {
                final Collection<U> c2 = flatMapper.apply(e);

                if (N.notNullOrEmpty(c2)) {
                    for (U u : c2) {
                        if (u != null) {
                            action.accept(e, u);
                        }
                    }
                }
            }
        }
    }

    public static <T, U> void forEachNonNull(final Collection<T> c, final Function<? super T, ? extends Collection<U>> flatMapper,
            final BiConsumer<? super T, ? super U> action) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        for (T e : c) {
            if (e != null) {
                final Collection<U> c2 = flatMapper.apply(e);

                if (N.notNullOrEmpty(c2)) {
                    for (U u : c2) {
                        if (u != null) {
                            action.accept(e, u);
                        }
                    }
                }
            }
        }
    }

    public static <T, T2, T3> void forEachNonNull(final T[] a, final Function<? super T, ? extends Collection<T2>> flatMapper,
            final Function<? super T2, ? extends Collection<T3>> flatMapper2, final TriConsumer<? super T, ? super T2, ? super T3> action) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (T e : a) {
            if (e != null) {
                final Collection<T2> c2 = flatMapper.apply(e);

                if (N.notNullOrEmpty(c2)) {
                    for (T2 t2 : c2) {
                        if (t2 != null) {
                            final Collection<T3> c3 = flatMapper2.apply(t2);

                            if (N.notNullOrEmpty(c3)) {
                                for (T3 t3 : c3) {
                                    if (t3 != null) {
                                        action.accept(e, t2, t3);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static <T, T2, T3> void forEachNonNull(final Collection<T> c, final Function<? super T, ? extends Collection<T2>> flatMapper,
            final Function<? super T2, ? extends Collection<T3>> flatMapper2, final TriConsumer<? super T, ? super T2, ? super T3> action) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        for (T e : c) {
            if (e != null) {
                final Collection<T2> c2 = flatMapper.apply(e);

                if (N.notNullOrEmpty(c2)) {
                    for (T2 t2 : c2) {
                        if (t2 != null) {
                            final Collection<T3> c3 = flatMapper2.apply(t2);

                            if (N.notNullOrEmpty(c3)) {
                                for (T3 t3 : c3) {
                                    if (t3 != null) {
                                        action.accept(e, t2, t3);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    public static BooleanList filter(final boolean[] a, final BooleanPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return BooleanList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static BooleanList filter(final boolean[] a, final BooleanPredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return BooleanList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static BooleanList filter(final boolean[] a, final int fromIndex, final int toIndex, final BooleanPredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static BooleanList filter(final boolean[] a, final int fromIndex, final int toIndex, final BooleanPredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return BooleanList.empty();
        }

        final BooleanList result = new BooleanList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static CharList filter(final char[] a, final CharPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return CharList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static CharList filter(final char[] a, final CharPredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return CharList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static CharList filter(final char[] a, final int fromIndex, final int toIndex, final CharPredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static CharList filter(final char[] a, final int fromIndex, final int toIndex, final CharPredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return CharList.empty();
        }

        final CharList result = new CharList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static ByteList filter(final byte[] a, final BytePredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return ByteList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static ByteList filter(final byte[] a, final BytePredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return ByteList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static ByteList filter(final byte[] a, final int fromIndex, final int toIndex, final BytePredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static ByteList filter(final byte[] a, final int fromIndex, final int toIndex, final BytePredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return ByteList.empty();
        }

        final ByteList result = new ByteList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static ShortList filter(final short[] a, final ShortPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return ShortList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static ShortList filter(final short[] a, final ShortPredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return ShortList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static ShortList filter(final short[] a, final int fromIndex, final int toIndex, final ShortPredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static ShortList filter(final short[] a, final int fromIndex, final int toIndex, final ShortPredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return ShortList.empty();
        }

        final ShortList result = new ShortList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static IntList filter(final int[] a, final IntPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return IntList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static IntList filter(final int[] a, final IntPredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return IntList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static IntList filter(final int[] a, final int fromIndex, final int toIndex, final IntPredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static IntList filter(final int[] a, final int fromIndex, final int toIndex, final IntPredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return IntList.empty();
        }

        final IntList result = new IntList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static LongList filter(final long[] a, final LongPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return LongList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static LongList filter(final long[] a, final LongPredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return LongList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static LongList filter(final long[] a, final int fromIndex, final int toIndex, final LongPredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static LongList filter(final long[] a, final int fromIndex, final int toIndex, final LongPredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return LongList.empty();
        }

        final LongList result = new LongList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static FloatList filter(final float[] a, final FloatPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return FloatList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static FloatList filter(final float[] a, final FloatPredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return FloatList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static FloatList filter(final float[] a, final int fromIndex, final int toIndex, final FloatPredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static FloatList filter(final float[] a, final int fromIndex, final int toIndex, final FloatPredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return FloatList.empty();
        }

        final FloatList result = new FloatList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static DoubleList filter(final double[] a, final DoublePredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return DoubleList.empty();
        }

        return filter(a, 0, a.length, filter);
    }

    public static DoubleList filter(final double[] a, final DoublePredicate filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return DoubleList.empty();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static DoubleList filter(final double[] a, final int fromIndex, final int toIndex, final DoublePredicate filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max maximum return result.
     * @return
     */
    public static DoubleList filter(final double[] a, final int fromIndex, final int toIndex, final DoublePredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return DoubleList.empty();
        }

        final DoubleList result = new DoubleList(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static <T> List<T> filter(final T[] a, final Predicate<? super T> filter) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        return filter(a, filter, Integer.MAX_VALUE);
    }

    public static <T> List<T> filter(final T[] a, final Predicate<? super T> filter, final int max) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        return filter(a, 0, a.length, filter, max);
    }

    public static <T> List<T> filter(final T[] a, final int fromIndex, final int toIndex, final Predicate<? super T> filter) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max
     * @return
     */
    public static <T> List<T> filter(final T[] a, final int fromIndex, final int toIndex, final Predicate<? super T> filter, final int max) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final List<T> result = new ArrayList<>(min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static <T> List<T> filter(final Collection<? extends T> c, final Predicate<? super T> filter) {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        return filter(c, filter, Integer.MAX_VALUE);
    }

    public static <T> List<T> filter(final Collection<? extends T> c, final Predicate<? super T> filter, final int max) {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        return filter(c, 0, c.size(), filter, max);
    }

    public static <T> List<T> filter(final Collection<? extends T> c, final int fromIndex, final int toIndex, final Predicate<? super T> filter) {
        return filter(c, fromIndex, toIndex, filter, Integer.MAX_VALUE);
    }

    public static <T> List<T> filter(final Collection<? extends T> c, final int fromIndex, final int toIndex, final Predicate<? super T> filter,
            final int max) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return new ArrayList<>();
        }

        final List<T> result = new ArrayList<>(min(9, max, (toIndex - fromIndex)));

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;
            T e = null;

            for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
                e = list.get(i);

                if (filter.test(e)) {
                    result.add(e);
                    cnt++;
                }
            }
        } else {
            int idx = 0;
            int cnt = 0;
            for (T e : c) {
                if (cnt >= max) {
                    break;
                }

                if (idx++ < fromIndex) {
                    continue;
                }

                if (filter.test(e)) {
                    result.add(e);
                    cnt++;
                }

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T, R extends Collection<T>> R filter(final T[] a, final Predicate<? super T> filter, final IntFunction<R> supplier) {
        if (N.isNullOrEmpty(a)) {
            return supplier.apply(0);
        }

        return filter(a, filter, Integer.MAX_VALUE, supplier);
    }

    public static <T, R extends Collection<T>> R filter(final T[] a, final Predicate<? super T> filter, final int max, final IntFunction<R> supplier) {
        if (N.isNullOrEmpty(a)) {
            return supplier.apply(0);
        }

        return filter(a, 0, a.length, filter, max, supplier);
    }

    public static <T, R extends Collection<T>> R filter(final T[] a, final int fromIndex, final int toIndex, final Predicate<? super T> filter,
            final IntFunction<R> supplier) {
        return filter(a, fromIndex, toIndex, filter, Integer.MAX_VALUE, supplier);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @param max
     * @param supplier
     * @return
     */
    public static <T, R extends Collection<T>> R filter(final T[] a, final int fromIndex, final int toIndex, final Predicate<? super T> filter, final int max,
            final IntFunction<R> supplier) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return supplier.apply(0);
        }

        final R result = supplier.apply(N.min(9, max, (toIndex - fromIndex)));

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (filter.test(a[i])) {
                result.add(a[i]);
                cnt++;
            }
        }

        return result;
    }

    public static <T, R extends Collection<T>> R filter(final Collection<? extends T> c, final Predicate<? super T> filter, final IntFunction<R> supplier) {
        if (N.isNullOrEmpty(c)) {
            return supplier.apply(0);
        }

        return filter(c, filter, Integer.MAX_VALUE, supplier);
    }

    public static <T, R extends Collection<T>> R filter(final Collection<? extends T> c, final Predicate<? super T> filter, final int max,
            final IntFunction<R> supplier) {
        if (N.isNullOrEmpty(c)) {
            return supplier.apply(0);
        }

        return filter(c, 0, c.size(), filter, max, supplier);
    }

    public static <T, R extends Collection<T>> R filter(final Collection<? extends T> c, final int fromIndex, final int toIndex,
            final Predicate<? super T> filter, final IntFunction<R> supplier) {
        return filter(c, fromIndex, toIndex, filter, Integer.MAX_VALUE, supplier);
    }

    public static <T, R extends Collection<T>> R filter(final Collection<? extends T> c, final int fromIndex, final int toIndex,
            final Predicate<? super T> filter, final int max, final IntFunction<R> supplier) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return supplier.apply(0);
        }

        final R result = supplier.apply(N.min(9, max, (toIndex - fromIndex)));

        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < c.size())) {
            return result;
        }

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;
            T e = null;

            for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
                e = list.get(i);

                if (filter.test(e)) {
                    result.add(e);
                    cnt++;
                }
            }
        } else {
            int idx = 0;
            int cnt = 0;
            for (T e : c) {
                if (cnt >= max) {
                    break;
                }

                if (idx++ < fromIndex) {
                    continue;
                }

                if (filter.test(e)) {
                    result.add(e);
                    cnt++;
                }

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> BooleanList mapToBoolean(final T[] a, final ToBooleanFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return BooleanList.empty();
        }

        return mapToBoolean(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> BooleanList mapToBoolean(final T[] a, final int fromIndex, final int toIndex, final ToBooleanFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return BooleanList.empty();
        }

        final BooleanList result = new BooleanList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsBoolean(a[i]));
        }

        return result;
    }

    public static <T> BooleanList mapToBoolean(final Collection<? extends T> c, final ToBooleanFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return BooleanList.empty();
        }

        return mapToBoolean(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> BooleanList mapToBoolean(final Collection<? extends T> c, final int fromIndex, final int toIndex,
            final ToBooleanFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return BooleanList.empty();
        }

        final BooleanList result = new BooleanList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsBoolean(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsBoolean(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> CharList mapToChar(final T[] a, final ToCharFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return CharList.empty();
        }

        return mapToChar(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> CharList mapToChar(final T[] a, final int fromIndex, final int toIndex, final ToCharFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return CharList.empty();
        }

        final CharList result = new CharList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsChar(a[i]));
        }

        return result;
    }

    public static <T> CharList mapToChar(final Collection<? extends T> c, final ToCharFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return CharList.empty();
        }

        return mapToChar(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> CharList mapToChar(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToCharFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return CharList.empty();
        }

        final CharList result = new CharList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsChar(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsChar(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> ByteList mapToByte(final T[] a, final ToByteFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return ByteList.empty();
        }

        return mapToByte(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> ByteList mapToByte(final T[] a, final int fromIndex, final int toIndex, final ToByteFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return ByteList.empty();
        }

        final ByteList result = new ByteList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsByte(a[i]));
        }

        return result;
    }

    public static <T> ByteList mapToByte(final Collection<? extends T> c, final ToByteFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return ByteList.empty();
        }

        return mapToByte(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> ByteList mapToByte(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToByteFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return ByteList.empty();
        }

        final ByteList result = new ByteList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsByte(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsByte(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> ShortList mapToShort(final T[] a, final ToShortFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return ShortList.empty();
        }

        return mapToShort(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> ShortList mapToShort(final T[] a, final int fromIndex, final int toIndex, final ToShortFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return ShortList.empty();
        }

        final ShortList result = new ShortList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsShort(a[i]));
        }

        return result;
    }

    public static <T> ShortList mapToShort(final Collection<? extends T> c, final ToShortFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return ShortList.empty();
        }

        return mapToShort(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> ShortList mapToShort(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToShortFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return ShortList.empty();
        }

        final ShortList result = new ShortList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsShort(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsShort(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> IntList mapToInt(final T[] a, final ToIntFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return IntList.empty();
        }

        return mapToInt(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> IntList mapToInt(final T[] a, final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return IntList.empty();
        }

        final IntList result = new IntList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsInt(a[i]));
        }

        return result;
    }

    public static <T> IntList mapToInt(final Collection<? extends T> c, final ToIntFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return IntList.empty();
        }

        return mapToInt(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> IntList mapToInt(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return IntList.empty();
        }

        final IntList result = new IntList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsInt(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsInt(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> LongList mapToLong(final T[] a, final ToLongFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return LongList.empty();
        }

        return mapToLong(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> LongList mapToLong(final T[] a, final int fromIndex, final int toIndex, final ToLongFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return LongList.empty();
        }

        final LongList result = new LongList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsLong(a[i]));
        }

        return result;
    }

    public static <T> LongList mapToLong(final Collection<? extends T> c, final ToLongFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return LongList.empty();
        }

        return mapToLong(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> LongList mapToLong(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToLongFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return LongList.empty();
        }

        final LongList result = new LongList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsLong(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsLong(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> FloatList mapToFloat(final T[] a, final ToFloatFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return FloatList.empty();
        }

        return mapToFloat(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> FloatList mapToFloat(final T[] a, final int fromIndex, final int toIndex, final ToFloatFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return FloatList.empty();
        }

        final FloatList result = new FloatList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsFloat(a[i]));
        }

        return result;
    }

    public static <T> FloatList mapToFloat(final Collection<? extends T> c, final ToFloatFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return FloatList.empty();
        }

        return mapToFloat(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> FloatList mapToFloat(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToFloatFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return FloatList.empty();
        }

        final FloatList result = new FloatList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsFloat(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsFloat(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> DoubleList mapToDouble(final T[] a, final ToDoubleFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return DoubleList.empty();
        }

        return mapToDouble(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> DoubleList mapToDouble(final T[] a, final int fromIndex, final int toIndex, final ToDoubleFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return DoubleList.empty();
        }

        final DoubleList result = new DoubleList(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.applyAsDouble(a[i]));
        }

        return result;
    }

    public static <T> DoubleList mapToDouble(final Collection<? extends T> c, final ToDoubleFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return DoubleList.empty();
        }

        return mapToDouble(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T> DoubleList mapToDouble(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToDoubleFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return DoubleList.empty();
        }

        final DoubleList result = new DoubleList(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.applyAsDouble(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.applyAsDouble(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T, R> List<R> map(final T[] a, final Function<? super T, ? extends R> func) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        return map(a, 0, a.length, func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T, R> List<R> map(final T[] a, final int fromIndex, final int toIndex, final Function<? super T, ? extends R> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final List<R> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.apply(a[i]));
        }

        return result;
    }

    public static <T, R> List<R> map(final Collection<? extends T> c, final Function<? super T, ? extends R> func) {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        return map(c, 0, c.size(), func);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func 
     * @return
     */
    public static <T, R> List<R> map(final Collection<? extends T> c, final int fromIndex, final int toIndex, final Function<? super T, ? extends R> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return new ArrayList<>();
        }

        final List<R> result = new ArrayList<>(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.apply(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.apply(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T, R, C extends Collection<R>> C map(final T[] a, final Function<? super T, ? extends R> func, final IntFunction<C> supplier) {
        if (N.isNullOrEmpty(a)) {
            return supplier.apply(0);
        }

        return map(a, 0, a.length, func, supplier);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func
     * @param max
     * @param supplier
     * @return
     */
    public static <T, R, C extends Collection<R>> C map(final T[] a, final int fromIndex, final int toIndex, final Function<? super T, ? extends R> func,
            final IntFunction<C> supplier) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return supplier.apply(0);
        }

        final C result = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.apply(a[i]));
        }

        return result;
    }

    public static <T, R, C extends Collection<R>> C map(final Collection<? extends T> c, final Function<? super T, ? extends R> func,
            final IntFunction<C> supplier) {
        if (N.isNullOrEmpty(c)) {
            return supplier.apply(0);
        }

        return map(c, 0, c.size(), func, supplier);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param func
     * @param max
     * @param supplier
     * @return
     */
    public static <T, R, C extends Collection<R>> C map(final Collection<? extends T> c, final int fromIndex, final int toIndex,
            final Function<? super T, ? extends R> func, final IntFunction<C> supplier) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return supplier.apply(0);
        }

        final C result = supplier.apply(toIndex - fromIndex);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result.add(func.apply(list.get(i)));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result.add(func.apply(e));

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> int sumInt(final T[] a, final ToIntFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return sumInt(a, 0, a.length, func);
    }

    public static <T> int sumInt(final T[] a, final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return 0;
        }

        int result = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            result += func.applyAsInt(a[i]);
        }

        return result;
    }

    public static <T> int sumInt(final Collection<? extends T> c, final ToIntFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return 0;
        }

        return sumInt(c, 0, c.size(), func);
    }

    public static <T> int sumInt(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (fromIndex == toIndex) {
            return 0;
        }

        int result = 0;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result += func.applyAsInt(list.get(i));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result += func.applyAsInt(e);

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> long sumLong(final T[] a, final ToLongFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return 0L;
        }

        return sumLong(a, 0, a.length, func);
    }

    public static <T> long sumLong(final T[] a, final int fromIndex, final int toIndex, final ToLongFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return 0L;
        }

        long result = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            result += func.applyAsLong(a[i]);
        }

        return result;
    }

    public static <T> long sumLong(final Collection<? extends T> c, final ToLongFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return 0L;
        }

        return sumLong(c, 0, c.size(), func);
    }

    public static <T> long sumLong(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToLongFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (fromIndex == toIndex) {
            return 0L;
        }

        long result = 0;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result += func.applyAsLong(list.get(i));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result += func.applyAsLong(e);

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> double sumDouble(final T[] a, final ToDoubleFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return 0D;
        }

        return sumDouble(a, 0, a.length, func);
    }

    public static <T> double sumDouble(final T[] a, final int fromIndex, final int toIndex, final ToDoubleFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return 0D;
        }

        double result = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            result += func.applyAsDouble(a[i]);
        }

        return result;
    }

    public static <T> double sumDouble(final Collection<? extends T> c, final ToDoubleFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return 0D;
        }

        return sumDouble(c, 0, c.size(), func);
    }

    public static <T> double sumDouble(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToDoubleFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (fromIndex == toIndex) {
            return 0D;
        }

        double result = 0;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                result += func.applyAsDouble(list.get(i));
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                result += func.applyAsDouble(e);

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return result;
    }

    public static <T> OptionalDouble averageInt(final T[] a, final ToIntFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return OptionalDouble.empty();
        }

        return averageInt(a, 0, a.length, func);
    }

    public static <T> OptionalDouble averageInt(final T[] a, final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        double result = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            result += func.applyAsInt(a[i]);
        }

        return OptionalDouble.of(result / (toIndex - fromIndex));
    }

    public static <T> OptionalDouble averageInt(final Collection<? extends T> c, final ToIntFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return OptionalDouble.empty();
        }

        return averageInt(c, 0, c.size(), func);
    }

    public static <T> OptionalDouble averageInt(final Collection<? extends T> c, final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(sumInt(c, fromIndex, toIndex, func) / (toIndex - fromIndex));
    }

    public static <T> OptionalDouble averageLong(final T[] a, final ToLongFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return OptionalDouble.empty();
        }

        return averageLong(a, 0, a.length, func);
    }

    public static <T> OptionalDouble averageLong(final T[] a, final int fromIndex, final int toIndex, final ToLongFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        double result = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            result += func.applyAsLong(a[i]);
        }

        return OptionalDouble.of(result / (toIndex - fromIndex));
    }

    public static <T> OptionalDouble averageLong(final Collection<? extends T> c, final ToLongFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return OptionalDouble.empty();
        }

        return averageLong(c, 0, c.size(), func);
    }

    public static <T> OptionalDouble averageLong(final Collection<? extends T> c, final int fromIndex, final int toIndex,
            final ToLongFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(sumLong(c, fromIndex, toIndex, func) / (toIndex - fromIndex));
    }

    public static <T> OptionalDouble averageDouble(final T[] a, final ToDoubleFunction<? super T> func) {
        if (N.isNullOrEmpty(a)) {
            return OptionalDouble.empty();
        }

        return averageDouble(a, 0, a.length, func);
    }

    public static <T> OptionalDouble averageDouble(final T[] a, final int fromIndex, final int toIndex, final ToDoubleFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        double result = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            result += func.applyAsDouble(a[i]);
        }

        return OptionalDouble.of(result / (toIndex - fromIndex));
    }

    public static <T> OptionalDouble averageDouble(final Collection<? extends T> c, final ToDoubleFunction<? super T> func) {
        if (N.isNullOrEmpty(c)) {
            return OptionalDouble.empty();
        }

        return averageDouble(c, 0, c.size(), func);
    }

    public static <T> OptionalDouble averageDouble(final Collection<? extends T> c, final int fromIndex, final int toIndex,
            final ToDoubleFunction<? super T> func) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(sumDouble(c, fromIndex, toIndex, func) / (toIndex - fromIndex));
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final boolean[] a, final BooleanPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final boolean[] a, final int fromIndex, final int toIndex, final BooleanPredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final char[] a, final CharPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final char[] a, final int fromIndex, final int toIndex, final CharPredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final byte[] a, final BytePredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final byte[] a, final int fromIndex, final int toIndex, final BytePredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final short[] a, final ShortPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final short[] a, final int fromIndex, final int toIndex, final ShortPredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final int[] a, final IntPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final int[] a, final int fromIndex, final int toIndex, final IntPredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final long[] a, final LongPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final long[] a, final int fromIndex, final int toIndex, final LongPredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final float[] a, final FloatPredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final float[] a, final int fromIndex, final int toIndex, final FloatPredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static int count(final double[] a, final DoublePredicate filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static int count(final double[] a, final int fromIndex, final int toIndex, final DoublePredicate filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param filter
     * @return
     */
    public static <T> int count(final T[] a, final Predicate<? super T> filter) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return count(a, 0, a.length, filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static <T> int count(final T[] a, final int fromIndex, final int toIndex, final Predicate<? super T> filter) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int count = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (filter.test(a[i])) {
                count++;
            }
        }

        return count;
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param c
     * @param filter
     * @return
     */
    public static <T> int count(final Collection<? extends T> c, final Predicate<? super T> filter) {
        if (N.isNullOrEmpty(c)) {
            return 0;
        }

        return count(c, 0, c.size(), filter);
    }

    /**
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public static <T> int count(final Collection<? extends T> c, final int fromIndex, final int toIndex, final Predicate<? super T> filter) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if ((N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < c.size())) {
            return 0;
        }

        int count = 0;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                if (filter.test(list.get(i))) {
                    count++;
                }
            }
        } else {
            int idx = 0;

            for (T e : c) {
                if (idx++ < fromIndex) {
                    continue;
                }

                if (filter.test(e)) {
                    count++;
                }

                if (idx >= toIndex) {
                    break;
                }
            }
        }

        return count;
    }

    public static short[] top(final short[] a, final int n) {
        return top(a, n, null);
    }

    public static short[] top(final short[] a, final int n, final Comparator<? super Short> cmp) {
        return top(a, 0, a.length, n, cmp);
    }

    public static short[] top(final short[] a, final int fromIndex, final int toIndex, final int n) {
        return top(a, fromIndex, toIndex, n, null);
    }

    public static short[] top(final short[] a, final int fromIndex, final int toIndex, final int n, final Comparator<? super Short> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return N.copyOfRange(a, fromIndex, toIndex);
        }

        final Comparator<IndexedShort> pairCmp = cmp == null ? new Comparator<IndexedShort>() {
            @Override
            public int compare(final IndexedShort o1, final IndexedShort o2) {
                return Short.compare(o1.value(), o2.value());
            }
        } : new Comparator<IndexedShort>() {
            @Override
            public int compare(final IndexedShort o1, final IndexedShort o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<IndexedShort> heap = new PriorityQueue<>(n, pairCmp);
        IndexedShort pair = null;

        for (int i = fromIndex; i < toIndex; i++) {
            pair = IndexedShort.of(a[i], i);

            if (heap.size() >= n) {
                if (pairCmp.compare(heap.peek(), pair) < 0) {
                    heap.poll();
                    heap.add(pair);
                }
            } else {
                heap.offer(pair);
            }
        }

        final IndexedShort[] arrayOfPair = heap.toArray(new IndexedShort[heap.size()]);

        N.sort(arrayOfPair, new Comparator<IndexedShort>() {
            @Override
            public int compare(final IndexedShort o1, final IndexedShort o2) {
                return o1.index() - o2.index();
            }
        });

        final short[] res = new short[arrayOfPair.length];

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res[i] = arrayOfPair[i].value();
        }

        return res;
    }

    public static int[] top(final int[] a, final int n) {
        return top(a, n, null);
    }

    public static int[] top(final int[] a, final int n, final Comparator<? super Integer> cmp) {
        return top(a, 0, a.length, n, cmp);
    }

    public static int[] top(final int[] a, final int fromIndex, final int toIndex, final int n) {
        return top(a, fromIndex, toIndex, n, null);
    }

    public static int[] top(final int[] a, final int fromIndex, final int toIndex, final int n, final Comparator<? super Integer> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return N.copyOfRange(a, fromIndex, toIndex);
        }

        final Comparator<IndexedInt> pairCmp = cmp == null ? new Comparator<IndexedInt>() {
            @Override
            public int compare(final IndexedInt o1, final IndexedInt o2) {
                return Integer.compare(o1.value(), o2.value());
            }
        } : new Comparator<IndexedInt>() {
            @Override
            public int compare(final IndexedInt o1, final IndexedInt o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<IndexedInt> heap = new PriorityQueue<>(n, pairCmp);
        IndexedInt pair = null;

        for (int i = fromIndex; i < toIndex; i++) {
            pair = IndexedInt.of(a[i], i);

            if (heap.size() >= n) {
                if (pairCmp.compare(heap.peek(), pair) < 0) {
                    heap.poll();
                    heap.add(pair);
                }
            } else {
                heap.offer(pair);
            }
        }

        final IndexedInt[] arrayOfPair = heap.toArray(new IndexedInt[heap.size()]);

        N.sort(arrayOfPair, new Comparator<IndexedInt>() {
            @Override
            public int compare(final IndexedInt o1, final IndexedInt o2) {
                return o1.index() - o2.index();
            }
        });

        final int[] res = new int[arrayOfPair.length];

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res[i] = arrayOfPair[i].value();
        }

        return res;
    }

    public static long[] top(final long[] a, final int n) {
        return top(a, n, null);
    }

    public static long[] top(final long[] a, final int n, final Comparator<? super Long> cmp) {
        return top(a, 0, a.length, n, cmp);
    }

    public static long[] top(final long[] a, final int fromIndex, final int toIndex, final int n) {
        return top(a, fromIndex, toIndex, n, null);
    }

    public static long[] top(final long[] a, final int fromIndex, final int toIndex, final int n, final Comparator<? super Long> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return N.copyOfRange(a, fromIndex, toIndex);
        }

        final Comparator<IndexedLong> pairCmp = cmp == null ? new Comparator<IndexedLong>() {
            @Override
            public int compare(final IndexedLong o1, final IndexedLong o2) {
                return Long.compare(o1.value(), o2.value());
            }
        } : new Comparator<IndexedLong>() {
            @Override
            public int compare(final IndexedLong o1, final IndexedLong o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<IndexedLong> heap = new PriorityQueue<>(n, pairCmp);
        IndexedLong pair = null;

        for (int i = fromIndex; i < toIndex; i++) {
            pair = IndexedLong.of(a[i], i);

            if (heap.size() >= n) {
                if (pairCmp.compare(heap.peek(), pair) < 0) {
                    heap.poll();
                    heap.add(pair);
                }
            } else {
                heap.offer(pair);
            }
        }

        final IndexedLong[] arrayOfPair = heap.toArray(new IndexedLong[heap.size()]);

        N.sort(arrayOfPair, new Comparator<IndexedLong>() {
            @Override
            public int compare(final IndexedLong o1, final IndexedLong o2) {
                return o1.index() - o2.index();
            }
        });

        final long[] res = new long[arrayOfPair.length];

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res[i] = arrayOfPair[i].value();
        }

        return res;
    }

    public static float[] top(final float[] a, final int n) {
        return top(a, n, null);
    }

    public static float[] top(final float[] a, final int n, final Comparator<? super Float> cmp) {
        return top(a, 0, a.length, n, cmp);
    }

    public static float[] top(final float[] a, final int fromIndex, final int toIndex, final int n) {
        return top(a, fromIndex, toIndex, n, null);
    }

    public static float[] top(final float[] a, final int fromIndex, final int toIndex, final int n, final Comparator<? super Float> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return N.copyOfRange(a, fromIndex, toIndex);
        }

        final Comparator<IndexedFloat> pairCmp = cmp == null ? new Comparator<IndexedFloat>() {
            @Override
            public int compare(final IndexedFloat o1, final IndexedFloat o2) {
                return Float.compare(o1.value(), o2.value());
            }
        } : new Comparator<IndexedFloat>() {
            @Override
            public int compare(final IndexedFloat o1, final IndexedFloat o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<IndexedFloat> heap = new PriorityQueue<>(n, pairCmp);
        IndexedFloat pair = null;

        for (int i = fromIndex; i < toIndex; i++) {
            pair = IndexedFloat.of(a[i], i);

            if (heap.size() >= n) {
                if (pairCmp.compare(heap.peek(), pair) < 0) {
                    heap.poll();
                    heap.add(pair);
                }
            } else {
                heap.offer(pair);
            }
        }

        final IndexedFloat[] arrayOfPair = heap.toArray(new IndexedFloat[heap.size()]);

        N.sort(arrayOfPair, new Comparator<IndexedFloat>() {
            @Override
            public int compare(final IndexedFloat o1, final IndexedFloat o2) {
                return o1.index() - o2.index();
            }
        });

        final float[] res = new float[arrayOfPair.length];

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res[i] = arrayOfPair[i].value();
        }

        return res;
    }

    public static double[] top(final double[] a, final int n) {
        return top(a, n, null);
    }

    public static double[] top(final double[] a, final int n, final Comparator<? super Double> cmp) {
        return top(a, 0, a.length, n, cmp);
    }

    public static double[] top(final double[] a, final int fromIndex, final int toIndex, final int n) {
        return top(a, fromIndex, toIndex, n, null);
    }

    public static double[] top(final double[] a, final int fromIndex, final int toIndex, final int n, final Comparator<? super Double> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return N.copyOfRange(a, fromIndex, toIndex);
        }

        final Comparator<IndexedDouble> pairCmp = cmp == null ? new Comparator<IndexedDouble>() {
            @Override
            public int compare(final IndexedDouble o1, final IndexedDouble o2) {
                return Double.compare(o1.value(), o2.value());
            }
        } : new Comparator<IndexedDouble>() {
            @Override
            public int compare(final IndexedDouble o1, final IndexedDouble o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<IndexedDouble> heap = new PriorityQueue<>(n, pairCmp);
        IndexedDouble pair = null;

        for (int i = fromIndex; i < toIndex; i++) {
            pair = IndexedDouble.of(a[i], i);

            if (heap.size() >= n) {
                if (pairCmp.compare(heap.peek(), pair) < 0) {
                    heap.poll();
                    heap.add(pair);
                }
            } else {
                heap.offer(pair);
            }
        }

        final IndexedDouble[] arrayOfPair = heap.toArray(new IndexedDouble[heap.size()]);

        N.sort(arrayOfPair, new Comparator<IndexedDouble>() {
            @Override
            public int compare(final IndexedDouble o1, final IndexedDouble o2) {
                return o1.index() - o2.index();
            }
        });

        final double[] res = new double[arrayOfPair.length];

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res[i] = arrayOfPair[i].value();
        }

        return res;
    }

    public static <T extends Comparable<T>> T[] top(final T[] a, final int n) {
        return (T[]) top(a, n, N.OBJ_COMPARATOR);
    }

    public static <T> T[] top(final T[] a, final int n, final Comparator<? super T> cmp) {
        return top(a, 0, a.length, n, cmp);
    }

    public static <T extends Comparable<T>> T[] top(final T[] a, final int fromIndex, final int toIndex, final int n) {
        return (T[]) top(a, fromIndex, toIndex, n, N.OBJ_COMPARATOR);
    }

    @SuppressWarnings("rawtypes")
    public static <T> T[] top(final T[] a, final int fromIndex, final int toIndex, final int n, final Comparator<? super T> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return N.copyOfRange(a, fromIndex, toIndex);
        }

        final Comparator<Indexed<T>> pairCmp = cmp == null ? (Comparator) new Comparator<Indexed<Comparable>>() {
            @Override
            public int compare(final Indexed<Comparable> o1, final Indexed<Comparable> o2) {
                return N.compare(o1.value(), o2.value());
            }
        } : new Comparator<Indexed<T>>() {
            @Override
            public int compare(final Indexed<T> o1, final Indexed<T> o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<Indexed<T>> heap = new PriorityQueue<>(n, pairCmp);
        Indexed<T> pair = null;

        for (int i = fromIndex; i < toIndex; i++) {
            pair = Indexed.of(a[i], i);

            if (heap.size() >= n) {
                if (pairCmp.compare(heap.peek(), pair) < 0) {
                    heap.poll();
                    heap.add(pair);
                }
            } else {
                heap.offer(pair);
            }
        }

        final Indexed<T>[] arrayOfPair = heap.toArray(new Indexed[heap.size()]);

        N.sort(arrayOfPair, new Comparator<Indexed<T>>() {
            @Override
            public int compare(final Indexed<T> o1, final Indexed<T> o2) {
                return o1.index() - o2.index();
            }
        });

        final T[] res = N.newArray(a.getClass().getComponentType(), arrayOfPair.length);

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res[i] = arrayOfPair[i].value();
        }

        return res;
    }

    public static <T extends Comparable<T>> List<T> top(final Collection<? extends T> c, final int n) {
        return top(c, n, null);
    }

    public static <T> List<T> top(final Collection<? extends T> c, final int n, final Comparator<? super T> cmp) {
        return top(c, 0, c.size(), n, cmp);
    }

    public static <T extends Comparable<T>> List<T> top(final Collection<? extends T> c, final int fromIndex, final int toIndex, final int n) {
        return top(c, fromIndex, toIndex, n, null);
    }

    @SuppressWarnings("rawtypes")
    public static <T> List<T> top(final Collection<? extends T> c, final int fromIndex, final int toIndex, final int n, final Comparator<? super T> cmp) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            final List<T> res = new ArrayList<>(toIndex - fromIndex);
            final Iterator<? extends T> iter = c.iterator();
            T e = null;

            for (int i = 0; i < toIndex && iter.hasNext(); i++) {
                e = iter.next();

                if (i < fromIndex) {
                    continue;
                }

                res.add(e);
            }

            return res;
        }

        final Comparator<Indexed<T>> pairCmp = cmp == null ? (Comparator) new Comparator<Indexed<Comparable>>() {
            @Override
            public int compare(final Indexed<Comparable> o1, final Indexed<Comparable> o2) {
                return N.compare(o1.value(), o2.value());
            }
        } : new Comparator<Indexed<T>>() {
            @Override
            public int compare(final Indexed<T> o1, final Indexed<T> o2) {
                return cmp.compare(o1.value(), o2.value());
            }
        };

        final Queue<Indexed<T>> heap = new PriorityQueue<>(n, pairCmp);

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;
            Indexed<T> pair = null;
            T e = null;

            for (int i = fromIndex; i < toIndex; i++) {
                e = list.get(i);

                pair = Indexed.of(e, i);

                if (heap.size() >= n) {
                    if (pairCmp.compare(heap.peek(), pair) < 0) {
                        heap.poll();
                        heap.add(pair);
                    }
                } else {
                    heap.offer(pair);
                }
            }
        } else {
            final Iterator<? extends T> iter = c.iterator();
            Indexed<T> pair = null;
            T e = null;

            for (int i = 0; i < toIndex && iter.hasNext(); i++) {
                e = iter.next();

                if (i < fromIndex) {
                    continue;
                }

                pair = Indexed.of(e, i);

                if (heap.size() >= n) {
                    if (pairCmp.compare(heap.peek(), pair) < 0) {
                        heap.poll();
                        heap.add(pair);
                    }
                } else {
                    heap.offer(pair);
                }
            }
        }

        final Indexed<T>[] arrayOfPair = heap.toArray(new Indexed[heap.size()]);

        N.sort(arrayOfPair, new Comparator<Indexed<T>>() {
            @Override
            public int compare(final Indexed<T> o1, final Indexed<T> o2) {
                return o1.index() - o2.index();
            }
        });

        final List<T> res = new ArrayList<>(arrayOfPair.length);

        for (int i = 0, len = arrayOfPair.length; i < len; i++) {
            res.add(arrayOfPair[i].value());
        }

        return res;
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static boolean[] distinct(final boolean[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static boolean[] distinct(final boolean[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static char[] distinct(final char[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static char[] distinct(final char[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static byte[] distinct(final byte[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static byte[] distinct(final byte[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static short[] distinct(final short[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static short[] distinct(final short[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static int[] distinct(final int[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static int[] distinct(final int[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static long[] distinct(final long[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static long[] distinct(final long[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static float[] distinct(final float[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static float[] distinct(final float[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static double[] distinct(final double[] a) {
        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static double[] distinct(final double[] a, final int fromIndex, final int toIndex) {
        return N.removeDuplicates(a, fromIndex, toIndex, false);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @return
     */
    public static <T> T[] distinct(final T[] a) {
        if (N.isNullOrEmpty(a)) {
            return a;
        }

        return distinct(a, 0, a.length);
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static <T> T[] distinct(final T[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return a;
        }

        final List<T> result = new ArrayList<>();
        final Set<Object> set = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            if (set.add(hashKey(a[i]))) {
                result.add(a[i]);
            }
        }

        return result.toArray((T[]) N.newArray(a.getClass().getComponentType(), result.size()));
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param c
     * @return
     */
    public static <T> List<T> distinct(final Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        return distinct(c, 0, c.size());
    }

    /**
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public static <T> List<T> distinct(final Collection<? extends T> c, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return new ArrayList<>();
        }

        final List<T> result = new ArrayList<>();
        final Set<Object> set = new HashSet<>();
        T e = null;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                e = list.get(i);

                if (set.add(hashKey(e))) {
                    result.add(e);
                }
            }
        } else {
            final Iterator<? extends T> it = c.iterator();

            for (int i = 0; i < toIndex && it.hasNext(); i++) {
                e = it.next();

                if (i < fromIndex) {
                    continue;
                }

                if (set.add(hashKey(e))) {
                    result.add(e);
                }
            }
        }

        return result;
    }

    /**
     * Distinct by the value mapped from <code>keyExtractor</code>.
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param keyExtractor don't change value of the input parameter.
     * @return
     */
    public static <T> T[] distinctBy(final T[] a, final Function<? super T, ?> keyExtractor) {
        if (N.isNullOrEmpty(a)) {
            return a;
        }

        return distinctBy(a, 0, a.length, keyExtractor);
    }

    /**
     * Distinct by the value mapped from <code>keyExtractor</code>.
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param keyExtractor don't change value of the input parameter.
     * @return
     */
    public static <T> T[] distinctBy(final T[] a, final int fromIndex, final int toIndex, final Function<? super T, ?> keyExtractor) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            return a;
        }

        final List<T> result = new ArrayList<>();
        final Set<Object> set = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            if (set.add(hashKey(keyExtractor.apply(a[i])))) {
                result.add(a[i]);
            }
        }

        return result.toArray((T[]) N.newArray(a.getClass().getComponentType(), result.size()));
    }

    /**
     * Distinct by the value mapped from <code>keyExtractor</code>.
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param c
     * @param keyExtractor don't change value of the input parameter.
     * @return
     */
    public static <T> List<T> distinctBy(final Collection<? extends T> c, final Function<? super T, ?> keyExtractor) {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        return distinctBy(c, 0, c.size(), keyExtractor);
    }

    /**
     * Distinct by the value mapped from <code>keyExtractor</code>.
     * 
     * Mostly it's designed for one-step operation to complete the operation in one step.
     * <code>java.util.stream.Stream</code> is preferred for multiple phases operation.
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param keyExtractor don't change value of the input parameter.
     * @return
     */
    public static <T> List<T> distinctBy(final Collection<? extends T> c, final int fromIndex, final int toIndex, final Function<? super T, ?> keyExtractor) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) && fromIndex == 0 && toIndex == 0) {
            return new ArrayList<>();
        }

        final List<T> result = new ArrayList<>();
        final Set<Object> set = new HashSet<>();
        T e = null;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i++) {
                e = list.get(i);

                if (set.add(hashKey(keyExtractor.apply(e)))) {
                    result.add(e);
                }
            }
        } else {
            final Iterator<? extends T> it = c.iterator();

            for (int i = 0; i < toIndex && it.hasNext(); i++) {
                e = it.next();

                if (i < fromIndex) {
                    continue;
                }

                if (set.add(hashKey(keyExtractor.apply(e)))) {
                    result.add(e);
                }
            }
        }

        return result;
    }

    private static Object hashKey(Object obj) {
        return obj == null || obj.getClass().isArray() == false ? obj : Wrapper.of(obj);
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<boolean[]> split(final boolean[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<boolean[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<boolean[]> split(final boolean[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<boolean[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<char[]> split(final char[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<char[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<char[]> split(final char[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<char[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<byte[]> split(final byte[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<byte[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<byte[]> split(final byte[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<byte[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<short[]> split(final short[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<short[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<short[]> split(final short[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<short[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<int[]> split(final int[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<int[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<int[]> split(final int[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<int[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<long[]> split(final long[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<long[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<long[]> split(final long[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<long[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<float[]> split(final float[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<float[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<float[]> split(final float[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<float[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static List<double[]> split(final double[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<double[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static List<double[]> split(final double[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<double[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param size
     * @return
     */
    public static <T> List<T[]> split(final T[] a, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = a.length;
        final List<T[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = 0, toIndex = a.length; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub arrays of an array, each of the same size (the final list may be smaller),
     * or an empty List if the specified array is null or empty.
     * 
     * @param a
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static <T> List<T[]> split(final T[] a, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<T[]> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(copyOfRange(a, from, from <= toIndex - size ? from + size : toIndex));
        }

        return res;
    }

    /**
     * Returns consecutive sub lists of a collection, each of the same size (the final list may be smaller).
     * or an empty List if the specified collection is null or empty. The order of elements in the original collection is kept
     * 
     * @param c
     * @param size
     * @return
     */
    public static <T> List<List<T>> split(final Collection<? extends T> c, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        return split(c, 0, c.size(), size);
    }

    /**
     * Returns consecutive sub lists of a collection, each of the same size (the final list may be smaller).
     * or an empty List if the specified collection is null or empty. The order of elements in the original collection is kept
     * 
     * @param c
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public static <T> List<List<T>> split(final Collection<? extends T> c, final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex, c == null ? 0 : c.size());

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<List<T>> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        if (c instanceof List) {
            final List<T> list = (List<T>) c;

            for (int i = fromIndex; i < toIndex; i += size) {
                res.add(new ArrayList<>(list.subList(i, i <= toIndex - size ? i + size : toIndex)));
            }
        } else {
            final Iterator<? extends T> iter = c.iterator();

            for (int i = 0; i < toIndex; i += size) {
                if (i < fromIndex) {
                    iter.next();
                    i++;
                    continue;
                }

                final List<T> subList = new ArrayList<>(N.min(size, toIndex - i));

                for (int j = i, to = i <= toIndex - size ? i + size : toIndex; j < to; j++) {
                    subList.add(iter.next());
                }

                res.add(subList);
            }
        }

        return res;
    }

    /**
     * Returns consecutive substring of the specified string, each of the same length (the final list may be smaller),
     * or an empty array if the specified string is null or empty.
     * 
     * @param str
     * @param size
     * @return
     */
    public static List<String> split(final CharSequence str, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(str)) {
            return new ArrayList<>();
        }

        return split(str, 0, str.length(), size);
    }

    public static List<String> split(final CharSequence str, final int fromIndex, final int toIndex, final int size) {
        N.checkFromToIndex(fromIndex, toIndex, str == null ? 0 : str.length());

        if (size < 1) {
            throw new IllegalArgumentException("The parameter 'size' can't be zero or less than zero");
        }

        if (N.isNullOrEmpty(str)) {
            return new ArrayList<>();
        }

        final int len = toIndex - fromIndex;
        final List<String> res = new ArrayList<>(len % size == 0 ? len / size : (len / size) + 1);

        for (int from = fromIndex; from < toIndex; from += size) {
            res.add(str.subSequence(from, from <= toIndex - size ? from + size : toIndex).toString());
        }

        return res;
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static boolean[] intersection(final boolean[] a, final boolean[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? a : N.EMPTY_BOOLEAN_ARRAY;
        }

        return BooleanList.of(a).intersection(BooleanList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static char[] intersection(final char[] a, final char[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        return CharList.of(a).intersection(CharList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static byte[] intersection(final byte[] a, final byte[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        return ByteList.of(a).intersection(ByteList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static short[] intersection(final short[] a, final short[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        return ShortList.of(a).intersection(ShortList.of(b)).trimToSize().array();
    }

    /**
     * Returns a new array with all the elements in <code>b</code> removed by occurrences.
     * 
     * <pre>
     * int[] a = {0, 1, 2, 2, 3};
     * int[] b = {2, 5, 1};
     * int[] c = retainAll(a, b); // The elements c in a will b: [1, 2, 2].
     * 
     * int[] a = {0, 1, 2, 2, 3};
     * int[] b = {2, 5, 1};
     * int[] c = intersection(a, b); // The elements c in a will b: [1, 2].
     * </pre>
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static int[] intersection(final int[] a, final int[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_INT_ARRAY;
        }

        return IntList.of(a).intersection(IntList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static long[] intersection(final long[] a, final long[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_LONG_ARRAY;
        }

        return LongList.of(a).intersection(LongList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static float[] intersection(final float[] a, final float[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        return FloatList.of(a).intersection(FloatList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static double[] intersection(final double[] a, final double[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        return DoubleList.of(a).intersection(DoubleList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static <T> T[] intersection(final T[] a, final Object[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? a : (T[]) N.newArray(a.getClass().getComponentType(), 0);
        }

        final Multiset<?> bOccurrences = Multiset.of(b);
        final List<T> result = new ArrayList<>(N.min(9, a.length, b.length));

        for (T e : a) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }
        }

        return result.toArray((T[]) N.newArray(a.getClass().getComponentType(), result.size()));
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public static <T> List<T> intersection(final Collection<? extends T> a, final Collection<?> b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return new ArrayList<>();
        }

        final Multiset<Object> bOccurrences = Multiset.from(b);

        final List<T> result = new ArrayList<>(N.min(a.size(), N.max(9, a.size() - b.size())));

        for (T e : a) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }
        }

        return result;
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static boolean[] difference(final boolean[] a, final boolean[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return BooleanList.of(a).difference(BooleanList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static char[] difference(final char[] a, final char[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return CharList.of(a).difference(CharList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static byte[] difference(final byte[] a, final byte[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return ByteList.of(a).difference(ByteList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static short[] difference(final short[] a, final short[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return ShortList.of(a).difference(ShortList.of(b)).trimToSize().array();
    }

    /**
     * Returns a new array with all the elements in <code>b</code> removed by occurrences.
     * 
     * <pre>
     * int[] a = {0, 1, 2, 2, 3};
     * int[] b = {2, 5, 1};
     * int[] c = removeAll(a, b); // The elements c in a will b: [0, 3].
     * 
     * int[] a = {0, 1, 2, 2, 3};
     * int[] b = {2, 5, 1};
     * int[] c = difference(a, b); // The elements c in a will b: [0, 2, 3].
     * </pre>
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static int[] difference(final int[] a, final int[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return IntList.of(a).difference(IntList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static long[] difference(final long[] a, final long[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return LongList.of(a).difference(LongList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static float[] difference(final float[] a, final float[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return FloatList.of(a).difference(FloatList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static double[] difference(final double[] a, final double[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return DoubleList.of(a).difference(DoubleList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static <T> T[] difference(final T[] a, final Object[] b) {
        if (N.isNullOrEmpty(a)) {
            return a;
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        final Multiset<?> bOccurrences = Multiset.of(b);
        final List<T> result = new ArrayList<>(N.min(9, a.length, b.length));

        for (T e : a) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        return result.toArray((T[]) N.newArray(a.getClass().getComponentType(), result.size()));
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public static <T> List<T> difference(final Collection<? extends T> a, final Collection<?> b) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        final Multiset<Object> bOccurrences = Multiset.from(b);

        final List<T> result = new ArrayList<>(N.min(a.size(), N.max(9, a.size() - b.size())));

        for (T e : a) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        return result;
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static boolean[] symmetricDifference(final boolean[] a, final boolean[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_BOOLEAN_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return BooleanList.of(a).symmetricDifference(BooleanList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static char[] symmetricDifference(final char[] a, final char[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_CHAR_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return CharList.of(a).symmetricDifference(CharList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static byte[] symmetricDifference(final byte[] a, final byte[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_BYTE_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return ByteList.of(a).symmetricDifference(ByteList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static short[] symmetricDifference(final short[] a, final short[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_SHORT_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return ShortList.of(a).symmetricDifference(ShortList.of(b)).trimToSize().array();
    }

    /**
     * <pre>
     * int[] a = {0, 1, 2, 2, 3};
     * int[] b = {2, 5, 1};
     * int[] c = symmetricDifference(a, b); // The elements c in a will b: [0, 2, 3, 5].
     * </pre>
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     * @see N#difference(int[], int[])
     */
    public static int[] symmetricDifference(final int[] a, final int[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_INT_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return IntList.of(a).symmetricDifference(IntList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static long[] symmetricDifference(final long[] a, final long[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_LONG_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return LongList.of(a).symmetricDifference(LongList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static float[] symmetricDifference(final float[] a, final float[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_FLOAT_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return FloatList.of(a).symmetricDifference(FloatList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static double[] symmetricDifference(final double[] a, final double[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_DOUBLE_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        return DoubleList.of(a).symmetricDifference(DoubleList.of(b)).trimToSize().array();
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static <T> T[] symmetricDifference(final T[] a, final T[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? a : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        final Multiset<T> bOccurrences = Multiset.of(b);

        final List<T> result = new ArrayList<>(N.max(9, Math.abs(a.length - b.length)));

        for (T e : a) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        for (T e : b) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }

            if (bOccurrences.isEmpty()) {
                break;
            }
        }

        return result.toArray((T[]) N.newArray(a.getClass().getComponentType(), result.size()));
    }

    /**
     * 
     * @param a
     * @param b
     * @return
     * @see IntList#symmetricDifference(IntList)
     */
    public static <T> List<T> symmetricDifference(final Collection<? extends T> a, final Collection<? extends T> b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? new ArrayList<T>() : N.asList((T[]) b.toArray());
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? new ArrayList<T>() : N.asList((T[]) a.toArray());
        }

        //        final List<T> result = difference(a, b);
        //
        //        result.addAll(difference(b, a));
        //
        //        return result;

        final Multiset<T> bOccurrences = Multiset.from(b);
        final List<T> result = new ArrayList<>(N.max(9, Math.abs(a.size() - b.size())));

        for (T e : a) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        for (T e : b) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }

            if (bOccurrences.isEmpty()) {
                break;
            }
        }

        return result;
    }

    /**
     * Returns <code>a + b</code>
     * 
     * @param a
     * @param b
     * @return
     */
    public static String concat(final String a, final String b) {
        return a + b;
    }

    public static String concat(final String a, final String b, final String c) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            return sb.append(a).append(b).append(b).toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    @SafeVarargs
    public static String concat(final String... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (String e : a) {
                sb.append(e);
            }
            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static boolean[] concat(final boolean[] a, final boolean[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_BOOLEAN_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_BOOLEAN_ARRAY : a.clone();
        }

        final boolean[] c = new boolean[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static boolean[] concat(final boolean[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_BOOLEAN_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (boolean[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final boolean[] c = new boolean[len];
        int fromIndex = 0;

        for (boolean[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static char[] concat(final char[] a, final char[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_CHAR_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_CHAR_ARRAY : a.clone();
        }

        final char[] c = new char[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static char[] concat(final char[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_CHAR_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_CHAR_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (char[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final char[] c = new char[len];
        int fromIndex = 0;

        for (char[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static byte[] concat(final byte[] a, final byte[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_BYTE_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_BYTE_ARRAY : a.clone();
        }

        final byte[] c = new byte[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static byte[] concat(final byte[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_BYTE_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_BYTE_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (byte[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final byte[] c = new byte[len];
        int fromIndex = 0;

        for (byte[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static short[] concat(final short[] a, final short[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_SHORT_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_SHORT_ARRAY : a.clone();
        }

        final short[] c = new short[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static short[] concat(final short[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_SHORT_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_SHORT_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (short[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final short[] c = new short[len];
        int fromIndex = 0;

        for (short[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static int[] concat(final int[] a, final int[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_INT_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_INT_ARRAY : a.clone();
        }

        final int[] c = new int[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static int[] concat(final int[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_INT_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_INT_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (int[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final int[] c = new int[len];
        int fromIndex = 0;

        for (int[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static long[] concat(final long[] a, final long[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_LONG_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_LONG_ARRAY : a.clone();
        }

        final long[] c = new long[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static long[] concat(final long[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_LONG_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_LONG_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (long[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final long[] c = new long[len];
        int fromIndex = 0;

        for (long[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static float[] concat(final float[] a, final float[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_FLOAT_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_FLOAT_ARRAY : a.clone();
        }

        final float[] c = new float[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static float[] concat(final float[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_FLOAT_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_FLOAT_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (float[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final float[] c = new float[len];
        int fromIndex = 0;

        for (float[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    public static double[] concat(final double[] a, final double[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_DOUBLE_ARRAY : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(a) ? N.EMPTY_DOUBLE_ARRAY : a.clone();
        }

        final double[] c = new double[a.length + b.length];

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     */
    @SafeVarargs
    public static double[] concat(final double[]... aa) {
        if (N.isNullOrEmpty(aa)) {
            return N.EMPTY_DOUBLE_ARRAY;
        } else if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? N.EMPTY_DOUBLE_ARRAY : aa[0].clone();
        }

        int len = 0;

        for (double[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final double[] c = new double[len];
        int fromIndex = 0;

        for (double[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    /**
     *
     * @param a
     * @param b
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> T[] concat(final T[] a, final T[] b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? a : b.clone();
        } else if (N.isNullOrEmpty(b)) {
            return a.clone();
        }

        final T[] c = (T[]) newArray(a.getClass().getComponentType(), a.length + b.length);

        copy(a, 0, c, 0, a.length);
        copy(b, 0, c, a.length, b.length);

        return c;
    }

    /**
     * 
     * @param aa 
     * @return
     * @throws NullPointerException if the specified <code>aa</code> is <code>null</code>.
     */
    @SafeVarargs
    public static <T> T[] concat(final T[]... aa) {
        N.requireNonNull(aa, "aa");

        if (aa.length == 1) {
            return N.isNullOrEmpty(aa[0]) ? aa[0] : aa[0].clone();
        }

        int len = 0;

        for (T[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            len += a.length;
        }

        final T[] c = N.newArray(aa.getClass().getComponentType().getComponentType(), len);
        int fromIndex = 0;

        for (T[] a : aa) {
            if (N.isNullOrEmpty(a)) {
                continue;
            }

            System.arraycopy(a, 0, c, fromIndex, a.length);

            fromIndex += a.length;
        }

        return c;
    }

    public static <T> List<T> concat(final Collection<? extends T> a, final Collection<? extends T> b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? new ArrayList<T>(0) : new ArrayList<>(b);
        } else if (N.isNullOrEmpty(b)) {
            return new ArrayList<>(a);
        }

        final List<T> result = new ArrayList<>(a.size() + b.size());

        result.addAll(a);
        result.addAll(b);

        return result;
    }

    public static int replaceAll(final boolean[] a, final boolean oldVal, final boolean newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == oldVal) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final char[] a, final char oldVal, final char newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == oldVal) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final byte[] a, final byte oldVal, final byte newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == oldVal) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final short[] a, final short oldVal, final short newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == oldVal) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final int[] a, final int oldVal, final int newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == oldVal) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final long[] a, final long oldVal, final long newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == oldVal) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final float[] a, final float oldVal, final float newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (Float.compare(a[i], oldVal) == 0) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static int replaceAll(final double[] a, final double oldVal, final double newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (Double.compare(a[i], oldVal) == 0) {
                a[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public static <T> int replaceAll(final T[] a, final Object oldVal, final T newVal) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        int result = 0;

        if (oldVal == null) {
            for (int i = 0, len = a.length; i < len; i++) {
                if (a[i] == null) {
                    a[i] = newVal;

                    result++;
                }
            }
        } else {
            for (int i = 0, len = a.length; i < len; i++) {
                if (equals(a[i], oldVal)) {
                    a[i] = newVal;

                    result++;
                }
            }
        }

        return result;
    }

    public static <T> int replaceAll(final List<T> list, final Object oldVal, final T newVal) {
        if (N.isNullOrEmpty(list)) {
            return 0;
        }

        int result = 0;

        final int size = list.size();

        if (size < REPLACEALL_THRESHOLD || list instanceof RandomAccess) {
            if (oldVal == null) {
                for (int i = 0; i < size; i++) {
                    if (list.get(i) == null) {
                        list.set(i, newVal);

                        result++;
                    }
                }
            } else {
                for (int i = 0; i < size; i++) {
                    if (oldVal.equals(list.get(i))) {
                        list.set(i, newVal);

                        result++;
                    }
                }
            }
        } else {
            final ListIterator<T> itr = list.listIterator();

            if (oldVal == null) {
                for (int i = 0; i < size; i++) {
                    if (itr.next() == null) {
                        itr.set(newVal);

                        result++;
                    }
                }
            } else {
                for (int i = 0; i < size; i++) {
                    if (oldVal.equals(itr.next())) {
                        itr.set(newVal);

                        result++;
                    }
                }
            }
        }

        return result;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static boolean[] add(final boolean[] a, final boolean element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final boolean[] newArray = new boolean[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static char[] add(final char[] a, final char element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final char[] newArray = new char[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static byte[] add(final byte[] a, final byte element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final byte[] newArray = new byte[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static short[] add(final short[] a, final short element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final short[] newArray = new short[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static int[] add(final int[] a, final int element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final int[] newArray = new int[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static long[] add(final long[] a, final long element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final long[] newArray = new long[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static float[] add(final float[] a, final float element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final float[] newArray = new float[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static double[] add(final double[] a, final double element) {
        if (N.isNullOrEmpty(a)) {
            return Array.of(element);
        }

        final double[] newArray = new double[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     */
    public static String[] add(final String[] a, final String element) {
        if (N.isNullOrEmpty(a)) {
            return N.asArray(element);
        }

        final String[] newArray = new String[a.length + 1];

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Copies the given array and adds the given element at the end of the new
     * array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements plus the new element
     * @throws NullPointerException if the specified <code>a</code> is <code>null</code>.
     */
    public static <T> T[] add(final T[] a, final T element) {
        N.requireNonNull(a, "a");

        if (N.isNullOrEmpty(a)) {
            return N.asArray(element);
        }

        final T[] newArray = (T[]) Array.newInstance(a.getClass().getComponentType(), a.length + 1);

        copy(a, 0, newArray, 0, a.length);
        newArray[a.length] = element;

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static boolean[] addAll(final boolean[] a, final boolean... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_BOOLEAN_ARRAY : b.clone();
        }

        final boolean[] newArray = new boolean[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static char[] addAll(final char[] a, final char... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_CHAR_ARRAY : b.clone();
        }

        final char[] newArray = new char[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static byte[] addAll(final byte[] a, final byte... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_BYTE_ARRAY : b.clone();
        }

        final byte[] newArray = new byte[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static short[] addAll(final short[] a, final short... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_SHORT_ARRAY : b.clone();
        }

        final short[] newArray = new short[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static int[] addAll(final int[] a, final int... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_INT_ARRAY : b.clone();
        }

        final int[] newArray = new int[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static long[] addAll(final long[] a, final long... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_LONG_ARRAY : b.clone();
        }

        final long[] newArray = new long[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static float[] addAll(final float[] a, final float... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_FLOAT_ARRAY : b.clone();
        }

        final float[] newArray = new float[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static double[] addAll(final double[] a, final double... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_DOUBLE_ARRAY : b.clone();
        }

        final double[] newArray = new double[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static String[] addAll(final String[] a, final String... b) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? N.EMPTY_STRING_ARRAY : b.clone();
        }

        final String[] newArray = new String[a.length + b.length];

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Adds all the elements of the given arrays into a new array.
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     * @throws NullPointerException if the specified <code>a</code> is <code>null</code>.
     */
    @SafeVarargs
    public static <T> T[] addAll(final T[] a, final T... b) {
        N.requireNonNull(a, "a");

        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? b : b.clone();
        }

        final T[] newArray = (T[]) Array.newInstance(a.getClass().getComponentType(), a.length + b.length);

        copy(a, 0, newArray, 0, a.length);
        copy(b, 0, newArray, a.length, b.length);

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static boolean[] insert(final boolean[] a, final int index, final boolean element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final boolean[] newArray = new boolean[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static char[] insert(final char[] a, final int index, final char element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final char[] newArray = new char[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static byte[] insert(final byte[] a, final int index, final byte element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final byte[] newArray = new byte[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static short[] insert(final short[] a, final int index, final short element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final short[] newArray = new short[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static int[] insert(final int[] a, final int index, final int element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final int[] newArray = new int[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static long[] insert(final long[] a, final int index, final long element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final long[] newArray = new long[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static float[] insert(final float[] a, final int index, final float element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final float[] newArray = new float[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     */
    public static double[] insert(final double[] a, final int index, final double element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return Array.of(element);
        }

        final double[] newArray = new double[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    public static String[] insert(final String[] a, final int index, final String element) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return N.asArray(element);
        }

        final String[] newArray = new String[a.length + 1];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified element at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * plus the given element on the specified position. The component type of
     * the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param index
     *            the position of the new object
     * @param element
     *            the object to add
     * @return A new array containing the existing elements and the new element
     * @throws NullPointerException if the specified <code>a</code> is <code>null</code>.
     */
    public static <T> T[] insert(final T[] a, final int index, final T element) {
        N.requireNonNull(a, "a");

        final T[] newArray = N.newArray(a.getClass().getComponentType(), a.length + 1);

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        newArray[index] = element;

        if (index < a.length) {
            copy(a, index, newArray, index + 1, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static boolean[] insertAll(final boolean[] a, final int index, final boolean... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final boolean[] newArray = new boolean[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static char[] insertAll(final char[] a, final int index, final char... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final char[] newArray = new char[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static byte[] insertAll(final byte[] a, final int index, final byte... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final byte[] newArray = new byte[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static short[] insertAll(final short[] a, final int index, final short... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final short[] newArray = new short[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static int[] insertAll(final int[] a, final int index, final int... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final int[] newArray = new int[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static long[] insertAll(final long[] a, final int index, final long... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final long[] newArray = new long[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static float[] insertAll(final float[] a, final int index, final float... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final float[] newArray = new float[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     */
    @SafeVarargs
    public static double[] insertAll(final double[] a, final int index, final double... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final double[] newArray = new double[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    @SafeVarargs
    public static String[] insertAll(final String[] a, final int index, final String... b) {
        if (N.isNullOrEmpty(a) && index == 0) {
            return b.clone();
        }

        final String[] newArray = new String[a.length + b.length];

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Inserts the specified elements at the specified position in the array.
     * Shifts the element currently at that position (if any) and any subsequent
     * elements to the right (adds one to their indices).
     * </p>
     *
     * @param a
     *            the first array whose elements are added to the new array.
     * @param index
     *            the position of the new elements start from
     * @param b
     *            the second array whose elements are added to the new array.
     * @return A new array containing the elements from a and b
     * @throws NullPointerException if the specified <code>a</code> is <code>null</code>.
     */
    @SafeVarargs
    public static <T> T[] insertAll(final T[] a, final int index, final T... b) {
        N.requireNonNull(a, "a");

        final T[] newArray = (T[]) Array.newInstance(a.getClass().getComponentType(), a.length + b.length);

        if (index > 0) {
            copy(a, 0, newArray, 0, index);
        }

        copy(b, 0, newArray, index, b.length);

        if (index < a.length) {
            copy(a, index, newArray, index + b.length, a.length - index);
        }

        return newArray;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static boolean[] delete(final boolean[] a, final int index) {
        final boolean[] result = new boolean[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static char[] delete(final char[] a, final int index) {
        final char[] result = new char[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static byte[] delete(final byte[] a, final int index) {
        final byte[] result = new byte[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static short[] delete(final short[] a, final int index) {
        final short[] result = new short[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static int[] delete(final int[] a, final int index) {
        final int[] result = new int[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static long[] delete(final long[] a, final int index) {
        final long[] result = new long[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static float[] delete(final float[] a, final int index) {
        final float[] result = new float[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static double[] delete(final double[] a, final int index) {
        final double[] result = new double[a.length - 1];

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the element at the specified position from the specified array.
     * All subsequent elements are shifted to the left (subtracts one from their
     * indices).
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the element on the specified position. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param index
     *            the position of the element to be removed
     * @return A new array containing the existing elements except the element
     *         at the specified position.
     */
    public static <T> T[] delete(final T[] a, final int index) {
        final T[] result = N.newArray(a.getClass().getComponentType(), a.length - 1);

        if (index > 0) {
            copy(a, 0, result, 0, index);
        }

        if (index + 1 < a.length) {
            copy(a, index + 1, result, index, a.length - index - 1);
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <p>
     * If the input array is {@code null}, an IndexOutOfBoundsException will be
     * thrown, because in that case no valid index can be specified.
     * </p>
     *
     * <pre>
     * N.deleteAll([true, false, true], 0, 2) = [false]
     * N.removeAll([true, false, true], 1, 2) = [true]
     * </pre>
     *
     * @param a
     *            the array to remove the element from, may not be {@code null}
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     */
    @SafeVarargs
    public static boolean[] deleteAll(final boolean[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final boolean[] result = new boolean[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     */
    @SafeVarargs
    public static char[] deleteAll(final char[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final char[] result = new char[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     *
     */
    @SafeVarargs
    public static byte[] deleteAll(final byte[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final byte[] result = new byte[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     *
     */
    @SafeVarargs
    public static short[] deleteAll(final short[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final short[] result = new short[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     * @throws IndexOutOfBoundsException
     *             if any index is out of range (index &lt; 0 || index &gt;=
     *             array.length), or if the array is {@code null}.
     *
     */
    @SafeVarargs
    public static int[] deleteAll(final int[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final int[] result = new int[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     *            the array to remove the element from, may not be {@code null}
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     */
    @SafeVarargs
    public static long[] deleteAll(final long[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final long[] result = new long[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     *
     */
    @SafeVarargs
    public static float[] deleteAll(final float[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final float[] result = new float[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     * <pre>
     * N.deleteAll([1], 0)             = []
     * N.deleteAll([2, 6], 0)          = [6]
     * N.deleteAll([2, 6], 0, 1)       = []
     * N.deleteAll([2, 6, 3], 1, 2)    = [2]
     * N.deleteAll([2, 6, 3], 0, 2)    = [6]
     * N.deleteAll([2, 6, 3], 0, 1, 2) = []
     * </pre>
     *
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     *
     */
    @SafeVarargs
    public static double[] deleteAll(final double[] a, int... indices) {
        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final double[] result = new double[a.length - diff];
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     * <p>
     * Removes the elements at the specified positions from the specified array.
     * All remaining elements are shifted to the left.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except those at the specified positions. The component type of the
     * returned array is always the same as that of the input array.
     * </p>
     *
     *
     * <pre>
     * N.deleteAll(["a", "b", "c"], 0, 2) = ["b"]
     * N.deleteAll(["a", "b", "c"], 1, 2) = ["a"]
     * </pre>
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @param indices
     *            the positions of the elements to be removed
     * @return A new array containing the existing elements except those at the
     *         specified positions.
     * @throws NullPointerException if the specified <code>a</code> is <code>null</code>.
     */
    @SafeVarargs
    public static <T> T[] deleteAll(final T[] a, int... indices) {
        N.requireNonNull(a, "a");

        if (N.isNullOrEmpty(indices)) {
            return a.clone();
        } else if (indices.length == 1) {
            return delete(a, indices[0]);
        }

        indices = indices.clone();
        N.sort(indices);
        final int lastIndex = indices[indices.length - 1];

        if (indices[0] < 0 || lastIndex >= a.length) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + lastIndex);
        }

        int diff = 1;
        for (int i = 1, len = indices.length; i < len; i++) {
            if (indices[i] == indices[i - 1]) {
                continue;
            }

            diff++;
        }

        final T[] result = N.newArray(a.getClass().getComponentType(), a.length - diff);
        int dest = 0;
        int len = 0;
        for (int i = 0, preIndex = -1; i < indices.length; preIndex = indices[i], i++) {
            if (indices[i] - preIndex > 1) {
                len = indices[i] - preIndex - 1;
                copy(a, preIndex + 1, result, dest, len);
                dest += len;
            }
        }

        if (lastIndex < a.length - 1) {
            len = a.length - lastIndex - 1;
            copy(a, lastIndex + 1, result, dest, len);
            dest += len;
        }

        return result;
    }

    /**
     *
     * Removes the elements at the specified positions from the specified List.
     *
     * @param list
     * @param indices
     * @return
     */
    @SafeVarargs
    public static boolean deleteAll(final List<?> list, int... indices) {
        N.requireNonNull(list);

        if (N.isNullOrEmpty(indices)) {
            return false;
        } else if (indices.length == 1) {
            list.remove(indices[0]);
            return true;
        }

        indices = indices.clone();
        sort(indices);

        if (indices[0] < 0 || indices[indices.length - 1] >= list.size()) {
            throw new IndexOutOfBoundsException("The specified indices are from: " + indices[0] + " to: " + indices[indices.length - 1]);
        }

        final Iterator<?> iterator = list.iterator();

        int idx = -1;
        for (int i = 0, len = indices.length; i < len; i++) {
            if (i > 0 && indices[i] == indices[i - 1]) {
                continue;
            }

            while (idx < indices[i]) {
                idx++;
                iterator.next();
            }

            iterator.remove();
        }

        return true;
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static boolean[] remove(final boolean[] a, final boolean element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static char[] remove(final char[] a, final char element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static byte[] remove(final byte[] a, final byte element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static short[] remove(final short[] a, final short element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static int[] remove(final int[] a, final int element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static long[] remove(final long[] a, final long element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static float[] remove(final float[] a, final float element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static double[] remove(final double[] a, final double element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     * </p>
     *
     * <p>
     * This method returns a new array with the same elements of the input array
     * except the first occurrence of the specified element. The component type
     * of the returned array is always the same as that of the input array.
     * </p>
     *
     * @param a
     * @param element
     *            the element to be removed
     * @return A new array containing the existing elements except the first
     *         occurrence of the specified element.
     */
    public static <T> T[] remove(final T[] a, final Object element) {
        if (N.isNullOrEmpty(a)) {
            return a;
        }

        int index = indexOf(a, 0, element);

        return index == INDEX_NOT_FOUND ? a.clone() : delete(a, index);
    }

    /**
     * <p>
     * Removes the first occurrence of the specified element from the specified
     * collection. If the collection doesn't contains such an element, no
     * elements are removed from the collection.
     * </p>
     *
     * @param c
     * @param element the element to be removed
     * @return <tt>true</tt> if this collection changed as a result of the call
     */
    static boolean remove(final Collection<?> c, final Object element) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return c.remove(element);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static boolean[] removeAllOccurrences(final boolean[] a, final boolean element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        final boolean[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == element) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static char[] removeAllOccurrences(final char[] a, final char element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        final char[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == element) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static byte[] removeAllOccurrences(final byte[] a, final byte element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        final byte[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == element) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static short[] removeAllOccurrences(final short[] a, final short element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        final short[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == element) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static int[] removeAllOccurrences(final int[] a, final int element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        }

        final int[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == element) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static long[] removeAllOccurrences(final long[] a, final long element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        }

        final long[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == element) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static float[] removeAllOccurrences(final float[] a, final float element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        final float[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (equals(a[i], element)) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static double[] removeAllOccurrences(final double[] a, final double element) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        final double[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (equals(a[i], element)) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    /**
     * Removes all the occurrences of the specified element from the specified
     * array. All subsequent elements are shifted to the left (subtracts one
     * from their indices). If the array doesn't contains such an element, no
     * elements are removed from the array.
     *
     * @param a
     * @param element
     * @return A new array containing the existing elements except the
     *         occurrences of the specified element.
     */
    public static <T> T[] removeAllOccurrences(final T[] a, final Object element) {
        if (N.isNullOrEmpty(a)) {
            return a;
        }

        final T[] copy = a.clone();
        int idx = 0;

        for (int i = 0, len = a.length; i < len; i++) {
            if (equals(a[i], element)) {
                continue;
            }

            copy[idx++] = a[i];
        }

        return idx == copy.length ? copy : N.copyOfRange(copy, 0, idx);
    }

    public static boolean removeAllOccurrences(Collection<?> c, final Object element) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return c.removeAll(N.asSet(element));
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static boolean[] removeAll(final boolean[] a, final boolean... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final BooleanList list = BooleanList.of(a.clone());
        list.removeAll(BooleanList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static char[] removeAll(final char[] a, final char... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final CharList list = CharList.of(a.clone());
        list.removeAll(CharList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static byte[] removeAll(final byte[] a, final byte... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final ByteList list = ByteList.of(a.clone());
        list.removeAll(ByteList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static short[] removeAll(final short[] a, final short... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final ShortList list = ShortList.of(a.clone());
        list.removeAll(ShortList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static int[] removeAll(final int[] a, final int... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final IntList list = IntList.of(a.clone());
        list.removeAll(IntList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static long[] removeAll(final long[] a, final long... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final LongList list = LongList.of(a.clone());
        list.removeAll(LongList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static float[] removeAll(final float[] a, final float... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final FloatList list = FloatList.of(a.clone());
        list.removeAll(FloatList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static double[] removeAll(final double[] a, final double... elements) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final DoubleList list = DoubleList.of(a.clone());
        list.removeAll(DoubleList.of(elements));
        return list.trimToSize().array();
    }

    /**
     * Returns a new array with removes all the occurrences of specified elements from <code>a</code>
     * 
     * @param a
     * @param elements
     * @return
     * @see Collection#removeAll(Collection)
     */
    @SafeVarargs
    public static <T> T[] removeAll(final T[] a, final Object... elements) {
        if (N.isNullOrEmpty(a)) {
            return a;
        } else if (N.isNullOrEmpty(elements)) {
            return a.clone();
        } else if (elements.length == 1) {
            return removeAllOccurrences(a, elements[0]);
        }

        final Set<Object> set = N.asSet(elements);
        final List<T> result = new ArrayList<>();

        for (T e : a) {
            if (!set.contains(e)) {
                result.add(e);
            }
        }

        return result.toArray((T[]) N.newArray(a.getClass().getComponentType(), result.size()));
    }

    @SafeVarargs
    public static boolean removeAll(final Collection<?> c, final Object... elements) {
        if (N.isNullOrEmpty(c) || N.isNullOrEmpty(elements)) {
            return false;
        } else {
            return c.removeAll(N.asSet(elements));
        }
    }

    public static boolean[] removeDuplicates(final boolean[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static boolean[] removeDuplicates(final boolean[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static boolean[] removeDuplicates(final boolean[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_BOOLEAN_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        final Boolean[] b = new Boolean[2];

        for (int i = from; i < to; i++) {
            if (b[0] == null) {
                b[0] = a[i];
            } else if (b[0].booleanValue() != a[i]) {
                b[1] = a[i];
                break;
            }
        }

        return b[1] == null ? new boolean[] { b[0].booleanValue() } : new boolean[] { b[0].booleanValue(), b[1].booleanValue() };
    }

    public static char[] removeDuplicates(final char[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static char[] removeDuplicates(final char[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static char[] removeDuplicates(final char[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_CHAR_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final char[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (b[i] == b[i - 1]) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {
            final Set<Character> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final char[] result = new char[set.size()];
                int i = 0;

                for (char e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    public static byte[] removeDuplicates(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static byte[] removeDuplicates(final byte[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static byte[] removeDuplicates(final byte[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_BYTE_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final byte[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (b[i] == b[i - 1]) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {
            final Set<Byte> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final byte[] result = new byte[set.size()];
                int i = 0;

                for (byte e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    public static short[] removeDuplicates(final short[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static short[] removeDuplicates(final short[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static short[] removeDuplicates(final short[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_SHORT_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final short[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (b[i] == b[i - 1]) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {
            final Set<Short> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final short[] result = new short[set.size()];
                int i = 0;

                for (short e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    public static int[] removeDuplicates(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static int[] removeDuplicates(final int[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static int[] removeDuplicates(final int[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_INT_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final int[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (b[i] == b[i - 1]) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {

            final Set<Integer> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final int[] result = new int[set.size()];
                int i = 0;

                for (int e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    public static long[] removeDuplicates(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static long[] removeDuplicates(final long[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static long[] removeDuplicates(final long[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_LONG_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final long[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (b[i] == b[i - 1]) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {
            final Set<Long> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final long[] result = new long[set.size()];
                int i = 0;

                for (long e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    public static float[] removeDuplicates(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static float[] removeDuplicates(final float[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static float[] removeDuplicates(final float[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_FLOAT_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final float[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (N.equals(b[i], b[i - 1])) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {

            final Set<Float> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final float[] result = new float[set.size()];
                int i = 0;

                for (float e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    public static double[] removeDuplicates(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        return removeDuplicates(a, false);
    }

    public static double[] removeDuplicates(final double[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static double[] removeDuplicates(final double[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return N.EMPTY_DOUBLE_ARRAY;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final double[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (N.equals(b[i], b[i - 1])) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {
            final Set<Double> set = new LinkedHashSet<>(N.initHashCapacity(a.length));

            for (int i = from; i < to; i++) {
                set.add(a[i]);
            }

            if (set.size() == to - from) {
                return (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            } else {
                final double[] result = new double[set.size()];
                int i = 0;

                for (double e : set) {
                    result[i++] = e;
                }

                return result;
            }
        }
    }

    /**
     * <p>
     * Removes all duplicates elements
     * </p>
     *
     * <pre>
     * N.removeElements(["a", "b", "a"]) = ["a", "b"]
     * </pre>
     *
     * @param <T>
     *            the component type of the array
     * @param a
     * @return A new array containing the existing elements except the duplicates
     *
     * @throws NullPointerException if the specified array <code>a</code> is null.
     */
    public static <T> T[] removeDuplicates(final T[] a) {
        if (N.isNullOrEmpty(a)) {
            return a;
        }

        return removeDuplicates(a, false);
    }

    public static <T> T[] removeDuplicates(final T[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return a;
        }

        return removeDuplicates(a, 0, a.length, isSorted);
    }

    public static <T> T[] removeDuplicates(final T[] a, final int from, final int to, final boolean isSorted) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a) && from == 0 && to == 0) {
            return a;
        } else if (to - from <= 1) {
            return N.copyOfRange(a, from, to);
        }

        if (isSorted) {
            final T[] b = (from == 0 && to == a.length) ? a.clone() : N.copyOfRange(a, from, to);
            int idx = 1;

            for (int i = 1, len = b.length; i < len; i++) {
                if (N.equals(b[i], b[i - 1])) {
                    continue;
                }

                b[idx++] = b[i];
            }

            return idx == b.length ? b : N.copyOfRange(b, 0, idx);
        } else {
            return distinct(a, from, to);
        }
    }

    /**
     *
     * @param c
     * @return <code>true</code> if there is one or more duplicated elements are removed. otherwise <code>false</code> is returned.
     */
    public static boolean removeDuplicates(final Collection<?> c) {
        if (N.isNullOrEmpty(c) || c.size() == 1) {
            return false;
        }

        return removeDuplicates(c, false);
    }

    /**
     *
     * @param c
     * @param isSorted
     * @return <code>true</code> if there is one or more duplicated elements are removed. otherwise <code>false</code> is returned.
     */
    @SuppressWarnings("rawtypes")
    public static boolean removeDuplicates(final Collection<?> c, final boolean isSorted) {
        if (N.isNullOrEmpty(c) || c.size() == 1) {
            return false;
        }

        if (isSorted) {
            boolean hasDuplicates = false;
            final Iterator<?> it = c.iterator();
            Object pre = it.next();
            Object next = null;
            while (it.hasNext()) {
                next = it.next();
                if (N.equals(next, pre)) {
                    it.remove();
                    hasDuplicates = true;
                } else {
                    pre = next;
                }
            }

            return hasDuplicates;
        } else {
            List<?> list = distinct(c);

            final boolean hasDuplicates = list.size() != c.size();

            if (hasDuplicates) {
                c.clear();
                c.addAll((List) list);
            }

            return hasDuplicates;
        }
    }

    // Primitive/Object array converters
    // ----------------------------------------------------------------------

    public static boolean hasDuplicates(final char[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final char[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final char[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return a[fromIndex] == a[fromIndex + 1];
        } else if (toIndex - fromIndex == 3) {
            return a[fromIndex] == a[fromIndex + 1] || a[fromIndex] == a[fromIndex + 2] || a[fromIndex + 1] == a[fromIndex + 2];
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (a[i] == a[i - 1]) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Character> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final byte[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final byte[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final byte[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return a[fromIndex] == a[fromIndex + 1];
        } else if (toIndex - fromIndex == 3) {
            return a[fromIndex] == a[fromIndex + 1] || a[fromIndex] == a[fromIndex + 2] || a[fromIndex + 1] == a[fromIndex + 2];
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (a[i] == a[i - 1]) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Byte> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final short[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final short[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final short[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return a[fromIndex] == a[fromIndex + 1];
        } else if (toIndex - fromIndex == 3) {
            return a[fromIndex] == a[fromIndex + 1] || a[fromIndex] == a[fromIndex + 2] || a[fromIndex + 1] == a[fromIndex + 2];
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (a[i] == a[i - 1]) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Short> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final int[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final int[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final int[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return a[fromIndex] == a[fromIndex + 1];
        } else if (toIndex - fromIndex == 3) {
            return a[fromIndex] == a[fromIndex + 1] || a[fromIndex] == a[fromIndex + 2] || a[fromIndex + 1] == a[fromIndex + 2];
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (a[i] == a[i - 1]) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Integer> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final long[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final long[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final long[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return a[fromIndex] == a[fromIndex + 1];
        } else if (toIndex - fromIndex == 3) {
            return a[fromIndex] == a[fromIndex + 1] || a[fromIndex] == a[fromIndex + 2] || a[fromIndex + 1] == a[fromIndex + 2];
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (a[i] == a[i - 1]) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Long> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final float[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final float[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final float[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return N.equals(a[fromIndex], a[fromIndex + 1]);
        } else if (toIndex - fromIndex == 3) {
            return N.equals(a[fromIndex], a[fromIndex + 1]) || N.equals(a[fromIndex], a[fromIndex + 2]) || N.equals(a[fromIndex + 1], a[fromIndex + 2]);
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (N.equals(a[i], a[i - 1])) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Float> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final double[] a) {
        return hasDuplicates(a, false);
    }

    public static boolean hasDuplicates(final double[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static boolean hasDuplicates(final double[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return N.equals(a[fromIndex], a[fromIndex + 1]);
        } else if (toIndex - fromIndex == 3) {
            return N.equals(a[fromIndex], a[fromIndex + 1]) || N.equals(a[fromIndex], a[fromIndex + 2]) || N.equals(a[fromIndex + 1], a[fromIndex + 2]);
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (N.equals(a[i], a[i - 1])) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Double> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(a[i]) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static <T> boolean hasDuplicates(final T[] a) {
        return hasDuplicates(a, false);
    }

    public static <T> boolean hasDuplicates(final T[] a, final boolean isSorted) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return hasDuplicates(a, 0, a.length, isSorted);
    }

    static <T> boolean hasDuplicates(final T[] a, final int fromIndex, final int toIndex, final boolean isSorted) {
        N.checkFromToIndex(fromIndex, toIndex, a.length);

        if (N.isNullOrEmpty(a) || toIndex - fromIndex < 2) {
            return false;
        } else if (toIndex - fromIndex == 2) {
            return N.equals(a[fromIndex], a[fromIndex + 1]);
        } else if (toIndex - fromIndex == 3) {
            return N.equals(a[fromIndex], a[fromIndex + 1]) || N.equals(a[fromIndex], a[fromIndex + 2]) || N.equals(a[fromIndex + 1], a[fromIndex + 2]);
        }

        if (isSorted) {
            for (int i = fromIndex + 1; i < toIndex; i++) {
                if (N.equals(a[i], a[i - 1])) {
                    return true;
                }
            }

            return false;
        } else {
            final Set<Object> set = new HashSet<>(min(9, initHashCapacity(toIndex - fromIndex)));

            for (int i = fromIndex; i < toIndex; i++) {
                if (set.add(hashKey(a[i])) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    public static boolean hasDuplicates(final Collection<?> c) {
        return hasDuplicates(c, false);
    }

    public static boolean hasDuplicates(final Collection<?> c, final boolean isSorted) {
        if (N.isNullOrEmpty(c) || c.size() == 1) {
            return false;
        }

        if (isSorted) {
            final Iterator<?> it = c.iterator();
            Object pre = it.next();
            Object next = null;
            while (it.hasNext()) {
                next = it.next();

                if (N.equals(next, pre)) {
                    return true;
                }

                pre = next;
            }

            return false;
        } else {
            final Set<Object> set = new HashSet<>(min(9, initHashCapacity(c.size())));

            for (Object e : c) {
                if (set.add(hashKey(e)) == false) {
                    return true;
                }
            }

            return false;
        }
    }

    /**
     *
     * @param a
     * @return a long number
     */
    @SafeVarargs
    public static int sum(final char... a) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return sum(a, 0, a.length);
    }

    public static int sum(final char[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0;
        }

        int sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    /**
     *
     * @param a
     * @return a long number
     */
    @SafeVarargs
    public static int sum(final byte... a) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return sum(a, 0, a.length);
    }

    public static int sum(final byte[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0;
        }

        int sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    /**
     *
     * @param a
     * @return a long number
     */
    @SafeVarargs
    public static int sum(final short... a) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return sum(a, 0, a.length);
    }

    public static int sum(final short[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0;
        }

        int sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    /**
     *
     * @param a
     * @return a long number
     */
    @SafeVarargs
    public static int sum(final int... a) {
        if (N.isNullOrEmpty(a)) {
            return 0;
        }

        return sum(a, 0, a.length);
    }

    public static int sum(final int[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0;
        }

        int sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    /**
     *
     * @param a
     * @return a long number
     */
    @SafeVarargs
    public static long sum(final long... a) {
        if (N.isNullOrEmpty(a)) {
            return 0L;
        }

        return sum(a, 0, a.length);
    }

    public static long sum(final long[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0L;
        }

        long sum = 0;

        for (int i = from; i < to; i++) {
            sum += a[i];
        }

        return sum;
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static float sum(final float... a) {
        if (N.isNullOrEmpty(a)) {
            return 0f;
        }

        return sum(a, 0, a.length);
    }

    public static float sum(final float[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0f;
        }

        //        double sum = 0;
        //
        //        for (int i = from; i < to; i++) {
        //            sum += a[i];
        //        }
        //
        //        return sum;

        return (float) FloatStream.of(a, from, to).sum();
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double sum(final double... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return sum(a, 0, a.length);
    }

    public static double sum(final double[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        //        double sum = 0;
        //
        //        for (int i = from; i < to; i++) {
        //            sum += a[i];
        //        }
        //
        //        return sum;

        return DoubleStream.of(a, from, to).sum();
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double average(final char... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final char[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);
    }

    /**
    *
    * @param a
    * @return a double number
    */
    @SafeVarargs
    public static double average(final byte... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final byte[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double average(final short... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final short[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double average(final int... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final int[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double average(final long... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final long[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double average(final float... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final float[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        // return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);

        return FloatStream.of(a, from, to).average().orElse(0);
    }

    /**
     *
     * @param a
     * @return a double number
     */
    @SafeVarargs
    public static double average(final double... a) {
        if (N.isNullOrEmpty(a)) {
            return 0d;
        }

        return average(a, 0, a.length);
    }

    public static double average(final double[] a, final int from, final int to) {
        checkFromToIndex(from, to, a == null ? 0 : a.length);

        if (N.isNullOrEmpty(a)) {
            if (to > 0) {
                throw new IndexOutOfBoundsException();
            }

            return 0d;
        }

        // return from == to ? 0d : ((double) sum(a, from, to)) / (to - from);

        return DoubleStream.of(a, from, to).average().orElse(0);
    }

    /**
     * <p>
     * Gets the minimum of two <code>char</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static char min(final char a, final char b) {
        return (a <= b) ? a : b;
    }

    /**
     * <p>
     * Gets the minimum of two <code>byte</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static byte min(final byte a, final byte b) {
        return (a <= b) ? a : b;
    }

    /**
     * <p>
     * Gets the minimum of two <code>short</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static short min(final short a, final short b) {
        return (a <= b) ? a : b;
    }

    /**
     * <p>
     * Gets the minimum of two <code>int</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static int min(final int a, final int b) {
        return (a <= b) ? a : b;
    }

    /**
     * <p>
     * Gets the minimum of two <code>long</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static long min(final long a, final long b) {
        return (a <= b) ? a : b;
    }

    /**
     * <p>
     * Gets the minimum of two <code>float</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static float min(final float a, final float b) {
        return Math.min(a, b);
    }

    /**
     * <p>
     * Gets the minimum of two <code>double</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the smallest of the values
     */
    public static double min(final double a, final double b) {
        return Math.min(a, b);
    }

    public static <T extends Comparable<? super T>> T min(final T a, final T b) {
        return (T) min(a, b, NULL_MAX_COMPARATOR);
    }

    public static <T> T min(final T a, final T b, final Comparator<? super T> cmp) {
        return (cmp == null ? NULL_MAX_COMPARATOR : cmp).compare(a, b) <= 0 ? a : b;
    }

    /**
     * <p>
     * Gets the minimum of three <code>char</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static char min(final char a, final char b, final char c) {
        final char m = (a <= b) ? a : b;

        return (m <= c) ? m : c;
    }

    /**
     * <p>
     * Gets the minimum of three <code>byte</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static byte min(final byte a, final byte b, final byte c) {
        final byte m = (a <= b) ? a : b;

        return (m <= c) ? m : c;
    }

    /**
     * <p>
     * Gets the minimum of three <code>short</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static short min(final short a, final short b, final short c) {
        final short m = (a <= b) ? a : b;

        return (m <= c) ? m : c;
    }

    /**
     * <p>
     * Gets the minimum of three <code>int</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static int min(final int a, final int b, final int c) {
        final int m = (a <= b) ? a : b;

        return (m <= c) ? m : c;
    }

    /**
     * <p>
     * Gets the minimum of three <code>long</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static long min(final long a, final long b, final long c) {
        final long m = (a <= b) ? a : b;

        return (m <= c) ? m : c;
    }

    /**
     * <p>
     * Gets the minimum of three <code>float</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static float min(final float a, final float b, final float c) {
        return Math.min(Math.min(a, b), c);
    }

    /**
     * <p>
     * Gets the minimum of three <code>double</code> values.
     * </p>
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static double min(final double a, final double b, final double c) {
        return Math.min(Math.min(a, b), c);
    }

    public static <T extends Comparable<? super T>> T min(final T a, final T b, final T c) {
        return (T) min(a, b, c, NULL_MAX_COMPARATOR);
    }

    public static <T> T min(final T a, final T b, final T c, final Comparator<? super T> cmp) {
        return min(min(a, b, cmp), c, cmp);
    }

    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     */
    @SafeVarargs
    public static char min(final char... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        if (N.isNullOrEmpty(a)) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return min(a, 0, a.length);
    }

    public static char min(final char[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        char min = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] < min) {
                min = a[i];
            }
        }

        return min;
    }

    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     */
    @SafeVarargs
    public static byte min(final byte... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static byte min(final byte[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        byte min = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] < min) {
                min = a[i];
            }
        }

        return min;
    }

    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     */
    @SafeVarargs
    public static short min(final short... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static short min(final short[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        short min = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] < min) {
                min = a[i];
            }
        }

        return min;
    }

    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     */
    @SafeVarargs
    public static int min(final int... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static int min(final int[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        int min = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] < min) {
                min = a[i];
            }
        }

        return min;
    }

    // Min in array
    // --------------------------------------------------------------------
    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     */
    @SafeVarargs
    public static long min(final long... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static long min(final long[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        long min = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] < min) {
                min = a[i];
            }
        }

        return min;
    }

    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     * @see IEEE754rUtil#min(float[]) IEEE754rUtils for a version of this method
     *      that handles NaN differently
     */
    @SafeVarargs
    public static float min(final float... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static float min(final float[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        float min = a[from];
        for (int i = from + 1; i < to; i++) {
            min = Math.min(min, a[i]);

            if (Float.isNaN(min)) {
                return min;
            }
        }

        return min;
    }

    /**
     * <p>
     * Returns the minimum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     * @see IEEE754rUtil#min(double[]) IEEE754rUtils for a version of this
     *      method that handles NaN differently
     */
    @SafeVarargs
    public static double min(final double... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static double min(final double[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns min
        double min = a[from];
        for (int i = from + 1; i < to; i++) {
            min = Math.min(min, a[i]);

            if (Double.isNaN(min)) {
                return min;
            }
        }

        return min;
    }

    /**
     * Returns the minimum element in the array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the minimum value in the array
     */
    public static <T extends Comparable<? super T>> T min(final T[] a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length);
    }

    public static <T extends Comparable<? super T>> T min(final T[] a, final int from, final int to) {
        return (T) min(a, from, to, NULL_MAX_COMPARATOR);
    }

    /**
     * Returns the minimum element in the array.
     *
     * @param a
     *            an array, must not be null or empty
     * @param cmp
     * @return the minimum value in the array
     */
    public static <T> T min(final T[] a, final Comparator<? super T> cmp) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return min(a, 0, a.length, cmp);
    }

    public static <T> T min(final T[] a, final int from, final int to, Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        cmp = cmp == null ? NULL_MAX_COMPARATOR : cmp;

        T candidate = a[from];
        for (int i = from + 1; i < to; i++) {
            if (cmp.compare(a[i], candidate) < 0) {
                candidate = a[i];
            }

            if (candidate == null && cmp == NULL_MIN_COMPARATOR) {
                return null;
            }
        }

        return candidate;
    }

    public static <T extends Comparable<? super T>> T min(final Collection<? extends T> c) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return min(c, 0, c.size());
    }

    public static <T extends Comparable<? super T>> T min(final Collection<? extends T> c, final int from, final int to) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return (T) min(c, from, to, NULL_MAX_COMPARATOR);
    }

    public static <T> T min(final Collection<? extends T> c, Comparator<? super T> cmp) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return min(c, 0, c.size(), cmp);
    }

    /**
     * Returns the minimum element in the collection.
     * 
     * @param c
     * @param from
     * @param to
     * @param cmp
     * @return the minimum value in the Collection
     */
    public static <T> T min(final Collection<? extends T> c, final int from, final int to, Comparator<? super T> cmp) {
        checkFromToIndex(from, to, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || to - from < 1 || from >= c.size()) {
            throw new IllegalArgumentException("The size of collection can't be null or empty");
        }

        cmp = cmp == null ? NULL_MAX_COMPARATOR : cmp;

        T candidate = null;
        T e = null;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;
            candidate = list.get(from);

            for (int i = from + 1; i < to; i++) {
                e = list.get(i);

                if (cmp.compare(e, candidate) < 0) {
                    candidate = e;
                }

                if (candidate == null && cmp == NULL_MIN_COMPARATOR) {
                    return null;
                }
            }
        } else {
            final Iterator<? extends T> it = c.iterator();

            for (int i = 0; i < to; i++) {
                if (i < from) {
                    it.next();
                    continue;
                } else if (i == from) {
                    candidate = it.next();
                } else {
                    e = it.next();

                    if (cmp.compare(e, candidate) < 0) {
                        candidate = e;
                    }

                    if (candidate == null && cmp == NULL_MIN_COMPARATOR) {
                        return null;
                    }
                }
            }
        }

        return candidate;
    }

    /**
     * <p>
     * Gets the maximum of two <code>char</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static char max(final char a, final char b) {
        return (a >= b) ? a : b;
    }

    /**
     * <p>
     * Gets the maximum of two <code>byte</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static byte max(final byte a, final byte b) {
        return (a >= b) ? a : b;
    }

    /**
     * <p>
     * Gets the maximum of two <code>short</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static short max(final short a, final short b) {
        return (a >= b) ? a : b;
    }

    /**
     * <p>
     * Gets the maximum of two <code>int</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static int max(final int a, final int b) {
        return (a >= b) ? a : b;
    }

    /**
     * <p>
     * Gets the maximum of two <code>long</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static long max(final long a, final long b) {
        return (a >= b) ? a : b;
    }

    /**
     * <p>
     * Gets the maximum of two <code>float</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static float max(final float a, final float b) {
        return Math.max(a, b);
    }

    /**
     * <p>
     * Gets the maximum of two <code>double</code> values.
     * </p>
     *
     * @param a
     * @param b
     * @return the largest of the values
     */
    public static double max(final double a, final double b) {
        return Math.max(a, b);
    }

    public static <T extends Comparable<? super T>> T max(final T a, final T b) {
        return (T) max(a, b, NULL_MIN_COMPARATOR);
    }

    public static <T> T max(final T a, final T b, final Comparator<? super T> cmp) {
        return (cmp == null ? NULL_MIN_COMPARATOR : cmp).compare(a, b) >= 0 ? a : b;
    }

    /**
     * Gets the maximum of three <code>char</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static char max(final char a, final char b, final char c) {
        final char m = (a >= b) ? a : b;

        return (m >= c) ? m : c;
    }

    /**
     * Gets the maximum of three <code>byte</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static byte max(final byte a, final byte b, final byte c) {
        final byte m = (a >= b) ? a : b;

        return (m >= c) ? m : c;
    }

    /**
     * Gets the maximum of three <code>short</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static short max(final short a, final short b, final short c) {
        final short m = (a >= b) ? a : b;

        return (m >= c) ? m : c;
    }

    /**
     * Gets the maximum of three <code>int</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static int max(final int a, final int b, final int c) {
        final int m = (a >= b) ? a : b;

        return (m >= c) ? m : c;
    }

    /**
     * Gets the maximum of three <code>long</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static long max(final long a, final long b, final long c) {
        final long m = (a >= b) ? a : b;

        return (m >= c) ? m : c;
    }

    /**
     * Gets the maximum of three <code>float</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static float max(final float a, final float b, final float c) {
        return Math.max(Math.max(a, b), c);
    }

    /**
     * Gets the maximum of three <code>double</code> values.
     * 
     * @param a
     * @param b
     * @param c
     * @return
     */
    public static double max(final double a, final double b, final double c) {
        return Math.max(Math.max(a, b), c);
    }

    public static <T extends Comparable<? super T>> T max(final T a, final T b, final T c) {
        return (T) max(a, b, c, NULL_MIN_COMPARATOR);
    }

    public static <T> T max(final T a, final T b, final T c, final Comparator<? super T> cmp) {
        return max(max(a, b, cmp), c, cmp);
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     */
    @SafeVarargs
    public static char max(final char... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static char max(final char[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        char max = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] > max) {
                max = a[i];
            }
        }

        return max;
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     */
    @SafeVarargs
    public static byte max(final byte... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static byte max(final byte[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        byte max = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] > max) {
                max = a[i];
            }
        }

        return max;
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     */
    @SafeVarargs
    public static short max(final short... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static short max(final short[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        short max = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] > max) {
                max = a[i];
            }
        }

        return max;
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     */
    @SafeVarargs
    public static int max(final int... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static int max(final int[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        int max = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] > max) {
                max = a[i];
            }
        }

        return max;
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     */
    @SafeVarargs
    public static long max(final long... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static long max(final long[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        long max = a[from];
        for (int i = from + 1; i < to; i++) {
            if (a[i] > max) {
                max = a[i];
            }
        }

        return max;
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     * @see IEEE754rUtil#max(float[]) IEEE754rUtils for a version of this method
     *      that handles NaN differently
     */
    @SafeVarargs
    public static float max(final float... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static float max(final float[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        float max = a[from];
        for (int i = from + 1; i < to; i++) {
            max = Math.max(max, a[i]);

            if (Float.isNaN(max)) {
                return max;
            }
        }

        return max;
    }

    /**
     * <p>
     * Returns the maximum value in an array.
     * </p>
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     * @see IEEE754rUtil#max(double[]) IEEE754rUtils for a version of this
     *      method that handles NaN differently
     */
    @SafeVarargs
    public static double max(final double... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static double max(final double[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        // Finds and returns max
        double max = a[from];
        for (int i = from + 1; i < to; i++) {
            max = Math.max(max, a[i]);

            if (Double.isNaN(max)) {
                return max;
            }
        }

        return max;
    }

    /**
     * Returns the maximum element in the array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the maximum value in the array
     * @throws IllegalArgumentException
     *             if <code>a</code> is <code>null</code> or empty.
     */
    public static <T extends Comparable<? super T>> T max(final T[] a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length);
    }

    public static <T extends Comparable<? super T>> T max(final T[] a, final int from, final int to) {
        return (T) max(a, from, to, NULL_MIN_COMPARATOR);
    }

    /**
     * Returns the maximum element in the array.
     *
     * @param a
     *            an array, must not be null or empty
     * @param cmp
     * @return the maximum value in the array
     */
    public static <T> T max(final T[] a, final Comparator<? super T> cmp) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return max(a, 0, a.length, cmp);
    }

    public static <T> T max(final T[] a, final int from, final int to, Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        cmp = cmp == null ? NULL_MIN_COMPARATOR : cmp;

        T candidate = a[from];
        for (int i = from + 1; i < to; i++) {
            if (cmp.compare(a[i], candidate) > 0) {
                candidate = a[i];
            }

            if (candidate == null && cmp == NULL_MAX_COMPARATOR) {
                return null;
            }
        }

        return candidate;
    }

    public static <T extends Comparable<? super T>> T max(final Collection<? extends T> c) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return max(c, 0, c.size());
    }

    public static <T extends Comparable<? super T>> T max(final Collection<? extends T> c, final int from, final int to) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return (T) max(c, from, to, NULL_MIN_COMPARATOR);
    }

    public static <T> T max(final Collection<? extends T> c, Comparator<? super T> cmp) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return max(c, 0, c.size(), cmp);
    }

    /**
     * Returns the maximum element in the collection.
     * 
     * @param c
     * @param from
     * @param to
     * @param cmp
     * @return the maximum value in the Collection
     */
    public static <T> T max(final Collection<? extends T> c, final int from, final int to, Comparator<? super T> cmp) {
        checkFromToIndex(from, to, c == null ? 0 : c.size());

        if (N.isNullOrEmpty(c) || to - from < 1 || from >= c.size()) {
            throw new IllegalArgumentException("The size of collection can't be null or empty");
        }

        cmp = cmp == null ? NULL_MIN_COMPARATOR : cmp;

        T candidate = null;
        T e = null;

        if (c instanceof List && c instanceof RandomAccess) {
            final List<T> list = (List<T>) c;
            candidate = list.get(from);

            for (int i = from + 1; i < to; i++) {
                e = list.get(i);

                if (cmp.compare(e, candidate) > 0) {
                    candidate = e;
                }

                if (candidate == null && cmp == NULL_MAX_COMPARATOR) {
                    return null;
                }
            }
        } else {
            final Iterator<? extends T> it = c.iterator();

            for (int i = 0; i < to; i++) {
                if (i < from) {
                    it.next();
                    continue;
                } else if (i == from) {
                    candidate = it.next();
                } else {
                    e = it.next();

                    if (cmp.compare(e, candidate) > 0) {
                        candidate = e;
                    }
                }

                if (candidate == null && cmp == NULL_MAX_COMPARATOR) {
                    return null;
                }
            }
        }

        return candidate;
    }

    /**
     * <p>
     * Gets the median of three <code>char</code> values.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static char median(final char a, final char b, final char c) {
        if ((a >= b && a <= c) || (a >= c && a <= b)) {
            return a;
        } else if ((b >= a && b <= c) || (b >= c && b <= a)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * <p>
     * Gets the median of three <code>byte</code> values.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static byte median(final byte a, final byte b, final byte c) {
        if ((a >= b && a <= c) || (a >= c && a <= b)) {
            return a;
        } else if ((b >= a && b <= c) || (b >= c && b <= a)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * <p>
     * Gets the median of three <code>short</code> values.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static short median(final short a, final short b, final short c) {
        if ((a >= b && a <= c) || (a >= c && a <= b)) {
            return a;
        } else if ((b >= a && b <= c) || (b >= c && b <= a)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * <p>
     * Gets the median of three <code>int</code> values.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static int median(final int a, final int b, final int c) {
        if ((a >= b && a <= c) || (a >= c && a <= b)) {
            return a;
        } else if ((b >= a && b <= c) || (b >= c && b <= a)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * <p>
     * Gets the median of three <code>long</code> values.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static long median(final long a, final long b, final long c) {
        if ((a >= b && a <= c) || (a >= c && a <= b)) {
            return a;
        } else if ((b >= a && b <= c) || (b >= c && b <= a)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * <p>
     * Gets the median of three <code>float</code> values.
     * </p>
     *
     * <p>
     * If any value is <code>NaN</code>, <code>NaN</code> is returned. Infinity
     * is handled.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static float median(final float a, final float b, final float c) {
        int ab = Float.compare(a, b);
        int ac = Float.compare(a, c);
        int bc = 0;

        if ((ab >= 0 && ac <= 0) || (ac >= 0 && ab <= 0)) {
            return a;
        } else if ((((bc = Float.compare(b, c)) <= 0) && ab <= 0) || (bc >= 0 && ab >= 0)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * <p>
     * Gets the median of three <code>double</code> values.
     * </p>
     *
     * <p>
     * If any value is <code>NaN</code>, <code>NaN</code> is returned. Infinity
     * is handled.
     * </p>
     *
     * @param a
     *            value 1
     * @param b
     *            value 2
     * @param c
     *            value 3
     * @return the median of the values
     */
    public static double median(final double a, final double b, final double c) {
        int ab = Double.compare(a, b);
        int ac = Double.compare(a, c);
        int bc = 0;

        if ((ab >= 0 && ac <= 0) || (ac >= 0 && ab <= 0)) {
            return a;
        } else if ((((bc = Double.compare(b, c)) <= 0) && ab <= 0) || (bc >= 0 && ab >= 0)) {
            return b;
        } else {
            return c;
        }
    }

    public static <T extends Comparable<? super T>> T median(final T a, final T b, final T c) {
        return (T) median(a, b, c, NULL_MIN_COMPARATOR);
    }

    public static <T> T median(final T a, final T b, final T c, final Comparator<? super T> cmp) {
        int ab = cmp.compare(a, b);
        int ac = cmp.compare(a, c);
        int bc = 0;

        if ((ab >= 0 && ac <= 0) || (ac >= 0 && ab <= 0)) {
            return a;
        } else if ((((bc = cmp.compare(b, c)) <= 0) && ab <= 0) || (bc >= 0 && ab >= 0)) {
            return b;
        } else {
            return c;
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static char median(final char... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static char median(final char[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static byte median(final byte... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static byte median(final byte[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static short median(final short... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static short median(final short[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static int median(final int... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static int median(final int[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static long median(final long... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static long median(final long[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static float median(final float... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static float median(final float[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     *            an array, must not be null or empty
     * @return the median value in the array
     */
    @SafeVarargs
    public static double median(final double... a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static double median(final double[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        final int len = to - from;

        if (len == 1) {
            return a[from];
        } else if (len == 2) {
            return min(a[from], a[from + 1]);
        } else if (len == 3) {
            return median(a[from], a[from + 1], a[from + 2]);
        } else {
            return kthLargest(a, from, to, len / 2 + 1);
        }
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param a
     * @return the median value in the array
     */
    public static <T extends Comparable<? super T>> T median(final T[] a) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length);
    }

    public static <T extends Comparable<? super T>> T median(final T[] a, final int from, final int to) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return (T) median(a, from, to, OBJ_COMPARATOR);
    }

    public static <T> T median(final T[] a, Comparator<? super T> cmp) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return median(a, 0, a.length, cmp);
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified array.
     *
     * @param array
     * @param c
     * @return the median value in the array
     */
    public static <T> T median(final T[] a, final int from, final int to, Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, a.length);

        cmp = cmp == null ? OBJ_COMPARATOR : cmp;

        final int len = to - from;

        return kthLargest(a, from, to, len / 2 + 1, cmp);
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified collection.
     *
     * @param c
     * @return the median value in the collection
     */
    public static <T extends Comparable<? super T>> T median(final Collection<? extends T> c) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return median(c, 0, c.size());
    }

    public static <T extends Comparable<? super T>> T median(final Collection<? extends T> c, final int from, final int to) {
        return (T) median(c, from, to, OBJ_COMPARATOR);
    }

    public static <T> T median(final Collection<? extends T> c, Comparator<? super T> cmp) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return median(c, 0, c.size(), cmp);
    }

    /**
     * Returns the <code>length / 2 + 1</code> largest value in the specified collection.
     *
     * @param c
     * @param cmp
     * @return the median value in the Collection
     */
    public static <T> T median(final Collection<? extends T> c, final int from, final int to, Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(c) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        checkFromToIndex(from, to, c.size());

        cmp = cmp == null ? OBJ_COMPARATOR : cmp;

        final int len = to - from;

        return kthLargest(c, from, to, len / 2 + 1, cmp);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static char kthLargest(final char[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static char kthLargest(final char[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static byte kthLargest(final byte[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static byte kthLargest(final byte[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static short kthLargest(final short[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static short kthLargest(final short[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static int kthLargest(final int[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static int kthLargest(final int[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static long kthLargest(final long[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static long kthLargest(final long[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static float kthLargest(final float[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static float kthLargest(final float[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static double kthLargest(final double[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static double kthLargest(final double[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static <T extends Comparable<T>> T kthLargest(final T[] a, final int k) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static <T extends Comparable<T>> T kthLargest(final T[] a, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k);
    }

    /**
     * 
     * @param a
     * @param k
     * @param cmp
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static <T> T kthLargest(final T[] a, final int k, final Comparator<? super T> cmp) {
        N.checkNullOrEmpty(a, "The spcified array 'a' can't be null or empty");

        return Array.kthLargest(a, k, cmp);
    }

    /**
     * 
     * @param a
     * @param from
     * @param to
     * @param k
     * @param cmp
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static <T> T kthLargest(final T[] a, final int from, final int to, final int k, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(a) || to - from < 1) {
            throw new IllegalArgumentException("The length of array can't be null or empty");
        }

        return Array.kthLargest(a, from, to, k, cmp);
    }

    /**
     * 
     * @param c
     * @param k
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static <T extends Comparable<T>> T kthLargest(final Collection<? extends T> c, final int k) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return Array.kthLargest(c, k);
    }

    /**
     * 
     * @param c
     * @param from
     * @param to
     * @param k
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static <T extends Comparable<T>> T kthLargest(final Collection<? extends T> c, final int from, final int to, final int k) {
        if (N.isNullOrEmpty(c) || to - from < 1) {
            throw new IllegalArgumentException("The length of collection can't be null or empty");
        }

        return Array.kthLargest(c, from, to, k);
    }

    /**
     * 
     * @param c
     * @param k
     * @param cmp
     * @return the kth largest element.
     * @throws IllegalArgumentException if the length of the specified array is less than <code>k</code>.
     */
    public static <T> T kthLargest(final Collection<? extends T> c, final int k, final Comparator<? super T> cmp) {
        N.checkNullOrEmpty(c, "The spcified collection 'c' can't be null or empty");

        return Array.kthLargest(c, k, cmp);
    }

    /**
     * 
     * @param c
     * @param from
     * @param to
     * @param k
     * @param cmp
     * @return the kth largest element from <code>a[from]</code> to <code>a[to]</code>
     * @throws IllegalArgumentException if <code>to - from</code> is less than <code>k</code>.
     */
    public static <T> T kthLargest(final Collection<? extends T> c, final int from, final int to, final int k, final Comparator<? super T> cmp) {
        if (N.isNullOrEmpty(c) || to - from < 1) {
            throw new IllegalArgumentException("The length of collection can't be null or empty");
        }

        return Array.kthLargest(c, from, to, k, cmp);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Character> percentiles(final char[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Character> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Byte> percentiles(final byte[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Byte> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Short> percentiles(final short[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Short> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Integer> percentiles(final int[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Integer> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Long> percentiles(final long[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Long> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Float> percentiles(final float[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Float> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static Map<Percentage, Double> percentiles(final double[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, Double> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static <T> Map<Percentage, T> percentiles(final T[] sortedArray) {
        N.checkNullOrEmpty(sortedArray, "The spcified 'sortedArray' can't be null or empty");

        final int len = sortedArray.length;
        final Map<Percentage, T> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedArray[(int) (len * p.doubleValue())]);
        }

        return ImmutableMap.of(m);
    }

    /**
     * Returns the elements at: <code>Percentage</code> * length of the specified array.
     * 
     * @param sortedArray
     * @return
     * @throws IllegalArgumentException if the specified <code>sortedArray</code> is null or empty.
     */
    public static <T> Map<Percentage, T> percentiles(final List<T> sortedList) {
        N.checkNullOrEmpty(sortedList, "The spcified 'sortedList' can't be null or empty");

        final int size = sortedList.size();
        final Map<Percentage, T> m = new LinkedHashMap<>(32);

        for (Percentage p : Percentage.values()) {
            m.put(p, sortedList.get((int) (size * p.doubleValue())));
        }

        return ImmutableMap.of(m);
    }

    public static String toJSON(final Object obj) {
        return jsonParser.serialize(obj, jsc);
    }

    public static String toJSON(final Object obj, final JSONSerializationConfig config) {
        return jsonParser.serialize(obj, config);
    }

    public static void toJSON(final File file, final Object obj) {
        jsonParser.serialize(file, obj);
    }

    public static void toJSON(final File file, final Object obj, final JSONSerializationConfig config) {
        jsonParser.serialize(file, obj, config);
    }

    public static void toJSON(final OutputStream os, final Object obj) {
        jsonParser.serialize(os, obj);
    }

    public static void toJSON(final OutputStream os, final Object obj, final JSONSerializationConfig config) {
        jsonParser.serialize(os, obj, config);
    }

    public static void toJSON(final Writer writer, final Object obj) {
        jsonParser.serialize(writer, obj);
    }

    public static void toJSON(final Writer writer, final Object obj, final JSONSerializationConfig config) {
        jsonParser.serialize(writer, obj, config);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final String json) {
        return jsonParser.deserialize(targetClass, json);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final String json, final JSONDeserializationConfig config) {
        return jsonParser.deserialize(targetClass, json, config);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final File json) {
        return jsonParser.deserialize(targetClass, json);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final File json, final JSONDeserializationConfig config) {
        return jsonParser.deserialize(targetClass, json, config);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final InputStream json) {
        return jsonParser.deserialize(targetClass, json);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final InputStream json, final JSONDeserializationConfig config) {
        return jsonParser.deserialize(targetClass, json, config);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final Reader json) {
        return jsonParser.deserialize(targetClass, json);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final Reader json, final JSONDeserializationConfig config) {
        return jsonParser.deserialize(targetClass, json, config);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final String json, final int fromIndex, final int toIndex) {
        return jsonParser.deserialize(targetClass, json, fromIndex, toIndex);
    }

    public static <T> T fromJSON(final Class<T> targetClass, final String json, final int fromIndex, final int toIndex,
            final JSONDeserializationConfig config) {
        return jsonParser.deserialize(targetClass, json, fromIndex, toIndex, config);
    }

    public static String toXML(final Object obj) {
        return xmlParser.serialize(obj);
    }

    public static String toXML(final Object obj, final XMLSerializationConfig config) {
        return xmlParser.serialize(obj, config);
    }

    public static void toXML(final File file, final Object obj) {
        xmlParser.serialize(file, obj);
    }

    public static void toXML(final File file, final Object obj, final XMLSerializationConfig config) {
        xmlParser.serialize(file, obj, config);
    }

    public static void toXML(final OutputStream os, final Object obj) {
        xmlParser.serialize(os, obj);
    }

    public static void toXML(final OutputStream os, final Object obj, final XMLSerializationConfig config) {
        xmlParser.serialize(os, obj, config);
    }

    public static void toXML(final Writer writer, final Object obj) {
        xmlParser.serialize(writer, obj);
    }

    public static void toXML(final Writer writer, final Object obj, final XMLSerializationConfig config) {
        xmlParser.serialize(writer, obj, config);
    }

    public static <T> T fromXML(final Class<T> targetClass, final String xml) {
        return xmlParser.deserialize(targetClass, xml);
    }

    public static <T> T fromXML(final Class<T> targetClass, final String xml, final XMLDeserializationConfig config) {
        return xmlParser.deserialize(targetClass, xml, config);
    }

    public static <T> T fromXML(final Class<T> targetClass, final File xml) {
        return xmlParser.deserialize(targetClass, xml);
    }

    public static <T> T fromXML(final Class<T> targetClass, final File xml, final XMLDeserializationConfig config) {
        return xmlParser.deserialize(targetClass, xml, config);
    }

    public static <T> T fromXML(final Class<T> targetClass, final InputStream xml) {
        return xmlParser.deserialize(targetClass, xml);
    }

    public static <T> T fromXML(final Class<T> targetClass, final InputStream xml, final XMLDeserializationConfig config) {
        return xmlParser.deserialize(targetClass, xml, config);
    }

    public static <T> T fromXML(final Class<T> targetClass, final Reader xml) {
        return xmlParser.deserialize(targetClass, xml);
    }

    public static <T> T fromXML(final Class<T> targetClass, final Reader xml, final XMLDeserializationConfig config) {
        return xmlParser.deserialize(targetClass, xml, config);
    }

    public static String xml2JSON(final String xml) {
        return xml2JSON(Map.class, xml);
    }

    public static String xml2JSON(final Class<?> cls, final String xml) {
        return jsonParser.serialize(xmlParser.deserialize(cls, xml), jsc);
    }

    public static String json2XML(final String json) {
        return json2XML(Map.class, json);
    }

    public static String json2XML(final Class<?> cls, final String json) {
        return xmlParser.serialize(jsonParser.deserialize(cls, json));
    }

    public static void execute(final Try.Runnable<? extends Exception> cmd, final int retryTimes, final long retryInterval,
            final Predicate<? super Exception> retryCondition) {
        try {
            Retry0.of(retryTimes, retryInterval, retryCondition).run(cmd);
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public static <T> T execute(final Callable<T> cmd, final int retryTimes, final long retryInterval,
            final BiPredicate<? super T, ? super Exception> retryCondition) {
        try {
            final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
            return retry.call(cmd);
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    public static CompletableFuture<Void> asyncExecute(final Runnable command) {
        return asyncExecutor.execute(command);
    }

    public static CompletableFuture<Void> asyncExecute(final Runnable command, final long delay) {
        return asyncExecutor.execute(command, delay);
    }

    @SafeVarargs
    public static List<CompletableFuture<Void>> asyncExecute(final Runnable... commands) {
        return asyncExecutor.execute(commands);
    }

    public static List<CompletableFuture<Void>> asyncExecute(final List<? extends Runnable> commands) {
        return asyncExecutor.execute(commands);
    }

    public static <T> CompletableFuture<T> asyncExecute(final Callable<T> command) {
        return asyncExecutor.execute(command);
    }

    public static <T> CompletableFuture<T> asyncExecute(final Callable<T> command, final long delay) {
        return asyncExecutor.execute(command, delay);
    }

    @SafeVarargs
    public static <T> List<CompletableFuture<T>> asyncExecute(final Callable<T>... commands) {
        return asyncExecutor.execute(commands);
    }

    public static <T> List<CompletableFuture<T>> asyncExecute(final Collection<? extends Callable<T>> commands) {
        return asyncExecutor.execute(commands);
    }

    public static CompletableFuture<Void> asyncExecute(final Try.Runnable<? extends Exception> cmd, final int retryTimes, final long retryInterval,
            final Predicate<? super Exception> retryCondition) {
        return asyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    Retry0.of(retryTimes, retryInterval, retryCondition).run(cmd);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    public static <T> CompletableFuture<T> asyncExecute(final Callable<T> cmd, final int retryTimes, final long retryInterval,
            final BiPredicate<? super T, ? super Exception> retryCondition) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() {
                try {
                    final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                    return retry.call(cmd);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    public static <T> void parse(final Iterator<? extends T> iter, final Consumer<? super T> elementParser) {
        parse(iter, elementParser, null);
    }

    public static <T> void parse(final Iterator<? extends T> iter, final Consumer<? super T> elementParser, final Runnable onComplete) {
        parse(iter, 0, Long.MAX_VALUE, elementParser, onComplete);
    }

    public static <T> void parse(final Iterator<? extends T> iter, final long offset, final long count, final Consumer<? super T> elementParser) {
        parse(iter, offset, count, elementParser, null);
    }

    public static <T> void parse(final Iterator<? extends T> iter, final long offset, final long count, final Consumer<? super T> elementParser,
            final Runnable onComplete) {
        parse(iter, offset, count, 0, 0, elementParser, onComplete);
    }

    public static <T> void parse(final Iterator<? extends T> iter, long offset, long count, final int processThreadNum, final int queueSize,
            final Consumer<? super T> elementParser) {
        parse(iter, offset, count, processThreadNum, queueSize, elementParser, null);
    }

    /**
     * Parse the elements in the specified iterators one by one.
     * 
     * @param iter
     * @param offset
     * @param count
     * @param processThreadNum new threads started to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param elementParser.
     * @param onComplete
     * @param onError
     */
    public static <T> void parse(final Iterator<? extends T> iter, long offset, long count, final int processThreadNum, final int queueSize,
            final Consumer<? super T> elementParser, final Runnable onComplete) {
        parse(N.asList(iter), offset, count, 0, processThreadNum, queueSize, elementParser, onComplete);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final Consumer<? super T> elementParser) {
        parse(iterators, elementParser, null);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final Consumer<? super T> elementParser,
            final Runnable onComplete) {
        parse(iterators, 0, Long.MAX_VALUE, elementParser, onComplete);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final long offset, final long count,
            final Consumer<? super T> elementParser) {
        parse(iterators, offset, count, elementParser, null);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final long offset, final long count,
            final Consumer<? super T> elementParser, final Runnable onComplete) {
        parse(iterators, offset, count, 0, 0, 0, elementParser, onComplete);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final int readThreadNum, final int processThreadNum,
            final int queueSize, final Consumer<? super T> elementParser) {
        parse(iterators, readThreadNum, processThreadNum, queueSize, elementParser, null);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final int readThreadNum, final int processThreadNum,
            final int queueSize, final Consumer<? super T> elementParser, final Runnable onComplete) {
        parse(iterators, 0, Long.MAX_VALUE, readThreadNum, processThreadNum, queueSize, elementParser);
    }

    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final long offset, final long count, final int readThreadNum,
            final int processThreadNum, final int queueSize, final Consumer<? super T> elementParser) {
        parse(iterators, offset, count, readThreadNum, processThreadNum, queueSize, elementParser, null);
    }

    /**
     * Parse the elements in the specified iterators one by one.
     * 
     * @param iterators
     * @param offset
     * @param count
     * @param readThreadNum new threads started to parse/process the lines/records
     * @param processThreadNum new threads started to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param elementParser.
     * @param onComplete
     */
    public static <T> void parse(final Collection<? extends Iterator<? extends T>> iterators, final long offset, final long count, final int readThreadNum,
            final int processThreadNum, final int queueSize, final Consumer<? super T> elementParser, final Runnable onComplete) {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);

        if (N.isNullOrEmpty(iterators)) {
            return;
        }

        if (logger.isInfoEnabled()) {
            logger.info("### Start to parse");
        }

        try (final Stream<T> stream = ((readThreadNum > 0 || queueSize > 0)
                ? Stream.parallelConcat2(iterators, (readThreadNum == 0 ? 1 : readThreadNum), (queueSize == 0 ? 1024 : queueSize))
                : Stream.concat2(iterators))) {

            final Iterator<? extends T> iteratorII = stream.skip(offset).limit(count).iterator();

            if (processThreadNum == 0) {
                while (iteratorII.hasNext()) {
                    elementParser.accept(iteratorII.next());
                }

                if (onComplete != null) {
                    onComplete.run();
                }
            } else {
                final AtomicInteger activeThreadNum = new AtomicInteger();
                final ExecutorService executorService = Executors.newFixedThreadPool(processThreadNum);
                final Holder<Throwable> errorHolder = new Holder<>();

                for (int i = 0; i < processThreadNum; i++) {
                    activeThreadNum.incrementAndGet();

                    executorService.execute(new Runnable() {
                        @Override
                        public void run() {
                            T element = null;
                            try {
                                while (errorHolder.value() == null) {
                                    synchronized (iteratorII) {
                                        if (iteratorII.hasNext()) {
                                            element = iteratorII.next();
                                        } else {
                                            break;
                                        }
                                    }

                                    elementParser.accept(element);
                                }
                            } catch (Throwable e) {
                                synchronized (errorHolder) {
                                    if (errorHolder.value() == null) {
                                        errorHolder.setValue(e);
                                    } else {
                                        errorHolder.value().addSuppressed(e);
                                    }
                                }
                            } finally {
                                activeThreadNum.decrementAndGet();
                            }
                        }
                    });
                }

                while (activeThreadNum.get() > 0) {
                    N.sleep(1);
                }

                if (errorHolder.value() == null && onComplete != null) {
                    try {
                        onComplete.run();
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    }
                }

                if (errorHolder.value() != null) {
                    throw N.toRuntimeException(errorHolder.value());
                }
            }
        } finally {
            if (logger.isInfoEnabled()) {
                logger.info("### ### End to parse");
            }
        }
    }

    @Beta
    @Internal
    @Deprecated
    public static char[] getCharsForReadOnly(final String str) {
        if (isStringCharsGettable && strValueField != null && str.length() > 3) {
            try {
                final char[] chars = (char[]) strValueField.get(str);

                if (chars.length == str.length()) {
                    return chars;
                } else {
                    isStringCharsGettable = false;
                }

            } catch (Exception e) {
                // ignore.
                isStringCharsGettable = false;
            }
        }

        return str.toCharArray();
    }

    public static RuntimeException toRuntimeException(Throwable e) {
        if (e instanceof RuntimeException) {
            return (RuntimeException) e;
        } else if (e instanceof ExecutionException || e instanceof InvocationTargetException) {
            return e.getCause() == null ? new RuntimeException(e) : toRuntimeException(e.getCause());
        } else if (e instanceof IOException) {
            return new UncheckedIOException(e);
        } else if (e instanceof SQLException) {
            return new UncheckedSQLException(e);
        } else {
            return new RuntimeException(e);
        }
    }

    public static void sleep(final long millis) {
        if (millis > 0) {
            try {
                Thread.sleep(millis);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * 
     * @param obj
     * @return the input <code>obj</code>
     */
    public static <T> T println(final T obj) {
        final String str = N.deepToString(obj);
        System.out.println(str);
        return obj;
    }

    //    public static <T> T println(final T obj, final int repeat) {
    //        final String str = N.repeat(N.deepToString(obj), repeat);
    //        System.out.println(str);
    //        return obj;
    //    }

    //    public static void println(final Object key, final Object value) {
    //        final StringBuilder sb = ObjectFactory.createStringBuilder();
    //        try {
    //            System.out.println(sb.append(N.deepToString(key)).append(" : ").append(N.deepToString(value)).toString());
    //        } finally {
    //            ObjectFactory.recycle(sb);
    //        }
    //    }
    //    
    //    public static void println(final Object a, final Object b, final Object c) {
    //        final StringBuilder sb = ObjectFactory.createStringBuilder();
    //        try {
    //            System.out.println(sb.append(N.deepToString(a)).append(" : ").append(N.deepToString(b)).append(" : ").append(N.deepToString(c)).toString());
    //        } finally {
    //            ObjectFactory.recycle(sb);
    //        }
    //    }

    /**
     * 
     * @param format
     * @param args
     * @return the input <code>args</code>
     */
    @SafeVarargs
    public static <T> T[] fprintln(final String format, final T... args) {
        System.out.printf(format, args);
        System.out.println();
        return args;
    }

    static <T> T createMask(final Class<T> interfaceClass) {
        InvocationHandler h = new InvocationHandler() {
            @Override
            public Object invoke(final Object proxy, final Method method, final Object[] args) throws Throwable {
                throw new UnsupportedOperationException("It's a mask");
            }
        };

        return newProxyInstance(interfaceClass, h);
    }

    static class NullMask implements Serializable {
        private static final long serialVersionUID = 5887875956120266479L;

        private NullMask() {
        }

        @Override
        public String toString() {
            return "NULL";
        }

        private Object readResolve() {
            return NULL_MASK;
        }
    }
}
