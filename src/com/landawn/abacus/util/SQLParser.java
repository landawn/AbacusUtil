/*
 * Copyright (C) 2015 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class SQLParser {
    private static final char TAB = '\t';
    private static final char ENTER = '\n';
    private static final Map<Object, Object> seperators = new HashMap<>();

    static {
        seperators.put(TAB, TAB);
        seperators.put(ENTER, ENTER);
        seperators.put(WD._SPACE, WD._SPACE);
        seperators.put(WD._COMMA, WD._COMMA);
        seperators.put(WD._SEMICOLON, WD._SEMICOLON);
        seperators.put(WD._PARENTHESES_L, WD._PARENTHESES_L);
        seperators.put(WD._PARENTHESES_R, WD._PARENTHESES_R);
        seperators.put(WD._EQUAL, WD._EQUAL);
        seperators.put(WD.NOT_EQUAL, WD.NOT_EQUAL);
        seperators.put(WD.NOT_EQUAL2, WD.NOT_EQUAL2);
        seperators.put(WD._GREATER_THAN, WD._GREATER_THAN);
        seperators.put(WD.GREATER_EQUAL, WD.GREATER_EQUAL);
        seperators.put(WD._LESS_THAN, WD._LESS_THAN);
        seperators.put(WD.LESS_EQUAL, WD.LESS_EQUAL);
        seperators.put(WD._PLUS, WD._PLUS);
        seperators.put(WD._MINUS, WD._MINUS);
        seperators.put(WD._PERCENT, WD._PERCENT);
        seperators.put(WD._SLASH, WD._SLASH);
        seperators.put(WD._ASTERISK, WD._ASTERISK);
        seperators.put(WD._AMPERSAND, WD._AMPERSAND);
        seperators.put(WD._VERTICALBAR, WD._VERTICALBAR);
        seperators.put(WD._CIRCUMFLEX, WD._CIRCUMFLEX);
        seperators.put(WD._UNARYBIT, WD._UNARYBIT);
        seperators.put(WD._EXCLAMATION, WD._EXCLAMATION);
    }

    private static final Map<String, String[]> compositeWords = new ObjectPool<String, String[]>(64);

    static {
        compositeWords.put(WD.LEFT_JOIN, new String[] { "LEFT", "JOIN" });
        compositeWords.put(WD.RIGHT_JOIN, new String[] { "RIGHT", "JOIN" });
        compositeWords.put(WD.FULL_JOIN, new String[] { "FULL", "JOIN" });
        compositeWords.put(WD.CROSS_JOIN, new String[] { "CROSS", "JOIN" });
        compositeWords.put(WD.INNER_JOIN, new String[] { "INNER", "JOIN" });
        compositeWords.put(WD.NATURAL_JOIN, new String[] { "NATURAL", "JOIN" });
        compositeWords.put(WD.INNER_JOIN, new String[] { "INNER", "JOIN" });
        compositeWords.put(WD.GROUP_BY, new String[] { "GROUP", "BY" });
        compositeWords.put(WD.ORDER_BY, new String[] { "ORDER", "BY" });
        compositeWords.put(WD.FOR_UPDATE, new String[] { "FOR", "UPDATE" });
        compositeWords.put(WD.FETCH_FIRST, new String[] { "FETCH", "FIRST" });
        compositeWords.put(WD.FETCH_NEXT, new String[] { "FETCH", "NEXT" });
        compositeWords.put(WD.ROWS_ONLY, new String[] { "ROWS", "ONLY" });
        compositeWords.put(WD.UNION_ALL, new String[] { "UNION", "ALL" });
        compositeWords.put(WD.IS_NOT, new String[] { "IS", "NOT" });
        compositeWords.put(WD.IS_NULL, new String[] { "IS", "NULL" });
        compositeWords.put(WD.IS_NOT_NULL, new String[] { "IS", "NOT", "NULL" });
        compositeWords.put(WD.IS_EMPTY, new String[] { "IS", "EMPTY" });
        compositeWords.put(WD.IS_NOT_EMPTY, new String[] { "IS", "NOT", "EMPTY" });
        compositeWords.put(WD.IS_BLANK, new String[] { "IS", "BLANK" });
        compositeWords.put(WD.IS_NOT_BLANK, new String[] { "IS", "NOT", "BLANK" });

        List<String> list = new ArrayList<>(compositeWords.keySet());

        for (String e : list) {
            e = e.toLowerCase();

            if (!compositeWords.containsKey(e)) {
                compositeWords.put(e, Splitter.with(WD.SPACE).trim(true).splitToArray(e));
            }

            e = e.toUpperCase();

            if (!compositeWords.containsKey(e)) {
                compositeWords.put(e, Splitter.with(WD.SPACE).trim(true).splitToArray(e));
            }
        }
    }

    private SQLParser() {
    }

    public static List<String> parse(String sql) {
        final int sqlLength = sql.length();
        final StringBuilder sb = ObjectFactory.createStringBuilder();
        final List<String> words = new ArrayList<>();

        char quoteChar = 0;

        for (int index = 0; index < sqlLength; index++) {
            // TODO [performance improvement]. will it improve performance if
            // change to char array?
            // char c = sqlCharArray[charIndex];
            char c = sql.charAt(index);

            if ((WD._BACKSLASH == c) && (index < (sqlLength - 1))) {
                sb.append(c);
                sb.charAt(++index);

                continue;
            }

            // is it in a quoted identifier?
            if (quoteChar != 0) {
                sb.append(c);

                // end in quote.
                if (c == quoteChar) {
                    words.add(sb.toString());
                    sb.setLength(0);

                    quoteChar = 0;
                }
            } else {
                if (seperators.containsKey(c)) {
                    if (sb.length() > 0) {
                        words.add(sb.toString());
                        sb.setLength(0);
                    }

                    if ((index < (sqlLength - 1))) {
                        String temp = sql.substring(index, index + 2);

                        if (seperators.containsKey(temp)) {
                            words.add(temp);
                            index++;
                        } else {
                            if ((c == WD._SPACE) || (c == TAB) || (c == ENTER)) {
                                if ((words.size() > 0) && !words.get(words.size() - 1).equals(WD.SPACE)) {
                                    words.add(WD.SPACE);
                                }
                            } else {
                                words.add(String.valueOf(c));
                            }
                        }
                    } else {
                        if ((c == WD._SPACE) || (c == TAB) || (c == ENTER)) {
                            if ((words.size() > 0) && !words.get(words.size() - 1).equals(WD.SPACE)) {
                                words.add(WD.SPACE);
                            }
                        } else {
                            words.add(String.valueOf(c));
                        }
                    }
                } else {
                    sb.append(c);

                    if ((c == WD._QUOTATION_S) || (c == WD._QUOTATION_D)) {
                        quoteChar = c;
                    }
                }
            }

            if ((index == (sqlLength - 1)) && (sb.length() > 0)) {
                words.add(sb.toString());
            }
        }

        ObjectFactory.recycle(sb);

        return words;
    }

    public static int indexWord(String sql, String word, int fromIndex, boolean caseSensitive) {
        int result = -1;

        String[] subWords = compositeWords.get(word);

        if (subWords == null) {
            subWords = Splitter.with(WD.SPACE).trim(true).splitToArray(word);
            compositeWords.put(word, subWords);
        }

        if ((subWords == null) || (subWords.length <= 1)) {
            final StringBuilder sb = ObjectFactory.createStringBuilder();
            final int sqlLength = sql.length();
            char quoteChar = 0;

            for (int index = fromIndex; index < sqlLength; index++) {
                char c = sql.charAt(index);

                if ((WD._BACKSLASH == c) && (index < (sqlLength - 1))) {
                    sb.append(c);
                    sb.charAt(++index);

                    continue;
                }

                // is it in a quoted identifier?
                if (quoteChar != 0) {
                    sb.append(c);

                    // end in quote.
                    if (c == quoteChar) {
                        String temp = sb.toString();

                        if (word.equals(temp) || (!caseSensitive && word.equalsIgnoreCase(temp))) {
                            result = index - word.length();

                            break;
                        }

                        sb.setLength(0);
                        quoteChar = 0;
                    }
                } else {
                    if (seperators.containsKey(c)) {
                        if ((sb.length() == 0) && (c != WD._SPACE) && (c != TAB) && (c != ENTER)) {
                            if ((index < (sqlLength - 1))) {
                                String temp = sql.substring(index, index + 2);

                                if (seperators.containsKey(temp)) {
                                    sb.append(temp);
                                    index++;
                                } else {
                                    sb.append(c);
                                }
                            } else {
                                sb.append(c);
                            }
                        }

                        if (sb.length() > 0) {
                            String temp = sb.toString();

                            if (word.equals(temp) || (!caseSensitive && word.equalsIgnoreCase(temp))) {
                                result = index - word.length();

                                break;
                            }

                            sb.setLength(0);
                        }
                    } else {
                        sb.append(c);

                        if ((c == WD._QUOTATION_S) || (c == WD._QUOTATION_D)) {
                            quoteChar = c;
                        }
                    }
                }

                if ((index == (sqlLength - 1)) && (sb.length() > 0)) {
                    String temp = sb.toString();

                    if (word.equals(temp) || (!caseSensitive && word.equalsIgnoreCase(temp))) {
                        result = sqlLength - word.length();

                        break;
                    }
                }
            }

            ObjectFactory.recycle(sb);
        } else {
            result = indexWord(sql, subWords[0], fromIndex, caseSensitive);

            if (result >= 0) {
                int tmpIndex = result + subWords[0].length();
                String nextWord = null;

                for (int i = 1; i < subWords.length; i++) {
                    nextWord = nextWord(sql, tmpIndex);

                    if ((nextWord != null) && (nextWord.equals(subWords[i]) || (!caseSensitive && nextWord.equalsIgnoreCase(subWords[i])))) {
                        tmpIndex += (subWords[i].length() + 1);
                    } else {
                        result = -1;

                        break;
                    }
                }
            }
        }

        return result;
    }

    public static String nextWord(String sql, int fromIndex) {
        final int sqlLength = sql.length();
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        char quoteChar = 0;

        for (int index = fromIndex; index < sqlLength; index++) {
            char c = sql.charAt(index);

            if ((WD._BACKSLASH == c) && (index < (sqlLength - 1))) {
                sb.append(c);
                sb.charAt(++index);

                continue;
            }

            // is it in a quoted identifier?
            if (quoteChar != 0) {
                sb.append(c);

                // end in quote.
                if (c == quoteChar) {
                    break;
                }
            } else {
                if (seperators.containsKey(c)) {
                    if ((sb.length() == 0) && (c != WD._SPACE) && (c != TAB) && (c != ENTER)) {
                        if ((index < (sqlLength - 1))) {
                            String temp = sql.substring(index, index + 2);

                            if (seperators.containsKey(temp)) {
                                sb.append(temp);
                            } else {
                                sb.append(c);
                            }
                        } else {
                            sb.append(c);
                        }
                    }

                    if (sb.length() > 0) {
                        break;
                    }
                } else {
                    sb.append(c);

                    if ((c == WD._QUOTATION_S) || (c == WD._QUOTATION_D)) {
                        quoteChar = c;
                    }
                }
            }
        }

        String st = (sb.length() == 0) ? null : sb.toString();
        ObjectFactory.recycle(sb);

        return st;
    }
}
