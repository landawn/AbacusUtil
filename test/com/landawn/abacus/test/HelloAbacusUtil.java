package com.landawn.abacus.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import com.landawn.abacus.parser.JSONParser;
import com.landawn.abacus.parser.ParserFactory;
import com.landawn.abacus.parser.XMLParser;
import com.landawn.abacus.test.entity.Account;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.TestUtil;

/**
 * Refer to HelloAbacusUtil for more tests: http://landawn.com/download.html
 * 
 * @author HaiYang Li
 *
 */
public class HelloAbacusUtil {
    static final XMLParser xmlParser = ParserFactory.createXMLParser();
    static final JSONParser jsonParser = ParserFactory.createJSONParser();

    @Test
    public void test_01() {
        char[] a = { 'a', 'b', 'c' };
        char[] b = { 'd', 'e', 'f' };
        char[] c = N.concat(a, b);

        String str = N.join(c);
        N.println(str);

        char[] d = N.string2Array(char[].class, str);
        assertTrue(N.equals(c, d));

        int[] e = Array.range(0, 6);
        N.println(e);
    }

    @Test
    public void test_as() {
        String[] strs = N.asArray("a", "b", "c");
        N.println(strs);

        List<String> list = N.asList("a", "b", "c");
        N.println(list);

        Set<String> set = N.asSet("a", "b", "c");
        N.println(set);

        Map<String, Integer> map = N.asMap("a", 1, "b", 2, "c", 3);
        N.println(map);

        List<String> unmodifiableList = N.asImmutableList(list);
        N.println(unmodifiableList);

        Map<String, Integer> unmodifiableMap = N.asImmutableMap(map);
        N.println(unmodifiableMap);
    }

    @Test
    public void test_parser() {
        Account account = TestUtil.createEntity(Account.class);
        String xml = xmlParser.serialize(account);
        N.println(xml);

        Account account2 = xmlParser.deserialize(Account.class, xml);
        assertEquals(account, account2);

        String json = jsonParser.serialize(account);
        N.println(json);

        account2 = jsonParser.deserialize(Account.class, json);
        assertEquals(account, account2);
    }

    @Test
    public void test_lambda() {
        Account[] accounts = N.asArray(TestUtil.createEntity(Account.class), TestUtil.createEntity(Account.class));
        Account[] accounts2 = N.filter(accounts, e -> e.getFirstName().equals(accounts[1].getFirstName()));
        assertEquals(accounts[1], accounts2[0]);
    }
}
