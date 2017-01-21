package com.landawn.samples.abacus;

import java.util.List;
import java.util.Random;

import org.junit.Test;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.samples.abacus.entity.Account;
import com.landawn.samples.abacus.entity.Contact;

public class DataSetTest {
    static final Random RAND = new Random();

    @Test
    public void test_join() {
        List<Account> accounts = N.asList(N.fill(Account.class), N.fill(Account.class), N.fill(Account.class));
        List<Contact> contacts = N.asList(N.fill(Contact.class), N.fill(Contact.class), N.fill(Contact.class));

        IntStream.range(0, N.min(accounts.size(), contacts.size())).forEach(i -> contacts.get(i).setAccountId(accounts.get(i).getId()));

        DataSet ds1 = N.newDataSet(accounts);
        ds1.println();

        DataSet ds2 = N.newDataSet(contacts);
        ds2.println();
        ds2.renameColumn(N.asList("id", "status", "lastUpdateTime", "createTime"), e -> "contact" + N.capitalize(e));

        DataSet ds3 = ds1.join(ds2, N.asMap("id", "accountId"));
        ds3.println();
    }

}
