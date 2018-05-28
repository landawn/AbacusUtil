package com.landawn.samples.abacus;

import java.util.Random;

import org.junit.Test;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.util.N;
import com.landawn.samples.abacus.entity.Account;
import com.landawn.samples.abacus.entity.Contact;

public class DataSetTest {
    static final Random RAND = new Random();

    @Test
    public void test_join() {
        Account account = new Account().setId(1001).setFirstName("Tom");
        Contact contact = new Contact().setId(2001).setAccountId(1001).setAddress("1 Rd");

        DataSet ds1 = N.newDataSet(N.asList("id", "firstName"), N.asList(account));
        ds1.println();

        DataSet ds2 = N.newDataSet(N.asList("id", "accountId", "address"), N.asList(contact));
        ds2.println();
        ds2.renameColumn("id", "contactId");

        DataSet ds3 = ds1.innerJoin(ds2, N.asMap("id", "accountId"));
        ds3.println();
    }

}
