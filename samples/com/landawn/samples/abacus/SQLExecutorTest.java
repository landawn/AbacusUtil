package com.landawn.samples.abacus;

import static org.junit.Assert.assertEquals;

import java.sql.Timestamp;
import java.util.List;

import javax.sql.DataSource;

import org.junit.Test;

import com.landawn.abacus.annotation.Id;
import com.landawn.abacus.annotation.ReadOnly;
import com.landawn.abacus.condition.ConditionFactory.CF;
import com.landawn.abacus.util.JdbcUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.SQLBuilder.NSC;
import com.landawn.abacus.util.SQLBuilder.SP;
import com.landawn.abacus.util.SQLExecutor;
import com.landawn.abacus.util.SQLExecutor.JdbcSettings;
import com.landawn.abacus.util.SQLExecutor.Mapper;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * All I want to do is: insert -> read -> update -> delete a record in DB table.
 */
public class SQLExecutorTest {
    static final DataSource dataSource = JdbcUtil.createDataSource("jdbc:h2:~/test", "sa", "");
    static final SQLExecutor sqlExecutor = new SQLExecutor(dataSource);
    static final Mapper<User> userMapper = sqlExecutor.mapper(User.class);

    // initialize DB schema.
    static {
        final String sql_user_drop_table = "DROP TABLE IF EXISTS user";

        final String sql_user_creat_table = "CREATE TABLE IF NOT EXISTS user (" //
                + "id bigint(20) NOT NULL AUTO_INCREMENT PRIMARY KEY, " //
                + "first_name varchar(32) NOT NULL, " //
                + "last_name varchar(32) NOT NULL, " //
                + "email varchar(32), " //
                + "create_time timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP)";

        sqlExecutor.execute(sql_user_drop_table);
        sqlExecutor.execute(sql_user_creat_table);
    }

    // Entity Object mapped to record in DB table.
    @Builder
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class User {
        @Id
        private long id;
        private String firstName;
        private String lastName;
        private String email;
        @ReadOnly
        private Timestamp createTime;
    }

    @Test
    public void batch_01() {

        List<User> users = N.fill(User.class, 99);
        String sql_insert = NSC.insertInto(User.class, N.asSet("id")).sql();
        JdbcSettings jdbcSettings = JdbcSettings.create().setBatchSize(1000);
        // insert 99 users, less the specified batch size.
        List<Long> ids = sqlExecutor.batchInsert(sql_insert, jdbcSettings, users);
        N.println(ids);

        assertEquals(users.size(), ids.size());

        SP sp = NSC.selectFrom(User.class).where(CF.in("id", ids)).pair();
        sqlExecutor.query(sp.sql, sp.parameters).println();

        // insert 3001 users, bigger the specified batch size.
        users = N.fill(User.class, 3001);
        ids = sqlExecutor.batchInsert(sql_insert, jdbcSettings, users);
        N.println(ids);

        assertEquals(users.size(), ids.size());

        sp = NSC.selectFrom(User.class).where(CF.in("id", ids)).pair();
        sqlExecutor.query(sp.sql, sp.parameters).println();
    }
}
