/*
 * Copyright (C) 2018 HaiYang Li
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

/**
 * Dynamic SQL builder. Must remember to call {@code build()} to generate target sql and release resources.
 * 
 * @author haiyangl
 *
 */
public class DynamicSQLBuilder {
    private Select select = new Select(ObjectFactory.createStringBuilder());
    private From from = new From(ObjectFactory.createStringBuilder());
    private Where where;
    private GroupBy groupBy;
    private Having having;
    private OrderBy orderBy;
    private String limitCond;
    private StringBuilder moreParts = null;

    private DynamicSQLBuilder() {

    }

    public static DynamicSQLBuilder create() {
        return new DynamicSQLBuilder();
    }

    public Select select() {
        return select;
    }

    public From from() {
        return from;
    }

    public Where where() {
        if (where == null) {
            where = new Where(ObjectFactory.createStringBuilder());
        }

        return where;
    }

    public GroupBy groupBy() {
        if (groupBy == null) {
            groupBy = new GroupBy(ObjectFactory.createStringBuilder());
        }

        return groupBy;
    }

    public Having having() {
        if (having == null) {
            having = new Having(ObjectFactory.createStringBuilder());
        }

        return having;
    }

    public OrderBy orderBy() {
        if (orderBy == null) {
            orderBy = new OrderBy(ObjectFactory.createStringBuilder());
        }

        return orderBy;
    }

    public DynamicSQLBuilder limit(String limitCond) {
        this.limitCond = limitCond;

        return this;
    }

    public DynamicSQLBuilder limit(int count) {
        return limit("LIMIT " + count);
    }

    public DynamicSQLBuilder limit(int offset, int count) {
        return limit("LIMIT " + offset + ", " + count);
    }

    public DynamicSQLBuilder limitByRowNum(int count) {
        return limit("ROWNUM < " + count);
    }

    public DynamicSQLBuilder union(final String query) {
        if (moreParts == null) {
            moreParts = ObjectFactory.createStringBuilder();
        }

        moreParts.append(" UNION ").append(query);

        return this;
    }

    public DynamicSQLBuilder unionAll(final String query) {
        if (moreParts == null) {
            moreParts = ObjectFactory.createStringBuilder();
        }

        moreParts.append(" UNION ALL ").append(query);

        return this;
    }

    public DynamicSQLBuilder intersect(final String query) {
        if (moreParts == null) {
            moreParts = ObjectFactory.createStringBuilder();
        }

        moreParts.append(" INTERSECT ").append(query);

        return this;
    }

    public DynamicSQLBuilder except(final String query) {
        if (moreParts == null) {
            moreParts = ObjectFactory.createStringBuilder();
        }

        moreParts.append(" EXCEPT ").append(query);

        return this;
    }

    public DynamicSQLBuilder minus(final String query) {
        if (moreParts == null) {
            moreParts = ObjectFactory.createStringBuilder();
        }

        moreParts.append(" MINUS ").append(query);

        return this;
    }

    public String build() {
        select.sb.append(" ").append(from.sb);

        if (where != null) {
            select.sb.append(" ").append(where.sb);
            ObjectFactory.recycle(where.sb);
            where = null;
        }

        if (groupBy != null) {
            select.sb.append(" ").append(groupBy.sb);
            ObjectFactory.recycle(groupBy.sb);
            groupBy = null;
        }

        if (having != null) {
            select.sb.append(" ").append(having.sb);
            ObjectFactory.recycle(having.sb);
            having = null;
        }

        if (orderBy != null) {
            select.sb.append(" ").append(orderBy.sb);
            ObjectFactory.recycle(orderBy.sb);
            orderBy = null;
        }

        if (N.notNullOrEmpty(limitCond)) {
            select.sb.append(" ").append(limitCond);
        }

        if (moreParts != null) {
            select.sb.append(moreParts);
            ObjectFactory.recycle(moreParts);
        }

        final String sql = select.sb.toString();
        ObjectFactory.recycle(from.sb);
        ObjectFactory.recycle(select.sb);

        select = null;
        from = null;

        return sql;
    }

    public static class Select {
        final StringBuilder sb;

        Select(final StringBuilder sb) {
            this.sb = sb;
        }

        public Select append(String column) {
            if (sb.length() > 0) {
                sb.append(", ");
            } else {
                sb.append("SELECT ");
            }

            sb.append(column);

            return this;
        }

        public Select append(String column, String alias) {
            if (sb.length() > 0) {
                sb.append(", ");
            } else {
                sb.append("SELECT ");
            }

            sb.append(column).append(" AS ").append(alias);

            return this;
        }
    }

    public static class From {
        final StringBuilder sb;

        From(final StringBuilder sb) {
            this.sb = sb;
        }

        public From append(String table) {
            if (sb.length() > 0) {
                sb.append(", ");
            } else {
                sb.append("FROM ");
            }

            sb.append(table);

            return this;
        }

        public From append(String table, String alias) {
            if (sb.length() > 0) {
                sb.append(", ");
            } else {
                sb.append("FROM");
            }

            sb.append(table).append(" ").append(alias);

            return this;
        }

        public From join(String table, String on) {
            sb.append(" JOIN ").append(table).append(" ON ").append(on);

            return this;
        }

        public From innerJoin(String table, String on) {
            sb.append(" INNER JOIN ").append(table).append(" ON ").append(on);

            return this;
        }

        public From leftJoin(String table, String on) {
            sb.append(" LEFT JOIN ").append(table).append(" ON ").append(on);

            return this;
        }

        public From rightJoin(String table, String on) {
            sb.append(" RIGHT JOIN ").append(table).append(" ON ").append(on);

            return this;
        }

        public From fullJoin(String table, String on) {
            sb.append(" FULL JOIN ").append(table).append(" ON ").append(on);

            return this;
        }
    }

    public static class Where {
        final StringBuilder sb;

        Where(final StringBuilder sb) {
            this.sb = sb;
        }

        public Where append(String cond) {
            if (sb.length() > 0) {
                sb.append(" ");
            } else {
                sb.append("WHERE ");
            }

            sb.append(cond);

            return this;
        }

        public Where and(String cond) {
            sb.append(" AND ").append(cond);

            return this;
        }

        public Where or(String cond) {
            sb.append(" OR ").append(cond);

            return this;
        }
    }

    public static class GroupBy {
        final StringBuilder sb;

        GroupBy(final StringBuilder sb) {
            this.sb = sb;
        }

        public GroupBy append(String column) {
            if (sb.length() > 0) {
                sb.append(", ");
            } else {
                sb.append("GROUP BY ");
            }

            sb.append(column);

            return this;
        }
    }

    public static class Having {
        final StringBuilder sb;

        Having(final StringBuilder sb) {
            this.sb = sb;
        }

        public Having append(String cond) {
            if (sb.length() > 0) {
                sb.append(" ");
            } else {
                sb.append("HAVING ");
            }

            sb.append(cond);

            return this;
        }

        public Having and(String cond) {
            sb.append(" AND ").append(cond);

            return this;
        }

        public Having or(String cond) {
            sb.append(" OR ").append(cond);

            return this;
        }
    }

    public static class OrderBy {
        final StringBuilder sb;

        OrderBy(final StringBuilder sb) {
            this.sb = sb;
        }

        public OrderBy append(String column) {
            if (sb.length() > 0) {
                sb.append(", ");
            } else {
                sb.append("ORDER BY ");
            }

            sb.append(column);

            return this;
        }
    }

    public static final class DSB extends DynamicSQLBuilder {
        private DSB() {
            super();
        }
    }
}
