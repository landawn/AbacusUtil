/*
 * Copyright (C) 2016 HaiYang Li
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

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import org.neo4j.ogm.cypher.Filter;
import org.neo4j.ogm.cypher.Filters;
import org.neo4j.ogm.cypher.query.Pagination;
import org.neo4j.ogm.cypher.query.SortOrder;
import org.neo4j.ogm.session.Session;
import org.neo4j.ogm.session.SessionFactory;

import com.landawn.abacus.util.stream.Stream;

/**
 * It's a simple wrapper of Neo4j Java client.
 * Refer to: http://neo4j.com/docs/ogm/java/stable/ 
 * 
 * @since 0.8
 * 
 * @author HaiYang Li
 */
public final class Neo4jExecutor {
    private final LinkedBlockingQueue<Session> sessionPool = new LinkedBlockingQueue<Session>(8192);
    private final SessionFactory sessionFactory;

    public Neo4jExecutor(SessionFactory sessionFactory) {
        this.sessionFactory = sessionFactory;
    }

    public SessionFactory sessionFactory() {
        return sessionFactory;
    }

    public Session openSession() {
        return sessionFactory.openSession();
    }

    public <T> T load(Class<T> targetClass, Long id) {
        final Session session = getSession();

        try {
            return session.load(targetClass, id);
        } finally {
            closeSession(session);
        }
    }

    public <T> T load(Class<T> targetClass, Long id, int depth) {
        final Session session = getSession();

        try {
            return session.load(targetClass, id, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, SortOrder sortOrder) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, sortOrder);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, SortOrder sortOrder, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, sortOrder, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, SortOrder sortOrder, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, sortOrder, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Collection<Long> ids, SortOrder sortOrder, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, ids, sortOrder, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects) {
        final Session session = getSession();

        try {
            return session.loadAll(objects);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, SortOrder sortOrder) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, sortOrder);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, SortOrder sortOrder, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, sortOrder, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, SortOrder sortOrder, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, sortOrder, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Collection<T> objects, SortOrder sortOrder, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(objects, sortOrder, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, SortOrder sortOrder) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, sortOrder);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, SortOrder sortOrder, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, sortOrder, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, SortOrder sortOrder, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, sortOrder, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, SortOrder sortOrder, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, sortOrder, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, SortOrder sortOrder) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, sortOrder);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, SortOrder sortOrder, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, sortOrder, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, SortOrder sortOrder, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, sortOrder, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filter filter, SortOrder sortOrder, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filter, sortOrder, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, SortOrder sortOrder) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, sortOrder);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, SortOrder sortOrder, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, sortOrder, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, SortOrder sortOrder, Pagination pagination) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, sortOrder, pagination);
        } finally {
            closeSession(session);
        }
    }

    public <T> Collection<T> loadAll(Class<T> targetClass, Filters filters, SortOrder sortOrder, Pagination pagination, int depth) {
        final Session session = getSession();

        try {
            return session.loadAll(targetClass, filters, sortOrder, pagination, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> void save(T object) {
        final Session session = getSession();

        try {
            session.save(object);
        } finally {
            closeSession(session);
        }
    }

    public <T> void save(T object, int depth) {
        final Session session = getSession();

        try {
            session.save(object, depth);
        } finally {
            closeSession(session);
        }
    }

    public <T> void delete(T object) {
        final Session session = getSession();

        try {
            session.delete(object);
        } finally {
            closeSession(session);
        }
    }

    public <T> void deleteAll(Class<T> targetClass) {
        final Session session = getSession();

        try {
            session.deleteAll(targetClass);
        } finally {
            closeSession(session);
        }

    }

    public <T> T queryForObject(Class<T> objectType, String cypher, Map<String, ?> parameters) {
        final Session session = getSession();

        try {
            return session.queryForObject(objectType, cypher, parameters);
        } finally {
            closeSession(session);
        }
    }

    public Stream<Map<String, Object>> query(String cypher, Map<String, ?> parameters) {
        final Session session = getSession();

        return Stream.of(session.query(cypher, parameters).iterator()).onClose(newCloseHandle(session));
    }

    public Stream<Map<String, Object>> query(String cypher, Map<String, ?> parameters, boolean readOnly) {
        final Session session = getSession();

        return Stream.of(session.query(cypher, parameters, readOnly).iterator()).onClose(newCloseHandle(session));
    }

    public <T> Stream<T> query(Class<T> objectType, String cypher, Map<String, ?> parameters) {
        final Session session = getSession();

        return Stream.of(session.query(objectType, cypher, parameters).iterator()).onClose(newCloseHandle(session));
    }

    private Runnable newCloseHandle(final Session session) {
        return new Runnable() {
            @Override
            public void run() {
                closeSession(session);
            }
        };
    }

    public long countEntitiesOfType(Class<?> entity) {
        final Session session = getSession();

        try {
            return session.countEntitiesOfType(entity);
        } finally {
            closeSession(session);
        }
    }

    public Long resolveGraphIdFor(Object possibleEntity) {
        final Session session = getSession();

        try {
            return session.resolveGraphIdFor(possibleEntity);
        } finally {
            closeSession(session);
        }
    }

    private Session getSession() {
        Session session = null;

        try {
            session = sessionPool.poll(100, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            // ignore.
        }

        if (session == null) {
            session = openSession();
        }

        return session;
    }

    private void closeSession(Session session) {
        if (session != null) {
            try {
                sessionPool.offer(session, 100, TimeUnit.MILLISECONDS);
            } catch (Exception e) {
                // ignore.
            }
        }
    }

}
