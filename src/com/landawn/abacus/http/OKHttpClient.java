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

package com.landawn.abacus.http;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import javax.net.ssl.SSLSocketFactory;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.BufferedReader;
import com.landawn.abacus.util.BufferedWriter;
import com.landawn.abacus.util.ByteArrayOutputStream;
import com.landawn.abacus.util.ContinuableFuture;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Objectory;
import com.landawn.abacus.util.URLEncodedUtil;

import okhttp3.ConnectionPool;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.RequestBody;

/**
 * Any header can be set into the parameter <code>settings</code>
 * 
 * <br>HttpClient is thread safe.</br>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class OKHttpClient extends AbstractHttpClient {
    static final Logger logger = LoggerFactory.getLogger(OKHttpClient.class);

    private static final Map<String, MediaType> mediaTypePool = new ConcurrentHashMap<>();
    private final OkHttpClient client;
    private final AtomicInteger _activeConnectionCounter;

    protected OKHttpClient(String url) {
        this(url, DEFAULT_MAX_CONNECTION);
    }

    protected OKHttpClient(String url, int maxConnection) {
        this(url, maxConnection, DEFAULT_CONNECTION_TIMEOUT, DEFAULT_READ_TIMEOUT);
    }

    protected OKHttpClient(String url, int maxConnection, long connTimeout, long readTimeout) {
        this(url, maxConnection, connTimeout, readTimeout, null);
    }

    protected OKHttpClient(String url, int maxConnection, long connTimeout, long readTimeout, Map<String, Object> settings) {
        this(url, maxConnection, connTimeout, readTimeout, settings, new AtomicInteger(0));
    }

    protected OKHttpClient(String url, int maxConnection, long connTimeout, long readTimeout, Map<String, Object> settings,
            final AtomicInteger sharedActiveConnectionCounter) {
        super(url, maxConnection, connTimeout, readTimeout, settings);

        final SSLSocketFactory ssf = (SSLSocketFactory) (settings == null ? null : settings.get(HttpHeaders.SSL_SOCKET_FACTORY));
        final OkHttpClient.Builder builder = new OkHttpClient.Builder();

        if (ssf != null) {
            builder.socketFactory(ssf);
        }

        this.client = builder.connectionPool(new ConnectionPool(Math.min(8, maxConnection), 5, TimeUnit.MINUTES))
                .connectTimeout(connTimeout, TimeUnit.MILLISECONDS)
                .readTimeout(readTimeout, TimeUnit.MILLISECONDS)
                .build();

        this._activeConnectionCounter = sharedActiveConnectionCounter;
    }

    protected OKHttpClient(OkHttpClient client, String url, int maxConnection) {
        this(client, url, maxConnection, null);
    }

    protected OKHttpClient(OkHttpClient client, String url, int maxConnection, Map<String, Object> settings) {
        this(client, url, maxConnection, settings, new AtomicInteger(0));
    }

    protected OKHttpClient(OkHttpClient client, String url, int maxConnection, Map<String, Object> settings,
            final AtomicInteger sharedActiveConnectionCounter) {
        super(url, maxConnection, client.connectTimeoutMillis(), client.readTimeoutMillis(), settings);
        this.client = client;
        this._activeConnectionCounter = sharedActiveConnectionCounter;
    }

    public static OKHttpClient create(String url) {
        return new OKHttpClient(url);
    }

    public static OKHttpClient create(String url, int maxConnection) {
        return new OKHttpClient(url, maxConnection);
    }

    public static OKHttpClient create(String url, long connTimeout, long readTimeout) {
        return new OKHttpClient(url, DEFAULT_MAX_CONNECTION, connTimeout, readTimeout);
    }

    public static OKHttpClient create(String url, int maxConnection, long connTimeout, long readTimeout) {
        return new OKHttpClient(url, maxConnection, connTimeout, readTimeout);
    }

    public static OKHttpClient create(String url, int maxConnection, long connTimeout, long readTimeout, Map<String, Object> settings) {
        return new OKHttpClient(url, maxConnection, connTimeout, readTimeout, settings);
    }

    public static OKHttpClient create(String url, int maxConnection, long connTimeout, long readTimeout, Map<String, Object> settings,
            final AtomicInteger sharedActiveConnectionCounter) {
        return new OKHttpClient(url, maxConnection, connTimeout, readTimeout, settings, sharedActiveConnectionCounter);
    }

    public static OKHttpClient create(OkHttpClient client, String url, int maxConnection) {
        return new OKHttpClient(client, url, maxConnection);
    }

    public static OKHttpClient create(OkHttpClient client, String url, int maxConnection, Map<String, Object> settings) {
        return new OKHttpClient(client, url, maxConnection, settings);
    }

    public static OKHttpClient create(OkHttpClient client, String url, int maxConnection, Map<String, Object> settings,
            final AtomicInteger sharedActiveConnectionCounter) {
        return new OKHttpClient(client, url, maxConnection, settings, sharedActiveConnectionCounter);
    }

    //    public static OKHttpClient of(String url) {
    //        return new OKHttpClient(url);
    //    }
    //
    //    public static OKHttpClient of(String url, long connTimeout, long readTimeout) {
    //        return new OKHttpClient(url, DEFAULT_MAX_CONNECTION, connTimeout, readTimeout);
    //    }

    @Deprecated
    public static String get(final String url, final Object parameters, final Map<String, Object> settings) {
        return get(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> T get(final Class<T> resultClass, final String url, final Object parameters, final Map<String, Object> settings) {
        return OKHttpClient.create(URLEncodedUtil.encode(url, parameters)).get(resultClass, (Object) null, settings);
    }

    @Deprecated
    public static ContinuableFuture<String> asyncGet(final String url, final Object parameters, final Map<String, Object> settings) {
        return asyncGet(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> ContinuableFuture<T> asyncGet(final Class<T> resultClass, final String url, final Object parameters, final Map<String, Object> settings) {
        final Callable<T> cmd = new Callable<T>() {
            @Override
            public T call() throws Exception {
                return get(resultClass, url, parameters, settings);
            }
        };

        return asyncExecutor.execute(cmd);
    }

    @Deprecated
    public static String delete(final String url, final Object parameters, final Map<String, Object> settings) {
        return delete(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> T delete(final Class<T> resultClass, final String url, final Object parameters, final Map<String, Object> settings) {
        return OKHttpClient.create(URLEncodedUtil.encode(url, parameters)).delete(resultClass, (Object) null, settings);
    }

    @Deprecated
    public static ContinuableFuture<String> asyncDelete(final String url, final Object parameters, final Map<String, Object> settings) {
        return asyncDelete(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> ContinuableFuture<T> asyncDelete(final Class<T> resultClass, final String url, final Object parameters,
            final Map<String, Object> settings) {
        final Callable<T> cmd = new Callable<T>() {
            @Override
            public T call() throws Exception {
                return delete(resultClass, url, parameters, settings);
            }
        };

        return asyncExecutor.execute(cmd);
    }

    @Override
    public <T> T execute(final Class<T> resultClass, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        return execute(resultClass, null, null, httpMethod, request, settings);
    }

    @Override
    public void execute(final File output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        OutputStream os = null;

        try {
            os = new FileOutputStream(output);
            execute(os, httpMethod, request, settings);
        } catch (FileNotFoundException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(os);
        }
    }

    @Override
    public void execute(final OutputStream output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        execute(null, output, null, httpMethod, request, settings);
    }

    @Override
    public void execute(final Writer output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        execute(null, null, output, httpMethod, request, settings);
    }

    private <T> T execute(final Class<T> resultClass, final OutputStream outputStream, final Writer outputWriter, final HttpMethod httpMethod,
            final Object request, final Map<String, Object> settings) {

        if (_activeConnectionCounter.get() > _maxConnection) {
            throw new AbacusException("Can not get connection, exceeded max connection number: " + _maxConnection);
        }

        final ContentFormat requestContentFormat = getContentFormat(settings);
        final String contentType = getContentType(settings);
        final String contentEncoding = getContentEncoding(settings);

        okhttp3.Request httpRequest = null;
        okhttp3.Response httpResponse = null;

        _activeConnectionCounter.incrementAndGet();

        try {
            final okhttp3.Request.Builder requestBuilder = new okhttp3.Request.Builder()
                    .url((request != null && (httpMethod.equals(HttpMethod.GET) || httpMethod.equals(HttpMethod.DELETE))) ? URLEncodedUtil.encode(_url, request)
                            : _url);

            setHeaders(requestBuilder, N.notNullOrEmpty(settings) ? settings : _settings);

            if (request != null && (httpMethod.equals(HttpMethod.POST) || httpMethod.equals(HttpMethod.PUT))) {
                MediaType mediaType = null;

                if (N.notNullOrEmpty(contentType)) {
                    mediaType = mediaTypePool.get(contentType);
                    if (mediaType == null) {
                        mediaType = MediaType.parse(contentType);

                        if (mediaType != null) {
                            mediaTypePool.put(contentType, mediaType);
                        }
                    }
                }

                RequestBody body = null;
                final Type<Object> type = N.typeOf(request.getClass());
                final ByteArrayOutputStream bos = Objectory.createByteArrayOutputStream();

                try {
                    final OutputStream os = HTTP.wrapOutputStream(bos, requestContentFormat);

                    if (type.isInputStream()) {
                        IOUtil.write(os, (InputStream) request);
                    } else if (type.isReader()) {
                        final BufferedWriter bw = Objectory.createBufferedWriter(os);

                        try {
                            IOUtil.write(bw, (Reader) request);

                            bw.flush();
                        } finally {
                            Objectory.recycle(bw);
                        }
                    } else {
                        if (type.isSerializable()) {
                            IOUtil.write(os, type.stringOf(request).getBytes());
                        } else {
                            IOUtil.write(os, HTTP.getParser(requestContentFormat).serialize(request).getBytes());
                        }
                    }

                    HTTP.flush(os);

                    body = RequestBody.create(mediaType, bos.toByteArray());
                } finally {
                    Objectory.recycle(bos);
                }

                requestBuilder.method(httpMethod.name(), body);

                if (N.notNullOrEmpty(contentType)) {
                    requestBuilder.addHeader(HttpHeaders.CONTENT_TYPE, contentType);
                }
                if (N.notNullOrEmpty(contentEncoding)) {
                    requestBuilder.addHeader(HttpHeaders.CONTENT_ENCODING, contentEncoding);
                }
            } else {
                requestBuilder.method(httpMethod.name(), null);
            }

            httpRequest = requestBuilder.build();
            httpResponse = client.newCall(httpRequest).execute();

            if (httpResponse.isSuccessful() == false) {
                throw new RuntimeException(httpResponse.code() + ": " + httpResponse.message());
            }

            final ContentFormat responseContentFormat = HTTP.getContentFormat(httpResponse.header(HttpHeaders.CONTENT_TYPE),
                    httpResponse.header(HttpHeaders.CONTENT_ENCODING));

            if (isOneWayRequest(settings)) {
                return null;
            } else {
                final InputStream is = HTTP.wrapInputStream(httpResponse.body().byteStream(), responseContentFormat);

                if (outputStream != null) {
                    IOUtil.write(outputStream, is, true);

                    return null;
                } else if (outputWriter != null) {
                    final BufferedReader br = Objectory.createBufferedReader(is);

                    try {
                        IOUtil.write(outputWriter, br, true);
                    } finally {
                        Objectory.recycle(br);
                    }

                    return null;
                } else {
                    final Type<Object> type = resultClass == null ? null : N.typeOf(resultClass);

                    if (type == null) {
                        return (T) IOUtil.readString(is);
                    } else if (type.isSerializable()) {
                        return (T) type.valueOf(IOUtil.readString(is));
                    } else {
                        return HTTP.getParser(responseContentFormat).deserialize(resultClass, is);
                    }
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            _activeConnectionCounter.decrementAndGet();

            if (httpResponse != null && httpResponse.body() != null) {
                httpResponse.body().close();
            }
        }
    }

    private void setHeaders(okhttp3.Request.Builder requestBuilder, Map<String, Object> settings) {
        if (N.notNullOrEmpty(settings)) {
            for (Map.Entry<String, Object> entry : settings.entrySet()) {
                if (HTTP.NON_HTTP_PROP_NAMES.contains(entry.getKey())) {
                    continue;
                }

                if (entry.getValue() instanceof Collection) {
                    final Iterator<Object> iter = ((Collection<Object>) entry).iterator();

                    if (iter.hasNext()) {
                        requestBuilder.header(entry.getKey(), N.stringOf(iter.next()));
                    }

                    while (iter.hasNext()) {
                        requestBuilder.addHeader(entry.getKey(), N.stringOf(iter.next()));
                    }
                } else {
                    requestBuilder.header(entry.getKey(), N.stringOf(entry.getValue()));
                }
            }
        }
    }

    public static class OKHttpRequest {
        final OKHttpClient okHttpClient;

        Map<String, Object> settings;
        HttpMethod httpMethod;
        Object request;

        OKHttpRequest(final OKHttpClient okHttpClient) {
            this.okHttpClient = okHttpClient;
        }

        OKHttpRequest(String url) {
            this(url, 0, 0);
        }

        OKHttpRequest(String url, long connTimeout, long readTimeout) {
            this.okHttpClient = OKHttpClient.create(url, 1, connTimeout, readTimeout);
        }

        public static OKHttpRequest create(final OKHttpClient okHttpClient) {
            return new OKHttpRequest(okHttpClient);
        }

        public static OKHttpRequest url(String url) {
            return new OKHttpRequest(url);
        }

        public static OKHttpRequest url(String url, long connTimeout, long readTimeout) {
            return new OKHttpRequest(url, connTimeout, readTimeout);
        }

        public OKHttpRequest header(String name, Object value) {
            checkSettings();

            settings.put(name, value);

            return this;
        }

        public OKHttpRequest headers(String name1, Object value1, String name2, Object value2) {
            checkSettings();

            settings.put(name1, value1);
            settings.put(name2, value2);

            return this;
        }

        public OKHttpRequest headers(String name1, Object value1, String name2, Object value2, String name3, Object value3) {
            checkSettings();

            settings.put(name1, value1);
            settings.put(name2, value2);
            settings.put(name3, value3);

            return this;
        }

        public OKHttpRequest headers(Map<String, Object> headers) {
            checkSettings();

            settings.putAll(headers);

            return this;
        }

        public OKHttpRequest headers(HttpHeaders headers) {
            checkSettings();

            settings.putAll(headers.map);

            return this;
        }

        //    public OkHttpRequest addHeader(String name, Object value) {
        //        checkSettings();
        //
        //        final Object existingValue = settings.get(name);
        //
        //        if (existingValue == null) {
        //            settings.put(name, value);
        //        } else if (existingValue instanceof Collection) {
        //            ((Collection<Object>) existingValue).add(value);
        //        } else {
        //            settings.put(name, N.asList(existingValue, value));
        //        }
        //
        //        return this;
        //    }
        //
        //    public OkHttpRequest removeHeader(String name) {
        //        checkSettings();
        //
        //        settings.remove(name);
        //
        //        return this;
        //    }

        public String get() {
            return get(String.class);
        }

        public <T> T get(Class<T> resultClass) {
            return get(resultClass, null);
        }

        public String get(Object query) {
            return get(String.class, query);
        }

        public <T> T get(Class<T> resultClass, Object query) {
            this.httpMethod = HttpMethod.GET;
            this.request = query;

            return execute(resultClass);
        }

        public String post(Object body) {
            return post(String.class, body);
        }

        public <T> T post(Class<T> resultClass, Object body) {
            this.httpMethod = HttpMethod.POST;
            this.request = body;

            return execute(resultClass);
        }

        public String put(Object body) {
            return put(String.class, body);
        }

        public <T> T put(Class<T> resultClass, Object body) {
            this.httpMethod = HttpMethod.PUT;
            this.request = body;

            return execute(resultClass);
        }

        public String delete() {
            return delete(String.class);
        }

        public <T> T delete(Class<T> resultClass) {
            return delete(resultClass, null);
        }

        public String delete(Object query) {
            return delete(String.class, query);
        }

        public <T> T delete(Class<T> resultClass, Object query) {
            this.httpMethod = HttpMethod.DELETE;
            this.request = query;

            return execute(resultClass);
        }

        public ContinuableFuture<String> asyncGet() {
            return asyncGet(String.class);
        }

        public <T> ContinuableFuture<T> asyncGet(Class<T> resultClass) {
            return asyncGet(resultClass, null);
        }

        public ContinuableFuture<String> asyncGet(Object query) {
            return asyncGet(String.class, query);
        }

        public <T> ContinuableFuture<T> asyncGet(Class<T> resultClass, Object query) {
            this.httpMethod = HttpMethod.GET;
            this.request = query;

            return asyncExecute(resultClass);
        }

        public ContinuableFuture<String> asyncPost(Object body) {
            return asyncPost(String.class, body);
        }

        public <T> ContinuableFuture<T> asyncPost(Class<T> resultClass, Object body) {
            this.httpMethod = HttpMethod.POST;
            this.request = body;

            return asyncExecute(resultClass);
        }

        public ContinuableFuture<String> asyncPut(Object body) {
            return asyncPut(String.class, body);
        }

        public <T> ContinuableFuture<T> asyncPut(Class<T> resultClass, Object body) {
            this.httpMethod = HttpMethod.PUT;
            this.request = body;

            return asyncExecute(resultClass);
        }

        public ContinuableFuture<String> asyncDelete() {
            return asyncDelete(String.class);
        }

        public <T> ContinuableFuture<T> asyncDelete(Class<T> resultClass) {
            return asyncDelete(resultClass, null);
        }

        public ContinuableFuture<String> asyncDelete(Object query) {
            return asyncDelete(String.class, query);
        }

        public <T> ContinuableFuture<T> asyncDelete(Class<T> resultClass, Object query) {
            this.httpMethod = HttpMethod.DELETE;
            this.request = query;

            return asyncExecute(resultClass);
        }

        protected <T> T execute(final Class<T> resultClass) {
            if (httpMethod == null) {
                throw new RuntimeException("HTTP method is not set");
            }

            switch (httpMethod) {
                case GET:
                    return okHttpClient.get(resultClass, request, settings);

                case POST:
                    return okHttpClient.post(resultClass, request, settings);

                case PUT:
                    return okHttpClient.put(resultClass, request, settings);

                case DELETE:
                    return okHttpClient.delete(resultClass, request, settings);

                default:
                    throw new RuntimeException("Unsupported HTTP method: " + httpMethod);
            }
        }

        protected <T> ContinuableFuture<T> asyncExecute(final Class<T> resultClass) {
            if (httpMethod == null) {
                throw new RuntimeException("HTTP method is not set");
            }

            switch (httpMethod) {
                case GET:
                    return okHttpClient.asyncGet(resultClass, request, settings);

                case POST:
                    return okHttpClient.asyncPost(resultClass, request, settings);

                case PUT:
                    return okHttpClient.asyncPut(resultClass, request, settings);

                case DELETE:
                    return okHttpClient.asyncDelete(resultClass, request, settings);

                default:
                    throw new RuntimeException("Unsupported HTTP method: " + httpMethod);
            }
        }

        protected void checkSettings() {
            if (settings == null) {
                settings = new HashMap<>();
            }
        }
    }
}
