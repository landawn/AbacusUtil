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

import java.io.Closeable;
import java.io.File;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.SSLSocketFactory;

import com.landawn.abacus.util.AsyncExecutor;
import com.landawn.abacus.util.ContinuableFuture;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Properties;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class AbstractHttpClient implements Closeable {
    // ...
    public static final int DEFAULT_MAX_CONNECTION = 16;
    /**
     * Unit is milliseconds
     */
    public static final int DEFAULT_CONNECTION_TIMEOUT = 8000;
    public static final int DEFAULT_READ_TIMEOUT = 16000;

    // for static asynchronization operation.
    protected static final AsyncExecutor asyncExecutor = new AsyncExecutor(256, 300L, TimeUnit.SECONDS);

    // ...
    protected final String _url;
    protected final int _maxConnection;
    protected final long _connTimeout;
    protected final long _readTimeout;
    protected final Properties<String, Object> _settings = new Properties<String, Object>();

    protected final AsyncExecutor _asyncExecutor;

    protected AbstractHttpClient(String url) {
        this(url, DEFAULT_MAX_CONNECTION, DEFAULT_CONNECTION_TIMEOUT, DEFAULT_READ_TIMEOUT);
    }

    protected AbstractHttpClient(String url, int maxConnection) {
        this(url, maxConnection, DEFAULT_CONNECTION_TIMEOUT, DEFAULT_READ_TIMEOUT);
    }

    protected AbstractHttpClient(String url, int maxConnection, long connTimeout, long readTimeout) {
        this(url, maxConnection, connTimeout, readTimeout, null);
    }

    protected AbstractHttpClient(String url, int maxConnection, long connTimeout, long readTimeout, Map<String, Object> settings) {
        if (N.isNullOrEmpty(url)) {
            throw new IllegalArgumentException("url can't be null or empty");
        }

        if ((maxConnection < 0) || (connTimeout < 0) || (readTimeout < 0)) {
            throw new IllegalArgumentException(
                    "maxConnection, connTimeout or readTimeout can't be less than 0: " + maxConnection + ", " + connTimeout + ", " + readTimeout);
        }

        this._url = url;
        this._maxConnection = (maxConnection == 0) ? DEFAULT_MAX_CONNECTION : maxConnection;
        this._connTimeout = (connTimeout == 0) ? DEFAULT_CONNECTION_TIMEOUT : connTimeout;
        this._readTimeout = (readTimeout == 0) ? DEFAULT_READ_TIMEOUT : readTimeout;

        if (N.notNullOrEmpty(settings)) {
            this._settings.putAll(settings);
        }

        _asyncExecutor = new AsyncExecutor(this._maxConnection, 300L, TimeUnit.SECONDS);
    }

    public String url() {
        return _url;
    }

    public String get() {
        return get(String.class);
    }

    public String get(final Object parameters) {
        return get(String.class, parameters);
    }

    public String get(final Object parameters, final Map<String, Object> settings) {
        return get(String.class, parameters, settings);
    }

    public <T> T get(final Class<T> resultClass) {
        return get(resultClass, (Object) null);
    }

    public <T> T get(final Class<T> resultClass, final Object parameters) {
        return get(resultClass, parameters, _settings);
    }

    public <T> T get(final Class<T> resultClass, final Object parameters, final Map<String, Object> settings) {
        return execute(resultClass, HttpMethod.GET, parameters, settings);
    }

    public ContinuableFuture<String> asyncGet() {
        return asyncGet(String.class);
    }

    public ContinuableFuture<String> asyncGet(final Object parameters) {
        return asyncGet(String.class, parameters);
    }

    public ContinuableFuture<String> asyncGet(final Object parameters, final Map<String, Object> settings) {
        return asyncGet(String.class, parameters, settings);
    }

    public <T> ContinuableFuture<T> asyncGet(final Class<T> resultClass) {
        return asyncGet(resultClass, (Object) null);
    }

    public <T> ContinuableFuture<T> asyncGet(final Class<T> resultClass, final Object parameters) {
        return asyncGet(resultClass, parameters, _settings);
    }

    public <T> ContinuableFuture<T> asyncGet(final Class<T> resultClass, final Object parameters, final Map<String, Object> settings) {
        return asyncExecute(resultClass, HttpMethod.GET, parameters, settings);
    }

    public String delete() {
        return delete(String.class);
    }

    public String delete(final Object parameters) {
        return delete(String.class, parameters);
    }

    public String delete(final Object parameters, final Map<String, Object> settings) {
        return delete(String.class, parameters, settings);
    }

    public <T> T delete(final Class<T> resultClass) {
        return delete(resultClass, (Object) null);
    }

    public <T> T delete(final Class<T> resultClass, final Object parameters) {
        return delete(resultClass, parameters, _settings);
    }

    public <T> T delete(final Class<T> resultClass, final Object parameters, final Map<String, Object> settings) {
        return execute(resultClass, HttpMethod.DELETE, parameters, settings);
    }

    public ContinuableFuture<String> asyncDelete() {
        return asyncDelete(String.class);
    }

    public ContinuableFuture<String> asyncDelete(final Object parameters) {
        return asyncDelete(String.class, parameters);
    }

    public ContinuableFuture<String> asyncDelete(final Object parameters, final Map<String, Object> settings) {
        return asyncDelete(String.class, parameters, settings);
    }

    public <T> ContinuableFuture<T> asyncDelete(final Class<T> resultClass) {
        return asyncDelete(resultClass, (Object) null);
    }

    public <T> ContinuableFuture<T> asyncDelete(final Class<T> resultClass, final Object parameters) {
        return asyncDelete(resultClass, parameters, _settings);
    }

    public <T> ContinuableFuture<T> asyncDelete(final Class<T> resultClass, final Object parameters, final Map<String, Object> settings) {
        return asyncExecute(resultClass, HttpMethod.DELETE, parameters, settings);
    }

    public String post(final Object request) {
        return post(String.class, request);
    }

    public String post(final Object request, final Map<String, Object> settings) {
        return post(String.class, request, settings);
    }

    public <T> T post(final Class<T> resultClass, final Object request) {
        return post(resultClass, request, _settings);
    }

    public <T> T post(final Class<T> resultClass, final Object request, final Map<String, Object> settings) {
        return execute(resultClass, HttpMethod.POST, request, settings);
    }

    public ContinuableFuture<String> asyncPost(final Object request) {
        return asyncPost(String.class, request);
    }

    public ContinuableFuture<String> asyncPost(final Object request, final Map<String, Object> settings) {
        return asyncPost(String.class, request, settings);
    }

    public <T> ContinuableFuture<T> asyncPost(final Class<T> resultClass, final Object request) {
        return asyncPost(resultClass, request, _settings);
    }

    public <T> ContinuableFuture<T> asyncPost(final Class<T> resultClass, final Object request, final Map<String, Object> settings) {
        return asyncExecute(resultClass, HttpMethod.POST, request, settings);
    }

    public String put(final Object request) {
        return put(String.class, request);
    }

    public String put(final Object request, final Map<String, Object> settings) {
        return put(String.class, request, settings);
    }

    public <T> T put(final Class<T> resultClass, final Object request) {
        return put(resultClass, request, _settings);
    }

    public <T> T put(final Class<T> resultClass, final Object request, final Map<String, Object> settings) {
        return execute(resultClass, HttpMethod.PUT, request, settings);
    }

    public ContinuableFuture<String> asyncPut(final Object request) {
        return asyncPut(String.class, request);
    }

    public ContinuableFuture<String> asyncPut(final Object request, final Map<String, Object> settings) {
        return asyncPut(String.class, request, settings);
    }

    public <T> ContinuableFuture<T> asyncPut(final Class<T> resultClass, final Object request) {
        return asyncPut(resultClass, request, _settings);
    }

    public <T> ContinuableFuture<T> asyncPut(final Class<T> resultClass, final Object request, final Map<String, Object> settings) {
        return asyncExecute(resultClass, HttpMethod.PUT, request, settings);
    }

    public String execute(final HttpMethod httpMethod, final Object request) {
        return execute(String.class, httpMethod, request);
    }

    public String execute(final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        return execute(String.class, httpMethod, request, settings);
    }

    public <T> T execute(final Class<T> resultClass, final HttpMethod httpMethod, final Object request) {
        return execute(resultClass, httpMethod, request, _settings);
    }

    /**
     * Write the specified <code>request</code> to request body.
     * 
     * @param resultClass
     * @param methodName
     * @param request can be String/Map/Entity/InputStream/Reader...
     * @param settings
     * @return
     */
    public abstract <T> T execute(final Class<T> resultClass, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings);

    /**
     * 
     * @param output write the InputStream in the response to this specified File.
     * @param httpMethod
     * @param request
     * @param settings
     */
    public abstract void execute(final File output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings);

    /**
     * 
     * @param output write the InputStream in the response to this specified OutputStream.
     * @param httpMethod
     * @param request
     * @param settings
     */
    public abstract void execute(final OutputStream output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings);

    /**
     * 
     * @param output write the InputStream in the response to this specified Writer.
     * @param httpMethod
     * @param request
     * @param settings
     */
    public abstract void execute(final Writer output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings);

    public ContinuableFuture<String> asyncExecute(final HttpMethod httpMethod, final Object request) {
        return asyncExecute(String.class, httpMethod, request);
    }

    public ContinuableFuture<String> asyncExecute(final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        return asyncExecute(String.class, httpMethod, request, settings);
    }

    public <T> ContinuableFuture<T> asyncExecute(final Class<T> resultClass, final HttpMethod httpMethod, final Object request) {
        return asyncExecute(resultClass, httpMethod, request, _settings);
    }

    public <T> ContinuableFuture<T> asyncExecute(final Class<T> resultClass, final HttpMethod httpMethod, final Object request,
            final Map<String, Object> settings) {
        final Callable<T> cmd = new Callable<T>() {
            @Override
            public T call() throws Exception {
                return execute(resultClass, httpMethod, request, settings);
            }
        };

        return _asyncExecutor.execute(cmd);
    }

    public ContinuableFuture<Void> asyncExecute(final File output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        final Callable<Void> cmd = new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                execute(output, httpMethod, request, settings);

                return null;
            }
        };

        return _asyncExecutor.execute(cmd);
    }

    public ContinuableFuture<Void> asyncExecute(final OutputStream output, final HttpMethod httpMethod, final Object request,
            final Map<String, Object> settings) {
        final Callable<Void> cmd = new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                execute(output, httpMethod, request, settings);

                return null;
            }
        };

        return _asyncExecutor.execute(cmd);
    }

    public ContinuableFuture<Void> asyncExecute(final Writer output, final HttpMethod httpMethod, final Object request, final Map<String, Object> settings) {
        final Callable<Void> cmd = new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                execute(output, httpMethod, request, settings);

                return null;
            }
        };

        return _asyncExecutor.execute(cmd);
    }

    @Override
    public void close() {
        // do nothing.
    }

    protected boolean isOneWayRequest(Map<String, Object> settings) {
        Boolean b = null;

        if (N.notNullOrEmpty(settings)) {
            b = (Boolean) settings.get(HttpHeaders.IS_ONE_WAY_REQUEST);
        }

        if (b == null) {
            b = (Boolean) _settings.get(HttpHeaders.IS_ONE_WAY_REQUEST);
        }

        if (b == null) {
            return false;
        }

        return b.booleanValue();
    }

    protected ContentFormat getContentFormat(Map<String, Object> settings) {
        ContentFormat contentFormat = null;

        if (N.notNullOrEmpty(settings)) {
            contentFormat = (ContentFormat) settings.get(HttpHeaders.CONTENT_FORMAT);

            if (contentFormat == null) {
                String contentType = (String) settings.get(HttpHeaders.CONTENT_TYPE);
                String contentEncoding = (String) settings.get(HttpHeaders.CONTENT_ENCODING);

                contentFormat = HTTP.getContentFormat(contentType, contentEncoding);
            }
        }

        if (contentFormat == null && N.notNullOrEmpty(_settings)) {
            contentFormat = (ContentFormat) _settings.get(HttpHeaders.CONTENT_FORMAT);

            if (contentFormat == null) {
                String contentType = (String) _settings.get(HttpHeaders.CONTENT_TYPE);
                String contentEncoding = (String) _settings.get(HttpHeaders.CONTENT_ENCODING);

                contentFormat = HTTP.getContentFormat(contentType, contentEncoding);
            }
        }

        return contentFormat;
    }

    protected String getContentType(Map<String, Object> settings) {
        String contentType = null;

        if (N.notNullOrEmpty(settings)) {
            contentType = (String) settings.get(HttpHeaders.CONTENT_TYPE);

            if (N.isNullOrEmpty(contentType)) {
                ContentFormat contentFormat = (ContentFormat) settings.get(HttpHeaders.CONTENT_FORMAT);

                if (contentFormat != null) {
                    contentType = HTTP.getContentType(contentFormat);
                }
            }
        }

        if (N.isNullOrEmpty(contentType) && N.notNullOrEmpty(_settings)) {
            contentType = (String) _settings.get(HttpHeaders.CONTENT_TYPE);

            if (N.isNullOrEmpty(contentType)) {
                ContentFormat contentFormat = (ContentFormat) _settings.get(HttpHeaders.CONTENT_FORMAT);

                if (contentFormat != null) {
                    contentType = HTTP.getContentType(contentFormat);
                }
            }
        }

        return contentType;
    }

    protected String getContentEncoding(Map<String, Object> settings) {
        String contentEncoding = null;

        if (N.notNullOrEmpty(settings)) {
            contentEncoding = (String) settings.get(HttpHeaders.CONTENT_ENCODING);

            if (N.isNullOrEmpty(contentEncoding)) {
                ContentFormat contentFormat = (ContentFormat) settings.get(HttpHeaders.CONTENT_FORMAT);

                if (contentFormat != null) {
                    contentEncoding = HTTP.getContentEncoding(contentFormat);
                }
            }
        }

        if (N.isNullOrEmpty(contentEncoding) && N.notNullOrEmpty(_settings)) {
            contentEncoding = (String) _settings.get(HttpHeaders.CONTENT_ENCODING);

            if (N.isNullOrEmpty(contentEncoding)) {
                ContentFormat contentFormat = (ContentFormat) _settings.get(HttpHeaders.CONTENT_FORMAT);

                if (contentFormat != null) {
                    contentEncoding = HTTP.getContentEncoding(contentFormat);
                }
            }
        }

        return contentEncoding;
    }

    public static final class HttpSettings extends Properties<String, Object> {
        public HttpSettings() {
            super();
        }

        public static HttpSettings create() {
            return new HttpSettings();
        }

        //        public static HttpSettings valueOf(Object... a) {
        //            final HttpSettings instance = new HttpSettings();
        //
        //            instance.putAll(N.asProps(a));
        //
        //            return instance;
        //        }

        public int getConnectionTimeout() {
            return get(int.class, HttpHeaders.CONNECTION_TIMEOUT);
        }

        public HttpSettings setConnectionTimeout(long connTimeout) {
            put(HttpHeaders.CONNECTION_TIMEOUT, connTimeout);

            return this;
        }

        public int getReadTimeout() {
            return get(int.class, HttpHeaders.READ_TIMEOUT);
        }

        public HttpSettings setReadTimeout(long readTimeout) {
            put(HttpHeaders.READ_TIMEOUT, readTimeout);

            return this;
        }

        public ContentFormat getContentFormat() {
            return get(ContentFormat.class, HttpHeaders.CONTENT_FORMAT);
        }

        public HttpSettings setContentFormat(ContentFormat contentFormat) {
            if (containsKey(HttpHeaders.CONTENT_TYPE)) {
                throw new IllegalArgumentException("The parameter 'content-type' has already been set");
            }

            put(HttpHeaders.CONTENT_FORMAT, contentFormat);

            return this;
        }

        public String getAcceptCharset() {
            return get(String.class, HttpHeaders.ACCEPT_CHARSET);
        }

        public HttpSettings setAcceptCharset(String acceptCharset) {
            put(HttpHeaders.ACCEPT_CHARSET, acceptCharset);

            return this;
        }

        public String getAcceptEncoding() {
            return get(String.class, HttpHeaders.ACCEPT_ENCODING);
        }

        public HttpSettings setAcceptEncoding(String acceptEncoding) {
            put(HttpHeaders.ACCEPT_ENCODING, acceptEncoding);

            return this;
        }

        public String getAcceptLanguage() {
            return get(String.class, HttpHeaders.ACCEPT_LANGUAGE);
        }

        public HttpSettings setAcceptLanguage(String acceptLanguage) {
            put(HttpHeaders.ACCEPT_LANGUAGE, acceptLanguage);

            return this;
        }

        public String getAcceptRanges() {
            return get(String.class, HttpHeaders.ACCEPT_RANGES);
        }

        public HttpSettings setAcceptRanges(String acceptRanges) {
            put(HttpHeaders.ACCEPT_RANGES, acceptRanges);

            return this;
        }

        public String getContentType() {
            return get(String.class, HttpHeaders.CONTENT_TYPE);
        }

        public HttpSettings setContentType(String contentType) {
            if (containsKey(HttpHeaders.CONTENT_FORMAT)) {
                throw new IllegalArgumentException("The parameter 'contentFormat' has already been set");
            }

            put(HttpHeaders.CONTENT_TYPE, contentType);

            return this;
        }

        public String getContentEncoding() {
            return get(String.class, HttpHeaders.CONTENT_ENCODING);
        }

        public HttpSettings setContentEncoding(String acceptEncoding) {
            put(HttpHeaders.CONTENT_ENCODING, acceptEncoding);

            return this;
        }

        public String getContentLanguage() {
            return get(String.class, HttpHeaders.CONTENT_LANGUAGE);
        }

        public HttpSettings setContentLanguage(String acceptLanguage) {
            put(HttpHeaders.CONTENT_LANGUAGE, acceptLanguage);

            return this;
        }

        public String getContentLength() {
            return get(String.class, HttpHeaders.CONTENT_LENGTH);
        }

        public HttpSettings setContentLength(String contentLength) {
            put(HttpHeaders.CONTENT_LENGTH, contentLength);

            return this;
        }

        public String getUserAgent() {
            return get(String.class, HttpHeaders.USER_AGENT);
        }

        public HttpSettings setUserAgent(String userAgent) {
            put(HttpHeaders.USER_AGENT, userAgent);

            return this;
        }

        @Override
        public HttpSettings set(String name, Object value) {
            super.set(name, value);

            return this;
        }

        public HttpSettings setSSLSocketFactory(final SSLSocketFactory sslSocketFactory) {
            put(HttpHeaders.SSL_SOCKET_FACTORY, sslSocketFactory);

            return this;
        }
    }
}
