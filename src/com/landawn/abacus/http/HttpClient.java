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
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.BufferedReader;
import com.landawn.abacus.util.BufferedWriter;
import com.landawn.abacus.util.ContinuableFuture;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Objectory;
import com.landawn.abacus.util.URLEncodedUtil;

/**
 * Any header can be set into the parameter <code>settings</code>
 * 
 * <br>HttpClient is thread safe.</br>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class HttpClient extends AbstractHttpClient {
    static final Logger logger = LoggerFactory.getLogger(HttpClient.class);

    static {
        if (IOUtil.IS_PLATFORM_ANDROID) {
            // ignore
        } else {
            System.setProperty("http.keepAlive", "true");
            System.setProperty("http.maxConnections", "32");
        }
    }

    protected final URL _netURL;

    protected final AtomicInteger _activeConnectionCounter;

    protected HttpClient(String url) {
        this(url, DEFAULT_MAX_CONNECTION);
    }

    protected HttpClient(String url, int maxConnection) {
        this(url, maxConnection, DEFAULT_CONNECTION_TIMEOUT, DEFAULT_READ_TIMEOUT);
    }

    protected HttpClient(String url, int maxConnection, long connTimeout, long readTimeout) {
        this(url, maxConnection, connTimeout, readTimeout, null);
    }

    protected HttpClient(String url, int maxConnection, long connTimeout, long readTimeout, HttpSettings settings) {
        this(url, maxConnection, connTimeout, readTimeout, settings, new AtomicInteger(0));
    }

    protected HttpClient(String url, int maxConnection, long connTimeout, long readTimeout, HttpSettings settings,
            final AtomicInteger sharedActiveConnectionCounter) {
        super(url, maxConnection, connTimeout, readTimeout, settings);

        try {
            this._netURL = new URL(url);
        } catch (MalformedURLException e) {
            throw N.toRuntimeException(e);
        }

        this._activeConnectionCounter = sharedActiveConnectionCounter;
    }

    public static HttpClient create(String url) {
        return new HttpClient(url);
    }

    public static HttpClient create(String url, int maxConnection) {
        return new HttpClient(url, maxConnection);
    }

    public static HttpClient create(String url, long connTimeout, long readTimeout) {
        return new HttpClient(url, DEFAULT_MAX_CONNECTION, connTimeout, readTimeout);
    }

    public static HttpClient create(String url, int maxConnection, long connTimeout, long readTimeout) {
        return new HttpClient(url, maxConnection, connTimeout, readTimeout);
    }

    public static HttpClient create(String url, int maxConnection, long connTimeout, long readTimeout, HttpSettings settings) {
        return new HttpClient(url, maxConnection, connTimeout, readTimeout, settings);
    }

    public static HttpClient create(String url, int maxConnection, long connTimeout, long readTimeout, HttpSettings settings,
            final AtomicInteger sharedActiveConnectionCounter) {
        return new HttpClient(url, maxConnection, connTimeout, readTimeout, settings, sharedActiveConnectionCounter);
    }

    //    public static HttpClient of(String url) {
    //        return new HttpClient(url);
    //    }
    //
    //    public static HttpClient of(String url, long connTimeout, long readTimeout) {
    //        return new HttpClient(url, DEFAULT_MAX_CONNECTION, connTimeout, readTimeout);
    //    }

    @Deprecated
    public static String get(final String url, final Object parameters, final HttpSettings settings) {
        return get(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> T get(final Class<T> resultClass, final String url, final Object parameters, final HttpSettings settings) {
        return HttpClient.create(URLEncodedUtil.encode(url, parameters)).get(resultClass, (Object) null, settings);
    }

    @Deprecated
    public static ContinuableFuture<String> asyncGet(final String url, final Object parameters, final HttpSettings settings) {
        return asyncGet(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> ContinuableFuture<T> asyncGet(final Class<T> resultClass, final String url, final Object parameters, final HttpSettings settings) {
        final Callable<T> cmd = new Callable<T>() {
            @Override
            public T call() throws Exception {
                return get(resultClass, url, parameters, settings);
            }
        };

        return asyncExecutor.execute(cmd);
    }

    @Deprecated
    public static String delete(final String url, final Object parameters, final HttpSettings settings) {
        return delete(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> T delete(final Class<T> resultClass, final String url, final Object parameters, final HttpSettings settings) {
        return HttpClient.create(URLEncodedUtil.encode(url, parameters)).delete(resultClass, (Object) null, settings);
    }

    @Deprecated
    public static ContinuableFuture<String> asyncDelete(final String url, final Object parameters, final HttpSettings settings) {
        return asyncDelete(String.class, url, parameters, settings);
    }

    @Deprecated
    public static <T> ContinuableFuture<T> asyncDelete(final Class<T> resultClass, final String url, final Object parameters, final HttpSettings settings) {
        final Callable<T> cmd = new Callable<T>() {
            @Override
            public T call() throws Exception {
                return delete(resultClass, url, parameters, settings);
            }
        };

        return asyncExecutor.execute(cmd);
    }

    @Override
    public <T> T execute(final Class<T> resultClass, final HttpMethod httpMethod, final Object request, final HttpSettings settings) {
        return execute(resultClass, null, null, httpMethod, request, settings);
    }

    @Override
    public void execute(final File output, final HttpMethod httpMethod, final Object request, final HttpSettings settings) {
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
    public void execute(final OutputStream output, final HttpMethod httpMethod, final Object request, final HttpSettings settings) {
        execute(null, output, null, httpMethod, request, settings);
    }

    @Override
    public void execute(final Writer output, final HttpMethod httpMethod, final Object request, final HttpSettings settings) {
        execute(null, null, output, httpMethod, request, settings);
    }

    private <T> T execute(final Class<T> resultClass, final OutputStream outputStream, final Writer outputWriter, final HttpMethod httpMethod,
            final Object request, final HttpSettings settings) {
        final ContentFormat requestContentFormat = getContentFormat(settings);
        final HttpURLConnection connection = openConnection(httpMethod, request, request != null, settings);
        InputStream is = null;
        OutputStream os = null;

        try {
            if (request != null && (httpMethod.equals(HttpMethod.POST) || httpMethod.equals(HttpMethod.PUT))) {
                os = getOutputStream(connection, requestContentFormat, settings);

                Type<Object> type = N.typeOf(request.getClass());

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
                        os.write(type.stringOf(request).getBytes());
                    } else {
                        HTTP.getParser(requestContentFormat).serialize(os, request);
                    }
                }

                flush(os);
            }

            final int code = connection.getResponseCode();

            if ((code < 200 || code >= 300) && (resultClass == null || !resultClass.equals(HttpResponse.class))) {
                throw new RuntimeException(code + ": " + connection.getResponseMessage());
            }

            final ContentFormat responseContentFormat = getContentFormat(connection);
            is = getInputStream(connection, responseContentFormat);

            if (isOneWayRequest(settings)) {
                return null;
            } else {
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
                    if (resultClass != null && resultClass.equals(HttpResponse.class)) {
                        return (T) new HttpResponse(code, connection.getResponseMessage(), connection.getHeaderFields(), IOUtil.readString(is),
                                responseContentFormat);
                    } else {
                        final Type<Object> type = resultClass == null ? null : N.typeOf(resultClass);

                        if (type == null) {
                            return (T) IOUtil.readString(is);
                        } else if (byte[].class.equals(resultClass)) {
                            return (T) IOUtil.readBytes(is);
                        } else if (type.isSerializable()) {
                            return (T) type.valueOf(IOUtil.readString(is));
                        } else {
                            return HTTP.getParser(responseContentFormat).deserialize(resultClass, is);
                        }
                    }
                }
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            close(os, is, connection);
        }
    }

    public OutputStream getOutputStream(final HttpURLConnection connection, final ContentFormat contentFormat) throws IOException {
        return getOutputStream(connection, contentFormat, null);
    }

    public OutputStream getOutputStream(final HttpURLConnection connection, final HttpSettings settings) throws IOException {
        return getOutputStream(connection, null, settings);
    }

    private OutputStream getOutputStream(final HttpURLConnection connection, final ContentFormat contentFormat, final HttpSettings settings)
            throws IOException {
        String contentType = getContentType(settings);

        if (N.isNullOrEmpty(contentType) && contentFormat != null) {
            contentType = HTTP.getContentType(contentFormat);
        }

        if (N.notNullOrEmpty(contentType)) {
            connection.setRequestProperty(HttpHeaders.Names.CONTENT_TYPE, contentType);
        }

        String contentEncoding = getContentEncoding(settings);

        if (N.isNullOrEmpty(contentEncoding) && contentFormat != null) {
            contentEncoding = HTTP.getContentEncoding(contentFormat);
        }

        if (N.notNullOrEmpty(contentEncoding)) {
            connection.setRequestProperty(HttpHeaders.Names.CONTENT_ENCODING, contentEncoding);
        }

        return HTTP.wrapOutputStream(connection.getOutputStream(), contentFormat == null ? getContentFormat(settings) : contentFormat);
    }

    public InputStream getInputStream(final HttpURLConnection connection) throws IOException {
        return getInputStream(connection, getContentFormat(connection));
    }

    public InputStream getInputStream(final HttpURLConnection connection, ContentFormat contentFormat) throws IOException {
        return HTTP.wrapInputStream(connection.getInputStream(), contentFormat);
    }

    ContentFormat getContentFormat(final HttpURLConnection connection) {
        return HTTP.getContentFormat(connection.getHeaderField(HttpHeaders.Names.CONTENT_TYPE), connection.getHeaderField(HttpHeaders.Names.CONTENT_ENCODING));
    }

    public HttpURLConnection openConnection(HttpMethod httpMethod) {
        return openConnection(httpMethod, HttpMethod.POST.equals(httpMethod) || HttpMethod.PUT.equals(httpMethod));
    }

    public HttpURLConnection openConnection(HttpMethod httpMethod, boolean doOutput) {
        return openConnection(httpMethod, doOutput, _settings);
    }

    public HttpURLConnection openConnection(HttpMethod httpMethod, HttpSettings settings) {
        return openConnection(httpMethod, HttpMethod.POST.equals(httpMethod) || HttpMethod.PUT.equals(httpMethod), settings);
    }

    public HttpURLConnection openConnection(HttpMethod httpMethod, boolean doOutput, HttpSettings settings) {
        return openConnection(httpMethod, null, doOutput, settings);
    }

    public HttpURLConnection openConnection(HttpMethod httpMethod, final Object request, boolean doOutput, HttpSettings settings) {
        HttpURLConnection connection = null;

        try {
            synchronized (_netURL) {
                if (_activeConnectionCounter.get() > _maxConnection) {
                    throw new AbacusException("Can not get connection, exceeded max connection number: " + _maxConnection);
                }

                if (request != null && (httpMethod.equals(HttpMethod.GET) || httpMethod.equals(HttpMethod.DELETE))) {
                    connection = (HttpURLConnection) new URL(URLEncodedUtil.encode(_url, request)).openConnection();
                } else {
                    connection = (HttpURLConnection) _netURL.openConnection();
                }
            }

            if (connection instanceof HttpsURLConnection) {
                SSLSocketFactory ssf = (settings == null ? _settings : settings).getSSLSocketFactory();

                if (ssf != null) {
                    ((HttpsURLConnection) connection).setSSLSocketFactory(ssf);
                }
            }

            int connTimeout = _connTimeout > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) _connTimeout;

            if (settings != null) {
                connTimeout = settings.getConnectionTimeout();
            }

            if (connTimeout > 0) {
                connection.setConnectTimeout(connTimeout);
            }

            int readTimeout = _readTimeout > Integer.MAX_VALUE ? Integer.MAX_VALUE : (int) _readTimeout;

            if (settings != null) {
                readTimeout = settings.getReadTimeout();
            }

            if (readTimeout > 0) {
                connection.setReadTimeout(readTimeout);
            }

            if (settings != null) {
                connection.setDoInput(settings.doInput());
                connection.setDoOutput(settings.doOutput());
            }

            connection.setUseCaches(false);

            setHttpProperties(connection, settings == null || settings.headers().isEmpty() ? _settings : settings);

            if (isOneWayRequest(settings)) {
                connection.setDoInput(false);
            }

            connection.setDoOutput(doOutput);
            connection.setRequestMethod(httpMethod.name());

            _activeConnectionCounter.incrementAndGet();

            return connection;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    void close(OutputStream os, InputStream is, HttpURLConnection connection) {
        try {
            IOUtil.closeQuietly(os);
            IOUtil.closeQuietly(is);
        } finally {
            _activeConnectionCounter.decrementAndGet();
        }

        // connection.disconnect();
    }

    protected void flush(OutputStream os) throws IOException {
        HTTP.flush(os);
    }

    private void setHttpProperties(HttpURLConnection connection, HttpSettings settings) {
        final HttpHeaders headers = settings.headers();

        if (headers != null) {
            Object headerValue = null;

            for (String headerName : headers.headerNameSet()) {
                if (HTTP.NON_HTTP_PROP_NAMES.contains(headerName)) {
                    continue;
                }

                headerValue = headers.get(headerName);

                if (headerName.equals(HTTP.USE_CACHES)) {
                    if (Boolean.valueOf((headerValue.toString())) == true) {
                        connection.setUseCaches(true);
                    }
                } else {
                    if (headerValue instanceof Collection) {
                        final Iterator<Object> iter = ((Collection<Object>) headerValue).iterator();

                        if (iter.hasNext()) {
                            connection.setRequestProperty(headerName, N.stringOf(iter.next()));
                        }

                        while (iter.hasNext()) {
                            connection.addRequestProperty(headerName, N.stringOf(iter.next()));
                        }
                    } else {
                        connection.setRequestProperty(headerName, N.stringOf(headerValue));
                    }
                }
            }
        }
    }
}
