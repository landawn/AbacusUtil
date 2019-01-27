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

package com.landawn.abacus.http;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class HttpHeaders implements Map<String, Object> {
    public static final String SSL_SOCKET_FACTORY = "sslSocketFactory";
    public static final String CONNECTION_TIMEOUT = "connectionTimeout";
    public static final String READ_TIMEOUT = "readTimeout";
    public static final String CONTENT_FORMAT = "contentFormat";
    public static final String IS_ONE_WAY_REQUEST = "isOneWayRequest";
    /**
     * Default value is false.
     */
    public static final String USE_CACHES = "useCaches";
    // ...
    public static final String ACCEPT = "Accept";
    public static final String ACCEPT_CHARSET = "Accept-Charset";
    public static final String ACCEPT_ENCODING = "Accept-Encoding";
    public static final String ACCEPT_LANGUAGE = "Accept-Language";
    public static final String ACCEPT_RANGES = "Accept-Ranges";
    public static final String AGE = "Age";
    public static final String ALLOW = "Allow";
    public static final String CHARSET = "charset";
    // ...
    public static final String CONTENT_TYPE = "Content-Type";
    public static final String CONTENT_ENCODING = "Content-Encoding";
    public static final String CONTENT_LENGTH = "Content-Length";
    public static final String CONTENT_LANGUAGE = "Content-Language";
    public static final String CONTENT_LOCATION = "Content-Location";
    public static final String CONTENT_RANGE = "Content-Range";
    public static final String CONTENT_MD5 = "Content-MD5";
    public static final String LAST_MODIFIED = "Last-Modified";
    public static final String DATE = "Date";
    public static final String EXPIRES = "Expires";
    public static final String RANGE = "Range";
    public static final String AUTHORIZATION = "Authorization";
    public static final String PROXY_AUTHORIZATION = "Proxy-Authorization";
    public static final String PROXY_AUTHENTICATE = "Proxy-Authenticate";
    public static final String WWW_AUTHENTICATE = " WWW-Authenticate";
    public static final String USER_AGENT = "User-Agent";
    public static final String TRANSFER_ENCODING = "Transfer-Encoding";
    public static final String UPGRADE = "Upgrade";
    public static final String FROM = "From";
    public static final String HOST = "Host";
    public static final String SERVER = "Server";
    public static final String CACHE_CONTROL = "Cache-Control";
    // application/content type.
    /**
     * The default HTML form content type.
     */
    public static final String APPLICATION_URL_ENCODED = "application/x-www-form-urlencoded";
    public static final String APPLICATION_XML = "application/xml";
    public static final String APPLICATION_JSON = "application/json";
    public static final String APPLICATION_KRYO = "application/kryo";
    public static final String TEXT_HTML = "text/html";
    public static final String TEXT_JSON = "text/json";
    public static final String TEXT_XML = "text/xml";
    public static final String IMAGE_GIF = "image/gif";
    public static final String IMAGE_JPG = "image/jpg";
    public static final String UTF_8 = "UTF-8";

    final Map<String, Object> map;

    public HttpHeaders() {
        this(new HashMap<String, Object>());
    }

    private HttpHeaders(Map<String, Object> map) {
        this.map = map;
    }

    public static HttpHeaders create() {
        return new HttpHeaders();
    }

    @SafeVarargs
    public static HttpHeaders of(Object... values) {
        return new HttpHeaders(N.asProps(values));
    }

    public HttpHeaders setContentType(String contentType) {
        map.put(HttpHeaders.CONTENT_TYPE, contentType);

        return this;
    }

    public HttpHeaders setContentEncoding(String contentEncoding) {
        map.put(HttpHeaders.CONTENT_ENCODING, contentEncoding);

        return this;
    }

    public HttpHeaders setContentLength(long contentLength) {
        map.put(HttpHeaders.CONTENT_LENGTH, contentLength);

        return this;
    }

    public HttpHeaders set(String name, Object value) {
        map.put(name, value);

        return this;
    }

    @Override
    public Object get(Object key) {
        return map.get(key);
    }

    @Override
    public Object put(String key, Object value) {
        return map.put(key, value);
    }

    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        map.putAll(m);
    }

    @Override
    public Object remove(Object key) {
        return map.remove(key);
    }

    @Override
    public boolean containsKey(Object key) {
        return map.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return map.containsValue(value);
    }

    @Override
    public Set<String> keySet() {
        return map.keySet();
    }

    @Override
    public Collection<Object> values() {
        return map.values();
    }

    @Override
    public Set<java.util.Map.Entry<String, Object>> entrySet() {
        return map.entrySet();
    }

    @Override
    public int size() {
        return map.size();
    }

    @Override
    public boolean isEmpty() {
        return map.isEmpty();
    }

    @Override
    public void clear() {
        map.clear();
    }
}
