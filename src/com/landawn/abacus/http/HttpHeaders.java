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

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class HttpHeaders {
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
    /**
     * Default value is false.
     */
    public static final String USE_CACHES = "useCaches";
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

    HttpHeaders() {
        this.map = new HashMap<>();
    }

    public static HttpHeaders create() {
        return new HttpHeaders();
    }

    public HttpHeaders setAcceptCharset(String acceptCharset) {
        set(HttpHeaders.ACCEPT_CHARSET, acceptCharset);

        return this;
    }

    public HttpHeaders setAcceptEncoding(String acceptEncoding) {
        set(HttpHeaders.ACCEPT_ENCODING, acceptEncoding);

        return this;
    }

    public HttpHeaders setAcceptLanguage(String acceptLanguage) {
        set(HttpHeaders.ACCEPT_LANGUAGE, acceptLanguage);

        return this;
    }

    public HttpHeaders setAcceptRanges(String acceptRanges) {
        set(HttpHeaders.ACCEPT_RANGES, acceptRanges);

        return this;
    }

    public HttpHeaders setContentType(String contentType) {
        //    if (hasHeader(HTTP.CONTENT_FORMAT)) {
        //        throw new IllegalArgumentException("The parameter 'contentFormat' has already been set");
        //    }

        set(HttpHeaders.CONTENT_TYPE, contentType);

        return this;
    }

    public HttpHeaders setContentEncoding(String acceptEncoding) {
        set(HttpHeaders.CONTENT_ENCODING, acceptEncoding);

        return this;
    }

    public HttpHeaders setContentLanguage(String acceptLanguage) {
        set(HttpHeaders.CONTENT_LANGUAGE, acceptLanguage);

        return this;
    }

    public HttpHeaders setContentLength(long contentLength) {
        set(HttpHeaders.CONTENT_LENGTH, contentLength);

        return this;
    }

    public HttpHeaders setUserAgent(String userAgent) {
        set(HttpHeaders.USER_AGENT, userAgent);

        return this;
    }

    public HttpHeaders set(String name, Object value) {
        map.put(name, value);

        return this;
    }

    public HttpHeaders setAll(Map<? extends String, ? extends Object> m) {
        map.putAll(m);

        return this;
    }

    public Object get(String headerName) {
        return map.get(headerName);
    }

    public Object remove(String headerName) {
        return map.remove(headerName);
    }

    public Set<String> headerNameSet() {
        return map.keySet();
    }

    public void clear() {
        map.clear();
    }

    public boolean isEmpty() {
        return map.isEmpty();
    }

    public HttpHeaders copy() {
        return new HttpHeaders().setAll(this.map);
    }

    @Override
    public int hashCode() {
        return map.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof HttpHeaders && this.map.equals(((HttpHeaders) obj).map);
    }

    @Override
    public String toString() {
        return map.toString();
    }
}
