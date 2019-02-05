/*
 * Copyright (C) 2019 HaiYang Li
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

import java.util.List;
import java.util.Map;

import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.N;

/**
 * 
 * @since 1.3
 * 
 * @author Haiyang Li
 */
public class HttpResponse {
    private final int code;
    private final String message;
    private final Map<String, List<String>> headers;
    private final String body;
    private final ContentFormat bodyFormat;

    HttpResponse(int code, String message, Map<String, List<String>> headers, String body, ContentFormat bodyFormat) {
        this.code = code;
        this.message = message;
        this.headers = headers;
        this.body = body;
        this.bodyFormat = bodyFormat;
    }

    public int code() {
        return code;
    }

    public String message() {
        return message;
    }

    public Map<String, List<String>> headers() {
        return headers;
    }

    public String body() {
        return body;
    }

    public <T> T body(Class<T> resultClass) {
        N.checkArgNotNull(resultClass, "resultClass");

        final Type<Object> type = N.typeOf(resultClass);

        if (resultClass.equals(String.class)) {
            return (T) body;
        } else if (type.isSerializable()) {
            return (T) type.valueOf(body);
        } else {
            return HTTP.getParser(bodyFormat).deserialize(resultClass, body);
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((body == null) ? 0 : body.hashCode());
        result = prime * result + ((bodyFormat == null) ? 0 : bodyFormat.hashCode());
        result = prime * result + code;
        result = prime * result + ((headers == null) ? 0 : headers.hashCode());
        result = prime * result + ((message == null) ? 0 : message.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        HttpResponse other = (HttpResponse) obj;
        if (body == null) {
            if (other.body != null)
                return false;
        } else if (!body.equals(other.body))
            return false;
        if (bodyFormat != other.bodyFormat)
            return false;
        if (code != other.code)
            return false;
        if (headers == null) {
            if (other.headers != null)
                return false;
        } else if (!headers.equals(other.headers))
            return false;
        if (message == null) {
            if (other.message != null)
                return false;
        } else if (!message.equals(other.message))
            return false;
        return true;
    }

    @Override
    public String toString() {
        return "{code=" + code + ", message=" + message + ", headers=" + headers + ", body=" + body + ", bodyFormat=" + bodyFormat + "}";
    }

}
