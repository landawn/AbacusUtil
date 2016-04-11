/**
 * Copyright (c) 2004-2011 QOS.ch
 * All rights reserved.
 *
 * Permission is hereby granted, free  of charge, to any person obtaining
 * a  copy  of this  software  and  associated  documentation files  (the
 * "Software"), to  deal in  the Software without  restriction, including
 * without limitation  the rights to  use, copy, modify,  merge, publish,
 * distribute,  sublicense, and/or sell  copies of  the Software,  and to
 * permit persons to whom the Software  is furnished to do so, subject to
 * the following conditions:
 *
 * The  above  copyright  notice  and  this permission  notice  shall  be
 * included in all copies or substantial portions of the Software.
 *
 * THE  SOFTWARE IS  PROVIDED  "AS  IS", WITHOUT  WARRANTY  OF ANY  KIND,
 * EXPRESS OR  IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE  WARRANTIES OF
 * MERCHANTABILITY,    FITNESS    FOR    A   PARTICULAR    PURPOSE    AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE,  ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.landawn.abacus.logging;

import com.landawn.abacus.util.N;

/**
 * Holds the results of formatting done by {@link MessageFormatter}.
 * 
 * @author Joern Huxhorn
 */
public class FormattedMessage {
    public static final FormattedMessage NULL = new FormattedMessage(null);

    private final String message;
    private final Throwable throwable;
    private final Object[] args;

    public FormattedMessage(String message) {
        this(message, null, null);
    }

    public FormattedMessage(String message, Object[] args, Throwable throwable) {
        this.message = message;
        this.throwable = throwable;

        if (throwable == null) {
            this.args = args;
        } else {
            if (N.isNullOrEmpty(args)) {
                throw new IllegalStateException("non-sensical empty or null argument array");
            }

            this.args = N.copyOfRange(args, 0, args.length - 1);
        }
    }

    public String getMessage() {
        return message;
    }

    public Object[] getArgs() {
        return args;
    }

    public Throwable getThrowable() {
        return throwable;
    }
}
