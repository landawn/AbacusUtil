/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Writer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;

/**
 * A simple way to run function performance by unit test.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Profiler {
    private static final Logger logger = LoggerFactory.getLogger(Profiler.class);

    private Profiler() {
        // singleton
    }

    public static MultiLoopsStatistics run(final int threadNum, final int loopNum, final int roundNum, final Runnable command) {
        return run(command, getMethod(command, "run"), threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Runnable command, final int threadNum, final int loopNum, final int roundNum) {
        return run(command, getMethod(command, "run"), threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Object instance, final String method, final int threadNum, final int loopNum, final int roundNum) {
        return run(instance, getMethod(instance, method), threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Object instance, final Method method, final int threadNum, final int loopNum, final int roundNum) {
        return run(instance, method, null, threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Object instance, final Method method, final Object arg, final int threadNum, final int loopNum,
            final int roundNum) {
        return run(instance, method, ((arg == null) ? null : N.asList(arg)), threadNum, loopNum, roundNum);
    }

    /**
     * 
     * @param instance
     * @param method
     * @param args the size of <code>args</code> can be 0, 1, or same size with <code>threadNum. It's the input argument for every loop in each thread.
     * @param threadNum
     * @param loopNum
     * @param roundNum
     * @return
     */
    public static MultiLoopsStatistics run(final Object instance, final Method method, final List<?> args, final int threadNum, final int loopNum,
            final int roundNum) {
        return run(instance, N.asList(method), args, threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Object instance, final List<Method> methodList, final int threadNum, final int loopNum, final int roundNum) {
        return run(instance, methodList, null, threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Object instance, final List<Method> methodList, final Object arg, final int threadNum, final int loopNum,
            final int roundNum) {
        return run(instance, methodList, ((arg == null) ? null : N.asList(arg)), threadNum, loopNum, roundNum);
    }

    public static MultiLoopsStatistics run(final Object instance, final List<Method> methodList, final List<?> args, final int threadNum, final int loopNum,
            final int roundNum) {
        return run(instance, methodList, args, null, null, null, null, threadNum, 0, loopNum, 0, roundNum);
    }

    /**
     * Run performance test for the specified <code>method</code> with the specified <code>threadNum</code> and <code>loopNum</code> for each thread.
     * The performance test will be repeatly execute times specified by <code>roundNum</code>. 
     * 
     * @param instance
     * @param method
     * @param arg
     * @param setUpForMethod
     * @param tearDownForMethod
     * @param setUpForLoop
     * @param tearDownForLoop
     * @param threadNum
     * @param threadDelay
     * @param loopNum
     * @param loopDelay
     * @param roundNum
     * @return
     */
    public static MultiLoopsStatistics run(final Object instance, final Method method, final Object arg, final Method setUpForMethod,
            final Method tearDownForMethod, final Method setUpForLoop, final Method tearDownForLoop, final int threadNum, final long threadDelay,
            final int loopNum, final long loopDelay, final int roundNum) {
        return run(instance, N.asList(method), ((arg == null) ? null : N.asList(arg)), setUpForMethod, tearDownForMethod, setUpForLoop, tearDownForLoop,
                threadNum, threadDelay, loopNum, loopDelay, roundNum);
    }

    /**
     * Run performance test for the specified <code>methodList</code> with the specified <code>threadNum</code> and <code>loopNum</code> for each thread.
     * The performance test will be repeatly execute times specified by <code>roundNum</code>. 
     * 
     * @param instance it can be null if methods in the specified <code>methodList</code> are static methods
     * @param methodList
     * @param args the size of <code>args</code> can be 0, 1, or same size with <code>threadNum. It's the input argument for every loop in each thread.
     * @param setUpForMethod
     * @param tearDownForMethod
     * @param setUpForLoop
     * @param tearDownForLoop
     * @param threadNum
     * @param threadDelay
     * @param loopNum loops run by each thread.
     * @param loopDelay
     * @param roundNum
     * @return
     */
    public static MultiLoopsStatistics run(final Object instance, final List<Method> methodList, final List<?> args, final Method setUpForMethod,
            final Method tearDownForMethod, final Method setUpForLoop, final Method tearDownForLoop, final int threadNum, final long threadDelay,
            final int loopNum, final long loopDelay, final int roundNum) {

        if (roundNum == 1) {
            return run(instance, methodList, args, setUpForMethod, tearDownForMethod, setUpForLoop, tearDownForLoop, threadNum, threadDelay, loopNum,
                    loopDelay);
        } else {
            MultiLoopsStatistics result = null;

            for (int i = 0; i < roundNum; i++) {
                if (result != null) {
                    result.printResult();
                    result = null;
                }

                result = run(instance, methodList, args, setUpForMethod, tearDownForMethod, setUpForLoop, tearDownForLoop, threadNum, threadDelay, loopNum,
                        loopDelay);
            }

            return result;
        }
    }

    private static MultiLoopsStatistics run(final Object instance, final List<Method> methodList, final List<?> args, final Method setUpForMethod,
            final Method tearDownForMethod, final Method setUpForLoop, final Method tearDownForLoop, final int threadNum, final long threadDelay,
            final int loopNum, final long loopDelay) {
        if (N.isNullOrEmpty(methodList) || (methodList.get(0) == null)) {
            throw new IllegalArgumentException("Methods can't be null");
        }

        if ((threadNum <= 0) || (loopNum <= 0) || (threadDelay < 0) || (loopDelay < 0)) {
            throw new IllegalArgumentException("threadNum=" + threadNum + ", loopNum=" + loopNum + ", threadDelay=" + threadDelay + ", loopDelay=" + loopDelay);
        }

        if (N.notNullOrEmpty(args) && (args.size() > 1) && (args.size() != threadNum)) {
            throw new IllegalArgumentException(
                    "The input args must be null or size = 1 or size = threadNum. It's the input parameter for the every loop in each thread ");
        }

        for (Method method : methodList) {
            if (!method.isAccessible()) {
                method.setAccessible(true);
            }
        }

        gc();

        N.sleep(1000);

        final AsyncExecutor asyncExecutor = new AsyncExecutor(threadNum, 300, TimeUnit.SECONDS);
        final AtomicInteger threadCounter = new AtomicInteger();

        // MXBean mxBean = new MXBean();
        final List<LoopStatistics> loopStatisticsList = Collections.synchronizedList(new ArrayList<LoopStatistics>());
        final PrintStream ps = System.out;

        final long startTime = System.currentTimeMillis();

        for (int threadIndex = 0; threadIndex < threadNum; threadIndex++) {
            final Object threadArg = (N.isNullOrEmpty(args)) ? null : ((args.size() == 1) ? args.get(0) : args.get(threadIndex));
            threadCounter.incrementAndGet();

            asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        runLoops(instance, methodList, threadArg, setUpForMethod, tearDownForMethod, setUpForLoop, tearDownForLoop, loopNum, loopDelay,
                                loopStatisticsList, ps);
                    } finally {
                        threadCounter.decrementAndGet();
                    }
                }
            });

            N.sleep(threadDelay);
        }

        while (threadCounter.get() > 0) {
            N.sleep(10);
        }

        final long endTime = System.currentTimeMillis();

        return new MultiLoopsStatistics(startTime, endTime, threadNum, loopStatisticsList);
    }

    private static void runLoops(final Object instance, final List<Method> methodList, final Object threadArg, final Method setUpForMethod,
            final Method tearDownForMethod, final Method setUpForLoop, final Method tearDownForLoop, final int loopNum, final long loopDelay,
            final List<LoopStatistics> loopStatisticsList, final PrintStream ps) {
        for (int loopIndex = 0; loopIndex < loopNum; loopIndex++) {
            if (setUpForLoop != null) {
                try {
                    setUpForLoop.invoke(instance);
                } catch (Exception e) {
                    // ignore;
                    e.printStackTrace(ps);
                    logger.warn(AbacusException.getErrorMsg(e));
                }
            }

            final long loopStartTime = System.currentTimeMillis();

            List<MethodStatistics> methodStatisticsList = runLoop(instance, methodList, threadArg, setUpForMethod, tearDownForMethod, ps);

            final long loopEndTime = System.currentTimeMillis();

            if (tearDownForLoop != null) {
                try {
                    tearDownForLoop.invoke(instance);
                } catch (Exception e) {
                    // ignore;
                    e.printStackTrace(ps);
                    logger.warn(AbacusException.getErrorMsg(e));
                }
            }

            SingleLoopStatistics loopStatistics = new SingleLoopStatistics(loopStartTime, loopEndTime, methodStatisticsList);

            loopStatisticsList.add(loopStatistics);

            N.sleep(loopDelay);
        }
    }

    private static List<MethodStatistics> runLoop(final Object instance, final List<Method> methodList, final Object loopArg, final Method setUpForMethod,
            final Method tearDownForMethod, final PrintStream ps) {
        List<MethodStatistics> methodStatisticsList = new ArrayList<MethodStatistics>();

        for (int methodIndex = 0; methodIndex < methodList.size(); methodIndex++) {
            if (setUpForMethod != null) {
                try {
                    setUpForMethod.invoke(instance);
                } catch (Exception e) {
                    // ignore;
                    e.printStackTrace(ps);
                    logger.warn(AbacusException.getErrorMsg(e));
                }
            }

            final long methodStartTime = System.currentTimeMillis();
            Object result = null;
            Method method = null;

            try {
                method = methodList.get(methodIndex);

                if (method.getParameterTypes().length == 0) {
                    method.invoke(instance);
                } else {
                    method.invoke(instance, loopArg);
                }
            } catch (InvocationTargetException e) {
                e.printStackTrace();
                e.printStackTrace(ps);
                logger.warn(AbacusException.getErrorMsg(e));
                result = e.getTargetException();
            } catch (Exception e) {
                e.printStackTrace();
                e.printStackTrace(ps);
                logger.warn(AbacusException.getErrorMsg(e));
                result = e;
            }

            final long methodEndTime = System.currentTimeMillis();

            if (tearDownForMethod != null) {
                try {
                    tearDownForMethod.invoke(instance);
                } catch (Exception e) {
                    // ignore;
                    e.printStackTrace(ps);
                    logger.warn(AbacusException.getErrorMsg(e));
                }
            }

            MethodStatistics methodStatistics = new MethodStatistics(methodList.get(methodIndex).getName(), methodStartTime, methodEndTime, result);
            methodStatisticsList.add(methodStatistics);
        }

        return methodStatisticsList;
    }

    private static Method getMethod(final Object instance, final String methodName) {
        Method method = N.getDeclaredMethod(instance.getClass(), methodName);

        if (method == null) {
            throw new IllegalArgumentException("No method found by name: " + methodName);
        } else if (!method.isAccessible()) {
            method.setAccessible(true);
        }
        return method;
    }

    private static void gc() {
        Runtime.getRuntime().gc();
        N.sleep(3000);
    }

    /**
     * @author Haiyang Li
     * @version $Revision: 0.8 $
     */
    static interface Statistics {
        Object getResult();

        void setResult(Object result);

        long getElapsedTime();

        long getStartTime();

        void setStartTime(long startTime);

        long getEndTime();

        void setEndTime(long endTime);
    }

    /**
     * @author Haiyang Li
     * @version $Revision: 0.8 $
     */
    static interface LoopStatistics extends Statistics {
        long getTotalElapsedTime();

        List<String> getMethodNameList();

        MethodStatistics getMinElapsedTimeMethod();

        MethodStatistics getMaxElapsedTimeMethod();

        public long getMethodTotalElapsedTime(String methodName);

        public long getMethodMaxElapsedTime(String methodName);

        public long getMethodMinElapsedTime(String methodName);

        public double getMethodAverageElapsedTime(String methodName);

        public int getMethodSize(String methodName);

        public List<MethodStatistics> getMethodStatisticsList(String methodName);

        public List<MethodStatistics> getFailedMethodStatisticsList(String methodName);

        List<MethodStatistics> getAllFailedMethodStatisticsList();
    }

    /**
     * @author Haiyang Li
     * @version $Revision: 0.8 $
     */
    static abstract class AbstractStatistics implements Statistics {
        private Object result;
        private long startTime;
        private long endTime;

        public AbstractStatistics() {
            this(0, 0);
        }

        public AbstractStatistics(final long startTime, final long endTime) {
            this.startTime = startTime;
            this.endTime = endTime;
        }

        @Override
        public Object getResult() {
            return result;
        }

        @Override
        public void setResult(final Object result) {
            this.result = result;
        }

        @Override
        public long getElapsedTime() {
            return endTime - startTime;
        }

        @Override
        public long getStartTime() {
            return startTime;
        }

        @Override
        public void setStartTime(final long startTime) {
            this.startTime = startTime;
        }

        @Override
        public long getEndTime() {
            return endTime;
        }

        @Override
        public void setEndTime(final long endTime) {
            this.endTime = endTime;
        }

        protected String time2String(final long timeInMillis) {
            return N.asTimestamp(timeInMillis).toString();
        }
    }

    /**
     * @author Haiyang Li
     * @version $Revision: 0.8 $
     */
    static class MethodStatistics extends AbstractStatistics {
        private final String methodName;
        private Object result;

        public MethodStatistics(final String methodName) {
            super();
            this.methodName = methodName;
        }

        public MethodStatistics(final String methodName, final long startTime, final long endTime) {
            super(startTime, endTime);
            this.methodName = methodName;
        }

        public MethodStatistics(final String methodName, final long startTime, final long endTime, final Object result) {
            super(startTime, endTime);
            this.methodName = methodName;
            this.result = result;
        }

        public String getMethodName() {
            return methodName;
        }

        @Override
        public Object getResult() {
            return result;
        }

        @Override
        public void setResult(final Object result) {
            this.result = result;
        }

        public boolean isFailed() {
            return (result != null) && result instanceof Exception;
        }

        @Override
        public String toString() {
            if (isFailed()) {
                Exception e = (Exception) result;

                return "method=" + methodName + ", startTime=" + getStartTime() + ", endTime=" + getEndTime() + ", result=" + e + ": "
                        + N.toString(e.getStackTrace()) + ". ";
            } else {
                return "method=" + methodName + ", startTime=" + getStartTime() + ", endTime=" + getEndTime() + ", result=" + result + ". ";
            }
        }
    }

    /**
     * @author Haiyang Li
     * @version $Revision: 0.8 $
     */
    static abstract class AbstractLoopStatistics extends AbstractStatistics implements LoopStatistics {
        private List<MethodStatistics> methodStatisticsList;

        public AbstractLoopStatistics() {
            super();
        }

        public AbstractLoopStatistics(final long startTime, final long endTime) {
            super(startTime, endTime);
        }

        public AbstractLoopStatistics(final long startTime, final long endTime, final List<MethodStatistics> methodStatisticsList) {
            super(startTime, endTime);
            this.methodStatisticsList = methodStatisticsList;
        }

        @Override
        public List<String> getMethodNameList() {
            List<String> result = new ArrayList<String>();

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (!result.contains(methodStatistics.getMethodName())) {
                        result.add(methodStatistics.getMethodName());
                    }
                }
            }

            return result;
        }

        public List<MethodStatistics> getMethodStatisticsList() {
            if (methodStatisticsList == null) {
                methodStatisticsList = new ArrayList<MethodStatistics>();
            }

            return methodStatisticsList;
        }

        public void setMethodStatisticsList(final List<MethodStatistics> methodStatisticsList) {
            this.methodStatisticsList = methodStatisticsList;
        }

        public void addMethodStatisticsList(final MethodStatistics methodStatistics) {
            getMethodStatisticsList().add(methodStatistics);
        }

        @Override
        public long getTotalElapsedTime() {
            long result = 0;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    result += methodStatistics.getElapsedTime();
                }
            }

            return result;
        }

        @Override
        public MethodStatistics getMaxElapsedTimeMethod() {
            MethodStatistics result = null;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if ((result == null) || (methodStatistics.getElapsedTime() > result.getElapsedTime())) {
                        result = methodStatistics;
                    }
                }
            }

            return result;
        }

        @Override
        public MethodStatistics getMinElapsedTimeMethod() {
            MethodStatistics result = null;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if ((result == null) || (methodStatistics.getElapsedTime() < result.getElapsedTime())) {
                        result = methodStatistics;
                    }
                }
            }

            return result;
        }

        @Override
        public long getMethodTotalElapsedTime(final String methodName) {
            long result = 0;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.getMethodName().equals(methodName)) {
                        result += methodStatistics.getElapsedTime();
                    }
                }
            }

            return result;
        }

        @Override
        public long getMethodMaxElapsedTime(final String methodName) {
            long result = 0;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.getMethodName().equals(methodName)) {
                        if (methodStatistics.getElapsedTime() > result) {
                            result = methodStatistics.getElapsedTime();
                        }
                    }
                }
            }

            return result;
        }

        @Override
        public long getMethodMinElapsedTime(final String methodName) {
            long result = Integer.MAX_VALUE;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.getMethodName().equals(methodName)) {
                        if (methodStatistics.getElapsedTime() < result) {
                            result = methodStatistics.getElapsedTime();
                        }
                    }
                }
            }

            return result;
        }

        @Override
        public double getMethodAverageElapsedTime(final String methodName) {
            double totalTime = 0;
            int methodNum = 0;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.getMethodName().equals(methodName)) {
                        totalTime += methodStatistics.getElapsedTime();
                        methodNum++;
                    }
                }
            }

            return (methodNum > 0) ? (totalTime / methodNum) : totalTime;
        }

        @Override
        public int getMethodSize(final String methodName) {
            int methodSize = 0;

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.getMethodName().equals(methodName)) {
                        methodSize++;
                    }
                }
            }

            return methodSize;
        }

        @Override
        public List<MethodStatistics> getMethodStatisticsList(final String methodName) {
            List<MethodStatistics> result = new ArrayList<>(getMethodSize(methodName));

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.getMethodName().equals(methodName)) {
                        result.add(methodStatistics);
                    }
                }
            }

            return result;
        }

        @Override
        public List<MethodStatistics> getFailedMethodStatisticsList(final String methodName) {
            List<MethodStatistics> result = new ArrayList<MethodStatistics>();

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.isFailed() && methodStatistics.getMethodName().equals(methodName)) {
                        result.add(methodStatistics);
                    }
                }
            }

            return result;
        }

        @Override
        public List<MethodStatistics> getAllFailedMethodStatisticsList() {
            List<MethodStatistics> result = new ArrayList<MethodStatistics>();

            if (methodStatisticsList != null) {
                for (MethodStatistics methodStatistics : methodStatisticsList) {
                    if (methodStatistics.isFailed()) {
                        result.add(methodStatistics);
                    }
                }
            }

            return result;
        }
    }

    static class SingleLoopStatistics extends AbstractLoopStatistics {
        public SingleLoopStatistics() {
            super();
        }

        public SingleLoopStatistics(final long startTime, final long endTime) {
            super(startTime, endTime);
        }

        public SingleLoopStatistics(final long startTime, final long endTime, final List<MethodStatistics> methodStatisticsList) {
            super(startTime, endTime, methodStatisticsList);
        }
    }

    public static class MultiLoopsStatistics extends AbstractStatistics implements LoopStatistics {
        private static final String SEPARATOR_LINE = "========================================================================================================================";
        static final DecimalFormat DOUBLE_FORMAT = new DecimalFormat("#.####");

        private final int threadNum;
        private List<LoopStatistics> loopStatisticsList;

        public MultiLoopsStatistics(final long startTime, final long endTime, final int threadNum) {
            super(startTime, endTime);
            this.threadNum = threadNum;
        }

        public MultiLoopsStatistics(final long startTime, final long endTime, final int threadNum, final List<LoopStatistics> loopStatisticsList) {
            this(startTime, endTime, threadNum);
            this.loopStatisticsList = loopStatisticsList;
        }

        public int getThreadNum() {
            return threadNum;
        }

        @Override
        public List<String> getMethodNameList() {
            List<String> result = null;

            if (loopStatisticsList == null) {
                result = new ArrayList<String>();
            } else {
                result = (loopStatisticsList.get(0)).getMethodNameList();
            }

            return result;
        }

        public List<LoopStatistics> getLoopStatisticsList() {
            if (loopStatisticsList == null) {
                loopStatisticsList = new ArrayList<LoopStatistics>();
            }

            return loopStatisticsList;
        }

        public void setLoopStatisticsList(final List<LoopStatistics> loopStatisticsList) {
            this.loopStatisticsList = loopStatisticsList;
        }

        public void addMethodStatisticsList(final LoopStatistics loopStatistics) {
            getLoopStatisticsList().add(loopStatistics);
        }

        @Override
        public long getTotalElapsedTime() {
            long result = 0;

            if (loopStatisticsList != null) {
                for (int index = 0; index < loopStatisticsList.size(); index++) {
                    LoopStatistics loopStatistics = loopStatisticsList.get(index);
                    result += loopStatistics.getTotalElapsedTime();
                }
            }

            return result;
        }

        @Override
        public MethodStatistics getMaxElapsedTimeMethod() {
            MethodStatistics result = null;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    MethodStatistics methodStatistics = loopStatistics.getMaxElapsedTimeMethod();

                    if ((result == null) || (methodStatistics.getElapsedTime() > result.getElapsedTime())) {
                        result = methodStatistics;
                    }
                }
            }

            return result;
        }

        @Override
        public MethodStatistics getMinElapsedTimeMethod() {
            MethodStatistics result = null;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    MethodStatistics methodStatistics = loopStatistics.getMinElapsedTimeMethod();

                    if ((result == null) || (methodStatistics.getElapsedTime() < result.getElapsedTime())) {
                        result = methodStatistics;
                    }
                }
            }

            return result;
        }

        @Override
        public long getMethodTotalElapsedTime(final String methodName) {
            long result = 0;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    result += loopStatistics.getMethodTotalElapsedTime(methodName);
                }
            }

            return result;
        }

        @Override
        public long getMethodMaxElapsedTime(final String methodName) {
            long result = 0;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    long loopMethodMaxTime = loopStatistics.getMethodMaxElapsedTime(methodName);

                    if (loopMethodMaxTime > result) {
                        result = loopMethodMaxTime;
                    }
                }
            }

            return result;
        }

        @Override
        public long getMethodMinElapsedTime(final String methodName) {
            long result = Integer.MAX_VALUE;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    long loopMethodMinTime = loopStatistics.getMethodMinElapsedTime(methodName);

                    if (loopMethodMinTime < result) {
                        result = loopMethodMinTime;
                    }
                }
            }

            return result;
        }

        @Override
        public double getMethodAverageElapsedTime(final String methodName) {
            double totalTime = 0;
            int methodNum = 0;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    double loopMethodTotalTime = loopStatistics.getMethodTotalElapsedTime(methodName);
                    int loopMethodSize = loopStatistics.getMethodSize(methodName);
                    totalTime += loopMethodTotalTime;
                    methodNum += loopMethodSize;
                }
            }

            return (methodNum > 0) ? (totalTime / methodNum) : totalTime;
        }

        @Override
        public int getMethodSize(final String methodName) {
            int result = 0;

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    result += loopStatistics.getMethodSize(methodName);
                }
            }

            return result;
        }

        @Override
        public List<MethodStatistics> getMethodStatisticsList(final String methodName) {
            List<MethodStatistics> methodStatisticsList = new ArrayList<>(getMethodSize(methodName));

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    methodStatisticsList.addAll(loopStatistics.getMethodStatisticsList(methodName));
                }
            }

            return methodStatisticsList;
        }

        @Override
        public List<MethodStatistics> getFailedMethodStatisticsList(final String methodName) {
            List<MethodStatistics> result = new ArrayList<MethodStatistics>();

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    result.addAll(loopStatistics.getFailedMethodStatisticsList(methodName));
                }
            }

            return result;
        }

        @Override
        public List<MethodStatistics> getAllFailedMethodStatisticsList() {
            List<MethodStatistics> result = new ArrayList<MethodStatistics>();

            if (loopStatisticsList != null) {
                for (LoopStatistics loopStatistics : loopStatisticsList) {
                    result.addAll(loopStatistics.getAllFailedMethodStatisticsList());
                }
            }

            return result;
        }

        public void printResult() {
            writeResult(new PrintWriter(System.out));
        }

        public void writeResult(final OutputStream os) {
            writeResult(new PrintWriter(os));
        }

        public void writeResult(final Writer writer) {
            writeResult(new PrintWriter(writer));
        }

        private void writeResult(final PrintWriter writer) {
            writer.println();
            writer.println(SEPARATOR_LINE);
            writer.println("(unit: milliseconds)");
            writer.println("threadNum=" + threadNum + "; loops=" + (loopStatisticsList.size() / threadNum));
            writer.println("startTime: " + time2String(getStartTime()));
            writer.println("endTime:   " + time2String(getEndTime()));
            writer.println("totalElapsedTime: " + getElapsedTime());
            writer.println();

            MethodStatistics methodStatistics = getMaxElapsedTimeMethod();

            if (methodStatistics != null) {
                writer.println("maxMethodTime(" + methodStatistics.getMethodName() + "): " + methodStatistics.getElapsedTime());
            }

            methodStatistics = getMinElapsedTimeMethod();

            if (methodStatistics != null) {
                writer.println("minMethodTime(" + methodStatistics.getMethodName() + "): " + methodStatistics.getElapsedTime());
            }

            String methodNameTitil = "<method name>";
            List<String> methodNameList = getMethodNameList();
            int maxMethodNameLength = methodNameTitil.length();

            if (methodNameList.size() > 0) {
                for (String methodName : methodNameList) {
                    if (methodName.length() > maxMethodNameLength) {
                        maxMethodNameLength = methodName.length();
                    }
                }
            }

            writer.println();
            maxMethodNameLength += 3;
            writer.println(N.padEnd(methodNameTitil + ",  ", maxMethodNameLength)
                    + "|avg time|, |min time|, |max time|, |0.01% >=|, |0.1% >=|,  |1% >=|,    |10% >=|,   |20% >=|,   |50% >=|,   |80% >=|,   |90% >=|,   |99% >=|,   |99.9% >=|, |99.99% >=|");

            for (String methodName : methodNameList) {
                List<MethodStatistics> methodStatisticsList = getMethodStatisticsList(methodName);
                int size = methodStatisticsList.size();
                Collections.sort(methodStatisticsList, new Comparator<MethodStatistics>() {
                    @Override
                    public int compare(final MethodStatistics o1, final MethodStatistics o2) {
                        return (o1.getElapsedTime() == o2.getElapsedTime()) ? 0 : ((o1.getElapsedTime() > o2.getElapsedTime()) ? (-1) : 1);
                    }
                });

                double avgTime = getMethodAverageElapsedTime(methodName);

                long maxTime = methodStatisticsList.get(0).getElapsedTime();
                long minTime = methodStatisticsList.get(size - 1).getElapsedTime();

                final int minLen = 12;
                writer.println(
                        N.padEnd(methodName + ",  ", maxMethodNameLength) + N.padEnd(DOUBLE_FORMAT.format(avgTime) + ",  ", minLen)
                                + N.padEnd(minTime + ",  ",
                                        minLen)
                                + N.padEnd(maxTime + ",  ",
                                        minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.0001)).getElapsedTime() + ",  ",
                                        minLen)
                                + N.padEnd(
                                        methodStatisticsList.get((int) (size * 0.001)).getElapsedTime()
                                                + ",  ",
                                        minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.01)).getElapsedTime() + ",  ",
                                        minLen)
                                + N.padEnd(
                                        methodStatisticsList.get((int) (size * 0.1)).getElapsedTime()
                                                + ",  ",
                                        minLen)
                                + N.padEnd(
                                        methodStatisticsList.get((int) (size * 0.2)).getElapsedTime()
                                                + ",  ",
                                        minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.5)).getElapsedTime() + ",  ", minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.8)).getElapsedTime() + ",  ", minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.9)).getElapsedTime() + ",  ", minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.99)).getElapsedTime() + ",  ", minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.999)).getElapsedTime() + ",  ", minLen)
                                + N.padEnd(methodStatisticsList.get((int) (size * 0.9999)).getElapsedTime() + ",  ", minLen));
            }

            writer.println();

            writeError(writer);

            writer.println(SEPARATOR_LINE);

            writer.flush();
        }

        private void writeError(final PrintWriter writer) {
            MethodStatistics methodStatistics;
            List<?> failedMethodList = getAllFailedMethodStatisticsList();

            if (failedMethodList.size() > 0) {
                writer.println();
                writer.println("Errors:");

                for (int index = 0; index < failedMethodList.size(); index++) {
                    writer.println("<<---------------------------------------------------------");
                    methodStatistics = (MethodStatistics) failedMethodList.get(index);
                    writer.println(methodStatistics.toString());
                    writer.println("--------------------------------------------------------->>");
                }
            }
        }

        public void writeHtmlResult(final OutputStream os) {
            writeHtmlResult(new PrintWriter(os));
        }

        public void writeHtmlResult(final Writer writer) {
            writeHtmlResult(new PrintWriter(writer));
        }

        private void writeHtmlResult(final PrintWriter writer) {
            writer.println(SEPARATOR_LINE);
            writer.println("<br/>" + "(unit: milliseconds)");
            writer.println("<br/>" + "threadNum=" + threadNum + "; loops=" + (loopStatisticsList.size() / threadNum) + "");
            writer.println("<br/>" + "startTime: " + time2String(getStartTime()) + "");
            writer.println("<br/>" + "endTime:   " + time2String(getEndTime()) + "");
            writer.println("<br/>" + "totalElapsedTime: " + getElapsedTime() + "");
            writer.println("<br/>");

            MethodStatistics methodStatistics = getMaxElapsedTimeMethod();

            if (methodStatistics != null) {
                writer.println("<br/>" + "maxMethodTime(" + methodStatistics.getMethodName() + "): " + methodStatistics.getElapsedTime());
            }

            methodStatistics = getMinElapsedTimeMethod();

            if (methodStatistics != null) {
                writer.println("<br/>" + "minMethodTime(" + methodStatistics.getMethodName() + "): " + methodStatistics.getElapsedTime());
            }

            writer.println("<br/>");
            writer.println("<table width=\"600\" border=\"1\">");
            writer.println("<tr>");
            writer.println("<th>method name</th>");
            writer.println("<th>avg time</th>");
            writer.println("<th>min time</th>");
            writer.println("<th>max time</th>");
            writer.println("<th>0.01% &gt;=</th>");
            writer.println("<th>0.1% &gt;=</th>");
            writer.println("<th>1% &gt;=</th>");
            writer.println("<th>10% &gt;=</th>");
            writer.println("<th>20% &gt;=</th>");
            writer.println("<th>50% &gt;=</th>");
            writer.println("<th>80% &gt;=</th>");
            writer.println("<th>90% &gt;=</th>");
            writer.println("<th>99% &gt;=</th>");
            writer.println("<th>99.9% &gt;=</th>");
            writer.println("<th>99.99% &gt;=</th>");
            writer.println("</tr>");

            List<String> methodNameList = getMethodNameList();

            for (String methodName : methodNameList) {
                List<MethodStatistics> methodStatisticsList = getMethodStatisticsList(methodName);
                int size = methodStatisticsList.size();
                Collections.sort(methodStatisticsList, new Comparator<MethodStatistics>() {
                    @Override
                    public int compare(final MethodStatistics o1, final MethodStatistics o2) {
                        return (o1.getElapsedTime() == o2.getElapsedTime()) ? 0 : ((o1.getElapsedTime() > o2.getElapsedTime()) ? (-1) : 1);
                    }
                });

                double avgTime = getMethodAverageElapsedTime(methodName);

                long minTime = methodStatisticsList.get(size - 1).getElapsedTime();
                long maxTime = methodStatisticsList.get(0).getElapsedTime();

                writer.println("<tr>");
                writer.println("<td>" + methodName + "</td>");
                writer.println("<td>" + DOUBLE_FORMAT.format(avgTime) + "</td>");
                writer.println("<td>" + minTime + "</td>");
                writer.println("<td>" + maxTime + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.0001)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.001)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.01)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.1)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.2)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.5)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.8)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.9)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.99)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.999)).getElapsedTime() + "</td>");
                writer.println("<td>" + methodStatisticsList.get((int) (size * 0.9999)).getElapsedTime() + "</td>");
                writer.println("</tr>");
            }

            writer.println("</table>");

            writeHtmlError(writer);

            writer.println(SEPARATOR_LINE);

            writer.flush();
        }

        private void writeHtmlError(final PrintWriter writer) {
            MethodStatistics methodStatistics;
            List<?> failedMethodList = getAllFailedMethodStatisticsList();

            if (failedMethodList.size() > 0) {
                writer.println("<h5>Errors:</h5>");

                for (int index = 0; index < failedMethodList.size(); index++) {
                    writer.println("<br/>" + "<<---------------------------------------------------------");
                    methodStatistics = (MethodStatistics) failedMethodList.get(index);

                    Exception e = (Exception) methodStatistics.getResult();
                    writer.println("<br/>" + (e.getClass().getSimpleName() + ": " + e.getMessage()));
                    writer.println("<br/>" + "--------------------------------------------------------->>");
                }
            }
        }

        public void writeXmlResult(final OutputStream os) {
            writeXmlResult(new PrintWriter(os));
        }

        public void writeXmlResult(final Writer writer) {
            writeXmlResult(new PrintWriter(writer));
        }

        private void writeXmlResult(final PrintWriter writer) {
            writer.println("<result>");

            writer.println("<unit>milliseconds</unit>");
            writer.println("<threadNum>" + threadNum + "</threadNum>");
            writer.println("<loops>" + (loopStatisticsList.size() / threadNum) + "</loops>");
            writer.println("<startTime>" + time2String(getStartTime()) + "</startTime>");
            writer.println("<endTime>" + time2String(getEndTime()) + "</endTime>");
            writer.println("<totalElapsedTime>" + getElapsedTime() + "</totalElapsedTime>");
            writer.println();

            MethodStatistics methodStatistics = getMinElapsedTimeMethod();

            if (methodStatistics != null) {
                writer.println("<minMethodTime>" + methodStatistics.getElapsedTime() + "</minMethodTime>");
            }

            methodStatistics = getMaxElapsedTimeMethod();

            if (methodStatistics != null) {
                writer.println("<maxMethodTime>" + methodStatistics.getElapsedTime() + "</maxMethodTime>");
            }

            List<String> methodNameList = getMethodNameList();

            for (String methodName : methodNameList) {
                List<MethodStatistics> methodStatisticsList = getMethodStatisticsList(methodName);
                int size = methodStatisticsList.size();
                Collections.sort(methodStatisticsList, new Comparator<MethodStatistics>() {
                    @Override
                    public int compare(final MethodStatistics o1, final MethodStatistics o2) {
                        return (o1.getElapsedTime() == o2.getElapsedTime()) ? 0 : ((o1.getElapsedTime() > o2.getElapsedTime()) ? (-1) : 1);
                    }
                });

                double avgTime = getMethodAverageElapsedTime(methodName);

                long minTime = methodStatisticsList.get(size - 1).getElapsedTime();
                long maxTime = methodStatisticsList.get(0).getElapsedTime();

                writer.println("<method name=\"" + methodName + "\">");
                writer.println("<avgTime>" + DOUBLE_FORMAT.format(avgTime) + "</avgTime>");
                writer.println("<minTime>" + minTime + "</minTime>");
                writer.println("<maxTime>" + maxTime + "</maxTime>");
                writer.println("<_0.0001>" + methodStatisticsList.get((int) (size * 0.0001)).getElapsedTime() + "</_0.0001>");
                writer.println("<_0.001>" + methodStatisticsList.get((int) (size * 0.001)).getElapsedTime() + "</_0.001>");
                writer.println("<_0.01>" + methodStatisticsList.get((int) (size * 0.01)).getElapsedTime() + "</_0.01>");
                writer.println("<_0.2>" + methodStatisticsList.get((int) (size * 0.2)).getElapsedTime() + "</_0.2>");
                writer.println("<_0.5>" + methodStatisticsList.get((int) (size * 0.5)).getElapsedTime() + "</_0.5>");
                writer.println("<_0.8>" + methodStatisticsList.get((int) (size * 0.8)).getElapsedTime() + "</_0.8>");
                writer.println("<_0.9>" + methodStatisticsList.get((int) (size * 0.9)).getElapsedTime() + "</_0.9>");
                writer.println("<_0.99>" + methodStatisticsList.get((int) (size * 0.99)).getElapsedTime() + "</_0.99>");
                writer.println("<_0.999>" + methodStatisticsList.get((int) (size * 0.999)).getElapsedTime() + "</_0.999>");
                writer.println("<_0.9999>" + methodStatisticsList.get((int) (size * 0.9999)).getElapsedTime() + "</_0.9999>");
                writer.println("</method>");
            }

            List<MethodStatistics> failedMethodList = getAllFailedMethodStatisticsList();

            if (failedMethodList.size() > 0) {
                List<String> failedMethodNameList = new ArrayList<String>();

                for (MethodStatistics ms : failedMethodList) {
                    failedMethodNameList.add(ms.getMethodName());
                }

                writer.println(
                        "<errorMethods>" + failedMethodNameList.toString().substring(1, failedMethodNameList.toString().length() - 1) + "</errorMethods>");
            }

            writer.println("</result>");

            writer.flush();
        }
    }
}
