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

package com.landawn.abacus.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.SAXException;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.exception.ParseException;

/**
 * the cql scripts are configured in xml file and mapped to short ids referenced in program. for example: <br>
 * {@code <cqlMapper>} <br>
 * {@code <cql id="findAccountById">select * from account where id=1</cql>} <br>
 * {@code <cql id="updateAccountNameById">update account set name=? where id=?</cql>} <br>
 * {@code </cqlMapper>}
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class CQLMapper {
    public static final String CQL_MAPPER = "cqlMapper";
    public static final String CQL = "cql";
    public static final String ID = "id";

    static final String TIMEOUT = "timeout";
    private final Map<String, NamedCQL> cqlMap = new LinkedHashMap<>();

    public CQLMapper() {
    }

    /**
     * 
     * @param filePath it could be multiple file paths separated by ',' or ';'
     * 
     */
    public CQLMapper(String filePath) {
        this();

        loadFrom(filePath);
    }

    /**
     * 
     * @param filePath it could be multiple file paths separated by ',' or ';'
     */
    public void loadFrom(String filePath) {
        String[] filePaths = Splitter.with(WD.COMMA).trim(true).splitToArray(filePath);

        if (filePaths.length == 1) {
            filePaths = Splitter.with(WD.SEMICOLON).trim(true).splitToArray(filePath);
        }

        for (String subFilePath : filePaths) {
            final File file = Configuration.formatPath(Configuration.findFile(subFilePath));

            InputStream is = null;

            try {
                is = new FileInputStream(file);

                Document doc = XMLUtil.createDOMParser(true, true).parse(is);
                NodeList cqlMapperEle = doc.getElementsByTagName(CQLMapper.CQL_MAPPER);

                if (0 == cqlMapperEle.getLength()) {
                    throw new AbacusException("There is no 'cqlMapper' element. ");
                }

                List<Element> cqlElementList = XMLUtil.getElementsByTagName((Element) cqlMapperEle.item(0), CQL);

                for (Element cqlElement : cqlElementList) {
                    Map<String, String> attrMap = XMLUtil.readAttributes(cqlElement);

                    add(attrMap.remove(ID), Configuration.getTextContent(cqlElement), attrMap);
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            } catch (SAXException e) {
                throw new ParseException(e);
            } finally {
                IOUtil.close(is);
            }
        }
    }

    public Set<String> keySet() {
        return cqlMap.keySet();
    }

    public NamedCQL get(String id) {
        return cqlMap.get(id);
    }

    public NamedCQL add(String id, NamedCQL namedCQL) {
        return cqlMap.put(id, namedCQL);
    }

    public void add(String id, String cql, Map<String, String> attrs) {
        if (cqlMap.containsKey(id)) {
            throw new IllegalArgumentException(id + " already exists with cql: " + cqlMap.get(id));
        }

        cqlMap.put(id, NamedCQL.parse(cql, attrs));
    }

    public void remove(String id) {
        cqlMap.remove(id);
    }

    public void saveTo(File file) {
        OutputStream os = null;

        try {

            Document doc = XMLUtil.createDOMParser(true, true).newDocument();
            Element cqlMapperNode = doc.createElement(CQLMapper.CQL_MAPPER);

            for (String id : cqlMap.keySet()) {
                NamedCQL namedCQL = cqlMap.get(id);

                Element cqlNode = doc.createElement(CQL);
                cqlNode.setAttribute(ID, id);

                if (!N.isNullOrEmpty(namedCQL.getAttribes())) {
                    Map<String, String> attrs = namedCQL.getAttribes();

                    for (String key : attrs.keySet()) {
                        cqlNode.setAttribute(key, attrs.get(key));
                    }
                }

                Text cqlText = doc.createTextNode(cqlMap.get(id).getNamedCQL());
                cqlNode.appendChild(cqlText);
                cqlMapperNode.appendChild(cqlNode);
            }

            doc.appendChild(cqlMapperNode);

            if (!file.exists()) {
                file.createNewFile();
            }

            os = new FileOutputStream(file);

            XMLUtil.transform(doc, os);

            os.flush();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(os);
        }
    }

    @Override
    public int hashCode() {
        return cqlMap.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj || (obj instanceof CQLMapper && N.equals(((CQLMapper) obj).cqlMap, cqlMap));
    }

    @Override
    public String toString() {
        return cqlMap.toString();
    }
}
