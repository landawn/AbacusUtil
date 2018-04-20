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

import static com.landawn.abacus.util.CodeGenerator.EntityMode.EXTEND_DIRTY_MARKER;
import static com.landawn.abacus.util.CodeGenerator.EntityMode.IMPL_DIRTY_MARKER;
import static com.landawn.abacus.util.WD._PERIOD;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Serializable;
import java.io.Writer;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Objects;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;

import javax.xml.bind.annotation.XmlTransient;

import org.w3c.dom.Comment;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.core.AbstractDirtyMarker;
import com.landawn.abacus.core.DirtyMarkerImpl;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.metadata.Column;
import com.landawn.abacus.metadata.ColumnType;
import com.landawn.abacus.metadata.Database;
import com.landawn.abacus.metadata.EntityDefXmlEle.DatabaseEle;
import com.landawn.abacus.metadata.EntityDefXmlEle.EntityDefEle;
import com.landawn.abacus.metadata.EntityDefXmlEle.EntityDefEle.EntityEle;
import com.landawn.abacus.metadata.EntityDefXmlEle.EntityDefEle.EntityEle.PropertyEle;
import com.landawn.abacus.metadata.EntityDefXmlEle.EntityDefEle.EntityEle.TableEle;
import com.landawn.abacus.metadata.EntityDefXmlEle.EntityDefEle.EntityEle.TableEle.ColumnEle;
import com.landawn.abacus.metadata.EntityDefinition;
import com.landawn.abacus.metadata.EntityDefinitionFactory;
import com.landawn.abacus.metadata.Property;
import com.landawn.abacus.metadata.Table;
import com.landawn.abacus.metadata.sql.SQLEntityDefinition;
import com.landawn.abacus.metadata.sql.SQLEntityDefinitionFactory;
import com.landawn.abacus.type.HBaseColumnType;
import com.landawn.abacus.type.ObjectType;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.type.TypeType;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class CodeGenerator {
    private static final Logger logger = LoggerFactory.getLogger(CodeGenerator.class);

    /**
     * To generate the entity classes which not depend on <code>N.class</code> in the codes of <code>hashcode/equals/toString</code> methods, specify the parameter <code>utilClassForHashEqualsToString</code> in method <code>entityDefinitionXml2Class</code> with this value <code>_N</code>.
     */
    public static final Class<?> _N = _N.class;

    private static final String POSTFIX_OF_JAVA_FILE = ".java";

    /**
     * Field TABLE_NAME2ENTITY_NAME. (value is ""tableName2EntityName"")
     */
    private static final String TABLE_NAME2ENTITY_NAME = "tableName2EntityName";

    /**
     * Field COLUMN_NAME2PROP_NAME. (value is ""columnName2PropName"")
     */
    private static final String COLUMN_NAME2PROP_NAME = "columnName2PropName";

    /**
     * Field PROP_NAME2VAR_NAME. (value is ""propName2VarName"")
     */
    private static final String PROP_NAME2VAR_NAME = "propName2VarName";

    /**
     * Field PROP_NAME2METHOD_NAME. (value is ""propName2MethodName"")
     */
    private static final String PROP_NAME2METHOD_NAME = "propName2MethodName";

    /**
     * Field UTF_8
     */
    private static final String UTF_8 = "UTF-8";

    /**
     * Field DOMAIN_NAME_VAR. (value is """_DN"")
     */
    private static final String DOMAIN_NAME_VAR = "_DN";

    /**
     * Field ENTITY_NAME_VAR. (value is ""__"")
     */
    private static final String ENTITY_NAME_VAR = "__";

    /**
     * Field PROP_NAME_LIST. (value is ""_PNL"")
     */
    private static final String PROP_NAME_LIST = "_PNL";

    /**
     * Field PROP_NAME_MAP. (value is ""_PNM"")
     */
    // private static final String PROP_NAME_MAP = "_PNM";

    /**
     * Field PROP_COLUMN_MAP. (value is ""_PCM"")
     */
    // private static final String PROP_COLUMN_MAP = "_PCM";

    /**
     * Field COLUMN_NAME_LIST. (value is ""_CNL"")
     */
    private static final String COLUMN_NAME_LIST = "_CNL";

    /**
     * Field COLUMN_NAME_MAP. (value is ""_CNM"")
     */
    // private static final String COLUMN_NAME_MAP = "_CNM";

    /**
     * Field COLUMN_PROP_MAP. (value is ""_CPM"")
     */
    // private static final String COLUMN_PROP_MAP = "_CPM";

    /**
     * Field POSTFIX_OF_PROP_NAME_LIST. (value is ""PNL"")
     */
    private static final String POSTFIX_OF_PROP_NAME_LIST = "PNL";

    /**
     * Field POSTFIX_OF_COLUMN_NAME_LIST. (value is ""CNL"")
     */
    private static final String POSTFIX_OF_COLUMN_NAME_LIST = "CNL";

    /**
     * The field name of <code>DirtyMarker</code> implementation in a generated entity class which implements the <code>DirtyMarker</code> interface. (value is ""dirtyMarkerImpl"")
     */
    private static final String DIRTY_MARKER_IMPL_FIELD_NAME = "dirtyMarkerImpl";

    /**
     * Field USUAL_TYPES.
     */
    private static Set<String> USUAL_TYPES = new LinkedHashSet<>();

    static {
        USUAL_TYPES.add("java.lang");
        USUAL_TYPES.add("java.util");
        USUAL_TYPES.add("java.util.concurrent");
        USUAL_TYPES.add("java.time");
        USUAL_TYPES.add("java.io");
        USUAL_TYPES.add("java.nio");
        USUAL_TYPES.add("java.sql");
        USUAL_TYPES.add("java.net");
        USUAL_TYPES.add("java.math");
        USUAL_TYPES.add("javax.xml");
        USUAL_TYPES.add(HBaseColumn.class.getCanonicalName());
    }

    private static Map<String, String> JAVA_TYPE_PROP_NAME = new HashMap<>();

    static {
        JAVA_TYPE_PROP_NAME.put(boolean.class.getName(), "boolean_");
        JAVA_TYPE_PROP_NAME.put(char.class.getName(), "char_");
        JAVA_TYPE_PROP_NAME.put(byte.class.getName(), "byte_");
        JAVA_TYPE_PROP_NAME.put(short.class.getName(), "short_");
        JAVA_TYPE_PROP_NAME.put(int.class.getName(), "int_");
        JAVA_TYPE_PROP_NAME.put(long.class.getName(), "long_");
        JAVA_TYPE_PROP_NAME.put(float.class.getName(), "float_");
        JAVA_TYPE_PROP_NAME.put(double.class.getName(), "double_");
        JAVA_TYPE_PROP_NAME.put("class", "class_");
    }

    /**
     * Constructor for CodeGenerator.
     */
    private CodeGenerator() {
        // No instance.
    }

    static String makePackageFolder(String srcPath, final String pkgName) {
        srcPath = (srcPath.endsWith("/") || srcPath.endsWith("\\")) ? srcPath : (srcPath + File.separator);

        String classFilePath = (pkgName == null) ? srcPath : (srcPath + pkgName.replace('.', File.separatorChar) + File.separator);
        File classFileFolder = new File(classFilePath);

        if (!classFileFolder.exists()) {
            classFileFolder.mkdirs();
        }

        return classFilePath;
    }

    static void database2Xml(final Database database, final File xml, final Map<String, String> databaseElementAttrs) {
        Document doc = XMLUtil.createDOMParser(true, true).newDocument();
        Element databaseNode = doc.createElement(DatabaseEle.DATABASE);
        database2Node(database, databaseNode, databaseElementAttrs);
        doc.appendChild(databaseNode);
        XMLUtil.transform(doc, xml);
    }

    static Element database2Node(final Database database, final Element node, final Map<String, String> databaseElementAttrs) {
        Document doc = node.getOwnerDocument();
        Element databaseNode = null;

        if (DatabaseEle.DATABASE.equals(node.getTagName())) {
            databaseNode = node;
        } else {
            databaseNode = doc.createElement(DatabaseEle.DATABASE);
            node.appendChild(databaseNode);
        }

        setAttributes(databaseNode, databaseElementAttrs);

        databaseNode.setAttribute(DatabaseEle.NAME, database.getName());

        Collection<String> tableNames = database.getTableNameList();

        for (String tableName : tableNames) {
            Table table = database.getTable(tableName);
            Element tableNode = table2Node(table, databaseNode);
            tableNode.removeAttribute(DatabaseEle.DATABASE);
        }

        return databaseNode;
    }

    static Element table2Node(final Table table, final Element node) {
        Document doc = node.getOwnerDocument();
        Element tableNode = null;

        if (TableEle.TABLE.equals(node.getTagName())) {
            tableNode = node;
        } else {
            tableNode = doc.createElement(TableEle.TABLE);
            node.appendChild(tableNode);
        }

        for (Map.Entry<String, String> entry : table.getAttributes().entrySet()) {
            tableNode.setAttribute(entry.getKey(), entry.getValue());
        }

        Collection<String> columnNames = table.getColumnNameList();

        for (String columnName : columnNames) {
            Element columnNode = doc.createElement(ColumnEle.COLUMN);
            Column column = table.getColumn(columnName);

            for (Map.Entry<String, String> entry : column.getAttributes().entrySet()) {
                columnNode.setAttribute(entry.getKey(), entry.getValue());
            }

            columnNode.setAttribute(ColumnEle.NAME, column.getName());
            tableNode.appendChild(columnNode);
        }

        return tableNode;
    }

    static void table2Xml(final Table table, final File xml) {
        Document doc = XMLUtil.createDOMParser(true, true).newDocument();
        Element tableNode = doc.createElement(TableEle.TABLE);
        table2Node(table, tableNode);

        doc.appendChild(tableNode);

        XMLUtil.transform(doc, xml);
    }

    public static void database2EntityDefinitionXml(final Database database, final File entityDefinitionXml) {
        database2EntityDefinitionXml(database, entityDefinitionXml, null);
    }

    public static void database2EntityDefinitionXml(final Database database, final File entityDefinitionXml, final Map<String, String> entityDefAttrs) {
        Map<String, String> newEntityNames = new HashMap<>();
        Map<String, String> newPropNames = new HashMap<>();
        database2EntityDefinitionXml(database, entityDefinitionXml, entityDefAttrs, newEntityNames, newPropNames, null);
    }

    public static void database2EntityDefinitionXml(final Database database, final File entityDefinitionXml, final Map<String, String> entityDefAttrs,
            final Map<String, String> newEntityNames, final Map<String, String> newPropNames, final Map<String, Set<String>> excludedeEntityPropNames) {
        Map<String, Map<String, String>> newEntityPropNames = new HashMap<>();
        Collection<Table> tables = database.getTableList();

        for (Table table : tables) {
            String entityName = table.getName();

            if (newEntityNames.get(entityName) != null) {
                entityName = newEntityNames.get(entityName);
            }

            newEntityPropNames.put(entityName, newPropNames);
        }

        database2EntityDefinitionXml(database, entityDefinitionXml, entityDefAttrs, null, newEntityNames, null, newEntityPropNames, excludedeEntityPropNames);
    }

    public static void database2EntityDefinitionXml(final Database database, final File entityDefinitionXml, final Map<String, String> entityDefAttrs,
            Method tableName2EntityName, final Method columnName2PropName, final Map<String, Set<String>> excludedeEntityPropNames) {
        if (tableName2EntityName == null) {
            tableName2EntityName = ClassUtil.getDeclaredMethod(CodeGenerator.class, TABLE_NAME2ENTITY_NAME, String.class);
        }

        Map<String, Method> entityColumnName2PropName = new HashMap<>();
        Collection<Table> tables = database.getTableList();

        for (Table table : tables) {
            String entityName = (String) ClassUtil.invokeMethod(tableName2EntityName, table.getName());

            entityColumnName2PropName.put(entityName, columnName2PropName);
        }

        database2EntityDefinitionXml(database, entityDefinitionXml, entityDefAttrs, tableName2EntityName, null, entityColumnName2PropName, null,
                excludedeEntityPropNames);
    }

    static void database2EntityDefinitionXml(final Database database, final File entityDefinitionXml, final Map<String, String> entityDefAttrs,
            Method tableName2EntityName, Map<String, String> newEntityNames, Map<String, Method> entityColumnName2PropName,
            Map<String, Map<String, String>> newEntityPropNames, Map<String, Set<String>> excludedeEntityPropNames) {
        if (tableName2EntityName == null) {
            tableName2EntityName = ClassUtil.getDeclaredMethod(CodeGenerator.class, TABLE_NAME2ENTITY_NAME, String.class);
        }

        if (newEntityNames == null) {
            newEntityNames = new HashMap<>();
        }

        if (entityColumnName2PropName == null) {
            entityColumnName2PropName = new HashMap<>();
        }

        if (newEntityPropNames == null) {
            newEntityPropNames = new HashMap<>();
        }

        if (excludedeEntityPropNames == null) {
            excludedeEntityPropNames = new HashMap<>();
        }

        Document doc = XMLUtil.createDOMParser(true, true).newDocument();
        Element entityDefNode = doc.createElement(EntityDefEle.ENTITY_DEF);
        Document existedDoc = null;

        if (entityDefinitionXml.exists()) {
            try {
                existedDoc = XMLUtil.createDOMParser(true, true).parse(entityDefinitionXml);
            } catch (Exception e) {
                throw new AbacusException(e);
            }

            NodeList nodes = existedDoc.getElementsByTagName(EntityDefEle.ENTITY_DEF);

            if ((nodes != null) && (nodes.getLength() > 0)) {
                NamedNodeMap attrs = nodes.item(0).getAttributes();

                for (int i = 0; i < attrs.getLength(); i++) {
                    entityDefNode.setAttribute(attrs.item(i).getNodeName(), attrs.item(i).getNodeValue());
                }
            }
        }

        setAttributes(entityDefNode, entityDefAttrs);

        String packageAttr = entityDefNode.getAttribute(EntityDefEle.PACKAGE);

        if ((packageAttr == null) || (packageAttr.length() == 0)) {
            entityDefNode.setAttribute(EntityDefEle.PACKAGE, database.getName());
        }

        Map<String, Element> existedEntityEleList = getEntityElementList(existedDoc);
        Set<String> entityNameList = new HashSet<>();
        Collection<Table> tables = database.getTableList();

        for (Table table : tables) {
            String entityName = (String) ClassUtil.invokeMethod(tableName2EntityName, table.getName());

            if (newEntityNames.get(entityName) != null) {
                entityName = newEntityNames.get(entityName);
            }

            Method columnName2PropName = entityColumnName2PropName.get(entityName);

            if (columnName2PropName == null) {
                for (String key : entityColumnName2PropName.keySet()) {
                    if (entityName.matches(key)) {
                        columnName2PropName = entityColumnName2PropName.get(key);
                    }
                }
            }

            if (columnName2PropName == null) {
                columnName2PropName = ClassUtil.getDeclaredMethod(CodeGenerator.class, COLUMN_NAME2PROP_NAME, String.class);
            }

            Map<String, String> newPropNames = newEntityPropNames.get(entityName);

            if (newPropNames == null) {
                for (String key : newEntityPropNames.keySet()) {
                    if (entityName.matches(key)) {
                        newPropNames = newEntityPropNames.get(key);
                    }
                }
            }

            Set<String> excludedPropNames = excludedeEntityPropNames.get(entityName);

            if (excludedPropNames == null) {
                for (String key : excludedeEntityPropNames.keySet()) {
                    if (entityName.matches(key)) {
                        excludedPropNames = excludedeEntityPropNames.get(key);
                    }
                }
            }

            Element importedEntityEelement = getEntityEleByName(entityName, existedEntityEleList);
            EntityDefinition entityDef = new SQLEntityDefinition(entityName, table, columnName2PropName);
            entityDefinition2Node(entityDef, entityDefNode, newPropNames, excludedPropNames, importedEntityEelement);

            entityNameList.add(entityName.toUpperCase());
        }

        for (String entityEleName : existedEntityEleList.keySet()) {
            if (!entityNameList.contains(entityEleName.toUpperCase())) {
                Node importedNode = doc.importNode(existedEntityEleList.get(entityEleName), true);
                entityDefNode.appendChild(importedNode);
            }
        }

        Comment comment = doc.createComment("Generated by Abacus");
        doc.appendChild(comment);
        doc.appendChild(entityDefNode);

        XMLUtil.transform(doc, entityDefinitionXml);
    }

    static void setAttributes(final Element element, final Map<String, String> attrs) {
        if (attrs != null) {
            for (String attrName : attrs.keySet()) {
                element.setAttribute(attrName, attrs.get(attrName));
            }
        }
    }

    /**
     * @param entityDef
     * @param node
     * @param newPropNames
     * @param excludedPropNames
     * @param importedEntityEelement
     */
    static void entityDefinition2Node(final EntityDefinition entityDef, final Element node, Map<String, String> newPropNames, Set<String> excludedPropNames,
            final Element importedEntityEelement) {
        Document doc = node.getOwnerDocument();
        Element entityNode = null;

        if (newPropNames == null) {
            newPropNames = new HashMap<>();
        }

        if (excludedPropNames == null) {
            excludedPropNames = new HashSet<>();
        } else {
            Set<String> tempList = new HashSet<>(excludedPropNames);

            for (String propName : excludedPropNames) {
                tempList.add(propName.toUpperCase());
            }

            excludedPropNames = tempList;
        }

        if (EntityEle.ENTITY.equals(node.getTagName())) {
            entityNode = node;
        } else {
            entityNode = doc.createElement(EntityEle.ENTITY);
            node.appendChild(doc.createComment(entityDef.getName()));
            node.appendChild(entityNode);
        }

        for (Map.Entry<String, String> entry : entityDef.getAttributes().entrySet()) {
            entityNode.setAttribute(entry.getKey(), entry.getValue());
        }

        if (entityDef.getIdPropertyList().size() > 0) {
            String idAttr = "";

            for (Property idProp : entityDef.getIdPropertyList()) {
                if (excludedPropNames.contains(idProp.getName()) || excludedPropNames.contains(idProp.getName().toUpperCase())) {
                    continue;
                }

                if (idAttr.length() > 0) {
                    idAttr += ", ";
                }

                idAttr += idProp.getName();
            }

            if (N.notNullOrEmpty(idAttr)) {
                entityNode.setAttribute(EntityEle.ID, idAttr);
            } else {
                entityNode.removeAttribute(EntityEle.ID);
            }
        }

        Element propsNode = doc.createElement(EntityEle.PROPERTIES);

        Collection<Property> props = entityDef.getPropertyList();

        for (Property prop : props) {
            String propName = prop.getName();

            if (excludedPropNames.contains(propName) || excludedPropNames.contains(propName.toUpperCase())) {
                continue;
            }

            if ((newPropNames.get(propName) != null)) {
                propName = newPropNames.get(propName);
            }

            Element propNode = null;

            if (prop.isList()) {
                propNode = doc.createElement(PropertyEle.LIST);
            } else if (prop.isSet()) {
                propNode = doc.createElement(PropertyEle.SET);
            } else {
                propNode = doc.createElement(PropertyEle.PROPERTY);
            }

            for (Map.Entry<String, String> entry : prop.getAttributes().entrySet()) {
                propNode.setAttribute(entry.getKey(), entry.getValue());
            }

            propNode.setAttribute(PropertyEle.NAME, propName);
            propNode.setAttribute(PropertyEle.TYPE, prop.getType().name());

            propNode.removeAttribute(PropertyEle.COLLECTION);
            propsNode.appendChild(propNode);
        }

        entityNode.appendChild(propsNode);

        if (importedEntityEelement != null) {
            NamedNodeMap attrs = importedEntityEelement.getAttributes();

            for (int i = 0; i < attrs.getLength(); i++) {
                Node attr = attrs.item(i);
                entityNode.setAttribute(attr.getNodeName(), attr.getNodeValue());
            }

            try {
                List<Element> propElementList = XMLUtil.getElementsByTagName(propsNode, PropertyEle.PROPERTY);

                NodeList nodeList = importedEntityEelement.getChildNodes();

                for (int i = 0; i < nodeList.getLength(); i++) {
                    if (nodeList.item(i) instanceof Element) {
                        Element ele = (Element) nodeList.item(i);

                        if (EntityEle.PROPERTIES.equals(ele.getTagName())) {
                            List<Element> importedPropEleList = new ArrayList<>();
                            NodeList propNodeList = ele.getChildNodes();

                            for (int j = 0; j < propNodeList.getLength(); j++) {
                                if (propNodeList.item(j) instanceof Element) {
                                    importedPropEleList.add((Element) propNodeList.item(j));
                                }
                            }

                            for (Element importedPropEle : importedPropEleList) {
                                boolean isExistedProp = false;

                                if (importedPropEle.getTagName().equals(PropertyEle.PROPERTY)) {
                                    String columnName = importedPropEle.getAttribute(PropertyEle.COLUMN);

                                    for (int k = 0; k < propElementList.size(); k++) {
                                        Element propEle = (propElementList.get(k));

                                        if (columnName.equalsIgnoreCase(propEle.getAttribute(PropertyEle.COLUMN))) {
                                            NamedNodeMap nnm = importedPropEle.getAttributes();

                                            for (int j = 0; j < nnm.getLength(); j++) {
                                                propEle.setAttribute(nnm.item(j).getNodeName(), nnm.item(j).getNodeValue());
                                            }

                                            isExistedProp = true;

                                            break;
                                        }
                                    }
                                }

                                if (!isExistedProp) {
                                    Node importedNode = doc.importNode(importedPropEle, true);
                                    propsNode.appendChild(importedNode);
                                }
                            }
                        } else {
                            // objectNode.getOwnerDocument().adoptNode(addedEle);
                            Node importedNode = doc.importNode(ele, true);
                            entityNode.appendChild(importedNode);
                        }
                    }
                }
            } catch (Exception e) {
                throw new AbacusException(e);
            }
        }
    }

    private static Map<String, Element> getEntityElementList(final Document doc) {
        Map<String, Element> result = new HashMap<>();

        if (doc != null) {
            List<Element> entityDefElementList = XMLUtil.getElementsByTagName(doc.getDocumentElement(), EntityDefEle.ENTITY_DEF);
            List<Element> entityElementList = null;

            if (entityDefElementList.size() > 0) {
                entityElementList = XMLUtil.getElementsByTagName((entityDefElementList.get(0)), EntityEle.ENTITY);
            } else {
                entityElementList = XMLUtil.getElementsByTagName(doc.getDocumentElement(), EntityEle.ENTITY);
            }

            for (int i = 0; i < entityElementList.size(); i++) {
                String entityName = XMLUtil.getAttribute(entityElementList.get(i), EntityEle.NAME);
                result.put(entityName, entityElementList.get(i));
            }
        }

        return result;
    }

    private static Element getEntityEleByName(final String entityName, final Map<String, Element> entityEleList) {
        Element importedElement = null;

        for (String entityEleName : entityEleList.keySet()) {
            if (entityName.equalsIgnoreCase(entityEleName)) {
                importedElement = entityEleList.get(entityEleName);

                break;
            }
        }

        return importedElement;
    }

    protected static String getSimplePropNameTableClassName(final String className) {
        String simpleClassName = className;
        int index = className.lastIndexOf(WD._PERIOD);

        if (index > -1) {
            simpleClassName = className.substring(index + 1);
        }

        return simpleClassName;
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode) {
        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, null, null, true);
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod) {
        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, propName2VarName, propName2MethodName, fluentSetMethod, null);
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod, final Class<?> extendedClass,
            final Class<?>... implementedInterfaces) {
        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, propName2VarName, propName2MethodName, fluentSetMethod, extendedClass,
                N.asList(implementedInterfaces), true, true);
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod, final Class<?> extendedClass,
            final List<Class<?>> implementedInterfaces, final boolean generateHashEqualsMethod, final boolean generateToStringMethod) {
        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, propName2VarName, propName2MethodName, fluentSetMethod, extendedClass,
                implementedInterfaces, generateHashEqualsMethod, generateToStringMethod, generateHashEqualsMethod, generateToStringMethod, Objects.class);
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod, final Class<?> extendedClass,
            final List<Class<?>> implementedInterfaces, final boolean generateHashEqualsMethod, final boolean generateToStringMethod,
            final boolean hashEqualsWithParentProperties, final boolean toStringWithParentProperties, final Class<?> utilClassForHashEqualsToString) {
        final EntityDefinitionFactory entityDefinitionFactory = SQLEntityDefinitionFactory.newInstance(domainName, entityDefinitionXml);

        Map<String, Class<?>> entityExtendedClasses = null;

        if (extendedClass != null) {
            entityExtendedClasses = new HashMap<>();
            for (EntityDefinition entityDef : entityDefinitionFactory.getDefinitionList()) {
                entityExtendedClasses.put(entityDef.getName(), extendedClass);
            }
        }

        Map<String, List<Class<?>>> entityImplementedInterfaces = null;

        if (N.notNullOrEmpty(implementedInterfaces)) {
            entityImplementedInterfaces = new HashMap<>();
            for (EntityDefinition entityDef : entityDefinitionFactory.getDefinitionList()) {
                entityImplementedInterfaces.put(entityDef.getName(), implementedInterfaces);
            }
        }

        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, propName2VarName, propName2MethodName, fluentSetMethod,
                entityExtendedClasses, entityImplementedInterfaces, generateHashEqualsMethod, generateToStringMethod, hashEqualsWithParentProperties,
                toStringWithParentProperties, utilClassForHashEqualsToString);
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod, final Map<String, Class<?>> entityExtendedClasses,
            final Map<String, List<Class<?>>> entityImplementedInterfaces) {
        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, propName2VarName, propName2MethodName, fluentSetMethod,
                entityExtendedClasses, entityImplementedInterfaces, true, true);
    }

    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod, final Map<String, Class<?>> entityExtendedClasses,
            final Map<String, List<Class<?>>> entityImplementedInterfaces, final boolean generateHashEqualsMethod, final boolean generateToStringMethod) {
        entityDefinitionXml2Class(domainName, entityDefinitionXml, srcPath, entityMode, propName2VarName, propName2MethodName, fluentSetMethod,
                entityExtendedClasses, entityImplementedInterfaces, generateHashEqualsMethod, generateToStringMethod, generateHashEqualsMethod,
                generateToStringMethod, Objects.class);
    }

    /**
     *
     * @param domainName
     * @param entityDefinitionXml
     * @param srcPath
     * @param entityMode
     * @param propName2VarName
     * @param propName2MethodName
     * @param fluentSetMethod <code>true</code> to generate the setter methods with returning <code>this</code> to support fluent setter. For example: <code>account.setFirstName("fn").setLastName("ln")...</code>
     * @param entityExtendedClasses
     * @param entityImplementedInterfaces
     * @param generateHashEqualsMethod
     * @param generateToStringMethod
     * @param hashEqualsWithParentProperties
     * @param toStringWithParentProperties
     * @param utilClassForHashEqualsToString is <code>Objects.class</code> by default. It can also be <code>N.class</code> or any classes else which provide the {@code hashCode/equals/toString} method.
     *      Or specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.jar for Methods {@code hashCode/equals/toString}.
     */
    public static void entityDefinitionXml2Class(final String domainName, final File entityDefinitionXml, final String srcPath, final EntityMode entityMode,
            Method propName2VarName, Method propName2MethodName, final boolean fluentSetMethod, Map<String, Class<?>> entityExtendedClasses,
            Map<String, List<Class<?>>> entityImplementedInterfaces, final boolean generateHashEqualsMethod, final boolean generateToStringMethod,
            final boolean hashEqualsWithParentProperties, final boolean toStringWithParentProperties, final Class<?> utilClassForHashEqualsToString) {

        final EntityDefinitionFactory entityDefinitionFactory = SQLEntityDefinitionFactory.newInstance(domainName, entityDefinitionXml);
        final String pkgName = entityDefinitionFactory.getAttribute(EntityDefEle.PACKAGE);

        if ((pkgName == null) || (pkgName.length() == 0)) {
            throw new AbacusException("the package attribute in 'entityDef' element can't be null or empty");
        }
        //
        //        if (generatePropNameTable && ((domainName == null) || (domainName.length() == 0))) {
        //            throw new AbacusException("the domain attribute in 'entityDef' element can't be null or empty");
        //        }

        if (entityMode == null) {
            throw new AbacusException("entityMode can't be null");
        }

        if (propName2VarName == null) {
            propName2VarName = ClassUtil.getDeclaredMethod(CodeGenerator.class, PROP_NAME2VAR_NAME, String.class);
        }

        if (propName2MethodName == null) {
            propName2MethodName = ClassUtil.getDeclaredMethod(CodeGenerator.class, PROP_NAME2METHOD_NAME, String.class);
        }

        if (entityExtendedClasses == null) {
            entityExtendedClasses = new HashMap<>();
        }

        if (entityImplementedInterfaces == null) {
            entityImplementedInterfaces = new HashMap<>();
        }

        final Class<?> utilClass = utilClassForHashEqualsToString == null ? Objects.class : utilClassForHashEqualsToString;
        final String classFilePath = makePackageFolder(srcPath, pkgName);

        if (_N.equals(utilClass)) {
            File utilClassFile = new File(classFilePath + ClassUtil.getSimpleClassName(_N) + POSTFIX_OF_JAVA_FILE);
            if (!utilClassFile.exists()) {
                String sourceCode = _N_STRING.replaceFirst("package com.landawn.abacus.util;", N.isNullOrEmpty(pkgName) ? "" : "package " + pkgName + ";");
                IOUtil.write(utilClassFile, sourceCode);
            }
        }

        String propNameTableClass = null;

        if (entityMode != EntityMode.POJO) {
            entityDefinitionXml2PropNameTable(domainName, entityDefinitionXml, srcPath, propName2VarName, utilClass.equals(Objects.class) ? _N : utilClass);
            propNameTableClass = getPropNameTableClassName(entityDefinitionFactory);
        }

        final ExecutorService executorService = Executors.newFixedThreadPool(8);
        final AtomicInteger activeThreadNum = new AtomicInteger();

        for (EntityDefinition entityDef : entityDefinitionFactory.getDefinitionList()) {
            if (entityDef.isSliceEntity()) {
                continue;
            }

            final EntityDefinition entityDef2 = entityDef;
            final String propNameTableClass2 = propNameTableClass;
            final Method propName2VarName2 = propName2VarName;
            final Method propName2MethodName2 = propName2MethodName;
            final File classFile = new File(classFilePath + entityDef.getName() + POSTFIX_OF_JAVA_FILE);
            final Class<?> extendedClass = entityExtendedClasses.get(entityDef.getName());
            final List<Class<?>> implementedInterfaces = entityImplementedInterfaces.get(entityDef.getName());

            activeThreadNum.incrementAndGet();

            executorService.execute(new Runnable() {
                @Override
                public void run() {
                    try {
                        entityDefinition2Class(entityDef2, classFile, entityMode, propNameTableClass2, propName2VarName2, propName2MethodName2, extendedClass,
                                implementedInterfaces, fluentSetMethod, generateHashEqualsMethod, generateToStringMethod, hashEqualsWithParentProperties,
                                toStringWithParentProperties, utilClass);
                    } finally {
                        activeThreadNum.decrementAndGet();
                    }
                }
            });
        }

        while (activeThreadNum.get() > 0) {
            N.sleep(1);
        }
    }

    private static String getPropNameTableClassName(final EntityDefinitionFactory entityDefinitionFactory) {
        String propNameTableClass = POSTFIX_OF_PROP_NAME_LIST;
        String domainName = entityDefinitionFactory.domainName();

        if (N.notNullOrEmpty(domainName)) {
            propNameTableClass = N.capitalize(domainName) + propNameTableClass;
        }

        String pkgName = entityDefinitionFactory.getAttribute(EntityDefEle.PACKAGE);

        if (pkgName != null) {
            propNameTableClass = pkgName + '.' + propNameTableClass;
        }

        return propNameTableClass;
    }

    static void entityDefinition2Class(final EntityDefinition entityDef, final File classFile, final EntityMode entityMode, final String propNameTableClass,
            final Method propName2VarName, final Method propName2MethodName, Class<?> extendedClass, List<Class<?>> implementedInterfaces,
            final boolean fluentSetMethod, boolean generateHashEqualsMethod, boolean generateToStringMethod, boolean hashEqualsWithParentProperties,
            boolean toStringWithParentProperties, final Class<?> utilClass) {

        final String pkgName = getPackageName(entityDef);

        String attr = entityDef.getAttribute("extends");
        if (N.notNullOrEmpty(attr)) {
            String className = attr;
            try {
                try {
                    ClassUtil.forClass(className);
                } catch (Throwable e) {
                    className = attr.indexOf('.') > 0 ? attr : pkgName + "." + attr;
                }

                extendedClass = ClassUtil.forClass(className);
            } catch (Throwable e) {
                // ignore for second time run.
                if (logger.isWarnEnabled()) {
                    logger.warn("No class found by name: " + className + ". Please run it again after the generated codes are compiled");
                }
            }
        }

        attr = entityDef.getAttribute("implements");
        if (N.notNullOrEmpty(attr)) {
            implementedInterfaces = new ArrayList<>();
            final List<String> strList = Splitter.with(",").trim(true).split(attr);

            for (String str : strList) {
                String className = str;

                try {
                    try {
                        ClassUtil.forClass(className);
                    } catch (Throwable e) {
                        className = str.indexOf('.') > 0 ? str : pkgName + "." + str;
                    }

                    implementedInterfaces.add(ClassUtil.forClass(className));
                } catch (Throwable e) {
                    // ignore for second time run.

                    if (logger.isWarnEnabled()) {
                        logger.warn("No class found by name: " + className + ". Please run it again after the generated codes are compiled");
                    }
                }
            }
        }

        if (implementedInterfaces == null) {
            implementedInterfaces = new ArrayList<>();
        }

        String generateHashEqualsMethodAttr = entityDef.getAttribute("generateHashEqualsMethod");

        if (N.notNullOrEmpty(generateHashEqualsMethodAttr)) {
            generateHashEqualsMethod = Boolean.valueOf(generateHashEqualsMethodAttr);
        }

        String generateToStringMethodAttr = entityDef.getAttribute("generateToStringMethod");

        if (N.notNullOrEmpty(generateToStringMethodAttr)) {
            generateToStringMethod = Boolean.valueOf(generateToStringMethodAttr);
        }

        String hashEqualsWithParentPropertiesAttr = entityDef.getAttribute("hashEqualsWithParentProperties");

        if (N.notNullOrEmpty(hashEqualsWithParentPropertiesAttr)) {
            hashEqualsWithParentProperties = Boolean.valueOf(hashEqualsWithParentPropertiesAttr);
        }

        String toStringWithParentPropertiesAttr = entityDef.getAttribute("toStringWithParentProperties");

        if (N.notNullOrEmpty(toStringWithParentPropertiesAttr)) {
            toStringWithParentProperties = Boolean.valueOf(toStringWithParentPropertiesAttr);
        }

        Writer fileWrite = null;

        try {
            if (EntityMode.POJO == entityMode || EntityMode.POJO_WITH_PROP_NAME_TABLE == entityMode) {
                // do something.
            } else if (EntityMode.EXTEND_DIRTY_MARKER == entityMode) {
                if (extendedClass == null) {
                    extendedClass = AbstractDirtyMarker.class;
                } else {
                    if (!AbstractDirtyMarker.class.isAssignableFrom(extendedClass)) {
                        throw new AbacusException(
                                "the class: " + extendedClass.getCanonicalName() + " must extend " + AbstractDirtyMarker.class.getCanonicalName());
                    }
                }
            } else if (EntityMode.IMPL_DIRTY_MARKER == entityMode) {
                if (extendedClass == null) {
                    extendedClass = DirtyMarkerImpl.class;
                }

                if (implementedInterfaces.size() == 0) {
                    implementedInterfaces.add(DirtyMarker.class);
                }
            }

            Map<String, List<String>> annotationLinesMapper = new HashMap<>();
            Set<Class<?>> annotationImportClasses = new LinkedHashSet<>();

            if (classFile.exists()) {
                List<String> lines = IOUtil.readLines(classFile);
                List<String> annotationLines = null;
                for (String line : lines) {
                    if (line.startsWith("import")) {
                        String str = line.trim().substring(6);
                        str = str.trim();
                        str = str.substring(0, str.length() - 1);

                        try {
                            Class<?> cls = ClassUtil.forClass(str);

                            if (cls.isAnnotation()) {
                                annotationImportClasses.add(cls);
                            }
                        } catch (Exception e) {
                            // ignore.
                        }
                    } else if (N.notNullOrEmpty(line) && line.length() > 4) {
                        boolean isAnnotationLine = false;

                        for (int i = 0, len = line.length(); i < len; i++) {
                            char ch = line.charAt(i);

                            if (ch == ' ' || ch == '\t') {
                                continue;
                            }

                            if (ch == '@') {
                                isAnnotationLine = true;
                            }

                            break;
                        }

                        if (isAnnotationLine) {
                            if (annotationLines == null) {
                                annotationLines = new ArrayList<>();
                            }

                            annotationLines.add(line);
                        } else if (N.notNullOrEmpty(annotationLines) && line.trim().length() > 0) {
                            annotationLinesMapper.put(line, annotationLines);
                            annotationLines = null;
                        }
                    }
                }
            } else {
                classFile.createNewFile();
            }

            fileWrite = newFileWriter(classFile);

            writeFileHead(fileWrite);

            writePackageName(pkgName, fileWrite);

            final Map<String, Class<?>> importedClasses = writeImport(entityDef, pkgName, propNameTableClass, entityMode, extendedClass, implementedInterfaces,
                    fileWrite, generateHashEqualsMethod, generateToStringMethod, utilClass, annotationImportClasses);

            fileWrite.write(IOUtil.LINE_SEPARATOR);
            fileWrite.write(IOUtil.LINE_SEPARATOR);

            writeClassComment(fileWrite);

            writeClass(entityDef, pkgName, propNameTableClass, propName2VarName, propName2MethodName, entityMode, extendedClass, implementedInterfaces, "",
                    fileWrite, generateHashEqualsMethod, generateToStringMethod, hashEqualsWithParentProperties, toStringWithParentProperties, utilClass,
                    fluentSetMethod, importedClasses);

            fileWrite.flush();

            if (N.notNullOrEmpty(annotationLinesMapper)) {
                IOUtil.close(fileWrite);

                List<String> lines = IOUtil.readLines(classFile);
                List<String> newLines = new ArrayList<>(lines.size() + 64);

                for (String line : lines) {
                    List<String> annotationLines = annotationLinesMapper.get(line);

                    if (N.notNullOrEmpty(annotationLines)) {
                        if (annotationLines.size() == 2) {
                            N.println(annotationLines);
                        }

                        for (int len = newLines.size(), i = len - 1; i > N.max(0, len - 5); i--) {
                            String str = newLines.get(i);
                            if (str.indexOf('@') > 0) {
                                if (annotationLines.size() > 0 && !annotationLines.remove(str)) {
                                    String annoLine = annotationLines.iterator().next();
                                    String prefixSpaces = annoLine.substring(0, annoLine.indexOf('@'));
                                    str = prefixSpaces + str.trim();
                                    annotationLines.remove(str);
                                }
                            } else if (str.trim().length() > 0) {
                                break;
                            }
                        }

                        if (N.notNullOrEmpty(annotationLines)) {
                            //                            String prevLine = newLines.get(newLines.size() - 1).trim();
                            //
                            //                            if (prevLine.length() > 0 && (prevLine.charAt(0) != '/' && prevLine.charAt(0) != '*')) {
                            //                                newLines.add(N.EMPTY_STRING);
                            //                            }

                            newLines.addAll(annotationLines);
                        }
                    }

                    newLines.add(line);
                }

                fileWrite = newFileWriter(classFile);

                IOUtil.writeLines(fileWrite, newLines);

                fileWrite.flush();
            }
        } catch (Exception e) {
            throw new AbacusException(e);
        } finally {
            IOUtil.close(fileWrite);
        }
    }

    public static void entityDefinitionXml2PropNameTable(final String domainName, final File entityDefinitionXml, final String srcPath) {
        entityDefinitionXml2PropNameTable(domainName, entityDefinitionXml, srcPath, null, _N);
    }

    /**
     * @param domainName
     * @param entityDefinitionXml
     * @param srcPath
     * @param propName2VarName
     * @param utilClass  specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.java
     */
    public static void entityDefinitionXml2PropNameTable(String domainName, final File entityDefinitionXml, final String srcPath, Method propName2VarName,
            Class<?> utilClass) {
        if (propName2VarName == null) {
            propName2VarName = ClassUtil.getDeclaredMethod(CodeGenerator.class, PROP_NAME2VAR_NAME, String.class);
        }

        if (utilClass == null) {
            utilClass = _N;
        }

        domainName = N.nullToEmpty(domainName);
        EntityDefinitionFactory entityDefinitionFactory = SQLEntityDefinitionFactory.newInstance(domainName, entityDefinitionXml);

        String pkgName = entityDefinitionFactory.getAttribute(EntityDefEle.PACKAGE);

        String classFilePath = makePackageFolder(srcPath, pkgName);

        //        if (utilClass.equals(_N)) {
        //            File utilClassFile = new File(classFilePath + ClassUtil.getSimpleClassName(_N) + POSTFIX_OF_JAVA_FILE);
        //            if (!utilClassFile.exists()) {
        //                String sourceCode = _N_STRING.replaceFirst("com.landawn.abacus.util", pkgName);
        //                IOUtil.write(utilClassFile, sourceCode);
        //            }
        //        }

        String propNameTableClassName = getSimplePropNameTableClassName(getPropNameTableClassName(entityDefinitionFactory));
        File propNameDefClassFile = new File(classFilePath + propNameTableClassName + POSTFIX_OF_JAVA_FILE);

        entityDefinition2PropNameTable(entityDefinitionFactory, propNameDefClassFile, pkgName, propName2VarName, utilClass);
    }

    static void entityDefinition2PropNameTable(final EntityDefinitionFactory entityDefinitionFactory, final File propNameDefClassFile, final String pkgName,
            final Method propName2VarName, final Class<?> utilClass) {

        if (propNameDefClassFile.exists()) {
            propNameDefClassFile.delete();
        }

        Writer fileWrite = null;

        try {
            propNameDefClassFile.createNewFile();
            fileWrite = newFileWriter(propNameDefClassFile);
            writeFileHead(fileWrite);

            writePackageName(pkgName, fileWrite);
            fileWrite.write(IOUtil.LINE_SEPARATOR);
            fileWrite.write("import " + Arrays.class.getCanonicalName() + ";" + IOUtil.LINE_SEPARATOR);
            fileWrite.write("import " + Collections.class.getCanonicalName() + ";" + IOUtil.LINE_SEPARATOR);
            fileWrite.write("import " + List.class.getCanonicalName() + ";" + IOUtil.LINE_SEPARATOR);

            //            if (utilClass.equals(N.class)) {
            //                fileWrite.write("import " + Map.class.getCanonicalName() + ";" + N.LINE_SEPARATOR);
            //                fileWrite.write("import " + BiMap.class.getCanonicalName() + ";" + N.LINE_SEPARATOR);
            //            }

            //            fileWrite.write(N.LINE_SEPARATOR);
            //            String utilClassName = ClassUtil.getCanonicalClassName(utilClass);
            //            if (pkgName.equals(utilClassName.substring(0, utilClassName.lastIndexOf('.'))) || utilClass.equals(_N)) {
            //                // ignore
            //            } else {
            //                fileWrite.write("import " + ClassUtil.getCanonicalClassName(utilClass) + ";" + N.LINE_SEPARATOR);
            //                fileWrite.write(N.LINE_SEPARATOR);
            //            }

            writeClassComment(fileWrite);

            String clazzName = propNameDefClassFile.getName().substring(0, propNameDefClassFile.getName().indexOf(_PERIOD));
            fileWrite.write("public interface " + clazzName + " {");

            String headSpace = "";
            String domainName = entityDefinitionFactory.domainName();
            writeDomainPropNameClass(domainName, fileWrite, headSpace);

            EntityDefinition[] entityDefinitions = entityDefinitionFactory.getDefinitionList()
                    .toArray(new EntityDefinition[entityDefinitionFactory.getDefinitionList().size()]);
            writePropNameTable(entityDefinitions, propName2VarName, headSpace, fileWrite, utilClass);

            // -------- write global domain property name.
            Map<String, String> allPropNamesMap = new LinkedHashMap<>();
            Map<String, String> allPropColumnNameMap = new LinkedHashMap<>();

            for (EntityDefinition entityDef : entityDefinitions) {
                if (entityDef.isSliceEntity()) {
                    continue;
                }

                for (Property prop : entityDef.getPropertyList()) {
                    allPropNamesMap.put(prop.getName().toUpperCase(), prop.getName());

                    if (prop.getColumnType() == ColumnType.TABLE_COLUMN) {
                        allPropColumnNameMap.put(prop.getName().toUpperCase(), prop.getColumnName());
                    }
                }
            }

            List<String> keys = new ArrayList<>(allPropNamesMap.keySet());
            N.sort(keys);

            for (String key : keys) {
                String propName = allPropNamesMap.get(key);
                String propVarName = ClassUtil.invokeMethod(propName2VarName, propName);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write("    public static final String " + propVarName + " = \"" + propName + "\".intern();");
                fileWrite.write(IOUtil.LINE_SEPARATOR);
            }

            //            if (utilClass.equals(N.class)) {
            //                fileWrite.write(N.LINE_SEPARATOR);
            //                fileWrite.write("    /**" + N.LINE_SEPARATOR);
            //                fileWrite.write("     * Mapping of property name to column name " + N.LINE_SEPARATOR);
            //                fileWrite.write("     */" + N.LINE_SEPARATOR);
            //                fileWrite.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
            //                fileWrite.write("    public static final BiMap<String, String> " + PROP_COLUMN_MAP + " = " + N.getSimpleClassName(utilClass)
            //                        + ".newImmutableBiMapForInterface((Map) " + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(");
            //
            //                int i = 0;
            //                for (String key : keys) {
            //                    String propName = allPropNamesMap.get(key);
            //                    String propVarName = N.invokeMethod(propName2VarName, propName);
            //                    String columnName = allPropColumnNameMap.get(key);
            //
            //                    if (N.isNullOrEmpty(columnName)) {
            //                        continue;
            //                    }
            //
            //                    if (i++ > 0) {
            //                        fileWrite.write(", ");
            //                    }
            //
            //                    fileWrite.write(propVarName + ", \"" + columnName + "\".intern()");
            //                }
            //
            //                fileWrite.write("));" + N.LINE_SEPARATOR);
            //            }

            fileWrite.write("}" + IOUtil.LINE_SEPARATOR);

            fileWrite.flush();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(fileWrite);
        }
    }

    private static void writePropNameTable(final EntityDefinition[] entityDefs, final Method propName2VarName, final String headSpace, final Writer fileWrite,
            final Class<?> utilClass) throws IOException {
        for (EntityDefinition entityDef : entityDefs) {
            if (entityDef.isSliceEntity()) {
                continue;
            }

            fileWrite.write(IOUtil.LINE_SEPARATOR);
            fileWrite.write(headSpace + "    public static interface " + entityDef.getName() + POSTFIX_OF_PROP_NAME_LIST + " {");

            writePropNameField(entityDef, propName2VarName, headSpace + "    ", fileWrite, utilClass);

            fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

            // write slice entities
            for (EntityDefinition hed : entityDef.getSliceEntityList()) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public static interface " + hed.getName() + POSTFIX_OF_PROP_NAME_LIST + " {");

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        /**" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "         * Name of \"" + hed.getName() + "\" entity. " + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "         */" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        public static final String " + ENTITY_NAME_VAR + " = \"" + hed.getName() + "\";" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }
        }
    }

    /**
     *
     * @param entityDef
     * @param propName2VarName
     * @param headSpace
     * @param fileWrite
     * @throws IOException
     */
    private static void writePropNameField(final EntityDefinition entityDef, final Method propName2VarName, final String headSpace, final Writer fileWrite,
            final Class<?> utilClass) throws IOException {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        sb.append(IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     * Name of \"" + entityDef.getName() + "\" entity. " + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    public static final String " + ENTITY_NAME_VAR + " = \"" + entityDef.getName() + "\".intern();" + IOUtil.LINE_SEPARATOR);

        String _propNameList = "";
        String _columnNameList = "";
        //  String _propNameMap = "";
        //  String _propColumnNameMap = "";
        //  String _columnNameMap = "";

        for (Property prop : entityDef.getPropertyList()) {
            String propVarName = ClassUtil.invokeMethod(propName2VarName, prop.getName());

            sb.append(IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     * Name of \"" + prop.getName() + "\" property. " + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     * type: " + prop.getType().name() + ". " + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     * column: \"" + prop.getAttribute(PropertyEle.COLUMN) + "\". " + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
            // sb.append(headSpace + "    public static final String " + propVarName + " = \"" + prop.getName() + "\".intern();" + N.LINE_SEPARATOR);
            // sb.append(headSpace + "    public static final String _" + propVarName + " = (_ + \"." + prop.getName() + "\").intern();" + N.LINE_SEPARATOR);
            sb.append(headSpace + "    public static final String " + propVarName + " = (" + ENTITY_NAME_VAR + " + \"." + prop.getName() + "\").intern();"
                    + IOUtil.LINE_SEPARATOR);

            _propNameList += (", " + propVarName);
            //  _propNameMap += (", " + propVarName + ", \"" + prop.getName() + "\".intern()");

            if (prop.getColumnType() == ColumnType.TABLE_COLUMN) {
                String columnName = "\"" + entityDef.getTableName() + "." + prop.getColumnName() + "\".intern()";
                _columnNameList += (", " + columnName + "");
                //  _propColumnNameMap += (", " + propVarName + ", " + columnName);
                //  _columnNameMap += (", " + columnName + ", \"" + prop.getColumnName() + "\".intern()");
            }
        }

        sb.append(IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     * Immutable property name list" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    public static final List<String> " + PROP_NAME_LIST + " = Collections.unmodifiableList(Arrays.asList("
                + _propNameList.substring(2) + "));" + IOUtil.LINE_SEPARATOR);

        //        if (utilClass.equals(N.class)) {
        //            sb.append(N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    /**" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     * Immutable property name to entityName.propName mapper" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     */" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    public static final BiMap<String, String> " + PROP_NAME_MAP + " = " + N.getSimpleClassName(utilClass)
        //                    + ".newImmutableBiMapForInterface((Map)" + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(" + _propNameMap.substring(2) + "));"
        //                    + N.LINE_SEPARATOR);
        //
        //            sb.append(N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    /**" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     * Immutable property name to column name mapper" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     */" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    public static final BiMap<String, String> " + PROP_COLUMN_MAP + " = " + N.getSimpleClassName(utilClass)
        //                    + ".newImmutableBiMapForInterface((Map)" + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(" + _propColumnNameMap.substring(2) + "));"
        //                    + N.LINE_SEPARATOR);
        //        }

        sb.append(IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     * Immutable column name list" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    public static final List<String> " + COLUMN_NAME_LIST + " = Collections.unmodifiableList(Arrays.asList("
                + _columnNameList.substring(2) + "));" + IOUtil.LINE_SEPARATOR);

        //        if (utilClass.equals(N.class)) {
        //            sb.append(N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    /**" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     * Immutable column name to tableName.columnName mapper" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     */" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    public static final BiMap<String, String> " + COLUMN_NAME_MAP + " = " + N.getSimpleClassName(utilClass)
        //                    + ".newImmutableBiMapForInterface((Map)" + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(" + _columnNameMap.substring(2) + "));"
        //                    + N.LINE_SEPARATOR);
        //        }

        fileWrite.write(sb.toString());

        ObjectFactory.recycle(sb);
    }

    public static void entityDefinitionXml2ColumnNameTable(final String domainName, final File entityDefinitionXml, final String srcPath) {
        entityDefinitionXml2ColumnNameTable(domainName, entityDefinitionXml, srcPath, null, _N);
    }

    /**
     *
     * @param domainName
     * @param entityDefinitionXml
     * @param srcPath
     * @param columnName2VarName
     * @param utilClass  specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.java
     */
    public static void entityDefinitionXml2ColumnNameTable(final String domainName, final File entityDefinitionXml, final String srcPath,
            Method columnName2VarName, Class<?> utilClass) {
        if (columnName2VarName == null) {
            columnName2VarName = ClassUtil.getDeclaredMethod(CodeGenerator.class, PROP_NAME2VAR_NAME, String.class);
        }

        if (utilClass == null) {
            utilClass = _N;
        }

        final EntityDefinitionFactory entityDefinitionFactory = SQLEntityDefinitionFactory.newInstance(domainName, entityDefinitionXml);
        final Collection<EntityDefinition> entityDefs = entityDefinitionFactory.getDefinitionList();
        final String pkgName = entityDefinitionFactory.getAttribute(EntityDefEle.PACKAGE);
        final String classFilePath = makePackageFolder(srcPath, pkgName);

        //        if (utilClass.equals(_N)) {
        //            File utilClassFile = new File(classFilePath + ClassUtil.getSimpleClassName(_N) + POSTFIX_OF_JAVA_FILE);
        //            if (!utilClassFile.exists()) {
        //                String sourceCode = _N_STRING.replaceFirst("com.landawn.abacus.util", pkgName);
        //                IOUtil.write(utilClassFile, sourceCode);
        //            }
        //        }

        String columnNameTableClassName = POSTFIX_OF_COLUMN_NAME_LIST;

        if (N.notNullOrEmpty(domainName)) {
            columnNameTableClassName = N.capitalize(domainName) + columnNameTableClassName;
        }

        File columnNameTableClassFile = new File(classFilePath + columnNameTableClassName + POSTFIX_OF_JAVA_FILE);

        if (columnNameTableClassFile.exists()) {
            columnNameTableClassFile.delete();
        }

        Writer fileWrite = null;

        try {
            columnNameTableClassFile.createNewFile();
            fileWrite = newFileWriter(columnNameTableClassFile);
            writeFileHead(fileWrite);

            writePackageName(pkgName, fileWrite);
            fileWrite.write(IOUtil.LINE_SEPARATOR);
            fileWrite.write("import " + Arrays.class.getCanonicalName() + ";" + IOUtil.LINE_SEPARATOR);
            fileWrite.write("import " + Collections.class.getCanonicalName() + ";" + IOUtil.LINE_SEPARATOR);
            fileWrite.write("import " + List.class.getCanonicalName() + ";" + IOUtil.LINE_SEPARATOR);

            //            if (utilClass.equals(N.class)) {
            //                fileWrite.write("import " + Map.class.getCanonicalName() + ";" + N.LINE_SEPARATOR);
            //                fileWrite.write("import " + BiMap.class.getCanonicalName() + ";" + N.LINE_SEPARATOR);
            //            }

            //            fileWrite.write(N.LINE_SEPARATOR);
            //            String utilClassName = ClassUtil.getCanonicalClassName(utilClass);
            //            if (pkgName.equals(utilClassName.substring(0, utilClassName.lastIndexOf('.'))) || utilClass.equals(_N)) {
            //                // ignore
            //            } else {
            //                fileWrite.write("import " + ClassUtil.getCanonicalClassName(utilClass) + ";" + N.LINE_SEPARATOR);
            //                fileWrite.write(N.LINE_SEPARATOR);
            //            }

            writeClassComment(fileWrite);

            String clazzName = columnNameTableClassFile.getName().substring(0, columnNameTableClassFile.getName().indexOf(_PERIOD));
            fileWrite.write("public interface " + clazzName + " {");

            String headSpace = "";
            writeDomainPropNameClass(domainName, fileWrite, headSpace);

            writeColumnNameTable(entityDefs, columnName2VarName, headSpace, fileWrite, utilClass);

            // -------- write global domain property name.
            Map<String, String> allColumnNamesMap = new LinkedHashMap<>();
            Map<String, String> allColumnPropNameMap = new LinkedHashMap<>();

            for (EntityDefinition entityDef : entityDefs) {
                for (Property prop : entityDef.getPropertyList()) {
                    if (prop.getColumnType() == ColumnType.ENTITY || N.isNullOrEmpty(prop.getColumnName())) {
                        continue;
                    }

                    String columnName = prop.getColumnName();

                    allColumnNamesMap.put(columnName.toUpperCase(), columnName);
                    allColumnPropNameMap.put(columnName.toUpperCase(), prop.getName());
                }
            }

            List<String> keys = new ArrayList<>(allColumnNamesMap.keySet());
            N.sort(keys);

            for (String key : keys) {
                String columnName = allColumnNamesMap.get(key);
                String columnVarName = ClassUtil.invokeMethod(columnName2VarName, columnName);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write("    public static final String " + columnVarName + " = \"" + columnName + "\".intern();");
                fileWrite.write(IOUtil.LINE_SEPARATOR);
            }

            //            if (utilClass.equals(N.class)) {
            //                fileWrite.write(N.LINE_SEPARATOR);
            //                fileWrite.write("    /**" + N.LINE_SEPARATOR);
            //                fileWrite.write("     * Mapping of column name to property name " + N.LINE_SEPARATOR);
            //                fileWrite.write("     */" + N.LINE_SEPARATOR);
            //                fileWrite.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
            //                fileWrite.write("    public static final BiMap<String, String> " + COLUMN_PROP_MAP + " = " + N.getSimpleClassName(utilClass)
            //                        + ".newImmutableBiMapForInterface((Map) " + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(");
            //
            //                int i = 0;
            //                for (String key : keys) {
            //                    String columnName = allColumnNamesMap.get(key);
            //                    String columnVarName = N.invokeMethod(columnName2VarName, columnName);
            //                    String propName = allColumnPropNameMap.get(key);
            //
            //                    if (i++ > 0) {
            //                        fileWrite.write(", ");
            //                    }
            //
            //                    fileWrite.write(columnVarName + ", \"" + propName + "\".intern()");
            //                }
            //
            //                fileWrite.write("));" + N.LINE_SEPARATOR);
            //            }

            fileWrite.write("}" + IOUtil.LINE_SEPARATOR);

            fileWrite.flush();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(fileWrite);
        }
    }

    private static void writeColumnNameTable(final Collection<EntityDefinition> entityDefs, final Method columnName2VarName, final String headSpace,
            final Writer fileWrite, final Class<?> utilClass) throws IOException {
        for (EntityDefinition entityDef : entityDefs) {
            if (entityDef.isSliceEntity()) {
                continue;
            }

            fileWrite.write(IOUtil.LINE_SEPARATOR);
            fileWrite.write(headSpace + "    public static interface " + entityDef.getName() + POSTFIX_OF_COLUMN_NAME_LIST + " {");

            writeColumnNameField(entityDef, columnName2VarName, headSpace + "    ", fileWrite, utilClass);

            fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
        }
    }

    private static void writeColumnNameField(final EntityDefinition entityDef, final Method columnName2VarName, final String headSpace, final Writer fileWrite,
            final Class<?> utilClass) throws IOException {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        sb.append(IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     * Name of \"" + entityDef.getTableName() + "\" table. " + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
        sb.append(
                headSpace + "    public static final String " + ENTITY_NAME_VAR + " = \"" + entityDef.getTableName() + "\".intern();" + IOUtil.LINE_SEPARATOR);

        String _columnNameList = "";
        String _propNameList = "";
        //  String _columnNameMap = "";
        //  String _columnPropNameMap = "";
        //  String _propNameMap = "";

        for (Property prop : entityDef.getPropertyList()) {
            if (prop.getColumnType().isEntity()) {
                continue;
            }

            String columnName = prop.getColumnName();

            if (N.isNullOrEmpty(columnName)) {
                continue;
            }

            String columnVarName = ClassUtil.invokeMethod(columnName2VarName, columnName);

            sb.append(IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     * Name of \"" + columnName + "\" column. " + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     * Java type: " + prop.getType().name() + ". " + IOUtil.LINE_SEPARATOR);
            sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
            // sb.append(headSpace + "    public static final String " + propVarName + " = \"" + columnName + "\".intern();" + N.LINE_SEPARATOR);
            // sb.append(headSpace + "    public static final String _" + propVarName + " = (_ + \"." + columnName + "\").intern();" + N.LINE_SEPARATOR);
            sb.append(headSpace + "    public static final String " + columnVarName + " = (" + ENTITY_NAME_VAR + " + \"." + columnName + "\").intern();"
                    + IOUtil.LINE_SEPARATOR);

            _columnNameList += (", " + columnVarName);
            String propName = "\"" + entityDef.getName() + "." + prop.getName() + "\".intern()";
            _propNameList += (", " + propName);
            //  _columnNameMap += (", " + columnVarName + ", \"" + columnName + "\".intern()");
            //  _columnPropNameMap += (", " + columnVarName + ", " + propName);
            //  _propNameMap += (", " + propName + ", \"" + prop.getName() + "\".intern()");
        }

        sb.append(IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     * Immutable column name list" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    public static final List<String> " + COLUMN_NAME_LIST + " = Collections.unmodifiableList(Arrays.asList("
                + _columnNameList.substring(2) + "));" + IOUtil.LINE_SEPARATOR);

        //        if (utilClass.equals(N.class)) {
        //            sb.append(N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    /**" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     * Immutable column name to tableName.columnName mapper" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     */" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    public static final BiMap<String, String> " + COLUMN_NAME_MAP + " = " + N.getSimpleClassName(utilClass)
        //                    + ".newImmutableBiMapForInterface((Map)" + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(" + _columnNameMap.substring(2) + "));"
        //                    + N.LINE_SEPARATOR);
        //
        //            sb.append(N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    /**" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     * Immutable column name to property name mapper" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     */" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    public static final BiMap<String, String> " + COLUMN_PROP_MAP + " = " + N.getSimpleClassName(utilClass)
        //                    + ".newImmutableBiMapForInterface((Map)" + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(" + _columnPropNameMap.substring(2) + "));"
        //                    + N.LINE_SEPARATOR);
        //        }

        sb.append(IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    /**" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     * Immutable property name list" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "     */" + IOUtil.LINE_SEPARATOR);
        sb.append(headSpace + "    public static final List<String> " + PROP_NAME_LIST + " = Collections.unmodifiableList(Arrays.asList("
                + _propNameList.substring(2) + "));" + IOUtil.LINE_SEPARATOR);

        //        if (utilClass.equals(N.class)) {
        //            sb.append(N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    /**" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     * Immutable property name to entityName.propName mapper" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     */" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "     @SuppressWarnings(\"rawtypes\")" + N.LINE_SEPARATOR);
        //            sb.append(headSpace + "    public static final BiMap<String, String> " + PROP_NAME_MAP + " = " + N.getSimpleClassName(utilClass)
        //                    + ".newImmutableBiMapForInterface((Map)" + N.getSimpleClassName(utilClass) + ".asLinkedHashMap(" + _propNameMap.substring(2) + "));"
        //                    + N.LINE_SEPARATOR);
        //        }

        fileWrite.write(sb.toString());

        ObjectFactory.recycle(sb);
    }

    public static void generateSQLMapperIdTable(final String filePath, final String srcPath, final String pkgName, final String className,
            final Method id2VarName) {
        generateSQLMapperIdTable(SQLMapper.fromFile(filePath), srcPath, pkgName, className, id2VarName);
    }

    public static void generateSQLMapperIdTable(final SQLMapper sqlMapper, final String srcPath, final String pkgName, final String className,
            Method id2VarName) {
        String classFilePath = makePackageFolder(srcPath, pkgName);
        File sqlMapperTableClassFile = new File(classFilePath + className + POSTFIX_OF_JAVA_FILE);

        if (sqlMapperTableClassFile.exists()) {
            sqlMapperTableClassFile.delete();
        }

        if (id2VarName == null) {
            id2VarName = ClassUtil.getDeclaredMethod(CodeGenerator.class, PROP_NAME2VAR_NAME, String.class);
        }

        Writer fileWrite = null;

        try {
            sqlMapperTableClassFile.createNewFile();
            fileWrite = newFileWriter(sqlMapperTableClassFile);
            writeFileHead(fileWrite);

            writePackageName(pkgName, fileWrite);
            fileWrite.write(IOUtil.LINE_SEPARATOR);
            writeClassComment(fileWrite);
            fileWrite.write("public final class " + className + " {");

            fileWrite.write(IOUtil.LINE_SEPARATOR);

            for (String id : sqlMapper.keySet()) {
                String propVarName = ClassUtil.invokeMethod(id2VarName, id);
                fileWrite.write("    public static final String " + propVarName + " = \"" + id + "\";" + IOUtil.LINE_SEPARATOR);
            }

            fileWrite.write("}" + IOUtil.LINE_SEPARATOR);

            fileWrite.flush();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(fileWrite);
        }
    }

    public static void generateEntity(File srcDir, String packageName, String className, Map<String, ?> fields) {
        generateEntity(srcDir, packageName, className, fields, false, false, false, Objects.class);
    }

    /**
     * 
     * @param srcDir
     * @param packageName
     * @param className
     * @param fields key is field name, value is field type, which must be <code>Class</code>, <code>Type</code> or the name of them. 
     *               It must be <code>LinkedHashMap</code>.
     * @param constructor
     * @param copyMethod
     * @param fluentSetter
     * @param utilClassForHashEqualsToString is <code>Objects.class</code> by default. It can also be <code>N.class</code> or any classes else which provide the {@code hashCode/equals/toString} method.
     *      Or specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.jar for Methods {@code hashCode/equals/toString}.
     */
    public static void generateEntity(File srcDir, String packageName, String className, Map<String, ?> fields, final boolean constructor,
            final boolean copyMethod, final boolean fluentSetter, final Class<?> utilClassForHashEqualsToString) {
        generateEntity(srcDir, packageName, className, fields, constructor, copyMethod, fluentSetter, null, null, null, null, utilClassForHashEqualsToString);
    }

    /**
     * 
     * @param srcDir
     * @param packageName
     * @param className
     * @param fields key is field name, value is field type, which must be <code>Class</code>, <code>Type</code> or the name of them. 
     *               It must be <code>LinkedHashMap</code>.
     * @param constructor
     * @param copyMethod
     * @param fluentSetter
     * @param fieldName2MethodName
     * @param parentClass
     * @param parentPropertyModeForHashEquals
     * @param parentPropertyModeForToString
     * @param utilClassForHashEqualsToString is <code>Objects.class</code> by default. It can also be <code>N.class</code> or any classes else which provide the {@code hashCode/equals/toString} method.
     *      Or specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.jar for Methods {@code hashCode/equals/toString}.
     */
    public static void generateEntity(File srcDir, String packageName, String className, Map<String, ?> fields, final boolean constructor,
            final boolean copyMethod, final boolean fluentSetter, final Map<String, String> fieldName2MethodName, Class<?> parentClass,
            final ParentPropertyMode parentPropertyModeForHashEquals, final ParentPropertyMode parentPropertyModeForToString,
            final Class<?> utilClassForHashEqualsToString) {

        if (fields instanceof LinkedHashMap == false) {
            throw new IllegalArgumentException("'fields has to be LinkedHashMap to keep the order");
        }

        if (N.isNullOrEmpty(fields)) {
            return;
        }

        final Class<?> utilClass = utilClassForHashEqualsToString == null ? Objects.class : utilClassForHashEqualsToString;
        final Map<String, Type<?>> fieldTypes = new LinkedHashMap<>(N.initHashCapacity(fields.size()));
        Type<?> type = null;

        for (Map.Entry<String, ?> entry : fields.entrySet()) {
            if (entry.getValue() instanceof Type) {
                type = (Type<?>) entry.getValue();
            } else if (entry.getValue() instanceof Class) {
                type = N.typeOf((Class<?>) entry.getValue());
            } else if (entry.getValue() instanceof String) {
                String typeName = (String) entry.getValue();
                final TypeAttrParser attrs = TypeAttrParser.parse(typeName);

                if (N.notNullOrEmpty(attrs.getTypeParameters())) {
                    if (attrs.getTypeParameters().length == 1) {
                        Class<?> eleClass = null;
                        try {
                            eleClass = ClassUtil.forClass(attrs.getTypeParameters()[0]);
                        } catch (Throwable e) {
                            // ignore.
                        }

                        if (eleClass == null && N.notNullOrEmpty(packageName)) {
                            try {
                                eleClass = ClassUtil.forClass(packageName + "." + attrs.getTypeParameters()[0]);
                            } catch (Throwable e) {
                                // ignore.
                            }

                            if (eleClass != null) {
                                typeName = attrs.getClassName() + "<" + eleClass.getCanonicalName() + ">";
                            }
                        }
                    } else if (attrs.getTypeParameters().length == 2) {
                        String keyTypeName = attrs.getTypeParameters()[0];
                        String valueTypeName = attrs.getTypeParameters()[1];

                        Class<?> keyClass = null;
                        Class<?> valueClass = null;

                        try {
                            keyClass = ClassUtil.forClass(attrs.getTypeParameters()[0]);
                        } catch (Throwable e) {
                            // ignore.
                        }

                        try {
                            valueClass = ClassUtil.forClass(attrs.getTypeParameters()[1]);
                        } catch (Throwable e) {
                            // ignore.
                        }

                        if ((keyClass == null || valueClass == null) && N.notNullOrEmpty(packageName)) {
                            if (keyClass == null) {
                                try {
                                    keyClass = ClassUtil.forClass(packageName + "." + attrs.getTypeParameters()[0]);

                                    if (keyClass != null) {
                                        keyTypeName = keyClass.getCanonicalName();
                                    }
                                } catch (Throwable e) {
                                    // ignore.
                                }
                            }

                            if (valueClass == null) {
                                try {
                                    valueClass = ClassUtil.forClass(packageName + "." + attrs.getTypeParameters()[1]);

                                    if (valueClass != null) {
                                        valueTypeName = valueClass.getCanonicalName();
                                    }
                                } catch (Throwable e) {
                                    // ignore.
                                }
                            }

                            if (keyClass != null && valueClass != null) {
                                typeName = attrs.getClassName() + "<" + keyTypeName + ", " + valueTypeName + ">";
                            }
                        }
                    }
                }

                type = N.typeOf(typeName);
            } else {
                throw new IllegalArgumentException("Only Class, Type or the name of them are supported");
            }

            fieldTypes.put(entry.getKey(), type);
        }

        final File dirFile = new File(
                srcDir.getAbsolutePath() + (N.isNullOrEmpty(packageName) ? "" : IOUtil.FILE_SEPARATOR + N.replaceAll(packageName, ".", IOUtil.FILE_SEPARATOR)));

        if (dirFile.exists() == false) {
            dirFile.mkdirs();
        }

        if (_N.equals(utilClass)) {
            File utilClassFile = new File(dirFile.getAbsolutePath() + IOUtil.FILE_SEPARATOR + ClassUtil.getSimpleClassName(_N) + POSTFIX_OF_JAVA_FILE);
            if (!utilClassFile.exists()) {
                String sourceCode = _N_STRING.replaceFirst("package com.landawn.abacus.util;",
                        N.isNullOrEmpty(packageName) ? "" : "package " + packageName + ";");
                IOUtil.write(utilClassFile, sourceCode);
            }
        }

        final File classFile = new File(dirFile.getAbsolutePath() + IOUtil.FILE_SEPARATOR + className + ".java");

        IOUtil.createIfNotExists(classFile);

        final BufferedWriter writer = IOUtil.newBufferedWriter(classFile);

        try {
            IOUtil.writeLine(writer, "package " + packageName + ";");
            // IOUtil.writeLine(writer, N.EMPTY_STRING);

            // write imports
            final Multimap<String, String, Set<String>> packClasses = new Multimap<>(SortedMap.class, Set.class);
            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                readPackageTypes(entry.getValue(), packageName, packClasses);
            }

            if (parentClass != null) {
                Package pkg = parentClass.getPackage();

                if (pkg == null || pkg.getName().equals(packageName) || parentClass.getCanonicalName().startsWith("java.lang.")) {
                    // ignore
                } else {
                    packClasses.put(pkg.getName(), parentClass.getCanonicalName());
                }
            }

            if (!ClassUtil.getPackageName(utilClass).equals(packageName) && !utilClass.equals(_N)) {
                packClasses.put(ClassUtil.getPackageName(utilClass), utilClass.getCanonicalName());
            }

            //            if (N.notNullOrEmpty(fields) && ClassUtil.getPackageName(com.landawn.abacus.annotation.Type.class).equals(packageName) == false) {
            //                packClasses.put(ClassUtil.getPackageName(com.landawn.abacus.annotation.Type.class),
            //                        com.landawn.abacus.annotation.Type.class.getCanonicalName());
            //            }

            final Map<String, Class<?>> importedClasses = new LinkedHashMap<>();

            try {
                for (Set<String> clsNames : packClasses.values()) {
                    for (String clsName : clsNames) {
                        // IOUtil.writeLine(writer, "import " + clsName + ";");
                        Class<?> cls = ClassUtil.forClass(clsName);
                        writeClassImport(writer, cls, importedClasses);
                    }

                    // IOUtil.writeLine(writer, N.EMPTY_STRING);
                }
            } catch (IOException e) {
                throw new UncheckedIOException(e);
            }

            IOUtil.writeLine(writer, N.EMPTY_STRING);
            IOUtil.writeLine(writer, N.EMPTY_STRING);

            final String iden = "    ";

            IOUtil.writeLine(writer, "/**");
            IOUtil.writeLine(writer, " * Generated by Abacus.");
            IOUtil.writeLine(writer, " * @version ${version}");
            IOUtil.writeLine(writer, " */");

            if (parentClass == null) {
                IOUtil.writeLine(writer, "public class " + className + " {");
            } else {
                IOUtil.writeLine(writer, "public class " + className + " extends " + parentClass.getSimpleName() + " {");
            }

            if (parentClass != null && Serializable.class.isAssignableFrom(parentClass)) {
                long hashCode = className.hashCode();

                for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                    hashCode += hashCode * 31 + entry.getKey().hashCode();
                    hashCode += hashCode * 31 + entry.getValue().name().hashCode();
                }

                long serialVersionUID = 0;

                if (String.valueOf(Long.MAX_VALUE).length() > (String.valueOf(hashCode).length() + 1)) {
                    serialVersionUID = Long.valueOf(String.valueOf(hashCode) + fieldTypes.size());
                } else {
                    serialVersionUID = Long.valueOf(String.valueOf(hashCode + fieldTypes.size()));
                }

                IOUtil.writeLine(writer, N.EMPTY_STRING);
                IOUtil.writeLine(writer, iden + "private static final long serialVersionUID = " + serialVersionUID + "L;");
            }

            IOUtil.writeLine(writer, N.EMPTY_STRING);

            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                IOUtil.writeLine(writer, iden + "private " + getSimpleType(entry.getValue(), null, packageName, importedClasses) + " " + entry.getKey() + ";");
            }

            writeClassMethod(null, className, parentClass, packageName, fieldTypes, constructor, copyMethod, fluentSetter, parentPropertyModeForHashEquals,
                    parentPropertyModeForToString, fieldName2MethodName, importedClasses, utilClass, writer);

            IOUtil.writeLine(writer, "}");
        } catch (NoSuchFieldException | SecurityException e) {
            throw N.toRuntimeException(e);
        } finally {
            IOUtil.closeQuietly(writer);
        }
    }

    private static void readPackageTypes(Type<?> type, String packageName, final Multimap<String, String, Set<String>> packClasses) {
        final Package pkg = type.clazz().getPackage();

        if (pkg == null || pkg.getName().equals(packageName) || type.clazz().getCanonicalName().startsWith("java.lang.")) {
            // ignore.
        } else {
            packClasses.put(pkg.getName(), type.clazz().getCanonicalName());
        }

        if (N.notNullOrEmpty(type.getParameterTypes())) {
            for (Type<?> paramType : type.getParameterTypes()) {
                readPackageTypes(paramType, packageName, packClasses);
            }
        }
    }

    /**
     * Write the generated methods by the fields defined the in specified class to the source file.
     * 
     * <br />
     * Add below comments to specified the section where the generated methods should be written to
     * <pre>
     * =====>
     * 
     * <=====
     * </pre>
     * 
     * @param srcDir
     * @param cls
     */
    public static void writeClassMethod(final File srcDir, final Class<?> cls) {
        writeClassMethod(srcDir, cls, false, false, false, null, null, Objects.class);
    }

    /**
     * Write the generated methods by the fields defined the in specified class to the source file.
     * 
     * <br />
     * Add below comments to specified the section where the generated methods should be written to
     * <pre>
     * =====>
     * 
     * <=====
     * </pre>
     * 
     * @param srcDir
     * @param cls
     * @param constructor generate constructor
     * @param copyMethod generate the copy method.
     * @param fluentSetter
     * @param ignoreFieldNames
     * @param fieldName2MethodName
     * @param utilClassForHashEqualsToString is <code>Objects.class</code> by default. It can also be <code>N.class</code> or any classes else which provide the {@code hashCode/equals/toString} method.
     *      Or specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.jar for Methods {@code hashCode/equals/toString}.
     */
    public static void writeClassMethod(final File srcDir, final Class<?> cls, final boolean constructor, final boolean copyMethod, final boolean fluentSetter,
            Set<String> ignoreFieldNames, final Map<String, String> fieldName2MethodName, final Class<?> utilClassForHashEqualsToString) {
        writeClassMethod(srcDir, cls, constructor, copyMethod, fluentSetter, ignoreFieldNames, fieldName2MethodName, ParentPropertyMode.FIRST,
                ParentPropertyMode.FIRST, utilClassForHashEqualsToString);
    }

    /**
     * Write the generated methods by the fields defined the in specified class to the source file.
     * 
     * <br />
     * Add below comments to specified the section where the generated methods should be written to
     * <pre>
     * =====>
     * 
     * <=====
     * </pre>
     *  
     * @param srcDir
     * @param cls
     * @param constructor
     * @param copyMethod
     * @param fluentSetter
     * @param ignoreFieldNames
     * @param fieldName2MethodName
     * @param parentPropertyModeForHashEquals
     * @param parentPropertyModeForToString
     * @param utilClassForHashEqualsToString is <code>Objects.class</code> by default. It can also be <code>N.class</code> or any classes else which provide the {@code hashCode/equals/toString} method.
     *      Or specify <code>CodeGenerator._N</code> or your own utility class to generate entity classes which not dependent on AbacusUtil.jar for Methods {@code hashCode/equals/toString}.
     */
    public static void writeClassMethod(final File srcDir, final Class<?> cls, final boolean constructor, final boolean copyMethod, final boolean fluentSetter,
            Set<String> ignoreFieldNames, final Map<String, String> fieldName2MethodName, final ParentPropertyMode parentPropertyModeForHashEquals,
            final ParentPropertyMode parentPropertyModeForToString, final Class<?> utilClassForHashEqualsToString) {

        final Package pkg = cls.getPackage();
        final String clsSourcePath = srcDir.getAbsolutePath()
                + (pkg == null ? "" : IOUtil.FILE_SEPARATOR + N.replaceAll(pkg.getName(), ".", IOUtil.FILE_SEPARATOR)) + IOUtil.FILE_SEPARATOR
                + cls.getSimpleName() + ".java";
        final File clsSourceFile = new File(clsSourcePath);

        if (clsSourceFile.exists() == false) {
            throw new RuntimeException("No source file found by path: " + clsSourcePath + " for class: " + cls.getCanonicalName());
        }

        if (ignoreFieldNames == null) {
            ignoreFieldNames = new HashSet<>();
        }

        final String simpleClassName = cls.getSimpleName();
        final List<String> lines = ImmutableList.of(IOUtil.readLines(clsSourceFile));
        final Map<String, Type<?>> fieldTypes = new LinkedHashMap<>();

        for (Field field : cls.getDeclaredFields()) {
            final String fieldName = field.getName();
            if (Modifier.isStatic(field.getModifiers()) || ignoreFieldNames.contains(fieldName)) {
                continue;
            } else {
                fieldTypes.put(fieldName, N.typeOf(field.getType()));
            }
        }

        if (N.isNullOrEmpty(fieldTypes)) {
            return;
        }

        final BiMap<String, Class<?>> importedClasses = new BiMap<>();
        boolean hasGenericTypeField = false;
        for (int i = 0, size = lines.size(); i < size; i++) {
            final String tmp = "class " + simpleClassName;

            while (i < size && lines.get(i).indexOf(tmp) < 0) {
                String line = lines.get(i);
                if (line.startsWith("import ") && line.endsWith(";") && line.indexOf(" static ") < 0) {
                    String clsName = N.substringBetween(line, ' ', line.lastIndexOf(';')).get();
                    importedClasses.put(clsName, ClassUtil.forClass(clsName));
                }
                i++;
            }

            int start = i + 1;

            while (i < size && (lines.get(i).indexOf(") {") < 0 /*&& lines.get(i).indexOf("}") < 0*/)) {
                i++;
            }

            int end = i;
            if (start < size && end <= size && start < end) {
                for (int j = start; j < end; j++) {
                    String line = lines.get(j).trim().replaceAll("  ", " ");
                    int lastIndex = line.lastIndexOf(' ');
                    String fieldName = null;

                    if (lastIndex > 0 && line.endsWith(";") && fieldTypes.containsKey((fieldName = line.substring(lastIndex + 1, line.length() - 1)))) {
                        int firstIdx = line.indexOf('<');

                        if (firstIdx > 0) {
                            hasGenericTypeField = true;

                            int fromIndex = line.lastIndexOf(' ', firstIdx);

                            if (fromIndex >= 0) {
                                String typeName = line.substring(fromIndex + 1, lastIndex);

                                if ((typeName = typeName.trim()).length() > 0) {

                                    // for java.util.Date
                                    if (importedClasses.containsKey("java.util.Date")) {
                                        typeName = N.replaceAll(typeName, "<Date>", "<java.util.Date>");
                                        typeName = N.replaceAll(typeName, "<Date,", "<java.util.Date,");
                                        typeName = N.replaceAll(typeName, " Date>", " java.util.Date>");
                                        typeName = N.replaceAll(typeName, ",Date>", ",java.util.Date>");
                                        typeName = N.replaceAll(typeName, " Date,", " java.util.Date,");
                                        typeName = N.replaceAll(typeName, ",Date,", ",java.util.Date,");

                                        typeName = N.replaceAll(typeName, "<Date[", "<java.util.Date[");
                                        typeName = N.replaceAll(typeName, " Date[", " java.util.Date[");
                                        typeName = N.replaceAll(typeName, ",Date[", ",java.util.Date[");
                                    }

                                    fieldTypes.put(fieldName, N.typeOf(typeName));
                                }
                            }
                        }
                    }
                }
            }
        }

        final String packageName = ClassUtil.getPackageName(cls);

        final File dirFile = new File(
                srcDir.getAbsolutePath() + (N.isNullOrEmpty(packageName) ? "" : IOUtil.FILE_SEPARATOR + N.replaceAll(packageName, ".", IOUtil.FILE_SEPARATOR)));

        if (dirFile.exists() == false) {
            dirFile.mkdirs();
        }

        final Class<?> utilClass = utilClassForHashEqualsToString == null ? Objects.class : utilClassForHashEqualsToString;

        if (_N.equals(utilClass)) {
            File utilClassFile = new File(dirFile.getAbsolutePath() + IOUtil.FILE_SEPARATOR + ClassUtil.getSimpleClassName(_N) + POSTFIX_OF_JAVA_FILE);
            if (!utilClassFile.exists()) {
                String sourceCode = _N_STRING.replaceFirst("package com.landawn.abacus.util;",
                        N.isNullOrEmpty(packageName) ? "" : "package " + packageName + ";");
                IOUtil.write(utilClassFile, sourceCode);
            }
        }

        try (StringWriter writer = new StringWriter()) {
            writeClassMethod(cls, ClassUtil.getSimpleClassName(cls), cls.getSuperclass(), packageName, fieldTypes, constructor, copyMethod, fluentSetter,
                    parentPropertyModeForHashEquals, parentPropertyModeForToString, fieldName2MethodName, importedClasses, utilClass, writer);

            int start = -1, end = -1;

            for (int i = 0, size = lines.size(); i < size; i++) {
                if (lines.get(i).indexOf("=====>") > 0) {
                    start = i;
                } else if (lines.get(i).indexOf("<=====") > 0) {
                    end = i;
                }
            }

            final List<String> newLines = new ArrayList<>();

            if (start >= 0 && end >= 0) {
                newLines.addAll(lines.subList(0, start + 1));
                newLines.add(writer.toString());
                newLines.addAll(lines.subList(end, lines.size()));
            } else {
                for (int i = 0, size = lines.size(); i < size; i++) {
                    if (lines.get(i).indexOf(") {") > 0) {
                        String tmp = lines.get(i).trim();
                        if (tmp.startsWith("public ") && (tmp.indexOf(cls.getSimpleName()) > 0 || tmp.indexOf(" get") > 0 || tmp.indexOf(" set") > 0)) {
                            start = i;
                            while (start-- > 0 && lines.get(start).trim().startsWith("@")) {

                            }

                            while (lines.get(start).trim().length() == 0 && start-- > 0) {

                            }

                            break;
                        }
                    }
                }

                if (start < 0) {
                    for (int i = lines.size() - 1; i >= 0; i--) {
                        if (lines.get(i).trim().startsWith("}")) {
                            start = i - 1;

                            break;
                        }
                    }
                }

                newLines.addAll(lines.subList(0, start + 1));
                newLines.add(writer.toString());
                newLines.add("}");
            }

            if (!ClassUtil.getPackageName(utilClass).equals(packageName) && !_N.equals(utilClass) && fieldTypes.size() > 0) {
                String importUtilClass = "import " + utilClass.getCanonicalName() + ";";

                for (int i = 0, size = newLines.size(); i < size; i++) {
                    if (newLines.get(i).indexOf(importUtilClass) >= 0) {
                        break;
                    } else if (newLines.get(i).indexOf("public ") >= 0) {

                        int j = i;
                        while (j-- >= 0) {
                            if (newLines.get(j).startsWith("import") || newLines.get(j).startsWith("package")) {
                                break;
                            }
                        }

                        newLines.add(j + 1, N.EMPTY_STRING);
                        newLines.add(j + 2, importUtilClass);

                        if (newLines.get(j + 3).trim().length() > 0) {
                            newLines.add(j + 3, N.EMPTY_STRING);
                        }

                        break;
                    }
                }
            }

            if (hasGenericTypeField) {
                //                String importTypeClass = "import com.landawn.abacus.annotation.Type;";
                //
                //                for (int i = 0, size = newLines.size(); i < size; i++) {
                //                    if (newLines.get(i).indexOf(importTypeClass) >= 0) {
                //                        break;
                //                    } else if (newLines.get(i).indexOf("public ") >= 0) {
                //                        int ins = 0;
                //                        if (newLines.get(i - 1).trim().length() > 0) {
                //                            newLines.add(i + ins++, N.EMPTY_STRING);
                //                        }
                //                        newLines.add(i + ins++, importTypeClass);
                //                        newLines.add(i + ins++, N.EMPTY_STRING);
                //                        break;
                //                    }
                //                }
            }

            if (newLines.get(newLines.size() - 1).startsWith("}") && newLines.get(newLines.size() - 2).endsWith(IOUtil.LINE_SEPARATOR)) {
                newLines.set(newLines.size() - 2, N.chop(newLines.get(newLines.size() - 2)));
            }

            IOUtil.writeLines(clsSourceFile, newLines);

            // Add annotation back.
            Map<String, Set<String>> annoMap = new HashMap<>();
            for (int i = 0, len = lines.size(); i < len; i++) {
                String line = lines.get(i);
                if (line.trim().startsWith("@")) {
                    Set<String> tmp = new LinkedHashSet<>();
                    tmp.add(line);
                    while (++i < len && lines.get(i).trim().startsWith("@")) {
                        tmp.add(lines.get(i));
                    }

                    while (i < len && lines.get(i).trim().equals("")) {
                        i++;
                    }

                    annoMap.put(lines.get(i), tmp);
                }
            }

            if (N.notNullOrEmpty(annoMap)) {
                final List<String> finalLines = new ArrayList<>();
                final List<String> srcLines = IOUtil.readLines(clsSourceFile);

                for (int i = 0, len = srcLines.size(); i < len; i++) {
                    final String line = srcLines.get(i);
                    final Set<String> annons = annoMap.get(line);

                    if (N.notNullOrEmpty(annons)) {
                        int j = finalLines.size() - 1;

                        while (j >= 0 && finalLines.get(j).trim().startsWith("@")) {
                            annons.add(finalLines.remove(j--));
                        }

                        finalLines.addAll(annoMap.get(line));
                    }

                    finalLines.add(line);
                }

                IOUtil.writeLines(clsSourceFile, finalLines);
            }

        } catch (IOException | NoSuchFieldException | SecurityException e) {
            throw N.toRuntimeException(e);
        }
    }

    //    public static void generateEntity(File srcDir, String packageName, Map<String, LinkedHashMap<String, ?>> classNameFields) {
    //        generateEntity(srcDir, packageName, classNameFields, false, false, false);
    //    }
    //
    //    /**
    //     * 
    //     * @param srcDir
    //     * @param packageName
    //     * @param classNameFields
    //     * @param constructor
    //     * @param copyMethod
    //     * @param fluentSetter
    //     */
    //    public static void generateEntity(File srcDir, String packageName, Map<String, LinkedHashMap<String, ?>> classNameFields, final boolean constructor,
    //            final boolean copyMethod, final boolean fluentSetter) {
    //        generateEntity(srcDir, packageName, classNameFields, constructor, copyMethod, fluentSetter, null, ParentPropertyMode.NONE, ParentPropertyMode.NONE);
    //    }
    //
    //    /**
    //     * 
    //     * @param srcDir
    //     * @param packageName
    //     * @param classNameFields
    //     * @param constructor
    //     * @param copyMethod
    //     * @param fluentSetter
    //     * @param parentClass
    //     * @param parentPropertyModeForHashEquals
    //     * @param parentPropertyModeForToString
    //     */
    //    public static void generateEntity(File srcDir, String packageName, Map<String, LinkedHashMap<String, ?>> classNameFields, final boolean constructor,
    //            final boolean copyMethod, final boolean fluentSetter, Class<?> parentClass, final ParentPropertyMode parentPropertyModeForHashEquals,
    //            final ParentPropertyMode parentPropertyModeForToString) {
    //
    //        for (String className : classNameFields.keySet()) {
    //            generateEntity(srcDir, packageName, className, classNameFields.get(className), constructor, copyMethod, fluentSetter, parentClass,
    //                    parentPropertyModeForHashEquals, parentPropertyModeForToString);
    //        }
    //    }

    //    /**
    //     * Generate and Print out the methods according to fields defined the in specified class.
    //     * 
    //     * @param cls
    //     */
    //    public static void printClassMethod(final Class<?> cls) {
    //        printClassMethod(cls, false, false, false, null, null);
    //    }
    //
    //    /**
    //     * Generate and Print out the methods according to fields defined the in specified class.
    //     * 
    //     * @param cls
    //     * @param constructor generate constructor
    //     * @param copyMethod generate the copy method.
    //     * @param fluentSetter
    //     * @param ignoreFieldNames
    //     * @param fieldName2MethodName
    //     */
    //    public static void printClassMethod(final Class<?> cls, final boolean constructor, final boolean copyMethod, final boolean fluentSetter,
    //            Set<String> ignoreFieldNames, final Map<String, String> fieldName2MethodName) {
    //        printClassMethod(cls, constructor, copyMethod, fluentSetter, ignoreFieldNames, fieldName2MethodName, ParentPropertyMode.FIRST,
    //                ParentPropertyMode.FIRST);
    //    }
    //
    //    /**
    //     * Generate and Print out the methods according to fields defined the in specified class.
    //     * 
    //     * @param cls
    //     * @param constructor
    //     * @param copyMethod
    //     * @param fluentSetter
    //     * @param ignoreFieldNames
    //     * @param fieldName2MethodName
    //     * @param parentPropertyModeForHashEquals
    //     * @param parentPropertyModeForToString
    //     */
    //    public static void printClassMethod(final Class<?> cls, final boolean constructor, final boolean copyMethod, final boolean fluentSetter,
    //            Set<String> ignoreFieldNames, final Map<String, String> fieldName2MethodName, final ParentPropertyMode parentPropertyModeForHashEquals,
    //            final ParentPropertyMode parentPropertyModeForToString) {
    //        if (ignoreFieldNames == null) {
    //            ignoreFieldNames = new HashSet<>();
    //        }
    //
    //        final Map<String, Type<?>> fieldTypes = new LinkedHashMap<>();
    //
    //        for (Field field : cls.getDeclaredFields()) {
    //            final String fieldName = field.getName();
    //            if (Modifier.isStatic(field.getModifiers()) || ignoreFieldNames.contains(fieldName)) {
    //                continue;
    //            } else {
    //                fieldTypes.put(fieldName, N.typeOf(field.getType()));
    //            }
    //        }
    //
    //        try (Writer writer = new OutputStreamWriter(System.out)) {
    //            printClassMethod(cls, ClassUtil.getSimpleClassName(cls), cls.getSuperclass(), cls.getPackage() == null ? null : ClassUtil.getPackageName(cls),
    //                    fieldTypes, constructor, copyMethod, fluentSetter, parentPropertyModeForHashEquals, parentPropertyModeForToString, fieldName2MethodName,
    //                    new LinkedHashMap<String, Class<?>>(), writer);
    //        } catch (IOException | NoSuchFieldException | SecurityException e) {
    //            throw N.toRuntimeException(e);
    //        }
    //    }

    private static void writeClassMethod(Class<?> cls, final String className, final Class<?> parentClass, final String pkgName,
            final Map<String, Type<?>> fieldTypes, final boolean constructor, final boolean copyMethod, final boolean fluentSetter,
            ParentPropertyMode parentPropertyModeForHashEquals, ParentPropertyMode parentPropertyModeForToString, Map<String, String> fieldName2MethodName,
            final Map<String, Class<?>> importedClasses, final Class<?> utilClass, Writer writer) throws NoSuchFieldException, SecurityException {

        if (N.isNullOrEmpty(fieldTypes)) {
            return;
        }

        if (parentPropertyModeForHashEquals == null) {
            parentPropertyModeForHashEquals = ParentPropertyMode.NONE;
        }

        if (parentPropertyModeForToString == null) {
            parentPropertyModeForToString = ParentPropertyMode.NONE;
        }

        if (fieldName2MethodName == null) {
            fieldName2MethodName = new HashMap<>();
        }

        final String utilClassName = utilClass.getSimpleName();
        final List<Method> parentGetterMethods = new ArrayList<>();
        final Map<String, Method> parentSettterMethods = new LinkedHashMap<>();
        final List<Class<?>> allClasses = new ArrayList<>();

        if (parentClass != null) {
            allClasses.add(parentClass);

            while (allClasses.get(allClasses.size() - 1).getSuperclass() != null) {
                allClasses.add(allClasses.get(allClasses.size() - 1).getSuperclass());
            }

            for (Class<?> superClass : allClasses) {
                parentGetterMethods.addAll(ClassUtil.getPropGetMethodList(superClass).values());
                parentSettterMethods.putAll(ClassUtil.getPropSetMethodList(superClass));
            }
        }

        final String iden = "    ";

        if (constructor) {
            IOUtil.writeLine(writer, N.EMPTY_STRING);
            IOUtil.writeLine(writer, iden + "public " + className + "() {");

            if (parentClass != null && AbstractDirtyMarker.class.isAssignableFrom(parentClass)) {
                IOUtil.writeLine(writer, iden + iden + "super(" + className + ".class.getSimpleName());");
            }

            IOUtil.writeLine(writer, iden + "}");

            IOUtil.writeLine(writer, N.EMPTY_STRING);
            String parameterStr = "";
            String signValues = "";

            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                if (parameterStr.length() > 0) {
                    parameterStr += ", ";
                }

                parameterStr += (getSimpleType(entry.getValue(), null, pkgName, importedClasses) + " " + entry.getKey());

                if (signValues.length() > 0) {
                    signValues += IOUtil.LINE_SEPARATOR;
                }

                signValues += (iden + iden + "this." + entry.getKey() + " = " + entry.getKey() + ";");
            }

            if (parameterStr.length() > 0) {
                IOUtil.writeLine(writer, iden + "public " + className + "(" + parameterStr + ") {");

                if (parentClass != null && AbstractDirtyMarker.class.isAssignableFrom(parentClass)) {
                    IOUtil.writeLine(writer, iden + iden + "super(" + className + ".class.getSimpleName());");
                    IOUtil.writeLine(writer, N.EMPTY_STRING);
                }

                IOUtil.writeLine(writer, signValues);
                IOUtil.writeLine(writer, iden + "}");
            }
        }

        if (fluentSetter && N.notNullOrEmpty(parentSettterMethods)) {
            for (Map.Entry<String, Method> entry : parentSettterMethods.entrySet()) {
                if (parentClass.isAssignableFrom(entry.getValue().getReturnType()) == false) {
                    continue;
                }

                final String methodName = entry.getValue().getName();
                String paraTypeName = ClassUtil.getParameterizedTypeNameByMethod(entry.getValue());

                if (N.notNullOrEmpty(pkgName)) {
                    String tmp = pkgName + ".";
                    int idx = 0;
                    char ch = 0;

                    while ((idx = paraTypeName.indexOf(tmp, idx)) >= 0) {
                        for (int i = idx + tmp.length(), len = paraTypeName.length(); i < len; i++) {
                            ch = paraTypeName.charAt(i);

                            if ((Character.isLetterOrDigit(ch) || ch == '$' || ch == '_') && i != len - 1) {
                                continue;
                            } else if (ch == '.') {
                                idx = i;
                                break;
                            } else {
                                paraTypeName = paraTypeName.replace(paraTypeName.substring(idx, i), paraTypeName.substring(idx + tmp.length(), i));
                                idx += (i - idx - tmp.length());
                                break;
                            }
                        }
                    }
                }

                IOUtil.writeLine(writer, N.EMPTY_STRING);
                IOUtil.writeLine(writer, iden + "public " + className + " " + methodName + "(" + paraTypeName + " " + entry.getKey() + ") {");
                IOUtil.writeLine(writer, iden + iden + "super." + methodName + "(" + entry.getKey() + ");");
                IOUtil.writeLine(writer, N.EMPTY_STRING);
                IOUtil.writeLine(writer, iden + iden + "return this;");
                IOUtil.writeLine(writer, iden + "}");
            }
        }

        for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
            final String fieldName = entry.getKey();
            final String simpleTypeName = getSimpleType(entry.getValue(), null, pkgName, importedClasses);
            //            final String getPrefix = boolean.class.equals(entry.getValue().getTypeClass()) || Boolean.class.equals(entry.getValue().getTypeClass()) ? "is"
            //                    : "get"; 
            final String postfix = fieldName2MethodName.containsKey(fieldName) ? fieldName2MethodName.get(fieldName)
                    : (N.isAllUpperCase(fieldName) ? fieldName : N.capitalize(fieldName));

            IOUtil.writeLine(writer, N.EMPTY_STRING);

            // final String annoTypeName = getAnnoType(entry.getValue(), pkgName, importedClasses);

            // IOUtil.writeLine(writer, iden + "@Type(\"" + entry.getValue().getName() + "\")");
            //            if (!entry.getValue().getName().equals(annoTypeName) || N.notNullOrEmpty(entry.getValue().getParameterTypes())) {
            //                IOUtil.writeLine(writer, iden + "@Type(\"" + annoTypeName + "\")");
            //            }

            IOUtil.writeLine(writer, iden + "public " + simpleTypeName + " get" + postfix + "() {");
            IOUtil.writeLine(writer, iden + iden + "return " + fieldName + ";");
            IOUtil.writeLine(writer, iden + "}");

            if (cls == null || Modifier.isFinal(cls.getDeclaredField(fieldName).getModifiers()) == false) {
                IOUtil.writeLine(writer, N.EMPTY_STRING);
                if (fluentSetter) {
                    IOUtil.writeLine(writer, iden + "public " + className + " set" + postfix + "(" + simpleTypeName + " " + fieldName + ") {");
                } else {
                    IOUtil.writeLine(writer, iden + "public void set" + postfix + "(" + simpleTypeName + " " + fieldName + ") {");
                }

                if (parentClass != null && AbstractDirtyMarker.class.isAssignableFrom(parentClass)) {
                    IOUtil.writeLine(writer, iden + iden + "super.setUpdatedPropName(\"" + fieldName + "\");");
                }

                IOUtil.writeLine(writer, iden + iden + "this." + fieldName + " = " + fieldName + ";");

                if (fluentSetter) {
                    IOUtil.writeLine(writer, N.EMPTY_STRING);
                    IOUtil.writeLine(writer, iden + iden + "return this;");
                }

                IOUtil.writeLine(writer, iden + "}");
            }
        }

        if (copyMethod) {
            IOUtil.writeLine(writer, N.EMPTY_STRING);
            IOUtil.writeLine(writer, iden + "public " + className + " copy() {");
            IOUtil.writeLine(writer, iden + iden + "final " + className + " copy = new " + className + "();");
            IOUtil.writeLine(writer, N.EMPTY_STRING);

            for (Method method : parentGetterMethods) {
                IOUtil.writeLine(writer,
                        iden + iden + "copy." + ClassUtil.getPropSetMethod(method.getDeclaringClass(), ClassUtil.getPropNameByMethod(method)).getName()
                                + "(this." + method.getName() + "());");
            }

            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                IOUtil.writeLine(writer, iden + iden + "copy." + entry.getKey() + " = this." + entry.getKey() + ";");
            }

            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + "return copy;");
            IOUtil.writeLine(writer, iden + "}");
        }

        {
            IOUtil.writeLine(writer, N.EMPTY_STRING);
            IOUtil.writeLine(writer, iden + "@Override");
            IOUtil.writeLine(writer, iden + "public int hashCode() {");
            IOUtil.writeLine(writer, iden + iden + "int h = 17;");

            if (parentPropertyModeForHashEquals == ParentPropertyMode.FIRST && parentGetterMethods.size() > 0) {
                for (Method method : parentGetterMethods) {
                    IOUtil.writeLine(writer, iden + iden + "h = 31 * h + " + utilClassName + ".hashCode(" + method.getName() + "());");
                }
            }

            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                IOUtil.writeLine(writer, iden + iden + "h = 31 * h + " + utilClassName + ".hashCode(" + entry.getKey() + ");");
            }

            if (parentPropertyModeForHashEquals == ParentPropertyMode.LAST && parentGetterMethods.size() > 0) {
                for (Method method : parentGetterMethods) {
                    IOUtil.writeLine(writer, iden + iden + "h = 31 * h + " + utilClassName + "hashCode(" + method.getName() + "());");
                }
            }

            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + "return h;");
            IOUtil.writeLine(writer, iden + "}");
        }

        {
            IOUtil.writeLine(writer, N.EMPTY_STRING);
            IOUtil.writeLine(writer, iden + "@Override");
            IOUtil.writeLine(writer, iden + "public boolean equals(Object obj) {");
            IOUtil.writeLine(writer, iden + iden + "if (this == obj) {");
            IOUtil.writeLine(writer, iden + iden + iden + "return true;");
            IOUtil.writeLine(writer, iden + iden + "}");

            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + "if (obj instanceof " + className + ") {");
            IOUtil.writeLine(writer, iden + iden + iden + "final " + className + " other = (" + className + ") obj;");

            int i = 0;

            if (parentPropertyModeForHashEquals == ParentPropertyMode.FIRST && parentGetterMethods.size() > 0) {
                for (Method method : parentGetterMethods) {
                    if (i++ == 0) {
                        if (i == parentGetterMethods.size() + fieldTypes.size()) {
                            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + iden + "return " + utilClassName + ".equals(" + method.getName()
                                    + "(), other." + method.getName() + "());");
                        } else {
                            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + iden + "return " + utilClassName + ".equals(" + method.getName()
                                    + "(), other." + method.getName() + "())");
                        }
                    } else {
                        if (i == parentGetterMethods.size() + fieldTypes.size()) {
                            IOUtil.writeLine(writer, iden + iden + iden + iden + "&& " + utilClassName + ".equals(" + method.getName() + "(), other."
                                    + method.getName() + "());");
                        } else {
                            IOUtil.writeLine(writer, iden + iden + iden + iden + "&& " + utilClassName + ".equals(" + method.getName() + "(), other."
                                    + method.getName() + "())");
                        }
                    }
                }
            }

            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                final String fieldName = entry.getKey();

                if (i++ == 0) {
                    if (i == fieldTypes.size() && (parentGetterMethods.size() == 0 || parentPropertyModeForHashEquals != ParentPropertyMode.LAST)) {
                        IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + iden + "return " + utilClassName + ".equals(" + fieldName + ", other."
                                + fieldName + ");");
                    } else {
                        IOUtil.writeLine(writer,
                                IOUtil.LINE_SEPARATOR + iden + iden + iden + "return " + utilClassName + ".equals(" + fieldName + ", other." + fieldName + ")");
                    }
                } else {
                    if (((parentPropertyModeForHashEquals != ParentPropertyMode.FIRST && i == fieldTypes.size())
                            || (parentPropertyModeForHashEquals == ParentPropertyMode.FIRST && i == fieldTypes.size() + parentGetterMethods.size()))
                            && (parentGetterMethods.size() == 0 || parentPropertyModeForHashEquals != ParentPropertyMode.LAST)) {
                        IOUtil.writeLine(writer, iden + iden + iden + iden + "&& " + utilClassName + ".equals(" + fieldName + ", other." + fieldName + ");");
                    } else {
                        IOUtil.writeLine(writer, iden + iden + iden + iden + "&& " + utilClassName + ".equals(" + fieldName + ", other." + fieldName + ")");
                    }
                }
            }

            if (parentPropertyModeForHashEquals == ParentPropertyMode.LAST && parentGetterMethods.size() > 0) {
                for (Method method : parentGetterMethods) {
                    if (i++ == 0) {
                        if (i == parentGetterMethods.size() + fieldTypes.size()) {
                            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + iden + "return " + utilClassName + ".equals(" + method.getName()
                                    + "(), other." + method.getName() + "());");
                        } else {
                            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + iden + "return " + utilClassName + ".equals(" + method.getName()
                                    + "(), other." + method.getName() + "())");
                        }
                    } else {
                        if (i == parentGetterMethods.size() + fieldTypes.size()) {
                            IOUtil.writeLine(writer, iden + iden + iden + iden + "&& " + utilClassName + ".equals(" + method.getName() + "(), other."
                                    + method.getName() + "());");
                        } else {
                            IOUtil.writeLine(writer, iden + iden + iden + iden + "&& " + utilClassName + ".equals(" + method.getName() + "(), other."
                                    + method.getName() + "())");
                        }
                    }
                }
            }

            IOUtil.writeLine(writer, iden + iden + "}");
            IOUtil.writeLine(writer, IOUtil.LINE_SEPARATOR + iden + iden + "return false;");
            IOUtil.writeLine(writer, iden + "}");
        }

        {
            final StringBuilder sb = new StringBuilder();
            IOUtil.writeLine(writer, N.EMPTY_STRING);
            sb.append(iden + "@Override" + IOUtil.LINE_SEPARATOR);
            sb.append(iden + "public String toString() {" + IOUtil.LINE_SEPARATOR);

            int i = 0;

            if (parentPropertyModeForToString == ParentPropertyMode.FIRST && parentGetterMethods.size() > 0) {
                for (Method method : parentGetterMethods) {
                    if (i++ == 0) {
                        sb.append(iden + iden + "return \"{" + ClassUtil.getPropNameByMethod(method) + "=\" + " + utilClassName + ".toString("
                                + method.getName() + "())");
                    } else {
                        sb.append(IOUtil.LINE_SEPARATOR + iden + iden + "         + \", " + ClassUtil.getPropNameByMethod(method) + "=\" + " + utilClassName
                                + ".toString(" + method.getName() + "())");
                    }

                    if (i == parentGetterMethods.size() + fieldTypes.size()) {
                        if (i > 1) {
                            sb.append(IOUtil.LINE_SEPARATOR + iden + iden + "         + \"}\";" + IOUtil.LINE_SEPARATOR);
                        } else {
                            sb.append(" + \"}\";" + IOUtil.LINE_SEPARATOR);
                        }
                    }
                }
            }

            for (Map.Entry<String, Type<?>> entry : fieldTypes.entrySet()) {
                final String fieldName = entry.getKey();

                if (i++ == 0) {
                    sb.append(iden + iden + "return \"{" + fieldName + "=\" + " + utilClassName + ".toString(" + fieldName + ")");
                } else {
                    sb.append(IOUtil.LINE_SEPARATOR + iden + iden + "         + \", " + fieldName + "=\" + " + utilClassName + ".toString(" + fieldName + ")");
                }

                if ((((parentPropertyModeForToString == null || parentPropertyModeForToString == ParentPropertyMode.NONE)
                        || (parentPropertyModeForToString == ParentPropertyMode.LAST && parentGetterMethods.size() == 0)) && i == fieldTypes.size())
                        || (parentPropertyModeForToString == ParentPropertyMode.FIRST && i == parentGetterMethods.size() + fieldTypes.size())) {
                    if (i > 1) {
                        sb.append(IOUtil.LINE_SEPARATOR + iden + iden + "         + \"}\";" + IOUtil.LINE_SEPARATOR);
                    } else {
                        sb.append(" + \"}\";" + IOUtil.LINE_SEPARATOR);
                    }
                }
            }

            if (parentPropertyModeForToString == ParentPropertyMode.LAST && parentGetterMethods.size() > 0) {
                for (Method method : parentGetterMethods) {
                    if (i++ == 0) {
                        sb.append(iden + iden + "return \"{" + ClassUtil.getPropNameByMethod(method) + "=\" + " + utilClassName + ".toString("
                                + method.getName() + "())");
                    } else {
                        sb.append(IOUtil.LINE_SEPARATOR + iden + iden + "         + \", " + ClassUtil.getPropNameByMethod(method) + "=\" + " + utilClassName
                                + ".toString(" + method.getName() + "())");
                    }

                    if (i == parentGetterMethods.size() + fieldTypes.size()) {
                        if (i > 1) {
                            sb.append(IOUtil.LINE_SEPARATOR + iden + iden + "         + \"}\";" + IOUtil.LINE_SEPARATOR);
                        } else {
                            sb.append(" + \"}\";" + IOUtil.LINE_SEPARATOR);
                        }
                    }
                }
            }

            sb.append(iden + "}");

            IOUtil.writeLine(writer, sb.toString());
        }
    }

    static String getAnnoType(Type<?> type, final String packageName, Map<String, Class<?>> importedClasses) {
        if (N.isNullOrEmpty(type.getParameterTypes()) && !type.clazz().equals(Object.class)) {
            return type.name();
        }

        String typeName = type.name();
        final StringBuilder sb = new StringBuilder();
        int start = 0;

        for (int i = 0, len = typeName.length(); i < len; i++) {
            char ch = typeName.charAt(i);

            if (ch == '<' || ch == '>' || ch == ' ' || ch == ',') {
                String str = typeName.substring(start, i);

                if (str.length() > 0 && N.typeOf(str).clazz().equals(Object.class)) {
                    String pkgName = packageName;

                    for (Map.Entry<String, Class<?>> entry : importedClasses.entrySet()) {
                        if (entry.getValue().getSimpleName().equals(str)) {
                            pkgName = ClassUtil.getPackageName(entry.getValue());
                            break;
                        }
                    }

                    if (N.notNullOrEmpty(pkgName) && !N.typeOf(pkgName + "." + str).clazz().equals(Object.class)) {
                        sb.append(pkgName + "." + str);
                    } else {
                        sb.append(str);
                    }
                } else {
                    sb.append(str);
                }

                sb.append(ch);
                start = i + 1;
            }
        }

        if (start < typeName.length()) {
            String str = typeName.substring(start);

            if (str.length() > 0 && N.typeOf(str).clazz().equals(Object.class)) {
                String pkgName = packageName;

                for (Map.Entry<String, Class<?>> entry : importedClasses.entrySet()) {
                    if (entry.getValue().getSimpleName().equals(str)) {
                        pkgName = ClassUtil.getPackageName(entry.getValue());
                        break;
                    }
                }

                if (N.notNullOrEmpty(pkgName) && !N.typeOf(pkgName + "." + str).clazz().equals(Object.class)) {
                    sb.append(pkgName + "." + str);
                } else {
                    sb.append(str);
                }
            } else {
                sb.append(str);
            }
        }

        return sb.toString();
    }

    /**
     * 
     * @param srcDir
     * @param pkgName
     */
    public static void writeUtilClassForHashEqualsToString(final File srcDir, final String pkgName) {
        writeUtilClassForHashEqualsToString(srcDir, pkgName, "_N");
    }

    /**
     * 
     * @param srcDir
     * @param pkgName
     * @param utilClassName
     */
    public static void writeUtilClassForHashEqualsToString(final File srcDir, final String pkgName, final String utilClassName) {
        final String utilClassFilePath = srcDir.getAbsolutePath()
                + (N.isNullOrEmpty(pkgName) ? "" : IOUtil.FILE_SEPARATOR + N.replaceAll(pkgName, ".", IOUtil.FILE_SEPARATOR)) + IOUtil.FILE_SEPARATOR
                + utilClassName + ".java";
        final File utilClassFile = new File(utilClassFilePath);

        if (utilClassFile.exists() == false && IOUtil.createIfNotExists(utilClassFile) == false) {
            throw new RuntimeException("Failed to create new File by path: " + utilClassFilePath);
        }

        if (N.isNullOrEmpty(pkgName)) {
            IOUtil.write(utilClassFile, _N_STRING.replaceFirst("package com.landawn.abacus.util;", "").replaceAll("_N", utilClassName));
        } else {
            IOUtil.write(utilClassFile, _N_STRING.replaceFirst("com.landawn.abacus.util", pkgName).replaceAll("_N", utilClassName));
        }
    }

    public static void printTransferMethod(final Class<?> sourceClass, final Class<?> targetClass) {
        printTransferMethod(sourceClass, targetClass, null);
    }

    public static void printTransferMethod(final Class<?> sourceClass, final Class<?> targetClass, final Map<String, String> propNameMapping) {
        final String iden = "    ";
        final String srcClassName = sourceClass.getSimpleName();
        final String targetClassName = targetClass.getSimpleName();

        StringBuilder sb = new StringBuilder();
        sb.append("public static ").append(targetClassName).append(" ").append(ClassUtil.formalizePropName(srcClassName)).append("2").append(targetClassName)
                .append(" (").append(srcClassName).append(" source) {").append(IOUtil.LINE_SEPARATOR);
        sb.append(iden).append("final ").append(targetClassName).append(" result = new ").append(targetClassName).append("();").append(IOUtil.LINE_SEPARATOR);

        for (Map.Entry<String, Method> entry : ClassUtil.getPropGetMethodList(sourceClass).entrySet()) {
            final Method getMethod = entry.getValue();
            String propName = entry.getKey();

            if (propNameMapping != null && propNameMapping.containsKey(propName)) {
                propName = propNameMapping.get(propName);
            }

            final Method setMethod = ClassUtil.getPropSetMethod(targetClass, propName);

            if (setMethod == null) {
                sb.append(iden).append("// No set method found for: source.").append(getMethod.getName()).append("()").append(IOUtil.LINE_SEPARATOR);
            } else if (!setMethod.getParameterTypes()[0].isAssignableFrom(getMethod.getReturnType())) {
                sb.append(iden).append("// Incompatible parameter type for: source.").append(getMethod.getName()).append("()").append(IOUtil.LINE_SEPARATOR);
            } else {
                sb.append(iden).append("result.").append(setMethod.getName()).append("(source.").append(getMethod.getName()).append("());")
                        .append(IOUtil.LINE_SEPARATOR);
            }
        }

        sb.append(iden).append("return result; ").append(IOUtil.LINE_SEPARATOR);
        sb.append("}").append(IOUtil.LINE_SEPARATOR);

        System.out.println(IOUtil.LINE_SEPARATOR);
        System.out.println(sb.toString());
        System.out.println(IOUtil.LINE_SEPARATOR);
    }

    //    /**
    //     * 
    //     * @param pkgName
    //     */
    //    public static void printUtilClassForHashEqualsToString(final String pkgName) {
    //        printUtilClassForHashEqualsToString(pkgName, "_N");
    //    }
    //
    //    /**
    //     * 
    //     * @param pkgName
    //     * @param utilClassName
    //     */
    //    public static void printUtilClassForHashEqualsToString(final String pkgName, final String utilClassName) {
    //        if (N.isNullOrEmpty(pkgName)) {
    //            N.println(_N_STRING.replaceFirst("package com.landawn.abacus.util;", "").replaceAll("_N", utilClassName));
    //        } else {
    //            N.println(_N_STRING.replaceFirst("com.landawn.abacus.util", pkgName).replaceAll("_N", utilClassName));
    //        }
    //    }

    /**
     * Method newFileWriter.
     *
     * @param classFile
     * @return Writer
     * @throws IOException
     */
    private static Writer newFileWriter(final File classFile) throws IOException {
        if (!classFile.exists()) {
            classFile.createNewFile();
        }

        return new BufferedWriter(new OutputStreamWriter(new FileOutputStream(classFile), UTF_8));
    }

    /**
     * Method writeDomanPropNameClass.
     *
     * @param domainName
     * @param fileWrite
     * @param headSpace
     * @throws IOException
     */
    private static void writeDomainPropNameClass(final String domainName, final Writer fileWrite, final String headSpace) throws IOException {
        fileWrite.write(IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "    public static final String " + DOMAIN_NAME_VAR + " = \"" + domainName + "\".intern();" + IOUtil.LINE_SEPARATOR);
    }

    /**
     * Upper case the first char of the table name.
     *
     * @param tableName
     * @return String
     */
    static String tableName2EntityName(final String tableName) {
        return N.capitalize(formalizePropName(tableName));
    }

    /**
     * SubString {@code columnName} from first under score if it exists. lower the first char of the subString.
     *
     * @param columnName
     * @return String
     */
    static String columnName2PropName(final String columnName) {
        String propName = formalizePropName(columnName);

        return JAVA_TYPE_PROP_NAME.containsKey(propName) ? JAVA_TYPE_PROP_NAME.get(propName) : propName;
    }

    /**
     *
     * @param propName
     * @return
     */
    static String propName2VarName(final String propName) {
        return ClassUtil.toUpperCaseWithUnderscore(ClassUtil.formalizePropName(propName));
    }

    static String propName2MethodName(final String propName) {
        return N.capitalize(propName);
    }

    static String propName2FieldName(final String propName) {
        if (propName.toUpperCase().equals(propName)) {
            return propName.toLowerCase();
        }

        return propName;
    }

    /**
     * Method rule.
     *
     * @param st
     * @return String
     */
    private static String formalizePropName(String st) {
        String temp = st;

        while ((st.length() > 0) && !Character.isLetter(st.charAt(0))) {
            st = st.substring(1);
        }

        if (st.length() == 0) {
            throw new AbacusException("NOT able to generate valid entity/property name by table/column name: " + temp);
        }

        String nst = "";

        boolean isAllUpperCase = true;
        boolean hasUnderScore = false;
        char c = 0;

        for (int i = 0; i < st.length(); i++) {
            c = st.charAt(i);

            if (Character.isLowerCase(c)) {
                isAllUpperCase = false;
            }

            if (c == WD._UNDERSCORE) {
                hasUnderScore = true;
            }
        }

        if (hasUnderScore) {
            if (isAllUpperCase) {
                for (int i = 0; i < st.length(); i++) {
                    c = st.charAt(i);

                    if (c == WD._UNDERSCORE) {
                        i++;

                        if (i < st.length()) {
                            c = st.charAt(i);
                            nst += Character.toUpperCase(c);
                        }
                    } else {
                        nst += Character.toLowerCase(c);
                    }
                }
            } else {
                for (int i = 0; i < st.length(); i++) {
                    c = st.charAt(i);

                    if (c == WD._UNDERSCORE) {
                        i++;

                        if (i < st.length()) {
                            c = st.charAt(i);
                            nst += Character.toUpperCase(c);
                        }
                    } else {
                        nst += c;
                    }
                }
            }
        } else if (isAllUpperCase) {
            nst = st.toLowerCase();
        } else {
            nst = st;
        }

        return N.uncapitalize(nst);
    }

    private static void writeFileHead(final Writer fileWrite) throws IOException {
        fileWrite.write("/*" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(" * Generated by Abacus." + IOUtil.LINE_SEPARATOR);
        fileWrite.write(" */" + IOUtil.LINE_SEPARATOR);
    }

    private static void writePackageName(final String pkgName, final Writer fileWrite) throws IOException {
        if (N.notNullOrEmpty(pkgName)) {
            fileWrite.write("package " + pkgName + ";" + IOUtil.LINE_SEPARATOR);
        }
    }

    private static Map<String, Class<?>> writeImport(final EntityDefinition entityDef, final String pkgName, final String propNameTableClass,
            final EntityMode entityMode, final Class<?> extendedClass, final List<Class<?>> implementedInterfaces, final Writer fileWrite,
            final boolean generateHashEqualsMethod, final boolean generateToStringMethod, final Class<?> utilClass, final Set<Class<?>> annotationImportClasses)
            throws IOException {
        final Map<String, Class<?>> importedClasses = new LinkedHashMap<>();

        if ((IMPL_DIRTY_MARKER == entityMode)) {
            writeClassImport(fileWrite, Collection.class, importedClasses);
            writeClassImport(fileWrite, Set.class, importedClasses);
        }

        for (Property prop : entityDef.getPropertyList()) {
            if (prop.isCollection()) {
                writeClassImport(fileWrite, prop.getType().clazz(), importedClasses);
            }
        }

        Set<Class<?>> sqlTypes = getUsualType(entityDef);

        for (Class<?> sqlType : sqlTypes) {
            if (!sqlType.getCanonicalName().startsWith("java.lang.")) {
                writeClassImport(fileWrite, sqlType, importedClasses);
            }
        }

        if ((IMPL_DIRTY_MARKER == entityMode)) {
            writeClassImport(fileWrite, DirtyMarkerImpl.class, importedClasses);
        }

        if ((generateHashEqualsMethod || generateToStringMethod) && (hasHashEqualsProperty(entityDef) || hasToStringProperty(entityDef))) {
            String utilClassName = ClassUtil.getCanonicalClassName(utilClass);
            if (pkgName.equals(utilClassName.substring(0, utilClassName.lastIndexOf('.'))) || _N.equals(utilClass)) {
                // ignore
            } else {
                writeClassImport(fileWrite, utilClass, importedClasses);
            }
        }

        for (Property prop : entityDef.getPropertyList()) {
            if (prop.getType().name().contains("HBaseColumn<")) {
                writeClassImport(fileWrite, HBaseColumn.class, importedClasses);

                if (prop.getType().isMap() || prop.getType().isCollection()) {
                    writeClassImport(fileWrite, N.class, importedClasses);
                }
            }
        }

        if (extendedClass != null) {
            writeClassImport(fileWrite, extendedClass, importedClasses);
        }

        if (implementedInterfaces != null) {
            for (Class<?> clazz : implementedInterfaces) {
                writeClassImport(fileWrite, clazz, importedClasses);
            }
        }

        if (propNameTableClass != null) {
            fileWrite.write(IOUtil.LINE_SEPARATOR + "import " + propNameTableClass + ";");

        }

        if ((IMPL_DIRTY_MARKER == entityMode) || ((EXTEND_DIRTY_MARKER == entityMode) && (entityDef.getEntiyPropertyList().size() > 0))) {
            writeClassImport(fileWrite, XmlTransient.class, importedClasses);
        }

        if (N.notNullOrEmpty(annotationImportClasses)) {
            for (Class<?> cls : annotationImportClasses) {
                writeClassImport(fileWrite, cls, importedClasses);
            }
        }

        if (N.notNullOrEmpty(entityDef.getPropertyList())) {
            for (Property prop : entityDef.getPropertyList()) {
                if (prop.getType().isGenericType()) {
                    writeClassImport(fileWrite, com.landawn.abacus.annotation.Type.class, importedClasses);
                    break;
                }
            }
        }

        return importedClasses;
    }

    private static void writeClassImport(final Writer fileWrite, Class<?> cls, final Map<String, Class<?>> importedClasses) throws IOException {
        //        if (!cls.isInterface()) {
        //            if (Map.class.isAssignableFrom(cls)) {
        //                cls = Map.class;
        //            } else if (List.class.isAssignableFrom(cls)) {
        //                cls = List.class;
        //            } else if (Set.class.isAssignableFrom(cls)) {
        //                cls = Set.class;
        //            } else if (Queue.class.isAssignableFrom(cls)) {
        //                cls = Queue.class;
        //            } else if (Collection.class.isAssignableFrom(cls)) {
        //                Class<?> tmp = Collection.class;
        //
        //                Class<?>[] interfaceClasses = cls.getInterfaces();
        //                for (Class<?> interfaceClass : interfaceClasses) {
        //                    if (Collection.class.isAssignableFrom(interfaceClass) && !interfaceClass.equals(Collection.class)) {
        //                        tmp = interfaceClass;
        //
        //                        break;
        //                    }
        //                }
        //
        //                cls = tmp;
        //            }
        //        }

        if ((cls != null) && !importedClasses.containsKey(cls.getSimpleName())) {
            fileWrite.write(IOUtil.LINE_SEPARATOR + "import " + ClassUtil.getCanonicalClassName(cls) + ";");
            importedClasses.put(cls.getSimpleName(), cls);
        }
    }

    private static void writeClass(final EntityDefinition entityDef, final String pkgName, final String propNameTableClass, final Method propName2VarName,
            final Method propName2MethodName, final EntityMode entityMode, Class<?> extendedClass, final List<Class<?>> implementedInterfaces,
            final String headSpace, final Writer fileWrite, final boolean generateHashEqualsMethod, final boolean generateToStringMethod,
            final boolean hashEqualsWithParentProperties, final boolean toStringWithParentProperties, final Class<?> utilClass, final boolean fluentSetMethod,
            final Map<String, Class<?>> importedClasses) throws IOException, IllegalAccessException, InvocationTargetException {
        writeClassHead(entityDef, propNameTableClass, extendedClass, implementedInterfaces, headSpace, fileWrite);

        if (entityMode.equals(EntityMode.IMPL_DIRTY_MARKER) && (extendedClass != null) && !DirtyMarkerImpl.class.isAssignableFrom(extendedClass)) {
            extendedClass = DirtyMarkerImpl.class;
        }

        // writePropNameField(entityDef, propName2VarName, fileWrite);
        // writePropNameField(entityDef, propName2VarName, headSpace, fileWrite);

        // =================================================================
        // writeClass(entityDef, fileWrite, isMarkable);
        writeField(entityDef, pkgName, entityMode, extendedClass, implementedInterfaces, headSpace, fileWrite, importedClasses);

        // write default constructor.
        writeDefaultConstructor(entityDef, entityMode, headSpace, fileWrite);

        // write id properties constructor.
        writeIdPropertyConstructor(entityDef, pkgName, propName2MethodName, headSpace, fileWrite, importedClasses);

        if (entityDef.getPropertyList().size() > 0) {
            // write full properties constructor.
            writeFullPropertyConstructor(entityDef, pkgName, propName2MethodName, headSpace, fileWrite, importedClasses);
        }

        // write copy constructor.
        // writeCopyConstructor(entityDef, pkgName, headSpace, fileWrite);

        writeDirtyMarkerMethod(entityDef, entityMode, headSpace, fileWrite);

        // write get/set methods
        writeGetSetMethod(entityDef, pkgName, extendedClass, entityMode, propName2VarName, propName2MethodName, fluentSetMethod, headSpace, fileWrite,
                importedClasses);

        if (entityDef.getPropertyList().size() > 0) {
            if (generateHashEqualsMethod) {
                // write hashCode() method
                writeHashCodeMethod(entityDef, headSpace, fileWrite, extendedClass, hashEqualsWithParentProperties, utilClass);

                // if ((entityMode == EXTEND_DIRTY_MARKER) || (entityMode ==
                // IMPL_DIRTY_MARKER)
                // || (entityMode == POJO)) {
                // write equals method
                writeEqualMethod(entityDef, headSpace, fileWrite, extendedClass, hashEqualsWithParentProperties, utilClass);
            }

            if (generateToStringMethod) {
                // write toString() method
                writeToStringMethod(entityDef, headSpace, fileWrite, extendedClass, toStringWithParentProperties, utilClass);
                // } else if (entityMode == IMPL_ACTIVE_RECORD) {
                // writeHES(headSpace, fileWrite);
                // }
            }
        }

        // =================================================================
        fileWrite.write(headSpace + "}");
    }

    /**
     * Method writeClassComment2.
     *
     * @param fileWrite
     * @throws IOException
     */
    private static void writeClassComment(final Writer fileWrite) throws IOException {
        fileWrite.write(IOUtil.LINE_SEPARATOR);
        fileWrite.write("/**" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(" * Generated by Abacus." + IOUtil.LINE_SEPARATOR);
        fileWrite.write(" * @version ${version}" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(" */" + IOUtil.LINE_SEPARATOR);
    }

    private static void writeClassHead(final EntityDefinition entityDef, final String propNameTableClass, final Class<?> extendedClass,
            final List<Class<?>> implementsInterface, final String headSpace, final Writer fileWrite) throws IOException {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        boolean isAbstract = N.asBoolean(entityDef.getAttribute("abstract"));

        if (isAbstract) {
            sb.append(headSpace + "public abstract class " + entityDef.getName());
        } else {
            sb.append(headSpace + "public class " + entityDef.getName());
        }

        if ((extendedClass != null) && !DirtyMarkerImpl.class.isAssignableFrom(extendedClass)) {
            if (sb.indexOf(" extends ") > 0) {
                sb.append(", " + ClassUtil.getSimpleClassName(extendedClass));
            } else {
                sb.append(" extends " + ClassUtil.getSimpleClassName(extendedClass));
            }
        }

        if (propNameTableClass != null) {
            String simplePropNameTableClass = propNameTableClass;
            int index = propNameTableClass.lastIndexOf(".");

            if (index > -1) {
                simplePropNameTableClass = propNameTableClass.substring(index + 1);
            }

            if (sb.indexOf(" implements ") > 0) {
                sb.append(", " + simplePropNameTableClass + WD._PERIOD + entityDef.getName() + POSTFIX_OF_PROP_NAME_LIST);
            } else {
                sb.append(" implements " + simplePropNameTableClass + WD._PERIOD + entityDef.getName() + POSTFIX_OF_PROP_NAME_LIST);
            }
        }

        if ((implementsInterface != null) && (implementsInterface.size() > 0)) {
            for (Class<?> cls : implementsInterface) {
                if (sb.indexOf(" implements ") > 0) {
                    sb.append(", " + ClassUtil.getSimpleClassName(cls));
                } else {
                    sb.append(" implements " + ClassUtil.getSimpleClassName(cls));
                }
            }
        }

        sb.append(" {" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(sb.toString());

        ObjectFactory.recycle(sb);
    }

    // /**
    // * Method writeStaticField.
    // *
    // * @param entityDef
    // * SQLEntityDefinition
    // * @param entityMode
    // * EntityMode
    // * @param headSpace
    // * String
    // * @param fileWrite
    // * Writer
    // * @throws IOException
    // */
    // private static void writeStaticField(EntityDefinition entityDef, EntityMode
    // entityMode, String headSpace, Writer fileWrite)
    // throws IOException {
    // if ((EntityMode.EXTEND_ACTIVE_RECORD == entityMode) ||
    // (EntityMode.IMPL_ACTIVE_RECORD == entityMode)) {
    // if (entityDef.getEntityType() == EntityType.ENTITY) {
    // fileWrite.write(D.LINE_SEPARATOR + headSpace + "    public static final "
    // + AbstractActiveRecord.class.getName() + "."
    // + AbstractActiveRecord.Calculator.class.getName() + " Cal = new "
    // + AbstractActiveRecord.class.getName() + "."
    // + AbstractActiveRecord.Calculator.class.getName() + "(" +
    // DOMAIN_NAME_VAR + ", _);"
    // + D.LINE_SEPARATOR);
    // }
    // }
    // }

    /**
     *
     * @param entityDef
     * @param pkgName
     * @param entityMode
     * @param extendedClass
     * @param implementedInterfaces
     * @param headSpace
     * @param fileWrite
     * @throws IOException
     */
    private static void writeField(final EntityDefinition entityDef, final String pkgName, final EntityMode entityMode, final Class<?> extendedClass,
            final List<Class<?>> implementedInterfaces, final String headSpace, final Writer fileWrite, final Map<String, Class<?>> importedClasses)
            throws IOException {
        boolean isSerializable = isSerializable(extendedClass, implementedInterfaces);

        if (isSerializable || entityMode == EntityMode.EXTEND_DIRTY_MARKER) {
            writeSerialVersionUID(entityDef, headSpace, fileWrite);
        }

        // writeStaticField(entityDef, entityMode, headSpace, fileWrite);
        if (entityMode == IMPL_DIRTY_MARKER) {
            fileWrite.write(headSpace + "    private final " + ClassUtil.getSimpleClassName(extendedClass) + " " + DIRTY_MARKER_IMPL_FIELD_NAME + " = new "
                    + ClassUtil.getSimpleClassName(extendedClass) + "(" + ENTITY_NAME_VAR + ");" + IOUtil.LINE_SEPARATOR + IOUtil.LINE_SEPARATOR);
        }

        Collection<Property> propList = entityDef.getPropertyList();

        for (Property prop : propList) {
            String fieldName = propName2FieldName(prop.getName());
            String type = getSimpleType(null, prop, pkgName, importedClasses);

            fileWrite.write(headSpace + "    private " + type + " " + fieldName + ";" + IOUtil.LINE_SEPARATOR);
        }
    }

    private static boolean isSerializable(final Class<?> extendedClass, final List<Class<?>> implementedInterfaces) {
        if ((extendedClass != null) && !DirtyMarkerImpl.class.isAssignableFrom(extendedClass) && Serializable.class.isAssignableFrom(extendedClass)) {
            return true;
        }

        if ((implementedInterfaces != null) && (implementedInterfaces.size() > 0)) {
            for (Class<?> clazz : implementedInterfaces) {
                if (Serializable.class.equals(clazz) || Serializable.class.isAssignableFrom(clazz)) {
                    return true;
                }
            }
        }

        return false;
    }

    private static void writeSerialVersionUID(final EntityDefinition entityDef, final String headSpace, final Writer fileWrite) throws IOException {
        long hashCode = entityDef.getName().hashCode();

        for (String propName : entityDef.getPropertyNameList()) {
            if (propName.hashCode() > 0) {
                hashCode += propName.hashCode();
            } else {
                hashCode -= propName.hashCode();
            }
        }

        long serialVersionUID = 0;

        if (String.valueOf(Long.MAX_VALUE).length() > (String.valueOf(hashCode).length() + 1)) {
            serialVersionUID = Long.valueOf(String.valueOf(hashCode) + entityDef.getPropertyList().size());
        } else {
            serialVersionUID = Long.valueOf(String.valueOf(hashCode + entityDef.getPropertyList().size()));
        }

        serialVersionUID = Long.valueOf(String.valueOf(hashCode) + entityDef.getPropertyList().size());
        fileWrite.write(
                headSpace + "    private static final long serialVersionUID = " + serialVersionUID + "L;" + IOUtil.LINE_SEPARATOR + IOUtil.LINE_SEPARATOR);
    }

    private static void writeDefaultConstructor(final EntityDefinition entityDef, final EntityMode entityMode, final String headSpace, final Writer fileWrite)
            throws IOException {
        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + "() {" + IOUtil.LINE_SEPARATOR);

        /*
         * if (inheritLevel == IMPL_ACTIVE_RECORD) { fileWrite.write(headSpace + " activeRecordImpl = new " +
         * inheritClass.getCanonicalName() + "(this);" + N.LINE_SEPARATOR); } else if (inheritLevel ==
         * IMPL_DIRTY_MARKER) { fileWrite.write(headSpace + " dirtyMarkerImpl = new
         * " + inheritClass.getCanonicalName() + "();" + N.LINE_SEPARATOR); }
         */

        if (entityMode == EntityMode.EXTEND_DIRTY_MARKER) {
            fileWrite.write(headSpace + "        super(" + ENTITY_NAME_VAR + ");" + IOUtil.LINE_SEPARATOR);
        }

        fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
    }

    private static Collection<String> writeIdPropertyConstructor(final EntityDefinition entityDef, final String pkgName, final Method propName2MethodName,
            final String headSpace, final Writer fileWrite, final Map<String, Class<?>> importedClasses) throws IOException {

        final List<Property> idPropList = entityDef.getIdPropertyList();
        final List<String> idPropNames = new ArrayList<>(idPropList.size());

        if ((idPropList.size() > 0) && (idPropList.size() < entityDef.getPropertyList().size())) {
            fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + "(");

            fileWrite.write(getIdParaString(pkgName, idPropList, importedClasses));

            fileWrite.write(") {" + IOUtil.LINE_SEPARATOR);

            fileWrite.write(headSpace + "        this();" + IOUtil.LINE_SEPARATOR);
            fileWrite.write(IOUtil.LINE_SEPARATOR);

            for (Property idProp : idPropList) {
                String fieldName = propName2FieldName(idProp.getName());
                fileWrite.write(headSpace + "        set" + ClassUtil.invokeMethod(propName2MethodName, idProp.getName()) + "(" + fieldName + ");"
                        + IOUtil.LINE_SEPARATOR);
            }

            fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

            List<String> tmp = new ArrayList<>(entityDef.getPropertyNameList());

            for (Property idProp : idPropList) {
                idPropNames.add(idProp.getName());
                tmp.remove(idProp.getName());
            }

            boolean writeNonIdConstructor = false;
            if (tmp.size() > entityDef.getIdPropertyList().size()) {
                writeNonIdConstructor = true;
            } else {
                for (int i = 0; i < tmp.size(); i++) {
                    if (!entityDef.getProperty(tmp.get(i)).getType().clazz().isAssignableFrom(entityDef.getIdPropertyList().get(i).getType().clazz())) {
                        writeNonIdConstructor = true;

                        break;
                    }
                }
            }

            if (writeNonIdConstructor) {
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + "(");

                int length = 8;
                String headEmpty = "";

                for (int j = 0; j < length; j++) {
                    headEmpty += " ";
                }

                int i = 1;

                // tempList.addAll(0, entityDef.getIdPropNameList());
                for (String propName : tmp) {
                    Property prop = entityDef.getProperty(propName);
                    String para = getSimpleType(null, prop, pkgName, importedClasses) + " " + propName2FieldName(prop.getName());

                    if (i < tmp.size()) {
                        para += ", ";
                    }

                    length += para.length();

                    if (length > 80) {
                        length = 8 + para.length();
                        para = IOUtil.LINE_SEPARATOR + headEmpty + para;
                    }

                    fileWrite.write(para);

                    i++;
                }

                fileWrite.write(") {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        this();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(IOUtil.LINE_SEPARATOR);

                for (String propName : tmp) {
                    Property prop = entityDef.getProperty(propName);
                    fileWrite.write(headSpace + "        set" + ClassUtil.invokeMethod(propName2MethodName, prop.getName()) + "("
                            + propName2FieldName(prop.getName()) + ");" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }
        }

        return idPropNames;
    }

    private static void writeFullPropertyConstructor(final EntityDefinition entityDef, final String pkgName, final Method propName2MethodName,
            final String headSpace, final Writer fileWrite, final Map<String, Class<?>> importedClasses) throws IOException {
        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + "(");

        int length = 8;
        String headEmpty = "";

        for (int j = 0; j < length; j++) {
            headEmpty += " ";
        }

        List<String> tempList = new ArrayList<>(entityDef.getPropertyNameList());
        for (Property idProp : entityDef.getIdPropertyList()) {
            tempList.remove(idProp.getName());
        }

        int i = 0;
        for (Property idProp : entityDef.getIdPropertyList()) {
            tempList.add(i++, idProp.getName());
        }

        i = 0;
        for (String propName : tempList) {
            Property prop = entityDef.getProperty(propName);
            String para = getSimpleType(null, prop, pkgName, importedClasses) + " " + propName2FieldName(prop.getName());

            if (++i < tempList.size()) {
                para += ", ";
            }

            length += para.length();

            if (length > 80) {
                length = 8 + para.length();
                para = IOUtil.LINE_SEPARATOR + headEmpty + para;
            }

            fileWrite.write(para);
        }

        fileWrite.write(") {" + IOUtil.LINE_SEPARATOR);

        fileWrite.write(headSpace + "        this();" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(IOUtil.LINE_SEPARATOR);

        for (String propName : tempList) {
            Property prop = entityDef.getProperty(propName);
            fileWrite.write(headSpace + "        set" + ClassUtil.invokeMethod(propName2MethodName, prop.getName()) + "(" + propName2FieldName(prop.getName())
                    + ");" + IOUtil.LINE_SEPARATOR);
        }

        fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
    }

    /**
     * Method writeCopyConstructor.
     *
     * @param entityDef
     * @param pkgName
     * @param headSpace
     * @param fileWrite
     * @throws IOException
     */
    /*
     * static void writeCopyConstructor(EntityDefinition entityDef, String pkgName, String headSpace, Writer
     * fileWrite) throws IOException { String objectFieldName = N.lowCaseFirstChar(entityDef.getName());
     * fileWrite.write(N.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + "(" + entityDef.getName() +
     * " " + objectFieldName + ") {" + N.LINE_SEPARATOR); fileWrite.write(headSpace + "        this();" +
     * N.LINE_SEPARATOR);
     *
     * for (Property prop : entityDef.getPropertyList()) { fileWrite.write(headSpace + "        set" +
     * getMethodName(prop) + "(" + objectFieldName + "." + prop.getName() + ");" + N.LINE_SEPARATOR); }
     *
     * fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR); }
     */
    @SuppressWarnings("rawtypes")
    private static void writeGetSetMethod(final EntityDefinition entityDef, final String pkgName, final Class<?> extendedClass, final EntityMode entityMode,
            final Method propName2VarName, final Method propName2MethodName, final boolean fluentSetMethod, final String headSpace, final Writer fileWrite,
            final Map<String, Class<?>> importedClasses) throws IOException {
        final Collection<Property> props = entityDef.getPropertyList();

        if (fluentSetMethod) {
            try {
                final Class<?> superClass = extendedClass == null && N.notNullOrEmpty(entityDef.getAttribute("extends"))
                        ? ClassUtil.forClass(entityDef.getAttribute("extends")) : extendedClass;
                if (superClass != null) {
                    Map<String, Method> propSetMethodMap = ClassUtil.getPropSetMethodList(superClass);
                    for (String propName : propSetMethodMap.keySet()) {
                        if (entityDef.getProperty(propName) != null) {
                            continue;
                        }

                        final String methodName = ClassUtil.invokeMethod(propName2MethodName, propName);
                        final String fieldName = propName2FieldName(propName);
                        String typeName = ClassUtil.getParameterizedTypeNameByMethod(propSetMethodMap.get(propName)).replaceAll("java.lang.", "");

                        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + typeName + " "
                                + fieldName + ") {" + IOUtil.LINE_SEPARATOR);

                        fileWrite.write(headSpace + "        super.set" + methodName + "(" + fieldName + ");" + IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
                    }
                }
            } catch (Throwable e) {
                logger.error("Failed to generate fluent set methods for parent properties", e);
            }
        }

        for (Property prop : props) {
            final String methodName = ClassUtil.invokeMethod(propName2MethodName, prop.getName());
            final String fieldName = propName2FieldName(prop.getName());
            final String propNameVar = ClassUtil.invokeMethod(propName2VarName, prop.getName());
            final String simpleTypeName = getSimpleType(null, prop, pkgName, importedClasses);

            // fileWrite.write(N.LINE_SEPARATOR + headSpace + "    @Type(\"" + prop.getType().getName() + "\")");
            //            if (N.notNullOrEmpty(prop.getType().getParameterTypes())) {
            //                fileWrite.write(N.LINE_SEPARATOR + headSpace + "    @Type(\"" + prop.getType().getName() + "\")");
            //            }

            fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + simpleTypeName + " get" + methodName + "() {" + IOUtil.LINE_SEPARATOR);
            fileWrite.write(headSpace + "        return " + fieldName + ";" + IOUtil.LINE_SEPARATOR);
            fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

            if (fluentSetMethod) {
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + simpleTypeName + " "
                        + fieldName + ") {" + IOUtil.LINE_SEPARATOR);
            } else {
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public void set" + methodName + "(" + simpleTypeName + " " + fieldName + ") {"
                        + IOUtil.LINE_SEPARATOR);
            }

            if (EntityMode.POJO != entityMode && EntityMode.POJO_WITH_PROP_NAME_TABLE != entityMode) {
                String word = EXTEND_DIRTY_MARKER == entityMode ? "super" : DIRTY_MARKER_IMPL_FIELD_NAME;
                fileWrite.write(headSpace + "        " + word + ".setUpdatedPropName(" + propNameVar + ");" + IOUtil.LINE_SEPARATOR);
            }

            fileWrite.write(headSpace + "        this." + fieldName + " = " + fieldName + ";" + IOUtil.LINE_SEPARATOR);

            if (fluentSetMethod) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
            }

            fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

            if (prop.getType().clazz().equals(HBaseColumn.class)) {
                HBaseColumnType<?> hbaseColumnType = (HBaseColumnType) prop.getType();

                if (hbaseColumnType.getParameterTypes()[0].isEntity()) {
                    throw new AbacusException("Family/Entity property can't/unnecessary to be HBaseColumn");
                }

                String valueTypeName = N.isPrimitiveWapper(hbaseColumnType.getElementType().clazz())
                        ? N.primitiveOf(hbaseColumnType.getElementType().clazz()).getSimpleName() : hbaseColumnType.getElementType().name();

                // =========================
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    /**");
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "     * Returns the (first) column or an empty column if it's null.");
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "     */");
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + hbaseColumnType.name() + " " + fieldName + "() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return (" + hbaseColumnType.name() + ") (this." + fieldName + " == null ? "
                        + HBaseColumn.class.getSimpleName() + ".emptyOf(" + valueTypeName + ".class) : " + fieldName + ");" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + valueTypeName
                            + " value) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(
                            IOUtil.LINE_SEPARATOR + headSpace + "    public void set" + methodName + "(" + valueTypeName + " value) {" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        set" + methodName + "(" + HBaseColumn.class.getSimpleName() + ".valueOf(value));" + IOUtil.LINE_SEPARATOR);

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + valueTypeName
                            + " value, long version) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public void set" + methodName + "(" + valueTypeName + " value, long version) {"
                            + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        set" + methodName + "(" + HBaseColumn.class.getSimpleName() + ".valueOf(value, version));"
                        + IOUtil.LINE_SEPARATOR);

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

            } else if ((Collection.class.isAssignableFrom(prop.getType().clazz()) && prop.getType().getParameterTypes()[0].clazz().equals(HBaseColumn.class))
                    || (Map.class.isAssignableFrom(prop.getType().clazz()) && prop.getType().getParameterTypes()[1].clazz().equals(HBaseColumn.class))) {

                HBaseColumnType<?> hbaseColumnType = null;
                if (Collection.class.isAssignableFrom(prop.getType().clazz())) {
                    hbaseColumnType = (HBaseColumnType) prop.getType().getParameterTypes()[0];
                } else {
                    hbaseColumnType = (HBaseColumnType) prop.getType().getParameterTypes()[1];
                }

                if (hbaseColumnType.getParameterTypes()[0].isEntity()) {
                    throw new AbacusException("Family/Entity property can't/unnecessary to be HBaseColumn");
                }

                String valueTypeName = N.isPrimitiveWapper(hbaseColumnType.getElementType().clazz())
                        ? N.primitiveOf(hbaseColumnType.getElementType().clazz()).getSimpleName() : hbaseColumnType.getElementType().name();

                // =========================
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    /**");
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "     * Returns the (first) column or an empty column if it's null.");
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "     */");
                fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + hbaseColumnType.name() + " " + fieldName + "() {" + IOUtil.LINE_SEPARATOR);

                if (Collection.class.isAssignableFrom(prop.getType().clazz())) {
                    fileWrite.write(headSpace + "        return (" + hbaseColumnType.name() + ") (N.isNullOrEmpty(" + fieldName + ") ? "
                            + HBaseColumn.class.getSimpleName() + ".emptyOf(" + valueTypeName + ".class) : " + fieldName + ".iterator().next());"
                            + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(headSpace + "        return (" + hbaseColumnType.name() + ") (N.isNullOrEmpty(" + fieldName + ") ? "
                            + HBaseColumn.class.getSimpleName() + ".emptyOf(" + valueTypeName + ".class) : " + fieldName + ".values().iterator().next());"
                            + IOUtil.LINE_SEPARATOR);
                }
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + valueTypeName
                            + " value) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(
                            IOUtil.LINE_SEPARATOR + headSpace + "    public void set" + methodName + "(" + valueTypeName + " value) {" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        set" + methodName + "(" + HBaseColumn.class.getSimpleName() + ".valueOf(value));" + IOUtil.LINE_SEPARATOR);

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + valueTypeName
                            + " value, long version) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public void set" + methodName + "(" + valueTypeName + " value, long version) {"
                            + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        set" + methodName + "(" + HBaseColumn.class.getSimpleName() + ".valueOf(value, version));"
                        + IOUtil.LINE_SEPARATOR);

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " set" + methodName + "(" + hbaseColumnType.name()
                            + " hbaseColumn) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public void set" + methodName + "(" + hbaseColumnType.name() + " hbaseColumn) {"
                            + IOUtil.LINE_SEPARATOR);
                }

                if (EntityMode.POJO != entityMode && EntityMode.POJO_WITH_PROP_NAME_TABLE != entityMode) {
                    String word = EXTEND_DIRTY_MARKER == entityMode ? "super" : DIRTY_MARKER_IMPL_FIELD_NAME;
                    fileWrite.write(headSpace + "        " + word + ".setUpdatedPropName(" + propNameVar + ");" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        if (" + fieldName + " == null) {" + IOUtil.LINE_SEPARATOR);

                if (prop.getType().clazz().equals(SortedSet.class) || prop.getType().clazz().equals(NavigableSet.class)) {
                    fileWrite.write(headSpace + "            " + fieldName + " = new java.util.TreeSet<" + prop.getType().getParameterTypes()[0].name()
                            + ">(HBaseColumn.DESC_HBASE_COLUMN_COMPARATOR);" + IOUtil.LINE_SEPARATOR);
                } else if (prop.getType().clazz().equals(SortedMap.class) || prop.getType().clazz().equals(NavigableMap.class)) {
                    fileWrite.write(headSpace + "            " + fieldName + " = new java.util.TreeMap<" + prop.getType().getParameterTypes()[0].name() + ", "
                            + prop.getType().getParameterTypes()[1].name() + ">(HBaseColumn.DESC_HBASE_VERSION_COMPARATOR);" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(headSpace + "            " + fieldName + " = N.newInstance(" + ClassUtil.getCanonicalClassName(prop.getType().clazz())
                            + ".class);" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        } else {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "            " + fieldName + ".clear();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        }" + IOUtil.LINE_SEPARATOR);

                if (Collection.class.isAssignableFrom(prop.getType().clazz())) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "        " + fieldName + ".add(hbaseColumn);" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(
                            IOUtil.LINE_SEPARATOR + headSpace + "        " + fieldName + ".put(hbaseColumn.version(), hbaseColumn);" + IOUtil.LINE_SEPARATOR);
                }

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " add" + methodName + "(" + valueTypeName
                            + " value) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(
                            IOUtil.LINE_SEPARATOR + headSpace + "    public void add" + methodName + "(" + valueTypeName + " value) {" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        add" + methodName + "(" + HBaseColumn.class.getSimpleName() + ".valueOf(value));" + IOUtil.LINE_SEPARATOR);

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " add" + methodName + "(" + valueTypeName
                            + " value, long version) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public void add" + methodName + "(" + valueTypeName + " value, long version) {"
                            + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        add" + methodName + "(" + HBaseColumn.class.getSimpleName() + ".valueOf(value, version));"
                        + IOUtil.LINE_SEPARATOR);

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                // =========================
                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public " + entityDef.getName() + " add" + methodName + "(" + hbaseColumnType.name()
                            + " hbaseColumn) {" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public void add" + methodName + "(" + hbaseColumnType.name() + " hbaseColumn) {"
                            + IOUtil.LINE_SEPARATOR);
                }

                if (EntityMode.POJO != entityMode && EntityMode.POJO_WITH_PROP_NAME_TABLE != entityMode) {
                    String word = EXTEND_DIRTY_MARKER == entityMode ? "super" : DIRTY_MARKER_IMPL_FIELD_NAME;
                    fileWrite.write(headSpace + "        " + word + ".setUpdatedPropName(" + propNameVar + ");" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        if (" + fieldName + " == null) {" + IOUtil.LINE_SEPARATOR);

                if (prop.getType().clazz().equals(SortedSet.class) || prop.getType().clazz().equals(NavigableSet.class)) {
                    fileWrite.write(headSpace + "            " + fieldName + " = new java.util.TreeSet<" + prop.getType().getParameterTypes()[0].name()
                            + ">(HBaseColumn.DESC_HBASE_COLUMN_COMPARATOR);" + IOUtil.LINE_SEPARATOR);
                } else if (prop.getType().clazz().equals(SortedMap.class) || prop.getType().clazz().equals(NavigableMap.class)) {
                    fileWrite.write(headSpace + "            " + fieldName + " = new java.util.TreeMap<" + prop.getType().getParameterTypes()[0].name() + ", "
                            + prop.getType().getParameterTypes()[1].name() + ">(HBaseColumn.DESC_HBASE_VERSION_COMPARATOR);" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(headSpace + "            " + fieldName + " = N.newInstance(" + ClassUtil.getCanonicalClassName(prop.getType().clazz())
                            + ".class);" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "        }" + IOUtil.LINE_SEPARATOR);

                if (Collection.class.isAssignableFrom(prop.getType().clazz())) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "        " + fieldName + ".add(hbaseColumn);" + IOUtil.LINE_SEPARATOR);
                } else {
                    fileWrite.write(
                            IOUtil.LINE_SEPARATOR + headSpace + "        " + fieldName + ".put(hbaseColumn.version(), hbaseColumn);" + IOUtil.LINE_SEPARATOR);
                }

                if (fluentSetMethod) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "        return this;" + IOUtil.LINE_SEPARATOR);
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }
        }
    }

    private static void writeHashCodeMethod(final EntityDefinition entityDef, final String headSpace, final Writer fileWrite, final Class<?> extendedClass,
            final boolean hashEqualsWithParentProperties, final Class<?> utilClass) throws IOException {
        if (hasHashEqualsProperty(entityDef) == false) {
            return;
        }

        final String utilClassName = ClassUtil.getSimpleClassName(utilClass);

        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public int hashCode() {" + IOUtil.LINE_SEPARATOR);

        String st = headSpace + "        int h = 17;";

        for (Property prop : entityDef.getIdPropertyList()) {
            String attr = prop.getAttribute("hashEquals");

            if (attr == null || Boolean.valueOf(attr)) {
                st += (IOUtil.LINE_SEPARATOR + headSpace + "        ");
                st += ("h = 31 * h + " + utilClassName + ".hashCode(" + propName2FieldName(prop.getName()) + ");");
            }
        }

        for (Property prop : entityDef.getPropertyList()) {
            if (!prop.isId()) {
                String attr = prop.getAttribute("hashEquals");

                if (attr == null || Boolean.valueOf(attr)) {
                    st += (IOUtil.LINE_SEPARATOR + headSpace + "        ");
                    st += ("h = 31 * h + " + utilClassName + ".hashCode(" + propName2FieldName(prop.getName()) + ");");
                }
            }
        }

        if (extendedClass != null && hashEqualsWithParentProperties) {
            Map<String, Method> propMethodMap = ClassUtil.getPropGetMethodList(extendedClass);

            for (Method method : propMethodMap.values()) {
                st += (IOUtil.LINE_SEPARATOR + headSpace + "        ");
                st += ("h = 31 * h + " + utilClassName + ".hashCode(" + method.getName() + "());");
            }
        }

        st += IOUtil.LINE_SEPARATOR;

        st += (IOUtil.LINE_SEPARATOR + headSpace + "        return h;");
        fileWrite.write(st + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
    }

    private static void writeEqualMethod(final EntityDefinition entityDef, final String headSpace, final Writer fileWrite, final Class<?> extendedClass,
            boolean hashEqualsWithParentProperties, final Class<?> utilClass) throws IOException {
        if (hasHashEqualsProperty(entityDef) == false) {
            return;
        }

        final String utilClassName = ClassUtil.getSimpleClassName(utilClass);

        String hashEqualsWithParentPropertiesAttr = entityDef.getAttribute("hashEqualsWithParentProperties");

        if (N.notNullOrEmpty(hashEqualsWithParentPropertiesAttr)) {
            hashEqualsWithParentProperties = Boolean.valueOf(hashEqualsWithParentPropertiesAttr);
        }

        fileWrite.write(IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "    public boolean equals(Object obj) {" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "        if (this == obj) {" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "            return true;" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "        }" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "        if (obj instanceof " + entityDef.getName() + ") {" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "            final " + entityDef.getName() + " other = (" + entityDef.getName() + ") obj;" + IOUtil.LINE_SEPARATOR);

        fileWrite.write(IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "            return ");

        int i = 0;

        for (Property prop : entityDef.getIdPropertyList()) {
            String attr = prop.getAttribute("hashEquals");

            if (attr == null || Boolean.valueOf(attr)) {
                if (i++ > 0) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "                && ");
                }

                String fieldName = propName2FieldName(prop.getName());
                fileWrite.write(utilClassName + ".equals(" + fieldName + ", other." + fieldName + ")");
            }
        }

        for (Property prop : entityDef.getPropertyList()) {
            if (!prop.isId()) {
                String attr = prop.getAttribute("hashEquals");

                if (attr == null || Boolean.valueOf(attr)) {
                    if (i++ > 0) {
                        fileWrite.write(IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "                && ");
                    }

                    String fieldName = propName2FieldName(prop.getName());
                    fileWrite.write(utilClassName + ".equals(" + fieldName + ", other." + fieldName + ")");
                }
            }
        }

        if (extendedClass != null && hashEqualsWithParentProperties) {
            Map<String, Method> propMethodMap = ClassUtil.getPropGetMethodList(extendedClass);

            for (Method method : propMethodMap.values()) {
                if (i++ > 0) {
                    fileWrite.write(IOUtil.LINE_SEPARATOR);
                    fileWrite.write(headSpace + "                && ");
                }

                String methodName = method.getName() + "()";
                fileWrite.write(utilClassName + ".equals(" + methodName + ", other." + methodName + ")");
            }
        }

        fileWrite.write(";" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "        }" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "        return false;" + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
    }

    private static void writeToStringMethod(final EntityDefinition entityDef, final String headSpace, final Writer fileWrite, final Class<?> extendedClass,
            final boolean toStringWithParentProperties, final Class<?> utilClass) throws IOException {
        if (hasToStringProperty(entityDef) == false) {
            return;
        }

        final String utilClassName = ClassUtil.getSimpleClassName(utilClass);

        String st = "";
        int i = 0;
        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "    public String toString() {" + IOUtil.LINE_SEPARATOR);

        /*
         * if (entityDef.getEntityType() == EntityType.ENTITY) { st = headSpace + " return \"" + entityDef.getName() +
         * "={\"" + N.LINE_SEPARATOR + headSpace + " "; } else { st = headSpace + " return \"{\"" + N.LINE_SEPARATOR +
         * headSpace + " "; }
         */

        if (extendedClass != null && toStringWithParentProperties) {
            Map<String, Method> propMethodMap = ClassUtil.getPropGetMethodList(extendedClass);

            for (Method method : propMethodMap.values()) {
                String propName = ClassUtil.getPropNameByMethod(method);
                if (i++ == 0) {
                    st += ("         return \"{" + propName + "=\"" + " + " + utilClassName + ".toString(" + method.getName() + "())");
                } else {
                    st += ("         + \", " + propName + "=\"" + " + " + utilClassName + ".toString(" + method.getName() + "())");
                }

                st += (IOUtil.LINE_SEPARATOR + headSpace + "        ");
            }
        }

        for (Property prop : entityDef.getIdPropertyList()) {
            String attr = prop.getAttribute("toString");

            if (attr == null || Boolean.valueOf(attr)) {
                if (i++ == 0) {
                    st += ("         return \"{" + propName2FieldName(prop.getName()) + "=\"" + " + " + utilClassName + ".toString("
                            + propName2FieldName(prop.getName()) + ")");

                } else {
                    st += ("         + \", " + propName2FieldName(prop.getName()) + "=\"" + " + " + utilClassName + ".toString("
                            + propName2FieldName(prop.getName()) + ")");
                }

                st += (IOUtil.LINE_SEPARATOR + headSpace + "        ");
            }
        }

        for (Property prop : entityDef.getPropertyList()) {
            if (!prop.isId()) {
                String attr = prop.getAttribute("toString");

                if (attr == null || Boolean.valueOf(attr)) {
                    if (i++ == 0) {
                        st += ("         return \"{" + propName2FieldName(prop.getName()) + "=\"" + " + " + utilClassName + ".toString("
                                + propName2FieldName(prop.getName()) + ")");

                    } else {
                        st += ("         + \", " + propName2FieldName(prop.getName()) + "=\"" + " + " + utilClassName + ".toString("
                                + propName2FieldName(prop.getName()) + ")");
                    }

                    st += (IOUtil.LINE_SEPARATOR + headSpace + "        ");
                }
            }
        }

        st = st.substring(0, st.length() - (IOUtil.LINE_SEPARATOR + headSpace + "        ").length());

        if (st.indexOf("         + \",") > 0) {
            st += (IOUtil.LINE_SEPARATOR + headSpace + "        ") + "         + \"}\";";
        } else {
            st += " + \"}\";";
        }
        fileWrite.write(st + IOUtil.LINE_SEPARATOR);
        fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
    }

    private static boolean hasHashEqualsProperty(final EntityDefinition entityDef) {
        final Collection<Property> props = entityDef.getPropertyList();

        for (Property prop : props) {
            String attr = prop.getAttribute("hashEquals");

            if (attr == null || Boolean.valueOf(attr)) {
                return true;
            }
        }

        return false;
    }

    private static boolean hasToStringProperty(final EntityDefinition entityDef) {
        final Collection<Property> props = entityDef.getPropertyList();

        for (Property prop : props) {
            String attr = prop.getAttribute("toString");

            if (attr == null || Boolean.valueOf(attr)) {
                return true;
            }
        }

        return false;
    }

    /*
     * static void writeHES(String headSpace, Writer fileWrite) throws IOException { fileWrite.write(N.LINE_SEPARATOR);
     * fileWrite.write(headSpace + "    public int hashCode() {" + N.LINE_SEPARATOR); fileWrite.write(headSpace +
     * "        return activeRecordImpl.hashCode();" + N.LINE_SEPARATOR); fileWrite.write(headSpace + "    }" +
     * N.LINE_SEPARATOR);
     *
     * fileWrite.write(N.LINE_SEPARATOR); fileWrite.write(headSpace + "    public boolean equals(Object obj) {" +
     * N.LINE_SEPARATOR); fileWrite.write(headSpace + "        return activeRecordImpl.equals(anObject);" +
     * N.LINE_SEPARATOR); fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR);
     *
     * fileWrite.write(N.LINE_SEPARATOR); fileWrite.write(headSpace + "    public String toString() {" +
     * N.LINE_SEPARATOR); fileWrite.write(headSpace + "        return activeRecordImpl.toString();" + N.LINE_SEPARATOR);
     * fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR); }
     */
    private static void writeDirtyMarkerMethod(final EntityDefinition entityDef, final EntityMode entityMode, final String headSpace, final Writer fileWrite)
            throws IOException {
        if (!(EntityMode.POJO.equals(entityMode) || EntityMode.POJO_WITH_PROP_NAME_TABLE.equals(entityMode))) {
            String word = EXTEND_DIRTY_MARKER == entityMode ? "super" : DIRTY_MARKER_IMPL_FIELD_NAME;

            if (entityMode == EntityMode.IMPL_DIRTY_MARKER) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public String entityName() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + ENTITY_NAME_VAR + ";" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }

            if (((EntityMode.IMPL_DIRTY_MARKER == entityMode) || (EntityMode.EXTEND_DIRTY_MARKER == entityMode))
                    && (entityDef.getEntiyPropertyList().size() > 0)) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public boolean isDirty() {" + IOUtil.LINE_SEPARATOR);

                String temp = headSpace + "        return " + word + ".isDirty()";

                Collection<Property> props = entityDef.getPropertyList();

                for (Property prop : props) {
                    if (prop.getColumnType().isEntity() && !prop.isCollection()) {
                        temp += (IOUtil.LINE_SEPARATOR + headSpace + "               || (" + prop.getName() + " == null ? false : " + prop.getName()
                                + ".isDirty())");
                    }
                }

                for (Property prop : props) {
                    if (prop.isCollection()) {
                        temp += (IOUtil.LINE_SEPARATOR + headSpace + "               || (" + prop.getName() + " == null ? false : " + word + ".isEntityDirty("
                                + prop.getName() + "))");
                    }
                }

                temp += (";" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(temp);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            } else if (entityMode == IMPL_DIRTY_MARKER) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public boolean isDirty() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + word + ".isDirty();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }

            if (entityMode == IMPL_DIRTY_MARKER) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public boolean isDirty(String propName) {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + word + ".isDirty(propName);" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }

            if (((EntityMode.IMPL_DIRTY_MARKER == entityMode) || (EntityMode.EXTEND_DIRTY_MARKER == entityMode))
                    && (entityDef.getEntiyPropertyList().size() > 0)) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public void markDirty(boolean isDirty) {" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(headSpace + "        " + word + ".markDirty(isDirty);" + IOUtil.LINE_SEPARATOR);

                Collection<Property> props = entityDef.getPropertyList();

                for (Property prop : props) {
                    if (prop.getColumnType().isEntity() && !prop.isCollection()) {
                        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "        if (" + prop.getName() + " != null) {" + IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "            " + prop.getName() + ".markDirty(isDirty);" + IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "        }" + IOUtil.LINE_SEPARATOR);
                    }
                }

                for (Property prop : props) {
                    if (prop.isCollection()) {
                        fileWrite.write(IOUtil.LINE_SEPARATOR + headSpace + "        if (" + prop.getName() + " != null) {" + IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "            " + word + ".markEntityDirty(" + prop.getName() + ", isDirty);" + IOUtil.LINE_SEPARATOR);
                        fileWrite.write(headSpace + "        }" + IOUtil.LINE_SEPARATOR);
                    }
                }

                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            } else if (entityMode == IMPL_DIRTY_MARKER) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public void markDirty(boolean isDirty) {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        " + word + ".markDirty(isDirty);" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }

            if (entityMode == IMPL_DIRTY_MARKER) {
                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public void markDirty(String propName, boolean isDirty) {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        " + word + ".markDirty(propName, isDirty);" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public void markDirty(Collection<String> propNames, boolean isDirty) {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        " + word + ".markDirty(propNames, isDirty);" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public Set<String> signedPropNames() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + word + ".signedPropNames();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public Set<String> dirtyPropNames() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + word + ".dirtyPropNames();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public void freeze() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        " + word + ".freeze();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public boolean frozen() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + word + ".frozen();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);

                fileWrite.write(IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    @XmlTransient" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    public long version() {" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "        return " + word + ".version();" + IOUtil.LINE_SEPARATOR);
                fileWrite.write(headSpace + "    }" + IOUtil.LINE_SEPARATOR);
            }

            // if (inheritLevel == IMPL_DIRTY_MARKER) {
            // fileWrite.write(N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    public int hashCode() {" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace +
            // "        return dirtyMarkerImpl.hashCode(this);"
            // + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR);
            //
            // fileWrite.write(N.LINE_SEPARATOR);
            // fileWrite.write(headSpace +
            // "    public boolean equals(Object obj) {"
            // + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace
            // + "        return dirtyMarkerImpl.equals(this, anObject);" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR);
            //
            // fileWrite.write(N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    public String toString() {" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace +
            // "        return dirtyMarkerImpl.toString(this);"
            // + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR);
            // }

            /*
             * if ((inheritLevel == IMPL_ACTIVE_RECORD) || (inheritLevel == IMPL_DIRTY_MARKER)) {
             * fileWrite.write(N.LINE_SEPARATOR); fileWrite.write(headSpace + " public void clear() {" +
             * N.LINE_SEPARATOR); fileWrite.write(headSpace + " " + word + ".clear();" + N.LINE_SEPARATOR);
             * fileWrite.write(headSpace + " }" + N.LINE_SEPARATOR); }
             */

            // if (hasCollectionProp) {
            // // write isDirty(Collection<? extends DirtyMarker> objects)
            // // method
            // fileWrite.write(N.LINE_SEPARATOR);
            // fileWrite.write(headSpace
            // + "    private boolean hasDirtyEntity(Collection<? extends "
            // + DirtyMarker.class.getName() + "> entities) {" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "        if (entities == null) {" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "            return false;" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "        } else {" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "            for (" +
            // DirtyMarker.class.getName()
            // + " entity : entities) {" + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace +
            // "                if (entity.isDirty()) {"
            // + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "                    return true;" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "                }" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "            }" + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "            return false;" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "        }" + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR);
            //
            // // write isDirty(Collection<? extends DirtyMarker> entities,
            // // String propName) method
            // /*
            // * fileWrite.write(N.LINE_SEPARATOR); fileWrite.write(headSpace +
            // "
            // * private boolean isDirty(Collection<? extends DirtyMarker>
            // * entities, String propName) {" + N.LINE_SEPARATOR);
            // * fileWrite.write(headSpace + " if (objects == null) {" +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " return false;"
            // +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " } else {" +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " for
            // * (DirtyMarker entity : entities) {" + N.LINE_SEPARATOR);
            // * fileWrite.write(headSpace + " if (entity.isDirty(propName)) {"
            // +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " return true;"
            // +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " }" +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " }" +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " return false;"
            // +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " }" +
            // * N.LINE_SEPARATOR); fileWrite.write(headSpace + " }" +
            // * N.LINE_SEPARATOR);
            // */
            //
            // // write markDirty(Collection<? extends ActiveRecord> entities,
            // // boolean isDirty) method
            // fileWrite.write(N.LINE_SEPARATOR);
            // fileWrite.write(headSpace
            // + "    private void markEntityDirty(Collection<? extends "
            // + DirtyMarker.class.getName() +
            // "> entities, boolean isDirty) {"
            // + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "        if (entities != null) {" +
            // N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "            for (" +
            // DirtyMarker.class.getName()
            // + " entity : entities) {" + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace +
            // "                entity.markDirty(isDirty);"
            // + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "            }" + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "        }" + N.LINE_SEPARATOR);
            // fileWrite.write(headSpace + "    }" + N.LINE_SEPARATOR);
            // }
        }
    }

    private static String getSimpleType(Type<?> type, final Property prop, final String pkgName, final Map<String, Class<?>> importedClasses) {
        type = type == null ? prop.getType() : type;
        final Class<?> typeClass = type.clazz();

        String typeName = null;

        if (type.isGenericType()) {
            typeName = type.name();
        } else {
            Class<?> clazz = type.clazz();
            typeName = Object.class.equals(clazz) && !type.name().equals(ObjectType.OBJECT) ? type.name() : clazz.getCanonicalName();
        }

        if (typeClass.isArray()) {
            String componentClassName = N.substring(typeName, 0, typeName.indexOf('[')).get();
            if (importedClasses.containsKey(componentClassName)) {
                typeName = typeName.replaceAll(componentClassName.substring(0, componentClassName.lastIndexOf('.') + 1), "");
            }
        } else if (typeName.startsWith("java.lang.") || (importedClasses.containsValue(typeClass) && N.notNullOrEmpty(ClassUtil.getPackageName(typeClass)))) {
            typeName = typeName.replace(ClassUtil.getPackageName(typeClass) + ".", "");
        }

        if (type.isGenericType()) {
            Type<?>[] paramTypes = type.getParameterTypes();

            if (N.notNullOrEmpty(paramTypes)) {
                String tmp = typeName.substring(0, typeName.indexOf('<')) + "<";

                for (int i = 0, len = paramTypes.length; i < len; i++) {
                    if (i > 0) {
                        tmp += ", ";
                    }

                    tmp += getSimpleType(paramTypes[i], null, pkgName, importedClasses);
                }

                tmp += ">";
                typeName = tmp;

                //                for (Type<?> paraType : parameterTypes) {                    
                //                    try {
                //                        if (isUsualType(paraType.getTypeClass().getCanonicalName())) {
                //                            typeName = typeName.replace(ClassUtil.getPackageName(paraType.getTypeClass()) + ".", "");
                //                        }
                //                    } catch (Exception e) {
                //                        // ignore;
                //                    }
                //                }
            }
        }

        if ((ClassUtil.getPackageName(TypeType.class) + "." + TypeType.TYPE).equals(typeName)) {
            typeName = typeName + "<Object>";
        }

        if (N.notNullOrEmpty(pkgName)) {
            if (prop != null && prop.getColumnType().isEntity()) {
                EntityDefinition columnEntityDef = prop.getColumnEntityDef();
                String objectType = pkgName + _PERIOD + columnEntityDef.getName();
                typeName = typeName.replace(objectType, columnEntityDef.getName());
            } else {
                if ((typeName.indexOf(pkgName) == 0) && (typeName.lastIndexOf(_PERIOD) == pkgName.length())) {
                    typeName = typeName.replace(pkgName + _PERIOD, "");
                }
            }
        }
        return typeName;
    }

    /**
     * Method getUsualType.
     *
     * @param entityDef
     * @return Set<String>
     */
    private static Set<Class<?>> getUsualType(final EntityDefinition entityDef) {
        Set<Class<?>> set = new LinkedHashSet<>();

        for (Property prop : entityDef.getPropertyList()) {
            if (prop.getColumnType().isEntity()) {
                //
            } else {
                getUsualType(set, prop.getType());
            }
        }

        return set;
    }

    private static void getUsualType(final Set<Class<?>> set, final Type<?> type) {
        if (isUsualType(type.clazz().getCanonicalName())) {
            set.add(type.clazz());
        }

        if (type.isGenericType()) {
            Type<?>[] parameterTypes = type.getParameterTypes();

            if (N.notNullOrEmpty(parameterTypes)) {
                for (Type<?> e : parameterTypes) {
                    getUsualType(set, e);
                }
            }
        }
    }

    private static boolean isUsualType(String canonicalName) {
        int lastIndex = canonicalName.lastIndexOf('.');
        return lastIndex > 0 && USUAL_TYPES.contains(canonicalName.substring(0, lastIndex));
    }

    /*
     * static String getSimpleType(EntityDefinition entityDef, String pkgName) { String type =
     * entityDef.getJavaType(); type = type.replace(pkgName + _PERIOD + entityDef.getName(), entityDef.getName());
     *
     * if (type.startsWith("java.lang.")) { type = type.replace("java.lang.", ""); }
     *
     * if (type.startsWith("java.util.")) { type = type.replace("java.util.", ""); }
     *
     * return type; }
     */
    private static String getPackageName(final EntityDefinition entityDef) {
        return entityDef.getAttribute(EntityDefEle.PACKAGE);
    }

    private static String getIdParaString(final String pkgName, final List<Property> idPropList, final Map<String, Class<?>> importedClasses) {
        String idPropParaStr = "";

        if (idPropList.size() > 0) {
            int length = 8;
            String headEmpty = "";

            for (int j = 0; j < length; j++) {
                headEmpty += " ";
            }

            int i = 1;

            for (Property idProp : idPropList) {
                String fieldName = propName2FieldName(idProp.getName());
                idPropParaStr += (getSimpleType(null, idProp, pkgName, importedClasses) + " " + fieldName);

                if (i < idPropList.size()) {
                    idPropParaStr += ", ";
                }

                length += idPropParaStr.length();

                if (length > 80) {
                    length = 8 + idPropParaStr.length();
                    idPropParaStr += (IOUtil.LINE_SEPARATOR + headEmpty + idPropParaStr);
                }

                i++;
            }
        }

        return idPropParaStr;
    }

    static void generateCodeForMethodNameConstant(final Class<?> cls) {
        generateCodeForMethodNameConstant(cls, Modifier.PUBLIC);
    }

    static void generateCodeForMethodNameConstant(final Class<?> cls, final int modifier) {
        for (Method method : cls.getDeclaredMethods()) {
            if ((method.getModifiers() & modifier) <= 0) {
                continue;
            }

            String methodName = method.getName();
            String constName = "";
            char ch = 0;

            for (int i = 0, len = methodName.length(); i < len; i++) {
                ch = methodName.charAt(i);

                if (Character.isUpperCase(ch) && (constName.length() > 0) && (constName.charAt(constName.length() - 1) != '_')) {
                    constName += "_";
                }

                constName += Character.toUpperCase(ch);
            }

            N.println("public static final String " + constName + " = " + "\"" + methodName + "\";");
        }
    }

    static void generateCodeForIntEnum(final Class<? extends Enum<?>> cls) {
        N.println("public static " + ClassUtil.getSimpleClassName(cls) + " valueOf(int intValue) {");
        N.println("switch (intValue) {");

        Method method = ClassUtil.getDeclaredMethod(cls, "intValue");

        for (Enum<?> e : cls.getEnumConstants()) {
            N.println("case " + ClassUtil.invokeMethod(e, method) + ": ");
            N.println("return " + e.name() + ";");
        }

        N.println("default:");
        N.println("throw new IllegalArgumentException(\"No mapping instance found by int value: \" + intValue);");
        N.println("}");
        N.println("}");
    }

    /**
     * @author Haiyang Li
     * @version $Revision: 0.8 $
     */
    public enum EntityMode {
        POJO, POJO_WITH_PROP_NAME_TABLE, IMPL_DIRTY_MARKER, EXTEND_DIRTY_MARKER;
    }

    public enum ParentPropertyMode {
        NONE, FIRST, LAST
    }

    private static final String _N_STRING = "/*\r\n" + " * Licensed to the Apache Software Foundation (ASF) under one or more\r\n"
            + " * contributor license agreements.  See the NOTICE file distributed with\r\n"
            + " * this work for additional information regarding copyright ownership.\r\n"
            + " * The ASF licenses this file to You under the Apache License, Version 2.0\r\n"
            + " * (the \"License\"); you may not use this file except in compliance with\r\n" + " * the License.  You may obtain a copy of the License at\r\n"
            + " *\r\n" + " *      http://www.apache.org/licenses/LICENSE-2.0\r\n" + " *\r\n"
            + " * Unless required by applicable law or agreed to in writing, software\r\n"
            + " * distributed under the License is distributed on an \"AS IS\" BASIS,\r\n"
            + " * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.\r\n"
            + " * See the License for the specific language governing permissions and\r\n" + " * limitations under the License.\r\n" + " */\r\n"
            + "package com.landawn.abacus.util;\r\n" + "\r\n" + "import java.util.ArrayList;\r\n" + "import java.util.HashMap;\r\n"
            + "import java.util.HashSet;\r\n" + "import java.util.LinkedHashMap;\r\n" + "import java.util.LinkedHashSet;\r\n"
            + "import java.util.LinkedList;\r\n" + "import java.util.List;\r\n" + "import java.util.Map;\r\n" + "import java.util.Set;\r\n" + "\r\n" + "/**\r\n"
            + " * \r\n" + " * @author Haiyang Li\r\n" + " * \r\n" + " * @since 0.8\r\n" + " */\r\n" + "public final class _N {\r\n" + "\r\n"
            + "    private _N() {\r\n" + "        //singleton\r\n" + "    }\r\n" + "\r\n"
            + "    private static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;\r\n"
            + "    private static final int MAX_HASH_LENGTH = (int) (MAX_ARRAY_SIZE / 1.25) - 1;\r\n"
            + "    private static final String NULL_STRING = \"null\";\r\n" + "\r\n"
            + "    public static boolean equals(final boolean a, final boolean b) {\r\n" + "        return a == b;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final char a, final char b) {\r\n" + "        return a == b;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final byte a, final byte b) {\r\n" + "        return a == b;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final short a, final short b) {\r\n" + "        return a == b;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final int a, final int b) {\r\n" + "        return a == b;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final long a, final long b) {\r\n" + "        return a == b;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final float a, final float b) {\r\n" + "        return Float.compare(a, b) == 0;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final double a, final double b) {\r\n" + "        return Double.compare(a, b) == 0;\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final String a, final String b) {\r\n"
            + "        return (a == null) ? b == null : (b == null ? false : a.length() == b.length() && a.equals(b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final Object a, final Object b) {\r\n"
            + "        return (a == null) ? b == null : (b == null ? false : a.equals(b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final boolean[] a, final boolean[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final boolean[] a, final int fromIndex, final int toIndex, final boolean[] b) {\r\n"
            + "        if (a == b) {\r\n" + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (a[i] != b[i]) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final char[] a, final char[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final char[] a, final int fromIndex, final int toIndex, final char[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (a[i] != b[i]) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final byte[] a, final byte[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final byte[] a, final int fromIndex, final int toIndex, final byte[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (a[i] != b[i]) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final short[] a, final short[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final short[] a, final int fromIndex, final int toIndex, final short[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (a[i] != b[i]) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final int[] a, final int[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final int[] a, final int fromIndex, final int toIndex, final int[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (a[i] != b[i]) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final long[] a, final long[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final long[] a, final int fromIndex, final int toIndex, final long[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (a[i] != b[i]) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final float[] a, final float[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final float[] a, final int fromIndex, final int toIndex, final float[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (Float.compare(a[i], b[i]) != 0) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final double[] a, final double[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final double[] a, final int fromIndex, final int toIndex, final double[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (Double.compare(a[i], b[i]) != 0) {\r\n" + "                return false;\r\n" + "            }\r\n" + "        }\r\n" + "\r\n"
            + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static boolean equals(final Object[] a, final Object[] b) {\r\n"
            + "        return (a == null || b == null) ? a == b : (a.length == b.length && equals(a, 0, a.length, b));\r\n" + "    }\r\n" + "\r\n"
            + "    public static boolean equals(final Object[] a, final int fromIndex, final int toIndex, final Object[] b) {\r\n" + "        if (a == b) {\r\n"
            + "            return true;\r\n" + "        }\r\n" + "\r\n"
            + "        if ((a == null && b != null) || (a != null && b == null) || a.length < toIndex || b.length < toIndex) {\r\n"
            + "            return false;\r\n" + "        }\r\n" + "\r\n" + "        for (int i = fromIndex; i < toIndex; i++) {\r\n"
            + "            if (!(a[i] == null ? b[i] == null : a[i].equals(b[i]))) {\r\n" + "                return false;\r\n" + "            }\r\n"
            + "        }\r\n" + "\r\n" + "        return true;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final boolean value) {\r\n"
            + "        return value ? 1231 : 1237;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final char value) {\r\n"
            + "        return value;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final byte value) {\r\n" + "        return value;\r\n"
            + "    }\r\n" + "\r\n" + "    public static int hashCode(final short value) {\r\n" + "        return value;\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final int value) {\r\n" + "        return value;\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final long value) {\r\n" + "        return (int) (value ^ (value >>> 32));\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final float value) {\r\n" + "        return Float.floatToIntBits(value);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final double value) {\r\n" + "        long bits = Double.doubleToLongBits(value);\r\n" + "\r\n"
            + "        return (int) (bits ^ (bits >>> 32));\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final Object obj) {\r\n"
            + "        if (obj == null) {\r\n" + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        return obj.hashCode();\r\n" + "    }\r\n"
            + "\r\n" + "    public static int hashCode(final boolean[] a) {\r\n" + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n"
            + "\r\n" + "    public static int hashCode(final boolean[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + (a[i] ? 1231 : 1237);\r\n" + "        }\r\n"
            + "\r\n" + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final char[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final char[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + a[i];\r\n" + "        }\r\n" + "\r\n"
            + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final byte[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final byte[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + a[i];\r\n" + "        }\r\n" + "\r\n"
            + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final short[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final short[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + a[i];\r\n" + "        }\r\n" + "\r\n"
            + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final int[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final int[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + a[i];\r\n" + "        }\r\n" + "\r\n"
            + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final long[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final long[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + (int) (a[i] ^ (a[i] >>> 32));\r\n"
            + "        }\r\n" + "\r\n" + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final float[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final float[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + Float.floatToIntBits(a[i]);\r\n" + "        }\r\n"
            + "\r\n" + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static int hashCode(final double[] a) {\r\n"
            + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static int hashCode(final double[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            long bits = Double.doubleToLongBits(a[i]);\r\n"
            + "            result = 31 * result + (int) (bits ^ (bits >>> 32));\r\n" + "        }\r\n" + "\r\n" + "        return result;\r\n" + "    }\r\n"
            + "\r\n" + "    public static int hashCode(final Object[] a) {\r\n" + "        return a == null ? 0 : hashCode(a, 0, a.length);\r\n" + "    }\r\n"
            + "\r\n" + "    public static int hashCode(final Object[] a, final int fromIndex, final int toIndex) {\r\n" + "        if (a == null) {\r\n"
            + "            return 0;\r\n" + "        }\r\n" + "\r\n" + "        int result = 1;\r\n" + "\r\n"
            + "        for (int i = fromIndex; i < toIndex; i++) {\r\n" + "            result = 31 * result + (a[i] == null ? 0 : a[i].hashCode());\r\n"
            + "        }\r\n" + "\r\n" + "        return result;\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final boolean value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final char value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final byte value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final short value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final int value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final long value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final float value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final double value) {\r\n"
            + "        return String.valueOf(value);\r\n" + "    }\r\n" + "\r\n" + "    public static String toString(final Object obj) {\r\n"
            + "        if (obj == null) {\r\n" + "            return NULL_STRING;\r\n" + "        }\r\n" + "\r\n" + "        return obj.toString();\r\n"
            + "    }\r\n" + "\r\n" + "    public static String toString(final boolean[] a) {\r\n" + "        if (a == null) {\r\n"
            + "            return NULL_STRING;\r\n" + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n"
            + "        }\r\n" + "\r\n" + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final boolean[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final boolean[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final boolean[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final char[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final char[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final char[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final char[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final byte[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final byte[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final byte[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final byte[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final short[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final short[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final short[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final short[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final int[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final int[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final int[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final int[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final long[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final long[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final long[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final long[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final float[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final float[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final float[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final float[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final double[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final double[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final double[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final double[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(a[i]);\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n" + "    }\r\n"
            + "\r\n" + "    public static String toString(final Object[] a) {\r\n" + "        if (a == null) {\r\n" + "            return NULL_STRING;\r\n"
            + "        }\r\n" + "\r\n" + "        if (a.length == 0) {\r\n" + "            return \"[]\";\r\n" + "        }\r\n" + "\r\n"
            + "        return toString(a, 0, a.length);\r\n" + "    }\r\n" + "\r\n"
            + "    public static String toString(final Object[] a, final int from, final int to) {\r\n"
            + "        final StringBuilder sb = new StringBuilder();\r\n" + "\r\n" + "        toString(sb, a, from, to);\r\n" + "\r\n"
            + "        return sb.toString();\r\n" + "    }\r\n" + "\r\n" + "    static void toString(final StringBuilder sb, final Object[] a) {\r\n"
            + "        if (a == null) {\r\n" + "            sb.append(NULL_STRING);\r\n" + "        } else if (a.length == 0) {\r\n"
            + "            sb.append(\"[]\");\r\n" + "        } else {\r\n" + "            toString(sb, a, 0, a.length);\r\n" + "        }\r\n" + "    }\r\n"
            + "\r\n" + "    static void toString(final StringBuilder sb, final Object[] a, final int from, final int to) {\r\n" + "        sb.append('[');\r\n"
            + "\r\n" + "        for (int i = from; i < to; i++) {\r\n" + "            if (i > from) {\r\n" + "                sb.append(\", \");\r\n"
            + "            }\r\n" + "\r\n" + "            sb.append(toString(a[i]));\r\n" + "        }\r\n" + "\r\n" + "        sb.append(']');\r\n"
            + "    }\r\n" + "\r\n" + "    @SafeVarargs\r\n" + "    public static <T> List<T> asList(final T... a) {\r\n" + "        if (a == null) {\r\n"
            + "            return new ArrayList<>();\r\n" + "        }\r\n" + "\r\n" + "        final List<T> list = new ArrayList<T>(a.length);\r\n" + "\r\n"
            + "        for (T e : a) {\r\n" + "            list.add(e);\r\n" + "        }\r\n" + "\r\n" + "        return list;\r\n" + "    }\r\n" + "\r\n"
            + "    @SafeVarargs\r\n" + "    public static <T> LinkedList<T> asLinkedList(final T... a) {\r\n" + "        if (a == null) {\r\n"
            + "            return new LinkedList<>();\r\n" + "        }\r\n" + "\r\n" + "        LinkedList<T> list = new LinkedList<T>();\r\n" + "\r\n"
            + "        for (T e : a) {\r\n" + "            list.add(e);\r\n" + "        }\r\n" + "\r\n" + "        return list;\r\n" + "    }\r\n" + "\r\n"
            + "    @SafeVarargs\r\n" + "    public static <T> Set<T> asSet(final T... a) {\r\n" + "        if (a == null) {\r\n"
            + "            return new HashSet<>();\r\n" + "        }\r\n" + "\r\n"
            + "        final Set<T> set = new HashSet<T>(initHashCapacity(a.length));\r\n" + "\r\n" + "        for (T e : a) {\r\n"
            + "            set.add(e);\r\n" + "        }\r\n" + "\r\n" + "        return set;\r\n" + "    }\r\n" + "\r\n" + "    @SafeVarargs\r\n"
            + "    public static <T> LinkedHashSet<T> asLinkedHashSet(final T... a) {\r\n" + "        if (a == null) {\r\n"
            + "            return new LinkedHashSet<>();\r\n" + "        }\r\n" + "\r\n"
            + "        final LinkedHashSet<T> set = new LinkedHashSet<T>(initHashCapacity(a.length));\r\n" + "\r\n" + "        for (T e : a) {\r\n"
            + "            set.add(e);\r\n" + "        }\r\n" + "\r\n" + "        return set;\r\n" + "    }\r\n" + "\r\n"
            + "    @SuppressWarnings(\"unchecked\")\r\n" + "    @SafeVarargs\r\n" + "    public static <K, V> Map<K, V> asMap(final Object... a) {\r\n"
            + "        if (a == null) {\r\n" + "            return new HashMap<>();\r\n" + "        }\r\n" + "\r\n"
            + "        final Map<K, V> m = new HashMap<K, V>(initHashCapacity(a.length / 2));\r\n" + "\r\n" + "        for (int i = 0; i < a.length; i++) {\r\n"
            + "            m.put((K) a[i], (V) a[++i]);\r\n" + "        }\r\n" + "\r\n" + "        return m;\r\n" + "    }\r\n" + "\r\n"
            + "    @SuppressWarnings(\"unchecked\")\r\n" + "    @SafeVarargs\r\n"
            + "    public static <K, V> LinkedHashMap<K, V> asLinkedHashMap(final Object... a) {\r\n" + "        if (a == null) {\r\n"
            + "            return new LinkedHashMap<>();\r\n" + "        }\r\n" + "\r\n"
            + "        final LinkedHashMap<K, V> m = new LinkedHashMap<K, V>(initHashCapacity(a.length / 2));\r\n" + "\r\n"
            + "        for (int i = 0; i < a.length; i++) {\r\n" + "            m.put((K) a[i], (V) a[++i]);\r\n" + "        }\r\n" + "\r\n"
            + "        return m;\r\n" + "    }\r\n" + "\r\n" + "    private static int initHashCapacity(final int size) {\r\n"
            + "        return size < MAX_HASH_LENGTH ? (int) (size * 1.25) + 1 : MAX_ARRAY_SIZE;\r\n" + "    }\r\n" + "}";
}
