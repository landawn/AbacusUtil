package com.landawn.abacus.dataSource;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.w3c.dom.Element;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.util.Configuration;
import com.landawn.abacus.util.N;

public final class DataSourceManagerConfiguration extends Configuration {
    /**
     * Field DATA_SOURCE_MANAGER. (value is ""dataSourceManager"")
     */
    public static final String DATA_SOURCE_MANAGER = "dataSourceManager";

    /**
     * Field DATA_SOURCE_SELECTOR. (value is ""dataSourceSelector"")
     */
    public static final String DATA_SOURCE_SELECTOR = "dataSourceSelector";

    /**
     * Field LIVE_ENV. (value is ""liveEnv"")
     */
    public static final String LIVE_ENV = "liveEnv";

    private final String liveEnv;
    private List<DataSourceConfiguration> dataSourceConfigurationList;

    public DataSourceManagerConfiguration(Element element, Map<String, String> properties) {
        super(element, properties);

        liveEnv = this.getAttribute(LIVE_ENV);

        if (N.isNullOrEmpty(liveEnv)) {
            throw new AbacusException("must set the 'liveEnv' attribute in 'dataSourceManager' element. for example: <dataSourceManager liveEnv=\"dev\"> ");
        }
    }

    public String getLiveEnv() {
        return liveEnv;
    }

    public List<DataSourceConfiguration> getDataSourceConfigurationList() {
        return dataSourceConfigurationList;
    }

    @Override
    protected void init() {
        dataSourceConfigurationList = new ArrayList<>();
    }

    @Override
    protected void complexElement2Attr(Element element) {
        String eleName = element.getNodeName();

        if (DataSourceConfiguration.DATA_SOURCE.equals(eleName)) {
            dataSourceConfigurationList.add(new DataSourceConfiguration(element, this.props));
        } else {
            throw new AbacusException("Unknown element: " + eleName);
        }
    }
}