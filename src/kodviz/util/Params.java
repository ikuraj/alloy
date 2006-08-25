/*
 * Alloy Analyzer
 * Copyright (c) 2002 Massachusetts Institute of Technology
 *
 * The Alloy Analyzer is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * The Alloy Analyzer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the Alloy Analyzer; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package kodviz.util;

import java.awt.Font;
import java.awt.FontFormatException;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Vector;

/**
 * Stores information about parameters.
 *
 * @see ParamReader
 * @see Parameter
 */
@SuppressWarnings("unchecked")
public class Params {

    /**
     * Global parameters; loaded at startup time as a resource, and each
     * part of the application reads the parameters relevant to it.
     * Somewhat similar to Java's {@link Properties}, except parameters
     * have structure that allows us to generate GUI editors for them automatically.
     */
    public static Params glob;

    static {

    try { glob = load(ResourceManager.getResourceAsStream("alloy.cfg")); }
    catch (IOException x) {
    	Dbg.fatal("Could not load config file", x);
    } catch (ParamFormatException x) {
        Dbg.fatal("Could not load config file", x);
    }

    }

    /**
     * {@link Hashtable} from {@link String} name of parameter group to
     * a {@link Vector} of {@link Parameter}s in that group.
     */
    private Hashtable _grp2params;

    /**
     * {@link Hashtable} from {@link String} name of parameter group
     * to {@link String} description of that group.
     */
    private Hashtable _grp2descr;

    /**
     * {@link Map} from {@link String} name of parameter group to
     * {@link Map} from {@link String} name of parameter in that group to
     * {@link Parameter} object.
     */
    private Map _grp2paramName2param;

    /**
     * {@link List} of {@link String} group names, in the order in which
     * they were specified in the parameter file.
     */
    private List _grpNameList;

    public static Params load(InputStream is) throws IOException, ParamFormatException { return ParamReader.loadParams(is); }

    public void save(OutputStream out_) { ParamReader.saveParams(out_, this); }

    /** 
     * <b>local preferences</b> are defined in the user's local alloy.cfg file <br>
     * <b>global preferences</b> are defined in the alloy.cfg file from the distribution <br>  
     * A <b>conflict</b> happens when local and global preferences are defined over different
     * parameters or parameter values.  A conflict is resolved as follows:
     * <br> if local preferences do not have a parameter defined in global preferences,
     *      the parameter from global preferences is added to local preferences 
     * <br> if the types of a parameter defined both in global and local preferences clash,
     *      the local parameter is replaced by the global parameter
     * <br> if the types of a parameter defined both in global and local preferences are the
     *      same but the values clash, the value of the local parameter is used
     */
    public boolean copyOnto(Params p) {
	boolean conflict = false;

	Iterator grpIt = p._grpNameList.iterator();
	while (grpIt.hasNext()) {
	    String grpName = (String)grpIt.next();
	    if (_grpNameList.contains(grpName)) {
		Vector yourGrpParams = p.getGroupParams(grpName);
		Map myParamName2param = (Map)_grp2paramName2param.get(grpName);
		Iterator yourParamIter = yourGrpParams.iterator();
		while (yourParamIter.hasNext()) {
		    Parameter yourParam = (Parameter)yourParamIter.next();
		    Parameter myParam = (Parameter)myParamName2param.get(yourParam.getName());
		    if (myParam == null || myParam.getType() != yourParam.getType() ||
			(myParam.getType() == Parameter.ENUM && 
			 !yourParam.getEnum().values.contains(myParam.getValue()))) {
			conflict = true;
		    } else {
			yourParam.setValue(myParam.getValue());
		    }
		}
	    } else {
		conflict = true;
	    }
	}
	return !conflict;
    }

    Params(Hashtable grp2params_, Hashtable grp2descr_, List grpNameList_) {
        _grp2params = grp2params_;
        _grp2descr = grp2descr_;
        _grpNameList = grpNameList_;

        _grp2paramName2param = new HashMap();
	//_grp2listeners = new HashMap();
        for (Iterator grpNameIter = grp2params_.keySet().iterator(); grpNameIter.hasNext();) {
            String grpName = (String)grpNameIter.next();
            Vector grpParams = getGroupParams(grpName);
            Map paramName2param = new HashMap();
            _grp2paramName2param.put(grpName, paramName2param);
            for (Iterator paramIter = grpParams.iterator(); paramIter.hasNext();) {
                Parameter param = (Parameter)paramIter.next();
                paramName2param.put(param.getName(), param);
            }
	    //List listeners = new ArrayList();
	    //_grp2listeners.put(grpName, listeners);
        }
    }

    public String getGroupDescr(String grpName_) { return (String)_grp2descr.get(grpName_); }

    public Vector /* of {@link Parameter} */ getGroupParams(String grpName_) {
        return (Vector)_grp2params.get(grpName_);
    }

    public List getGroupNames() { return _grpNameList; }

    public String getParam(String grpName_, String paramName_) {
        Map paramName2param = (Map)_grp2paramName2param.get(grpName_);
        Dbg.chk(paramName2param, "Unknown parameter group: " + grpName_);
        Parameter param = (Parameter)paramName2param.get(paramName_);
        Dbg.chk(param, "Unknown parameter: " + paramName_);
        return param.getValue();
    }

    public String getPathParam(String grpName_, String paramName_) {
        String resultStr = null;
        resultStr = getParam(grpName_, paramName_);
        Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);
        return resultStr.replace(':', File.pathSeparatorChar).replace(';', File.pathSeparatorChar);
    }

    public void setParam(String grpName_, String paramName_, String value_) {
        Map paramName2param = (Map)_grp2paramName2param.get(grpName_);
        Dbg.chk(paramName2param, "Unknown parameter group: " + grpName_);
	Parameter param = (Parameter)paramName2param.get(paramName_);
	//String oldVal = param.getValue();
	param.setValue(value_);
	//fireParamChanged(grpName_, paramName_, oldVal, value_);
    }

    public void setParam(String grpName_, String paramName_, boolean value_) {
        setParam(grpName_, paramName_, value_ ? "1" : "0");
    }

    public void setParam(String grpName_, String paramName_, int value_) {
        setParam(grpName_, paramName_, String.valueOf(value_));
    }

    public void setParam(String grpName_, String paramName_, long value_) {
        setParam(grpName_, paramName_, String.valueOf(value_));
    }

    public void setParam(String grpName_, String paramName_, float value_) {
        setParam(grpName_, paramName_, String.valueOf(value_));
    }

    public void setParam(String grpName_, String paramName_, double value_) {
        setParam(grpName_, paramName_, String.valueOf(value_));
    }

    public boolean getBoolParam(String grpName_, String paramName_) {
        String resultStr = null;
        try {
            resultStr = getParam(grpName_, paramName_);
            Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);
            resultStr = resultStr.trim();
            return resultStr.equals("1") || resultStr.equals("true");
        } catch (NumberFormatException nfe_) {
            Dbg.fail("Non-boolean value " + resultStr + " for parameter " + paramName_ + " of group " + grpName_);
            return false;
        }
    }

    public int getIntParam(String grpName_, String paramName_) {
        String resultStr = null;
        try {
            resultStr = getParam(grpName_, paramName_);
            Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);
            return Integer.parseInt(resultStr);
        } catch (NumberFormatException nfe_) {
            Dbg.fail("Non-integer value " + resultStr + " for parameter " + paramName_ + " of group " + grpName_);
            return 0;
        }
    }

    public long getLongParam(String grpName_, String paramName_) {
        String resultStr = null;
        try {
            resultStr = getParam(grpName_, paramName_);
            Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);
            return Long.parseLong(resultStr);
        } catch (NumberFormatException nfe_) {
            Dbg.fail("Non-long value " + resultStr + " for parameter " + paramName_ + " of group " + grpName_);
            return 0;
        }
    }

    public float getFloatParam(String grpName_, String paramName_) {
    String resultStr = null;
    try {
        resultStr = getParam(grpName_, paramName_);
        Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);
        return Float.parseFloat(resultStr);
    } catch (NumberFormatException nfe_) {
        Dbg.fail("Non-float value " + resultStr + " for parameter " + paramName_ + " of group " + grpName_);
        return (float)0.0;
    }
    }

    public double getDoubleParam(String grpName_, String paramName_) {
    String resultStr = null;
    try {
        resultStr = getParam(grpName_, paramName_);
        Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);
        return Double.parseDouble(resultStr);
    } catch (NumberFormatException nfe_) {
        Dbg.fail("Non-double value " + resultStr + " for parameter " + paramName_ + " of group " + grpName_);
        return 0.0;
    }
    }

    public Font getFontParam(String grpName_, String paramName_) {
    String resultStr = null;
    try {
        resultStr = getParam(grpName_, paramName_);
        Dbg.chk(resultStr, "Unknown parameter " + paramName_ + " in group " + grpName_);

        return Parameter.stringToFont(resultStr);
    } catch (FontFormatException ffe_) {
        Dbg.fail("Invalid font value " + resultStr + " for parameter " + paramName_ + " of group " + grpName_);
        return null;
    }
    }

    /**
     * Check whether the given parameter in the given group has been defined.
     * Note that calling variations of {@link #getParam} (ones that do not provide
     * a default value) on an undefined parameter will cause a fatal error.
     */
    public boolean defined(String grpName_, String paramName_) {
    Map paramName2param = (Map)_grp2paramName2param.get(grpName_);
    if (paramName2param==null) return false;
        Parameter param = (Parameter)paramName2param.get(paramName_);
        return param!=null;
    }

    public String getParam(String grpName_, String paramName_, String default_) {
    return defined(grpName_, paramName_) ? getParam(grpName_, paramName_) : default_;
    }

    public boolean getBoolParam(String grpName_, String paramName_, boolean default_) {
    return defined(grpName_, paramName_) ? getBoolParam(grpName_, paramName_) : default_;
    }

    public int getIntParam(String grpName_, String paramName_, int default_) {
    return defined(grpName_, paramName_) ? getIntParam(grpName_, paramName_) : default_;
    }

    public long getLongParam(String grpName_, String paramName_, long default_) {
    return defined(grpName_, paramName_) ? getLongParam(grpName_, paramName_) : default_;
    }

    public float getFloatParam(String grpName_, String paramName_, float default_) {
    return defined(grpName_, paramName_) ? getFloatParam(grpName_, paramName_) : default_;
    }

    public double getDoubleParam(String grpName_, String paramName_, double default_) {
    return defined(grpName_, paramName_) ? getDoubleParam(grpName_, paramName_) : default_;
    }

    public Font getFontParam(String grpName_, String paramName_, Font default_) {
    return defined(grpName_, paramName_) ? getFontParam(grpName_, paramName_) : default_;
    }

    public String toString() {
    ByteArrayOutputStream paramStr = new ByteArrayOutputStream();
    save(paramStr);
    try { paramStr.close(); } catch (IOException ioe_) { }
    return paramStr.toString();
    }

    public void addParameterListener(String grpName_, String paramName_, ParameterListener l) {
        Map paramName2param = (Map)_grp2paramName2param.get(grpName_);
        Dbg.chk(paramName2param, "Unknown parameter group: " + grpName_);
        Parameter param = (Parameter)paramName2param.get(paramName_);
        Dbg.chk(param, "Unknown parameter: " + paramName_);
	param.addParameterListener(l);
    }

    public void removeParameterListener(String grpName_, String paramName_, ParameterListener l) {
        Map paramName2param = (Map)_grp2paramName2param.get(grpName_);
        Dbg.chk(paramName2param, "Unknown parameter group: " + grpName_);
        Parameter param = (Parameter)paramName2param.get(paramName_);
        Dbg.chk(param, "Unknown parameter: " + paramName_);
	param.removeParameterListener(l);
    }
}
