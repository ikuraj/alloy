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
import java.util.HashMap;
import java.util.Map;

/**
 * A generator of unique names.  Given a prefix, generates a name starting with the prefix
 * and ending with a unique numeric suffix.  The suffix is only appended if necessary to maintain
 * uniqueness.  Uniqueness is guaranteed only against other names in the same Namer object.
 */
@SuppressWarnings("unchecked")
public class Namer {
    /**
     * For each base name, the maximum suffix generated so far.
     * Map from String to Integer.
     */
    private Map /* String -> Integer */ baseCount = new HashMap();

    /**
     * Add a name to avoid; {@link #getUniqueName} will never return this exact name.
     */
    public void addNameToAvoid(String name_) {
	getUniqueName(name_);  /* this records name_ as a name to avoid. */
    }

    /**
     * Returns an array ret such that ret[0] + ret[1] = name AND ret[1] = "" or ret[1] is
     * a string representation of an integer with no leading zeros AND the last character of 
     * ret[0] is not in the range from 1 to 9.
     **/
    private static String[] split(String name) {
	StringBuffer numericSuffix = new StringBuffer();

	// find the numeric suffix of name, including leading zeros
	int i ;
	for (i = name.length()-1; i >= 0; i--) {
	    char char_i = name.charAt(i);
	    if (char_i >= '\u0030' && char_i <= '\u0039')  // the ith character is a digit 
		numericSuffix.insert(0,char_i);
	    else 
		break;
	}
	// eliminate leading zeros from the numeric suffix
	for (i = i+1; i < name.length(); i++) {
	    char char_i = name.charAt(i);
	    if (char_i == '\u0030')  // the ith character is a leading zero 
		numericSuffix.deleteCharAt(0);
	    else 
		break;
	}

        String[] ret = new String[2];
	ret[0] = name.substring(0,i);
	ret[1] = numericSuffix.toString();

	return ret;
    }

    /**
     * Return a unique name with the given prefix.  Uniqueness is guaranteed
     * only against other names returned by this routine for the same Namer object.
     */
    public String getUniqueName(String base_) {
	/** 
	 * Note by Emina:  this actually doesn't guarantee uniqueness.  Consider
	 * the case when one variable is named h and the other h1.  If a name
	 * with base h is requested twice, it will conflict with base h1.  So, 
	 * we use the following algorithm to resolve naming conflicts.
	 * This was discovered when trying to fix the SkolemizationCrash2 bug. **/
	
	String[] baseSplit = split(base_);
	
	String basePrefix = baseSplit[0];
	int baseSuffix = (baseSplit[1].length() == 0 ? 0 : Integer.parseInt(baseSplit[1]));
	Dbg.chk(baseSuffix>0 || (baseSuffix==0 && baseSplit[1].length()==0));

	if (baseCount.containsKey(base_)) {
	    int uniqueSuffix = ((Integer)baseCount.get(base_)).intValue();
	    baseCount.put(base_, new Integer(uniqueSuffix + 1));
	    return base_ + "_" + uniqueSuffix;
	} else {
	    if ((basePrefix.charAt(basePrefix.length()-1)=='_') &&
		(baseCount.containsKey(basePrefix.substring(0, basePrefix.length())))) {
		baseCount.put(base_, new Integer(2));
		return base_ + "_1";
	    } 
	    baseCount.put(base_, new Integer(1));
	    return base_;
	}


	/**
	String[] baseSplit = split(base_);
	
	String basePrefix = baseSplit[0];
	int baseSuffix = (baseSplit[1].length() == 0 ? 0 : Integer.parseInt(baseSplit[1]));
	Dbg.chk(baseSuffix>0 || (baseSuffix==0 && baseSplit[1].length()==0));

	//System.out.println("base: " + base_ + " bPrefix " + basePrefix + " bSuffix " + baseSuffix); 
	if (!baseCount.containsKey(basePrefix)) { // we haven't seen this prefix before ...
	    baseCount.put(basePrefix, new Integer(baseSuffix + 1));
	    //System.out.println("generated " + base_);
	    return base_;
	} else { // we have seen this prefix before
	    int storedSuffix = ((Integer)baseCount.get(basePrefix)).intValue();
	    int currentSuffix = (storedSuffix > baseSuffix ? storedSuffix : baseSuffix);
	    baseCount.put(basePrefix, new Integer(currentSuffix + 1));
	    //System.out.println("generated " + basePrefix + currentSuffix); 
	    return basePrefix + currentSuffix;

	}
	**/
	/**
	Integer suffix = (Integer)baseCount.get(base_);
	if (suffix == null) {
	    baseCount.put(base_, new Integer(1));
	    return base_;
	} else {
	    baseCount.put(base_, new Integer(suffix.intValue() + 1));
	    return base_ + "@" + suffix;
	}
	**/
    }
}
