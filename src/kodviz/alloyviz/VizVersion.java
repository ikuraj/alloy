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
package kodviz.alloyviz;

import java.io.Serializable;

@SuppressWarnings("serial")
public class VizVersion implements Serializable {
	
	/*
	 * changes log:
	 * 1.1 - showInAttr, hideUnconnected for nodes
	 * 1.2 - removed hideUnconnected from general view (wasn't used before), 
	 *       number atoms options
	 */
	public static int CURRENT_VERSION=1;
	public static int CURRENT_REVISION=2;
	
	private int _version, _revision;
	
	public VizVersion(int version, int revision) {
		_version = version;
		_revision = revision;		
	}
	
	public static boolean isCompatible(VizVersion v) {
		return (v._version==CURRENT_VERSION && v._revision==CURRENT_REVISION);			
	}
	
	public int getVersion() {
		return _version;
	}
	
	public int getRevision() {
		return _revision;	
	}
	
	 
}
