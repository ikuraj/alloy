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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.io.Writer;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import ext.nanoxml.XMLParseException;

import kodviz.xml.PaletteXMLParseException;
import kodviz.xml.PaletteXMLParser;


/**
 * A ViewPalette is simply a collection of views for instances of a
 * model. Thus, with every model, there is an associated view palette. 
 * A ViewPalette is serializable.
 */
public class ViewPalette implements Serializable {

	private static final long serialVersionUID = 1L;

	/*
     * A collection of Views. Each view must have a name.
     */
    public List views;
    private View currentView;

    /*
     * The name of the palette. By default, the name of the palette
     * should correspond to the naem of the model it is associated
     * with.  
     */
    private String name;

    /**
     * Creates an empty view palette with the specified name.
     */
    public ViewPalette(String name) {
        views = new LinkedList();
        currentView = null;
        this.name = name;
    }

    @SuppressWarnings("unchecked")
    public ViewPalette copy() {
        ViewPalette newPal = new ViewPalette(this.name);
        for (Iterator v = this.views.iterator(); v.hasNext();) {
            View curView = (View) v.next();
            View newView = curView.copy();
            newPal.views.add(newView);
            if (curView == this.currentView) {
            	newPal.currentView = newView;
            }            
        }
        return newPal;
    }

    /**
     * Adds a view to this palette. In the case that there is a
     * naming conflict, the name of the view to be added is appended
     * with an integer suffix to differentiate. Note that as a result,
     * the name of the View itself will be changed.
     */
    public void addView(View view) {
        //System.out.println(view);
        addViewAux(view);
        currentView = view;
    }

    /**
     * sets current view
     */
    public void setCurrentView(View view) {
        if (!views.contains(view)) {
            throw new IllegalArgumentException("view doesn't exist");
        }
        else {
            currentView = view;
        }
    }

    @SuppressWarnings("unchecked")
    private void addViewAux(View view) {
        // this is a little convoluted
        // basically, we append an integer index
        // to the view name so there is no name 
        // conflict.
        boolean changedName = false;
        String viewName = view.getName();
        int indexOfBracket = viewName.indexOf("<");
        String origName;
        if (indexOfBracket == -1) {
            origName = viewName;
        }
        else {
            origName = viewName.substring(0, indexOfBracket);
        }
        int origLength = origName.length();
        StringBuffer newName = new StringBuffer(origName);
        for (int i = 1; i < Integer.MAX_VALUE; i++) {
            if (containsView(newName.toString())) {
                if (changedName) {
                    newName.delete(origLength, newName.length());
                    newName = newName.append("<" + i + ">");
                }
                else {
                    newName = newName.append("<" + i + ">");
                    changedName = true;
                }
            }
            else {
                break;
            }
        }
        view.setName(newName.toString());
        views.add(view);
    }

    /**
     * Appends the views in the specified palette to the views in this
     * palette. If there is a naming conflict, the next view to be
     * added appended with an integer index.  
     */
    public void incorporate(ViewPalette palette) {
        List viewsToInc = palette.getAllViews();
        for (Iterator i = viewsToInc.iterator(); i.hasNext();) {
            View v = (View) i.next();
            addViewAux(v);
        }
    }

    /**
     * gets the current view (if currentView is null, something could
     * be sketchy, and in that case, the first element is always returned.
     * if there's no view in the palette null is returned.
     */
    public View getCurrentView() {
        if (views.size() < 1) {
            return null;
        }
        else {
            if (currentView == null) {
                return (View) views.get(0);
            }
            else {
                return currentView;
            }
        }
    }

    /**
     * Returns true if the specified view name is in this view palette.
     */
    public boolean containsView(String viewName) {
        //System.out.println(viewName);
        for (Iterator i = views.iterator(); i.hasNext();) {
            View v = (View) i.next();
            String vname = v.getName();
            if (vname.equals(viewName)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Deletes the specified view from this palette. If view does not
     * exist in this palette, method does nothing.
     */
    public void removeView(View view) {
        if (currentView.equals(view)) {
            currentView = null;
        }
        views.remove(view);
    }

    /**
     * Returns an unmodifiable list of the views in this palette.
     * The views are sorted in alphabetical order based on their 
     * name.
     */
    @SuppressWarnings("unchecked")
    public List getAllViews() {
        Collections.sort(views, new Comparator() {
            public int compare(Object o1, Object o2) {
                View view1 = (View) o1;
                View view2 = (View) o2;
                return view1.getName().compareTo(view2.getName());
            }
        });
        return Collections.unmodifiableList(views);
    }

    // jbaek inserted code
    /**
     * Loads a file, returns null if not successful TODO
     */
    public void saveXML(File f, VizState _viz) throws IOException {
    	
    	if (f == null) throw new NullPointerException();
    	Writer output = null;
    	PaletteXMLParser p = new PaletteXMLParser(_viz, null);
    	try {
    		String oldXML = PaletteXMLParser.fileToString(f);
            String outXML = p.writeXML(this, name);
            output = new BufferedWriter( new FileWriter(f) );
            if (false || oldXML == null) {
            	//System.out.println("Saving as Alloy3");
            	output.write(outXML); // alloy3
            } else {
            	//System.out.println("Saving as Alloy4");
            	output.write(PaletteXMLParser.reconcilePalettes_Alloy4(
            		outXML, oldXML)); // alloy4
            }
        } catch (PaletteXMLParseException e) {
        	e.printStackTrace(); // jbaek
        } catch (IOException e) {
        	throw e;
        } finally {
        	if (output != null) output.close();
        }
    }
    
    /**
     * Saves this palette using the default serialization method to 
     * the specified file.
     */
    public void save(File f) throws IOException {
        if (f == null) {
            throw new NullPointerException();
        }

        try {
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(f));
            out.writeObject(new VizVersion(VizVersion.CURRENT_VERSION, VizVersion.CURRENT_REVISION));
            out.writeObject(this);
        }
        catch (IOException ioe) {
            throw ioe;
        }
    }

    // jbaek inserted code
    /**
     * Loads a file
     */
    public static ViewPalette loadXML(File f, VizState _viz) throws IOException,
    PaletteXMLParseException, IncompatiblePaletteException {
    	try {
    		PaletteXMLParser x = new PaletteXMLParser(_viz, _viz.getView()); // no default view
	    	return x.readXML(f);
    	} catch (XMLParseException e) {
    		throw e;
    	} catch (IOException e) {
    		throw e;
    	} catch (PaletteXMLParseException e) {
    		throw e;
    	}
    }
    
    /**
     * Loads from the given file a ViewPalette object.
     */
    public static ViewPalette load(File f) throws IOException, ClassNotFoundException, IncompatiblePaletteException {    	
        if (f == null) {
            throw new NullPointerException();
        }
        try {
            ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
            VizVersion vv = (VizVersion) in.readObject();
            if (VizVersion.isCompatible(vv)) {
                ViewPalette vp = (ViewPalette) in.readObject();
                return vp;
            }
            else {
                throw new IncompatiblePaletteException("Palette versions not compatible.");
            }
        }
        catch (ClassNotFoundException cnfe) {
            throw cnfe;
        }
        catch (IOException ioe) {
            throw ioe;
        }
    }

    /**
     * Returns a string representation of this palette.
     */
    public String toString() {
        StringBuffer s = new StringBuffer();
        s.append("ViewPalette " + name + ":\n");
        for (Iterator i = views.iterator(); i.hasNext();) {
            View nextView = (View) i.next();
            s.append(nextView.toString() + "\n");
        }
        return s.toString();
    }
}
