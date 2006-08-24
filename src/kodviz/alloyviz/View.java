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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;

public class View implements Serializable {

	private static final long serialVersionUID = 1L;
	private GeneralView genView;
    private ModelView modView;
    private String name;

    // CONSTRUCTOR

    public View(String name) {
        genView = new GeneralView();
        modView = new ModelView();
        this.name = name;
    }

    public View copy() {
        View newView = new View(this.getName());
        newView.genView = this.genView.copy();
        newView.modView = this.modView.copy();
        return newView;
    }

    //
    // MUTATORS
    //

    /**
     * Sets the name of the view to specified name.
     */
    public void setName(String name) {
        this.name = name;
    }

    // just call the methods on the returned genView and modView

    //
    // ACCESSORS
    //

    /**
     * Returns the GeneralView.  The returned object is modifiable.
     */
    public GeneralView getGeneralView() {
        return genView;
    }

    /**
     * Returns the ModelView.  The returned object is modifiable.
     */
    public ModelView getModelView() {
        return modView;
    }

    /**
     * Returns the name of this view.
     */
    public String getName() {
        return name;
    }

    /**
     * Saves this view object using the default Java serialization
     * mechanism.
     *
     * @param f the File to save to
     */
    public void save(File f) throws IOException {

        if (f == null) {
            throw new NullPointerException();
        }

        try {
            ObjectOutputStream out = new ObjectOutputStream(new FileOutputStream(f));
            out.writeObject(this);
        }
        catch (IOException ioe) {
            throw ioe;
        }

    }

    /**
     * Loads from given file a View object.
     *
     * @param f the File to load from
     */
    public static View load(File f) throws IOException, ClassNotFoundException {
        if (f == null) {
            throw new NullPointerException();
        }

        try {
            ObjectInputStream in = new ObjectInputStream(new FileInputStream(f));
            View temp = (View)in.readObject();
            return temp;
        }
        catch (ClassNotFoundException cnfe) {
            throw cnfe;
        }
        catch (IOException ioe) {
            throw ioe;
        }
    }

    /**
     * Returns a String representation of this View.
     */
    public String toString() {
        return "View:\n" + name + "\n" + genView.toString() + "\n" + modView.toString();
    }
}
