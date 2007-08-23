/*
 * Alloy Analyzer
 * Copyright (c) 2007 Massachusetts Institute of Technology
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA,
 * 02110-1301, USA
 */

package edu.mit.csail.sdg.alloy4compiler.parser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import edu.mit.csail.sdg.alloy4.A4Reporter;
import edu.mit.csail.sdg.alloy4.ConstList;
import edu.mit.csail.sdg.alloy4.Err;
import edu.mit.csail.sdg.alloy4.ErrorFatal;
import edu.mit.csail.sdg.alloy4.ErrorSyntax;
import edu.mit.csail.sdg.alloy4.Pos;
import edu.mit.csail.sdg.alloy4.Util;
import edu.mit.csail.sdg.alloy4compiler.ast.Command;
import edu.mit.csail.sdg.alloy4compiler.ast.Expr;
import edu.mit.csail.sdg.alloy4compiler.parser.Module.Open;

/** This class provides convenience methods for calling the parser and the compiler. */

public final class CompUtil {

    /** Constructor is private, since this class never needs to be instantiated. */
    private CompUtil() { }

    //=============================================================================================================//

    /**
     * Go up the directory hierachy 0 or more times.
     * <br> For example, on a UNIX machine, goUp("/home/abc/def",1) will return "/home/abc"
     * <br> For example, on a UNIX machine, goUp("/home/abc/def",2) will return "/home"
     * @param filepath - this must be an absolute path
     * @param numberOfSteps - the number of steps to go up
     */
    private static String up(String filepath, int numberOfSteps) {
        while(numberOfSteps > 0) {
            numberOfSteps--;
            int i=filepath.lastIndexOf(File.separatorChar);
            if (i<=0) return "";
            filepath=filepath.substring(0,i);
        }
        return filepath;
    }

    //=============================================================================================================//

    /**
     * Given the name of a module, and the filename for that module, compute the filename for another module
     * @param moduleA - must be a legal Alloy modulepath (eg. name) (eg. name/name/name) (must not start or end in '/')
     * @param fileA - the filename corresponding to moduleA
     * @param moduleB - must be a legal Alloy modulepath (eg. name) (eg. name/name/name) (must not start or end in '/')
     * @return the filename corresponding to moduleB
     */
    private static String computeModulePath(String moduleA, String fileA, String moduleB) {
        fileA=Util.canon(fileA); // Make sure it's a canonical absolute path
        if (moduleA.length()==0) moduleA="anything"; // Harmonizes the boundary case
        while(moduleA.length()>0 && moduleB.length()>0) {
            int a=moduleA.indexOf('/'), b=moduleB.indexOf('/');
            String headOfA = (a>=0) ? moduleA.substring(0,a) : moduleA;
            String headOfB = (b>=0) ? moduleB.substring(0,b) : moduleB;
            if (!headOfA.equals(headOfB) || a<0 || b<0) {
                // eg. util/boolean==/home/models/util/boolean.als, then test=>/home/models/test.als"
                // eg. util/boolean==/home/models/util/boolean.als, then sub/test=>/home/models/sub/test.als
                // eg. main==/home/models/main.als, then test=>/home/models/test.als
                // eg. main==/home/models/main.als, then sub/test=>/home/models/sub/test.als"
                int numberOfSlash=0;
                for(int i=0; i<moduleA.length(); i++)  if (moduleA.charAt(i)=='/') numberOfSlash++;
                return up(fileA, numberOfSlash+1)+File.separatorChar+moduleB.replace('/',File.separatorChar)+".als";
            }
            moduleA=moduleA.substring(a+1);
            moduleB=moduleB.substring(b+1);
        }
        return ""; // This shouldn't happen, since there should always be some character after '/' in the module name
    }

    //=============================================================================================================//

    /**
     * Helper method that recursively parse a file and all its included subfiles
     * @param loaded - this stores the text files we've loaded while parsing; cannot be null
     * @param fc - if a file cannot be found, we consult this cache first before attempting to load it from disk/jar; cannot be null
     * @param pos - the position of the "open" statement
     * @param filename - the filename to open
     * @param root - the root module (this field is ignored if prefix=="")
     * @param parentFileName -the "exact filename" of the parent module
     * @param prefix - the prefix for the file we are about to parse
     * @param thispath - the set of filenames involved in the current chain_of_file_opening
     */
    private static Module parseRecursively
    (Map<String,String> loaded, Map<String,String> fc, Pos pos, String filename, Module root, String prefix, Set<String> thispath)
    throws Err, FileNotFoundException, IOException {
        // Add the filename into a ArrayList, so that we can detect cycles in the module import graph
        // How? I'll argue that (filename appears > 1 time along a chain) <=> (infinite loop in the import graph)
        // => As you descend down the chain via OPEN, if you see the same FILE twice, then
        //    you will go into an infinite loop (since, regardless of the instantiating parameter,
        //    that file will attempt to OPEN the exact same set of files. leading back to itself, etc. etc.)
        // <= If there is an infinite loop, that means there is at least 1 infinite chain of OPEN (from root).
        //    Since the number of files is finite, at least 1 filename will be repeated.
        if (thispath.contains(filename))
           throw new ErrorSyntax(pos,
           "Circular dependency in module import. The file \""+(new File(filename)).getName()+"\" is imported infinitely often.");
        thispath.add(filename);
        // No cycle detected so far. So now we parse the file.
        if (prefix.length()==0) root=null;
        Module u = CompParser.alloy_parseStream(false, loaded, fc, root, 0, filename, prefix);
        if (prefix.length()==0) root=u;
        // Here, we recursively open the included files
        for(Open x: u.getOpens()) {
            String cp=Util.canon(computeModulePath(u.getModelName(), filename, x.filename)), content=fc.get(cp);
            try {
                if (content==null) content=loaded.get(cp);
                if (content==null) content=Util.readAll(cp);
            } catch(IOException ex1) {
                try {
                    content=Util.readAll(true, "models/"+x.filename+".als");
                    cp=x.filename;
                } catch(IOException ex) {
                    throw new ErrorSyntax(pos,
                    "This module cannot be found.\nIt is not a built-in library module, and it cannot be found at \""+cp+"\".\n");
                }
            }
            loaded.put(cp, content);
            Module y = parseRecursively(loaded, fc, x.pos, cp, root, (prefix.length()==0 ? x.alias : prefix+"/"+x.alias), thispath);
            x.connect(y);
        }
        thispath.remove(filename); // Remove this file from the CYCLE DETECTION LIST.
        return u;
    }

    //=============================================================================================================//

    /**
     * Parses 1 module from the input string (without loading any subfiles)
     * @return an array of 0 or more Command if no error occurred
     */
    public static ConstList<Command> parseOneModule_fromString(String content) throws Err {
        try {
            Map<String,String> fc=new LinkedHashMap<String,String>();
            fc.put("", content);
            Module u=CompParser.alloy_parseStream(false, null, fc, null, 0, "", "");
            return ConstList.make(u.getAllCommands());
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage(), ex);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    //=============================================================================================================//

    /**
     * Parses 1 module from the file (without loading any subfiles)
     * @return an array of 0 or more Command if no error occurred
     */
    public static ConstList<Command> parseOneModule_fromFile(String filename) throws Err {
        try {
            Module u=CompParser.alloy_parseStream(false, null, null, null, 0, filename, "");
            return ConstList.make(u.getAllCommands());
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage(), ex);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    //=============================================================================================================//

    /**
     * Parses then typecheck the given input String as an Alloy expression from that world
     * @return the fully-typechecked expression if no error occurred
     * @throws Err if world==null or if any other error occurred
     */
    public static Expr parseOneExpression_fromString (Module world, String input) throws Err {
        try {
            if (world==null) throw new ErrorFatal("Cannot parse an expression with null world.");
            return world.parseOneExpressionFromString(input);
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage(), ex);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }

    //=============================================================================================================//

    /**
     * Read everything from "file" and parse it; if it mentions submodules, open them and parse them too.
     * @param loaded - a cache of files that have been pre-fetched (can be null if there were no prefetching)
     * @param filename - the main module we are parsing
     * @return the root Module which contains pointers to all submodules
     * @throws Err if an error occurred
     * <p> And if loaded!=null, it will contain all the files needed for this parse, and furthermore, other entries will be deleted.
     */
    public static Module parseEverything_fromFile (Map<String,String> loaded, String filename) throws Err {
        try {
            filename = Util.canon(filename);
            Set<String> thispath = new LinkedHashSet<String>();
            if (loaded==null) loaded = new LinkedHashMap<String,String>();
            Map<String,String> fc = new LinkedHashMap<String,String>(loaded);
            loaded.clear();
            Module root = parseRecursively(loaded, fc, new Pos(filename,1,1), filename, null, "", thispath);
            A4Reporter rep = A4Reporter.getReporter();
            return Module.resolveAll(rep, root);
        } catch(FileNotFoundException ex) {
            throw new ErrorSyntax("File cannot be found.\n"+ex.getMessage());
        } catch(IOException ex) {
            throw new ErrorFatal("IOException occurred: "+ex.getMessage(), ex);
        } catch(Throwable ex) {
            if (ex instanceof Err) throw (Err)ex; else throw new ErrorFatal("Unknown exception occurred: "+ex, ex);
        }
    }
}
