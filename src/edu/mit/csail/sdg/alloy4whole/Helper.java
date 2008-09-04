/*
 * Alloy Analyzer 4 -- Copyright (c) 2006-2008, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4whole;

import java.io.File;
import java.util.Date;
import java.util.Random;
import edu.mit.csail.sdg.alloy4.OurDialog;
import edu.mit.csail.sdg.alloy4.Util;

/** This helper method is used by SimpleGUI. */

final class Helper {

    /** Constructor is private, since this utility class never needs to be instantiated. */
    private Helper() { }

    /** This variable caches the result of alloyHome() function call. */
    private static String alloyHome=null;

    /** Find a temporary directory to store Alloy files; it's guaranteed to be a canonical absolute path. */
    public static synchronized String alloyHome() {
        if (alloyHome!=null) return alloyHome;
        String temp=System.getProperty("java.io.tmpdir");
        if (temp==null || temp.length()==0)
            OurDialog.fatal(null,"Error. JVM need to specify a temporary directory using java.io.tmpdir property.");
        String username=System.getProperty("user.name");
        File tempfile=new File(temp+File.separatorChar+"alloy4tmp39-"+(username==null?"":username));
        tempfile.mkdirs();
        String ans=Util.canon(tempfile.getPath());
        if (!tempfile.isDirectory()) {
            OurDialog.fatal(null, "Error. Cannot create the temporary directory "+ans);
        }
        if (!Util.onWindows()) {
            String[] args={"chmod", "700", ans};
            try {Runtime.getRuntime().exec(args).waitFor();}
            catch (Throwable ex) {} // We only intend to make a best effort.
        }
        return alloyHome=ans;
    }

    /**
     * Create an empty temporary directory for use, designate it "deleteOnExit", then return it.
     * It is guaranteed to be a canonical absolute path.
     */
    public static String maketemp() {
        Random r=new Random(new Date().getTime());
        while(true) {
            int i=r.nextInt(1000000);
            String dest = alloyHome()+File.separatorChar+"tmp"+File.separatorChar+i;
            File f=new File(dest);
            if (f.mkdirs()) {
                f.deleteOnExit();
                return Util.canon(dest);
            }
        }
    }

    /** Return the number of bytes used by the Temporary Directory (or return -1 if the answer exceeds "long") */
    public static long computeTemporarySpaceUsed() {
        long ans = iterateTemp(null,false);
        if (ans<0) return -1; else return ans;
    }

    /** Delete every file in the Temporary Directory. */
    public static void clearTemporarySpace() {
        iterateTemp(null,true);
        // Also clear the temp dir from previous versions of Alloy4
        String temp=System.getProperty("java.io.tmpdir");
        if (temp==null || temp.length()==0) return;
        String username=System.getProperty("user.name");
        if (username==null) username="";
        for(int i=1; i<39; i++) iterateTemp(temp+File.separatorChar+"alloy4tmp"+i+"-"+username, true);
    }

    private static long iterateTemp(String filename, boolean delete) {
        long ans=0;
        if (filename==null) filename = alloyHome()+File.separatorChar+"tmp";
        File x = new File(filename);
        if (x.isDirectory()) {
            for(String subfile:x.list()) {
                long tmp=iterateTemp(filename+File.separatorChar+subfile, delete);
                if (ans>=0) ans=ans+tmp;
            }
        }
        else if (x.isFile()) {
            long tmp=x.length();
            if (ans>=0) ans=ans+tmp;
        }
        if (delete) x.delete();
        return ans;
    }
}
