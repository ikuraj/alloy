/* Alloy Analyzer 4 -- Copyright (c) 2006-2009, Felix Chang
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify,
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF
 * OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package edu.mit.csail.sdg.alloy4;

import java.awt.event.ActionEvent;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.NoSuchElementException;
import java.util.prefs.Preferences;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.DefaultSingleSelectionModel;
import javax.swing.Icon;

import edu.mit.csail.sdg.alloy4.ConstList.TempList;

/** This provides useful static methods for I/O and XML operations.
 *
 * <p><b>Thread Safety:</b>  Safe.
 */
@SuppressWarnings({"serial", "unchecked"})
public final class Util {

   /** This constructor is private, since this utility class never needs to be instantiated. */
   private Util() { }
   
   /** Base class for holding a single preference */
   public abstract static class Pref<T> extends DefaultSingleSelectionModel {

      /** The id associated with this preference. */
      public final String id;
      
      public String title;
      public String desc;
      
      /** Constructors a new BooleanPref object with the given id. */
      public Pref(String id)                            { this(id, id); }
      public Pref(String id, String title)              { this(id, title, title); }
      public Pref(String id, String title, String desc) { this.id=id; this.title = title; this.desc = desc; }
      
      /** Reads the value for this preference; if not set or is empty, we return the default value. */
      public T get() {
         String ans=Preferences.userNodeForPackage(Util.class).get(id, "");
         return (ans==null || ans.length()==0) ? defaultValue() : parse(ans);
      }
      
      /** Sets the value for this preference. */
      public void set(T value)  {
         T oldValue = get();
         String str = serialize(value);
         assert value != null && parse(str) != null;
         Preferences.userNodeForPackage(Util.class).put(id, str);
         if (!oldValue.equals(value)) fireStateChanged();
      }
      
      @Override public void setSelectedIndex(int index) { throw new UnsupportedOperationException(); }            
      @Override public int getSelectedIndex()           { return 0; }
      
      /** Returns the default value of this preference. */
      protected abstract T defaultValue();
      /** Returns a serialized (string) version of the given value that will be persisted using {@link Preferences}. */
      protected String serialize(T value) { return value.toString(); }     
      /** Parses a given string value into the original type (T) of this preference. */
      protected abstract T parse(String str);      
   }
   
   /** This reads and writes multi-choice persistent preferences. */
   public static class ChoicePref<T> extends Pref<T> {          
      private final List<T> validChoices;
      private final T defaultValue;
      
      public ChoicePref(String id, T[] validChoices, T... defValueCandidates)                       { this(id, Arrays.asList(validChoices), defValueCandidates); }
      public ChoicePref(String id, Iterable<T> validChoices, T... defValueCandidates)               { this(id, id, id, validChoices, defValueCandidates); }
      public ChoicePref(String id, String title, Iterable<T> validChoices, T... defValueCandidates) { this(id, title, title, validChoices, defValueCandidates); }
      public ChoicePref(String id, String title, String desc, Iterable<T> validChoices, T... defValueCandidates) {         
         super(id, title, desc);
         if (validChoices == null) {
            assert new Exception().getStackTrace()[1].getClassName().endsWith(DelayedChoicePref.class.getName());
            this.validChoices = null;
            this.defaultValue = null;
         } else {
            Object[] x = checkChoices(validChoices, defValueCandidates); 
            this.validChoices = (List<T>) x[0];
            this.defaultValue = (T) x[1];
         }
      }
      
      @Override public void setSelectedIndex(int index) { set(validChoices().get(index)); }            
      @Override public int getSelectedIndex()           { return index(get()); }
      
      protected Object[] checkChoices(Iterable<T> validChoices, T[] defValueCandidates) {
         List<T> lst = new ArrayList<T>();
         for (T t : validChoices) lst.add(t);
         T defaultValue = null;
         for (T defVal : defValueCandidates) { 
            if (lst.contains(defVal)) { defaultValue = defVal; break; }
         }
         if (defaultValue == null) { defaultValue = lst.get(0); }
         return new Object[] {Collections.unmodifiableList(lst), defaultValue};
      }
      
      /** Returns a list of valid choices for this multi-choice preference */
      public List<T> validChoices() { return validChoices; }
      /** Returns the index of the given value (choice) in the list of all valid choices */ 
      public int index(T value)     { return validChoices.indexOf(get()); }
      
      @Override protected String serialize(T value) { return value.toString(); }     
      @Override protected T parse(String str)       { for (T t : validChoices()) { if (serialize(t).equals(str)) return t; } return null; }
      @Override protected T defaultValue()          { return defaultValue; }
      
      public Action getAction(final T value) {
         return new AbstractAction() {           
            public void actionPerformed(ActionEvent e) { set(value); }
         };
      }
      
      public Object renderValueShort(T value) { return value; }
      public Object renderValueLong(T value)  { return renderValueShort(value); }  
   }
   
   /** Provides a way to delay supplying valid choices and a default value */
   public static class DelayedChoicePref<T> extends ChoicePref<T> {
      private List<T> validChoices2;
      private T defaultValue2;
      
      public DelayedChoicePref(String id)                            { this(id, id, id); }
      public DelayedChoicePref(String id, String title)              { this(id, title, title); }
      public DelayedChoicePref(String id, String title, String desc) { super(id, title, desc, null, ((T)new Object[]{})); }

      public DelayedChoicePref(String id, Iterable<T> validChoices, T... defValueCandidates)                            { super(id, validChoices, defValueCandidates); }
      public DelayedChoicePref(String id, String title, Iterable<T> validChoices, T... defValueCandidates)              { super(id, title, validChoices, defValueCandidates); }
      public DelayedChoicePref(String id, String title, String desc, Iterable<T> validChoices, T... defValueCandidates) { super(id, title, desc, validChoices, defValueCandidates); }
      
      public void setChoices(Iterable<T> validChoices, T... defValueCandidates) {
         Object[] x = checkChoices(validChoices, defValueCandidates); 
         this.validChoices2 = (List<T>) x[0];
         this.defaultValue2 = (T) x[1];
      }
      
      protected T defaultValue()    { return defaultValue2 != null ? defaultValue2 : super.defaultValue(); }
      public List<T> validChoices() { return validChoices2 != null ? validChoices2 : super.validChoices(); }
   }
   
   /** This reads and writes IntChoice-valued Java persistent preferences. */
   public static class IntChoicePref extends ChoicePref<Integer> {
      public IntChoicePref(String id, Iterable<Integer> validChoices, Integer defaultValue) {
         super(id, validChoices, defaultValue);
      }
      public IntChoicePref(String id, String title, Iterable<Integer> validChoices, Integer defaultValue) {
         super(id, title, validChoices, defaultValue);
      }
      public IntChoicePref(String id, String title, String desc, Iterable<Integer> validChoices, Integer defaultValue) {
         super(id, title, desc, validChoices, defaultValue);
      }

      public static IntChoicePref range(String id, int min, int step, int max, int defaultValue) { return range(id, id, id, min, step, max, defaultValue); }
      public static IntChoicePref range(String id, String title, int min, int step, int max, int defaultValue) { return range(id, title, title, min, step, max, defaultValue); }
      public static IntChoicePref range(String id, String title, String desc, int min, int step, int max, int defaultValue) {
         ArrayList<Integer> lst = new ArrayList<Integer>();
         for (int x = min; x <= max; x += step) { lst.add(x); }
         return new IntChoicePref(id, title, desc, lst, defaultValue);
      }      
   }
   
   /** This reads and writes StringChoice-valued Java persistent preferences. */
   public static class StringChoicePref extends ChoicePref<String> {      
      public StringChoicePref(String id, Iterable<String> validChoices, String... defaultValueCandidates) {
         super(id, validChoices, defaultValueCandidates);
      }
      public StringChoicePref(String id, String title, Iterable<String> validChoices, String... defaultValueCandidates) {
         super(id, title, validChoices, defaultValueCandidates);
      }
      public StringChoicePref(String id, String title, String desc, Iterable<String> validChoices, String... defaultValueCandidates) {
         super(id, title, desc, validChoices, defaultValueCandidates);
      }  
   }

   /** This reads and writes String-valued Java persistent preferences.
    * <p><b>Thread Safety:</b>  Safe. */
   public static final class StringPref extends Pref<String> {
      /** The default value for this preference. */
      private final String defaultValue;
      /** Constructs a new StringPref object with the given id. */
      public StringPref (String id) {super(id); this.defaultValue="";}
      /** Constructs a new StringPref object with the given id and the given default value. */
      public StringPref (String id, String defaultValue) {super(id); this.defaultValue=defaultValue;}
      
      @Override protected String serialize(String value) { return value; }     
      @Override protected String parse(String str)       { return str; }
      @Override protected String defaultValue()          { return defaultValue; }
   }

   /** This reads and writes boolean-valued Java persistent preferences.
    * <p><b>Thread Safety:</b>  Safe. */
   public static final class BooleanPref extends Pref<Boolean> {
      public BooleanPref(String id, String title, String desc) { super(id, title, desc); }
      public BooleanPref(String id, String title)              { super(id, title); }
      public BooleanPref(String id)                            { super(id); }
      
      @Override protected String serialize(Boolean value) { return value ? "y" : ""; }     
      @Override protected Boolean parse(String str)       { return "y".equals(str); }
      @Override protected Boolean defaultValue()          { return false; }
      
      /** Toggles the value for this preference. */
      public void toggle() { set(!get()); }
       
      public BooleanAction getAction(String title) { return new BooleanAction(this, title); }
      public BooleanAction getTitleAction()        { return getAction(title); }
      public BooleanAction getDescAction()         { return getAction(desc); }
   }

   /** This reads and writes integer-valued Java persistent preferences.
    * <p><b>Thread Safety:</b>  Safe. */
   public static final class IntPref extends Pref<Integer> {
      /** The minimum value for this preference. */
      private final int min;
      /** The maximum value for this preference. */
      private final int max;
      /** The default value for this preference. */
      private final int def;
      /** If min>n, we return min; else if n>max, we return max; otherwise we return n. */
      private int bound (int n) { return n<min ? min : (n>max? max : n); }
            
      /** Make a new IntPref object with the given id; you must ensure max >= min, but def does not have to be between min..max */
      public IntPref (String id, int min, int def, int max) {super(id); this.min=min; this.def=def; this.max=max;}    

      @Override protected String serialize(Integer value) { return "" + bound(value); }     
      @Override protected Integer parse(String str)       { return bound(Integer.parseInt(str)); }
      @Override protected Integer defaultValue()          { return def; }
   }
   
   public static final class BooleanAction extends AbstractAction {
      private static final long serialVersionUID = -7357369720337054603L;
      private final BooleanPref pref;
           
      public BooleanAction(BooleanPref pref)              { this(pref, null, null); }      
      public BooleanAction(BooleanPref pref, String name) { this(pref, name, null); }      
      public BooleanAction(BooleanPref pref, String name, Icon icon) {
         super(name, icon);
         this.pref = pref;
      }
      
      public void actionPerformed(ActionEvent e) { pref.toggle(); }
   }

   /** Copy the input list, append "element" to it, then return the result as an unmodifiable list. */
   public static<T> ConstList<T> append(List<T> list, T element) {
      TempList<T> ans = new TempList<T>(list.size()+1);
      ans.addAll(list).add(element);
      return ans.makeConst();
   }

   /** Copy the input array, append "element" to it, then return the result as a new array. */
   public static<T> T[] append(T[] list, T element) {
      T[] ans = (T[]) java.lang.reflect.Array.newInstance(list.getClass().getComponentType(), list.length+1);
      System.arraycopy(list, 0, ans, 0, list.length);
      ans[ans.length-1] = element;
      return ans;
   }

   /** Copy the input list, prepend "element" to it, then return the result as an unmodifiable list. */
   public static<T> ConstList<T> prepend(List<T> list, T element) {
      TempList<T> ans = new TempList<T>(list.size()+1);
      ans.add(element).addAll(list);
      return ans.makeConst();
   }

   /** Returns an unmodifiable List with same elements as the array. */
   public static<T> ConstList<T> asList(T... array) {
      return (new TempList<T>(array)).makeConst();
   }

   /** Returns a newly created LinkedHashSet containing the given elements in the given order. */
   public static<V> LinkedHashSet<V> asSet(V... values) {
      LinkedHashSet<V> ans = new LinkedHashSet<V>();
      for(int i=0; i<values.length; i++) ans.add(values[i]);
      return ans;
   }

   /** Returns a newly created LinkedHashMap mapping each key to its corresponding value, in the given order. */
   public static<K,V> LinkedHashMap<K,V> asMap(K[] keys, V... values) {
      LinkedHashMap<K,V> ans = new LinkedHashMap<K,V>();
      for(int i=0; i<keys.length && i<values.length; i++) ans.put(keys[i], values[i]);
      return ans;
   }

   /** Return an iterable whose iterator is a read-only iterator that first iterate over Collection1, and then Collection2. */
   public static<E> Iterable<E> fastJoin(final Iterable<E> collection1, final Iterable<E> collection2) {
      return new Iterable<E>() {
         public Iterator<E> iterator() {
            return new Iterator<E> () {
               private Iterator<E> a=collection1.iterator(), b=collection2.iterator();
               public boolean hasNext() {
                  if (a!=null) { if (a.hasNext()) return true; a=null; }
                  if (b!=null) { if (b.hasNext()) return true; b=null; }
                  return false;
               }
               public E next() {
                  if (a!=null) { if (a.hasNext()) return a.next(); a=null; }
                  if (b!=null) { if (b.hasNext()) return b.next(); b=null; }
                  throw new NoSuchElementException();
               }
               public void remove() { throw new UnsupportedOperationException(); }
            };
         }
      };
   }

   /** Converts Windows/Mac/Unix linebreaks into '\n', and replace non-tab non-linebreak control characters into space. */
   public static String convertLineBreak(String input) {
      return input.replace("\r\n","\n").replace('\r','\n').replaceAll("[\000-\010\013\014\016-\037]"," ");
   }

   /** Attempt to close the file/stream/reader/writer and return true if and only if we successfully closed it.
    * (If object==null, we return true right away)
    */
   public static boolean close(Closeable object) {
      if (object==null) return true;
      boolean ans=true;
      try {
         if (object instanceof PrintStream && ((PrintStream)object).checkError()) ans=false;
         if (object instanceof PrintWriter && ((PrintWriter)object).checkError()) ans=false;
         object.close();
         return ans;
      } catch(Throwable ex) {
         return false;
      }
   }

   /** This synchronized field stores the current "default directory" which is used by the FileOpen and FileSave dialogs. */
   private static String currentDirectory = null;

   /** Modifies the current "default directory" which is used by the FileOpen and FileSave dialogs. */
   public synchronized static void setCurrentDirectory(File newDirectory) {
      if (newDirectory==null) // this can actually happen
         currentDirectory = canon(System.getProperty("user.home"));
      else
         currentDirectory = canon(newDirectory.getAbsolutePath());
   }

   /** Returns the current "default directory" which is used by the FileOpen and FileSave dialogs. */
   public synchronized static String getCurrentDirectory() {
      if (currentDirectory == null) currentDirectory = canon(System.getProperty("user.home"));
      return currentDirectory;
   }

   /** This returns the constant prefix to denote whether Util.readAll() should read from a JAR or read from the file system.
    * (The reason we made this into a "method" rather than a constant String is that it is used
    * by Util.canon() which is called by many static initializer blocks... so if we made this into a static field
    * of Util, then it may not be initialized yet when we need it!)
    */
   public static String jarPrefix() { return File.separator + "$alloy4$" + File.separator; }

   /** Read everything into a String; throws IOException if an error occurred.
    * (If filename begins with Util.jarPrefix() then we read from the JAR instead)
    */
   public static String readAll(String filename) throws FileNotFoundException, IOException {
      String JAR = jarPrefix();
      boolean fromJar=false;
      if (filename.startsWith(JAR)) { fromJar=true; filename=filename.substring(JAR.length()).replace('\\', '/'); }
      InputStream fis=null;
      int now=0, max=4096;
      if (!fromJar) {
         long maxL = new File(filename).length();
         max = (int)maxL;
         if (max != maxL) throw new IOException("File too big to fit in memory");
      }
      byte[] buf;
      try {
         buf = new byte[max];
         fis = fromJar ? Util.class.getClassLoader().getResourceAsStream(filename) : new FileInputStream(filename);
         if (fis==null) throw new FileNotFoundException("File \""+filename+"\" cannot be found");
         while(true) {
            if (now >= max) {
               max = now + 4096;
               if (max<now) throw new IOException("File too big to fit in memory");
               byte[] buf2 = new byte[max];
               if (now>0) System.arraycopy(buf, 0, buf2, 0, now);
               buf = buf2;
            }
            int r = fis.read(buf, now, max-now);
            if (r<0) break;
            now = now + r;
         }
      } catch(OutOfMemoryError ex) {
         System.gc();
         throw new IOException("There is insufficient memory.");
      } finally {
         close(fis);
      }
      CodingErrorAction r = CodingErrorAction.REPORT;
      CodingErrorAction i = CodingErrorAction.IGNORE;
      ByteBuffer bbuf;
      String ans = "";
      try {
         // We first try UTF-8;
         bbuf=ByteBuffer.wrap(buf, 0, now);
         ans=Charset.forName("UTF-8").newDecoder().onMalformedInput(r).onUnmappableCharacter(r).decode(bbuf).toString();
      } catch(CharacterCodingException ex) {
         try {
            // if that fails, we try using the platform's default charset
            bbuf=ByteBuffer.wrap(buf, 0, now);
            ans=Charset.defaultCharset().newDecoder().onMalformedInput(r).onUnmappableCharacter(r).decode(bbuf).toString();
         } catch(CharacterCodingException ex2) {
            // if that also fails, we try using "ISO-8859-1" which should always succeed but may map some characters wrong
            bbuf=ByteBuffer.wrap(buf, 0, now);
            ans=Charset.forName("ISO-8859-1").newDecoder().onMalformedInput(i).onUnmappableCharacter(i).decode(bbuf).toString();
         }
      }
      return convertLineBreak(ans);
   }

   /** Open then overwrite the file with the given content; throws Err if an error occurred. */
   public static long writeAll(String filename, String content) throws Err {
      final FileOutputStream fos;
      try {
         fos=new FileOutputStream(filename);
      } catch(IOException ex) {
         throw new ErrorFatal("Cannot write to the file "+filename);
      }
      // Convert the line break into the UNIX line break, and remove ^L, ^F... and other characters that confuse JTextArea
      content = convertLineBreak(content);
      // If the last line does not have a LINEBREAK, add it
      if (content.length()>0 && content.charAt(content.length()-1)!='\n') content=content+"\n";
      // Now, convert the line break into the local platform's line break, then write it to the file
      try {
         final String NL = System.getProperty("line.separator");
         byte[] array = content.replace("\n",NL).getBytes("UTF-8");
         fos.write(array);
         fos.close();
         return array.length;
      } catch(IOException ex) {
         close(fos);
         throw new ErrorFatal("Cannot write to the file "+filename, ex);
      }
   }

   /** Returns the canonical absolute path for a file.
    * If an IO error occurred, or if the file doesn't exist yet,
    * we will at least return a noncanonical but absolute path for it.
    * <p> Note: if filename=="", we return "".
    */
   public static final String canon(String filename) {
      if (filename==null || filename.length()==0) return "";
      if (filename.startsWith(jarPrefix())) {
         char sep = File.separatorChar, other = (sep=='/' ? '\\' : '/');
         return filename.replace(other, sep);
      }
      File file = new File(filename);
      try { return file.getCanonicalPath(); } catch(IOException ex) { return file.getAbsolutePath(); }
   }

   /** Sorts two strings for optimum module order; we guarantee slashComparator(a,b)==0 iff a.equals(b).
    * <br> (1) First of all, the builtin names "extend" and "in" are sorted ahead of other names
    * <br> (2) Else, if one string starts with "this/", then it is considered smaller
    * <br> (3) Else, if one string has fewer '/' than the other, then it is considered smaller.
    * <br> (4) Else, we compare them lexically without case-sensitivity.
    * <br> (5) Finally, we compare them lexically with case-sensitivity.
    */
   public static final Comparator<String> slashComparator = new Comparator<String>() {
      public final int compare(String a, String b) {
         if (a==null) return (b==null)?0:-1; else if (b==null) return 1; else if (a.equals(b)) return 0;
         if (a.equals("extends")) return -1; else if (b.equals("extends")) return 1;
         if (a.equals("in"))      return -1; else if (b.equals("in"))      return 1;
         if (a.startsWith("this/")) {
            if (!b.startsWith("this/")) return -1;
         } else if (b.startsWith("this/")) {
            return 1;
         }
         int acount=0, bcount=0;
         for(int i=0; i<a.length(); i++) { if (a.charAt(i)=='/') acount++; }
         for(int i=0; i<b.length(); i++) { if (b.charAt(i)=='/') bcount++; }
         if (acount!=bcount) return (acount<bcount) ? -1 : 1;
         int result = a.compareToIgnoreCase(b);
         return result!=0 ? result : a.compareTo(b);
      }
   };

   /** Copy the given file from JAR into the destination file; if the destination file exists, we then do nothing.
    * Returns true iff a file was created and written.
    */
   private static boolean copy(String sourcename, String destname) {
      File destfileobj = new File(destname);
      if (destfileobj.isFile() && destfileobj.length()>0) return false;
      boolean result = true;
      InputStream in = null;
      FileOutputStream out = null;
      try {
         in = Util.class.getClassLoader().getResourceAsStream(sourcename);
         if (in==null) return false; // This means the file is not relevant for this setup, so we don't pop up a fatal dialog
         out = new FileOutputStream(destname);
         byte[] b = new byte[16384];
         while(true) {
            int numRead = in.read(b);
            if (numRead < 0) break;
            if (numRead > 0) out.write(b, 0, numRead);
         }
      } catch (IOException e) {
         result=false;
      }
      if (!close(out)) result=false;
      if (!close(in)) result=false;
      if (!result) OurDialog.fatal("Error occurred in creating the file \""+destname+"\"");
      return true;
   }

   /** Copy the list of files from JAR into the destination directory,
    * then set the correct permissions on them if possible.
    *
    * @param executable - if true, we will attempt to set the file's "executable" permission (failure to do this is ignored)
    * @param keepPath - if true, the full path will be created for the destination file
    * @param destdir - the destination directory
    * @param names - the files to copy from the JAR
    */
   public static void copy(boolean executable, boolean keepPath, String destdir, String... names) {
      String[] args = new String[names.length+2];
      args[0] = "/bin/chmod"; // This does not work on Windows, but the "executable" bit is not needed on Windows anyway.
      args[1] = (executable ? "700" : "600"); // 700 means read+write+executable; 600 means read+write.
      int j=2;
      for(int i=0; i<names.length; i++) {
         String name = names[i];
         String destname = name;
         if (!keepPath) { int ii=destname.lastIndexOf('/'); if (ii>=0) destname=destname.substring(ii+1); }
         destname=(destdir+'/'+destname).replace('/', File.separatorChar);
         int last=destname.lastIndexOf(File.separatorChar);
         new File(destname.substring(0,last+1)).mkdirs(); // Error will be caught later by the file copy
         if (copy(name, destname)) { args[j]=destname; j++; }
      }
      if (onWindows() || j<=2) return;
      String[] realargs = new String[j];
      for(int i=0; i<j; i++) realargs[i] = args[i];
      try {
         Runtime.getRuntime().exec(realargs).waitFor();
      } catch (Throwable ex) {
         // We only intend to make a best effort
      }
   }

   /** Copy file.content[from...f.length-1] into file.content[to...], then truncate the file after that point.
    * <p> If (from &gt; to), this means we simply delete the portion of the file beginning at "to" and up to but excluding "from".
    * <p> If (from &lt; to), this means we insert (to-from) number of ARBITRARY bytes into the "from" location
    *     and shift the original file content accordingly.
    * <p> Note: after this operation, the file's current position will be moved to the start of the file.
    * @throws IOException if (from &lt; 0) || (to &lt; 0) || (from &gt;= file.length())
    */
   public static void shift (RandomAccessFile file, long from, long to) throws IOException {
      long total = file.length();
      if (from<0 || from>=total || to<0) throw new IOException(); else if (from==to) {file.seek(0); return;}
      final byte buf[] = new byte[4096];
      int res;
      if (from>to) {
         while(true) {
            file.seek(from);
            if ((res=file.read(buf)) <= 0) { file.setLength(to); file.seek(0); return; }
            file.seek(to); file.write(buf, 0, res); from=from+res; to=to+res;
         }
      } else {
         file.seek(total);
         for(long todo=to-from; todo>0;) {
            if (todo >= buf.length) {file.write(buf); todo = todo - buf.length;} else {file.write(buf, 0, (int)todo); break;}
         }
         for(long todo=total-from; todo>0; total=total-res, todo=todo-res) {
            if (todo > buf.length) res=buf.length; else res=(int)todo;
            file.seek(total - res);
            for(int done=0; done<res;) { int r=file.read(buf, done, res-done); if (r<=0) throw new IOException(); else done += r; }
            file.seek(total - res + (to - from));
            file.write(buf, 0, res);
         }
      }
      file.seek(0);
   }

   /** Write a String into a PrintWriter, and encode special characters using XML-specific encoding.
    *
    * <p>
    * In particular, it changes LESS THAN, GREATER THAN, AMPERSAND, SINGLE QUOTE, and DOUBLE QUOTE
    * into "&amp;lt;" "&amp;gt;" "&amp;amp;" "&amp;apos;" and "&amp;quot;" and turns any characters
    * outside of the 32..126 range into the "&amp;#xHHHH;" encoding
    * (where HHHH is the 4 digit lowercase hexadecimal representation of the character value).
    *
    * @param out - the PrintWriter to write into
    * @param str - the String to write out
    */
   public static void encodeXML(PrintWriter out, String str) {
      int n=str.length();
      for(int i=0; i<n; i++) {
         char c=str.charAt(i);
         if (c=='<') { out.write("&lt;"); continue; }
         if (c=='>') { out.write("&gt;"); continue; }
         if (c=='&') { out.write("&amp;"); continue; }
         if (c=='\'') { out.write("&apos;"); continue; }
         if (c=='\"') { out.write("&quot;"); continue; }
         if (c>=32 && c<=126) { out.write(c); continue; }
         out.write("&#x");
         String v=Integer.toString(c, 16);
         for(int j=v.length(); j<4; j++) out.write('0');
         out.write(v);
         out.write(';');
      }
   }

   /** Write a String into a StringBuilder, and encode special characters using XML-specific encoding.
    *
    * <p>
    * In particular, it changes LESS THAN, GREATER THAN, AMPERSAND, SINGLE QUOTE, and DOUBLE QUOTE
    * into "&amp;lt;" "&amp;gt;" "&amp;amp;" "&amp;apos;" and "&amp;quot;" and turns any characters
    * outside of the 32..126 range into the "&amp;#xHHHH;" encoding
    * (where HHHH is the 4 digit lowercase hexadecimal representation of the character value).
    *
    * @param out - the StringBuilder to write into
    * @param str - the String to write out
    */
   public static void encodeXML(StringBuilder out, String str) {
      int n=str.length();
      for(int i=0; i<n; i++) {
         char c=str.charAt(i);
         if (c=='<') { out.append("&lt;"); continue; }
         if (c=='>') { out.append("&gt;"); continue; }
         if (c=='&') { out.append("&amp;"); continue; }
         if (c=='\'') { out.append("&apos;"); continue; }
         if (c=='\"') { out.append("&quot;"); continue; }
         if (c>=32 && c<=126) { out.append(c); continue; }
         out.append("&#x");
         String v=Integer.toString(c, 16);
         for(int j=v.length(); j<4; j++) out.append('0');
         out.append(v).append(';');
      }
   }

   /** Encode special characters of a String using XML/HTML encoding.
    *
    * <p>
    * In particular, it changes LESS THAN, GREATER THAN, AMPERSAND, SINGLE QUOTE, and DOUBLE QUOTE
    * into "&amp;lt;" "&amp;gt;" "&amp;amp;" "&amp;apos;" and "&amp;quot;" and turns any characters
    * outside of the 32..126 range into the "&amp;#xHHHH;" encoding
    * (where HHHH is the 4 digit lowercase hexadecimal representation of the character value).
    */
   public static String encode(String str) {
      if (str.length() == 0) return str;
      StringBuilder sb = new StringBuilder();
      encodeXML(sb, str);
      return sb.toString();
   }

   /** Write a list of Strings into a PrintWriter, where strs[2n] are written as-is, and strs[2n+1] are XML-encoded.
    *
    * <p> For example, if you call encodeXML(out, A, B, C, D, E), it is equivalent to the following:
    * <br> out.print(A);
    * <br> encodeXML(out, B);
    * <br> out.print(C);
    * <br> encodeXML(out, D);
    * <br> out.print(E);
    * <br> In other words, it writes the even entries as-is, and writes the odd entries using XML encoding.
    *
    * @param out - the PrintWriter to write into
    * @param strs - the list of Strings to write out
    */
   public static void encodeXMLs(PrintWriter out, String... strs) {
      for(int i=0; i<strs.length; i++) {
         if ((i%2)==0) out.print(strs[i]); else encodeXML(out,strs[i]);
      }
   }

   /** Write a list of Strings into a StringBuilder, where strs[2n] are written as-is, and strs[2n+1] are XML-encoded.
    *
    * <p> For example, if you call encodeXML(out, A, B, C, D, E), it is equivalent to the following:
    * <br> out.append(A);
    * <br> encodeXML(out, B);
    * <br> out.append(C);
    * <br> encodeXML(out, D);
    * <br> out.append(E);
    * <br> In other words, it writes the even entries as-is, and writes the odd entries using XML encoding.
    *
    * @param out - the StringBuilder to write into
    * @param strs - the list of Strings to write out
    */
   public static void encodeXMLs(StringBuilder out, String... strs) {
      for(int i=0; i<strs.length; i++) {
         if ((i%2)==0) out.append(strs[i]); else encodeXML(out,strs[i]);
      }
   }

   /** Finds the first occurrence of <b>small</b> within <b>big</b>.
    * @param big - the String that we want to perform the search on
    * @param small - the pattern we are looking forward
    * @param start - the offset within "big" to start (for example: 0 means to start from the beginning of "big")
    * @param forward - true if the search should go forward; false if it should go backwards
    * @param caseSensitive - true if the search should be done in a case-sensitive manner
    *
    * @return 0 or greater if found, -1 if not found (Note: if small=="", then we always return -1)
    */
   public static int indexOf(String big, String small, int start, boolean forward, boolean caseSensitive) {
      int len=big.length(), slen=small.length();
      if (slen==0) return -1;
      while(start>=0 && start<len) {
         for(int i=0 ; ; i++) {
            if (i>=slen) return start;
            if (start+i>=len) break;
            int b=big.charAt(start+i), s=small.charAt(i);
            if (!caseSensitive && b>='A' && b<='Z') b=(b-'A')+'a';
            if (!caseSensitive && s>='A' && s<='Z') s=(s-'A')+'a';
            if (b!=s) break;
         }
         if (forward) start++; else start--;
      }
      return -1;
   }

   /** Returns true iff running on Windows **/
   public static boolean onWindows() {
      return System.getProperty("os.name").toLowerCase(Locale.US).startsWith("windows");
   };

   /** Returns true iff running on Mac OS X. **/
   public static boolean onMac() {
      return System.getProperty("mrj.version")!=null || System.getProperty("os.name").toLowerCase(Locale.US).startsWith("mac ");
   }

   /** Returns the substring after the last "/" */
   public static String tail(String string)  { int i=string.lastIndexOf('/'); return (i<0) ? string : string.substring(i+1); }

   /** Returns the largest allowed integer, or -1 if no integers are allowed (bitwidth < 1). */
   public static int max(int bitwidth) { return bitwidth < 1 ? -1 : (1<<(bitwidth-1))-1; }
   
   /** Returns the smallest allowed integer, or 0 if no integers are allowed (bitwidth < 1)*/
   public static int min(int bitwidth) { return bitwidth < 1 ?  0 : 0-(1<<(bitwidth-1)); }

   /** Returns a mask of the form 000..0011..11 where the number of 1s is equal to the number of significant bits of the highest integer withing the given bitwidth */
   public static int shiftmask(int bitwidth) { return bitwidth < 1 ? 0 : (1 << (32 - Integer.numberOfLeadingZeros(bitwidth-1))) - 1; }

}
