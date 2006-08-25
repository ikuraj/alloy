package kodviz.dotviz;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.List;

import javax.swing.JPanel;

import ext.grappa.Element;
import ext.grappa.GrappaPanel;
import ext.grappa.GrappaPoint;
import ext.grappa.Subgraph;

import kodviz.util.Dbg;
import kodviz.util.ProcessInfo;
import kodviz.util.TmpFiles;

import kodviz.graph.Cartoon;
import kodviz.graph.Graph;


/**
 * The Visualizer class is used for transforming a Cartoon object (and a
 * setting/set of indices) into a grappa JPanel with the generated graph.
 */
public class Visualizer {

    //private String _root;

    public JPanel generateGraphPanel(Cartoon toon_, List setting_) {
        //System.out.println("getting: "+setting_);
        Graph g = toon_.getGraph(setting_);
        if (g==null) {
            return new JPanel();
        }

        final File dotFile = TmpFiles.createHandleToATempFile("dotfile.dot");
        final File layoutFile = TmpFiles.createHandleToATempFile("layoutfile.dot");

        (new DotFileGenerator()).generateDotFile(toon_, "g", dotFile, setting_);
        // no graph name currently?

        // creates the layout file
        {
		layoutFile.delete();
		final String[] cmdArgs = {dotFile.getName(), "-o", layoutFile.getName()};
		ProcessInfo procInfo=null;
		Exception ex=null;
		if (procInfo==null) {
		  try {procInfo = new ProcessInfo("dotbin", cmdArgs, TmpFiles.getTmpDir());} catch (Exception x) { ex=x; }
	    }
		if (procInfo==null) { ex.printStackTrace(); Dbg.fatal("Could not create layout file: ", ex); }
		procInfo.waitForTermination();
        }

        //****************************************************
        // gives it to grappa (code recycled form VizData.java)
        //****************************************************
        InputStream input = null;
        try {
            input = new FileInputStream(layoutFile);
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
            return new JPanel();
        }

        // grappa parses it
        ext.grappa.Parser program = new ext.grappa.Parser(input, System.err);
        try {
            program.parse();
            input.close();
        }
        catch (Exception ex) {
            ex.printStackTrace();
        }

        ext.grappa.Graph graph = program.getGraph();
        // ***hmmm should I recycle the editable line too?  will check!
        graph.setEditable(true);
        graph.setErrorWriter(new PrintWriter(System.err, true));

        // extract the grappa JPanel
        ext.grappa.GrappaPanel gp = new ext.grappa.GrappaPanel(graph);

        // HERE IS SOMETHING TO CONSIDER--rewrite adapter?
        gp.addGrappaListener(new ext.grappa.GrappaAdapter() {
            public String grappaTip(Subgraph subg, Element elem, GrappaPoint pt, int modifiers, GrappaPanel panel) {
                return null;
            }
        });
        gp.setScaleToFit(false); // do I really?
        

        // there is some disfunctional code in VisData to add a title label...
        // I'm not convinced of that feature's usefulness though.

        //System.out.println("Generated Graph Panel\n");
        return gp;

    }

 }
