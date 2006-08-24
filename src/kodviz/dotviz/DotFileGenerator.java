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

package kodviz.dotviz;

import java.awt.Font;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import kodviz.util.Params;

import kodviz.graph.Cartoon;
import kodviz.graph.Edge;
import kodviz.graph.Graph;
import kodviz.graph.Node;


/*  mostly recycled code from the former viz file with the same name */
class DotFileGenerator {

    private Graph _graph;

    private DotOrientation _orientation;

    private Font _font;
    private String _graphName;

    /**
     * Generates the dot file required to use the tool Dot
     */
    public void generateDotFile(Cartoon toon_, String graphName_, File file_, List setting_) {
        _graph = toon_.getGraph(setting_);
        _graphName = graphName_;
        _orientation = toon_.getOrientation();
        // Use the global text font
        _font = Params.glob.getFontParam("GUI", "textfont");   
        //_font = new Font(toon_.getFontName(), Font.PLAIN, toon_.getFontSize());
        String dotOutput = generateDotOutput(graphName_);
        writeFile(dotOutput, file_);
        //System.out.println("Generating dot file\n");
    }

    private String generateDotOutput(String graphName) {

        // generate the dot output of <instance> with graph name
        // <graphName>
        StringBuffer output = new StringBuffer();
        output.append("digraph \"" + _graphName + "\" {\n");
        output.append(generateFont());
        output.append(generateRankDir());
        output.append(generateEdges());
        output.append(generateNodes());
        output.append(generateSameRank());
        output.append("}\n");
        //Util.ww(output.toString()); what in the world is this?
        return output.toString();
    }

    private String generateRankDir() {
        StringBuffer sb = new StringBuffer();
        if (_orientation.equals(DotOrientation.VERTICAL)) {
            sb.append("rankdir = TB;\n");
        }
        else {
            sb.append("rankdir = LR;\n");
        }
        return sb.toString();
    }

    public void setFont(Font f_) {
        _font = f_;
    }

    private String generateFont() {
        StringBuffer sb = new StringBuffer();
        String font;
        if (_font == null) {
            font = (new Font(null, Font.PLAIN, 12)).getPSName();
            // this shouldn't really happen though
        }
        else {
            font = _font.getPSName();
        }
        sb.append("graph[fontname =\"" + font + "\"" + ", fontsize = " + _font.getSize() + "]\n");
        sb.append("node[fontname =\"" + font + "\"" + ", fontsize = " + _font.getSize() + "]\n");
        sb.append("edge[fontname =\"" + font + "\"" + ", fontsize = " + _font.getSize() + "]\n");
        return sb.toString();
    }

    private String generateNodes() {
        // generate the dot output for its nodes, corresponding
        // to the atoms of <instance>
        StringBuffer nodeOutput = new StringBuffer();
        Node node;
        // go through all the nodes...
        for (Iterator nodes = _graph.getNodes(); nodes.hasNext();) {
            node = (Node) nodes.next();
            // Don't bother with invisible nodes, they won't show up anyway, and
            // they could stretch the graph awkwardly.
            if (node.getStyle() != DotStyle.INVIS) {
                nodeOutput.append(generateNode(node));
            }
        }
        return nodeOutput.toString();
    }

    private String generateNode(Node node_) {

        StringBuffer ret = new StringBuffer();

        String nodeID = node_.getID();
        if (nodeID.indexOf("\"") > -1) {
            nodeID = addEscapeForQuotes(nodeID);
        }

        ret.append("\"" + nodeID + "\""); // quotes should fix empty graphs
        ret.append(" [");

        /*
        String nameLabel = "";
        if(node_.getBoolAttr(AttrType.LABEL)){
            nameLabel = node_.getAttr(AttrType.NAME_LABEL);
            }*/

        /******************GET SET LABEL*********************************/
        /*
        String setLabel = node_.setLabelString();
        
        if(!nameLabel.equals("") &&
           !setLabel.equals("")){
            nameLabel = nameLabel + "\\n";
        }
        
        nameLabel = nameLabel + setLabel;
        */
        /******************DOT ATTRIBUTES*********************************/

        String nodeLabel = node_.getLabel();
        if (nodeLabel.indexOf("\"") > -1) {
            nodeLabel = addEscapeForQuotes(nodeLabel);
        }
        ret.append("label =\"" + nodeLabel + "\"");
        ret.append(
            ", " + node_.getColor().getDotName() + " =\"" + node_.getColor().getDotText() + "\"");
        ret.append(", fontcolor = " + node_.getColor().getLabelColorText());
        // don't know what to do about this line
        //ret.append(", fontcolor =\"" + node_.getAttr(AttrType.TEXT_COLOR) + "\"");

        ret.append(", " + node_.getShape().getDotName() + " = " + node_.getShape().getDotText());
        ret.append(
            ", "
                + node_.getStyle().getDotName()
                + " = \"filled, "
                + node_.getStyle().getDotText()
                + "\"");
        ret.append("]\n");

        //************** RECORD STUFF DOESN'T WORK YET****************
        /*
        if(node_.getAttr(AttrType.SHAPE).equals("record") && !attrLabel.equals("")){
            ret.append(", label = \"{" + nameLabel + " | " + attrLabel + " }\"");
        }else{
            ret.append(", label = \"" + nameLabel);
        
            if(!nameLabel.equals("") &&
               !attrLabel.equals("")){
        	ret.append("\\n");
            }
            ret.append(attrLabel + "\"");
            }*/

        return ret.toString();
    }

    private String generateEdges() {
        StringBuffer edgeOutput = new StringBuffer();
        Edge edge;
        for (Iterator edges = _graph.getEdges(); edges.hasNext();) {
            edge = (Edge) edges.next();
            // Don't bother with invisible edges.
            if (edge.getStyle() != DotStyle.INVIS) {
                edgeOutput.append(generateEdge(edge));
            }
        }
        return edgeOutput.toString();
    }

    private String generateEdge(Edge edge_) {
        StringBuffer ret = new StringBuffer();

        //*****************DOT ATTRIBUTES***************************************

        // no longer need to differentiate between edges with two nodes or
        // "hyperarcs" because this is handled by the cartoonist who will
        // generate the empty nodes for hyperarcs

        String fromNode = edge_.getFromNode().getID();
        String toNode = edge_.getToNode().getID();

        if (fromNode.indexOf("\"") > -1) {
            fromNode = addEscapeForQuotes(fromNode);
        }
        if (toNode.indexOf("\"") > -1) {
            toNode = addEscapeForQuotes(toNode);
        }
        ret.append("\"" + fromNode + "\"");
        ret.append("->");
        ret.append("\"" + toNode + "\"");
        ret.append("[");

        ret.append(
            edge_.getColor().getDotName()
                + " = \""
                + edge_.getColor().getDotText()
                + "\", fontcolor = \""
                + edge_.getColor().getDotText()
                + "\"");
        ret.append(", " + edge_.getStyle().getDotName() + " = " + edge_.getStyle().getDotText());
        // old version seems to worry that the label is empty...??
        // i don't think that's necessary
        String edgeLabel = edge_.getLabel();
        if (edgeLabel.indexOf("\"") > -1) {
            edgeLabel = addEscapeForQuotes(edgeLabel);
        }
        ret.append(", label = \"" + edgeLabel + "\"");
        ret.append(
            ", "
                + edge_.getDirection().getDotName()
                + " = \""
                + edge_.getDirection().getDotText()
                + "\"");
        ret.append(", weight = \"" + edge_.getWeight() + "\"");

        ret.append("]\n");

        return ret.toString();
    }

    private String generateSameRank() {
        StringBuffer ret = new StringBuffer();

        for (Iterator setRanks = _graph.getNodeRankings(); setRanks.hasNext();) {
            ret.append("{ rank = same; ");

            for (Iterator sameRankNodes = ((Set) setRanks.next()).iterator();
                sameRankNodes.hasNext();
                ) {
                String sameRankNodeID = ((Node) sameRankNodes.next()).getID();
                if (sameRankNodeID.indexOf("\"") > -1) {
                    sameRankNodeID = addEscapeForQuotes(sameRankNodeID);
                }
                ret.append("\"");
                ret.append(sameRankNodeID);
				ret.append("\"");
                ret.append("; ");
            }

            ret.append("}\n");
        }

        return ret.toString();
    }

    // write <output> to file <filename>
    private void writeFile(String output, File file) {
        FileOutputStream fos = null;
        try {
            fos = new FileOutputStream(file);
        }
        catch (IOException e) {
            e.printStackTrace();
        }
        PrintWriter pw = new PrintWriter(fos);
        pw.write(output);
        pw.close();
    }

    // The Dot utility requires an escape sequence for names that have quotes
    private String addEscapeForQuotes(String name) {
        StringTokenizer tokenizer = new StringTokenizer(name, "\"", true);
        StringBuffer modifiedName = new StringBuffer();
        while (tokenizer.hasMoreTokens()) {
            String nextToken = tokenizer.nextToken();
            if (nextToken.equals("\"")) {
                modifiedName.append("\\\"");
            }
            else {
                modifiedName.append(nextToken);
            }
        }
        return modifiedName.toString();
    }
}
