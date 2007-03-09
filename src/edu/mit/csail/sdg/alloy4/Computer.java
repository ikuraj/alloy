package edu.mit.csail.sdg.alloy4;

/**
 * This defines a compute() method that take a String input and produces a String output.
 */

public interface Computer {

    /**
     * This method takes a String inputs and produces a String output.
     * @throws Exception if an error occurred during the computation.
     */
    public String compute(String input) throws Exception;

    /**
     * This method sets the filename associated with this Computer.
     */
    public void setSourceFile(String filename);
}
