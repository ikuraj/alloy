package edu.mit.csail.sdg.alloy4.translator;

/**
 * This class contains a global flag
 * (indicating whether the user wishes to stop the analysis).
 *
 * @author Felix Chang
 */

public final class Stopper {

  /** The constructor is private, since this class never needs to be instantiated. */
  private Stopper() { }

  /**
   * This volatile static field indicates whether the user wishes to stop the analysis.
   *
   * <p/> When the user wishes to stop the analysis, the user should set this flag to true;
   * <br/> When the system is idle, the flag will be set to false.
   *
   * <p/> (The default value is false).
   */
  public static volatile boolean stopped=false;
}
