package edu.mit.csail.sdg.alloy4.ui;

import java.io.PrintWriter;
import java.io.StringWriter;

public final class CrashReport implements Thread.UncaughtExceptionHandler {

    public void uncaughtException(Thread thread, Throwable ex) {
        // TODO: should ask the user whether we can email this to the Alloy developers or not
        StringWriter sw=new StringWriter();
        PrintWriter pw=new PrintWriter(sw);
        pw.printf("Thread name: %s\n\nException message: %s\n\nStack trace:\n", thread.getName(), ex.getMessage());
        ex.printStackTrace(pw);
        pw.close();
        sw.flush();
        System.out.println(sw.toString());
        System.exit(1);
    }
}
