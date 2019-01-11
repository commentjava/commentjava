/*
 * @(#)ProcessMonitorThread.java	1.7 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.corba.se.internal.Activation;

import java.util.*;
import com.sun.corba.se.internal.orbutil.ORBConstants;

/** ProcessMonitorThread is started when ServerManager is instantiated. The 
  * thread wakes up every minute (This can be changed by setting sleepTime) and
  * makes sure that all the processes (Servers) registered with the ServerTool
  * are healthy. If not the state in ServerTableEntry will be changed to
  * De-Activated.
  * Note: This thread can be killed from the main thread by calling 
  *       interrupThread()
  */
public class ProcessMonitorThread extends java.lang.Thread {
    private HashMap serverTable;
    private int sleepTime; 
    private static ProcessMonitorThread instance = null;

    private ProcessMonitorThread( HashMap ServerTable, int SleepTime ) {
        serverTable = ServerTable;
        sleepTime = SleepTime;
    }

    public void run( ) {
        while( true ) {
            try {
                // Sleep's for a specified time, before checking
                // the Servers health. This will repeat as long as
                // the ServerManager (ORBD) is up and running.
                Thread.sleep( sleepTime );
            } catch( java.lang.InterruptedException e ) {
                break;
            }
            synchronized ( serverTable ) {
                // Check each ServerTableEntry to make sure that they
                // are in the right state.
                Iterator serverList = serverTable.values().iterator();
                checkServerHealth( serverList );
            }
        }
    }

    private void checkServerHealth( Iterator serverList ) {
        while (serverList.hasNext( ) ) {
            ServerTableEntry entry = (ServerTableEntry) serverList.next();
            entry.checkProcessHealth( );
        }
    }

    static void start( HashMap serverTable ) { 
	int sleepTime = ORBConstants.DEFAULT_SERVER_POLLING_TIME;

	String pollingTime = System.getProperties().getProperty( 
	    ORBConstants.SERVER_POLLING_TIME ); 

	if ( pollingTime != null ) {
	    try {
		sleepTime = Integer.parseInt( pollingTime ); 
	    } catch (Exception e ) {
		// Too late to complain, Just use the default 
		// sleepTime
	    }
	}

	instance = new ProcessMonitorThread( serverTable, 
	    sleepTime );
	instance.setDaemon( true );
	instance.start();
    }

    static void interruptThread( ) {
        instance.interrupt();
    }
}
 
