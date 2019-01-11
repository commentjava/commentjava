/*
 * @(#)MotifScrollPaneUI.java	1.17 03/01/23
 *
 * Copyright 2003 Sun Microsystems, Inc. All rights reserved.
 * SUN PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 */

package com.sun.java.swing.plaf.motif;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.plaf.*;
import javax.swing.plaf.basic.BasicScrollPaneUI;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * A CDE/Motif L&F implementation of ScrollPaneUI.  
 * <p>
 * <strong>Warning:</strong>
 * Serialized objects of this class will not be compatible with
 * future Swing releases.  The current serialization support is appropriate
 * for short term storage or RMI between applications running the same
 * version of Swing.  A future release of Swing will provide support for
 * long term persistence.
 *
 * @version 1.10 08/28/98
 * @author Hans Muller
 */
public class MotifScrollPaneUI extends BasicScrollPaneUI
{
    private final static Border vsbMarginBorderR = new EmptyBorder(0, 4, 0, 0);
    private final static Border vsbMarginBorderL = new EmptyBorder(0, 0, 0, 4);
    private final static Border hsbMarginBorder = new EmptyBorder(4, 0, 0, 0);

    private Border vsbBorder;
    private Border hsbBorder;

    private PropertyChangeListener propertyChangeHandler;

    protected void installListeners(JScrollPane scrollPane) {
        super.installListeners(scrollPane);
	propertyChangeHandler = createPropertyChangeHandler();
	scrollPane.addPropertyChangeListener(propertyChangeHandler);
    }

    protected void uninstallListeners(JScrollPane scrollPane) {
        super.uninstallListeners(scrollPane);
	scrollPane.removePropertyChangeListener(propertyChangeHandler);
    }

    private PropertyChangeListener createPropertyChangeHandler() {
        return new PropertyChangeListener() {
	    public void propertyChange(PropertyChangeEvent e) {
		  String propertyName = e.getPropertyName();

		  if (propertyName.equals("componentOrientation")) {
			JScrollPane pane = (JScrollPane)e.getSource();
			JScrollBar vsb = pane.getVerticalScrollBar();
			if (vsb != null) {
			    if (MotifGraphicsUtils.isLeftToRight(pane)) {
			        vsbBorder = new CompoundBorder(new EmptyBorder(0, 4, 0, -4),
							vsb.getBorder());
			    } else {
			        vsbBorder = new CompoundBorder(new EmptyBorder(0, -4, 0, 4),
							vsb.getBorder());
			    }
			    vsb.setBorder(vsbBorder);
			}
                  }
	}};
    }

    protected void installDefaults(JScrollPane scrollpane) {
	super.installDefaults(scrollpane);

	JScrollBar vsb = scrollpane.getVerticalScrollBar();
	if (vsb != null) {
	    if (MotifGraphicsUtils.isLeftToRight(scrollpane)) {
	        vsbBorder = new CompoundBorder(vsbMarginBorderR, 
					       vsb.getBorder());
	    }
	    else {
	        vsbBorder = new CompoundBorder(vsbMarginBorderL, 
					       vsb.getBorder());
	    }
	    vsb.setBorder(vsbBorder);
	}

	JScrollBar hsb = scrollpane.getHorizontalScrollBar();
	if (hsb != null) {
	    hsbBorder = new CompoundBorder(hsbMarginBorder, hsb.getBorder());
	    hsb.setBorder(hsbBorder);
	}
    }


    protected void uninstallDefaults(JScrollPane c) {
	super.uninstallDefaults(c);

	JScrollBar vsb = scrollpane.getVerticalScrollBar();
	if (vsb != null) {
	    if (vsb.getBorder() == vsbBorder) {
		vsb.setBorder(null);
	    }
	    vsbBorder = null;
	}

	JScrollBar hsb = scrollpane.getHorizontalScrollBar();
	if (hsb != null) {
	    if (hsb.getBorder() == hsbBorder) {
		hsb.setBorder(null);
	    }
	    hsbBorder = null;
	}
    }


    public static ComponentUI createUI(JComponent x) {
	return new MotifScrollPaneUI();
    }
}

