package org.metagnostic.jniport.eg;

import javax.swing.event.TreeModelListener;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import java.util.*;


import org.metagnostic.jniport.DolphinRequest;


/**
 * A TreeModelExample is an example of how to use callbacks to pull
 * data out of the Dolphin session that owns us.  It implements the
 * TreeModel interface (so it can be used by JTree components), and
 * exposes the Dolphin class heirarchy.  Each Smalltalk class is
 * represented by its String name.  The root of the tree is a synthetic
 * node called "<<root>>".  The Smalltalk package 'CU Java Examples'
 * contains the corresponding Smalltalk-side code to handle the requests
 * these objects issue, see class OMJEgTreeModelExample.
 *<p>
 * Copyright &copy; 2003 and ongoing by Chris Uppal.
 *<p>
 * @author Chris Uppal (chris.uppal@metagnostic.org)
 */
public class TreeModelExample
implements TreeModel
{
    // the tags used to identify this class's callbacks,
    // we use the String ctor to ensure runtime uniqueness
    private static final Object
        s_getChildTag = new String("TreeModelExample.getChildTag()"),
        s_getChildCountTag = new String("TreeModelExample.getChildCount()"),
        s_getIndexOfChildTag = new String("TreeModelExample.getIndexOfChild()");


    // the usual list of Observers
    private final List  m_listeners = new LinkedList();

    
    /**
     * Returns the 'tag' used to identify getChild() requests from this class.
     */
    public static Object
    getChildTag()
    {
        return s_getChildTag;
    }


    /**
     * Returns the 'tag' used to identify getChildCount() requests from this class.
     */
    public static Object
    getChildCountTag()
    {
        return s_getChildCountTag;
    }


    /**
     * Returns the 'tag' used to identify getIndexOfChild() requests from this class.
     */
    public static Object
    getIndexOfChildTag()
    {
        return s_getIndexOfChildTag;
    }


    /**
     * @see javax.swing.tree.TreeModel#getRoot()
     */
    public Object
    getRoot()
    {
        // we identify the root of the tree by this special string
        return "<<root>>";
    }


    /**
     * @see javax.swing.tree.TreeModel#getChild(Object, int)
     */
    public Object
    getChild(Object parent, int index)
    {
        try
        {
            DolphinRequest req = new DolphinRequest(
                                    s_getChildTag,
                                    this,
                                    parent,
                                    new Integer(index));
            return req.value();
        }
        catch (Throwable e)
        {
            return null;
        }
    }


    /**
     * @see javax.swing.tree.TreeModel#getChildCount(Object)
     */
    public int
    getChildCount(Object parent)
    {
        try
        {
            DolphinRequest req = new DolphinRequest(
                                    s_getChildCountTag,
                                    this,
                                    parent);
            Object value = req.value();
            return ((Integer)value).intValue();
        }
        catch (Throwable e)
        {
            return 0;
        }
    }


    /**
     * @see javax.swing.tree.TreeModel#isLeaf(Object)
     */
    public boolean
    isLeaf(Object parent)
    {
        return getChildCount(parent) < 1;
    }


    /**
     * @see javax.swing.tree.TreeModel#valueForPathChanged(TreePath, Object)
     */
    public void
    valueForPathChanged(TreePath arg0, Object arg1)
    {
        // not interested
    }


    /**
     * @see javax.swing.tree.TreeModel#getIndexOfChild(Object, Object)
     */
    public int
    getIndexOfChild(Object parent, Object child)
    {
        if (parent == null || child == null)
            return -1;
        try
        {
            DolphinRequest req = new DolphinRequest(
                                    s_getIndexOfChildTag,
                                    this,
                                    parent,
                                    child);
            Object value = req.value();
            return ((Integer)value).intValue();
        }
        catch (Throwable e)
        {
            return -1;
        }
    }


    /**
     * @see javax.swing.tree.TreeModel#addTreeModelListener(TreeModelListener)
     */
    public void
    addTreeModelListener(TreeModelListener listener)
    {
        m_listeners.add(listener);
    }


    /**
     * @see javax.swing.tree.TreeModel#removeTreeModelListener(TreeModelListener)
     */
    public void
    removeTreeModelListener(TreeModelListener listener)
    {
        m_listeners.remove(listener);
    }
}
